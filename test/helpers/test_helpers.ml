(** Test helper functions for CSS comparison and minimization *)

module Css = Cascade.Css
module Css_compare = Cascade_diff.Css_compare

(** Check that a utility value produces the expected class name *)
let check_class expected t =
  Alcotest.check Alcotest.string "class" expected (Tw.Utility.to_class t)

(** Extract utilities layer from CSS *)
let extract_utilities_layer_rules css =
  let stmts = Css.statements css in
  List.find_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some [ "utilities" ], rules) -> Some rules
      | _ -> None)
    stmts
  |> Option.value ~default:[]

(** Extract selectors from rules *)
let extract_rule_selectors stmts =
  List.filter_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, _, _) -> Some (Css.Selector.to_string selector)
      | None -> None)
    stmts

let our_css utilities =
  Tw.to_css ~base:true utilities |> Css.to_string ~minify:true ~lossless:true

let tailwind_css ?(forms = false) classnames =
  (* Parity tests need the pinned tailwindcss CLI; skip where it is absent (e.g.
     opam-repo-ci has no node). *)
  if not (Tw_tools.Tailwind_gen.available ()) then Alcotest.skip ();
  Tw_tools.Tailwind_gen.generate ~minify:true ~optimize:true ~forms classnames

(* Which CSS properties a class declares. Custom properties count: a drop-shadow
   colour and a drop-shadow size conflict only on [--tw-drop-shadow]. *)
let properties_of_class cls =
  match Tw.of_string cls with
  | Error _ -> []
  | Ok u ->
      Tw.to_css ~base:false [ u ]
      |> Css.fold
           (fun acc stmt ->
             match Css.as_rule stmt with
             | Some (sel, decls, _) when Css.Selector.to_string sel <> ":root"
               ->
                 List.map Css.Declaration.property_key decls @ acc
             | _ -> acc)
           []

(* What a class writes on an element carrying it, as one rule. The theme
   bindings it drags in are left out - every class reading the spacing scale
   writes [--spacing], which says nothing about what two of them do to each
   other - and so are the [*, ::before] property defaults: neither is a selector
   holding a [.]. *)
let class_rule cls =
  let decls =
    match Tw.of_string cls with
    | Error _ -> []
    | Ok u ->
        Tw.to_css ~base:false [ u ]
        |> Css.fold
             (fun acc stmt ->
               match Css.as_rule stmt with
               | Some (sel, decls, _)
                 when String.contains (Css.Selector.to_string sel) '.' ->
                   decls @ acc
               | _ -> acc)
             []
  in
  Css.rule ~selector:(Css.Selector.class_ cls) decls

(* Two classes whose relative order is cascade-significant. Canonicalisation
   sorts statements that cannot observe each other into a content-keyed order
   and leaves the order of the ones that can, so a pair whose two spellings
   canonicalise apart is a pair writing a common slot - cascade's own footprint
   model, shorthand expansion included. This is what pairing on the declared
   property name cannot see: [inset-px] and [top-px] share no property name and
   both decide [top]. *)
let cascade_conflict a b =
  let canon stmts =
    Css.canonicalize_rule_order (Css.v stmts) |> Css.to_string ~minify:true
  in
  canon [ a; b ] <> canon [ b; a ]

let overlapping_pairs classes =
  let rules = List.map (fun cls -> (cls, class_rule cls)) classes in
  let rec go acc = function
    | [] | [ _ ] -> acc
    | (a, ra) :: tl ->
        let acc =
          List.fold_left
            (fun acc (b, rb) ->
              if cascade_conflict ra rb then (a, b) :: acc else acc)
            acc tl
        in
        go acc tl
  in
  go [] rules

(* Classes that declare a property in common. Kept alongside the footprint
   model, which is about order alone: two classes writing the same property with
   the same value do not conflict, and still compose - [shadow-lg] reads the
   colour [shadow-current] writes, and only an element carrying both shows what
   the pair computes to. *)
let same_property_pairs classes =
  let by_prop = Hashtbl.create 256 in
  List.iter
    (fun cls ->
      List.iter
        (fun k ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt by_prop k) in
          Hashtbl.replace by_prop k (cls :: prev))
        (properties_of_class cls))
    classes;
  let seen = Hashtbl.create 1024 in
  Hashtbl.fold
    (fun _ cs acc ->
      let cs = List.sort_uniq String.compare cs in
      let rec pairs acc = function
        | [] | [ _ ] -> acc
        | a :: tl ->
            let acc =
              List.fold_left
                (fun acc b ->
                  let p = (a, b) in
                  if Hashtbl.mem seen p then acc
                  else (
                    Hashtbl.add seen p ();
                    p :: acc))
                acc tl
            in
            pairs acc tl
      in
      pairs acc cs)
    by_prop []

(* Classes that write on each other, paired up. An ordering difference is only
   observable on an element carrying two such classes; one class on its own can
   only show a difference in value. *)
let interacting_pairs classes =
  let key (a, b) = if a <= b then (a, b) else (b, a) in
  same_property_pairs classes @ overlapping_pairs classes
  |> List.map key |> List.sort_uniq compare

(* The single ordering predicate. The fuzzer minimises with it and every suite
   asserts on it, so a minimal case it reports is a case the assertion also
   rejects; two predicates that disagree let the fuzzer print a failing case and
   pass anyway. Pruning dead custom properties makes it blind to utilities whose
   only output is an unreferenced binding - check_rendering_matches covers that
   class now, and agreeing on one predicate is worth more than the extra
   sensitivity here. *)
let ordering_diff ?(forms = false) utilities =
  let classnames = List.map Tw.pp utilities in
  Css_compare.diff ~mode:`Canonical ~prune_unused_custom_props:true
    (tailwind_css ~forms classnames)
    (our_css utilities)

let check_ordering_fails ?forms utilities =
  match (ordering_diff ?forms utilities).Css_compare.result with
  | Css_compare.No_diff -> false
  | _ -> true

(** Delta Debugging (ddmin algorithm by Zeller) Minimizes a failing test case by
    binary search *)
let rec take k l =
  match (k, l) with
  | 0, _ | _, [] -> ([], l)
  | k, x :: xs ->
      let taken, rest = take (k - 1) xs in
      (x :: taken, rest)

let split_into_subsets subset_size lst =
  let rec go acc remaining =
    if remaining = [] then List.rev acc
    else
      let subset, rest = take subset_size remaining in
      go (subset :: acc) rest
  in
  go [] lst

let delta_debug check_fails lst =
  let rec ddmin lst n =
    let len = List.length lst in
    if len = 1 then lst
    else
      let subsets = split_into_subsets (max 1 (len / n)) lst in
      (* Test 1: Try each subset alone (reduce to subset) *)
      let rec try_subsets = function
        | [] -> None
        | subset :: rest ->
            if check_fails subset then Some subset else try_subsets rest
      in
      match try_subsets subsets with
      | Some subset ->
          Fmt.epr "Reduced to %d items@." (List.length subset);
          ddmin subset 2
      | None -> (
          (* Test 2: Try removing each subset (reduce to complement) *)
          let rec try_complements idx = function
            | [] -> None
            | _ :: rest ->
                let complement =
                  List.concat (List.filteri (fun i _ -> i <> idx) subsets)
                in
                if List.length complement < len && check_fails complement then
                  Some complement
                else try_complements (idx + 1) rest
          in
          match try_complements 0 subsets with
          | Some complement ->
              Fmt.epr "Reduced to %d items@." (List.length complement);
              ddmin complement (max (n - 1) 2)
          | None ->
              (* Increase granularity *)
              if n < len then ddmin lst (min (n * 2) len) else lst)
  in
  ddmin lst 2

(** Find minimal failing pair from a list *)
let minimal_pair check_fails lst =
  let rec find_pair lst =
    match lst with
    | [] | [ _ ] -> None
    | a :: rest ->
        let rec try_with = function
          | [] -> find_pair rest
          | b :: rest' ->
              if check_fails [ a; b ] then Some [ a; b ] else try_with rest'
        in
        try_with rest
  in
  find_pair lst

(** Minimize a failing test case to smallest possible set Uses delta debugging
    followed by pair finding if needed *)
let minimize_failing_case check_fails initial =
  if not (check_fails initial) then None
  else
    let minimal = delta_debug check_fails initial in

    (* If we have more than 2 items, try to find a minimal pair *)
    let final =
      if List.length minimal > 2 then
        match minimal_pair check_fails minimal with
        | Some pair -> pair
        | None -> minimal
      else minimal
    in
    Some final

(* Differential rendering. Both sheets are loaded in headless Chromium and the
   computed styles compared: a property that computes the same is equivalent
   however the two sheets spell it, and one that differs is observable by
   definition. It needs node, Playwright and its Chromium, none of which a plain
   opam build has, so the check skips when they are missing rather than being
   gated in dune. Set TW_BROWSER_TESTS=0 to opt out where they are present. *)
let rec dir_containing name dir =
  if Sys.file_exists (Filename.concat dir name) then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else dir_containing name parent

(* Tests run from the build directory, which is itself inside the project, so
   the walk up finds the root whether or not dune sandboxed us. node_modules is
   what we are really after: node resolves playwright from there. *)
let project_root = lazy (dir_containing "node_modules" (Sys.getcwd ()))
let browser_script = "test/helpers/browser/compare.js"

let browser_available root =
  Sys.getenv_opt "TW_BROWSER_TESTS" <> Some "0"
  && Sys.file_exists (Filename.concat root "node_modules/playwright")
  && Sys.file_exists (Filename.concat root browser_script)
  && Sys.command "node --version > /dev/null 2>&1" = 0

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

(* One directory per test, kept after the run: the two sheets and the element
   list are what you need to reproduce a failure by hand. *)
let render_dir root test_name =
  let safe =
    String.map
      (fun c ->
        match c with 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> c | _ -> '_')
      test_name
  in
  let mkdir dir =
    (* Another runner may have created it between the two calls. *)
    if not (Sys.file_exists dir) then
      try Sys.mkdir dir 0o755 with Sys_error _ -> ()
  in
  List.fold_left
    (fun dir part ->
      let dir = Filename.concat dir part in
      mkdir dir;
      dir)
    root [ "tmp"; "browser"; safe ]

(* First occurrence wins and the order is kept. Through a table rather than a
   scan of what has been seen: the list this runs on is quadratic in the number
   of classes. *)
let dedup l =
  let seen = Hashtbl.create 256 in
  List.rev
    (List.fold_left
       (fun acc x ->
         if Hashtbl.mem seen x then acc
         else (
           Hashtbl.add seen x ();
           x :: acc))
       [] l)

(* The elements the browser check builds: every class on its own, since a class
   alone can still differ in value, and one carrying each interacting pair,
   which is where an ordering difference shows. *)
let render_elements classnames =
  dedup
    (classnames
    @ List.map (fun (a, b) -> a ^ " " ^ b) (interacting_pairs classnames))

let check_rendering_matches ?(forms = false) ~test_name utilities =
  let root =
    match Lazy.force project_root with Some r -> r | None -> Alcotest.skip ()
  in
  if not (browser_available root) then Alcotest.skip ();
  let classnames = List.map Tw.pp utilities in
  let elements = render_elements classnames in
  let tailwind = tailwind_css ~forms classnames in
  let dir = render_dir root test_name in
  let path name = Filename.concat dir name in
  write_file (path "tw.css") (our_css utilities);
  write_file (path "tailwind.css") tailwind;
  write_file (path "elements.txt") (String.concat "\n" elements);
  let out = path "diff.txt" and err = path "stderr.txt" in
  let cmd =
    Fmt.str "node %s %s %s %s > %s 2> %s"
      (Filename.quote (Filename.concat root browser_script))
      (Filename.quote (path "elements.txt"))
      (Filename.quote (path "tw.css"))
      (Filename.quote (path "tailwind.css"))
      (Filename.quote out) (Filename.quote err)
  in
  match Sys.command cmd with
  | 0 -> ()
  | 1 -> Alcotest.failf "%s\n%s" test_name (read_file out)
  | 3 ->
      (* The elements did not carry the classes we asked for, so what they
         computed compares nothing. Fail: it is a broken harness, not a missing
         tool. *)
      Alcotest.failf "%s: browser harness built unusable markup\n%s" test_name
        (read_file out)
  | _ ->
      (* No usable browser (Chromium not downloaded, sandbox refused to start).
         Report it and skip: it is a missing tool, not a difference. *)
      Fmt.epr "browser rendering unavailable: %s@."
        (String.trim (read_file err));
      Alcotest.skip ()

let check_ordering_matches ?forms ~test_name utilities =
  let diff = ordering_diff ?forms utilities in
  match diff.Css_compare.result with
  | Css_compare.No_diff -> ()
  | _ ->
      let buf = Buffer.create 1024 in
      Css_compare.pp ~expected:"Tailwind" ~actual:"Our TW" buf diff;
      Alcotest.failf "%s\n%s" test_name (Buffer.contents buf)

(* Where a class's rule starts in a sheet. The match has to end where the class
   name ends, so [.bg-top] does not report [.bg-top-left]; what follows it is
   not constrained beyond that, so a selector that carries on past the class
   ([.divide-x>*], [.group:hover .x]) is found rather than reported absent. *)
let continues_class_name c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true
  (* A backslash starts an escape, which is part of the name it sits in; a byte
     at or above 0x80 is part of a non-ASCII identifier. *)
  | '\\' -> true
  | c -> Char.code c >= 0x80

let class_position sheet cls =
  let sel = Css.Selector.to_string (Css.Selector.class_ cls) in
  let n = String.length sel and len = String.length sheet in
  let rec scan i =
    if i + n > len then None
    else if
      String.sub sheet i n = sel
      (* An escaped dot belongs to the class name before it: [.w-1\.5] is one
         class, not a [.5] inside another. *)
      && (i = 0 || sheet.[i - 1] <> '\\')
      && (i + n = len || not (continues_class_name sheet.[i + n]))
    then Some i
    else scan (i + 1)
  in
  scan 0

let check_class_order ?forms ~test_name classes =
  let utilities =
    List.map
      (fun c ->
        match Tw.of_string c with
        | Ok u -> u
        | Error (`Msg m) -> Alcotest.failf "%s: %s" c m)
      classes
  in
  let order label sheet =
    List.map
      (fun cls ->
        match class_position sheet cls with
        | Some i -> (i, cls)
        | None ->
            Alcotest.failf "%s: %s missing from the %s sheet" test_name cls
              label)
      classes
    |> List.sort compare |> List.map snd
  in
  let expected = order "Tailwind" (tailwind_css ?forms classes) in
  Alcotest.(check (list string))
    test_name expected
    (order "tw" (our_css utilities))

(** CSS Test Helpers *)

(** Check if a layer exists in the stylesheet *)
let has_layer name css =
  List.exists
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some declared, _)
        when Css.Stylesheet.equal_layer_name declared [ name ] ->
          true
      | _ -> false)
    (Css.statements css)

(** Get all custom property names from a layer *)
let vars_in_layer layer_name css = Css.custom_props ~layer:[ layer_name ] css

(** Check if a variable name exists in a layer *)
let has_var_in_layer var_name layer_name css =
  let vars = vars_in_layer layer_name css in
  List.exists (fun v -> v = var_name) vars

(** Get all selectors from a layer *)
let selectors_in_layer layer_name css =
  match Css.layer_block [ layer_name ] css with
  | None -> []
  | Some stmts ->
      List.filter_map
        (fun stmt ->
          match Css.as_rule stmt with
          | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
          | None -> None)
        stmts

(** Check if a selector exists in a layer *)
let has_selector_in_layer selector layer_name css =
  let sels = selectors_in_layer layer_name css in
  List.mem selector sels

(** Get all media query conditions from stylesheet, recursively *)
let media_conditions css =
  Css.fold
    (fun acc stmt ->
      match Css.as_media stmt with
      | Some (cond, _) -> Css.Media.to_string cond :: acc
      | None -> acc)
    [] css
  |> List.rev

(** Check if a specific media condition exists *)
let has_media_condition condition css =
  List.mem condition (media_conditions css)

(** Return statements for a given media condition, if present *)
let media_block condition css =
  Css.fold
    (fun acc stmt ->
      match (acc, Css.as_media stmt) with
      | Some _, _ -> acc
      | None, Some (cond, inner)
        when String.equal (Css.Media.to_string cond) condition ->
          Some inner
      | None, _ -> None)
    None css

let selectors_in_media ~condition css =
  match media_block condition css with
  | None -> []
  | Some stmts ->
      List.filter_map
        (fun s ->
          match Css.as_rule s with
          | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
          | None -> None)
        stmts

let has_selector_in_media ~condition ~selector css =
  List.mem selector (selectors_in_media ~condition css)

let count_selector_in_media ~condition ~selector css =
  selectors_in_media ~condition css
  |> List.fold_left
       (fun acc s -> if String.equal s selector then acc + 1 else acc)
       0

(* Selector testable and utilities *)
let pp_selector fmt sel = Fmt.string fmt (Css.Selector.to_string sel)
let selector_testable = Alcotest.testable pp_selector ( = )

let sort_selectors sels =
  let cmp a b =
    String.compare (Css.Selector.to_string a) (Css.Selector.to_string b)
  in
  List.sort cmp sels

let selectors_in_media_sel ~condition css =
  match media_block condition css with
  | None -> []
  | Some stmts ->
      List.filter_map
        (fun s ->
          match Css.as_rule s with Some (sel, _, _) -> Some sel | None -> None)
        stmts

let selector_eq a b =
  String.equal (Css.Selector.to_string a) (Css.Selector.to_string b)

let has_selector_in_media_sel ~condition ~selector css =
  selectors_in_media_sel ~condition css
  |> List.exists (fun sel -> selector_eq sel selector)

let count_selector_in_media_sel ~condition ~selector css =
  selectors_in_media_sel ~condition css
  |> List.fold_left
       (fun acc sel -> if selector_eq sel selector then acc + 1 else acc)
       0

(** Check if inline style contains a specific property *)
let inline_has_property prop_name inline_style =
  String.split_on_char ';' inline_style
  |> List.exists (fun prop ->
      String.trim prop |> String.split_on_char ':' |> function
      | prop :: _ -> String.trim prop = prop_name
      | [] -> false)

(** Check if declarations contain any var() references *)
let has_var_in_declarations ?(inline = false) decls =
  (* Anywhere in the value, not only at its head: [calc(var(--spacing)*4)] and
     [color-mix(in oklab, var(--c) 50%, #0000)] reference a variable as much as
     a bare [var(--c)] does, and an assertion that a sheet holds none is vacuous
     on them otherwise. *)
  List.exists
    (fun decl ->
      Astring.String.is_infix ~affix:"var(" (Css.declaration_value ~inline decl))
    decls

(** {1 Utility Generators} *)

(** Common spacing values used in Tailwind *)
let spacing_values =
  [ 0; 1; 2; 3; 4; 5; 6; 8; 10; 12; 16; 20; 24; 32; 40; 48; 64 ]

(** Global RNG for randomized tests. Initialized with a random seed that is
    printed to stderr for reproducibility. Set [TEST_SEED] env var to replay a
    specific seed. *)
let test_rng =
  let seed =
    match Sys.getenv_opt "TEST_SEED" with
    | Some s -> (
        match int_of_string_opt s with
        | Some n -> n
        | None ->
            (* Raising here aborts the whole suite at module initialisation with
               a message that never mentions the variable. *)
            Fmt.failwith "TEST_SEED is not an integer: %S" s)
    | None ->
        Random.self_init ();
        Random.bits ()
  in
  Fmt.epr "Test seed: %d (replay with TEST_SEED=%d)@." seed seed;
  Random.State.make [| seed |]

(** Shuffle a list using Fisher-Yates algorithm with the global test RNG. *)
let shuffle lst =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.State.int test_rng (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

(** {1 Generic Test Patterns} *)

module type Handler = sig
  type t

  val of_class : Tw.Scheme.t -> string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
end

(** Generic handler test - checks that parsing and pretty-printing round-trip
    correctly. Takes a class name string, parses it with of_class, converts back
    with to_class, and verifies they match. *)
let check_handler_roundtrip (module H : Handler) class_name =
  (* Test of_class -> to_class roundtrip *)
  match H.of_class Tw.Scheme.default class_name with
  | Ok result ->
      let class_name2 = H.to_class result in
      Alcotest.(check string)
        "of_class -> to_class roundtrip" class_name class_name2
  | Error (`Msg msg) ->
      Alcotest.fail ("of_class failed for '" ^ class_name ^ "': " ^ msg)

(** Generic test for invalid inputs - expects parsing to fail *)
let check_invalid_input (module H : Handler) input =
  match H.of_class Tw.Scheme.default input with
  | Ok _ -> Alcotest.fail ("Expected error for: " ^ input)
  | Error _ -> ()

let standard ~roundtrip ~invalid =
  Alcotest.
    [
      test_case "roundtrip" `Quick roundtrip; test_case "invalid" `Quick invalid;
    ]

(** Helper that takes a list of parts, concatenates with "-", and checks
    roundtrip *)
let check_parts (module H : Handler) parts =
  let class_name = String.concat "-" parts in
  check_handler_roundtrip (module H) class_name

(** Helper that takes a list of parts, concatenates with "-", and checks that
    parsing fails *)
let check_invalid_parts (module H : Handler) parts =
  let class_name = String.concat "-" parts in
  check_invalid_input (module H) class_name

(** [check_typed_class cls value] checks that the typed constructor [value]
    pretty-prints to class name [cls] and that [cls] round-trips back through
    [Tw.of_string] to the same name. Used to pin newly exposed typed
    constructors against the string parser. *)
let check_typed_class cls value =
  Alcotest.(check string) cls cls (Tw.pp value);
  match Tw.of_string cls with
  | Ok u -> Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u)
  | Error (`Msg m) ->
      Alcotest.failf "%s: parser rejected its own class: %s" cls m
