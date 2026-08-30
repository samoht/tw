(** Test helper functions for CSS comparison and minimization *)

module Css = Cascade.Css
module Css_compare = Cascade_diff.Css_compare
module Tree_diff = Cascade_diff.Tree_diff

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

(* Parity tests need the pinned tailwindcss CLI. Skipping is right on a
   developer machine without node, and wrong on CI, where it retires a fifth of
   the suite into [SKIP] lines that dune swallows on success, so the run reports
   agreement with a tool it never ran. Set TW_TAILWIND_TESTS=1 where the CLI is
   meant to be present and a missing or off-version one fails instead. *)
let tailwind_required () = Sys.getenv_opt "TW_TAILWIND_TESTS" = Some "1"

(* Alcotest files a test's own stderr under [_build/_tests/] and exits the
   process itself, so a notice written from inside a test, or after
   [Alcotest.run] returns, is never read. Count the skips and report the total
   at exit, where it lands next to the summary line that does not mention
   them. *)
let skipped_without_cli = ref 0

let note_skip reason =
  if !skipped_without_cli = 0 then
    at_exit (fun () ->
        Fmt.epr "@.%d test(s) skipped: no usable tailwindcss CLI.@.%s@."
          !skipped_without_cli reason;
        Fmt.epr "Set TW_TAILWIND_TESTS=1 to fail on this instead of skipping.@.");
  incr skipped_without_cli

let require_tailwind_cli () =
  match Tw_tools.Tailwind_gen.availability () with
  | Ok () -> ()
  | Error reason ->
      if tailwind_required () then
        Alcotest.failf
          "TW_TAILWIND_TESTS=1 but the tailwindcss CLI is unusable: %s" reason
      else begin
        note_skip reason;
        Alcotest.skip ()
      end

let tailwind_css ?(forms = false) classnames =
  require_tailwind_cli ();
  Tw_tools.Tailwind_gen.generate ~minify:true ~optimize:true ~forms classnames

(* Which CSS properties a class declares. Custom properties count: a drop-shadow
   colour and a drop-shadow size conflict only on [--tw-drop-shadow]. *)
(* Compiling a class is the expensive part of building the pair list, and each
   class is compiled by both readers below, once for its property names and
   once for its rule. Memoised on the class name; the suite's class set bounds
   the table. *)
let compiled_cache : (string, Css.t option) Hashtbl.t = Hashtbl.create 512

let compiled cls =
  match Hashtbl.find_opt compiled_cache cls with
  | Some sheet -> sheet
  | None ->
      let sheet =
        match Tw.of_string cls with
        | Error _ -> None
        | Ok u -> Some (Tw.to_css ~base:false [ u ])
      in
      Hashtbl.add compiled_cache cls sheet;
      sheet

let properties_of_class cls =
  match compiled cls with
  | None -> []
  | Some sheet ->
      sheet
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
    match compiled cls with
    | None -> []
    | Some sheet ->
        sheet
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

(* [Css_compare] drops a declaration its reader rejects from that side's AST
   before the comparison runs, so a real difference can surface as a phantom
   addition on the side that parsed, or as no difference at all when both sides
   collapse to the same AST. A rejection is a finding about the comparison, not
   noise to ignore.

   One rejection is known and is Tailwind's own: it writes the [color-mix]
   mixing amount as a bare number ([color-mix(in srgb, red .5, transparent)]),
   where CSS Color 5 sec. 3.1 admits only a [<percentage>], so a browser drops
   that declaration too. The upstream runner rewrites that spelling before
   either side is parsed, so the exception is there for the suites that compare
   against the live Tailwind CLI, which does not go through that rewrite. *)
let known_reader_rejection (error : Cascade.Error.t) =
  match error.kind with
  | Cascade.Error.Bad_value { reason = "expected color-mix percentage"; _ } ->
      true
  | _ -> false

let dropped_declarations (diff : Css_compare.t) =
  diff.Css_compare.expected_warnings @ diff.Css_compare.actual_warnings
  |> List.filter (fun e -> not (known_reader_rejection e))

let check_no_dropped_declarations ~test_name diff =
  let dropped = dropped_declarations diff in
  if dropped <> [] then
    Alcotest.failf
      "%s: the comparison could not read %d declaration(s) and dropped them, \
       so it compared less than it appears to:\n\
       %s"
      test_name (List.length dropped)
      (String.concat "\n" (List.map Cascade.Error.to_string dropped))

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

(* Skipping is right on a developer machine with no browser, and wrong on CI,
   where it reports eight suites as finding no rendering difference because they
   never looked. Set TW_BROWSER_TESTS=1 where the browser is meant to be present
   and a missing one fails instead. *)
let browser_required () = Sys.getenv_opt "TW_BROWSER_TESTS" = Some "1"

let unavailable test_name reason =
  if browser_required () then
    Alcotest.failf "%s: TW_BROWSER_TESTS=1 but no usable browser: %s" test_name
      reason
  else begin
    Fmt.epr "browser rendering unavailable: %s@." reason;
    Alcotest.skip ()
  end

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

(* The element list is line-oriented, so markup that spans lines in the test
   source is folded onto one. HTML reads the two the same, and nothing the
   comparison looks at is computed from the markup's own whitespace. *)
let one_line s = String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) s

let check_rendering_matches ?(forms = false) ?(inner = "") ~test_name utilities
    =
  let root =
    match Lazy.force project_root with Some r -> r | None -> Alcotest.skip ()
  in
  if not (browser_available root) then
    unavailable test_name "node, Playwright or the compare script is missing";
  let classnames = List.map Tw.pp utilities in
  let elements = render_elements classnames in
  let tailwind = tailwind_css ~forms classnames in
  let dir = render_dir root test_name in
  let path name = Filename.concat dir name in
  let entry cls =
    if String.equal inner "" then cls else cls ^ "\t" ^ one_line inner
  in
  write_file (path "tw.css") (our_css utilities);
  write_file (path "tailwind.css") tailwind;
  write_file (path "elements.txt")
    (String.concat "\n" (List.map entry elements));
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
         A missing tool, not a difference. *)
      unavailable test_name (String.trim (read_file err))

let check_ordering_matches ?forms ~test_name utilities =
  let diff = ordering_diff ?forms utilities in
  check_no_dropped_declarations ~test_name diff;
  match diff.Css_compare.result with
  | Css_compare.No_diff -> ()
  | _ ->
      let buf = Buffer.create 1024 in
      Css_compare.pp ~expected:"Tailwind" ~actual:"Our TW" buf diff;
      Alcotest.failf "%s\n%s" test_name (Buffer.contents buf)

(* Cascade's printer and Tailwind's minifier spell the same value differently
   wherever CSS makes the difference insignificant: a custom property holds
   [oklch(63.7% .237 25.331)] on one side and [oklch(63.7%.237 25.331)] on the
   other, a font stack keeps its quotes on one and drops them on the other.
   Neither sheet is wrong, so a comparison sensitive to the bytes reads both
   through the same printer first. Parsing and re-printing respells a sheet
   without changing what it declares: the printer merges no rules, moves none,
   and drops no binding, so a rule written twice stays written twice and a
   binding nothing reads stays bound. A sheet the reader rejects is passed
   through untouched, leaving {!Css_compare} to report the parse error. *)
let respelled css =
  match Css.of_string css with
  | Ok { Css.stylesheet; _ } -> Css.to_string ~minify:true stylesheet
  | Error _ -> css

let tree_diff_css ~expected ~actual =
  Css_compare.diff ~mode:`Tree (respelled expected) (respelled actual)

let tree_diff ?(forms = false) utilities =
  let classnames = List.map Tw.pp utilities in
  tree_diff_css
    ~expected:(tailwind_css ~forms classnames)
    ~actual:(our_css utilities)

(* What tw's sheet carries that Tailwind's does not: a whole rule, or a
   declaration inside a rule both sheets write. A rule emitted twice reads as
   the second copy being added, and a custom-property binding nothing references
   as the binding being added, so the two are one question. Mode [`Canonical]
   answers neither: its optimizer folds the second copy away, and every caller
   prunes unreferenced bindings off both sides before comparing.

   A container only one sheet has contributes nothing: the two sheets spell an
   [@container] query differently and the block comes and goes as a pair, so
   reading its rules here would report the spelling. *)
let rule_surplus where acc (rule : Tree_diff.rule_diff) =
  match rule with
  | Tree_diff.Added { selector; _ } ->
      Fmt.str "%s%s (the whole rule)" where selector :: acc
  | Tree_diff.Content_changed { selector; added_properties; _ } ->
      List.fold_left
        (fun acc prop -> Fmt.str "%s%s { %s }" where selector prop :: acc)
        acc added_properties
  | _ -> acc

let rec container_surplus where acc (container : Tree_diff.container_diff) =
  match container with
  | Tree_diff.Modified { info; rule_changes; container_changes; _ } ->
      let where = where ^ info.condition ^ " " in
      let acc = List.fold_left (rule_surplus where) acc rule_changes in
      List.fold_left (container_surplus where) acc container_changes
  | _ -> acc

let surplus (diff : Css_compare.t) =
  match diff.Css_compare.result with
  | Css_compare.Tree_diff t ->
      let acc = List.fold_left (rule_surplus "") [] t.Tree_diff.rules in
      List.rev
        (List.fold_left (container_surplus "") acc t.Tree_diff.containers)
  | _ -> []

let check_no_surplus ~test_name diff =
  match surplus diff with
  | [] -> ()
  | extra ->
      Alcotest.failf
        "%s: tw writes %d rule(s) or declaration(s) Tailwind does not:\n%s"
        test_name (List.length extra) (String.concat "\n" extra)

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

(* Whole-sheet statement order. [class_position] answers where one named class
   sits, and [check_class_order] asks that of a handful at a time; the pair
   below asks it of every statement in a layer at once. Both read the minified
   text rather than a parsed AST, because the order a browser resolves is the
   order the bytes carry, and both sheets are read the same way, so a difference
   is tw's and not the reader's. *)

(* One pass over CSS text has to honour strings and escapes, or a [}] inside
   [content: "}"] closes the wrong block. [skip_quoted s hi i q] is the index
   after the string that [q] opened at [i]. *)
let rec skip_quoted s hi i q =
  if i >= hi then i
  else if s.[i] = '\\' then skip_quoted s hi (i + 2) q
  else if s.[i] = q then i + 1
  else skip_quoted s hi (i + 1) q

(* Whitespace runs collapse to one space, so a prelude reads the same however
   the printer broke it. *)
let normalize_prelude s =
  let buf = Buffer.create (String.length s) in
  let space = ref false in
  String.iter
    (fun c ->
      match c with
      | ' ' | '\t' | '\r' | '\n' -> space := true
      | c ->
          if !space && Buffer.length buf > 0 then Buffer.add_char buf ' ';
          space := false;
          Buffer.add_char buf c)
    s;
  Buffer.contents buf

(* The statements of [sheet] between [lo] and [hi] as (prelude, body) pairs,
   where a body is the span inside the braces and [None] marks a [;]-terminated
   statement such as [@layer a, b;]. Bodies are not descended into: what is
   measured is one layer's top-level sequence. *)
let statements_between sheet lo hi =
  let rec skip_ws i =
    if i < hi then
      match sheet.[i] with
      | ' ' | '\t' | '\r' | '\n' -> skip_ws (i + 1)
      | _ -> i
    else i
  in
  let rec prelude_end i =
    if i >= hi then `Eof i
    else
      match sheet.[i] with
      | '\\' -> prelude_end (i + 2)
      | ('"' | '\'') as q -> prelude_end (skip_quoted sheet hi (i + 1) q)
      | '{' -> `Block i
      | ';' -> `Semi i
      | _ -> prelude_end (i + 1)
  in
  let rec block_end i depth =
    if i >= hi then i
    else
      match sheet.[i] with
      | '\\' -> block_end (i + 2) depth
      | ('"' | '\'') as q -> block_end (skip_quoted sheet hi (i + 1) q) depth
      | '{' -> block_end (i + 1) (depth + 1)
      | '}' -> if depth = 1 then i + 1 else block_end (i + 1) (depth - 1)
      | _ -> block_end (i + 1) depth
  in
  let rec loop i acc =
    let i = skip_ws i in
    if i >= hi then List.rev acc
    else
      match prelude_end i with
      | `Eof j ->
          let last = String.sub sheet i (j - i) in
          List.rev (if String.trim last = "" then acc else (last, None) :: acc)
      | `Semi j -> loop (j + 1) ((String.sub sheet i (j - i), None) :: acc)
      | `Block j ->
          let e = block_end (j + 1) 1 in
          loop e ((String.sub sheet i (j - i), Some (j + 1, e - 1)) :: acc)
  in
  loop lo []

(* A selector list is one statement but several rules as far as order goes, so
   each branch is keyed on its own. The split takes a [,] outside any string,
   escape, [(...)] or [[...]]: [:is(a, b)] and [[title="a,b"]] carry their
   own. *)
let selector_branches sel =
  let hi = String.length sel in
  let out = ref [] and start = ref 0 and depth = ref 0 and i = ref 0 in
  let emit stop =
    let s = String.trim (String.sub sel !start (stop - !start)) in
    if s <> "" then out := s :: !out
  in
  while !i < hi do
    (match sel.[!i] with
    | '\\' -> incr i
    | ('"' | '\'') as q -> i := skip_quoted sel hi (!i + 1) q - 1
    | '(' | '[' -> incr depth
    | ')' | ']' -> decr depth
    | ',' when !depth = 0 ->
        emit !i;
        start := !i + 1
    | _ -> ());
    incr i
  done;
  emit hi;
  List.rev !out

let layer_statement_keys sheet ~layer =
  let wanted = "@layer " ^ layer in
  let body =
    List.find_map
      (fun (prelude, body) ->
        match body with
        | Some span when normalize_prelude prelude = wanted -> Some span
        | _ -> None)
      (statements_between sheet 0 (String.length sheet))
  in
  match body with
  | None -> []
  | Some (lo, hi) ->
      List.concat_map
        (fun (prelude, _) ->
          let p = normalize_prelude prelude in
          if p = "" then []
          else if p.[0] = '@' then [ p ]
          else selector_branches p)
        (statements_between sheet lo hi)

(* Patience sorting. [tails.(l)] holds the index of the smallest value that can
   end an increasing run of length [l + 1], so a binary search places each
   element and the predecessor links rebuild one longest run. Everything outside
   that run has to move, and nothing smaller does. *)
let longest_increasing_subsequence seq =
  let n = Array.length seq in
  if n = 0 then [||]
  else begin
    let tails = Array.make n 0 and prev = Array.make n (-1) and len = ref 0 in
    for i = 0 to n - 1 do
      let lo = ref 0 and hi = ref !len in
      while !lo < !hi do
        let mid = (!lo + !hi) / 2 in
        if seq.(tails.(mid)) < seq.(i) then lo := mid + 1 else hi := mid
      done;
      let l = !lo in
      prev.(i) <- (if l > 0 then tails.(l - 1) else -1);
      tails.(l) <- i;
      if l = !len then incr len
    done;
    let out = Array.make !len 0 and k = ref tails.(!len - 1) in
    for j = !len - 1 downto 0 do
      out.(j) <- !k;
      k := prev.(!k)
    done;
    out
  end

type order_gap = { pairs : int; moves : int; moved : (string * int * int) list }

let sheet_order_gap ~layer ~tailwind ~tw =
  let ours = layer_statement_keys tw ~layer in
  let theirs = layer_statement_keys tailwind ~layer in
  let occurrences keys =
    let tbl = Hashtbl.create 4096 in
    List.iter
      (fun k ->
        let n = Option.value ~default:0 (Hashtbl.find_opt tbl k) in
        Hashtbl.replace tbl k (n + 1))
      keys;
    tbl
  in
  let ours_count = occurrences ours and theirs_count = occurrences theirs in
  let ours_at = Hashtbl.create 4096 in
  List.iteri
    (fun i k -> if not (Hashtbl.mem ours_at k) then Hashtbl.add ours_at k i)
    ours;
  (* Only a key occurring exactly once on each side pairs without a choice, so
     no pairing decision of the gate's can inflate or deflate what follows. *)
  let common =
    List.filter
      (fun k ->
        Hashtbl.find_opt ours_count k = Some 1
        && Hashtbl.find_opt theirs_count k = Some 1)
      theirs
    |> Array.of_list
  in
  let seq = Array.map (fun k -> Hashtbl.find ours_at k) common in
  let keep = longest_increasing_subsequence seq in
  let kept = Hashtbl.create (Array.length keep) in
  Array.iter (fun i -> Hashtbl.replace kept i ()) keep;
  (* Rank inside the paired set on each side: a byte offset means nothing to a
     reader, and a full-sheet index counts statements the pairing dropped. *)
  let rank = Hashtbl.create (Array.length common) in
  Array.to_list common
  |> List.mapi (fun i k -> (seq.(i), k))
  |> List.sort compare
  |> List.iteri (fun r (_, k) -> Hashtbl.replace rank k r);
  let moved = ref [] in
  Array.iteri
    (fun i k ->
      if not (Hashtbl.mem kept i) then
        moved := (k, i, Hashtbl.find rank k) :: !moved)
    common;
  {
    pairs = Array.length common;
    moves = Array.length common - Array.length keep;
    moved = List.rev !moved;
  }

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

(* ------------------------------------------------------------------ *)
(* Adversarial sweep over arbitrary values                             *)
(* ------------------------------------------------------------------ *)

let rec all_selectors stmts =
  List.concat_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, _, _) -> [ Css.Selector.to_string selector ]
      | None -> (
          match Css.as_media stmt with
          | Some (_, inner) -> all_selectors inner
          | None -> (
              match Css.as_supports stmt with
              | Some (_, inner) -> all_selectors inner
              | None -> (
                  match Css.as_container stmt with
                  | Some (_, _, inner) -> all_selectors inner
                  | None -> (
                      match Css.as_layer stmt with
                      | Some (_, inner) -> all_selectors inner
                      | None -> [])))))
    stmts

let selectors_of_utility u =
  all_selectors (Css.statements (Tw.to_css ~base:false [ u ]))

(* Undo CSS Syntax 3 (ED) sec. 4.3.7 escaping so an emitted selector can be
   compared against the class the author wrote. A backslash escapes either one
   to six hex digits naming a code point, with an optional single whitespace
   closing the run, or the next character literally. Comparing this way rather
   than re-escaping the class keeps the check independent of which spelling
   cascade picks: [caf\\e9 ] and [café] are the same selector. *)
let unescape_selector s =
  let n = String.length s in
  let buf = Buffer.create n in
  let is_hex c =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  in
  let rec go i =
    if i >= n then ()
    else if s.[i] <> '\\' || i + 1 >= n then (
      Buffer.add_char buf s.[i];
      go (i + 1))
    else
      let j = ref (i + 1) in
      while !j < n && !j - i <= 6 && is_hex s.[!j] do
        incr j
      done;
      if !j = i + 1 then (
        (* a literal escape: the next character stands for itself *)
        Buffer.add_char buf s.[i + 1];
        go (i + 2))
      else
        let code = int_of_string ("0x" ^ String.sub s (i + 1) (!j - i - 1)) in
        let code =
          if Uchar.is_valid code then code else Uchar.to_int Uchar.rep
        in
        Buffer.add_utf_8_uchar buf (Uchar.of_int code);
        (* one whitespace may close the hex run and is not part of the text *)
        let k =
          if !j < n && (s.[!j] = ' ' || s.[!j] = '\n' || s.[!j] = '\t') then
            !j + 1
          else !j
        in
        go k
  in
  go 0;
  Buffer.contents buf

let adversarial_payloads =
  [
    (* a number whose canonical spelling is not the author's *)
    "0.5ch";
    "1.50px";
    "1e2px";
    "1E2px";
    "1e-2px";
    "0.0";
    ".5px";
    "+1px";
    "0600px";
    (* text that ends the declaration it is written into, or the rule *)
    "0)/*1";
    "a;b";
    "a}b";
    "a{b";
    "1px}";
    "var(--x);";
    "1px;color:red";
    "}.x{color:red";
    "0)";
    "(";
    "()";
    "url(a;b)";
    (* a comment, quotes, and text no CSS grammar reads *)
    "1px/*x";
    "*/";
    {|"q"|};
    "'q'";
    "--custom";
    "<value>";
    "a:b";
    "#";
    "&";
    "a\\b";
    (* a non-ASCII identifier *)
    "caf\xc3\xa9";
    "\xc3\xbcnicode";
    (* a bare word that also names a variant shorthand, so a family which folds
       the bracket onto the shorthand loses the brackets from the class *)
    "hover";
  ]

type sweep_verdict =
  | Rejected  (** [of_string] refused the class: a legitimate outcome *)
  | Emitted_nothing  (** parsed, but contributed no rule *)
  | Matched  (** parsed, and every rule it emits is selected by the class *)
  | Mismatched of string
      (** parsed, and emitted a rule the class cannot match *)

let sweep_one cls =
  match Tw.of_string cls with
  | exception e ->
      Alcotest.failf "%s: of_string raised %s" cls (Printexc.to_string e)
  | Error (`Msg _) -> Rejected
  | Ok u -> (
      match selectors_of_utility u with
      | exception e ->
          Alcotest.failf "%s: to_css raised %s" cls (Printexc.to_string e)
      | [] -> Emitted_nothing
      | selectors ->
          let printed = Tw.pp u in
          let carries sel =
            Astring.String.is_infix ~affix:cls (unescape_selector sel)
          in
          let detail suffix =
            Mismatched
              (String.concat ""
                 [
                   suffix;
                   " (pp=";
                   printed;
                   ", selectors=";
                   String.concat "; " selectors;
                   ")";
                 ])
          in
          if not (List.exists carries selectors) then
            detail "no emitted selector carries the class"
          else if printed <> cls then detail "pp respells the class"
          else Matched)

(* Every class prefix that takes a bracket value, found by feeding a benign one
   to each [val] exported by [lib/tw.mli] and by the family modules it
   re-exports, and to each literal match-arm prefix in [lib/*.ml] - the string
   parser accepts families no OCaml constructor names ([filter-],
   [drop-shadow-]) so the [.mli] alone does not see them. *)
let arbitrary_families =
  [
    "accent";
    "align";
    "animate";
    "aspect";
    "auto-cols";
    "auto-rows";
    "backdrop-blur";
    "backdrop-brightness";
    "backdrop-contrast";
    "backdrop-filter";
    "backdrop-grayscale";
    "backdrop-hue-rotate";
    "backdrop-invert";
    "backdrop-opacity";
    "backdrop-saturate";
    "backdrop-sepia";
    "basis";
    "bg";
    "bg-linear";
    "bg-position";
    "bg-radial";
    "bg-size";
    "block";
    "blur";
    "border";
    "border-b";
    "border-be";
    "border-bs";
    "border-e";
    "border-l";
    "border-r";
    "border-s";
    "border-spacing";
    "border-spacing-x";
    "border-spacing-y";
    "border-t";
    "border-x";
    "border-y";
    "bottom";
    "brightness";
    "caret";
    "col";
    "col-end";
    "col-span";
    "col-start";
    "columns";
    "contain";
    "content";
    "contrast";
    "cursor";
    "decoration";
    "delay";
    "divide";
    "divide-x";
    "divide-y";
    "drop-shadow";
    "duration";
    "ease";
    "fill";
    "filter";
    "flex";
    "font";
    "font-features";
    "from";
    "gap";
    "gap-x";
    "gap-y";
    "grayscale";
    "grid-cols";
    "grid-rows";
    "grow";
    "h";
    "hue-rotate";
    "indent";
    "inline";
    "inset";
    "inset-be";
    "inset-bs";
    "inset-e";
    "inset-ring";
    "inset-s";
    "inset-shadow";
    "inset-x";
    "inset-y";
    "invert";
    "leading";
    "left";
    "line-clamp";
    "list";
    "list-image";
    "list-image-none";
    "list-image-url";
    "m";
    "mask";
    "mask-b-from";
    "mask-b-to";
    "mask-conic";
    "mask-conic-from";
    "mask-conic-to";
    "mask-l-from";
    "mask-l-to";
    "mask-linear";
    "mask-linear-from";
    "mask-linear-to";
    "mask-position";
    "mask-r-from";
    "mask-r-to";
    "mask-radial";
    "mask-radial-at";
    "mask-radial-from";
    "mask-radial-to";
    "mask-size";
    "mask-t-from";
    "mask-t-to";
    "mask-x-from";
    "mask-x-to";
    "mask-y-from";
    "mask-y-to";
    "max-block";
    "max-h";
    "max-inline";
    "max-w";
    "mb";
    "min-block";
    "min-h";
    "min-inline";
    "min-w";
    "ml";
    "mr";
    "mt";
    "mx";
    "my";
    "object";
    "opacity";
    "order";
    "origin";
    "outline";
    "outline-offset";
    "p";
    "pb";
    "perspective";
    "perspective-origin";
    "pl";
    "placeholder";
    "pr";
    "pt";
    "px";
    "py";
    "right";
    "ring";
    "ring-offset";
    "rotate";
    "rotate-x";
    "rotate-y";
    "rotate-z";
    "rounded";
    "rounded-b";
    "rounded-bl";
    "rounded-br";
    "rounded-e";
    "rounded-ee";
    "rounded-es";
    "rounded-l";
    "rounded-r";
    "rounded-s";
    "rounded-se";
    "rounded-ss";
    "rounded-t";
    "rounded-tl";
    "rounded-tr";
    "row";
    "row-end";
    "row-span";
    "row-start";
    "saturate";
    "scale";
    "scale-x";
    "scale-y";
    "scale-z";
    "scroll-m";
    "scroll-mb";
    "scroll-mbe";
    "scroll-mbs";
    "scroll-me";
    "scroll-ml";
    "scroll-mr";
    "scroll-ms";
    "scroll-mt";
    "scroll-mx";
    "scroll-my";
    "scroll-p";
    "scroll-pb";
    "scroll-pbe";
    "scroll-pbs";
    "scroll-pe";
    "scroll-pl";
    "scroll-pr";
    "scroll-ps";
    "scroll-pt";
    "scroll-px";
    "scroll-py";
    "sepia";
    "shadow";
    "shrink";
    "size";
    "skew";
    "skew-x";
    "skew-y";
    "space-x";
    "space-y";
    "stroke";
    "tab";
    "text";
    "text-shadow";
    "to";
    "top";
    "tracking";
    "transform";
    "transition";
    "translate";
    "translate-x";
    "translate-y";
    "underline-offset";
    "via";
    "w";
    "will-change";
    "z";
    "zoom";
  ]
