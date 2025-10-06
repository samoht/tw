(** Test helper functions for CSS comparison and minimization *)

(** Extract utilities layer from CSS *)
let extract_utilities_layer_rules css =
  let stmts = Css.statements css in
  List.find_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some "utilities", rules) -> Some rules
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

(** Check if utilities produce different ordering than Tailwind *)
let check_ordering_fails ?(forms = false) utilities =
  let classnames = List.map Tw.pp utilities in
  let tw_css = Tw.to_css ~base:true ~optimize:true utilities in
  let tw_utilities_rules = extract_utilities_layer_rules tw_css in
  let tw_order = extract_rule_selectors tw_utilities_rules in

  let tailwind_css_str =
    Tw_tools.Tailwind_gen.generate ~minify:true ~optimize:true ~forms classnames
  in
  let tailwind_css =
    match Css.of_string tailwind_css_str with
    | Ok css -> css
    | Error _ -> failwith "Failed to parse Tailwind CSS"
  in
  let tailwind_utilities_rules = extract_utilities_layer_rules tailwind_css in
  let tailwind_order = extract_rule_selectors tailwind_utilities_rules in

  tw_order <> tailwind_order

(** Delta Debugging (ddmin algorithm by Zeller) Minimizes a failing test case by
    binary search *)
let delta_debug check_fails lst =
  let rec ddmin lst n =
    let len = List.length lst in
    if len = 1 then lst
    else
      (* Split into n subsets *)
      let subset_size = max 1 (len / n) in
      let rec make_subsets acc remaining =
        if remaining = [] then List.rev acc
        else
          let rec take k l =
            match (k, l) with
            | 0, _ | _, [] -> ([], l)
            | k, x :: xs ->
                let taken, rest = take (k - 1) xs in
                (x :: taken, rest)
          in
          let subset, rest = take subset_size remaining in
          make_subsets (subset :: acc) rest
      in
      let subsets = make_subsets [] lst in

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
let find_minimal_pair check_fails lst =
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
        match find_minimal_pair check_fails minimal with
        | Some pair -> pair
        | None -> minimal
      else minimal
    in
    Some final

(** Get rule selector ordering from Tailwind CSS *)
let get_tailwind_order ?(forms = false) classes =
  let tailwind_css_str =
    Tw_tools.Tailwind_gen.generate ~minify:true ~optimize:true ~forms classes
  in
  let tailwind_css =
    match Css.of_string tailwind_css_str with
    | Ok css -> css
    | Error err ->
        let formatted_error = Css.pp_parse_error err in
        Alcotest.fail ("Failed to parse Tailwind CSS: " ^ formatted_error)
  in
  let tailwind_utilities_rules = extract_utilities_layer_rules tailwind_css in
  extract_rule_selectors tailwind_utilities_rules

(** Get rule selector ordering from our implementation *)
let get_our_order utilities =
  let tw_css = Tw.to_css ~base:true ~optimize:true utilities in
  let tw_utilities_rules = extract_utilities_layer_rules tw_css in
  extract_rule_selectors tw_utilities_rules

(** Compare ordering between our implementation and Tailwind *)
let check_ordering_matches ?(forms = false) ~test_name utilities =
  let classes = List.map Tw.pp utilities in
  let tailwind_order = get_tailwind_order ~forms classes in
  let our_order = get_our_order utilities in

  (* If ordering doesn't match, try to minimize the test case *)
  if tailwind_order <> our_order then (
    Fmt.epr "@.Ordering mismatch detected. Minimizing test case...@.";
    match minimize_failing_case (check_ordering_fails ~forms) utilities with
    | Some minimal ->
        let minimal_classes = List.map Tw.pp minimal in
        Fmt.epr "@.Minimal failing case (%d utilities): %a@."
          (List.length minimal)
          Fmt.(list ~sep:(const string " ") string)
          minimal_classes;
        let minimal_tw_order = get_tailwind_order ~forms minimal_classes in
        let minimal_our_order = get_our_order minimal in
        Fmt.epr "@.Expected (Tailwind): %a@."
          Fmt.(list ~sep:(const string ", ") (quote string))
          minimal_tw_order;
        Fmt.epr "Received (tw):       %a@.@."
          Fmt.(list ~sep:(const string ", ") (quote string))
          minimal_our_order;
        (* Fail with minimal test case for clearer error message *)
        Alcotest.(check (list string))
          test_name minimal_tw_order minimal_our_order
    | None ->
        (* No minimization possible, fail with full test case *)
        Alcotest.(check (list string)) test_name tailwind_order our_order)
  else
    (* Test passes *)
    Alcotest.(check (list string)) test_name tailwind_order our_order

(** CSS Test Helpers *)

(** Check if a layer exists in the stylesheet *)
let has_layer name css =
  List.exists
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some layer_name, _) when layer_name = name -> true
      | _ -> false)
    (Css.statements css)

(** Get all custom property names from a layer *)
let vars_in_layer layer_name css = Css.custom_props ~layer:layer_name css

(** Check if a variable name exists in a layer *)
let has_var_in_layer var_name layer_name css =
  let vars = vars_in_layer layer_name css in
  List.exists (fun v -> v = var_name) vars

(** Get all selectors from a layer *)
let selectors_in_layer layer_name css =
  match Css.layer_block layer_name css with
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
      match Css.as_media stmt with Some (cond, _) -> cond :: acc | None -> acc)
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
      | None, Some (cond, inner) when String.equal cond condition -> Some inner
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

let has_selector_in_media_sel ~condition ~selector css =
  selectors_in_media_sel ~condition css
  |> List.exists (fun sel -> sel = selector)

let count_selector_in_media_sel ~condition ~selector css =
  selectors_in_media_sel ~condition css
  |> List.fold_left (fun acc sel -> if sel = selector then acc + 1 else acc) 0

(** Check if inline style contains a specific property *)
let inline_has_property prop_name inline_style =
  String.split_on_char ';' inline_style
  |> List.exists (fun prop ->
         String.trim prop |> String.split_on_char ':' |> function
         | prop :: _ -> String.trim prop = prop_name
         | [] -> false)

(** Check if declarations contain any var() references *)
let has_var_in_declarations ?(inline = false) decls =
  List.exists
    (fun decl ->
      let value = Css.declaration_value ~inline decl in
      String.length value >= 4 && String.sub value 0 4 = "var(")
    decls

(** {1 Utility Generators} *)

(** Common spacing values used in Tailwind *)
let spacing_values =
  [ 0; 1; 2; 3; 4; 5; 6; 8; 10; 12; 16; 20; 24; 32; 40; 48; 64 ]

(** Shuffle a list in place using Fisher-Yates algorithm *)
let shuffle lst =
  let arr = Array.of_list lst in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr

(** {1 Generic Test Patterns} *)

module type Handler = sig
  type t

  val of_class : string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val to_style : t -> Tw.Style.t
end

(** Generic handler test - checks that parsing and pretty-printing round-trip
    correctly. Takes a class name string, parses it with of_class, converts back
    with to_class, and verifies they match. *)
let check_handler_roundtrip (module H : Handler) class_name =
  (* Test of_class -> to_class roundtrip *)
  match H.of_class class_name with
  | Ok result ->
      let class_name2 = H.to_class result in
      Alcotest.(check string)
        "of_class -> to_class roundtrip" class_name class_name2
  | Error (`Msg msg) ->
      Alcotest.fail ("of_class failed for '" ^ class_name ^ "': " ^ msg)

(** Generic test for invalid inputs - expects parsing to fail *)
let check_invalid_input (module H : Handler) input =
  match H.of_class input with
  | Ok _ -> Alcotest.fail ("Expected error for: " ^ input)
  | Error _ -> ()

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
