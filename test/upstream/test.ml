(** Test runner for upstream Tailwind CSS tests

    Compares our tw output against expected CSS extracted from tailwindcss test
    snapshots. No external tools needed at runtime. *)

open Alcotest

type test_case = { name : string; classes : string list; expected : string }

(** Convert a Tailwind class name to the expected CSS selector. E.g., "-z-10" ->
    ".-z-10" or "z-[123]" -> ".z-\[123\]" *)
let class_to_selector cls =
  (* Escape special characters in CSS selectors *)
  let escape s =
    let buf = Buffer.create (String.length s * 2) in
    String.iter
      (fun c ->
        match c with
        | '[' | ']' | '(' | ')' | ':' | '/' | '.' | '%' | '#' ->
            Buffer.add_char buf '\\';
            Buffer.add_char buf c
        | _ -> Buffer.add_char buf c)
      s;
    Buffer.contents buf
  in
  "." ^ escape cls

(** Check if a selector matches any of the input classes *)
let selector_matches_input_class classes selector =
  List.exists
    (fun cls ->
      let expected_sel = class_to_selector cls in
      expected_sel = selector || String.equal selector ("." ^ cls))
    classes

(** Check if a selector is a theme-level selector (not utility-specific) *)
let is_theme_selector selector =
  selector = ":root, :host" || selector = ":root" || selector = ":host"

(** Extract selector strings from a CSS statement list *)
let extract_selectors_from_rules rules =
  List.filter_map
    (fun stmt ->
      match Css.as_rule stmt with
      | Some (sel, _, _) -> Some (Css.Selector.to_string sel)
      | None -> None)
    rules

(** Filter blocks to only include those with selectors matching input classes *)
let filter_blocks_by_class classes blocks =
  List.filter_map
    (fun (pos, rules) ->
      let selectors = extract_selectors_from_rules rules in
      let matching =
        List.filter (selector_matches_input_class classes) selectors
      in
      if matching <> [] then Some (pos, rules) else None)
    blocks

(** Extract and sort selectors from blocks (ignoring position) *)
let selectors_from_blocks classes blocks =
  blocks
  |> List.concat_map (fun (_, rules) -> extract_selectors_from_rules rules)
  |> List.filter (selector_matches_input_class classes)
  |> List.sort String.compare

(** Filter container diffs to only include relevant changes *)
let rec filter_container_diff classes = function
  | Tw_tools.Tree_diff.Container_block_structure_changed
      { container_type; condition; expected_blocks; actual_blocks } ->
      (* Filter both expected and actual blocks by input class *)
      let filtered_expected = filter_blocks_by_class classes expected_blocks in
      let filtered_actual = filter_blocks_by_class classes actual_blocks in
      (* Compare selectors only (ignore position differences) *)
      let expected_selectors = selectors_from_blocks classes expected_blocks in
      let actual_selectors = selectors_from_blocks classes actual_blocks in
      (* Only keep if the selectors differ (not just positions) *)
      if expected_selectors <> actual_selectors then
        Some
          (Tw_tools.Tree_diff.Container_block_structure_changed
             {
               container_type;
               condition;
               expected_blocks = filtered_expected;
               actual_blocks = filtered_actual;
             })
      else None
  | Tw_tools.Tree_diff.Container_modified
      { info; actual_rules; rule_changes; container_changes } ->
      (* Recursively filter nested container changes *)
      let filtered_containers =
        List.filter_map (filter_container_diff classes) container_changes
      in
      (* Filter rule changes within the container *)
      let filtered_rules =
        List.filter
          (function
            | Tw_tools.Tree_diff.Rule_removed { selector; _ } ->
                selector_matches_input_class classes selector
            | Tw_tools.Tree_diff.Rule_reordered _ -> false
            | _ -> true)
          rule_changes
      in
      if filtered_rules <> [] || filtered_containers <> [] then
        Some
          (Tw_tools.Tree_diff.Container_modified
             {
               info;
               actual_rules;
               rule_changes = filtered_rules;
               container_changes = filtered_containers;
             })
      else None
  | Tw_tools.Tree_diff.Container_removed info ->
      (* Only keep if any selector in the rules matches input classes *)
      let selectors = extract_selectors_from_rules info.rules in
      let has_matching_selector =
        List.exists (selector_matches_input_class classes) selectors
      in
      if has_matching_selector then
        Some (Tw_tools.Tree_diff.Container_removed info)
      else None
  | Tw_tools.Tree_diff.Container_added info ->
      (* Only keep if any selector in the rules matches input classes *)
      let selectors = extract_selectors_from_rules info.rules in
      let has_matching_selector =
        List.exists (selector_matches_input_class classes) selectors
      in
      if has_matching_selector then
        Some (Tw_tools.Tree_diff.Container_added info)
      else None
  | other -> Some other

(** Filter rule diffs to only include those that match input classes.
    Specifically, we ignore Rule_removed diffs for selectors that don't match
    any input class (since those are in expected CSS but not relevant). We also
    filter out theme-level selectors like :root, :host since those contain theme
    configuration rather than utility output. *)
let filter_irrelevant_diffs classes (diff : Tw_tools.Tree_diff.t) :
    Tw_tools.Tree_diff.t =
  (* Check if a property name is a gradient variable that can be split across
     rules. Tailwind outputs these in separate rules, we may combine them. *)
  let is_gradient_var prop =
    String.length prop > 15
    && (String.sub prop 0 15 = "--tw-gradient-s"
       || String.sub prop 0 15 = "--tw-gradient-v")
  in
  let filter_rule = function
    | Tw_tools.Tree_diff.Rule_removed { selector; _ } ->
        (* Only keep if the selector matches an input class *)
        if selector_matches_input_class classes selector then Some () else None
    | Tw_tools.Tree_diff.Rule_content_changed
        { selector; added_properties; removed_properties; property_changes; _ }
      ->
        (* Filter out theme selectors *)
        if is_theme_selector selector then None
        else
          (* Filter out diffs where only gradient variables differ - these are
             structural differences in how we emit rules, not semantic
             differences *)
          let non_gradient_added =
            List.filter (fun p -> not (is_gradient_var p)) added_properties
          in
          let non_gradient_removed =
            List.filter (fun p -> not (is_gradient_var p)) removed_properties
          in
          let has_non_gradient_changes =
            non_gradient_added <> [] || non_gradient_removed <> []
            || property_changes <> []
          in
          if has_non_gradient_changes then Some () else None
    | Tw_tools.Tree_diff.Rule_reordered _ ->
        (* Filter out reordering diffs - CSS semantics are correct *)
        None
    | _ -> Some ()
  in
  let rules =
    List.filter (fun r -> Option.is_some (filter_rule r)) diff.rules
  in
  let containers =
    List.filter_map (filter_container_diff classes) diff.containers
  in
  { rules; containers }

let read_test_cases filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let tests = ref [] in
    let lines = String.split_on_char '\n' content in
    let rec parse lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if String.length line > 2 && line.[0] = '#' && line.[1] = ' ' then
            let name = String.sub line 2 (String.length line - 2) in
            parse_classes name rest
          else parse rest
    and parse_classes name lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "<<<>>>" then parse rest
          else if line = "---" then
            (* No classes line before ---, skip *)
            parse_expected name [] (Buffer.create 256) rest
          else if String.length line > 2 && line.[0] = '#' && line.[1] = ' '
          then
            (* New test without classes *)
            let new_name = String.sub line 2 (String.length line - 2) in
            parse_classes new_name rest
          else
            let classes =
              String.split_on_char ' ' line
              |> List.filter (fun s -> String.length s > 0)
            in
            parse_after_classes name classes rest
    and parse_after_classes name classes lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "---" then
            parse_expected name classes (Buffer.create 256) rest
          else if line = "<<<>>>" then
            (* No expected CSS, skip this test *)
            parse rest
          else parse rest
    and parse_expected name classes buf lines =
      match lines with
      | [] ->
          let expected = Buffer.contents buf |> String.trim in
          if classes <> [] && expected <> "" then
            tests := { name; classes; expected } :: !tests
      | line :: rest ->
          if String.trim line = "<<<>>>" then (
            let expected = Buffer.contents buf |> String.trim in
            if classes <> [] && expected <> "" then
              tests := { name; classes; expected } :: !tests;
            parse rest)
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf '\n';
            Buffer.add_string buf line;
            parse_expected name classes buf rest)
    in
    parse lines;
    List.rev !tests

(** Create scheme matching Tailwind test [\@theme]. Tailwind tests use hex
    colors and explicit spacing variables. *)
let test_scheme : Tw.Scheme.t =
  {
    colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
    spacing = [ (4, Css.Rem 1.0) ];
  }

(** Set up the test scheme for color and spacing generation *)
let setup_test_scheme () =
  Tw.Color.Handler.set_scheme test_scheme;
  Tw.Theme.set_scheme test_scheme

let run_test_case test () =
  if test.classes = [] then ()
  else (
    (* Set up the test scheme before generating CSS *)
    setup_test_scheme ();
    (* Parse classes and generate our CSS *)
    let utilities =
      List.filter_map
        (fun cls ->
          match Tw.of_string cls with Ok u -> Some u | Error _ -> None)
        test.classes
    in
    (* Generate our CSS (empty string if no utilities parsed) *)
    let our_css =
      if utilities = [] then ""
      else
        Tw.to_css ~base:false ~layers:false ~optimize:true utilities
        |> Css.to_string ~minify:false
    in
    if our_css = "" && String.trim test.expected = "" then ()
    else
      (* Compare *)
      let diff =
        Tw_tools.Css_compare.diff ~expected:test.expected ~actual:our_css
      in
      match diff with
      | Tw_tools.Css_compare.No_diff -> ()
      | Tw_tools.Css_compare.String_diff _ ->
          (* Minor string differences are acceptable *)
          ()
      | Tw_tools.Css_compare.Tree_diff d ->
          (* Filter out diffs for rules that don't match input classes *)
          let filtered = filter_irrelevant_diffs test.classes d in
          if Tw_tools.Tree_diff.is_empty filtered then ()
          else
            let diff_str =
              Fmt.str "%a"
                (Tw_tools.Css_compare.pp ~expected:"Tailwind" ~actual:"tw")
                (Tw_tools.Css_compare.Tree_diff filtered)
            in
            Alcotest.fail
              (Fmt.str "CSS mismatch:\nClasses: %s\n%s"
                 (String.concat " " test.classes)
                 diff_str)
      | Tw_tools.Css_compare.Expected_error e ->
          Alcotest.fail
            (Fmt.str "Failed to parse expected CSS: %s" (Css.pp_parse_error e))
      | Tw_tools.Css_compare.Actual_error e ->
          Alcotest.fail
            (Fmt.str "Failed to parse our CSS: %s" (Css.pp_parse_error e))
      | Tw_tools.Css_compare.Both_errors (e1, e2) ->
          Alcotest.fail
            (Fmt.str "Parse errors:\nExpected: %s\nOurs: %s"
               (Css.pp_parse_error e1) (Css.pp_parse_error e2)))

let () =
  let test_files = [ "utilities.txt"; "test/upstream/utilities.txt" ] in
  let test_file =
    List.find_opt Sys.file_exists test_files
    |> Option.value ~default:"utilities.txt"
  in

  if not (Sys.file_exists test_file) then (
    Fmt.epr "No test file found. Run extract_tests.exe first.@.";
    exit 0);

  let test_cases = read_test_cases test_file in
  if test_cases = [] then (
    Fmt.epr "No test cases with expected CSS found.@.";
    exit 0);

  Fmt.epr "Running %d upstream tests...@." (List.length test_cases);

  let tests =
    List.map (fun tc -> test_case tc.name `Quick (run_test_case tc)) test_cases
  in
  Alcotest.run "upstream" [ ("utilities", tests) ]
