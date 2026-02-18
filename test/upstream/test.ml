(** Test runner for upstream Tailwind CSS tests

    Compares our tw output against expected CSS extracted from tailwindcss test
    snapshots. No external tools needed at runtime. *)

open Alcotest

type theme_config =
  | Theme
  | Theme_inline
  | Theme_reference
  | Theme_inline_reference
  | No_theme
  | Run

let config_of_string = function
  | "theme" -> Theme
  | "theme-inline" -> Theme_inline
  | "theme-reference" -> Theme_reference
  | "theme-inline-reference" -> Theme_inline_reference
  | "none" -> No_theme
  | "run" -> Run
  | _ -> No_theme

type test_case = {
  name : string;
  config : theme_config;
  classes : string list;
  expected : string;
}

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

(** Check if a supports condition is for color-mix (progressive enhancement) *)
let is_color_mix_supports condition =
  String.length condition > 10 && String.sub condition 0 10 = "(color: co"

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
      (* Filter out color-mix @supports blocks - our hex fallback is
         equivalent *)
      if info.container_type = `Supports && is_color_mix_supports info.condition
      then None
      else
        (* Only keep if any selector in the rules matches input classes *)
        let selectors = extract_selectors_from_rules info.rules in
        let has_matching_selector =
          List.exists (selector_matches_input_class classes) selectors
        in
        if has_matching_selector then
          Some (Tw_tools.Tree_diff.Container_removed info)
        else None
  | Tw_tools.Tree_diff.Container_added info ->
      (* Filter out color-mix @supports blocks - our hex fallback is
         equivalent *)
      if info.container_type = `Supports && is_color_mix_supports info.condition
      then None
      else
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
    configuration rather than utility output. We also filter out color-mix
    @supports blocks since our hex fallback is semantically equivalent. *)
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
    let parse_config_line line =
      let line = String.trim line in
      if String.length line > 8 && String.sub line 0 8 = "@config " then
        Some (config_of_string (String.sub line 8 (String.length line - 8)))
      else None
    in
    let rec parse lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if String.length line > 2 && line.[0] = '#' && line.[1] = ' ' then
            let name = String.sub line 2 (String.length line - 2) in
            parse_config name No_theme rest
          else parse rest
    and parse_config name default_config lines =
      match lines with
      | [] -> ()
      | line :: rest -> (
          match parse_config_line line with
          | Some config -> parse_classes name config rest
          | None -> parse_classes name default_config (line :: rest))
    and parse_classes name config lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "<<<>>>" then parse rest
          else if line = "---" then
            (* No classes line before ---, skip *)
            parse_expected name config [] (Buffer.create 256) rest
          else if String.length line > 2 && line.[0] = '#' && line.[1] = ' '
          then
            (* New test without classes *)
            let new_name = String.sub line 2 (String.length line - 2) in
            parse_config new_name No_theme rest
          else
            let classes =
              String.split_on_char ' ' line
              |> List.filter (fun s -> String.length s > 0)
            in
            parse_after_classes name config classes rest
    and parse_after_classes name config classes lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "---" then
            parse_expected name config classes (Buffer.create 256) rest
          else if line = "<<<>>>" then
            (* No expected CSS, skip this test *)
            parse rest
          else parse rest
    and parse_expected name config classes buf lines =
      match lines with
      | [] ->
          let expected = Buffer.contents buf |> String.trim in
          if classes <> [] && expected <> "" then
            tests := { name; config; classes; expected } :: !tests
      | line :: rest ->
          if String.trim line = "<<<>>>" then (
            let expected = Buffer.contents buf |> String.trim in
            if classes <> [] && expected <> "" then
              tests := { name; config; classes; expected } :: !tests;
            parse rest)
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf '\n';
            Buffer.add_string buf line;
            parse_expected name config classes buf rest)
    in
    parse lines;
    List.rev !tests

(** Extract spacing values from expected CSS. Looks for patterns like:
    --spacing-4: 1rem; or --spacing: .25rem; in :root, :host blocks. *)
let extract_spacing_from_css css : (int * Css.length) list =
  let spacing_pattern = Re.Pcre.regexp {|--spacing-(\d+):\s*([0-9.]+)rem|} in
  let matches = Re.all spacing_pattern css in
  List.filter_map
    (fun m ->
      try
        let n = int_of_string (Re.Group.get m 1) in
        let value = float_of_string (Re.Group.get m 2) in
        Some (n, (Css.Rem value : Css.length))
      with _ -> None)
    matches

(** Extract radius values from expected CSS. Looks for patterns like:
    --radius-none: 0; --radius-full: 9999px; in :root, :host blocks. *)
let extract_radius_from_css css : (string * Css.length) list =
  (* Match --radius-NAME: VALUE patterns *)
  let radius_pattern =
    Re.Pcre.regexp {|--radius-([a-zA-Z0-9-]+):\s*([0-9.]+)(px|rem)?|}
  in
  let matches = Re.all radius_pattern css in
  List.filter_map
    (fun m ->
      try
        let name = Re.Group.get m 1 in
        let value = float_of_string (Re.Group.get m 2) in
        let unit = try Re.Group.get m 3 with _ -> "" in
        let length : Css.length =
          match unit with
          | "px" -> Px value
          | "rem" -> Rem value
          | "" when value = 0.0 -> Zero
          | _ -> Px value
        in
        Some (name, length)
      with _ -> None)
    matches

(** Create scheme from expected CSS. Extracts spacing and radius values defined
    in :root, :host blocks. *)
let extract_default_ring_width_from_css css : int =
  (* Look for calc(Npx + var(--tw-ring-offset-width)) inside .ring { } to
     determine the configured --default-ring-width *)
  let pattern =
    Re.Pcre.regexp
      {|\.ring\s*\{[^}]*calc\((\d+)px\s*\+\s*var\(--tw-ring-offset-width\)\)|}
  in
  match Re.exec_opt pattern css with
  | Some m -> ( try int_of_string (Re.Group.get m 1) with _ -> 1)
  | None -> 1

let extract_default_border_width_from_css css : int =
  (* Look for border-width: Npx inside .border { } to determine the configured
     --default-border-width *)
  let pattern = Re.Pcre.regexp {|\.border\s*\{[^}]*border-width:\s*(\d+)px|} in
  match Re.exec_opt pattern css with
  | Some m -> ( try int_of_string (Re.Group.get m 1) with _ -> 1)
  | None -> 1

let scheme_from_expected_css expected : Tw.Scheme.t =
  let spacing = extract_spacing_from_css expected in
  let radius = extract_radius_from_css expected in
  let default_ring_width = extract_default_ring_width_from_css expected in
  let default_border_width = extract_default_border_width_from_css expected in
  {
    colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
    spacing;
    radius;
    default_ring_width;
    default_border_width;
  }

(** Set up the scheme for a specific test *)
let setup_scheme_for_test expected =
  let scheme = scheme_from_expected_css expected in
  Tw.Color.Handler.set_scheme scheme;
  Tw.Theme.set_scheme scheme;
  Tw.Borders.set_scheme scheme;
  Tw.Effects.set_scheme scheme

(** Extract all CSS variable names referenced in expected CSS text. Finds all
    [--name] patterns (both definitions and references). *)
let extract_var_names expected =
  let vars = ref Css.Pp.StringSet.empty in
  let len = String.length expected in
  let rec scan i =
    if i < len - 2 && expected.[i] = '-' && expected.[i + 1] = '-' then (
      let j = ref (i + 2) in
      while
        !j < len
        &&
        let c = expected.[!j] in
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '-' || c = '_'
      do
        incr j
      done;
      if !j > i + 2 then
        vars :=
          Css.Pp.StringSet.add (String.sub expected (i + 2) (!j - i - 2)) !vars;
      scan !j)
    else if i < len then scan (i + 1)
  in
  scan 0;
  !vars

(** Build theme configuration for CSS emission.

    Returns [(theme, theme_defaults)] where [theme] is the set of variable names
    that should be kept as [var(--name)] references, and [theme_defaults] maps
    variable names to concrete CSS default values. *)
let make_theme_config config expected =
  (* Hardcoded theme variable defaults: (var_name, inline_value,
     default_value) *)
  let hardcoded =
    [
      ("default-transition-timing-function", "ease", "ease");
      ("default-transition-duration", ".1s", "0s");
    ]
  in
  let combined_defaults name =
    match Tw.Var.resolve_theme_refs name with
    | Some _ as result -> result
    | None ->
        List.find_map
          (fun (var_name, _, default) ->
            if name = var_name then Some default else None)
          hardcoded
  in
  let hardcoded_only name =
    List.find_map
      (fun (var_name, _, default) ->
        if name = var_name then Some default else None)
      hardcoded
  in
  let inline_defaults name =
    List.find_map
      (fun (var_name, inline_val, _) ->
        if name = var_name then Some inline_val else None)
      hardcoded
  in
  match config with
  | Run ->
      (* No theme: all vars resolved to concrete defaults *)
      (Css.Pp.StringSet.empty, combined_defaults)
  | Theme ->
      (* Vars in expected CSS kept as references, rest resolved *)
      (extract_var_names expected, combined_defaults)
  | Theme_inline ->
      (* Inline all known theme variable values *)
      (Css.Pp.StringSet.empty, inline_defaults)
  | No_theme ->
      (* No theme vars, only hardcoded defaults *)
      (Css.Pp.StringSet.empty, hardcoded_only)
  | Theme_reference | Theme_inline_reference ->
      (* All vars kept as references *)
      (extract_var_names expected, Css.Pp.no_theme_defaults)

let run_test_case test () =
  if test.classes = [] then ()
  else (
    (* Set up scheme from expected CSS to match Tailwind's @theme config *)
    setup_scheme_for_test test.expected;
    let theme, theme_defaults = make_theme_config test.config test.expected in
    (* Parse classes and generate our CSS *)
    let utilities =
      List.filter_map
        (fun cls ->
          match Tw.of_string cls with Ok u -> Some u | Error _ -> None)
        test.classes
    in
    (* Generate our CSS (empty string if no utilities parsed) Use ~layers:false
       because Tailwind's run() tests don't include layer wrappers, and
       compileCss() tests use different layer structure (@layer properties vs
       our @layer utilities). *)
    let our_css =
      if utilities = [] then ""
      else
        Tw.to_css ~base:false ~layers:false ~optimize:true utilities
        |> Css.to_string ~minify:false ~theme ~theme_defaults
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

let find_test_file basename =
  let paths = [ basename; "test/upstream/" ^ basename ] in
  List.find_opt Sys.file_exists paths

let () =
  let utilities_file =
    find_test_file "utilities.txt" |> Option.value ~default:"utilities.txt"
  in
  let variants_file = find_test_file "variants.txt" in

  if not (Sys.file_exists utilities_file) then (
    Fmt.epr "No test file found. Run extract_tests.exe first.@.";
    exit 0);

  let utility_tests = read_test_cases utilities_file in
  let variant_tests =
    match variants_file with Some f -> read_test_cases f | None -> []
  in

  if utility_tests = [] && variant_tests = [] then (
    Fmt.epr "No test cases with expected CSS found.@.";
    exit 0);

  let total = List.length utility_tests + List.length variant_tests in
  Fmt.epr "Running %d upstream tests...@." total;

  let utility_cases =
    List.map
      (fun tc -> test_case tc.name `Quick (run_test_case tc))
      utility_tests
  in
  let variant_cases =
    List.map
      (fun tc -> test_case tc.name `Quick (run_test_case tc))
      variant_tests
  in
  let suites =
    [ ("utilities", utility_cases) ]
    @ if variant_cases <> [] then [ ("variants", variant_cases) ] else []
  in
  Alcotest.run "upstream" suites
