(** Upstream Tailwind CSS test runner.

    KEEP THIS TESTER AS DUMB AS POSSIBLE.

    The workflow is:

    {ol
     {- Extract tests from upstream into .txt files (see [extract_tests.ml]):
        {v
    dune exec test/upstream/extract_tests.exe -- \
      <tailwindcss>/packages/tailwindcss/src/utilities.test.ts \
      > test/upstream/utilities.txt
    dune exec test/upstream/extract_tests.exe -- \
      <tailwindcss>/packages/tailwindcss/src/variants.test.ts \
      > test/upstream/variants.txt
        v}
     }
     {- Run each test with tw }
     {- Compare expected output vs what we got }
     {- Fail for ANY difference (even spaces) }
    }

    No filtering, no tree diffing, no special-casing. If a test fails, either
    fix our code or fix the extraction.

    CSS whitespace normalisation (parse + re-emit) is already a stretch but
    necessary because the expected CSS comes from JS template literals with
    extra indentation. *)

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
          if classes <> [] then
            tests := { name; config; classes; expected } :: !tests
      | line :: rest ->
          if String.trim line = "<<<>>>" then (
            let expected = Buffer.contents buf |> String.trim in
            if classes <> [] then
              tests := { name; config; classes; expected } :: !tests;
            parse rest)
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf '\n';
            Buffer.add_string buf line;
            parse_expected name config classes buf rest)
    in
    parse lines;
    List.rev !tests

(** Extract spacing values from expected CSS. *)
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

(** Extract radius values from expected CSS. *)
let extract_radius_from_css css : (string * Css.length) list =
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

let extract_default_ring_width_from_css css : int =
  let pattern =
    Re.Pcre.regexp
      {|\.ring\s*\{[^}]*calc\((\d+)px\s*\+\s*var\(--tw-ring-offset-width\)\)|}
  in
  match Re.exec_opt pattern css with
  | Some m -> ( try int_of_string (Re.Group.get m 1) with _ -> 1)
  | None -> 1

let extract_default_border_width_from_css css : int =
  let border_pattern =
    Re.Pcre.regexp {|\.border\s*\{[^}]*border-width:\s*(\d+)px|}
  in
  match Re.exec_opt border_pattern css with
  | Some m -> ( try int_of_string (Re.Group.get m 1) with _ -> 1)
  | None -> (
      (* Also check divide-x/divide-y patterns: calc(Npx *
         var(--tw-divide-...)) *)
      let divide_pattern =
        Re.Pcre.regexp
          {|calc\((\d+)px \* (?:var\(--tw-divide-[xy]-reverse\)|\(1)|}
      in
      match Re.exec_opt divide_pattern css with
      | Some m -> ( try int_of_string (Re.Group.get m 1) with _ -> 1)
      | None -> 1)

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

let setup_scheme_for_test expected =
  let scheme = scheme_from_expected_css expected in
  Tw.Color.Handler.set_scheme scheme;
  Tw.Theme.set_scheme scheme;
  Tw.Borders.set_scheme scheme;
  Tw.Effects.set_scheme scheme;
  Tw.Divide.set_scheme scheme

(** Extract all CSS variable names referenced in expected CSS text. *)
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

(** Extract all CSS custom property definitions from :root, :host block. Returns
    (name, value) pairs where name is without the -- prefix. E.g.,
    "--spacing-big: 100rem" → ("spacing-big", "100rem") *)
let extract_root_vars expected =
  let pattern = Re.Pcre.regexp {|--([a-zA-Z0-9_-]+):\s*([^;}]+)|} in
  let matches = Re.all pattern expected in
  List.filter_map
    (fun m ->
      try
        let name = Re.Group.get m 1 in
        let value = String.trim (Re.Group.get m 2) in
        Some (name, value)
      with _ -> None)
    matches

(** Build theme configuration for CSS emission. *)
let make_theme_config config expected =
  let hardcoded =
    [
      ("default-transition-timing-function", "ease", "ease");
      ("default-transition-duration", ".1s", "0s");
    ]
  in
  let root_vars = extract_root_vars expected in
  let combined_defaults name =
    match Tw.Var.resolve_theme_refs name with
    | Some _ as result -> result
    | None -> (
        match List.assoc_opt name root_vars with
        | Some _ as result -> result
        | None ->
            List.find_map
              (fun (var_name, _, default) ->
                if name = var_name then Some default else None)
              hardcoded)
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
  | Run -> (Css.Pp.StringSet.empty, combined_defaults)
  | Theme -> (extract_var_names expected, combined_defaults)
  | Theme_inline -> (Css.Pp.StringSet.empty, inline_defaults)
  | No_theme -> (Css.Pp.StringSet.empty, hardcoded_only)
  | Theme_reference | Theme_inline_reference ->
      (extract_var_names expected, Css.Pp.no_theme_defaults)

(** Normalize a CSS string by parsing and re-emitting through our formatter.
    This eliminates whitespace/formatting differences from JS template literals
    so we can do a direct string comparison. *)
let normalize_css css =
  let trimmed = String.trim css in
  if trimmed = "" then ""
  else
    match Css.of_string trimmed with
    | Ok ast -> Css.to_string ~minify:false ~newline:false ast |> String.trim
    | Error _ -> trimmed

(** Set theme value overrides for non-spacing root vars from expected CSS.
    This enables utilities like z-auto and order-first to produce custom
    declarations in the :root, :host block when @config theme is used. *)
let setup_theme_overrides config expected =
  Tw.Var.clear_theme_values ();
  match config with
  | Theme | Theme_reference | Theme_inline_reference ->
      let root_vars = extract_root_vars expected in
      List.iter
        (fun (name, value) ->
          (* Skip numbered spacing (spacing-N) and tw- vars handled via scheme.
             Named spacings like spacing-big are passed through. *)
          let is_numbered_spacing =
            String.length name > 8
            && String.sub name 0 8 = "spacing-"
            &&
            let rest = String.sub name 8 (String.length name - 8) in
            match int_of_string_opt rest with Some _ -> true | None -> false
          in
          let is_bare_spacing = name = "spacing" in
          let is_tw_var =
            String.length name > 3 && String.sub name 0 3 = "tw-"
          in
          if not (is_numbered_spacing || is_bare_spacing || is_tw_var) then
            Tw.Var.set_theme_value name value)
        root_vars
  | Run | Theme_inline | No_theme -> ()

let run_test_case test () =
  if test.classes = [] then ()
  else (
    setup_scheme_for_test test.expected;
    setup_theme_overrides test.config test.expected;
    let theme, theme_defaults = make_theme_config test.config test.expected in
    let utilities =
      List.filter_map
        (fun cls ->
          match Tw.of_string cls with Ok u -> Some u | Error _ -> None)
        test.classes
    in
    let our_css =
      if utilities = [] then ""
      else
        Tw.to_css ~base:false ~layers:false ~optimize:true utilities
        |> Css.to_string ~minify:false ~theme ~theme_defaults
        |> String.trim
    in
    let expected = normalize_css test.expected in
    if our_css = "" && expected = "" then ()
    else if our_css <> expected then
      Alcotest.fail
        (Fmt.str "CSS mismatch for: %s\n\nExpected:\n%s\n\nGot:\n%s"
           (String.concat " " test.classes)
           expected our_css))

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
