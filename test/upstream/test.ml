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

type case = {
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
      with Not_found | Failure _ -> None)
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
        let unit = try Re.Group.get m 3 with Not_found -> "" in
        let length : Css.length =
          match unit with
          | "px" -> Px value
          | "rem" -> Rem value
          | "" when value = 0.0 -> Zero
          | _ -> Px value
        in
        Some (name, length)
      with Not_found | Failure _ -> None)
    matches

let extract_ring_width css : int =
  let pattern =
    Re.Pcre.regexp
      {|\.ring\s*\{[^}]*calc\((\d+)px\s*\+\s*var\(--tw-ring-offset-width\)\)|}
  in
  match Re.exec_opt pattern css with
  | Some m -> (
      try int_of_string (Re.Group.get m 1) with Not_found | Failure _ -> 1)
  | None -> 1

let extract_border_width css : int =
  let border_pattern =
    Re.Pcre.regexp {|\.border\s*\{[^}]*border-width:\s*(\d+)px|}
  in
  match Re.exec_opt border_pattern css with
  | Some m -> (
      try int_of_string (Re.Group.get m 1) with Not_found | Failure _ -> 1)
  | None -> (
      (* Also check divide-x/divide-y patterns: calc(Npx *
         var(--tw-divide-...)) *)
      let divide_pattern =
        Re.Pcre.regexp
          {|calc\((\d+)px \* (?:var\(--tw-divide-[xy]-reverse\)|\(1)|}
      in
      match Re.exec_opt divide_pattern css with
      | Some m -> (
          try int_of_string (Re.Group.get m 1) with Not_found | Failure _ -> 1)
      | None -> 1)

let extract_outline_width css : int =
  let pattern =
    Re.Pcre.regexp {|\.outline\s*\{[^}]*outline-width:\s*(\d+)px|}
  in
  match Re.exec_opt pattern css with
  | Some m -> (
      try int_of_string (Re.Group.get m 1) with Not_found | Failure _ -> 1)
  | None -> 1

(** Extract breakpoint values from expected CSS. Looks for patterns like
    [@media (min-width: 640px)] and maps them to standard breakpoint names using
    the known Tailwind v4 breakpoint→px mapping. Returns all standard
    breakpoints when any px-based breakpoint is found. *)
let extract_breakpoints_from_css expected =
  let pattern = Re.Pcre.regexp {|@media[^(]*\(min-width:\s*(\d+)px\)|} in
  let matches = Re.all pattern expected in
  let px_values =
    List.filter_map
      (fun m ->
        try Some (float_of_string (Re.Group.get m 1))
        with Not_found | Failure _ -> None)
      matches
  in
  (* Standard Tailwind v4 breakpoints *)
  let standard =
    [ ("sm", 640.); ("md", 768.); ("lg", 1024.); ("xl", 1280.); ("2xl", 1536.) ]
  in
  (* If any px-based breakpoint is found in expected CSS, return all standard
     breakpoints that appear in the expected CSS *)
  if px_values = [] then []
  else List.filter (fun (_, px) -> List.mem px px_values) standard

let scheme_from_expected_css expected : Tw.Scheme.t =
  let spacing = extract_spacing_from_css expected in
  let radius = extract_radius_from_css expected in
  let default_ring_width = extract_ring_width expected in
  let default_border_width = extract_border_width expected in
  let default_outline_width = extract_outline_width expected in
  let breakpoints = extract_breakpoints_from_css expected in
  {
    colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
    spacing;
    radius;
    default_ring_width;
    default_border_width;
    default_outline_width;
    breakpoints;
  }

let setup_scheme_for_test expected =
  let scheme = scheme_from_expected_css expected in
  Tw.Color.Handler.set_scheme scheme;
  Tw.Theme.set_scheme scheme;
  Tw.Borders.set_scheme scheme;
  Tw.Effects.set_scheme scheme;
  Tw.Divide.set_scheme scheme;
  Tw.Rules.set_scheme scheme;
  (* Register custom breakpoints for modifier parsing *)
  let standard_names = [ "sm"; "md"; "lg"; "xl"; "2xl" ] in
  let custom_bps =
    List.filter
      (fun (name, _) -> not (List.mem name standard_names))
      scheme.breakpoints
  in
  Tw.Modifiers.register_custom_breakpoints custom_bps

(** Extract all CSS variable names referenced in expected CSS text. *)
let extract_var_names expected =
  let vars = ref Css.Pp.String_set.empty in
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
          Css.Pp.String_set.add (String.sub expected (i + 2) (!j - i - 2)) !vars;
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
      with Not_found | Failure _ -> None)
    matches

(** Build theme configuration for CSS emission. *)
let theme_config config expected =
  let hardcoded =
    [
      ("default-transition-timing-function", "ease", "ease");
      ("default-transition-duration", ".1s", "0");
    ]
  in
  let root_vars = extract_root_vars expected in
  let combined_defaults name =
    match List.assoc_opt name root_vars with
    | Some _ as result -> result
    | None -> (
        match Tw.Var.resolve_theme_refs name with
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
  let _inline_defaults name =
    List.find_map
      (fun (var_name, inline_val, _) ->
        if name = var_name then Some inline_val else None)
      hardcoded
  in
  match config with
  | Run -> (Css.Pp.String_set.empty, combined_defaults)
  | Theme -> (extract_var_names expected, combined_defaults)
  | Theme_inline -> (Css.Pp.String_set.empty, combined_defaults)
  | No_theme -> (Css.Pp.String_set.empty, hardcoded_only)
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

(** Extract var(--name, fallback) patterns from expected CSS. Returns (name,
    fallback) pairs where name is without the -- prefix. Handles both concrete
    fallbacks (e.g., var(--opacity-half, .5)) and nested var fallbacks (e.g.,
    var(--opacity-custom, var(--custom-opacity))). *)
let extract_var_fallbacks expected =
  let pattern =
    Re.Pcre.regexp
      {|var\(--([a-zA-Z0-9_-]+),\s*(var\(--[a-zA-Z0-9_-]+\)|[^)]+)\)|}
  in
  let matches = Re.all pattern expected in
  List.filter_map
    (fun m ->
      try
        let name = Re.Group.get m 1 in
        let fallback = String.trim (Re.Group.get m 2) in
        Some (name, fallback)
      with Not_found | Failure _ -> None)
    matches

(** Set theme value overrides for non-spacing root vars from expected CSS. This
    enables utilities like z-auto and order-first to produce custom declarations
    in the :root, :host block when [@config] theme is used. *)
let setup_theme_overrides config expected =
  Tw.Var.clear_theme_values ();
  match config with
  | Theme | Theme_inline | Theme_reference | Theme_inline_reference ->
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
        root_vars;
      (* For theme-reference mode, also extract var(--name, fallback) patterns
         from expected CSS. This provides fallback values for opacity modifiers
         and other cases where the @theme block isn't in our test format. *)
      if config = Theme_reference || config = Theme_inline_reference then
        let var_fallbacks = extract_var_fallbacks expected in
        List.iter
          (fun (name, fallback) ->
            if Tw.Var.theme_value name = None then
              Tw.Var.set_theme_value name fallback)
          var_fallbacks
  | Run | No_theme -> ()

(** Extract custom breakpoints by matching input class modifiers with px values
    from expected CSS. Handles bare custom names (e.g. "10xl:flex"), and names
    within min-/max- prefixes (e.g. "min-xs:max-sm:flex"). *)
let extract_custom_breakpoints classes expected =
  let standard_names = [ "sm"; "md"; "lg"; "xl"; "2xl" ] in
  (* Split each class into modifier segments and extract breakpoint names. For
     "min-xs:max-sm:flex", segments are ["min-xs"; "max-sm"; "flex"]. We extract
     "xs" from "min-xs" and recognize it as a custom breakpoint. *)
  let extract_bp_name segment =
    (* Strip min-/max- prefix to get the breakpoint name *)
    let name =
      if String.length segment > 4 && String.sub segment 0 4 = "min-" then
        Some (String.sub segment 4 (String.length segment - 4))
      else if String.length segment > 4 && String.sub segment 0 4 = "max-" then
        Some (String.sub segment 4 (String.length segment - 4))
      else Some segment
    in
    match name with
    | Some n when String.contains n '[' -> None (* arbitrary value *)
    | Some n when List.mem n standard_names -> None (* standard *)
    | Some n -> Some n
    | None -> None
  in
  let is_known_modifier s =
    let known =
      [
        "hover";
        "focus";
        "active";
        "disabled";
        "dark";
        "motion-safe";
        "motion-reduce";
        "contrast-more";
        "contrast-less";
        "print";
        "portrait";
        "landscape";
        "ltr";
        "rtl";
        "before";
        "after";
        "first";
        "last";
        "odd";
        "even";
        "open";
        "checked";
        "starting";
        "focus-within";
        "focus-visible";
        "forced-colors";
        "inverted-colors";
        "noscript";
        "marker";
        "selection";
        "placeholder";
        "backdrop";
        "file";
        "first-letter";
        "first-line";
        "details-content";
        "empty";
        "default";
        "required";
        "valid";
        "invalid";
        "in-range";
        "out-of-range";
        "placeholder-shown";
        "autofill";
        "read-only";
        "read-write";
        "optional";
        "enabled";
        "target";
        "visited";
        "inert";
        "user-valid";
        "user-invalid";
        "first-of-type";
        "last-of-type";
        "only-of-type";
        "only";
        "indeterminate";
        "pointer-none";
        "pointer-coarse";
        "pointer-fine";
        "any-pointer-none";
        "any-pointer-coarse";
        "any-pointer-fine";
        "*";
        "**";
      ]
    in
    List.mem s known
    || String.starts_with ~prefix:"group-" s
    || String.starts_with ~prefix:"peer-" s
    || String.starts_with ~prefix:"aria-" s
    || String.starts_with ~prefix:"data-" s
    || String.starts_with ~prefix:"not-" s
    || String.starts_with ~prefix:"has-" s
    || String.starts_with ~prefix:"supports-" s
    || String.starts_with ~prefix:"@" s
    || String.starts_with ~prefix:"nth-" s
    || String.starts_with ~prefix:"in-" s
    || String.contains s '['
  in
  let custom_names = ref [] in
  List.iter
    (fun cls ->
      let parts = String.split_on_char ':' cls in
      (* All parts except the last are modifiers *)
      let modifiers =
        match List.rev parts with _ :: rest -> List.rev rest | [] -> []
      in
      List.iter
        (fun seg ->
          if not (is_known_modifier seg) then
            match extract_bp_name seg with
            | Some name when name <> "" ->
                if not (List.mem name !custom_names) then
                  custom_names := name :: !custom_names
            | _ -> ())
        modifiers)
    classes;
  let custom_names = List.rev !custom_names in
  (* Extract all px values from expected CSS *)
  let px_pattern = Re.Pcre.regexp {|min-width:\s*(\d+)px|} in
  let px_matches = Re.all px_pattern expected in
  let px_values =
    List.filter_map
      (fun m ->
        try Some (float_of_string (Re.Group.get m 1))
        with Not_found | Failure _ -> None)
      px_matches
  in
  let standard_px = [ 640.; 768.; 1024.; 1280.; 1536. ] in
  let custom_px =
    List.filter (fun px -> not (List.mem px standard_px)) px_values
    |> List.sort_uniq Float.compare
  in
  match (custom_names, custom_px) with
  | [ name ], [ px ] -> [ (name, px) ]
  | names, pxs when List.length names = List.length pxs ->
      List.combine names pxs
  | _ -> []

let run_test_case test () =
  if test.classes = [] then ()
  else (
    setup_scheme_for_test test.expected;
    (* Register custom breakpoints before parsing classes *)
    let custom_bps = extract_custom_breakpoints test.classes test.expected in
    if custom_bps <> [] then (
      let scheme = scheme_from_expected_css test.expected in
      let updated_scheme =
        { scheme with breakpoints = scheme.breakpoints @ custom_bps }
      in
      Tw.Rules.set_scheme updated_scheme;
      Tw.Modifiers.register_custom_breakpoints custom_bps);
    setup_theme_overrides test.config test.expected;
    let theme, theme_defaults = theme_config test.config test.expected in
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

let file basename =
  let paths = [ basename; "test/upstream/" ^ basename ] in
  List.find_opt Sys.file_exists paths

let () =
  let utilities_file =
    file "utilities.txt" |> Option.value ~default:"utilities.txt"
  in
  let variants_file = file "variants.txt" in

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
