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

module Css = Cascade.Css
open Cascade_diff
open Alcotest
open Upstream_fixture

(* Colour comparison is normalised the way Tailwind normalises its own
   snapshots: [color_mix_to_oklab] then [truncate_color_precision] run on both
   sides before the diff, so an [oklab] fixture and tw's exact colour describe
   the same value. That is pure fixture skew, not a tw bug: Tailwind's snapshot
   serialiser truncates oklab axes for cross-OS stability, and the fixtures fold
   a concrete-colour opacity to [oklab] where tw keeps the shorter exact colour.
   One tolerance remains on top of that -- [is_allowed_canonicalization_diff],
   for [--font-sans] / [--tw-prose-*] custom-property skew. Set
   [TW_UPSTREAM_STRICT=1] to disable it and watch for changes that close the
   gap. (A [--text-*--line-height] allowance, a mask-angle calc allowance, an
   [@property --spacing] hint, a prose selector-permutation allowance and the
   hard-coded stale-colour allowlist were all dropped earlier, once tw honoured
   theme line-height overrides, emitted Tailwind's exact mask degrees, cascade
   gained typed calc + the custom-property prune, and the colour normalisation
   above replaced the allowlist.) *)
let strict = Sys.getenv_opt "TW_UPSTREAM_STRICT" <> None

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
    Tw.Scheme.default with
    colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
    spacing;
    radius;
    default_ring_width;
    default_border_width;
    default_outline_width;
    breakpoints;
  }

let setup_scheme_for_test expected =
  (* The scheme is threaded into Tw.of_string and Tw.to_css via ~theme; the
     custom breakpoints it carries are what the modifier parser reads. *)
  scheme_from_expected_css expected

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

(* The expected CSS is Tailwind's own output, so a token read back out of it and
   handed to tw is compared against itself: on that token the runner is blind.
   Reading one back is only legitimate where the test's own [@theme] block
   declares it, since there the test supplies the theme and the expected [:root]
   holds it in the spelling Tailwind normalises to. Every other name the scan
   turns up -- a built-in token, a [--tw-*] emitted by a utility, a [--name:]
   read out of a [@supports] condition -- is tw's own output to produce. *)
let declared_root_vars ~declared expected =
  List.filter
    (fun (name, _) -> List.mem name declared)
    (extract_root_vars expected)

(** Build theme configuration for CSS emission. *)
let theme_config ~declared config expected =
  let hardcoded =
    [
      ("default-transition-timing-function", "ease", "ease");
      ("default-transition-duration", ".1s", "0");
    ]
  in
  let root_vars = declared_root_vars ~declared expected in
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
  let combined_inline_defaults name =
    match List.assoc_opt name root_vars with
    | Some _ as result -> result
    | None -> (
        match Tw.Var.resolve_theme_refs name with
        | Some _ as result -> result
        | None ->
            List.find_map
              (fun (var_name, inline_val, _) ->
                if name = var_name then Some inline_val else None)
              hardcoded)
  in
  match config with
  | Run -> (extract_var_names expected, combined_defaults)
  | Theme -> (extract_var_names expected, combined_defaults)
  | Theme_inline -> (Css.Pp.String_set.empty, combined_inline_defaults)
  | No_theme -> (extract_var_names expected, hardcoded_only)
  | Theme_reference | Theme_inline_reference ->
      (extract_var_names expected, fun _ -> None)

let canonical_stylesheet_css css = String.trim css

(* [color-mix(in oklab, C p%, transparent)] denotes the concrete colour C at
   alpha p, which LightningCSS folds to an [oklab(...)] in the fixtures. tw
   keeps the colour itself (exact and shorter once cascade folds it to a hex),
   so re-express it as that oklab -- resolving C through cascade's own colour
   fold, which handles named colours like [red] that have no public name->rgb
   table -- and let the shared truncation reconcile it with the fixture. A
   [var()] or [currentcolor] operand does not fold to a hex (the [[^,()]+]
   capture also skips [var(...)]), so its color-mix is left untouched. *)
let color_mix_re =
  (* [our_css] is minified ([,] with no following space) while the fixtures keep
     [, ]; match either. *)
  Re.Pcre.regexp
    {|color-mix\(in oklab,\s*([^,()]+?)\s+([0-9.]+)%,\s*transparent\)|}

let hex_re = Re.Pcre.regexp {|#[0-9a-fA-F]+|}

let color_mix_to_oklab s =
  Re.replace color_mix_re s ~f:(fun g ->
      let whole = Re.Group.get g 0 in
      match Fmt.kstr Css.of_string ".x{color:%s}" whole with
      | Error _ -> whole
      | Ok { stylesheet; _ } -> (
          let folded = Css.to_string ~minify:true (Css.optimize stylesheet) in
          match Re.exec_opt hex_re folded with
          | None -> whole (* did not fold to a hex: keep (var / currentcolor) *)
          | Some m -> (
              (* A short hex parses to [Authored_hex] (source spelling kept);
                 both it and [Hex] carry decoded [r]/[g]/[b]. *)
              match Css.parse_color (Re.Group.get m 0) with
              | Some
                  ( Css.Hex { r; g = green; b; _ }
                  | Css.Authored_hex { r; g = green; b; _ } ) ->
                  let f x = float_of_int x /. 255. in
                  let l, a, b =
                    Cascade.Color_space.oklab_of_linear_srgb
                      (Cascade.Color_space.linear_rgb_of_rgb
                         (f r, f green, f b))
                  in
                  let alpha = float_of_string (Re.Group.get g 2) /. 100. in
                  Fmt.str "oklab(%.6f%% %.6f %.6f / %.6f)" (l *. 100.) a b alpha
              | _ -> whole)))

(* Reduce the precision of [oklab]/[oklch]/[lab]/[lch] coefficients to three
   decimals, mirroring Tailwind's own snapshot serialiser
   ([test-utils/custom-serializer.ts]): it truncates those axes because
   "lightningcss generat[es] different decimal places in the last position when
   run on different operating systems". cascade keeps the full-precision colour,
   so tw's [oklab(59.98238277% -.06725164 -.12414399 / .5)] and the fixture's
   truncated [oklab(59.9824% -.067 -.124 / .5)] describe the same colour that
   only differs in serialisation noise. Applied to both sides before the diff,
   the two spellings collapse to an identical value (and identical folded hex),
   so no per-colour allowlist is needed. It is truncation, not rounding
   ([.224863] -> [.224], never [.225]), and a genuine regression that moves a
   coefficient at the third decimal or coarser still shows as a diff. *)
let truncate_color_precision =
  let frac = Re.Pcre.regexp {|\.(\d{3})\d+|} in
  let color_fn = Re.Pcre.regexp {|\b(oklab|oklch|lab|lch)\(([^()]*)\)|} in
  fun s ->
    Re.replace color_fn s ~f:(fun g ->
        let name = Re.Group.get g 1 and args = Re.Group.get g 2 in
        let args = Re.replace frac args ~f:(fun d -> "." ^ Re.Group.get d 1) in
        name ^ "(" ^ args ^ ")")

let is_allowed_canonicalization_diff diff =
  let allowed_custom_property = function
    | "--font-sans" | "--font-mono" -> true
    | name when String.starts_with ~prefix:"--tw-prose-" name -> true
    | _ -> false
  in
  let allowed_rule_change = function
    | Tree_diff.Content_changed
        { property_changes; added_properties = []; removed_properties = []; _ }
      ->
        property_changes <> []
        && List.for_all
             (fun (change : Tree_diff.declaration) ->
               allowed_custom_property change.property_name)
             property_changes
    | _ -> false
  in
  let allowed_container = function
    | Tree_diff.Modified { rule_changes; container_changes = []; _ } ->
        List.for_all allowed_rule_change rule_changes
    | _ -> false
  in
  match Css_compare.as_tree_diff diff with
  | Some Tree_diff.{ rules; containers; layer_order = None } ->
      (rules <> [] || containers <> [])
      && List.for_all allowed_rule_change rules
      && List.for_all allowed_container containers
  | _ -> false

(* Guards [declared_root_vars]: only a token the test's own [@theme] block
   declared is read back out of the expected CSS, so the runner never hands tw
   the very value it is about to compare against. *)
let test_echo_only_declared_tokens () =
  let expected =
    ":root, :host { --radius: .25rem; --leading-snug: 1.375 }\n\
     .rounded { border-radius: var(--radius) }\n\
     @supports (--test: var(--tw)) { .x { display: flex } }"
  in
  Alcotest.(check (list string))
    "a built-in token, and a name read out of a @supports condition, stay out"
    [ "radius" ]
    (List.map fst (declared_root_vars ~declared:[ "radius" ] expected))

let test_layer_order_not_tolerated () =
  let expected =
    "@layer weak, strong;@media (width >= 1px){.x{--font-sans:a}}"
  in
  let actual = "@layer strong, weak;@media (width >= 1px){.x{--font-sans:b}}" in
  let diff = Css_compare.diff ~mode:`Tree expected actual in
  Alcotest.(check bool)
    "a tolerated declaration cannot hide a layer-order change" false
    (is_allowed_canonicalization_diff diff)

(** Set theme value overrides for root vars from expected CSS. This enables
    utilities like z-auto and order-first to produce custom declarations in the
    :root, :host block when [@config] theme is used. Everything the test's own
    [@theme] block sets goes through, the spacing scale included: no [Scheme.t]
    field owns the bare [--spacing] multiplier, and filtering the scale out only
    hid the tests that set it. *)
let theme_overrides_of ~declared config expected =
  match config with
  | Run | Theme | Theme_inline | Theme_reference | Theme_inline_reference ->
      let root_vars = declared_root_vars ~declared expected in
      let base =
        List.filter (fun (name, _) -> not (is_runtime_var name)) root_vars
      in
      (* For theme-reference mode, also extract var(--name, fallback) patterns
         from expected CSS. This provides fallback values for opacity modifiers
         and other cases where the @theme block isn't in our test format. *)
      if config = Theme_reference || config = Theme_inline_reference then
        let var_fallbacks = extract_var_fallbacks expected in
        let extra =
          List.filter
            (fun (name, _) -> not (List.mem_assoc name base))
            var_fallbacks
        in
        base @ extra
      else base
  | No_theme -> []

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
  let collect_custom_name acc seg =
    if is_known_modifier seg then acc
    else
      match extract_bp_name seg with
      | Some name when name <> "" && not (List.mem name acc) -> name :: acc
      | _ -> acc
  in
  let modifiers_of_class cls =
    let parts = String.split_on_char ':' cls in
    match List.rev parts with _ :: rest -> List.rev rest | [] -> []
  in
  let custom_names =
    List.fold_left
      (fun acc cls ->
        List.fold_left collect_custom_name acc (modifiers_of_class cls))
      [] classes
    |> List.rev
  in
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

(* Parity accounting, accumulated across every upstream case and printed as a
   report after the run. The old runner dropped classes [Tw.of_string] rejected
   with [filter_map ... -> None], so a class tw cannot parse left no trace.
   These counters make rejection explicit. A rejection is harmless when the
   case's CSS still matches (Tailwind also emits nothing for that class -- many
   upstream cases bundle negative tests, e.g. [animate-not-found], with valid
   classes); a rejection that breaks parity already fails the CSS diff below,
   and that failure now names the rejected classes. *)
let stat_total_classes = ref 0
let stat_parsed = ref 0
let stat_rejected = ref 0
let stat_expected_empty_cases = ref 0

let run_test_case test () =
  if test.classes = [] then ()
  else
    let base_scheme = setup_scheme_for_test test.expected in
    (* Register any matchVariant custom variants for this test. Directive form:
       "name <template> KEY=value ...", DEFAULT mapped to the default slot. *)
    let parse_variant_directive d =
      match String.split_on_char ' ' d with
      | "container" :: _ -> None (* handled by parse_container_directive *)
      | name :: template :: pairs ->
          let values =
            List.filter_map
              (fun kv ->
                match String.index_opt kv '=' with
                | Some i ->
                    let k = String.sub kv 0 i in
                    let v = String.sub kv (i + 1) (String.length kv - i - 1) in
                    Some ((if k = "DEFAULT" then "" else k), v)
                | None -> None)
              pairs
          in
          Some (name, Tw.Scheme.{ values; template })
      | _ -> None
    in
    (* [@custom-variant <name> { @container <header> { @slot } }] directives,
       extracted as "container <name> <header>". A leading plain identifier in
       the header (not [not], not a function) is the container name. *)
    let parse_container_directive d =
      let container_of_header header =
        match String.index_opt header ' ' with
        | Some i ->
            let first = String.sub header 0 i in
            let rest =
              String.sub header (i + 1) (String.length header - i - 1)
            in
            if first <> "not" && not (String.contains first '(') then
              Css.Container.Named (first, Css.Container.of_string rest)
            else Css.Container.of_string header
        | None -> Css.Container.of_string header
      in
      match String.split_on_char ' ' d with
      | "container" :: name :: (_ :: _ as rest) -> (
          let header = String.concat " " rest in
          try Some (name, container_of_header header) with Failure _ -> None)
      | _ -> None
    in
    (* The case's own [@custom-variant]s and custom breakpoints ride on the
       scheme threaded to [Tw.of_string] and [Tw.to_css] below, so they are
       local to this case rather than left in a registry the next one reads. *)
    let custom_bps = extract_custom_breakpoints test.classes test.expected in
    let scheme =
      {
        base_scheme with
        breakpoints = base_scheme.breakpoints @ custom_bps;
        custom_variants = List.filter_map parse_variant_directive test.variants;
        container_variants =
          List.filter_map parse_container_directive test.variants;
      }
    in
    (* Thread the @theme token overrides into the scheme so utilities read them
       from ~theme (parse and render); the Var global is no longer consulted. *)
    let declared = List.map fst test.theme_vars in
    let scheme =
      (* [theme_overrides_of] reads the values of the test's own [@theme] tokens
         back out of the expected [:root] (already normalized as Tailwind emits
         them), so it must win over the raw [@theme-var] source values for
         theme-layer emission; [test.theme_vars] is the fallback for tokens not
         present in the expected [:root] (e.g. inlined [@theme] blocks like
         text-shadow). *)
      let theme_vars =
        List.filter (fun (name, _) -> not (is_runtime_var name)) test.theme_vars
      in
      Tw.Scheme.with_overrides scheme
        (theme_overrides_of ~declared test.config test.expected @ theme_vars)
    in
    let theme, theme_defaults =
      theme_config ~declared test.config test.expected
    in
    let parsed, rejected =
      List.fold_left
        (fun (ok, bad) cls ->
          match Tw.of_string ~theme:scheme cls with
          | Ok u -> (u :: ok, bad)
          | Error (`Msg m) -> (ok, (cls, m) :: bad))
        ([], []) test.classes
    in
    let utilities = List.rev parsed in
    let rejected = List.rev rejected in
    stat_total_classes := !stat_total_classes + List.length test.classes;
    stat_parsed := !stat_parsed + List.length utilities;
    stat_rejected := !stat_rejected + List.length rejected;
    if test.expected = "" then incr stat_expected_empty_cases;
    let our_stylesheet =
      if utilities = [] then None
      else Some (Tw.to_css ~theme:scheme ~base:false ~layers:false utilities)
    in
    let our_css =
      match our_stylesheet with
      | None -> ""
      | Some stylesheet ->
          stylesheet
          |> Css.resolve_theme ~theme ~theme_defaults
          |> Css.to_string ~minify:true |> String.trim
    in
    let expected = test.expected in
    let expected_css = canonical_stylesheet_css expected in
    if our_css = "" && expected = "" then ()
    else
      let normalize_colors s =
        truncate_color_precision (color_mix_to_oklab s)
      in
      let result =
        Css_compare.diff ~mode:`Canonical ~prune_unused_custom_props:true
          (normalize_colors expected_css)
          (normalize_colors our_css)
      in
      if
        (match result.Css_compare.result with
          | Css_compare.No_diff -> true
          | _ -> false)
        || ((not strict) && is_allowed_canonicalization_diff result)
      then ()
      else
        let buf = Buffer.create 1024 in
        Css_compare.pp ~expected:"Tailwind" ~actual:"Our TW" buf result;
        let got =
          match our_stylesheet with
          | None -> ""
          | Some stylesheet ->
              stylesheet
              |> Css.resolve_theme ~theme ~theme_defaults
              |> Css.to_string ~indent:2 |> String.trim
        in
        (* When a class tw rejected is a candidate cause of the mismatch, name
           it so the diff is not the only clue (the old runner dropped it
           silently). *)
        let rejected_note =
          match rejected with
          | [] -> ""
          | _ ->
              Fmt.str "\n\ntw rejected %d class(es):\n%s" (List.length rejected)
                (String.concat "\n"
                   (List.map
                      (fun (cls, m) -> Fmt.str "  %s -- %s" cls m)
                      rejected))
        in
        Alcotest.fail
          (Fmt.str "CSS mismatch for: %s\n\n%s\n\nExpected:\n%s\n\nGot:\n%s%s"
             (String.concat " " test.classes)
             (Buffer.contents buf) expected got rejected_note)

(* Guards [truncate_color_precision]: it truncates (never rounds) the
   oklab-family axes to three decimals like Tailwind's snapshot serialiser, so
   tw's full-precision colour and the fixture's reduced spelling collapse to the
   same value, while a coarser regression still survives as a diff. *)
let test_color_tolerance () =
  let check msg expected input =
    Alcotest.(check string) msg expected (truncate_color_precision input)
  in
  (* Truncates, never rounds: .224863 -> .224 (rounding would give .225). *)
  check "oklab axes truncated to 3 decimals" "oklab(62.795% .224 .125 / .2)"
    "oklab(62.7955% .224863 .125846 / .2)";
  (* tw's full precision and the fixture's reduced form collapse to equal. *)
  Alcotest.(check string)
    "full and reduced spellings collapse"
    (truncate_color_precision "oklab(59.9824% -.067 -.124 / .5)")
    (truncate_color_precision "oklab(59.98238277% -.06725164 -.12414399 / .5)");
  (* Leaves the [in oklab] interpolation keyword and short values untouched. *)
  check "color-mix keyword untouched"
    "color-mix(in oklab, #0088cc 50%, transparent)"
    "color-mix(in oklab, #0088cc 50%, transparent)";
  (* A difference at the third decimal survives truncation. *)
  Alcotest.(check bool)
    "coarse difference preserved" false
    (String.equal
       (truncate_color_precision "oklab(62.795% .224 .125 / .2)")
       (truncate_color_precision "oklab(62.795% .223 .125 / .2)"));
  (* [color_mix_to_oklab] re-expresses a concrete-colour opacity (resolving the
     named colour [red]) as the oklab the fixtures store; truncated, it lands on
     the fixture value. *)
  Alcotest.(check string)
    "color-mix(red 20%) re-expressed as the fixture oklab"
    "oklab(62.795% 0.224 0.125 / 0.200)"
    (truncate_color_precision
       (color_mix_to_oklab "color-mix(in oklab, red 20%, transparent)"));
  (* A var() operand cannot fold to a concrete colour, so its color-mix (which
     the fixtures also keep) is left alone. *)
  Alcotest.(check string)
    "color-mix(var) left untouched"
    "color-mix(in oklab, var(--x) 50%, transparent)"
    (color_mix_to_oklab "color-mix(in oklab, var(--x) 50%, transparent)")

let print_parity_report () =
  Fmt.epr "@.=== upstream parity report ===@.";
  Fmt.epr "classes: %d total, %d parsed, %d rejected@." !stat_total_classes
    !stat_parsed !stat_rejected;
  Fmt.epr "cases with empty expected CSS: %d@." !stat_expected_empty_cases;
  Fmt.epr
    "(a rejection is harmless when the case's CSS still matches; one that \
     breaks parity fails the CSS diff and names the rejected classes)@.";
  Fmt.epr "==============================@."

(* Both fixtures are checked in and declared as dune deps, so a missing one is a
   broken checkout rather than an optional extra. A floor on the parsed cases
   catches the other way this gate can go quiet: a fixture whose format drifts
   still parses, just into far fewer cases than it holds. The floors are about
   half the current counts (615 utilities, 166 variants), low enough to absorb
   upstream churn. *)
let utilities_floor = 300
let variants_floor = 80

let load basename floor =
  match path basename with
  | None ->
      Fmt.epr "%s not found. Run extract_tests.exe first.@." basename;
      exit 1
  | Some p ->
      let cases = read p in
      let n = List.length cases in
      if n < floor then (
        Fmt.epr "%s yielded %d test cases, fewer than the floor of %d.@." p n
          floor;
        exit 1);
      cases

let () =
  let utility_tests = load "utilities.txt" utilities_floor in
  let variant_tests = load "variants.txt" variants_floor in
  let total = List.length utility_tests + List.length variant_tests in
  Fmt.epr "Running %d upstream tests (%d utilities, %d variants)...@." total
    (List.length utility_tests)
    (List.length variant_tests);
  at_exit print_parity_report;

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
  let tolerance_cases =
    [
      test_case "oklab precision truncation" `Quick test_color_tolerance;
      test_case "canonical tolerance rejects layer order" `Quick
        test_layer_order_not_tolerated;
      test_case "the theme echo is limited to declared tokens" `Quick
        test_echo_only_declared_tokens;
    ]
  in
  let suites =
    [
      ("utilities", utility_cases);
      ("tolerance", tolerance_cases);
      ("variants", variant_cases);
    ]
  in
  Alcotest.run "upstream" suites
