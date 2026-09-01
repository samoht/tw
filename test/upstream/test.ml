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

    Both fixtures are generated, and nothing else belongs in them. A case
    upstream does not have is a tw regression test: it goes in the
    [test_<module>.ml] for the behaviour it pins, where it runs by name and the
    next reader of that module finds it. Put one here and the next regeneration
    drops it with the run still green -- which is how a [supports] block and a
    container-query block sat here duplicating coverage [test_modifiers.ml] and
    [test_sort.ml] already had. {!check_generated} counts each fixture's blocks
    against the banner the extractor wrote, so an addition is reported while it
    is still there.

    CSS whitespace normalisation (parse + re-emit) is already a stretch but
    necessary because the expected CSS comes from JS template literals with
    extra indentation. *)

module Css = Cascade.Css
module Entrypoint = Tw_tools.Entrypoint
open Cascade_diff
open Alcotest
open Upstream_fixture

(* Fixture comparison normalises semantic artifacts of Tailwind's serializer and
   compatibility minifier on both sides before the diff: colour precision,
   concrete colour-mix folds, redundant vendor prefixes, and a static math
   function that reduces to one number. None of those is a generator choice.
   Nothing else is tolerated: the diff a case reports is the diff it fails on.
   (A [--font-sans] / [--tw-prose-*] custom-property allowance, a
   [--text-*--line-height] allowance, a mask-angle calc allowance, an [@property
   --spacing] hint, a prose selector-permutation allowance and the hard-coded
   stale-colour allowlist were all dropped, once tw honoured theme line-height
   overrides, emitted Tailwind's exact mask degrees, cascade gained typed calc +
   the custom-property prune, and the colour normalisation above replaced the
   allowlist.) *)

(** The scheme fields a fixture's own [@theme] declarations set.

    Reading them out of the expected CSS instead would hand tw the very value it
    is about to be compared against: on such a token the runner is blind, and a
    built-in default can be wrong without a single case failing. A fixture's
    [@theme-var] lines are the test's own input, so they are an oracle the run
    cannot contaminate. Tokens with no typed [Scheme.t] field (the bare
    [--spacing] multiplier, [--breakpoint-*], a named [--spacing-big]) ride on
    the token overrides applied further down. *)
let declared_with_prefix ~prefix theme_vars =
  List.filter_map
    (fun (name, value) ->
      if String.starts_with ~prefix name then
        let n = String.length prefix in
        Some (String.sub name n (String.length name - n), String.trim value)
      else None)
    theme_vars

let scheme_of_theme_vars theme_vars : Tw.Scheme.t =
  let spacing =
    declared_with_prefix ~prefix:"spacing-" theme_vars
    |> List.filter_map (fun (key, value) ->
        match (int_of_string_opt key, Css.parse_length value) with
        | Some n, Some length -> Some (n, length)
        | _ -> None)
  in
  let radius =
    declared_with_prefix ~prefix:"radius-" theme_vars
    |> List.filter_map (fun (key, value) ->
        Option.map (fun length -> (key, length)) (Css.parse_length value))
  in
  let colors =
    declared_with_prefix ~prefix:"color-" theme_vars
    |> List.filter_map (fun (key, value) ->
        if String.length value > 0 && value.[0] = '#' then
          Some (key, Tw.Scheme.Hex value)
        else None)
  in
  let px name default =
    match List.assoc_opt name theme_vars with
    | Some value -> (
        match Css.parse_length (String.trim value) with
        | Some (Css.Px px) -> int_of_float px
        | Some Css.Zero -> 0
        | _ -> default)
    | None -> default
  in
  let default = Tw.Scheme.default in
  {
    default with
    colors;
    spacing;
    radius;
    default_ring_width = px "default-ring-width" default.default_ring_width;
    default_border_width =
      px "default-border-width" default.default_border_width;
    default_outline_width =
      px "default-outline-width" default.default_outline_width;
  }

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

(* [Css.resolve_theme]'s keep-set decides which of tw's [var()] references
   survive and which are inlined from [theme_defaults]. Keying it on the names
   the expected CSS happens to spell makes the comparison one-directional: where
   tw emits [var(--color-red-500)] and Tailwind emitted [#ef4444], the name is
   absent from the expected bytes, so tw's reference is inlined to the same
   literal before the diff and the case passes. That is exactly the
   variables-versus-inline choice the [Var] system exists to get right.

   Which tokens are inlined is a property of the fixture's own [@config], not of
   the output being compared: [@theme inline] inlines every token by definition,
   and the [run()] harness inlines the two [--default-transition-*] tokens it
   treats as literal fallbacks. Every other reference is tw's to defend, so the
   keep-set holds it live and the diff sees it. Binding a free reference at
   [:root] is a separate pass with no keep-set: it adds the declaration tw's
   [~base:false] render leaves out rather than replacing a value. *)
let inlined_tokens =
  [ "default-transition-timing-function"; "default-transition-duration" ]

let theme_resolution ~declared config expected =
  let hardcoded =
    [
      ("default-transition-timing-function", "ease", "ease");
      ("default-transition-duration", ".1s", "0");
    ]
  in
  let root_vars = declared_root_vars ~declared expected in
  let of_hardcoded pick name =
    List.find_map
      (fun (var_name, inline_val, default) ->
        if name = var_name then Some (pick (inline_val, default)) else None)
      hardcoded
  in
  let defaults pick name =
    match List.assoc_opt name root_vars with
    | Some _ as result -> result
    | None -> (
        match Tw.Var.resolve_theme_refs name with
        | Some _ as result -> result
        | None -> of_hardcoded pick name)
  in
  let combined_defaults = defaults snd in
  let combined_inline_defaults = defaults fst in
  (* Inline the named tokens and nothing else: an empty keep-set with a lookup
     that answers only for them. *)
  let inline_pass names theme_defaults stylesheet =
    Css.resolve_theme ~theme:Css.Pp.String_set.empty
      ~theme_defaults:(fun name ->
        if List.mem name names then theme_defaults name else None)
      stylesheet
  in
  let bind_pass theme_defaults stylesheet =
    Css.resolve_theme ~theme_defaults stylesheet
  in
  match config with
  | Run ->
      (* The [run()] harness renders the [--default-*] tokens as literal
         fallbacks whether or not the case redeclares them. *)
      fun stylesheet ->
        stylesheet
        |> inline_pass inlined_tokens combined_defaults
        |> bind_pass combined_defaults
  | Theme ->
      (* Under [@theme] a token the case declares is a real theme token with a
         [:root] declaration behind it, so its reference stays live; one it does
         not declare comes from the implicit default theme, which the fixtures
         render inline. *)
      let inlined =
        List.filter (fun name -> not (List.mem name declared)) inlined_tokens
      in
      fun stylesheet ->
        stylesheet
        |> inline_pass inlined combined_defaults
        |> bind_pass combined_defaults
  | Theme_inline ->
      (* [@theme inline] means every token is inlined at its use site, so the
         empty keep-set is the config's own semantics rather than a reading of
         the expected output. *)
      fun stylesheet ->
        Css.resolve_theme ~theme:Css.Pp.String_set.empty
          ~theme_defaults:combined_inline_defaults stylesheet
  | No_theme ->
      fun stylesheet -> inline_pass inlined_tokens (of_hardcoded snd) stylesheet
  | Theme_reference | Theme_inline_reference -> Fun.id

let canonical_stylesheet_css css = String.trim css

(* [@layer <name> { @tailwind utilities; }] in a case's template puts the
   generated utilities in [<name>] and leaves everything else beside it: the
   theme block, the [@property] rules, and whatever a declared utility hoists.
   tw wraps the whole sheet in its own layer scaffolding, so keep the layer the
   template named and flatten the rest. *)
let keep_only_layer ~before_theme name stylesheet =
  let rec go stmts =
    List.concat_map
      (fun stmt ->
        if Option.is_some (Css.layer_statement_name_list stmt) then []
        else
          match Css.as_layer stmt with
          | Some (Some n, _) when Css.Stylesheet.equal_layer_name n [ name ] ->
              [ stmt ]
          | Some (_, inner) -> go inner
          | None -> [ stmt ])
      stmts
  in
  let statements = go (Css.statements stylesheet) in
  let statements =
    if not before_theme then statements
    else
      let wrapped, rest =
        List.partition
          (fun stmt ->
            match Css.as_layer stmt with
            | Some (Some n, _) -> Css.Stylesheet.equal_layer_name n [ name ]
            | Some (None, _) | None -> false)
          statements
      in
      wrapped @ rest
  in
  Css.v statements

(* Tailwind compiled the corpus from each case's own [@theme] block, with no
   default theme behind it, so the [@keyframes] that theme declares are in no
   expected sheet: even [animate-spin] comes out without [@keyframes spin]. tw
   renders against the built-in theme, which carries them, so they are dropped
   here rather than read as rules Tailwind failed to emit. The same classes
   compiled against a real entrypoint do get them, and [tw --diff] covers
   that. *)
let drop_theme_keyframes stylesheet =
  Css.v
    (List.filter
       (fun stmt -> Option.is_none (Css.as_keyframes stmt))
       (Css.statements stylesheet))

(* Compatibility minifiers add prefixed declarations beside the declaration a
   utility authored. For parity, that duplicate is semantically inert only when
   the same declaration block holds an identical unprefixed property and value.
   A lone prefix, or a prefix whose value maps to another keyword, stays in the
   comparison. Parse warnings leave the input alone so this normalisation cannot
   hide a declaration the CSS reader dropped. *)
let vendor_prefixes = [ "-webkit-"; "-moz-"; "-ms-" ]

let declaration_parts declaration =
  let text = Css.Declaration.to_string ~minify:true declaration in
  match String.index_opt text ':' with
  | None -> None
  | Some i ->
      Some
        ( String.sub text 0 i,
          String.sub text (i + 1) (String.length text - i - 1) )

let unprefixed_name name =
  List.find_map
    (fun prefix ->
      if String.starts_with ~prefix name then
        Some
          (String.sub name (String.length prefix)
             (String.length name - String.length prefix))
      else None)
    vendor_prefixes

let drop_redundant_vendor_prefixes css =
  match Css.of_string css with
  | Ok { stylesheet; warnings = []; _ } ->
      let stylesheet =
        Css.Stylesheet.map_declarations
          (fun declarations ->
            List.filter
              (fun declaration ->
                match declaration_parts declaration with
                | Some (name, value) -> (
                    match unprefixed_name name with
                    | None -> true
                    | Some name ->
                        not
                          (List.exists
                             (fun candidate ->
                               declaration_parts candidate = Some (name, value))
                             declarations))
                | None -> true)
              declarations)
          stylesheet
      in
      Css.to_string ~minify:true stylesheet |> String.trim
  | Ok _ | Error _ -> css

(* LightningCSS evaluates a static dimensionless math function before writing a
   snapshot. Cascade deliberately preserves authored math in several property
   grammars, so canonical comparison evaluates only a function that parses in
   full as a [<number>] and reduces to a concrete numeric leaf. A dimension, a
   [var()] or an unknown function remains byte-for-byte visible. Concrete
   results use the snapshot serializer's six-significant-figure budget. *)

let rec concrete_number : Css.number -> float option = function
  | Css.Num value -> Some value
  | Css.Calc calc -> concrete_number_calc calc
  | Css.Var _ | Css.Round _ | Css.Mod _ | Css.Rem _ | Css.Hypot _ | Css.Pow _
  | Css.Sqrt _ | Css.Abs _ | Css.Sign _ | Css.Sin _ ->
      None

and concrete_number_calc : Css.number Css.calc -> float option = function
  | Css.Num value -> Some value
  | Css.Val number -> concrete_number number
  | Css.Nested calc | Css.Parens calc -> concrete_number_calc calc
  | Css.Expr (left, op, right) -> (
      match (concrete_number_calc left, concrete_number_calc right) with
      | Some left, Some right -> (
          match op with
          | Css.Add -> Some (left +. right)
          | Css.Sub -> Some (left -. right)
          | Css.Mul -> Some (left *. right)
          | Css.Div when right <> 0. -> Some (left /. right)
          | Css.Div -> None)
      | None, _ | _, None -> None)
  | Css.Var _ | Css.Math_const _ | Css.Sibling_index | Css.Sibling_count
  | Css.Math_fn _ ->
      None

let fold_declaration_static_number_math declaration =
  let authored = Css.Declaration.string_of_value ~minify:true declaration in
  let cursor = Cascade.Cursor.of_string authored in
  match
    try Some (Css.Values.read_number cursor)
    with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> None
  with
  | Some (Css.Num _) -> declaration
  | Some _ when not (Cascade.Cursor.is_done cursor) -> declaration
  | Some number -> (
      match concrete_number (Css.Values.normalize_number number) with
      | Some value when Float.is_finite value -> (
          let value = Css.Pp.round_sig 6 value in
          let value =
            Css.Pp.to_string ~minify:true Css.Values.pp_number (Css.Num value)
          in
          try Css.Declaration.with_value declaration value
          with Cascade.Cursor.Parse_error _ | Invalid_argument _ ->
            declaration)
      | Some _ | None -> declaration)
  | None -> declaration

let fold_static_number_math css =
  match Css.of_string css with
  | Ok { stylesheet; warnings = []; _ } ->
      Css.Stylesheet.map_declarations
        (List.map fold_declaration_static_number_math)
        stylesheet
      |> Css.to_string ~minify:true |> String.trim
  | Ok _ | Error _ -> css

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

(* Tailwind writes the [color-mix] fallback percentage as a bare number:
   [color-mix(in srgb, red .5, transparent)]. CSS Color 5 sec. 3.1 admits only a
   [<percentage>] there, so a browser drops that declaration and cascade's
   reader refuses it, which takes the whole declaration off the expected side
   and reports tw's own (valid) [50%] as an addition. The two spell one value,
   so the fixture's spelling is normalised to the percentage before either side
   is parsed. Only a bare number directly before the closing paren or a comma
   inside [color-mix(...)] is touched. *)
let color_mix_percentage =
  let mix = Re.Pcre.regexp {|\bcolor-mix\(([^()]*)\)|} in
  let bare = Re.Pcre.regexp {|( )(\.\d+|0?\.\d+)(\s*[,)]|\s*$)|} in
  fun s ->
    Re.replace mix s ~f:(fun g ->
        let args = Re.Group.get g 1 in
        let args =
          Re.replace bare args ~f:(fun d ->
              let n = float_of_string (Re.Group.get d 2) in
              Fmt.str "%s%g%%%s" (Re.Group.get d 1) (n *. 100.)
                (Re.Group.get d 3))
        in
        "color-mix(" ^ args ^ ")")

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

(* Guards [scheme_of_theme_vars]: a value that only ever appears in the expected
   CSS cannot reach tw as a scheme override, so a wrong built-in default still
   fails the case. Only the fixture's own [@theme] declarations get through. *)
let test_scheme_from_declared_tokens_only () =
  let scheme =
    scheme_of_theme_vars
      [
        ("spacing-4", "1rem");
        ("radius-full", "9999px");
        ("default-border-width", "2px");
      ]
  in
  Alcotest.(check int)
    "the declared border width is read" 2 scheme.Tw.Scheme.default_border_width;
  Alcotest.(check int)
    "an undeclared ring width keeps the built-in default"
    Tw.Scheme.default.default_ring_width scheme.Tw.Scheme.default_ring_width;
  Alcotest.(check int)
    "an undeclared outline width keeps the built-in default"
    Tw.Scheme.default.default_outline_width
    scheme.Tw.Scheme.default_outline_width;
  Alcotest.(check bool)
    "the declared radius is read" true
    (Option.equal Css.Values.equal_length
       (Tw.Scheme.radius scheme "full")
       (Some (Css.Px 9999.)));
  Alcotest.(check bool)
    "an undeclared radius stays absent" false
    (Tw.Scheme.has_explicit_radius scheme "sm");
  Alcotest.(check bool)
    "the declared spacing step is read" true
    (Option.equal Css.Values.equal_length
       (Tw.Scheme.spacing scheme 4)
       (Some (Css.Rem 1.)));
  Alcotest.(check bool)
    "an undeclared spacing step stays absent" false
    (Tw.Scheme.has_explicit_spacing scheme 8)

(* Guards [theme_resolution]: what gets inlined before the diff is decided by
   the case's [@config] and its own declarations, not by the names the expected
   CSS happens to spell. The keep-set used to be every [--name] in the expected
   bytes, so a reference tw emitted where Tailwind emitted a literal was inlined
   off tw's side first and the case could not fail -- the one behaviour the
   [Var] system exists to get right, never compared. *)
let test_reference_survives_theme_resolution () =
  let stylesheet =
    match
      Css.of_string
        ".x { transition-duration: var(--default-transition-duration) }"
    with
    | Ok { stylesheet; _ } -> stylesheet
    | Error _ -> Alcotest.fail "the fixture stylesheet does not parse"
  in
  (* An expected CSS that spells no variable at all: under the old keep-set
     every reference was inlined away. *)
  let rendered ~declared =
    Css.to_string ~minify:true (theme_resolution ~declared Theme "" stylesheet)
  in
  let references css =
    Re.execp (Re.compile (Re.str "var(--default-transition-duration)")) css
  in
  Alcotest.(check bool)
    "a token the case declares keeps tw's reference" true
    (references (rendered ~declared:[ "default-transition-duration" ]));
  Alcotest.(check bool)
    "a token the case does not declare is inlined, as @theme renders it" false
    (references (rendered ~declared:[]))

(* Guards [Test_helpers.check_no_dropped_declarations], which every comparison
   in the runner goes through: a declaration the reader rejects is dropped from
   that side's AST, so the diff compares less than it appears to. Only
   Tailwind's own bare-number [color-mix] amount is let through. *)
let test_dropped_declarations_are_reported () =
  let reported declaration =
    Css_compare.diff ~mode:`Canonical
      (Fmt.str ".x{%s}" declaration)
      ".x{color:red}"
    |> Test_helpers.dropped_declarations <> []
  in
  Alcotest.(check bool)
    "a declaration the reader drops is reported" true (reported "width:12quux");
  Alcotest.(check bool)
    "Tailwind's bare-number color-mix amount is allowed through" false
    (reported "color:color-mix(in srgb, red .5, transparent)")

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
let stat_routed = ref 0
let stat_expected_empty_cases = ref 0

let run_test_case test expected () =
  if test.classes = [] then ()
  else
    let base_scheme = scheme_of_theme_vars test.theme_vars in
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
    (* The case's own [@custom-variant]s ride on the scheme threaded to
       [Tw.of_string] and [Tw.to_css] below, so they are local to this case
       rather than left in a registry the next one reads. Its custom breakpoints
       arrive the same way, through the [@theme] token overrides applied just
       below. *)
    let scheme =
      {
        base_scheme with
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
      (* [@theme inline] and [@theme reference] change how a token reads, not
         what it is: an inline one stands for its value at the use site, a
         reference one is declared elsewhere and carries its value as the
         fallback of the reference. A case can put two tokens of one namespace
         in blocks that differ, so the modes come from the token's own block
         rather than from the case's [@config]. *)
      let tokens_in_mode mode =
        List.filter_map
          (fun (name, modes) -> if List.mem mode modes then Some name else None)
          test.theme_modes
      in
      let inline = tokens_in_mode "inline" in
      let reference = tokens_in_mode "reference" in
      Tw.Scheme.with_overrides ~inline ~reference scheme
        (theme_overrides_of ~declared test.config expected @ theme_vars)
    in
    let resolve_theme = theme_resolution ~declared test.config expected in
    (* A class the case's own [@utility] declares means nothing to
       [Tw.of_string]: the declaration is CSS, and it reaches the sheet the way
       a project entrypoint's does, through [Entrypoint]. The rest of the case
       still compiles class by class, so the two sets are generated separately
       and sorted together by [~extra]. With no declarations the partition is
       empty and this is the plain per-class path. *)
    let udefs = test.utility_defs in
    let routed, direct =
      List.partition (Entrypoint.is_custom_routed ~defs:[] ~udefs) test.classes
    in
    let parsed, rejected =
      List.fold_left
        (fun (ok, bad) cls ->
          match Tw.of_string ~theme:scheme cls with
          | Ok u -> (u :: ok, bad)
          | Error (`Msg m) -> (ok, (cls, m) :: bad))
        ([], []) direct
    in
    let utilities = List.rev parsed in
    let rejected = List.rev rejected in
    let routed_count, routed_extra, routed_stmts =
      if routed = [] then (0, [], [])
      else
        Entrypoint.custom_routed_utilities ~theme:scheme ~defs:[] ~udefs routed
    in
    stat_total_classes := !stat_total_classes + List.length test.classes;
    stat_parsed := !stat_parsed + List.length utilities;
    stat_rejected := !stat_rejected + List.length rejected;
    stat_routed := !stat_routed + routed_count;
    if expected = "" then incr stat_expected_empty_cases;
    let our_stylesheet =
      if utilities = [] && routed_extra = [] && routed_stmts = [] then None
      else
        let layers = Option.is_some test.layer_wrap in
        let sheet =
          Entrypoint.place_routed routed_stmts
            (Tw.to_css ~theme:scheme ~base:false ~layers ~extra:routed_extra
               utilities)
        in
        let sheet =
          match test.layer_wrap with
          | None -> sheet
          | Some name ->
              keep_only_layer ~before_theme:test.layer_before_theme name sheet
        in
        Some (drop_theme_keyframes sheet)
    in
    let our_css =
      match our_stylesheet with
      | None -> ""
      | Some stylesheet ->
          stylesheet |> resolve_theme |> Css.to_string ~minify:true
          |> String.trim |> drop_redundant_vendor_prefixes
    in
    let expected_css =
      canonical_stylesheet_css expected |> drop_redundant_vendor_prefixes
    in
    if our_css = "" && expected = "" then ()
    else
      let normalize_colors s =
        s |> fold_static_number_math |> color_mix_percentage
        |> color_mix_to_oklab |> truncate_color_precision
      in
      let result =
        Tw_tools.Parity_compare.diff ~mode:`Canonical
          (normalize_colors expected_css)
          (normalize_colors our_css)
      in
      Test_helpers.check_no_dropped_declarations
        ~test_name:(String.concat " " test.classes)
        result;
      if
        match result.Css_compare.result with
        | Css_compare.No_diff -> true
        | _ -> false
      then ()
      else
        let buf = Buffer.create 1024 in
        Css_compare.pp ~expected:"Tailwind" ~actual:"Our TW" buf result;
        let got =
          match our_stylesheet with
          | None -> ""
          | Some stylesheet ->
              stylesheet |> resolve_theme |> Css.to_string ~indent:2
              |> String.trim
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

(* Guards [color_mix_percentage]: the bare-number mixing percentage Tailwind
   writes in its [color-mix] fallback becomes the percentage CSS Color 5 sec.
   3.1 requires, and nothing else in the function is touched. *)
let test_color_mix_percentage () =
  let check msg expected input =
    Alcotest.(check string) msg expected (color_mix_percentage input)
  in
  check "bare fraction becomes a percentage"
    "color-mix(in srgb, red 50%, transparent)"
    "color-mix(in srgb, red .5, transparent)";
  check "a percentage is left alone" "color-mix(in srgb, red 50%, transparent)"
    "color-mix(in srgb, red 50%, transparent)";
  (* The [var()] fallback spelling carries its own bare number and is a value,
     not a mixing percentage, so it stays as written. *)
  check "var fallback untouched"
    "color-mix(in oklab, red var(--opacity-half, .5), transparent)"
    "color-mix(in oklab, red var(--opacity-half, .5), transparent)";
  check "text outside color-mix untouched" "opacity: .5" "opacity: .5"

let test_redundant_vendor_prefixes () =
  let check msg expected input =
    Alcotest.(check string) msg expected (drop_redundant_vendor_prefixes input)
  in
  check "identical prefixed declarations are redundant"
    ".x{mask-image:var(--x)}"
    ".x{-webkit-mask-image:var(--x);-webkit-mask-image:var(--x);mask-image:var(--x)}";
  check "a mapped prefix remains observable"
    ".x{-webkit-mask-composite:source-in;mask-composite:intersect}"
    ".x{-webkit-mask-composite:source-in;mask-composite:intersect}";
  check "a prefix with no unprefixed declaration remains observable"
    ".x{-webkit-mask-image:var(--x)}" ".x{-webkit-mask-image:var(--x)}"

let test_static_number_math () =
  let check msg expected input =
    Alcotest.(check string) msg expected (fold_static_number_math input)
  in
  check "static division is evaluated" ".x{line-height:1.33333}"
    ".x{line-height:calc(1 / 0.75)}";
  check "the serializer's six significant figures are used"
    ".x{opacity:.333333}" ".x{opacity:calc(1 / 3)}";
  check "static addition is evaluated" ".x{opacity:3}" ".x{opacity:calc(1 + 2)}";
  check "dimensional math is not a number" ".x{width:calc(1px + 2px)}"
    ".x{width:calc(1px + 2px)}";
  check "a variable remains observable" ".x{opacity:calc(var(--x)/2)}"
    ".x{opacity:calc(var(--x) / 2)}";
  check "another CSS function remains observable" ".x{color:rgb(1 2 3)}"
    ".x{color:rgb(1 2 3)}";
  check "a function in a string remains observable"
    ".x{content:\"calc(1 / 3)\"}" ".x{content:\"calc(1 / 3)\"}"

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

(* The reader's own grammar. Both fixtures are machine-written, so a line the
   grammar has no place for means [extract_tests.ml] and [upstream_fixture.ml]
   have drifted apart, or the fixture was edited by hand. The reader raises on
   one instead of resuming its scan, which would drop the block the line sits in
   and leave the case count as the only trace. *)
let write_reader_regression_fixture name contents =
  let dir = "tmp" in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let path = Filename.concat dir name in
  let oc = open_out path in
  output_string oc contents;
  close_out oc;
  path

let reader_regression_banner =
  "#! 1 block extracted from variants.test.ts by extract_tests.exe -- do not \
   edit\n"

let test_reader_keeps_a_compile_error_block () =
  let path =
    write_reader_regression_fixture "reader_compile_error.txt"
      (reader_regression_banner
     ^ "# a case that throws\n@config run\nfoo bar\n<<<>>>\n")
  in
  Alcotest.(check int) "one case" 1 (List.length (read path))

let test_reader_rejects_a_stray_line () =
  let path =
    write_reader_regression_fixture "reader_stray.txt"
      (reader_regression_banner
     ^ "# a case\n\
        @config run\n\
        flex\n\
        stray\n\
        ---\n\
        .flex { display: flex }\n\
        <<<>>>\n")
  in
  match read path with
  | exception _ -> ()
  | cases ->
      Alcotest.failf "read %d cases instead of rejecting the stray line"
        (List.length cases)

let banner =
  "#! 1 block extracted from variants.test.ts by extract_tests.exe -- do not \
   edit\n"

let read_result path =
  match read path with
  | cases -> Ok cases
  | exception Malformed msg -> Error msg

let check_rejected name contents =
  let path =
    write_reader_regression_fixture (name ^ ".txt") (banner ^ contents)
  in
  match read_result path with
  | Error _ -> ()
  | Ok cases ->
      Alcotest.failf "%s: read %d cases instead of raising" name
        (List.length cases)

let test_reader_reads_a_block () =
  let path =
    write_reader_regression_fixture "reader_good.txt"
      (banner
     ^ "# a case\n@config run\nflex\n---\n.flex { display: flex }\n<<<>>>\n")
  in
  match read_result path with
  | Error msg -> Alcotest.failf "a well-formed block was rejected: %s" msg
  | Ok [ case ] ->
      Alcotest.(check (list string)) "classes" [ "flex" ] case.classes;
      Alcotest.(check (option string))
        "expected CSS" (Some ".flex { display: flex }") case.expected
  | Ok cases -> Alcotest.failf "one block read as %d cases" (List.length cases)

(* Upstream tests that assert the compile throws are written without a [---]
   section. They are a shape the extractor produces, so the reader keeps them
   and says so rather than dropping them the way it drops nothing else. *)
let test_reader_keeps_a_block_asserting_an_error () =
  let path =
    write_reader_regression_fixture "reader_throws.txt"
      (banner ^ "# a case that throws\n@config run\nfoo bar\n<<<>>>\n")
  in
  match read_result path with
  | Error msg -> Alcotest.failf "a block with no --- was rejected: %s" msg
  | Ok [ case ] ->
      Alcotest.(check (option string)) "no expected CSS" None case.expected
  | Ok cases -> Alcotest.failf "one block read as %d cases" (List.length cases)

let test_reader_rejects_a_stray_line_with_message () =
  check_rejected "reader_stray"
    "# a case\n@config run\nflex\nstray\n---\n.flex { display: flex }\n<<<>>>\n"

let test_reader_rejects_an_unknown_config () =
  check_rejected "reader_config"
    "# a case\n@config bogus\nflex\n---\n.flex { display: flex }\n<<<>>>\n"

let test_reader_rejects_a_headerless_block () =
  check_rejected "reader_headerless"
    "@config run\nflex\n---\n.flex { display: flex }\n<<<>>>\n"

let test_reader_rejects_an_unclosed_block () =
  check_rejected "reader_unclosed"
    "# a case\n\
     @config run\n\
     flex\n\
     ---\n\
     .flex { display: flex }\n\
     <<<>>>\n\
     # added by hand\n\
     @config run\n\
     block\n\
     ---\n\
     .block { display: block }\n"

let test_reader_rejects_a_directive_with_no_value () =
  check_rejected "reader_directive"
    "# a case\n\
     @config run\n\
     @theme-var spacing\n\
     flex\n\
     ---\n\
     .flex { display: flex }\n\
     <<<>>>\n"

let print_parity_report () =
  Fmt.epr "@.=== upstream parity report ===@.";
  Fmt.epr "classes: %d total, %d parsed, %d routed, %d rejected@."
    !stat_total_classes !stat_parsed !stat_routed !stat_rejected;
  Fmt.epr "cases with empty expected CSS: %d@." !stat_expected_empty_cases;
  Fmt.epr
    "(a routed class is one the case's own @utility declares, compiled through \
     Entrypoint rather than of_string)@.";
  Fmt.epr
    "(a rejection is harmless when the case's CSS still matches; one that \
     breaks parity fails the CSS diff and names the rejected classes)@.";
  Fmt.epr "==============================@."

(* All three fixtures are checked in and declared as dune deps, so a missing one
   is a broken checkout rather than an optional extra. A floor on the parsed
   cases catches the other way this gate can go quiet: a fixture whose format
   drifts still parses, just into far fewer cases than it holds.

   Set near the real counts rather than at half. Half (300 and 80) let a fixture
   lose most of itself and still pass, which is the drift the floor is for; a
   regenerated corpus that legitimately shrinks past these wants a human to look
   and move the number. Counts on 2026-08-29: 696 utilities, 166 variants. *)
let utilities_floor = 620
let variants_floor = 150

(* A block the extractor writes without a [---] section is an upstream test that
   asserts the compile throws: it carries no CSS to replay. Those are the only
   blocks that do not become a test, and the count is printed so every block of
   the fixture is accounted for either way. *)
let load basename floor =
  match path basename with
  | None ->
      Fmt.epr "%s not found. All three fixtures are checked in here.@." basename;
      exit 1
  | Some p ->
      let cases = read p in
      (* [read] returns one case per block or raises, so a mismatch here is the
         reader having grown a skip that says nothing. *)
      let found = blocks p in
      if List.length cases <> found then (
        Fmt.epr "%s holds %d blocks but read as %d cases.@." p found
          (List.length cases);
        exit 1);
      let replayable =
        List.filter_map
          (fun c -> Option.map (fun expected -> (c, expected)) c.expected)
          cases
      in
      let n = List.length replayable in
      Fmt.epr "%s: %d blocks, %d replayed, %d asserting a compile error@." p
        found n (found - n);
      if n < floor then (
        Fmt.epr "%s yielded %d test cases, fewer than the floor of %d.@." p n
          floor;
        exit 1);
      replayable

(* The floors count what a fixture holds, not where it came from: a block added
   to a generated fixture by hand raises the count and passes them, then
   disappears at the next regeneration with nothing to say it went. The
   extractor writes the number of blocks it produced into the file's banner, so
   counting the blocks back names such a block while it is still there. *)
let check_generated basename =
  match path basename with
  | None -> ()
  | Some p -> (
      match declared_blocks p with
      | None ->
          Fmt.epr
            "%s has no provenance banner. Regenerate it with \
             extract_tests.exe.@."
            p;
          exit 1
      | Some declared ->
          let found = blocks p in
          if found <> declared then (
            Fmt.epr
              "%s holds %d blocks but was generated with %d, so it has been \
               edited since. A case upstream does not have is a tw regression \
               test and belongs in its own test_<module>.ml.@."
              p found declared;
            exit 1))

let () =
  Tw_tools.Cascade_provenance.report ();
  check_generated "utilities.txt";
  check_generated "variants.txt";
  let utility_tests = load "utilities.txt" utilities_floor in
  let variant_tests = load "variants.txt" variants_floor in
  let total = List.length utility_tests + List.length variant_tests in
  Fmt.epr "Running %d upstream tests (%d utilities, %d variants)...@." total
    (List.length utility_tests)
    (List.length variant_tests);
  at_exit print_parity_report;

  let alcotest_cases tests =
    List.map
      (fun (tc, expected) ->
        test_case tc.name `Quick (run_test_case tc expected))
      tests
  in
  let tolerance_cases =
    [
      test_case "oklab precision truncation" `Quick test_color_tolerance;
      test_case "color-mix percentage" `Quick test_color_mix_percentage;
      test_case "redundant vendor prefixes" `Quick
        test_redundant_vendor_prefixes;
      test_case "static number math" `Quick test_static_number_math;
      test_case "the theme echo is limited to declared tokens" `Quick
        test_echo_only_declared_tokens;
      test_case "the scheme is built from declared tokens only" `Quick
        test_scheme_from_declared_tokens_only;
      test_case "a tw reference survives theme resolution" `Quick
        test_reference_survives_theme_resolution;
      test_case "a dropped declaration is reported" `Quick
        test_dropped_declarations_are_reported;
    ]
  in
  let reader_regression_cases =
    [
      test_case "a compile-error block keeps its place" `Quick
        test_reader_keeps_a_compile_error_block;
      test_case "a stray fixture line is rejected" `Quick
        test_reader_rejects_a_stray_line;
    ]
  in
  let reader_cases =
    [
      test_case "a well-formed block reads" `Quick test_reader_reads_a_block;
      test_case "a block with no --- keeps its place" `Quick
        test_reader_keeps_a_block_asserting_an_error;
      test_case "a stray line raises" `Quick
        test_reader_rejects_a_stray_line_with_message;
      test_case "an unknown @config raises" `Quick
        test_reader_rejects_an_unknown_config;
      test_case "a block with no header raises" `Quick
        test_reader_rejects_a_headerless_block;
      test_case "a block with no <<<>>> raises" `Quick
        test_reader_rejects_an_unclosed_block;
      test_case "a directive with no value raises" `Quick
        test_reader_rejects_a_directive_with_no_value;
    ]
  in
  let suites =
    [
      ("utilities", alcotest_cases utility_tests);
      ("tolerance", tolerance_cases);
      ("reader regression", reader_regression_cases);
      ("reader", reader_cases);
      ("variants", alcotest_cases variant_tests);
    ]
  in
  Alcotest.run "upstream" suites
