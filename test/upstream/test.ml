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
    (Tw.Scheme.radius scheme "full" = Some (Css.Px 9999.));
  Alcotest.(check bool)
    "an undeclared radius stays absent" false
    (Tw.Scheme.has_explicit_radius scheme "sm");
  Alcotest.(check bool)
    "the declared spacing step is read" true
    (Tw.Scheme.spacing scheme 4 = Some (Css.Rem 1.));
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
let stat_expected_empty_cases = ref 0

let run_test_case test () =
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
      Tw.Scheme.with_overrides scheme
        (theme_overrides_of ~declared test.config test.expected @ theme_vars)
    in
    let resolve_theme = theme_resolution ~declared test.config test.expected in
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
      else
        Some
          (drop_theme_keyframes
             (Tw.to_css ~theme:scheme ~base:false ~layers:false utilities))
    in
    let our_css =
      match our_stylesheet with
      | None -> ""
      | Some stylesheet ->
          stylesheet |> resolve_theme |> Css.to_string ~minify:true
          |> String.trim
    in
    let expected = test.expected in
    let expected_css = canonical_stylesheet_css expected in
    if our_css = "" && expected = "" then ()
    else
      let normalize_colors s =
        truncate_color_precision (color_mix_to_oklab (color_mix_percentage s))
      in
      let result =
        Css_compare.diff ~mode:`Canonical ~prune_unused_custom_props:true
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
   still parses, just into far fewer cases than it holds.

   Set near the real counts rather than at half. Half (300 and 80) let a fixture
   lose most of itself and still pass, which is the drift the floor is for; a
   regenerated corpus that legitimately shrinks past these wants a human to look
   and move the number. Counts on 2026-08-29: 627 utilities, 168 variants. *)
let utilities_floor = 560
let variants_floor = 150

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
  Tw_tools.Cascade_provenance.report ();
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
      test_case "color-mix percentage" `Quick test_color_mix_percentage;
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
  let suites =
    [
      ("utilities", utility_cases);
      ("tolerance", tolerance_cases);
      ("variants", variant_cases);
    ]
  in
  Alcotest.run "upstream" suites
