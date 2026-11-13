module Css = Cascade.Css
open Alcotest

let check_utilities = Test_helpers.check_utilities

(* One token reads into the modifier it names, and a token naming none reads as
   nothing, so [Modifiers.apply] can refuse the whole class. A named group holds
   the state it names, not the unnamed [group-] modifier. *)
let test_modifier_reads_one_token () =
  let theme = Tw.Scheme.default in
  let reads token expected =
    check bool token true
      (Tw.Variant_parse.modifier ~theme token = Some expected)
  in
  reads "hover" Tw.Style.Hover;
  reads "md" (Tw.Style.Responsive `Md);
  reads "not-hover" (Tw.Style.Not Tw.Style.Hover);
  reads "group-hover/item" (Tw.Style.Named_group (Tw.Style.Hover, "item"));
  check bool "unknown" true (Tw.Variant_parse.modifier ~theme "unknown" = None);
  check bool "split name" true
    (Tw.Variant_parse.split_name "checked/name" = ("checked", Some "name"));
  check bool "aria shorthand" true
    (Tw.Variant_parse.is_aria_shorthand "checked");
  check bool "not an aria shorthand" false
    (Tw.Variant_parse.is_aria_shorthand "sort")

(* The bracket holds a length, so a word is not a breakpoint at all. *)
let test_arbitrary_breakpoint_rejects_non_length () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "min-[abc]:flex"; "max-[abc]:flex"; "min-[]:flex" ]

(* The number in front of the unit is a CSS number, not an OCaml one. Reading it
   with [float_of_string_opt] turned [min-[0x600px]] into a live [(min-width:
   1536px)] breakpoint: a query the browser honours, built out of a value
   Tailwind passes through verbatim for the browser to drop. Nothing here is a
   length either, so tw declines the whole class. *)
let test_arbitrary_breakpoint_rejects_ocaml_literals () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "%s parsed as %s emitting %s" cls (Tw.pp u)
            (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
      | Error (`Msg _) -> ())
    [
      "min-[0x600px]:p-4";
      "max-[0x600px]:p-4";
      "min-[0x600]:p-4";
      "min-[0o17px]:p-4";
      "min-[1_000px]:p-4";
    ]

(* A [@custom-variant] belongs to the [@theme] block that declared it. Two
   stylesheets built in one process read different themes, so a variant the
   first declared must be unknown to the second. *)
let test_custom_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      custom_variants =
        [
          ( "is-data",
            Tw.Scheme.{ values = [ ("", "&[data-x]") ]; template = "&:is({})" }
          );
        ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "is-data:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "is-data:flex"))

(* Same for a [@custom-variant] whose body is a container query: it is held in
   its own field, and belongs to its own theme just the same. *)
let test_container_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      container_variants = [ ("has-a", Css.Container.of_string "style(--a)") ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "has-a:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "has-a:flex"))

(* A [@theme] block that clears [--breakpoint-*] takes the built-in responsive
   variants with it: [md:] then names a breakpoint the project no longer has,
   and the candidate stops resolving the way an unknown variant does. The
   [min-], [max-] and [not-] spellings read the same breakpoint, so they go too,
   while a breakpoint the block declared for itself still resolves. *)
let test_removed_breakpoint_drops_its_variants () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("breakpoint-*", "initial"); ("breakpoint-tablet", "40rem") ]
  in
  List.iter
    (fun cls ->
      check bool
        (cls ^ " names a breakpoint the theme removed")
        true
        (Result.is_error (Tw.of_string ~theme cls)))
    [ "md:flex"; "min-md:flex"; "max-md:flex"; "not-md:flex" ];
  check bool "the theme's own breakpoint still resolves" true
    (Result.is_ok (Tw.of_string ~theme "tablet:flex"))

(* [--breakpoint-md: initial] removes the one breakpoint, and Tailwind emits
   nothing for [md:flex]. The built-in table let it go, and the custom
   breakpoints then took it back: [initial] reads as a length, so the removed
   token was a custom breakpoint of that width, and the sheet carried [@media
   (min-width: initial)], a query no browser honours. *)
let test_removed_single_breakpoint_drops_its_variants () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("breakpoint-md", "initial") ]
  in
  List.iter
    (fun cls ->
      check bool
        (cls ^ " names a breakpoint the theme removed")
        true
        (Result.is_error (Tw.of_string ~theme cls)))
    [ "md:flex"; "min-md:flex"; "max-md:flex"; "not-md:flex" ];
  check bool "the other breakpoints still resolve" true
    (Result.is_ok (Tw.of_string ~theme "lg:flex"))

(* The bracket is negated as a selector, so content the selector grammar cannot
   read is not a negation and the class is refused at parse time. The reader
   raises rather than answering, and it stops at the first thing it cannot use,
   so a trailing remainder is refused too rather than silently dropped. *)
let test_not_bracket_unreadable_selector_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [
      "not-[a;b]:flex";
      "not-[0.5ch]:flex";
      "not-[1px}]:flex";
      "not-[a{b]:flex";
      "not-[*/]:flex";
      "not-[<value>]:flex";
      "not-[url(a;b)]:flex";
      "not-[()]:flex";
    ];
  (* the readable ones still parse *)
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok _ -> ()
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m)
    [
      "not-[.foo]:flex";
      "not-[.os-macos_&]:flex";
      "not-[:checked]:flex";
      "not-[@media_print]:flex";
    ]

(* An nth-* bracket holds an An+B expression and a supports- bracket holds an
   @supports condition; both are taken verbatim, so content the CSS grammar has
   no production for escapes as a parse error out of the selector or condition
   reader. They are validated where data-[ and has-[ are. *)
let test_invalid_bracket_modifiers () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "nth-[]:flex";
  rejected "nth-[abc]:flex";
  rejected "nth-last-[]:flex";
  rejected "nth-of-type-[]:flex";
  rejected "nth-last-of-type-[]:flex";
  rejected "supports-[]:flex"

(* An aria- or data- variant names an attribute, so an empty argument would
   compile to [[aria-]] or [[data-]]: a selector browsers parse and keep, and
   nothing ever matches. An underscore stands for a space, so it is empty
   too. *)
let test_empty_attribute_brackets () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "aria-[]:flex";
  rejected "aria-[_]:flex";
  rejected "aria-_:flex";
  rejected "group-aria-[]:flex";
  rejected "peer-aria-[]:flex";
  rejected "data-[]:flex";
  rejected "data-[_]:flex";
  rejected "group-data-[]:flex";
  rejected "peer-data-[]:flex";
  rejected "group-data-_:flex";
  rejected "peer-data-_:flex"

let test_padded_attribute_brackets () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "aria-[_modal_]:flex";
  rejected "data-[_state=open_]:flex"

(* The attribute spellings that do name something keep working, including the
   empty-name-with-value form Tailwind also accepts. *)
let test_attribute_brackets_still_parse () =
  check_utilities "aria-[modal]:flex"
    {|.aria-\[modal\]\:flex[aria-modal]{display:flex}|};
  check_utilities "aria-[sort=ascending]:flex"
    {|.aria-\[sort\=ascending\]\:flex[aria-sort=ascending]{display:flex}|};
  check_utilities "aria-[=true]:flex"
    {|.aria-\[\=true\]\:flex[aria-=true]{display:flex}|};
  check_utilities "data-[state=open]:flex"
    {|.data-\[state\=open\]\:flex[data-state=open]{display:flex}|};
  check_utilities "group-data-[dragging]:flex"
    {|.group-data-\[dragging\]\:flex:is(:where(.group)[data-dragging] *){display:flex}|};
  check_utilities "peer-data-[dragging]:flex"
    {|.peer-data-\[dragging\]\:flex:is(:where(.peer)[data-dragging]~*){display:flex}|};
  check_utilities "group-data-modified:flex"
    {|.group-data-modified\:flex:is(:where(.group)[data-modified] *){display:flex}|}

(* [parse_data_expr] reads the [data-[...]] bracket body into an attribute
   match: every operator ([$=], [^=], [*=], [~=], [|=], bare [=]), the
   presence-only bare form, a quoted value holding a decoded space, and a
   trailing case-sensitivity flag. This pins the selector each spelling produces
   today so a rewrite of the reader cannot silently change one. *)
let test_data_bracket_operators () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  check_utilities "data-[size~=large]:flex"
    {|.data-\[size\~\=large\]\:flex[data-size~=large]{display:flex}|};
  check_utilities "data-[foo=bar]:flex"
    {|.data-\[foo\=bar\]\:flex[data-foo=bar]{display:flex}|};
  check_utilities "data-[foo^=bar]:flex"
    {|.data-\[foo\^\=bar\]\:flex[data-foo^=bar]{display:flex}|};
  check_utilities "data-[foo$=bar]:flex"
    {|.data-\[foo\$\=bar\]\:flex[data-foo$=bar]{display:flex}|};
  check_utilities "data-[foo*=bar]:flex"
    {|.data-\[foo\*\=bar\]\:flex[data-foo*=bar]{display:flex}|};
  check_utilities "data-[foo|=bar]:flex"
    {|.data-\[foo\|\=bar\]\:flex[data-foo|=bar]{display:flex}|};
  check_utilities "data-[open]:flex"
    {|.data-\[open\]\:flex[data-open]{display:flex}|};
  (* A decoded space in the value stays on a substring: tw quotes the attribute
     value where the pinned CLI escapes the space instead ([data-foo=a\ b]). *)
  emits {|[data-foo="a b"]|} "data-[foo='a_b']:flex";
  check_utilities "data-[foo=bar_i]:flex"
    {|.data-\[foo\=bar_i\]\:flex[data-foo=bar i]{display:flex}|};
  check_utilities "data-[foo=bar_s]:flex"
    {|.data-\[foo\=bar_s\]\:flex[data-foo=bar s]{display:flex}|};
  check_utilities "group-data-[size~=large]:flex"
    {|.group-data-\[size\~\=large\]\:flex:is(:where(.group)[data-size~=large] *){display:flex}|};
  check_utilities "peer-data-[size~=large]:flex"
    {|.peer-data-\[size\~\=large\]\:flex:is(:where(.peer)[data-size~=large]~*){display:flex}|}

(* A bare [[...]] variant compounds its selector onto the utility's own class,
   so what the brackets hold has to be a compound selector. [~] is both the
   sibling combinator and the [~=] whitespace-list attribute operator, and only
   reading the bracket as a selector tells them apart: a character scan that
   rejects every [~] rejects [[data-size~=large]] along with [p_~_span]. *)
let test_bare_selector_variant_attribute_operators () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  check_utilities "[[data-size~=large]]:underline"
    {|.\[\[data-size\~\=large\]\]\:underline[data-size~=large]{text-decoration-line:underline}|};
  check_utilities "group-[[data-size~=large]]:underline"
    {|.group-\[\[data-size\~\=large\]\]\:underline:is(:where(.group)[data-size~=large] *){text-decoration-line:underline}|};
  check_utilities "peer-[[data-size~=large]]:underline"
    {|.peer-\[\[data-size\~\=large\]\]\:underline:is(:where(.peer)[data-size~=large]~*){text-decoration-line:underline}|};
  (* the attribute operators that never collided with a combinator stay put *)
  check_utilities "[[lang|=en]]:underline"
    {|.\[\[lang\|\=en\]\]\:underline[lang|=en]{text-decoration-line:underline}|};
  check_utilities "[[href^=https]]:underline"
    {|.\[\[href\^\=https\]\]\:underline[href^=https]{text-decoration-line:underline}|};
  (* a combinator really is one, and none of these is a compound *)
  rejected "[p_~_span]:underline";
  rejected "[>img]:underline";
  rejected "[.a_.b]:underline"

(* The valid spellings the validation must keep accepting. *)
let test_valid_bracket_modifiers () =
  check_utilities "nth-[2n+1]:flex"
    {|.nth-\[2n\+1\]\:flex:nth-child(odd){display:flex}|};
  check_utilities "nth-[3]:flex" {|.nth-\[3\]\:flex:nth-child(3){display:flex}|};
  check_utilities "nth-last-[2n]:flex"
    {|.nth-last-\[2n\]\:flex:nth-last-child(2n){display:flex}|};
  check_utilities "nth-of-type-[odd]:flex"
    {|.nth-of-type-\[odd\]\:flex:nth-of-type(odd){display:flex}|};
  check_utilities "supports-[display:grid]:flex"
    {|@supports(display:grid){.supports-\[display\:grid\]\:flex{display:flex}}|}

(* A [supports-[...]] condition that is not a bare [prop:value] pair goes to the
   condition reader whole. [not(display:grid)] was split at its colon into the
   property [not(display] and the value [grid)], which cascade refused with a
   [Failure] out of a pure render, exit 125. Tailwind 4.3.3 writes [@supports
   not (display:grid)], spacing a keyword the function-token spelling would hide
   from the [@supports] grammar, and passes [selector(...)] through. A condition
   the grammar has no production for is refused, not raised. *)
let test_supports_condition_text () =
  check_utilities "supports-[not(display:grid)]:flex"
    {|@supports not (display:grid){.supports-\[not\(display\:grid\)\]\:flex{display:flex}}|};
  check_utilities "supports-[not_(display:grid)]:flex"
    {|@supports not (display:grid){.supports-\[not_\(display\:grid\)\]\:flex{display:flex}}|};
  check_utilities "supports-[selector(:has(a))]:flex"
    {|@supports selector(:has(a)){.supports-\[selector\(\:has\(a\)\)\]\:flex{display:flex}}|};
  match Tw.of_string "supports-[display:)]:flex" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "supports-[display:)]:flex should be refused"

(* A class that parses but renders no rule at all is worse than one that is
   refused: a typo in a variant name stays silent. Tailwind emits nothing for
   these four either, so rejecting them moves no rendered output - it only turns
   silence into a message. [group-not-hover] is the shape: [rule.ml] already
   knows a group negation cannot wrap a media query and answers with no rules,
   so the parser should not have accepted it. *)
let test_silent_empty_variants_rejected () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "group-not-hover:flex";
  rejected "group-not-device-hocus:flex";
  rejected "peer-not-hover:flex";
  (* The negations that do have a selector form still parse. *)
  check bool "group-not-checked still parses" true
    (Result.is_ok (Tw.of_string "group-not-checked:flex"));
  check bool "not-hover still parses" true
    (Result.is_ok (Tw.of_string "not-hover:flex"))

let tests =
  [
    test_case "arbitrary breakpoint rejects non-length" `Quick
      test_arbitrary_breakpoint_rejects_non_length;
    test_case "arbitrary breakpoint rejects OCaml literals" `Quick
      test_arbitrary_breakpoint_rejects_ocaml_literals;
    test_case "custom variant is theme-local" `Quick
      test_custom_variant_is_theme_local;
    test_case "container variant is theme-local" `Quick
      test_container_variant_is_theme_local;
    test_case "removed breakpoint drops its variants" `Quick
      test_removed_breakpoint_drops_its_variants;
    test_case "a removed single breakpoint drops its variants" `Quick
      test_removed_single_breakpoint_drops_its_variants;
    test_case "invalid bracket modifiers" `Quick test_invalid_bracket_modifiers;
    test_case "empty attribute brackets" `Quick test_empty_attribute_brackets;
    test_case "padded attribute brackets" `Quick test_padded_attribute_brackets;
    test_case "attribute brackets still parse" `Quick
      test_attribute_brackets_still_parse;
    test_case "data bracket operators" `Quick test_data_bracket_operators;
    test_case "bare selector variant attribute operators" `Quick
      test_bare_selector_variant_attribute_operators;
    test_case "valid bracket modifiers" `Quick test_valid_bracket_modifiers;
    test_case "supports condition text" `Quick test_supports_condition_text;
    test_case "not-[selector] unreadable content rejected" `Quick
      test_not_bracket_unreadable_selector_rejected;
    test_case "silent empty variants rejected" `Quick
      test_silent_empty_variants_rejected;
  ]

let suite =
  ( "variant_parse",
    test_case "modifier reads one token" `Quick test_modifier_reads_one_token
    :: tests )
