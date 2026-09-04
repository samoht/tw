open Alcotest
open Test_helpers

let check_early =
  check_handler_roundtrip (module Tw.Typography.Typography_early)

let check_late = check_handler_roundtrip (module Tw.Typography.Typography_late)

(* Try both handlers - the utility could be in either *)
let check class_name =
  match
    Tw.Typography.Typography_early.of_class Tw.Scheme.default class_name
  with
  | Ok _ -> check_early class_name
  | Error _ -> check_late class_name

let test_font_family () =
  check "font-sans";
  check "font-serif";
  check "font-mono"

(* Tailwind v4.3.2 moved [--font-sans] off [ui-sans-serif, system-ui, ...] to
   the platform system-font stack; v4.3.3 theme.css still carries that stack,
   and preflight's [html] fallback has to agree with it. *)
let test_font_family_default_stack () =
  let css ?(base = false) cls =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~base [ u ] |> Tw.Css.inline_vars
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let sans = css "font-sans" in
  Alcotest.(check bool)
    "font-sans leads with the platform stack" true
    (Astring.String.is_infix ~affix:"-apple-system,BlinkMacSystemFont," sans);
  Alcotest.(check bool)
    "font-sans dropped ui-sans-serif" false
    (Astring.String.is_infix ~affix:"ui-sans-serif" sans);
  let base = css ~base:true "font-sans" in
  Alcotest.(check bool)
    "preflight html falls back to the platform stack" true
    (Astring.String.is_infix
       ~affix:"font-family:-apple-system,BlinkMacSystemFont," base);
  Alcotest.(check bool)
    "preflight html dropped ui-sans-serif" false
    (Astring.String.is_infix ~affix:"ui-sans-serif" base)

let test_font_size () =
  List.iter
    (fun s -> check ("text-" ^ s))
    [
      "xs";
      "sm";
      "base";
      "lg";
      "xl";
      "2xl";
      "3xl";
      "4xl";
      "5xl";
      "6xl";
      "7xl";
      "8xl";
      "9xl";
    ]

let test_font_weight () =
  List.iter
    (fun v -> check ("font-" ^ v))
    [
      "thin";
      "extralight";
      "light";
      "normal";
      "medium";
      "semibold";
      "bold";
      "extrabold";
      "black";
    ]

let test_text_alignment () =
  List.iter
    (fun v -> check ("text-" ^ v))
    [ "left"; "center"; "right"; "justify" ]

let test_text_decoration () =
  check "underline";
  check "overline";
  check "line-through";
  check "no-underline";
  (* decoration color/thickness *)
  check "decoration-from-font";
  check "decoration-2";
  check "decoration-blue-500"

let test_text_transform () =
  check "uppercase";
  check "lowercase";
  check "capitalize";
  check "normal-case"

let test_line_height () =
  List.iter
    (fun v -> check ("leading-" ^ v))
    [
      "0";
      "1";
      "2";
      "11";
      "3";
      "4";
      "5";
      "6";
      "7";
      "8";
      "9";
      "10";
      "none";
      "tight";
      "snug";
      "normal";
      "relaxed";
      "loose";
    ]

let test_letter_spacing () =
  List.iter
    (fun v -> check ("tracking-" ^ v))
    [ "tighter"; "tight"; "normal"; "wide"; "wider"; "widest" ]

let test_line_clamp () =
  check "line-clamp-0";
  check "line-clamp-3"

(* A project [@theme] override of [--line-clamp-none] is a namespace key, not a
   value Tailwind type-checks at build time: any override at all, decimal or
   not, switches line-clamp-none to the variable-driven form. Reading the value
   with [int_of_string_opt] read [0x3] as the integer 3 (an OCaml-only spelling)
   yet rejected a non-numeric override like [banana], which Tailwind still
   honours the same way. *)
let test_line_clamp_none_theme_override () =
  let css theme cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let hex =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("line-clamp-none", "0x3") ]
  in
  let out = css hex "line-clamp-none" in
  Alcotest.(check bool)
    "a hex-looking override still drives the variable form" true
    (Astring.String.is_infix ~affix:"-webkit-line-clamp: var(--line-clamp-none)"
       out);
  Alcotest.(check bool)
    "the theme layer keeps the override text as authored" true
    (Astring.String.is_infix ~affix:"--line-clamp-none: 0x3" out);
  let non_numeric =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("line-clamp-none", "banana") ]
  in
  Alcotest.(check bool)
    "a non-numeric override still drives the variable form, too" true
    (Astring.String.is_infix ~affix:"-webkit-line-clamp: var(--line-clamp-none)"
       (css non_numeric "line-clamp-none"))

let test_text_overflow_wrap () =
  check "text-ellipsis";
  check "overflow-ellipsis";
  check "text-clip";
  check "text-wrap";
  check "text-nowrap";
  check "text-balance";
  check "text-pretty"

let test_text_overflow_order () =
  Test_helpers.check_class_order ~test_name:"text-overflow order"
    [ "text-ellipsis"; "overflow-ellipsis"; "text-clip" ]

let test_antialiasing_order () =
  Test_helpers.check_class_order ~test_name:"antialiasing order"
    [
      "caret-red-500";
      "placeholder-gray-400";
      "subpixel-antialiased";
      "antialiased";
    ]

let test_variant_antialiasing_order () =
  Test_helpers.check_class_order ~test_name:"variant antialiasing order"
    [
      "hover:placeholder-gray-400";
      "hover:subpixel-antialiased";
      "hover:antialiased";
    ]

let test_word_overflow_wrap () =
  check "break-normal";
  check "break-words";
  check "break-all";
  check "break-keep";
  check "overflow-wrap-normal";
  check "overflow-wrap-anywhere";
  check "overflow-wrap-break-word"

let test_hyphens () =
  check "hyphens-none";
  check "hyphens-manual";
  check "hyphens-auto"

let test_list_style () =
  check "list-none";
  check "list-disc";
  check "list-decimal";
  check "list-inside";
  check "list-outside";
  check "list-image-none"

let test_text_indent () =
  check "indent-4";
  check "-indent-8";
  check "indent-px";
  check "-indent-px"

(* [indent'] takes a half-step float; the int base keeps emitting what it always
   did. *)
let test_text_indent_prime () =
  check_typed_class "indent-0.5" (Tw.indent' 0.5);
  check_typed_class "indent-4" (Tw.indent 4)

let test_vertical_align () =
  check "align-baseline";
  check "align-top";
  check "align-middle";
  check "align-bottom";
  check "align-text-top";
  check "align-text-bottom";
  check "align-sub";
  check "align-super"

let test_font_stretch () =
  check "font-stretch-ultra-condensed";
  check "font-stretch-extra-condensed";
  check "font-stretch-condensed";
  check "font-stretch-semi-condensed";
  check "font-stretch-normal";
  check "font-stretch-semi-expanded";
  check "font-stretch-expanded";
  check "font-stretch-extra-expanded";
  check "font-stretch-ultra-expanded";
  check "font-stretch-150%"

let test_numeric_variants () =
  check "normal-nums";
  check "ordinal";
  check "slashed-zero";
  check "lining-nums";
  check "oldstyle-nums";
  check "proportional-nums";
  check "tabular-nums";
  check "diagonal-fractions";
  check "stacked-fractions"

let test_content () =
  check "content-none";
  (* Arbitrary content round-trips with its original quote style: the
     double-quoted form already worked, the single-quoted form used to mangle to
     content-[["x"]]. *)
  check "content-[\"x\"]";
  check "content-['x']";
  (* unquoted function values *)
  check "content-[attr(before)]";
  check "content-[counter(x)]";
  (* the bracket binds the channel and [content] reads it back *)
  check_declarations "content-[attr(before)]"
    [ "--tw-content:attr(before)"; "content:var(--tw-content)" ]

(* content-<token> parses only when the @theme defines --content-<token>; a bare
   word like content-wrapper with no token is rejected (it used to parse as a
   named content value, a false positive Tailwind does not emit). *)
let test_content_named_requires_theme () =
  (match
     Tw.Typography.Typography_late.of_class Tw.Scheme.default "content-wrapper"
   with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "content-wrapper should be rejected without a token");
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("content-slash", "\"/\"") ]
  in
  match Tw.Typography.Typography_late.of_class themed "content-slash" with
  | Ok _ -> ()
  | Error (`Msg m) -> Alcotest.failf "content-slash with theme rejected: %s" m

(* A project @theme can name font families of its own. The token gates the
   parse, so a stray source word (font-awesome) stays an unknown class; an
   @theme inline token carries its value into the utility rather than a
   reference, except when the value refers back to the token itself. *)
(* A [--text-*] token the project declared names a font size, the way a
   [--font-*] one names a family. Tailwind emits the reference alone: the token
   carries no line height, so the utility sets none. *)
let test_named_text_size () =
  (match Tw.of_string "text-huge" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "text-huge should be rejected without a token");
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("text-huge", "9rem") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
        Tw.Css.to_string ~minify:true (Tw.to_css ~base:false ~theme [ u ])
  in
  Alcotest.(check bool)
    "text-huge references its token" true
    (Astring.String.is_infix ~affix:"font-size:var(--text-huge)"
       (css "text-huge"));
  Alcotest.(check bool)
    "text-huge sets no line height" false
    (Astring.String.is_infix ~affix:"line-height" (css "text-huge"));
  Alcotest.(check bool)
    "text-huge/7 takes the modifier's leading" true
    (Astring.String.is_infix ~affix:"line-height:calc(var(--spacing)*7)"
       (css "text-huge/7"))

(* [--text-shadow-*] is a namespace of its own and [--text-<name>--line-height]
   is a modifier on another token, so neither names a font size. Nor does a
   [--text-*] token whose value is not a length. *)
let test_text_size_namespace_boundaries () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [
        ("text-shadow-pop", "0 1px 0 teal");
        ("text-huge", "9rem");
        ("text-huge--line-height", "1.5rem");
        ("text-loud", "bolder");
      ]
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Error _ -> ()
      | Ok u -> Alcotest.failf "%s parsed as a font size (%s)" cls (Tw.pp u))
    [ "text-shadow-pop"; "text-huge--line-height"; "text-loud" ]

(* The line-height modifier names a [--leading-*] token, and a project's own
   counts the same as a built-in one. *)
let test_theme_leading_modifier () =
  (match Tw.of_string "text-base/airy" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "text-base/airy should be rejected without a token");
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("leading-airy", "2.5"); ("text-huge", "9rem") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
        Tw.Css.to_string ~minify:true (Tw.to_css ~base:false ~theme [ u ])
  in
  List.iter
    (fun cls ->
      Alcotest.(check bool)
        (cls ^ " reads the token") true
        (Astring.String.is_infix ~affix:"line-height:var(--leading-airy)"
           (css cls)))
    [ "text-base/airy"; "text-huge/airy" ]

let test_named_font_family () =
  (match Tw.of_string "font-awesome" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "font-awesome should be rejected without a token");
  let css theme cls =
    match Tw.of_string ~theme cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u -> Tw.Css.to_string ~minify:true (Tw.to_css ~base:false ~theme [ u ])
  in
  let themed =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("font-source", "Georgia, serif") ]
  in
  Alcotest.(check bool)
    "font-source references its token" true
    (Astring.String.is_infix ~affix:"font-family:var(--font-source)"
       (css themed "font-source"));
  let inline =
    Tw.Scheme.with_overrides
      ~inline:[ "font-source"; "font-self" ]
      Tw.Scheme.default
      [ ("font-source", "Georgia, serif"); ("font-self", "var(--font-self)") ]
  in
  Alcotest.(check bool)
    "an inline token is inlined" true
    (Astring.String.is_infix ~affix:"font-family:Georgia,serif"
       (css inline "font-source"));
  Alcotest.(check bool)
    "a self-referential inline token keeps its declaration" true
    (Astring.String.is_infix ~affix:"--font-self:var(--font-self)"
       (css inline "font-self"))

(* text-[<value>] accepts values that CSS font-size accepts: lengths with a
   unit, percentages, font-size keywords (larger/smaller/xxx-large/...),
   clamp(...), and var(...). Bare identifiers without a unit are rejected --
   including things that look like hex colors but lack the leading '#' (CSS
   Color §5.4.6). *)
let test_text_bracket_size_valid () =
  check "text-[16px]";
  check "text-[1.5rem]";
  check "text-[1.25em]";
  check "text-[100%]";
  check "text-[xxx-large]";
  check "text-[xx-small]";
  check "text-[larger]";
  check "text-[smaller]";
  check "text-[length:var(--my-size)]";
  check "text-[percentage:50%]"

let test_text_bracket_size_invalid () =
  let bad input =
    match Tw.Typography.Typography_early.of_class Tw.Scheme.default input with
    | Ok _ -> Alcotest.fail ("Expected early handler to reject: " ^ input)
    | Error _ -> (
        match
          Tw.Typography.Typography_late.of_class Tw.Scheme.default input
        with
        | Ok _ -> Alcotest.fail ("Expected late handler to reject: " ^ input)
        | Error _ -> ())
  in
  (* Regression: these used to silently emit font-size: var(--<garbage>). *)
  bad "text-[1A202C]";
  (* Hex-looking, missing '#' *)
  bad "text-[FF0000]";
  bad "text-[totallyNotAColor]";
  (* Missing unit *)
  bad "text-[16]";
  (* The unitless zero reads as a colour in v4, not a size *)
  bad "text-[0]"

(* A bracket font-size is any CSS length, not the px/rem/em/% subset the
   hand-rolled suffix parser knew. Same for an arbitrary text-indent. *)
let test_bracket_length_units () =
  check_declarations "text-[3ch]" [ "font-size:3ch" ];
  check_declarations "text-[2vh]" [ "font-size:2vh" ];
  check_declarations "text-[calc(1rem+2px)]" [ "font-size:calc(1rem + 2px)" ];
  check_declarations "indent-[3ch]" [ "text-indent:3ch" ];
  check_declarations "-indent-[3ch]" [ "text-indent:-3ch" ]

(* The [/leading] modifier on a text size and the standalone [leading-[...]]
   utility read an arbitrary line-height with one reader, so they agree on every
   spelling. The modifier used to render anything but px, rem or a bare number
   as a zero, collapsing the line box under a selector that matched. *)
let test_arbitrary_leading () =
  (* The standalone utility sets the channel and reads it back; the modifier
     writes the line height beside the size it decorates. *)
  let agree spelling value =
    check_declarations
      ("leading-[" ^ spelling ^ "]")
      [ "--tw-leading:" ^ value; "line-height:" ^ value ];
    check_declarations
      ("text-lg/[" ^ spelling ^ "]")
      [ "font-size:var(--text-lg)"; "line-height:" ^ value ]
  in
  agree "2em" "2em";
  agree "150%" "150%";
  agree "normal" "normal";
  agree "var(--lh)" "var(--lh)";
  agree "calc(1rem_+_2px)" "calc(1rem + 2px)";
  (* every unit the length grammar names, not the handful the line-height type
     happens to have a constructor for *)
  agree "2vw" "2vw";
  agree "3ch" "3ch";
  agree "10dvh" "10dvh";
  agree "2cqw" "2cqw";
  agree "4lh" "4lh";
  (* the spellings that already worked keep working *)
  agree "24px" "24px";
  agree "2rem" "2rem";
  agree "1.5" "1.5";
  (* the class name is spelled as the author wrote it *)
  Alcotest.(check string)
    "text-lg/[2em] round-trips" "text-lg/[2em]"
    (Tw.pp (Result.get_ok (Tw.of_string "text-lg/[2em]")))

(* A standalone arbitrary leading utility forwards a safe token stream. Font
   size modifiers remain typed line-height values. *)
let test_arbitrary_leading_token_stream () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "leading-[red]";
  accepted "leading-[1zz]";
  rejected "text-lg/[red]";
  rejected "text-lg/[1zz]"

(* [tracking-[...]] forwards any safe declaration value. *)
let test_arbitrary_tracking_token_stream () =
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  renders "tracking-[foo]";
  renders "tracking-[red]";
  renders "tracking-[45deg]";
  renders "tracking-[1e]";
  renders "tracking-[a,b]";
  renders "-tracking-[foo]";
  renders "tracking-[1px]";
  renders "tracking-[.5em]";
  renders "tracking-[-1px]";
  renders "tracking-[2%]";
  renders "tracking-[calc(1px_+_2px)]";
  renders "tracking-[var(--x)]";
  renders "-tracking-[.5em]"

(* A shade the palette does not define is not a colour. [decoration-*] read the
   shade without checking it, so the class was accepted and then referenced a
   variable no theme declares. *)
let test_decoration_undefined_shade () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "decoration-red-999";
  rejected "decoration-red-0";
  rejected "decoration-red-42";
  rejected "decoration-red-999/50";
  accepted "decoration-red-500";
  accepted "decoration-red-950";
  accepted "decoration-red-500/50"

let of_string_invalid () =
  (* Invalid typography values *)
  let fail_maybe input =
    let class_name = String.concat "-" input in
    (* Both handlers should reject the input *)
    (match
       Tw.Typography.Typography_early.of_class Tw.Scheme.default class_name
     with
    | Error _ -> ()
    | Ok _ ->
        Alcotest.fail
          (String.concat ""
             [ "Expected early handler to reject: "; class_name ]));
    match
      Tw.Typography.Typography_late.of_class Tw.Scheme.default class_name
    with
    | Error _ -> ()
    | Ok _ ->
        Alcotest.fail
          (String.concat "" [ "Expected late handler to reject: "; class_name ])
  in

  fail_maybe [ "font"; "invalid" ];
  (* Invalid font family *)
  fail_maybe [ "text"; "10xl" ];
  (* Invalid text size *)
  fail_maybe [ "font"; "superheavy" ];
  (* Invalid font weight *)
  fail_maybe [ "text"; "middle" ];
  (* Invalid text alignment *)
  fail_maybe [ "tracking"; "tightest" ];
  (* Invalid letter spacing *)
  fail_maybe [ "unknown" ]
(* Unknown typography type *)

(* line-clamp sits between box-sizing and the display family in Tailwind's
   order, not among the typography utilities its class name suggests. *)
let line_clamp_sorts_with_box_sizing () =
  let order cls =
    Tw.Utility.base_of_class Tw.Scheme.default cls
    |> Result.get_ok |> Tw.Utility.order
  in
  let named_priority, _ = order "line-clamp-2" in
  let arbitrary_priority, _ = order "line-clamp-[<value>]" in
  Alcotest.(check int)
    "arbitrary line-clamp stays in the line-clamp property band" named_priority
    arbitrary_priority;
  let classes =
    [ "indent-4"; "line-clamp-2"; "block"; "box-border"; "ml-auto" ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"line-clamp order" utilities

(* Tailwind's tail runs the [color] property, then text-transform, font-style
   and text-decoration-line, then the decoration colour, style and thickness,
   the underline offset, and last the placeholder, caret and accent colours.
   Every one of those writes a different property, so a reorder among them is
   cascade-neutral and the canonical differ reads the two sheets as equal;
   reading each class's position back out of the sheet is what sees it. *)
let late_typography_colour_block_order () =
  Test_helpers.check_class_order ~test_name:"late typography colour block"
    [
      "tracking-wide";
      "whitespace-nowrap";
      "text-red-500";
      "uppercase";
      "italic";
      "underline";
      "decoration-red-500";
      "decoration-solid";
      "decoration-2";
      "underline-offset-2";
      "placeholder-red-500";
      "caret-red-500";
      "accent-red-500";
    ]

(* [underline-offset-[N]] is a bracket value, not a spelling of the bare
   [underline-offset-N] step: it names its own rule and its value carries the
   unit the author wrote rather than the [px] the scale supplies. Folding the
   two together emitted [.underline-offset-0] for [underline-offset-[0.0]], a
   rule nothing selects. *)
let test_underline_offset_bracket_keeps_its_class () =
  List.iter
    (fun (cls, escaped) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          let css = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string in
          Alcotest.(check bool)
            (cls ^ " selects itself") true
            (Astring.String.is_infix ~affix:escaped css))
    [
      ("underline-offset-[0.0]", {|.underline-offset-\[0\.0\]|});
      ("underline-offset-[3px]", {|.underline-offset-\[3px\]|});
      ("underline-offset-[1.50px]", {|.underline-offset-\[1\.50px\]|});
      ("underline-offset-[calc(1px+2px)]", {|.underline-offset-\[calc\(|});
      ("-underline-offset-[3px]", {|.-underline-offset-\[3px\]|});
    ];
  (* the bare step keeps its own class and the [px] the scale gives it *)
  check_declarations "underline-offset-3" [ "text-underline-offset:3px" ]

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      text_xs;
      text_sm;
      text_base;
      text_lg;
      text_xl;
      text_2xl;
      text_3xl;
      font_thin;
      font_light;
      font_normal;
      font_medium;
      font_semibold;
      font_bold;
      font_extrabold;
      font_black;
      text_left;
      text_center;
      text_right;
      text_justify;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"typography suborder matches Tailwind" shuffled

(* text-<size> also sets the line height, so a size and a leading on the same
   element decide between them what the text is laid out with. The quoted
   content values also pin the harness: a class whose arbitrary value carries
   quotes has to reach the rendered element intact. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "content-[\"x\"]";
      "content-['x']";
      "text-xs";
      "text-base";
      "text-lg";
      "text-3xl";
      "font-thin";
      "font-normal";
      "font-bold";
      "leading-none";
      "leading-relaxed";
      "leading-6";
      "tracking-tight";
      "tracking-wide";
      "text-left";
      "text-center";
      "text-right";
      "underline";
      "line-through";
      "no-underline";
      "italic";
      "not-italic";
      "uppercase";
      "truncate";
      "indent-4";
      "align-middle";
    ]
  in
  Test_helpers.check_rendering_matches
    ~test_name:"typography renders like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* tracking-normal's token must keep the em unit (0em), not collapse to 0. *)
let test_tracking_normal_unit () =
  let css = Tw.to_css [ Tw.tracking_normal ] |> Tw.Css.to_string ~minify:true in
  Alcotest.check bool "tracking-normal token keeps em unit" true
    (Astring.String.is_infix ~affix:"--tracking-normal:0em" css)

(* Numeric leading derives from the spacing scale in v4.3.1 (not a --leading-N
   theme token). *)
let test_numeric_leading_spacing () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "could not parse %s" cls
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " -> " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  (* Any non-negative N is accepted; N>=2 scales spacing, 1 is bare, 0 is 0. *)
  let scaled cls value =
    check_declarations cls [ "--tw-leading:" ^ value; "line-height:" ^ value ]
  in
  scaled "leading-6" "calc(var(--spacing)*6)";
  scaled "leading-2" "calc(var(--spacing)*2)";
  scaled "leading-11" "calc(var(--spacing)*11)";
  scaled "leading-1" "var(--spacing)";
  (* leading-0 stays on a substring: the pinned CLI writes [--tw-leading: 0px]
     where tw writes [0]. *)
  has "leading-0" "line-height:0"

(* [leading'] takes a half-step float, same convention as [p]/[p']; the int base
   keeps emitting what it always did. *)
let test_leading_prime () =
  check_typed_class "leading-1.5" (Tw.leading' 1.5);
  check_typed_class "leading-6" (Tw.leading 6);
  check_declarations "leading-1.5"
    [
      "--tw-leading:calc(var(--spacing)*1.5)";
      "line-height:calc(var(--spacing)*1.5)";
    ]

(* leading-none has no v4.3 theme token, so it inlines line-height: 1 rather
   than minting a --leading-none var. *)
let test_leading_none_inline () =
  let css =
    match Tw.of_string "leading-none" with
    | Ok u -> Tw.to_css [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.fail "could not parse leading-none"
  in
  check_declarations "leading-none" [ "--tw-leading:1"; "line-height:1" ];
  Alcotest.(check bool)
    "leading-none mints no --leading-none token" false
    (Astring.String.is_infix ~affix:"--leading-none" css)

(* A text size must honor a --text-N--line-height theme override at use time,
   not bake in the spacing-derived default at module load. *)
let test_text_line_height_override () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("text-sm--line-height", "1.25rem") ]
  in
  let css =
    match Tw.of_string "text-sm" with
    | Ok u -> Tw.to_css ~theme [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.fail "could not parse text-sm"
  in
  Alcotest.(check bool)
    "text-sm honors --text-sm--line-height override" true
    (Astring.String.is_infix ~affix:"--text-sm--line-height:1.25rem" css)

(* Tailwind's [--spacing(N)] and [--alpha(<color>/<pct>)] are usable inside an
   arbitrary value. Verified against the real v4.3.3 CLI, which emits
   [calc(var(--spacing) * 2)] and [color-mix(in oklab, red 20%,
   transparent)]. *)
let test_text_bracket_functions () =
  check_declarations "text-[--spacing(2)]"
    [ "font-size:calc(var(--spacing)*2)" ];
  (* an alpha bracket names a colour, so it is a text colour rather than a
     size *)
  check_declarations "text-[--alpha(red/20%)]"
    [ "color:color-mix(in oklab,red 20%,transparent)" ]

(* A bracket list-style value is read with the CSS parser rather than a
   hand-rolled keyword table: list-[square] and list-image-[url(...)] used to be
   unknown classes. *)
let test_bracket_list_style () =
  check_declarations "list-[square]" [ "list-style-type:square" ];
  check_declarations "list-image-[url(/carrot.png)]"
    [ "list-style-image:url(/carrot.png)" ];
  Alcotest.(check bool)
    "an unknown counter style is rejected" true
    (Result.is_error (Tw.of_string "list-[nonsense-style]"))

(* A [url()] argument is left verbatim, so a file name keeps the [_] it is
   written with. [list-image-] and [content-] read their bracket through the
   arbitrary-value decoder, which used to turn every [_] into a space. *)
let test_bracket_url_underscore () =
  check_declarations "list-image-[url('a_b.png')]"
    [ "list-style-image:url(a_b.png)" ];
  check_declarations "list-image-[url(a_b.png)]"
    [ "list-style-image:url(a_b.png)" ];
  (* Tailwind writes this one [url('a_b.png')]. The quoting is cascade's
     canonical spelling of the same URL; the underscore is the point. *)
  check_declarations "content-[url('a_b.png')]"
    [ "--tw-content:url(a_b.png)"; "content:var(--tw-content)" ]

(* List position, type, and image are three property bands; candidates inside
   each band use their natural class-name order. *)
let test_list_style_property_bands () =
  Test_helpers.check_class_order ~test_name:"list-style property bands"
    [
      "list-image-none";
      "list-image-[url(/carrot.png)]";
      "list-none";
      "list-disc";
      "list-decimal";
      "list-[square]";
      "list-outside";
      "list-inside";
    ]

(* CSS Fonts 4 sec. 6.4: a feature setting is a quoted four-character tag with
   an optional integer / on / off, so the docs' [<value>] placeholder is not
   one; the underscore in [font-features-["liga"_0]] is a space. *)
let test_font_features_value () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "font-features-[<value>]";
  (* the underscore is a space *)
  check_declarations "font-features-[\"liga\"_0]"
    [ "font-feature-settings:\"liga\" 0" ]

(* font-feature-settings follows font-family and precedes font-size in
   Tailwind's property table. Keeping it after the weight band displaces both
   the feature rules and every larger text size in a full sheet. *)
let test_font_feature_property_band () =
  Test_helpers.check_class_order ~test_name:"font feature property band"
    [
      "font-bold";
      "leading-tight";
      "text-9xl";
      "text-2xl";
      "font-features-[\"tnum\"]";
      "font-features-(--my-features)";
      "font-serif";
      "font-sans";
    ]

(* A bracket font-size carrying a line-height modifier follows the large named
   sizes and precedes the base size in Tailwind's candidate order. *)
let test_bracket_font_size_candidate_band () =
  Test_helpers.check_class_order ~test_name:"bracket font-size candidate band"
    [ "text-base"; "text-[13px]/6"; "text-9xl"; "text-2xl" ]

(* A font family is idents or quoted strings; the docs' [<value>] placeholder
   used to be quoted into font-family: "<value>". *)
let test_invalid_font_family () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "font-[<value>]";
  accepted "font-[ui-sans-serif]";
  accepted "font-[var(--x)]";
  accepted "font-[600]"

(* A quoted bracket font family carries its own quoting; Tailwind passes the
   decoded text through rather than wrapping it in one more layer of quotes.
   When the bracket mixes a quoted string with a trailing bare token
   ([font-["liga"_0x10]]) the decoded text is not a single family name at all
   (CSS Fonts 4 sec. 2.1 has no such shape); cascade's own reader marks it
   [Invalid] and the printer drops the declaration, the same fate a browser
   gives Tailwind's literal (spec-invalid) text, so the double-quoted mangling
   must not appear either. *)
let test_font_bracket_family_quoted () =
  let css cls =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u -> Tw.Css.to_string (Tw.to_css ~base:false [ u ])
  in
  (* one layer of quotes, which minified prints as a bare two-word family *)
  check_declarations {|font-["arial_rounded"]|} [ "font-family:arial rounded" ];
  Alcotest.(check bool)
    "a quoted string mixed with a bare token is never double-quoted" false
    (Astring.String.is_infix ~affix:{|font-family: "\"liga\"|}
       (css {|font-["liga"_0x10]|}))

(* A comma-separated bracket family list ([font-[Papyrus,fantasy]]) is a
   fallback stack, not one literal name: each comma segment is its own
   [<family-name>], so a bare generic keyword among them (here [fantasy]) stays
   an unquoted keyword rather than folding into one quoted string. *)
let test_font_bracket_family_comma_list () =
  check_declarations "font-[Papyrus,fantasy]" [ "font-family:Papyrus,fantasy" ]

(* An arbitrary decoration thickness takes any CSS length unit, not just the px
   the hand-rolled suffix parser knew; the percentage form keeps its em
   conversion. A bracket that is not a length is rejected by the parser rather
   than raising once the sheet is rendered. *)
let test_decoration_bracket_thickness () =
  let thickness cls value =
    check_declarations cls [ "text-decoration-thickness:" ^ value ]
  in
  thickness "decoration-[3rem]" "3rem";
  thickness "decoration-[2px]" "2px";
  thickness "decoration-[2ch]" "2ch";
  thickness "decoration-[50%]" ".5em";
  (* The bracket goes through the arbitrary-value decoder, so an escaped space
     and a [--spacing()] call both read as lengths. *)
  thickness "decoration-[calc(1rem_+_2px)]" "calc(1rem + 2px)";
  thickness "decoration-[--spacing(2)]" "calc(var(--spacing)*2)";
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "decoration-[.]";
  rejected "decoration-[1e]"

(* Tailwind treats a unitless arbitrary decoration value as a colour candidate,
   whether it is written as an integer or with a fractional part. The resulting
   colour declaration is invalid CSS and has no browser effect; TW must keep the
   candidate accepted without turning it into a visible pixel thickness. *)
let test_decoration_bracket_unitless_is_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let inert cls =
    Alcotest.(check bool)
      (cls ^ " does not turn the invalid colour into a thickness")
      false
      (Astring.String.is_infix ~affix:"text-decoration-thickness" (css cls))
  in
  List.iter inert
    [
      "decoration-[0]"; "decoration-[2]"; "decoration-[1.5]"; "decoration-[.5]";
    ]

(* A shadeless decoration colour takes an opacity modifier the same way every
   other colour family does, and keeps its shadeless class name. *)
let test_decoration_shadeless_opacity () =
  check_late "decoration-white/50";
  check_late "decoration-black/50";
  check_late "decoration-white/[0.5]";
  (* the folded sRGB fallback, then the [color-mix] enhancement under its
     [@supports] guard, aliased for WebKit *)
  let mixed cls hex token =
    check_declarations cls
      [
        "text-decoration-color:" ^ hex;
        "-webkit-text-decoration-color:color-mix(in oklab,var(" ^ token
        ^ ") 50%,transparent)";
        "text-decoration-color:color-mix(in oklab,var(" ^ token
        ^ ") 50%,transparent)";
      ]
  in
  mixed "decoration-white/50" "#ffffff80" "--color-white";
  mixed "decoration-black/50" "#00000080" "--color-black";
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "decoration-2/50";
  rejected "decoration-nosuchcolor/50";
  rejected "decoration-white/"

(* The pre-color-mix fallback has to carry the modifier's alpha, or a browser
   without [color-mix()] paints the decoration fully opaque. A palette colour
   folds the alpha into a hex; a project token, whose value the theme supplies,
   takes the sRGB mix instead. *)
let test_decoration_opacity_fallback () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(55% 0.2 250)") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u ->
        Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "text-decoration-color:#fb2c3680" "decoration-red-500/50";
  emits "text-decoration-color:#ffffff80" "decoration-white/50";
  emits
    "text-decoration-color:color-mix(in srgb,oklch(55%.2 250) 50%,transparent)"
    "decoration-brand/50";
  (* An alpha read from a var has no percentage to fold in, so the fallback is
     the colour itself and the enhanced value reads the var. *)
  emits "text-decoration-color:oklch(55%.2 250)" "decoration-brand/(--a)";
  emits "color-mix(in oklab,var(--color-brand) var(--a),transparent)"
    "decoration-brand/(--a)"

(* A bracket colour CSS names without spelling it as a function - a named
   colour, a keyword - is a decoration colour too. The reader admitted only a
   [#] hex and a colour function, so [decoration-[rebeccapurple]] was an unknown
   class, with or without an opacity modifier. *)
let test_decoration_bracket_named_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  (* the value a class sets for [text-decoration-color], so two spellings of one
     colour compare without their class names *)
  let value cls =
    let sheet = css cls in
    let key = "text-decoration-color:" in
    match Astring.String.find_sub ~sub:key sheet with
    | None -> Alcotest.failf "%s sets no decoration colour: %s" cls sheet
    | Some i ->
        let first = i + String.length key in
        Astring.String.with_range ~first sheet
        |> Astring.String.take ~sat:(fun c -> c <> ';' && c <> '}')
  in
  check_declarations "decoration-[rebeccapurple]"
    [ "text-decoration-color:rebeccapurple" ];
  check_declarations "decoration-[currentColor]"
    [ "text-decoration-color:currentColor" ];
  (* the modifier folds into the colour the bracket named, not into black *)
  Alcotest.(check string)
    "decoration-[rebeccapurple]/50 is decoration-[#663399]/50"
    (value "decoration-[#663399]/50")
    (value "decoration-[rebeccapurple]/50");
  (* a bracket naming no colour and no thickness is still not a class *)
  Alcotest.(check bool)
    "decoration-[notacolour] is not a class" true
    (Result.is_error (Tw.of_string "decoration-[notacolour]"))

(* A [#] bracket is only a decoration colour when what follows is a hex
   spelling. The decoration reader handed everything after the [#] to the
   raising constructor from inside [of_class], so a malformed hex escaped the
   parser as an exception instead of failing the match. *)
let test_invalid_decoration_bracket_hex () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "decoration-[#zz]";
  rejected "decoration-[#]";
  rejected "decoration-[#12345]";
  rejected "decoration-[#zz]/50";
  check_declarations "decoration-[#ff0000]" [ "text-decoration-color:#f00" ]

(* Integers in a class name are plain decimal here too: the leading modifier
   [text-lg/0x10] was read as /16 and the font-weight bracket [font-[0x10]] as
   16, so tw painted a weight where Tailwind, which passes [0x10] through for
   the browser to drop, paints nothing. *)
let test_non_decimal_integers () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "text-lg/0x10";
  rejected "font-[0x10]";
  let accepted cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "line-clamp-[0x10]";
  accepted "line-clamp-[1_0]"

(* A [--font-weight-*] or [--leading-*] token the project declared in its
   [@theme] names a value the built-in scale has no slot for. Tailwind generates
   the utility from each, channel variable included; tw rejected both
   outright. *)
let test_project_font_and_leading_tokens () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("font-weight-chonk", "900"); ("leading-roomy", "2.5") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "--tw-font-weight: var(--font-weight-chonk)" "font-chonk";
  emits "font-weight: var(--font-weight-chonk)" "font-chonk";
  emits "--tw-leading: var(--leading-roomy)" "leading-roomy";
  emits "line-height: var(--leading-roomy)" "leading-roomy";
  Alcotest.(check bool)
    "an undeclared weight name is rejected" true
    (Result.is_error (Tw.of_string ~theme "font-nope"))

(* A [--tracking-*] token the project declared in its [@theme] names a letter
   spacing the built-in scale has no slot for. Tailwind generates the utility
   from it, channel variable included; tw rejected the class outright. *)
let test_project_tracking_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("tracking-airy", "0.2em") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let out = css "tracking-airy" in
  Alcotest.(check bool)
    "sets the channel" true
    (Astring.String.is_infix ~affix:"--tw-tracking: var(--tracking-airy)" out);
  Alcotest.(check bool)
    "sets letter-spacing" true
    (Astring.String.is_infix ~affix:"letter-spacing: var(--tracking-airy)" out);
  Alcotest.(check bool)
    "an undeclared tracking name is rejected" true
    (Result.is_error (Tw.of_string ~theme "tracking-nope"))

(* An arbitrary value writes a space as [_] and a literal underscore as [\_].
   Both readings have to reach the value: a family name and a content string are
   written by hand, so either character can be the one meant. *)
let test_arbitrary_underscore_escape () =
  let content cls value =
    check_declarations cls
      [ "--tw-content:" ^ value; "content:var(--tw-content)" ]
  in
  check_declarations {|font-['My\_Font']|} [ "font-family:My_Font" ];
  check_declarations "font-[Arial_Black]" [ "font-family:Arial Black" ];
  content {|content-["hello\_world"]|} {|"hello_world"|};
  content {|content-[attr(a\_b)]|} "attr(a_b)";
  (* The single-quoted spelling already reads both, and stays that way. *)
  content {|content-['hello\_world']|} {|"hello_world"|};
  content {|content-['hello_world']|} {|"hello world"|}

(* A data-type hint says how to read the value written after it; it does not
   make that value the name of a custom property. [text-[length:1.25rem]] wrote
   [font-size: var(--1\.25rem)] where Tailwind writes [font-size: 1.25rem]. *)
let test_bracket_data_type_hint_reads_the_value () =
  check_declarations "text-[length:1.25rem]" [ "font-size:1.25rem" ];
  check_declarations "text-[percentage:80%]" [ "font-size:80%" ];
  check_declarations "text-[absolute-size:large]" [ "font-size:large" ];
  check_declarations "text-[relative-size:larger]" [ "font-size:larger" ];
  check_declarations "font-[family-name:Inter]" [ "font-family:Inter" ];
  check_declarations "font-[generic-name:serif]" [ "font-family:serif" ];
  check_declarations "font-[number:600]"
    [ "--tw-font-weight:600"; "font-weight:600" ];
  check_declarations "decoration-[length:3px]"
    [ "text-decoration-thickness:3px" ];
  check_declarations "decoration-[color:red]" [ "text-decoration-color:red" ];
  (* a var() reference after the hint still names a custom property *)
  check_declarations "text-[length:var(--my-size)]"
    [ "font-size:var(--my-size)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "text-[length:1.25rem] round-trips" "text-[length:1.25rem]"
    (Tw.pp (Result.get_ok (Tw.of_string "text-[length:1.25rem]")));
  (* A value the reading refuses is held open, not settled: Tailwind writes the
     bracket out whatever it says, so refusing is an intermediate. *)
  check_invalid_input
    ~why:(Diverges "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Typography.Typography_early)
    "text-[length:notalength]"

let tests =
  [
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "arbitrary underscore escape" `Quick
      test_arbitrary_underscore_escape;
    test_case "invalid decoration bracket hex" `Quick
      test_invalid_decoration_bracket_hex;
    test_case "non-decimal integers" `Quick test_non_decimal_integers;
    test_case "project tracking token" `Quick test_project_tracking_token;
    test_case "project font and leading tokens" `Quick
      test_project_font_and_leading_tokens;
    test_case "decoration bracket thickness" `Quick
      test_decoration_bracket_thickness;
    test_case "unitless decoration bracket is a colour" `Quick
      test_decoration_bracket_unitless_is_color;
    test_case "decoration shadeless opacity" `Quick
      test_decoration_shadeless_opacity;
    test_case "bracket list-style" `Quick test_bracket_list_style;
    test_case "list-style property bands" `Slow test_list_style_property_bands;
    test_case "invalid font family" `Quick test_invalid_font_family;
    test_case "font bracket family quoted" `Quick
      test_font_bracket_family_quoted;
    test_case "font bracket family comma list" `Quick
      test_font_bracket_family_comma_list;
    test_case "font-features value" `Quick test_font_features_value;
    test_case "font-feature property band" `Slow test_font_feature_property_band;
    test_case "bracket font-size candidate band" `Slow
      test_bracket_font_size_candidate_band;
    test_case "tracking-normal unit" `Quick test_tracking_normal_unit;
    test_case "numeric leading from spacing" `Quick test_numeric_leading_spacing;
    test_case "leading half-step" `Quick test_leading_prime;
    test_case "leading-none inline" `Quick test_leading_none_inline;
    test_case "text line-height override" `Quick test_text_line_height_override;
    test_case "font family" `Quick test_font_family;
    test_case "font family default stack" `Quick test_font_family_default_stack;
    test_case "font size" `Quick test_font_size;
    test_case "font weight" `Quick test_font_weight;
    test_case "text alignment" `Quick test_text_alignment;
    test_case "text decoration" `Quick test_text_decoration;
    test_case "text transform" `Quick test_text_transform;
    test_case "line height" `Quick test_line_height;
    test_case "letter spacing" `Quick test_letter_spacing;
    test_case "line clamp" `Quick test_line_clamp;
    test_case "line-clamp-none theme override" `Quick
      test_line_clamp_none_theme_override;
    test_case "text overflow/wrap" `Quick test_text_overflow_wrap;
    test_case "text overflow order" `Slow test_text_overflow_order;
    test_case "antialiasing order" `Slow test_antialiasing_order;
    test_case "variant antialiasing order" `Slow test_variant_antialiasing_order;
    test_case "word/overflow wrap" `Quick test_word_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
    test_case "list style" `Quick test_list_style;
    test_case "text indent" `Quick test_text_indent;
    test_case "text indent half-step" `Quick test_text_indent_prime;
    test_case "vertical align" `Quick test_vertical_align;
    test_case "font stretch" `Quick test_font_stretch;
    test_case "numeric variants" `Quick test_numeric_variants;
    test_case "content" `Quick test_content;
    test_case "content-named requires theme token" `Quick
      test_content_named_requires_theme;
    test_case "text-[<font-size>] valid values" `Quick
      test_text_bracket_size_valid;
    test_case "text-[<font-size>] invalid values" `Quick
      test_text_bracket_size_invalid;
    test_case "bracket length units" `Quick test_bracket_length_units;
    test_case "bracket url underscores" `Quick test_bracket_url_underscore;
    test_case "arbitrary leading" `Quick test_arbitrary_leading;
    test_case "arbitrary leading token stream" `Quick
      test_arbitrary_leading_token_stream;
    test_case "arbitrary tracking token stream" `Quick
      test_arbitrary_tracking_token_stream;
    test_case "text-[--spacing()/--alpha()] functions" `Quick
      test_text_bracket_functions;
    test_case "named font family from the theme" `Quick test_named_font_family;
    test_case "named text size from the theme" `Quick test_named_text_size;
    test_case "text size namespace boundaries" `Quick
      test_text_size_namespace_boundaries;
    test_case "leading modifier from the theme" `Quick
      test_theme_leading_modifier;
    test_case "decoration undefined colour shade" `Quick
      test_decoration_undefined_shade;
    test_case "typography of_string - invalid values" `Quick of_string_invalid;
    test_case "late typography colour block order" `Quick
      late_typography_colour_block_order;
    test_case "typography suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "line-clamp sorts with box-sizing" `Quick
      line_clamp_sorts_with_box_sizing;
    test_case "decoration opacity fallback" `Quick
      test_decoration_opacity_fallback;
    test_case "decoration bracket named colour" `Quick
      test_decoration_bracket_named_color;
    test_case "underline-offset-[...] keeps its bracket" `Quick
      test_underline_offset_bracket_keeps_its_class;
    test_case "typography renders like Tailwind" `Slow
      rendering_matches_tailwind;
  ]

let suite = ("typography", tests)
