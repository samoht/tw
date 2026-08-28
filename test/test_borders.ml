open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Borders.Handler)

let of_string_valid () =
  check "border";
  check "border-0";
  check "border-2";
  check "border-4";
  check "border-8";
  (* v4 accepts any bare integer width, not just the fixed scale *)
  check "border-1";
  check "border-3";
  check "outline-3";
  check "outline-5";
  (* outline-offset accepts any bare integer, positive and negative *)
  check "outline-offset-6";
  check "outline-offset-3";
  check "-outline-offset-6";
  check "-outline-offset-1";

  check "border-t";
  check "border-r";
  check "border-b";
  check "border-l";
  check "border-x";
  check "border-y";

  check "border-t-2";
  check "border-r-4";
  check "border-x-4";
  check "border-y-2";
  check "border-x-0";
  check "border-y-8";

  (* A side or axis takes any integer, as the bare border does: border-x-16 was
     rejected where Tailwind emits border-inline-width: 16px. *)
  check "border-x-16";
  check "border-y-16";
  check "border-t-16";
  check "border-x-3";
  check "border-l-5";
  check "border-b-12";
  check "border-bs-12";
  check "border-e-7";

  (* logical single-side borders: inline/block start and end *)
  check "border-s";
  check "border-e";
  check "border-bs";
  check "border-be";
  check "border-s-4";
  check "border-e-0";
  check "border-bs-2";
  check "border-be-8";

  check "border-solid";
  check "border-dashed";
  check "border-dotted";
  check "border-double";
  check "border-none";

  check "rounded";
  check "rounded-none";
  check "rounded-xs";
  check "rounded-sm";
  check "rounded-md";
  check "rounded-lg";
  check "rounded-xl";
  check "rounded-2xl";
  check "rounded-3xl";
  check "rounded-4xl";
  check "rounded-full";

  check "rounded-t";
  check "rounded-r";
  check "rounded-b";
  check "rounded-l";

  check "rounded-tl";
  check "rounded-tr";
  check "rounded-br";
  check "rounded-bl";

  check "rounded-t-lg";
  check "rounded-t-xs";
  check "rounded-tl-2xl";
  check "rounded-ee-4xl";
  check "rounded-tl-xs"

let of_string_invalid () =
  (* Invalid border values *)
  let fail_maybe =
    Test_helpers.check_invalid_parts (module Tw.Borders.Handler)
  in

  fail_maybe [ "border"; "invalid" ];
  (* Invalid style *)
  fail_maybe [ "border"; "z" ];
  (* Invalid side *)
  fail_maybe [ "rounded"; "z" ];
  (* Invalid corner *)
  fail_maybe [ "unknown" ]
(* Unknown border type *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      border_xs;
      border_sm;
      border_md;
      border_lg;
      border_solid;
      border_dashed;
      border_dotted;
      border_double;
      border_none;
      rounded_none;
      rounded_sm;
      rounded;
      rounded_md;
      rounded_lg;
      rounded_xl;
      rounded_2xl;
      rounded_3xl;
      rounded_full;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"borders suborder matches Tailwind" shuffled

(* A width, a side width and a style all write border-*-width or -style, so what
   an element is actually bordered with is a rendering question. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "border";
      "border-2";
      "border-4";
      "border-x-2";
      "border-y-4";
      "border-t-4";
      "border-b-2";
      "border-solid";
      "border-dashed";
      "border-dotted";
      "border-none";
      "border-current";
      "rounded";
      "rounded-md";
      "rounded-lg";
      "rounded-full";
      "rounded-t-lg";
      "rounded-br-xl";
      "outline";
      "outline-2";
      "outline-dashed";
      "outline-offset-2";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"borders render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* rounded-sm's default radius is .25rem in v4.3.1, not the old .125rem. *)
let test_rounded_sm_default () =
  let css = Tw.to_css ~base:false [ Tw.rounded_sm ] |> Tw.Css.to_string in
  Alcotest.(check bool)
    "rounded-sm default is .25rem" true
    (Astring.String.is_infix ~affix:"--radius-sm: .25rem" css)

(* rounded-xs is a v4.3.1 addition: references var(--radius-xs) and emits the
   .125rem default token. *)
let test_rounded_xs () =
  let css = Tw.to_css ~base:false [ Tw.rounded_xs ] |> Tw.Css.to_string in
  Alcotest.(check bool)
    "rounded-xs default is .125rem" true
    (Astring.String.is_infix ~affix:"--radius-xs: .125rem" css);
  Alcotest.(check bool)
    "rounded-xs references var(--radius-xs)" true
    (Astring.String.is_infix ~affix:"border-radius: var(--radius-xs)" css)

(* rounded-4xl is a v4.3.1 addition: references var(--radius-4xl) and emits the
   2rem default token. *)
let test_rounded_4xl () =
  let css = Tw.to_css ~base:false [ Tw.rounded_4xl ] |> Tw.Css.to_string in
  Alcotest.(check bool)
    "rounded-4xl default is 2rem" true
    (Astring.String.is_infix ~affix:"--radius-4xl: 2rem" css);
  Alcotest.(check bool)
    "rounded-4xl references var(--radius-4xl)" true
    (Astring.String.is_infix ~affix:"border-radius: var(--radius-4xl)" css)

(* Per-side/corner full radius inlines the infinite value (matching the
   all-corners variant and Tailwind's calc(infinity*1px)), not a --radius-full
   token that defaulted to the wrong 9999px. *)
let test_rounded_side_full_inlined () =
  let css =
    match Tw.of_string "rounded-l-full" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.fail "could not parse rounded-l-full"
  in
  Alcotest.(check bool)
    "rounded-l-full inlines the infinite radius" true
    (Astring.String.is_infix ~affix:"3.40282e38px" css);
  Alcotest.(check bool)
    "rounded-l-full emits no --radius-full token" false
    (Astring.String.is_infix ~affix:"--radius-full" css);
  Alcotest.(check bool)
    "rounded-l-full is not the old 9999px" false
    (Astring.String.is_infix ~affix:"9999px" css)

(* Numeric outline widths (outline-1/2/4/8) emit outline-width: Npx with the
   outline-style var; they used to be unknown classes. *)
let test_outline_widths () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "outline-2 emits outline-width: 2px" true
    (Astring.String.is_infix ~affix:"outline-width: 2px" (css "outline-2"));
  Alcotest.(check bool)
    "outline-1 emits outline-width: 1px" true
    (Astring.String.is_infix ~affix:"outline-width: 1px" (css "outline-1"));
  Alcotest.(check bool)
    "outline-3 emits outline-width: 3px (v4 bare integer)" true
    (Astring.String.is_infix ~affix:"outline-width: 3px" (css "outline-3"))

(* Arbitrary outline-offset lengths (outline-offset-[3px]) emit the length,
   alongside the var() form (outline-offset-[var(--x)]). *)
let test_outline_offset_arbitrary () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "outline-offset-[3px] emits outline-offset: 3px" true
    (Astring.String.is_infix ~affix:"outline-offset: 3px"
       (css "outline-offset-[3px]"))

(* outline-hidden's forced-colors reset is its own @media block; under a state
   modifier (focus, focus-within) the block must use the modified selector, not
   the bare .outline-hidden, and stay grouped with the regular rule. It used to
   keep the bare selector and reorder before the regular rule. *)
let test_outline_hidden_modifier_forced_colors () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let out = css "focus:outline-hidden" in
  Alcotest.(check bool)
    "forced-colors block uses the focus-modified selector" true
    (Astring.String.is_infix ~affix:".focus\\:outline-hidden:focus" out);
  Alcotest.(check bool)
    "forced-colors block is not the bare .outline-hidden" false
    (Astring.String.is_infix ~affix:" .outline-hidden " out)

(* The outline family sorts as one block: outline-hidden, the widths, the
   offsets, the colors, then the styles. Order matters beyond byte parity here:
   outline-hidden's forced-colors reset writes the outline shorthand, so a color
   rule ahead of it loses its outline-color. *)
let test_outline_ordering () =
  let parse cls =
    match Tw.of_string cls with
    | Ok u -> u
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let utilities =
    List.map parse
      [
        "outline";
        "outline-2";
        "outline-[3px]";
        "outline-hidden";
        "outline-offset-2";
        "-outline-offset-4";
        "outline-red-500";
        "outline-blue-500";
        "outline-current";
        "outline-transparent";
        "outline-dashed";
        "outline-solid";
        "outline-none";
      ]
  in
  Test_helpers.check_ordering_matches ~test_name:"outline family order"
    (Test_helpers.shuffle utilities)

(* Arbitrary per-side border widths (border-t-[1px], ...) emit the side width
   plus the side border-style var; they used to be unknown classes. *)
let test_border_side_arbitrary_width () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "border-t-[1px] sets border-top-width" true
    (Astring.String.is_infix ~affix:"border-top-width: 1px"
       (css "border-t-[1px]"));
  Alcotest.(check bool)
    "border-l-[0.5rem] sets border-left-width" true
    (Astring.String.is_infix ~affix:"border-left-width: .5rem"
       (css "border-l-[0.5rem]"))

(* Logical single-side borders emit the inline/block start/end style var and
   width, like the physical per-side borders do. *)
let test_logical_side_borders () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "border-s sets border-inline-start-width: 1px" true
    (Astring.String.is_infix ~affix:"border-inline-start-width: 1px"
       (css "border-s"));
  Alcotest.(check bool)
    "border-be-2 sets border-block-end-width: 2px" true
    (Astring.String.is_infix ~affix:"border-block-end-width: 2px"
       (css "border-be-2"));
  Alcotest.(check bool)
    "border-e references the border-style var" true
    (Astring.String.is_infix
       ~affix:"border-inline-end-style: var(--tw-border-style)" (css "border-e"))

(* Arbitrary outline and border widths take any CSS length unit, not just the
   px/rem/em/% the hand-rolled suffix parsers knew: Tailwind emits
   outline-width: 3rem for outline-[3rem] and border-width: 3vw for
   border-[3vw]. *)
let test_bracket_width_units () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "outline-width: 3rem" "outline-[3rem]";
  emits "outline-width: 2em" "outline-[2em]";
  emits "outline-width: 3px" "outline-[3px]";
  emits "outline-width: 50%" "outline-[50%]";
  emits "border-width: 3vw" "border-[3vw]";
  emits "border-width: 2ch" "border-[2ch]";
  emits "border-width: .5rem" "border-[0.5rem]";
  emits "border-top-width: 3vw" "border-t-[3vw]"

(* A math function in a width bracket stands for the width it computes, so it is
   a width and not a colour. The bracket was classified by its first character,
   which put [calc(...)] on the colour side and refused the class Tailwind
   renders. *)
let test_bracket_math_function_width () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "border-width: calc(1rem + 2px)" "border-[calc(1rem_+_2px)]";
  emits "border-top-width: calc(1rem + 2px)" "border-t-[calc(1rem_+_2px)]";
  emits "border-width: min(2px, 1rem)" "border-[min(2px,1rem)]";
  emits "border-width: clamp(1px, 2vw, 3rem)" "border-[clamp(1px,2vw,3rem)]";
  emits "outline-width: calc(1rem + 2px)" "outline-[calc(1rem_+_2px)]";
  (* a bare var() is still a colour on both *)
  emits "border-color: var(--w)" "border-[var(--w)]";
  emits "outline-color: var(--w)" "outline-[var(--w)]"

(* The three CSS line-width keywords are border widths in their own right, so a
   bracket naming one is a width and not an unknown class: Tailwind emits
   border-width: thin for border-[thin], and the same for medium and thick on
   every side. *)
let test_bracket_line_width_keywords () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "border-width: thin" "border-[thin]";
  emits "border-width: medium" "border-[medium]";
  emits "border-width: thick" "border-[thick]";
  emits "border-top-width: thin" "border-t-[thin]";
  emits "border-left-width: thick" "border-l-[thick]"

(* A [--radius-*] token the project declared in its [@theme] names a corner
   radius the built-in scale has no slot for. Tailwind generates the utility
   from it, on the all-corners form and on every side and corner; tw rejected
   the class outright, so the project token emitted nothing. *)
let test_project_radius_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("radius-blob", "3rem") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "border-radius: var(--radius-blob)" "rounded-blob";
  emits "border-top-left-radius: var(--radius-blob)" "rounded-t-blob";
  emits "border-top-right-radius: var(--radius-blob)" "rounded-t-blob";
  emits "border-top-left-radius: var(--radius-blob)" "rounded-tl-blob";
  (* A name the project did not declare stays an unknown class. *)
  Alcotest.(check bool)
    "an undeclared radius name is rejected" true
    (Result.is_error (Tw.of_string ~theme "rounded-nope"))

(* Every [border-<side>-<n>] writes the same [border-top-width], so their
   relative order decides which one wins. The four nullary widths (0, 2, 4, 8)
   were sorted on an ordinal that packed them into four slots and left the
   bracket in the next one, so any other width sorted past the bracket:
   [border-t-6] landed after [border-t-\[3px\]] where Tailwind puts it between 4
   and 8. *)
let test_side_width_order () =
  Test_helpers.check_class_order ~test_name:"border side widths"
    [
      "border-t";
      "border-t-0";
      "border-t-2";
      "border-t-4";
      "border-t-6";
      "border-t-8";
      "border-t-[3px]";
    ]

(* A bracket whose content is not a length is not an outline or border width:
   the parser rejects it, rather than accepting it and raising from the length
   conversion once the sheet is rendered. *)
let test_invalid_bracket_widths () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "outline-[.]";
  rejected "outline-[1e]";
  rejected "outline-[-]";
  rejected "border-[.]";
  rejected "border-[abc]";
  rejected "border-t-[1e]"

let tests =
  [
    test_case "bracket width units" `Quick test_bracket_width_units;
    test_case "invalid bracket widths" `Quick test_invalid_bracket_widths;
    test_case "bracket line-width keywords" `Quick
      test_bracket_line_width_keywords;
    test_case "bracket math-function width" `Quick
      test_bracket_math_function_width;
    test_case "project radius token" `Quick test_project_radius_token;
    test_case "border side width order" `Slow test_side_width_order;
    test_case "rounded-sm default radius" `Quick test_rounded_sm_default;
    test_case "rounded-xs default radius" `Quick test_rounded_xs;
    test_case "rounded-4xl default radius" `Quick test_rounded_4xl;
    test_case "rounded-l-full inlined radius" `Quick
      test_rounded_side_full_inlined;
    test_case "outline numeric widths" `Quick test_outline_widths;
    test_case "outline-offset arbitrary length" `Quick
      test_outline_offset_arbitrary;
    test_case "outline-hidden modifier forced-colors" `Quick
      test_outline_hidden_modifier_forced_colors;
    test_case "outline family order matches Tailwind" `Quick
      test_outline_ordering;
    test_case "border side arbitrary widths" `Quick
      test_border_side_arbitrary_width;
    test_case "logical single-side borders" `Quick test_logical_side_borders;
    test_case "borders of_string - valid values" `Quick of_string_valid;
    test_case "borders of_string - invalid values" `Quick of_string_invalid;
    test_case "borders suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "borders render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("borders", tests)
