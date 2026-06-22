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

  check "border-t";
  check "border-r";
  check "border-b";
  check "border-l";
  check "border-x";
  check "border-y";

  check "border-t-2";
  check "border-r-4";

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

let tests =
  [
    test_case "rounded-sm default radius" `Quick test_rounded_sm_default;
    test_case "rounded-xs default radius" `Quick test_rounded_xs;
    test_case "rounded-4xl default radius" `Quick test_rounded_4xl;
    test_case "rounded-l-full inlined radius" `Quick
      test_rounded_side_full_inlined;
    test_case "outline numeric widths" `Quick test_outline_widths;
    test_case "outline-offset arbitrary length" `Quick
      test_outline_offset_arbitrary;
    test_case "border side arbitrary widths" `Quick
      test_border_side_arbitrary_width;
    test_case "borders of_string - valid values" `Quick of_string_valid;
    test_case "borders of_string - invalid values" `Quick of_string_invalid;
    test_case "borders suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("borders", tests)
