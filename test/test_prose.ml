(* Tests for prose typography utilities *)

open Tw

let test_classes () =
  Alcotest.(check string) "prose base class" "prose" (pp prose);
  Alcotest.(check string) "prose-sm class" "prose-sm" (pp prose_sm);
  Alcotest.(check string) "prose-lg class" "prose-lg" (pp prose_lg);
  Alcotest.(check string) "prose-xl class" "prose-xl" (pp prose_xl);
  Alcotest.(check string) "prose-2xl class" "prose-2xl" (pp prose_2xl);
  Alcotest.(check string) "prose-gray class" "prose-gray" (pp prose_gray);
  Alcotest.(check string) "prose-slate class" "prose-slate" (pp prose_slate)

let test_combinations () =
  let combined = to_classes [ prose; prose_lg ] in
  Alcotest.(check string) "prose with size" "prose prose-lg" combined;

  let with_theme = to_classes [ prose; prose_slate ] in
  Alcotest.(check string) "prose with theme" "prose prose-slate" with_theme;

  let complex = to_classes [ prose; prose_xl; prose_gray; mx_auto ] in
  Alcotest.(check string)
    "complex prose" "prose prose-xl prose-gray mx-auto" complex

let test_css_generation () =
  (* Test that prose generates CSS rules *)
  let css = to_css [ prose ] in
  let css_string = Css.to_string css in

  (* Check that CSS variables are included *)
  Alcotest.(check bool)
    "has prose body variable" true
    (Astring.String.is_infix ~affix:"--tw-prose-body" css_string);

  (* Check that prose class is generated *)
  Alcotest.(check bool)
    "has prose class" true
    (Astring.String.is_infix ~affix:".prose" css_string);

  (* Check that descendant selectors are generated *)
  Alcotest.(check bool)
    "has prose h1 selector" true
    (Astring.String.is_infix ~affix:".prose :where(h1)" css_string)

let test_inline_styles () =
  (* Prose utilities don't generate inline styles - they only use descendant
     selectors *)
  let inline = to_inline_style [ prose ] in
  Alcotest.(check string) "prose has no inline styles" "" inline;

  (* Size variants also don't generate inline styles *)
  let inline_lg = to_inline_style [ prose_lg ] in
  Alcotest.(check string) "prose-lg has no inline styles" "" inline_lg

let suite =
  ( "prose",
    [
      Alcotest.test_case "classes" `Quick test_classes;
      Alcotest.test_case "combinations" `Quick test_combinations;
      Alcotest.test_case "CSS generation" `Quick test_css_generation;
      Alcotest.test_case "inline styles" `Quick test_inline_styles;
    ] )
