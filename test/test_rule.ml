module Css = Cascade.Css
open Alcotest
open Tw.Output
open Tw.Color
open Tw.Padding
open Tw.Modifiers

(* ===== Tests ===== *)

let check_extract_selector_props () =
  let rules = Tw.Rule.outputs (p 4) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      check string "correct selector" ".p-4" (Css.Selector.to_string selector)
  | _ -> fail "Expected Regular rule"

let check_extract_hover () =
  let rules = Tw.Rule.outputs (hover [ bg blue ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Regular { selector; _ } ] ->
      let sel_str = Css.Selector.to_string selector in
      check string "hover selector" ".hover\\:bg-blue-500:hover" sel_str
  | _ -> fail "Expected Regular rule with hover"

let check_extract_responsive () =
  let rules = Tw.Rule.outputs (sm [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "media condition" "(min-width: 40rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "sm selector"
        (Css.Selector.class_ "sm:p-4")
        selector
  | _ -> fail "Expected Media_query rule"

let check_extract_responsive_md () =
  let rules = Tw.Rule.outputs (md [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "md media condition" "(min-width: 48rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "md selector"
        (Css.Selector.class_ "md:p-4")
        selector
  | _ -> fail "Expected Media_query rule for md"

let check_extract_responsive_lg () =
  let rules = Tw.Rule.outputs (lg [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "lg media condition" "(min-width: 64rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "lg selector"
        (Css.Selector.class_ "lg:p-4")
        selector
  | _ -> fail "Expected Media_query rule for lg"

let check_extract_responsive_xl () =
  let rules = Tw.Rule.outputs (xl [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "xl media condition" "(min-width: 80rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "xl selector"
        (Css.Selector.class_ "xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for xl"

let check_extract_responsive_2xl () =
  let rules = Tw.Rule.outputs (xl2 [ p 4 ]) in
  check int "single rule extracted" 1 (List.length rules);
  match rules with
  | [ Media_query { condition; selector; _ } ] ->
      check string "2xl media condition" "(min-width: 96rem)"
        (Css.Media.to_string condition);
      check Test_helpers.selector_testable "2xl selector"
        (Css.Selector.class_ "2xl:p-4")
        selector
  | _ -> fail "Expected Media_query rule for 2xl"

let check_escape_class_name () =
  check string "escapes brackets" "p-\\[10px\\]"
    (Tw.Rule.escape_class_name "p-[10px]");
  check string "escapes colon" "hover\\:bg-blue-500"
    (Tw.Rule.escape_class_name "hover:bg-blue-500");
  check string "escapes slash" "w-1\\/2" (Tw.Rule.escape_class_name "w-1/2");
  check string "escapes dot" "text-1\\.5" (Tw.Rule.escape_class_name "text-1.5")

let test_modifier_to_rule () =
  let rule =
    Tw.Rule.modifier_to_rule Tw.Style.Hover "bg-blue-500"
      (Css.Selector.class_ "bg-blue-500")
      [ Css.background_color (Css.hex "#3b82f6") ]
  in
  match rule with
  | Tw.Output.Regular { selector; props; has_hover; _ } ->
      (* Hover modifier uses Modifiers.to_selector which includes the prefix *)
      check string "hover selector" ".hover\\:bg-blue-500:hover"
        (Css.Selector.to_string selector);
      check int "preserves props" 1 (List.length props);
      check bool "marked as hover" true has_hover
  | _ -> fail "Expected Regular rule for hover"

(* Arbitrary selector variants whose remainder starts with a combinator
   ([&>div], [&+p], [&~p]) used to crash the renderer with a Cascade
   Parse_error: the code forced a descendant combine and re-parsed ">div", which
   the selector reader rejects. They must flatten to the combinator selector. *)
let test_arbitrary_selector_combinator () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check bool "[&>div:first-child] keeps the child combinator" true
    (Astring.String.is_infix ~affix:"> div:first-child"
       (css "[&>div:first-child]:ring-2"));
  check bool "[&+p] keeps the adjacent-sibling combinator" true
    (Astring.String.is_infix ~affix:"+ p" (css "[&+p]:underline"));
  check bool "[&_p] still flattens to a descendant" true
    (Astring.String.is_infix ~affix:"]\\:underline p" (css "[&_p]:underline"))

(* Regression: an opacity color emits a progressive-enhancement @supports block.
   Under a variant, that block must stay scoped to the variant instead of
   leaking a bare base-class rule. dark:text-white/80 previously emitted a
   top-level .text-white/80 @supports rule alongside the correct
   .dark:text-white/80. *)
let test_opacity_color_variant_no_leak () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  (* The base class must never appear as a standalone selector (".text-white");
     only the variant-decorated ".dark\:text-white" / ".hover\:text-red" is
     allowed. *)
  check bool "dark:text-white/80 leaks no bare .text-white" false
    (Astring.String.is_infix ~affix:".text-white" (css "dark:text-white/80"));
  check bool "hover:text-red-500/50 leaks no bare .text-red" false
    (Astring.String.is_infix ~affix:".text-red" (css "hover:text-red-500/50"))

let tests =
  [
    test_case "arbitrary selector combinator variants" `Quick
      test_arbitrary_selector_combinator;
    test_case "opacity color variant does not leak base rule" `Quick
      test_opacity_color_variant_no_leak;
    test_case "extract selector props - basic" `Quick
      check_extract_selector_props;
    test_case "extract selector props - hover" `Quick check_extract_hover;
    test_case "extract selector props - responsive" `Quick
      check_extract_responsive;
    test_case "extract selector props - responsive md" `Quick
      check_extract_responsive_md;
    test_case "extract selector props - responsive lg" `Quick
      check_extract_responsive_lg;
    test_case "extract selector props - responsive xl" `Quick
      check_extract_responsive_xl;
    test_case "extract selector props - responsive 2xl" `Quick
      check_extract_responsive_2xl;
    test_case "escape class name" `Quick check_escape_class_name;
    test_case "modifier_to_rule" `Quick test_modifier_to_rule;
  ]

let suite = ("rule", tests)
