open Alcotest
open Tw.Output
open Tw.Color
open Tw.Padding
open Tw.Margin
open Tw.Modifiers

let test_is_hover_rule () =
  let hover_rules = Tw.Rule.outputs (hover [ bg blue 500 ]) in
  let non_hover_rules = Tw.Rule.outputs (bg blue 500) in

  (match hover_rules with
  | [ hover_rule ] ->
      check bool "detects simple hover rule" true (is_hover_rule hover_rule)
  | _ -> fail "Expected single hover rule");

  (match non_hover_rules with
  | [ non_hover_rule ] ->
      check bool "detects no hover" false (is_hover_rule non_hover_rule)
  | _ -> fail "Expected single non-hover rule");

  let sm_hover_rules = Tw.Rule.outputs (sm [ hover [ p 4 ] ]) in
  (match sm_hover_rules with
  | [ media_rule ] ->
      check bool "responsive+hover is not detected as hover" false
        (is_hover_rule media_rule)
  | _ -> fail "Expected single media rule");

  let dark_hover_rules = Tw.Rule.outputs (dark [ hover [ m 2 ] ]) in
  (match dark_hover_rules with
  | [ regular_rule ] ->
      check bool "dark+hover is not detected as hover" false
        (is_hover_rule regular_rule)
  | _ -> fail "Expected single regular rule with nested CSS");

  let focus_rules = Tw.Rule.outputs (focus [ bg red 400 ]) in
  (match focus_rules with
  | [ focus_rule ] ->
      check bool "focus alone is not hover" false (is_hover_rule focus_rule)
  | _ -> fail "Expected single focus rule");

  let group_hover_rules = Tw.Rule.outputs (group_hover [ text white 0 ]) in
  match group_hover_rules with
  | [ group_rule ] ->
      check bool "group-hover is detected as hover" true
        (is_hover_rule group_rule)
  | _ -> fail "Expected single group rule"

let test_classify () =
  let rules =
    [
      regular
        ~selector:(Css.Selector.class_ "p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      media_query ~condition:(Css.Media.Min_width 640.)
        ~selector:(Css.Selector.class_ "sm\\:p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      container_query ~condition:(Css.Container.Min_width_px 640)
        ~selector:(Css.Selector.class_ "\\@sm\\:p-4")
        ~props:[ Css.padding [ Css.Rem 1.0 ] ]
        ();
      starting_style
        ~selector:(Css.Selector.class_ "animate-in")
        ~props:[ Css.opacity (Css.Opacity_number 0.0) ]
        ();
      regular
        ~selector:(Css.Selector.class_ "m-2")
        ~props:[ Css.margin [ Css.Rem 0.5 ] ]
        ();
    ]
  in
  let classified = classify_by_type rules in
  check int "2 regular rules" 2 (List.length classified.regular);
  check int "1 media rule" 1 (List.length classified.media);
  check int "1 container rule" 1 (List.length classified.container);
  check int "1 starting rule" 1 (List.length classified.starting)

let tests =
  [
    test_case "is_hover_rule" `Quick test_is_hover_rule;
    test_case "classify_by_type" `Quick test_classify;
  ]

let suite = ("output", tests)
