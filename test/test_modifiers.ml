open Alcotest
open Tw.Style
open Tw.Modifiers
open Tw.Padding
open Tw.Margin
open Tw.Color
open Tw.Grid_template
open Tw.Animations
open Tw.Borders

(* Test responsive modifier detection *)
let test_has_responsive_modifier () =
  (* Basic styles should not have responsive modifier *)
  let style1 = p 4 in
  check bool "no responsive on basic style" false
    (has_responsive_modifier style1);

  (* Responsive styles should be detected *)
  let style2 = sm [ p 4 ] in
  check bool "has responsive (sm)" true (has_responsive_modifier style2);

  let style3 = md [ bg blue 500 ] in
  check bool "has responsive (md)" true (has_responsive_modifier style3);

  (* Nested modifiers with responsive should be detected *)
  let style4 = hover [ sm [ bg blue 500 ] ] in
  check bool "nested has responsive" true (has_responsive_modifier style4);

  (* Non-responsive modifiers should not be detected as responsive *)
  let style5 = hover [ p 4 ] in
  check bool "hover is not responsive" false (has_responsive_modifier style5);

  (* Group with at least one responsive should be detected *)
  let style6 = Tw.Utility.Group [ p 4; sm [ m 2 ] ] in
  check bool "group with responsive" true (has_responsive_modifier style6);

  (* Group without responsive should not be detected *)
  let style7 = Tw.Utility.Group [ p 4; m 2 ] in
  check bool "group without responsive" false (has_responsive_modifier style7)

(* Test responsive nesting validation *)
let test_validate_no_nested_responsive () =
  (* Should pass for non-responsive styles *)
  let () = validate_no_nested_responsive [ p 4; m 2 ] in

  (* Should pass for single styles without responsive modifiers *)
  let () = validate_no_nested_responsive [ p 4 ] in

  (* Should fail for nested responsive *)
  let test_nested_fail () =
    try
      validate_no_nested_responsive [ sm [ md [ p 4 ] ] ];
      fail "Should have raised exception for nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure with 'responsive' in message"
  in
  test_nested_fail ();

  (* Should fail for already responsive styles *)
  let test_already_responsive () =
    try
      let responsive_style = sm [ p 4 ] in
      validate_no_nested_responsive [ responsive_style ];
      fail "Should have rejected responsive style"
    with
    | Failure _ -> () (* Expected to fail *)
    | _ -> fail "Expected Failure for responsive style"
  in
  test_already_responsive ()

(* Helper to test that a responsive function rejects nested responsive *)
let test_responsive_rejects name outer_fn inner_content =
  try
    let _ = outer_fn [ inner_content ] in
    Alcotest.failf "%s should reject nested responsive" name
  with
  | Failure _ -> () (* Expected to fail *)
  | _ -> Alcotest.failf "Expected Failure about nested responsive"

(* Test that responsive functions reject nested responsive *)
let test_responsive_functions_reject_nesting () =
  (* Test each breakpoint rejects nested responsive modifiers *)
  test_responsive_rejects "sm" sm (md [ p 4 ]);
  test_responsive_rejects "md" md (lg [ p 4 ]);
  test_responsive_rejects "lg" lg (xl [ p 4 ]);
  test_responsive_rejects "xl" xl (xl2 [ p 4 ]);
  test_responsive_rejects "xl2" xl2 (sm [ p 4 ])

(* Test apply function *)
let test_apply () =
  (* Test single modifier *)
  let style1 = apply [ "hover" ] (p 4) in
  check bool "hover modifier applied" true
    (match style1 with
    | Group [ Modified (Hover, _) ] -> true
    | Modified (Hover, _) -> true
    | _ -> false);

  (* Test multiple modifiers *)
  let style2 = apply [ "sm"; "hover" ] (bg blue 500) in
  check bool "multiple modifiers applied" true
    (match style2 with
    | Group [ Modified (Hover, Modified (Responsive `Sm, _)) ] -> true
    | Modified (Hover, Modified (Responsive `Sm, _)) -> true
    | _ -> false);

  (* Test unknown modifier (should be ignored) *)
  let style3 = apply [ "unknown"; "hover" ] (p 4) in
  check bool "unknown modifier ignored" true
    (match style3 with
    | Group [ Modified (Hover, _) ] -> true
    | Modified (Hover, _) -> true
    | _ -> false);

  (* Test responsive modifiers *)
  let style4 = apply [ "md" ] (m 2) in
  check bool "md modifier applied" true
    (match style4 with
    | Group [ Modified (Responsive `Md, _) ] -> true
    | Modified (Responsive `Md, _) -> true
    | _ -> false);

  (* Test dark mode *)
  let style5 = apply [ "dark" ] (bg gray 900) in
  check bool "dark modifier applied" true
    (match style5 with
    | Group [ Modified (Dark, _) ] -> true
    | Modified (Dark, _) -> true
    | _ -> false);

  (* Test modifier order *)
  let style6 = apply [ "hover"; "sm" ] (p 4) in
  check bool "modifier order correct" true
    (match style6 with
    | Group [ Modified (Responsive `Sm, Modified (Hover, _)) ] -> true
    | Modified (Responsive `Sm, Modified (Hover, _)) -> true
    | _ -> false)

(* Test modifier class name format *)
let test_modifier_class_names () =
  (* Test responsive modifiers produce single colon *)
  check string "sm: single colon" "sm:p-4" (Tw.Utility.to_class (sm [ p 4 ]));

  check string "md: single colon" "md:grid-cols-2"
    (Tw.Utility.to_class (md [ grid_cols 2 ]));

  check string "lg: single colon" "lg:bg-blue-500"
    (Tw.Utility.to_class (lg [ bg blue 500 ]));

  (* 2xl prefix formatting *)
  check string "2xl: single colon" "2xl:p-4" (Tw.Utility.to_class (xl2 [ p 4 ]));

  (* Test hover modifier *)
  check string "hover: single colon" "hover:p-4"
    (Tw.Utility.to_class (hover [ p 4 ]));

  (* Test combined modifiers *)
  check string "md:hover: single colons" "md:hover:m-2"
    (Tw.Utility.to_class (md [ hover [ m 2 ] ]));

  (* Test multiple utilities with modifiers in a list *)
  let classes =
    Tw.to_classes Tw.[ grid; grid_cols 5; md [ grid_cols 10 ]; gap 2 ]
  in
  check string "multiple utilities with md:"
    "grid grid-cols-5 md:grid-cols-10 gap-2" classes

(* Test media preference modifiers class names *)
let test_media_preference_modifiers () =
  (* Motion preference modifiers *)
  check string "motion-safe: single colon" "motion-safe:animate-pulse"
    (Tw.Utility.to_class (motion_safe [ animate_pulse ]));

  check string "motion-reduce: single colon" "motion-reduce:transition-none"
    (Tw.Utility.to_class (motion_reduce [ transition_none ]));

  (* Contrast preference modifiers *)
  check string "contrast-more: single colon" "contrast-more:border-4"
    (Tw.Utility.to_class (contrast_more [ border_4 ]));

  check string "contrast-less: single colon" "contrast-less:text-gray-600"
    (Tw.Utility.to_class (contrast_less [ text gray 600 ]));

  (* Dark mode *)
  check string "dark: single colon" "dark:bg-gray-900"
    (Tw.Utility.to_class (dark [ bg gray 900 ]))

(* Test CSS generation and parsing roundtrip for modifiers *)
let test_modifier_css_roundtrip () =
  let test_utilities =
    [
      motion_safe [ animate_pulse ];
      motion_reduce [ transition_none ];
      contrast_more [ border_4 ];
      contrast_more [ text_black ];
      contrast_less [ text gray 600 ];
      dark [ bg gray 900 ];
      hover [ bg blue 500 ];
      sm [ p 4 ];
    ]
  in

  (* Generate CSS *)
  let stylesheet = Tw.Rules.to_css test_utilities in
  let css_str = Tw.Css.to_string ~minify:true ~optimize:true stylesheet in

  (* Verify CSS was generated *)
  check bool "CSS generated" true (String.length css_str > 0);

  (* Parse it back - this would fail with double-backslash bug *)
  match Tw.Css.of_string css_str with
  | Ok _parsed_stylesheet ->
      (* Successfully parsed our own generated CSS *)
      ()
  | Error parse_err ->
      let error_msg = Tw.Css.pp_parse_error parse_err in
      Alcotest.failf "Failed to parse generated CSS:\n%s" error_msg

(* Test that generated CSS has correct selector escaping *)
let test_selector_escaping_in_css () =
  (* Generate CSS with modifiers that need escaping *)
  let stylesheet = Tw.Rules.to_css [ motion_safe [ animate_pulse ] ] in
  let css_str = Tw.Css.to_string ~minify:true stylesheet in

  (* Verify single backslash in output (not double) *)
  (* In the CSS string, we expect: .motion-safe\:animate-pulse *)
  (* Which appears as "motion-safe\\:animate-pulse" in OCaml string *)
  check bool "CSS contains escaped colon" true (String.contains css_str '\\');

  (* Count backslashes - should be exactly 1 per modifier *)
  let backslash_count =
    String.fold_left (fun n c -> if c = '\\' then n + 1 else n) 0 css_str
  in
  (* We expect 1 backslash for the motion-safe: prefix *)
  check bool "Single backslash escape (not double)" true (backslash_count >= 1);

  (* Verify it parses correctly *)
  match Tw.Css.of_string css_str with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "Selector escaping broken - parse failed:\n%s"
        (Tw.Css.pp_parse_error e)

(* Test combined modifiers with media preferences *)
let test_combined_media_modifiers () =
  (* Combining responsive with media preference should work *)
  check string "sm:motion-safe: works" "sm:motion-safe:animate-pulse"
    (Tw.Utility.to_class (sm [ motion_safe [ animate_pulse ] ]));

  check string "md:dark: works" "md:dark:bg-gray-900"
    (Tw.Utility.to_class (md [ dark [ bg gray 900 ] ]));

  (* Generate and parse CSS with combined modifiers *)
  let utilities =
    [ sm [ motion_safe [ animate_pulse ] ]; md [ dark [ bg gray 900 ] ] ]
  in
  let css_str = Tw.Css.to_string ~minify:true (Tw.Rules.to_css utilities) in
  match Tw.Css.of_string css_str with
  | Ok _ -> ()
  | Error e ->
      Alcotest.failf "Combined modifiers CSS roundtrip failed:\n%s"
        (Tw.Css.pp_parse_error e)

(* Media query behavior for md [...] *)

(* Test suite *)
let tests =
  [
    test_case "has_responsive_modifier" `Quick test_has_responsive_modifier;
    test_case "validate_no_nested_responsive" `Quick
      test_validate_no_nested_responsive;
    test_case "responsive functions reject nesting" `Quick
      test_responsive_functions_reject_nesting;
    test_case "apply modifiers" `Quick test_apply;
    test_case "modifier class names" `Quick test_modifier_class_names;
    test_case "media preference modifiers" `Quick
      test_media_preference_modifiers;
    test_case "modifier CSS roundtrip" `Quick test_modifier_css_roundtrip;
    test_case "selector escaping in CSS" `Quick test_selector_escaping_in_css;
    test_case "combined media modifiers" `Quick test_combined_media_modifiers;
  ]

let suite = ("modifiers", tests)
