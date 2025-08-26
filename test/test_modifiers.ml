open Alcotest
open Tw.Core
open Tw.Modifiers
open Tw.Spacing
open Tw.Color

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
  let style6 = Group [ p 4; sm [ m 2 ] ] in
  check bool "group with responsive" true (has_responsive_modifier style6);

  (* Group without responsive should not be detected *)
  let style7 = Group [ p 4; m 2 ] in
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

(* Test that responsive functions reject nested responsive *)
let test_responsive_functions_reject_nesting () =
  (* Test sm rejects nested responsive *)
  let test_sm_rejects () =
    try
      let _ = sm [ md [ p 4 ] ] in
      fail "sm should reject nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure about nested responsive"
  in
  test_sm_rejects ();

  (* Test md rejects nested responsive *)
  let test_md_rejects () =
    try
      let _ = md [ lg [ p 4 ] ] in
      fail "md should reject nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure about nested responsive"
  in
  test_md_rejects ();

  (* Test lg rejects nested responsive *)
  let test_lg_rejects () =
    try
      let _ = lg [ xl [ p 4 ] ] in
      fail "lg should reject nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure about nested responsive"
  in
  test_lg_rejects ();

  (* Test xl rejects nested responsive *)
  let test_xl_rejects () =
    try
      let _ = xl [ xxl [ p 4 ] ] in
      fail "xl should reject nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure about nested responsive"
  in
  test_xl_rejects ();

  (* Test xxl rejects nested responsive *)
  let test_xxl_rejects () =
    try
      let _ = xxl [ sm [ p 4 ] ] in
      fail "xxl should reject nested responsive"
    with
    | Failure _ -> ()
    | _ -> fail "Expected Failure about nested responsive"
  in
  test_xxl_rejects ()

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

(* Test suite *)
let tests =
  [
    test_case "has_responsive_modifier" `Quick test_has_responsive_modifier;
    test_case "validate_no_nested_responsive" `Quick
      test_validate_no_nested_responsive;
    test_case "responsive functions reject nesting" `Quick
      test_responsive_functions_reject_nesting;
    test_case "apply modifiers" `Quick test_apply;
  ]

let suite = ("modifiers", tests)
