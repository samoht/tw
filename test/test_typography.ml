open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Typography.of_string parts with
  | Ok result ->
      Alcotest.check string "typography class name" expected (Tw.Core.pp result)
  | Error (`Msg msg) -> fail msg

let test_font_family () =
  check [ "font"; "sans" ];
  check [ "font"; "serif" ];
  check [ "font"; "mono" ]

let test_font_size () =
  List.iter
    (fun s -> check [ "text"; s ])
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
    (fun w -> check [ "font"; w ])
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
    (fun a -> check [ "text"; a ])
    [ "left"; "center"; "right"; "justify"; "start"; "end" ]

let test_text_decoration () =
  check [ "underline" ];
  check [ "overline" ];
  check [ "line"; "through" ];
  check [ "no"; "underline" ]

let test_text_transform () =
  check [ "uppercase" ];
  check [ "lowercase" ];
  check [ "capitalize" ];
  check [ "normal"; "case" ]

let test_line_height () =
  List.iter
    (fun v -> check [ "leading"; v ])
    [
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
    (fun v -> check [ "tracking"; v ])
    [ "tighter"; "tight"; "normal"; "wide"; "wider"; "widest" ]

let test_typography_of_string_invalid () =
  (* Invalid typography values *)
  let fail_maybe input =
    match Tw.Typography.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "font"; "invalid" ];
  (* Invalid font family *)
  fail_maybe [ "text"; "10xl" ];
  (* Invalid text size *)
  fail_maybe [ "font"; "superheavy" ];
  (* Invalid font weight *)
  fail_maybe [ "text"; "middle" ];
  (* Invalid text alignment *)
  fail_maybe [ "leading"; "11" ];
  (* Invalid line height *)
  fail_maybe [ "leading"; "2" ];
  (* Invalid line height *)
  fail_maybe [ "tracking"; "tightest" ];
  (* Invalid letter spacing *)
  fail_maybe [ "unknown" ]
(* Unknown typography type *)

let tests =
  [
    test_case "font family" `Quick test_font_family;
    test_case "font size" `Quick test_font_size;
    test_case "font weight" `Quick test_font_weight;
    test_case "text alignment" `Quick test_text_alignment;
    test_case "text decoration" `Quick test_text_decoration;
    test_case "text transform" `Quick test_text_transform;
    test_case "line height" `Quick test_line_height;
    test_case "letter spacing" `Quick test_letter_spacing;
    test_case "typography of_string - invalid values" `Quick
      test_typography_of_string_invalid;
  ]

let suite = ("typography", tests)
