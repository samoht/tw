open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Typography.Handler)

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
    [ "left"; "center"; "right"; "justify" ]

let test_text_decoration () =
  check [ "underline" ];
  check [ "overline" ];
  check [ "line"; "through" ];
  check [ "no"; "underline" ];
  (* decoration color/thickness *)
  check [ "decoration"; "from"; "font" ];
  check [ "decoration"; "2" ];
  check [ "decoration"; "blue"; "500" ]

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

let test_line_clamp () =
  check [ "line"; "clamp"; "0" ];
  check [ "line"; "clamp"; "3" ]

let test_text_overflow_wrap () =
  check [ "text"; "ellipsis" ];
  check [ "text"; "clip" ];
  check [ "text"; "wrap" ];
  check [ "text"; "nowrap" ];
  check [ "text"; "balance" ];
  check [ "text"; "pretty" ]

let test_word_overflow_wrap () =
  check [ "break"; "normal" ];
  check [ "break"; "words" ];
  check [ "break"; "all" ];
  check [ "break"; "keep" ];
  check [ "overflow"; "wrap"; "normal" ];
  check [ "overflow"; "wrap"; "anywhere" ];
  check [ "overflow"; "wrap"; "break"; "word" ]

let test_hyphens () =
  check [ "hyphens"; "none" ];
  check [ "hyphens"; "manual" ];
  check [ "hyphens"; "auto" ]

let test_list_style () =
  check [ "list"; "none" ];
  check [ "list"; "disc" ];
  check [ "list"; "decimal" ];
  check [ "list"; "inside" ];
  check [ "list"; "outside" ];
  check [ "list"; "image"; "none" ]

let test_text_indent () = check [ "indent"; "4" ]

let test_vertical_align () =
  check [ "align"; "baseline" ];
  check [ "align"; "top" ];
  check [ "align"; "middle" ];
  check [ "align"; "bottom" ];
  check [ "align"; "text"; "top" ];
  check [ "align"; "text"; "bottom" ];
  check [ "align"; "sub" ];
  check [ "align"; "super" ]

let test_font_stretch () =
  check [ "font"; "stretch"; "normal" ];
  check [ "font"; "stretch"; "condensed" ];
  check [ "font"; "stretch"; "expanded" ];
  check [ "font"; "stretch"; "150" ]

let test_numeric_variants () =
  check [ "normal"; "nums" ];
  check [ "ordinal" ];
  check [ "slashed"; "zero" ];
  check [ "lining"; "nums" ];
  check [ "oldstyle"; "nums" ];
  check [ "proportional"; "nums" ];
  check [ "tabular"; "nums" ];
  check [ "diagonal"; "fractions" ];
  check [ "stacked"; "fractions" ]

let test_content () = check [ "content"; "none" ]

let of_string_invalid () =
  (* Invalid typography values *)
  let fail_maybe = check_invalid_input (module Tw.Typography.Handler) in

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
    test_case "line clamp" `Quick test_line_clamp;
    test_case "text overflow/wrap" `Quick test_text_overflow_wrap;
    test_case "word/overflow wrap" `Quick test_word_overflow_wrap;
    test_case "hyphens" `Quick test_hyphens;
    test_case "list style" `Quick test_list_style;
    test_case "text indent" `Quick test_text_indent;
    test_case "vertical align" `Quick test_vertical_align;
    test_case "font stretch" `Quick test_font_stretch;
    test_case "numeric variants" `Quick test_numeric_variants;
    test_case "content" `Quick test_content;
    test_case "typography of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("typography", tests)
