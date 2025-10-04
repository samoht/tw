open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Sizing.Handler)

let test_widths () =
  check "w-0";
  check "w-1";
  check "w-4";
  check "w-px";
  check "w-0.5";
  check "w-1/2";
  check "w-1/3";
  check "w-2/3";
  check "w-1/4";
  check "w-3/4";
  check "w-full";
  check "w-screen";
  check "w-min";
  check "w-max";
  check "w-fit";
  check "w-auto"

let test_heights () =
  check "h-0";
  check "h-4";
  check "h-full";
  check "h-screen";
  check "h-1/2"

let test_min_sizes () =
  check "min-w-0";
  check "min-w-full";
  check "min-h-0";
  check "min-h-screen"

let test_max_sizes () =
  check "max-w-none";
  check "max-w-xs";
  check "max-w-sm";
  check "max-w-md";
  check "max-w-lg";
  check "max-w-xl";
  check "max-w-2xl";
  check "max-w-7xl";
  check "max-w-full";
  check "max-w-screen-sm";
  check "max-h-4";
  check "max-h-full";
  check "max-h-screen"

let test_square_sizes () =
  check "size-0";
  check "size-4";
  check "size-full";
  check "size-1/2"

let of_string_invalid () =
  (* Invalid sizing values *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    check_invalid_input (module Tw.Sizing.Handler) class_name
  in

  test_invalid [ "w" ];
  (* Missing value *)
  test_invalid [ "w"; "invalid" ];
  (* Invalid value *)
  test_invalid [ "w"; "1/7" ];
  (* Invalid fraction - 1/7 not supported *)
  test_invalid [ "h"; "1/7" ];
  (* Invalid fraction *)
  test_invalid [ "min" ];
  (* Missing dimension *)
  test_invalid [ "min"; "z"; "4" ];
  (* Invalid dimension *)
  test_invalid [ "max"; "w"; "8xl" ];
  (* Invalid max size *)
  test_invalid [ "max"; "w"; "screen"; "3xl" ];
  (* Invalid screen size *)
  test_invalid [ "unknown"; "4" ]
(* Unknown sizing type *)

let test_aspect_classes () =
  let open Tw in
  Alcotest.check string "square class" "aspect-square" (Tw.pp aspect_square);
  Alcotest.check string "video class" "aspect-video" (Tw.pp aspect_video);
  Alcotest.check string "ratio class" "aspect-[16/9]"
    (Tw.pp (aspect_ratio 16 9))

let test_aspect_css () =
  let open Tw in
  let css = to_css [ aspect_ratio 16 9 ] |> Css.pp ~minify:true in
  Alcotest.check bool "has aspect-ratio" true
    (Astring.String.is_infix ~affix:"aspect-ratio" css);
  Alcotest.check bool "has 16/9" true
    (Astring.String.is_infix ~affix:"16/9" css)

let all_utilities () =
  let open Tw in
  List.concat_map (fun n -> [ w n; h n ]) [ 0; 1; 2; 4; 8; 12; 16; 24; 32 ]
  @ [
      min_w 0;
      min_h 0;
      max_w_none;
      max_w_full;
      max_w_2xl;
      max_w_3xl;
      max_w_4xl;
      max_w_5xl;
      max_w_6xl;
    ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"sizing suborder matches Tailwind" shuffled

let tests =
  [
    test_case "widths" `Quick test_widths;
    test_case "heights" `Quick test_heights;
    test_case "min sizes" `Quick test_min_sizes;
    test_case "max sizes" `Quick test_max_sizes;
    test_case "square sizes" `Quick test_square_sizes;
    test_case "sizing of_string - invalid values" `Quick of_string_invalid;
    test_case "aspect classes" `Quick test_aspect_classes;
    test_case "aspect css" `Quick test_aspect_css;
    test_case "sizing suborder matches Tailwind" `Slow suborder_matches_tailwind;
  ]

let suite = ("sizing", tests)
