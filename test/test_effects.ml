open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Effects.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Effects.Handler.to_style result in
      Alcotest.check string "effects class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  (* Box shadow *)
  check [ "shadow" ];
  check [ "shadow"; "sm" ];
  check [ "shadow"; "md" ];
  check [ "shadow"; "lg" ];
  check [ "shadow"; "xl" ];
  check [ "shadow"; "2xl" ];
  check [ "shadow"; "inner" ];
  check [ "shadow"; "none" ];

  (* Opacity *)
  check [ "opacity"; "0" ];
  check [ "opacity"; "5" ];
  check [ "opacity"; "10" ];
  check [ "opacity"; "25" ];
  check [ "opacity"; "50" ];
  check [ "opacity"; "75" ];
  check [ "opacity"; "100" ];

  (* Mix blend mode *)
  check [ "mix"; "blend"; "normal" ];
  check [ "mix"; "blend"; "multiply" ];
  check [ "mix"; "blend"; "screen" ];
  check [ "mix"; "blend"; "overlay" ]

let test_ring_of_string_valid () =
  check [ "ring" ];
  check [ "ring"; "0" ];
  check [ "ring"; "1" ];
  check [ "ring"; "2" ];
  check [ "ring"; "4" ];
  check [ "ring"; "8" ];
  check [ "ring"; "inset" ]

let test_filters_css_generation () =
  (* Spot-check a few filter/backdrop utilities *)
  let open Tw in
  let css =
    Rules.to_css
      [
        Utility.to_style Filters.blur;
        Utility.to_style Filters.backdrop_blur_lg;
        Utility.to_style (Filters.backdrop_brightness 125);
        Utility.to_style (Filters.backdrop_opacity 50);
      ]
    |> Css.to_string
  in
  Alcotest.check bool "has filter property" true
    (Astring.String.is_infix ~affix:"filter:" css);
  Alcotest.check bool "has backdrop-filter property" true
    (Astring.String.is_infix ~affix:"backdrop-filter:" css)

let of_string_invalid () =
  (* Invalid effects values *)
  let fail_maybe input =
    match Tw.Effects.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "shadow"; "3xl" ];
  (* Invalid shadow size *)
  fail_maybe [ "opacity"; "110" ];
  (* Invalid opacity value *)
  fail_maybe [ "mix"; "blend"; "invalid" ];
  (* Invalid blend mode *)
  fail_maybe [ "unknown" ]
(* Unknown effects type *)

let all_utilities () =
  let open Tw in
  [
    shadow_sm;
    shadow;
    shadow_md;
    shadow_lg;
    shadow_none;
    opacity 0;
    opacity 50;
    opacity 100;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"effects suborder matches Tailwind" shuffled

let tests =
  [
    test_case "effects of_string - valid values" `Quick of_string_valid;
    test_case "effects of_string - invalid values" `Quick of_string_invalid;
    test_case "ring of_string - valid values" `Quick test_ring_of_string_valid;
    test_case "filters css generation" `Quick test_filters_css_generation;
    test_case "effects suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("effects", tests)
