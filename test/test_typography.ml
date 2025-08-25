open Alcotest

let test_typography_of_string_valid () =
  (* Valid typography values *)
  let test_valid input expected =
    match Tw.Typography.of_string input with
    | Ok result ->
        check string "typography class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  (* Font family *)
  test_valid [ "font"; "sans" ] "font-sans";
  test_valid [ "font"; "serif" ] "font-serif";
  test_valid [ "font"; "mono" ] "font-mono";

  (* Font size *)
  test_valid [ "text"; "xs" ] "text-xs";
  test_valid [ "text"; "sm" ] "text-sm";
  test_valid [ "text"; "base" ] "text-base";
  test_valid [ "text"; "lg" ] "text-lg";
  test_valid [ "text"; "xl" ] "text-xl";
  test_valid [ "text"; "2xl" ] "text-2xl";
  test_valid [ "text"; "3xl" ] "text-3xl";
  test_valid [ "text"; "4xl" ] "text-4xl";
  test_valid [ "text"; "5xl" ] "text-5xl";
  test_valid [ "text"; "6xl" ] "text-6xl";
  test_valid [ "text"; "7xl" ] "text-7xl";
  test_valid [ "text"; "8xl" ] "text-8xl";
  test_valid [ "text"; "9xl" ] "text-9xl";

  (* Font weight *)
  test_valid [ "font"; "thin" ] "font-thin";
  test_valid [ "font"; "extralight" ] "font-extralight";
  test_valid [ "font"; "light" ] "font-light";
  test_valid [ "font"; "normal" ] "font-normal";
  test_valid [ "font"; "medium" ] "font-medium";
  test_valid [ "font"; "semibold" ] "font-semibold";
  test_valid [ "font"; "bold" ] "font-bold";
  test_valid [ "font"; "extrabold" ] "font-extrabold";
  test_valid [ "font"; "black" ] "font-black";

  (* Text alignment *)
  test_valid [ "text"; "left" ] "text-left";
  test_valid [ "text"; "center" ] "text-center";
  test_valid [ "text"; "right" ] "text-right";
  test_valid [ "text"; "justify" ] "text-justify";
  test_valid [ "text"; "start" ] "text-start";
  test_valid [ "text"; "end" ] "text-end";

  (* Text decoration *)
  test_valid [ "underline" ] "underline";
  test_valid [ "overline" ] "overline";
  test_valid [ "line"; "through" ] "line-through";
  test_valid [ "no"; "underline" ] "no-underline";

  (* Text transform *)
  test_valid [ "uppercase" ] "uppercase";
  test_valid [ "lowercase" ] "lowercase";
  test_valid [ "capitalize" ] "capitalize";
  test_valid [ "normal"; "case" ] "normal-case";

  (* Line height *)
  test_valid [ "leading"; "3" ] "leading-3";
  test_valid [ "leading"; "4" ] "leading-4";
  test_valid [ "leading"; "5" ] "leading-5";
  test_valid [ "leading"; "6" ] "leading-6";
  test_valid [ "leading"; "7" ] "leading-7";
  test_valid [ "leading"; "8" ] "leading-8";
  test_valid [ "leading"; "9" ] "leading-9";
  test_valid [ "leading"; "10" ] "leading-10";
  test_valid [ "leading"; "none" ] "leading-none";
  test_valid [ "leading"; "tight" ] "leading-tight";
  test_valid [ "leading"; "snug" ] "leading-snug";
  test_valid [ "leading"; "normal" ] "leading-normal";
  test_valid [ "leading"; "relaxed" ] "leading-relaxed";
  test_valid [ "leading"; "loose" ] "leading-loose";

  (* Letter spacing *)
  test_valid [ "tracking"; "tighter" ] "tracking-tighter";
  test_valid [ "tracking"; "tight" ] "tracking-tight";
  test_valid [ "tracking"; "normal" ] "tracking-normal";
  test_valid [ "tracking"; "wide" ] "tracking-wide";
  test_valid [ "tracking"; "wider" ] "tracking-wider";
  test_valid [ "tracking"; "widest" ] "tracking-widest"

let test_typography_of_string_invalid () =
  (* Invalid typography values *)
  let test_invalid input =
    match Tw.Typography.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "font"; "invalid" ];
  (* Invalid font family *)
  test_invalid [ "text"; "10xl" ];
  (* Invalid text size *)
  test_invalid [ "font"; "superheavy" ];
  (* Invalid font weight *)
  test_invalid [ "text"; "middle" ];
  (* Invalid text alignment *)
  test_invalid [ "leading"; "11" ];
  (* Invalid line height *)
  test_invalid [ "leading"; "2" ];
  (* Invalid line height *)
  test_invalid [ "tracking"; "tightest" ];
  (* Invalid letter spacing *)
  test_invalid [ "unknown" ]
(* Unknown typography type *)

let tests =
  [
    test_case "typography of_string - valid values" `Quick
      test_typography_of_string_valid;
    test_case "typography of_string - invalid values" `Quick
      test_typography_of_string_invalid;
  ]
