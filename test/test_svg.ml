open Alcotest

let check_class = Test_helpers.check_class

let basic_svg () =
  check_class "fill-none" Tw.Svg.fill_none;
  check_class "stroke-2" Tw.Svg.stroke_2

let tests = [ test_case "basic svg" `Quick basic_svg ]
let suite = ("svg", tests)
