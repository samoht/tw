open Alcotest

let check_class = Test_helpers.check_class

let basic_accessibility () =
  check_class "forced-color-adjust-auto"
    Tw.Accessibility.forced_color_adjust_auto;
  check_class "forced-color-adjust-none"
    Tw.Accessibility.forced_color_adjust_none

let tests = [ test_case "basic accessibility" `Quick basic_accessibility ]
let suite = ("accessibility", tests)
