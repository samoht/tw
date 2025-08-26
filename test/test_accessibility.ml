open Alcotest
open Tw.Core

let check_class expected t = Alcotest.check string "class" expected (pp t)

let basic_accessibility () =
  check_class "forced-color-adjust-auto"
    Tw.Accessibility.forced_color_adjust_auto;
  check_class "forced-color-adjust-none"
    Tw.Accessibility.forced_color_adjust_none

let tests = [ test_case "basic accessibility" `Quick basic_accessibility ]
let suite = ("accessibility", tests)
