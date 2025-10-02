open Alcotest
open Tw.Style

let check_class expected t = Alcotest.check string "class" expected (pp t)

let basic_accessibility () =
  check_class "forced-color-adjust-auto"
    (Tw.Utility.to_style Tw.Accessibility.forced_color_adjust_auto);
  check_class "forced-color-adjust-none"
    (Tw.Utility.to_style Tw.Accessibility.forced_color_adjust_none)

let tests = [ test_case "basic accessibility" `Quick basic_accessibility ]
let suite = ("accessibility", tests)
