open Alcotest

let check_class expected t =
  Alcotest.check string "class" expected (Tw.Core.pp t)

let basic_flexbox () =
  check_class "flex-row" Tw.Flexbox.flex_row;
  check_class "flex-col" Tw.Flexbox.flex_col;
  check_class "items-center" Tw.Flexbox.items_center;
  check_class "justify-between" Tw.Flexbox.justify_between

let tests = [ test_case "basic flexbox" `Quick basic_flexbox ]
let suite = ("flexbox", tests)
