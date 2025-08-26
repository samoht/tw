open Alcotest
open Tw.Flow

let check_class expected t =
  Alcotest.check string "class" expected (Tw.Core.pp t)

let basic_grid () =
  check_class "grid-cols-3" (grid_cols 3);
  check_class "grid-rows-2" (grid_rows 2)

let basic_flexbox () =
  check_class "flex-row" flex_row;
  check_class "flex-col" flex_col;
  check_class "items-center" items_center;
  check_class "justify-between" justify_between

let tests =
  [
    test_case "basic grid" `Quick basic_grid;
    test_case "basic flexbox" `Quick basic_flexbox;
  ]

let suite = ("flow", tests)
