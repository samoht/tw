open Alcotest

let check_class expected t =
  Alcotest.check string "class" expected (Tw.Core.pp t)

let basic_grid () =
  check_class "grid-cols-3" Tw.Grid.grid_cols_3;
  check_class "grid-rows-2" Tw.Grid.grid_rows_2

let tests = [ test_case "basic grid" `Quick basic_grid ]
let suite = ("grid", tests)
