open Alcotest

let check_class expected t =
  Alcotest.check string "class" expected (Tw.Style.pp t)

let basic_tables () =
  check_class "border-collapse" (Tw.Utility.to_style Tw.Tables.border_collapse);
  check_class "border-separate" (Tw.Utility.to_style Tw.Tables.border_separate);
  check_class "border-spacing-2"
    (Tw.Utility.to_style (Tw.Tables.border_spacing 2));
  check_class "table-auto" (Tw.Utility.to_style Tw.Tables.table_auto);
  check_class "table-fixed" (Tw.Utility.to_style Tw.Tables.table_fixed)

let tests = [ test_case "basic tables" `Quick basic_tables ]
let suite = ("tables", tests)
