open Alcotest

let check_class = Test_helpers.check_class

let basic_tables () =
  check_class "border-collapse" Tw.Private.Tables.border_collapse;
  check_class "border-separate" Tw.Private.Tables.border_separate;
  check_class "border-spacing-2" (Tw.Private.Tables.border_spacing 2.);
  check_class "table-auto" Tw.Private.Tables.table_auto;
  check_class "table-fixed" Tw.Private.Tables.table_fixed

let tests = [ test_case "basic tables" `Quick basic_tables ]
let suite = ("tables", tests)
