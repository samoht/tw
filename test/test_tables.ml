open Alcotest

let check_class expected t =
  Alcotest.check string "class" expected (Tw.Style.pp t)

let basic_tables () =
  check_class "border-collapse" Tw.Tables.border_collapse;
  check_class "border-separate" Tw.Tables.border_separate;
  check_class "border-spacing-2" (Tw.Tables.border_spacing 2);
  check_class "table-auto" Tw.Tables.table_auto;
  check_class "table-fixed" Tw.Tables.table_fixed

let tests = [ test_case "basic tables" `Quick basic_tables ]
let suite = ("tables", tests)
