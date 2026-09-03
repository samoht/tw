let check = Test_helpers.check_handler_roundtrip (module Tw.Tab.Handler)

let test_roundtrip () =
  check "tab-2";
  check "tab-8";
  check "tab-[3]";
  check "tab-[12px]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-2.5";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-unknown";
  (* A tab size is written in plain decimal: [tab-0x4] named itself [.tab-4]. *)
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-0x4";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-04";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-1_0"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("tab", tests)
