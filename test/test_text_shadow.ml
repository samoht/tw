let check = Test_helpers.check_handler_roundtrip (module Tw.Text_shadow.Handler)

let test_roundtrip () =
  check "text-shadow-none";
  check "text-shadow-2xs";
  check "text-shadow-xs";
  check "text-shadow-sm";
  check "text-shadow-md";
  check "text-shadow-lg"

let test_invalid () =
  Test_helpers.check_invalid_input
    (module Tw.Text_shadow.Handler)
    "text-shadow-foo";
  (* Bare `text-shadow` is not a v4 utility (the CLI emits nothing); only the
     named scale `text-shadow-{2xs,xs,sm,md,lg}` is valid. *)
  Test_helpers.check_invalid_input (module Tw.Text_shadow.Handler) "text-shadow"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("text_shadow", tests)
