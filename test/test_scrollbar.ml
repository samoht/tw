let check = Test_helpers.check_handler_roundtrip (module Tw.Scrollbar.Handler)

let test_roundtrip () =
  (* scrollbar-width *)
  check "scrollbar-auto";
  check "scrollbar-none";
  check "scrollbar-thin";
  (* scrollbar-gutter *)
  check "scrollbar-gutter-auto";
  check "scrollbar-gutter-stable";
  check "scrollbar-gutter-both";
  (* scrollbar-thumb / -track colours *)
  check "scrollbar-thumb-red-500";
  check "scrollbar-track-gray-200";
  check "scrollbar-thumb-current";
  check "scrollbar-thumb-transparent";
  check "scrollbar-thumb-inherit";
  check "scrollbar-thumb-[#ff0000]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Scrollbar.Handler) "scrollbar";
  Test_helpers.check_invalid_input (module Tw.Scrollbar.Handler) "scrollbar-foo";
  Test_helpers.check_invalid_input
    (module Tw.Scrollbar.Handler)
    "scrollbar-gutter-foo"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid

let suite = ("scrollbar", tests)
