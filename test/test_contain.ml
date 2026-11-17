let check = Test_helpers.check_handler_roundtrip (module Tw.Contain.Handler)

let test_roundtrip () =
  check "contain-none";
  check "contain-strict";
  check "contain-content";
  check "contain-size";
  check "contain-inline-size";
  check "contain-layout";
  check "contain-paint";
  check "contain-style"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Contain.Handler) "contain";
  Test_helpers.check_invalid_input (module Tw.Contain.Handler) "contain-foo"

(* A data-type hint chooses the longhand and is no part of the value, so a hint
   the family does not know still leaves only what follows it in the sheet. *)
let test_unknown_hint_is_not_the_value () =
  List.iter Test_helpers.check_class_matches_tailwind
    [ "contain-[foo:layout]"; "contain-[foo:var(--x)]" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "unknown hint is not the value" `Quick
        test_unknown_hint_is_not_the_value;
    ]

let suite = ("contain", tests)
