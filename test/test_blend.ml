open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Blend.Handler)

let of_string_valid () =
  (* Mix blend mode *)
  check "mix-blend-normal";
  check "mix-blend-multiply";
  check "mix-blend-screen";
  check "mix-blend-overlay";
  check "mix-blend-color-dodge";
  check "mix-blend-plus-lighter";
  (* Background blend mode *)
  check "bg-blend-normal";
  check "bg-blend-multiply";
  check "bg-blend-soft-light";
  check "bg-blend-luminosity"

(* Tailwind has no blend mode by either name, and background-blend-mode has no
   [plus-*] keyword at all. *)
let of_string_invalid () =
  Test_helpers.check_invalid_input (module Tw.Blend.Handler) "mix-blend-invalid";
  Test_helpers.check_invalid_input (module Tw.Blend.Handler) "bg-blend-invalid";
  Test_helpers.check_invalid_input
    (module Tw.Blend.Handler)
    "bg-blend-plus-lighter"

let test_declarations () =
  Test_helpers.check_declarations "mix-blend-multiply"
    [ "mix-blend-mode:multiply" ];
  Test_helpers.check_declarations "mix-blend-plus-darker"
    [ "mix-blend-mode:plus-darker" ];
  Test_helpers.check_declarations "bg-blend-color-burn"
    [ "background-blend-mode:color-burn" ]

let tests =
  [
    test_case "blend of_string - valid values" `Quick of_string_valid;
    test_case "blend of_string - invalid values" `Quick of_string_invalid;
    test_case "blend declarations" `Quick test_declarations;
  ]

let suite = ("blend", tests)
