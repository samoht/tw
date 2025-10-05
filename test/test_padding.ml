open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Padding.Handler)

let of_string_valid () =
  check "p-0";
  check "p-1";
  check "p-4";
  check "p-px";
  check "p-0.5";
  check "p-1.5";

  check "px-4";
  check "px-0";
  check "px-8";
  check "py-2";
  check "py-6";

  check "pt-2";
  check "pr-4";
  check "pb-6";
  check "pl-8"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    check_invalid_input (module Tw.Padding.Handler) class_name
  in

  fail_maybe [ "p" ];
  (* Missing value *)
  fail_maybe [ "p"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "p"; "-1" ];
  (* Negative not allowed for padding *)
  fail_maybe [ "px"; "auto" ];
  (* Auto not valid for px *)
  fail_maybe [ "py"; "auto" ]
(* Auto not valid for py *)

let all_utilities () =
  let open Tw in
  List.concat_map
    (fun n -> [ p n; px n; py n; pt n; pb n; pl n; pr n ])
    Test_helpers.spacing_values

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"padding suborder matches Tailwind" shuffled

let tests =
  [
    test_case "padding of_string - valid values" `Quick of_string_valid;
    test_case "padding of_string - invalid values" `Quick of_string_invalid;
    test_case "padding suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("padding", tests)
