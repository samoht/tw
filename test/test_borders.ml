open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Borders.Handler)

let of_string_valid () =
  check "border";
  check "border-0";
  check "border-2";
  check "border-4";
  check "border-8";

  check "border-t";
  check "border-r";
  check "border-b";
  check "border-l";
  check "border-x";
  check "border-y";

  check "border-t-2";
  check "border-r-4";

  check "border-solid";
  check "border-dashed";
  check "border-dotted";
  check "border-double";
  check "border-none";

  check "rounded";
  check "rounded-none";
  check "rounded-sm";
  check "rounded-md";
  check "rounded-lg";
  check "rounded-xl";
  check "rounded-2xl";
  check "rounded-3xl";
  check "rounded-full";

  check "rounded-t";
  check "rounded-r";
  check "rounded-b";
  check "rounded-l";

  check "rounded-tl";
  check "rounded-tr";
  check "rounded-br";
  check "rounded-bl";

  check "rounded-t-lg";
  check "rounded-tl-2xl"

let of_string_invalid () =
  (* Invalid border values *)
  let fail_maybe =
    Test_helpers.check_invalid_parts (module Tw.Borders.Handler)
  in

  fail_maybe [ "border"; "3" ];
  (* Invalid width *)
  fail_maybe [ "border"; "invalid" ];
  (* Invalid style *)
  fail_maybe [ "border"; "z" ];
  (* Invalid side *)
  fail_maybe [ "rounded"; "4xl" ];
  (* Invalid size *)
  fail_maybe [ "rounded"; "z" ];
  (* Invalid corner *)
  fail_maybe [ "unknown" ]
(* Unknown border type *)

let all_utilities () =
  let open Tw in
  [
    border_xs;
    border_sm;
    border_md;
    border_lg;
    border_solid;
    border_dashed;
    border_dotted;
    border_double;
    border_none;
    rounded_none;
    rounded_sm;
    rounded;
    rounded_md;
    rounded_lg;
    rounded_xl;
    rounded_2xl;
    rounded_3xl;
    rounded_full;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"borders suborder matches Tailwind" shuffled

let tests =
  [
    test_case "borders of_string - valid values" `Quick of_string_valid;
    test_case "borders of_string - invalid values" `Quick of_string_invalid;
    test_case "borders suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("borders", tests)
