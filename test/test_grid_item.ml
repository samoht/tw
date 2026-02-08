open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Grid_item.Handler)

let of_string_valid () =
  (* Column placement *)
  check "col-auto";
  check "col-span-1";
  check "col-span-2";
  check "col-span-3";
  check "col-span-6";
  check "col-span-12";
  check "col-span-full";
  check "col-start-1";
  check "col-start-2";
  check "col-start-auto";
  check "col-end-1";
  check "col-end-7";
  check "col-end-auto";

  (* Row placement *)
  check "row-auto";
  check "row-span-1";
  check "row-span-2";
  check "row-span-3";
  check "row-span-6";
  check "row-span-12";
  check "row-span-full";
  check "row-start-1";
  check "row-start-2";
  check "row-start-auto";
  check "row-end-1";
  check "row-end-7";
  check "row-end-auto"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Grid_item.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "col" ];
  (* Missing value *)
  fail_maybe [ "col"; "span" ];
  (* Missing span value *)
  (* Note: Tailwind v4 accepts col-span-0 and values beyond 12 *)
  fail_maybe [ "col"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "col"; "start" ];
  (* Missing start value *)
  fail_maybe [ "col"; "end" ];

  (* Missing end value *)
  fail_maybe [ "row" ];
  (* Missing value *)
  fail_maybe [ "row"; "span" ];
  (* Missing span value *)
  (* Note: Tailwind v4 accepts row-span-0 and values beyond 12 *)
  fail_maybe [ "row"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "row"; "start" ];
  (* Missing start value *)
  fail_maybe [ "row"; "end" ]
(* Missing end value *)

let all_utilities () =
  let open Tw in
  [
    col_auto;
    col_span 1;
    col_span 2;
    col_span 6;
    col_span_full;
    col_start 1;
    col_start_auto;
    col_end 1;
    col_end_auto;
    row_auto;
    row_span 1;
    row_span 3;
    row_span_full;
    row_start 2;
    row_start_auto;
    row_end 7;
    row_end_auto;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"grid_item suborder matches Tailwind" shuffled

let tests =
  [
    test_case "grid_item of_string - valid values" `Quick of_string_valid;
    test_case "grid_item of_string - invalid values" `Quick of_string_invalid;
    test_case "grid_item suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("grid_item", tests)
