open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Grid.Handler)

let of_string_valid () =
  (* Display *)
  check "grid";
  check "inline-grid"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Grid.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "grid"; "cols" ];
  (* Wrong utility - this is grid_template *)
  fail_maybe [ "col"; "auto" ];
  (* Wrong utility - this is grid_item *)
  fail_maybe [ "row"; "span"; "1" ]
(* Wrong utility - this is grid_item *)

let all_utilities () =
  let open Tw in
  [ grid; inline_grid ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"grid suborder matches Tailwind" shuffled

let tests =
  [
    test_case "grid of_string - valid values" `Quick of_string_valid;
    test_case "grid of_string - invalid values" `Quick of_string_invalid;
    test_case "grid suborder matches Tailwind" `Quick suborder_matches_tailwind;
  ]

let suite = ("grid", tests)
