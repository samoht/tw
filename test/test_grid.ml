open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Grid.Handler)

let of_string_valid () =
  (* Display *)
  check "grid";
  check "inline-grid"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Grid.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "grid"; "cols" ];
  (* Wrong utility - this is grid_template *)
  fail_maybe [ "col"; "auto" ];
  (* Wrong utility - this is grid_item *)
  fail_maybe [ "row"; "span"; "1" ]
(* Wrong utility - this is grid_item *)

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled = Test_helpers.shuffle [ grid; inline_grid ] in

  Test_helpers.check_ordering_matches
    ~test_name:"grid suborder matches Tailwind" shuffled

(* grid and inline-grid share one priority with every other display utility and
   are told apart by suborder alone, so which display an element ends up with is
   decided by the order the two sheets emit the band in. The band is the set the
   suborder table in Grid.Handler enumerates. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "grid";
      "inline-grid";
      "block";
      "inline-block";
      "inline";
      "flex";
      "inline-flex";
      "hidden";
      "contents";
      "flow-root";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"grid renders like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

let tests =
  [
    test_case "grid of_string - valid values" `Quick of_string_valid;
    test_case "grid of_string - invalid values" `Quick of_string_invalid;
    test_case "grid suborder matches Tailwind" `Quick suborder_matches_tailwind;
    test_case "grid renders like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("grid", tests)
