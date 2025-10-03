open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Grid_template.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Grid_template.Handler.to_style result in
      Alcotest.check string "grid template class name" expected
        (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  (* Grid template columns *)
  check [ "grid"; "cols"; "1" ];
  check [ "grid"; "cols"; "2" ];
  check [ "grid"; "cols"; "3" ];
  check [ "grid"; "cols"; "4" ];
  check [ "grid"; "cols"; "6" ];
  check [ "grid"; "cols"; "12" ];
  check [ "grid"; "cols"; "none" ];
  check [ "grid"; "cols"; "subgrid" ];

  (* Grid template rows *)
  check [ "grid"; "rows"; "1" ];
  check [ "grid"; "rows"; "2" ];
  check [ "grid"; "rows"; "3" ];
  check [ "grid"; "rows"; "4" ];
  check [ "grid"; "rows"; "6" ];
  check [ "grid"; "rows"; "12" ];
  check [ "grid"; "rows"; "none" ];
  check [ "grid"; "rows"; "subgrid" ];

  (* Grid auto flow *)
  check [ "grid"; "flow"; "row" ];
  check [ "grid"; "flow"; "col" ];
  check [ "grid"; "flow"; "dense" ];
  check [ "grid"; "flow"; "row"; "dense" ];
  check [ "grid"; "flow"; "col"; "dense" ];

  (* Grid auto columns *)
  check [ "auto"; "cols"; "auto" ];
  check [ "auto"; "cols"; "min" ];
  check [ "auto"; "cols"; "max" ];
  check [ "auto"; "cols"; "fr" ];

  (* Grid auto rows *)
  check [ "auto"; "rows"; "auto" ];
  check [ "auto"; "rows"; "min" ];
  check [ "auto"; "rows"; "max" ];
  check [ "auto"; "rows"; "fr" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Grid_template.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "grid" ];
  (* Missing subcommand *)
  fail_maybe [ "grid"; "cols" ];
  (* Missing value *)
  fail_maybe [ "grid"; "cols"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "grid"; "rows" ];
  (* Missing value *)
  fail_maybe [ "grid"; "rows"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "grid"; "flow" ];
  (* Missing flow direction *)
  fail_maybe [ "grid"; "flow"; "invalid" ];

  (* Invalid flow direction *)
  fail_maybe [ "auto" ];
  (* Missing subcommand *)
  fail_maybe [ "auto"; "cols" ];
  (* Missing value *)
  fail_maybe [ "auto"; "cols"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "auto"; "rows" ];
  (* Missing value *)
  fail_maybe [ "auto"; "rows"; "invalid" ]
(* Invalid value *)

let all_utilities () =
  let open Tw in
  List.init 12 (fun i -> grid_cols (i + 1))
  @ List.init 6 (fun i -> grid_rows (i + 1))

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"grid_template suborder matches Tailwind" shuffled

let tests =
  [
    test_case "grid_template of_string - valid values" `Quick of_string_valid;
    test_case "grid_template of_string - invalid values" `Quick
      of_string_invalid;
    test_case "grid_template suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("grid_template", tests)
