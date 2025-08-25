open Alcotest

let test_layout_of_string_valid () =
  (* Valid layout values *)
  let test_valid input expected =
    match Tw.Layout.of_string input with
    | Ok result -> check string "layout class name" expected (Tw.Core.pp result)
    | Error (`Msg msg) -> fail msg
  in

  (* Display utilities *)
  test_valid [ "block" ] "block";
  test_valid [ "inline"; "block" ] "inline-block";
  test_valid [ "inline" ] "inline";
  test_valid [ "flex" ] "flex";
  test_valid [ "inline"; "flex" ] "inline-flex";
  test_valid [ "table" ] "table";
  test_valid [ "inline"; "table" ] "inline-table";
  test_valid [ "table"; "caption" ] "table-caption";
  test_valid [ "table"; "cell" ] "table-cell";
  test_valid [ "table"; "column" ] "table-column";
  test_valid [ "table"; "row" ] "table-row";
  test_valid [ "grid" ] "grid";
  test_valid [ "inline"; "grid" ] "inline-grid";
  test_valid [ "contents" ] "contents";
  test_valid [ "list"; "item" ] "list-item";
  test_valid [ "hidden" ] "hidden";
  test_valid [ "flow"; "root" ] "flow-root";

  (* Position utilities *)
  test_valid [ "static" ] "static";
  test_valid [ "fixed" ] "fixed";
  test_valid [ "absolute" ] "absolute";
  test_valid [ "relative" ] "relative";
  test_valid [ "sticky" ] "sticky";

  (* Visibility *)
  test_valid [ "visible" ] "visible";
  test_valid [ "invisible" ] "invisible";
  test_valid [ "collapse" ] "collapse";

  (* Z-index *)
  test_valid [ "z"; "0" ] "z-0";
  test_valid [ "z"; "10" ] "z-10";
  test_valid [ "z"; "20" ] "z-20";
  test_valid [ "z"; "30" ] "z-30";
  test_valid [ "z"; "40" ] "z-40";
  test_valid [ "z"; "50" ] "z-50";
  test_valid [ "z"; "auto" ] "z-auto";

  (* Overflow *)
  test_valid [ "overflow"; "auto" ] "overflow-auto";
  test_valid [ "overflow"; "hidden" ] "overflow-hidden";
  test_valid [ "overflow"; "clip" ] "overflow-clip";
  test_valid [ "overflow"; "visible" ] "overflow-visible";
  test_valid [ "overflow"; "scroll" ] "overflow-scroll";
  test_valid [ "overflow"; "x"; "auto" ] "overflow-x-auto";
  test_valid [ "overflow"; "y"; "hidden" ] "overflow-y-hidden"

let test_layout_of_string_invalid () =
  (* Invalid layout values *)
  let test_invalid input =
    match Tw.Layout.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  test_invalid [ "inline"; "invalid" ];
  (* Invalid display *)
  test_invalid [ "table"; "invalid" ];
  (* Invalid table display *)
  test_invalid [ "z"; "60" ];
  (* Invalid z-index *)
  test_invalid [ "z"; "-10" ];
  (* Negative z-index *)
  test_invalid [ "overflow"; "invalid" ];
  (* Invalid overflow *)
  test_invalid [ "overflow"; "z"; "auto" ];
  (* Invalid axis *)
  test_invalid [ "unknown" ]
(* Unknown layout type *)

let tests =
  [
    test_case "layout of_string - valid values" `Quick
      test_layout_of_string_valid;
    test_case "layout of_string - invalid values" `Quick
      test_layout_of_string_invalid;
  ]
