open Alcotest
open Tw.Style

let check_class expected t = Alcotest.check string "class" expected (pp t)

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Layout.of_string parts with
  | Ok result ->
      let style = Tw.Layout.to_style result in
      Alcotest.check string "layout class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let test_display_utilities () =
  check [ "block" ];
  check [ "inline"; "block" ];
  check [ "inline" ];
  check [ "hidden" ]

let test_visibility () =
  check [ "visible" ];
  check [ "invisible" ];
  check [ "collapse" ]

let test_z_index () =
  check [ "z"; "0" ];
  check [ "z"; "10" ];
  check [ "z"; "20" ];
  check [ "z"; "30" ];
  check [ "z"; "40" ];
  check [ "z"; "50" ];
  check [ "z"; "auto" ]

let test_overflow () =
  check [ "overflow"; "auto" ];
  check [ "overflow"; "hidden" ];
  check [ "overflow"; "clip" ];
  check [ "overflow"; "visible" ];
  check [ "overflow"; "scroll" ];
  check [ "overflow"; "x"; "auto" ];
  check [ "overflow"; "y"; "hidden" ]

let of_string_invalid () =
  (* Invalid layout values *)
  let fail_maybe input =
    match Tw.Layout.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "inline"; "invalid" ];
  (* Invalid display *)
  fail_maybe [ "table"; "invalid" ];
  (* Invalid table display *)
  fail_maybe [ "z"; "60" ];
  (* Invalid z-index *)
  fail_maybe [ "z"; "-10" ];
  (* Negative z-index *)
  fail_maybe [ "overflow"; "invalid" ];
  (* Invalid overflow *)
  fail_maybe [ "overflow"; "z"; "auto" ];
  (* Invalid axis *)
  fail_maybe [ "unknown" ]
(* Unknown layout type *)

let test_screen_reader () =
  check_class "sr-only" Tw.Layout.sr_only;
  check_class "not-sr-only" Tw.Layout.not_sr_only

let tests =
  [
    test_case "display utilities" `Quick test_display_utilities;
    test_case "visibility" `Quick test_visibility;
    test_case "z-index" `Quick test_z_index;
    test_case "overflow" `Quick test_overflow;
    test_case "screen reader utilities" `Quick test_screen_reader;
    test_case "layout of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("layout", tests)
