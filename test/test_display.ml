open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Layout.Handler.of_string parts with
  | Ok result ->
      let style = Tw.Layout.Handler.to_style result in
      Alcotest.check string "display class name" expected (Tw.Style.pp style)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check [ "block" ];
  check [ "inline" ];
  check [ "inline"; "block" ];
  check [ "hidden" ]

let of_string_invalid () =
  let fail_maybe input =
    match Tw.Layout.Handler.of_string input with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "invalid" ];
  fail_maybe [ "inline"; "invalid" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  [ block; inline; inline_block; hidden ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"display suborder matches Tailwind" shuffled

let tests =
  [
    test_case "display of_string - valid values" `Quick of_string_valid;
    test_case "display of_string - invalid values" `Quick of_string_invalid;
    test_case "display suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("display", tests)
