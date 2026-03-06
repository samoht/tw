open Alcotest
open Tw.Arbitrary.Handler

let check input =
  match of_class input with
  | Ok result ->
      Alcotest.check string "arbitrary class name" input (to_class result)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check "[color:red]/50";
  check "[background:blue]/[50%]";
  check "[border-color:#ff0000]/25"

let of_string_invalid () =
  let fail_maybe input =
    match of_class input with
    | Ok _ -> fail ("Expected error for: " ^ input)
    | Error _ -> ()
  in
  fail_maybe "";
  fail_maybe "color:red";
  fail_maybe "[invalid]";
  fail_maybe "[]";
  (* No opacity = not handled by this handler *)
  fail_maybe "[color:red]"

let tests =
  [
    test_case "arbitrary of_string - valid values" `Quick of_string_valid;
    test_case "arbitrary of_string - invalid values" `Quick of_string_invalid;
  ]

let suite = ("arbitrary", tests)
