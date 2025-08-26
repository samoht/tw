open Alcotest
open Tw

let test_int_pos () =
  match Parse.int_pos ~name:"n" "3" with
  | Ok 3 -> ()
  | _ -> fail "int_pos expected Ok 3"

let test_int_pos_invalid () =
  match Parse.int_pos ~name:"n" "-1" with
  | Error _ -> ()
  | _ -> fail "int_pos expected Error for -1"

let tests =
  [
    test_case "int_pos ok" `Quick test_int_pos;
    test_case "int_pos invalid" `Quick test_int_pos_invalid;
  ]

let suite = ("parse", tests)
