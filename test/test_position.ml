open Alcotest

let check class_name =
  match Tw.Position.Handler.of_class class_name with
  | Ok util ->
      check string "positioning class" class_name
        (Tw.Position.Handler.to_class util)
  | Error (`Msg msg) -> fail msg

let test_inset_and_z () =
  check "inset-0";
  check "z-10"

let test_negative () = check "-top-4"

let test_position_utilities () =
  check "static";
  check "fixed";
  check "absolute";
  check "relative";
  check "sticky"

let all_utilities () =
  let open Tw in
  [ static; fixed; absolute; relative; sticky; inset 0; top 4; left 2 ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"position suborder matches Tailwind" shuffled

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "negative top" `Quick test_negative;
    test_case "position utilities" `Quick test_position_utilities;
    test_case "position suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("position", tests)
