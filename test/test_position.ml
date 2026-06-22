open Alcotest

let check class_name =
  match Tw.Position.Handler.of_class Tw.Scheme.default class_name with
  | Ok util ->
      check string "positioning class" class_name
        (Tw.Position.Handler.to_class util)
  | Error (`Msg msg) -> fail msg

let test_inset_and_z () = check "inset-0"
let test_negative () = check "-top-4"

let test_position_utilities () =
  check "static";
  check "fixed";
  check "absolute";
  check "relative";
  check "sticky"

(* Arbitrary values round-trip verbatim in the class name: the leading zero of
   0.67rem (and the sign of negatives) is preserved, not re-serialised to a
   normalised .67rem that would no longer match the HTML class. *)
let test_arbitrary_roundtrip () =
  check "top-[0.67rem]";
  check "right-[-0.9rem]";
  check "bottom-[5rem]";
  check "left-[0.5rem]";
  check "inset-[0.25rem]";
  check "inset-x-[0.5rem]"

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle
      [ static; fixed; absolute; relative; sticky; inset 0; top 4; left 2 ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"position suborder matches Tailwind" shuffled

let tests =
  [
    test_case "inset and z" `Quick test_inset_and_z;
    test_case "negative top" `Quick test_negative;
    test_case "arbitrary value roundtrip" `Quick test_arbitrary_roundtrip;
    test_case "position utilities" `Quick test_position_utilities;
    test_case "position suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("position", tests)
