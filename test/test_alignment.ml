open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Alignment.Handler)

let of_string_valid () =
  (* Justify content *)
  check "justify-start";
  check "justify-end";
  check "justify-center";
  check "justify-between";
  check "justify-around";
  check "justify-evenly";

  (* Align items *)
  check "items-start";
  check "items-end";
  check "items-center";
  check "items-baseline";
  check "items-stretch";

  (* Align content *)
  check "content-start";
  check "content-end";
  check "content-center";
  check "content-between";
  check "content-around";
  check "content-evenly";
  check "content-stretch";

  (* Align self *)
  check "self-auto";
  check "self-start";
  check "self-end";
  check "self-center";
  check "self-baseline";
  check "self-stretch";

  (* Justify items *)
  check "justify-items-start";
  check "justify-items-end";
  check "justify-items-center";
  check "justify-items-stretch";

  (* Justify self *)
  check "justify-self-auto";
  check "justify-self-start";
  check "justify-self-end";
  check "justify-self-center";
  check "justify-self-stretch";

  (* Place content *)
  check "place-content-start";
  check "place-content-end";
  check "place-content-center";
  check "place-content-between";
  check "place-content-around";
  check "place-content-evenly";
  check "place-content-stretch";

  (* Place items *)
  check "place-items-start";
  check "place-items-end";
  check "place-items-center";
  check "place-items-stretch";

  (* Place self *)
  check "place-self-auto";
  check "place-self-start";
  check "place-self-end";
  check "place-self-center";
  check "place-self-stretch"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Alignment.Handler.of_class class_name with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
    | Error _ -> ()
  in

  fail_maybe [ "justify" ];
  fail_maybe [ "justify"; "invalid" ];
  fail_maybe [ "items" ];
  fail_maybe [ "content" ];
  fail_maybe [ "self" ];
  fail_maybe [ "place" ];
  fail_maybe [ "place"; "content" ];
  fail_maybe []

let all_utilities () =
  let open Tw in
  [
    justify_start;
    justify_end;
    justify_center;
    justify_between;
    items_start;
    items_end;
    items_center;
    items_baseline;
    content_start;
    content_end;
    content_center;
    self_auto;
    self_start;
    self_end;
    justify_items_start;
    justify_self_auto;
    place_content_start;
    place_items_start;
    place_self_auto;
  ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"alignment suborder matches Tailwind" shuffled

let tests =
  [
    test_case "alignment of_string - valid values" `Quick of_string_valid;
    test_case "alignment of_string - invalid values" `Quick of_string_invalid;
    test_case "alignment suborder matches Tailwind" `Slow
      suborder_matches_tailwind;
  ]

let suite = ("alignment", tests)
