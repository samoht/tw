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
  check "justify-items-normal";

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
    match Tw.Alignment.Handler.of_class Tw.Scheme.default class_name with
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

(* The four baseline spellings around justify-content. Tailwind emits
   [justify-content: baseline] for [justify-baseline] and nothing at all for the
   other three, so only the first is a gap on tw's side. Filing all four holds
   them to the pinned CLI through [check_negative_premises], which is what stops
   a later reading of "the baseline arm is missing" from adding the three that
   name no utility. *)
let baseline_family_matches_tailwind () =
  let invalid ?why input =
    Test_helpers.check_invalid_input ?why (module Tw.Alignment.Handler) input
  in
  invalid
    ~why:
      (Test_helpers.Diverges
         "cascade's justify_content carries no baseline value, so tw has no \
          typed way to write the declaration")
    "justify-baseline";
  invalid "justify-items-baseline";
  invalid "justify-self-baseline";
  invalid "place-self-baseline"

(* Tailwind offers the [-safe] suffix on exactly two positions, [center] and
   [end], and does so uniformly across all nine alignment families. No other
   position takes it, [start] included, even where CSS would allow [safe start]
   as an <overflow-position> <self-position> pair. The pinned CLI emits an empty
   utilities layer for every spelling below the divide, and
   [check_negative_premises] holds that claim to it on every run. *)
let safe_family_matches_tailwind () =
  let invalid =
    Test_helpers.check_invalid_input (module Tw.Alignment.Handler)
  in
  check "justify-center-safe";
  check "justify-end-safe";
  check "justify-items-center-safe";
  check "justify-items-end-safe";
  check "justify-self-center-safe";
  check "justify-self-end-safe";
  check "items-center-safe";
  check "items-end-safe";
  check "content-center-safe";
  check "content-end-safe";
  check "self-center-safe";
  check "self-end-safe";
  check "place-content-center-safe";
  check "place-content-end-safe";
  check "place-items-center-safe";
  check "place-items-end-safe";
  check "place-self-center-safe";
  check "place-self-end-safe";

  (* [start] takes no [-safe] in any family. *)
  invalid "justify-start-safe";
  invalid "justify-items-start-safe";
  invalid "justify-self-start-safe";
  invalid "items-start-safe";
  invalid "content-start-safe";
  invalid "self-start-safe";
  invalid "place-content-start-safe";
  invalid "place-items-start-safe";
  invalid "place-self-start-safe";

  (* Nor does any of the remaining positions. *)
  invalid "justify-between-safe";
  invalid "justify-items-normal-safe";
  invalid "items-baseline-safe";
  invalid "self-auto-safe";
  invalid "place-content-stretch-safe"

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
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
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"alignment suborder matches Tailwind" shuffled

(* The last align-content candidate still precedes the first align-items
   candidate at the shared property boundary. *)
let content_stretch_boundary_matches_tailwind () =
  Test_helpers.check_class_order ~test_name:"content stretch boundary"
    [ "items-baseline"; "content-stretch" ]

let tests =
  [
    test_case "alignment of_string - valid values" `Quick of_string_valid;
    test_case "alignment of_string - invalid values" `Quick of_string_invalid;
    test_case "alignment baseline family matches Tailwind" `Quick
      baseline_family_matches_tailwind;
    test_case "alignment safe family matches Tailwind" `Quick
      safe_family_matches_tailwind;
    test_case "alignment suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "content stretch boundary matches Tailwind" `Quick
      content_stretch_boundary_matches_tailwind;
  ]

let suite = ("alignment", tests)
