open Alcotest

let check class_name =
  match Tw.Transforms.Handler.of_class Tw.Scheme.default class_name with
  | Ok t ->
      check string "transforms class" class_name
        (Tw.Transforms.Handler.to_class t)
  | Error (`Msg msg) -> fail msg

let test_translate_rotate () =
  check "translate-x-4";
  check "rotate-90"

(* translate-px (all axes) and the negative px / arbitrary-value variants used
   to be unknown classes: only the per-axis px and positive arbitrary forms
   parsed, and negatives only accepted [var(...)] brackets, not lengths like
   [110%]. *)
let test_translate_px_and_neg_arbitrary () =
  check "translate-px";
  check "-translate-px";
  check "-translate-x-px";
  check "-translate-y-px";
  check "-translate-y-[110%]";
  check "-translate-x-[3px]";
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "translate-px sets both axes to 1px" true
    (Astring.String.is_infix ~affix:"--tw-translate-x: 1px" (css "translate-px"));
  Alcotest.(check bool)
    "-translate-y-[110%] negates the value" true
    (Astring.String.is_infix ~affix:"calc(110% * -1)"
       (css "-translate-y-[110%]"))

let test_of_string_invalid () =
  (* Invalid transform utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Transforms.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  (* Invalid translate - missing value *)
  test_invalid [ "translate"; "x" ];
  test_invalid [ "translate"; "y" ];
  test_invalid [ "translate"; "z" ];

  (* Invalid rotate - missing value *)
  test_invalid [ "rotate" ];
  test_invalid [ "rotate"; "x" ];
  test_invalid [ "rotate"; "y" ];
  test_invalid [ "rotate"; "z" ];

  (* Invalid scale - missing value *)
  test_invalid [ "scale" ];
  test_invalid [ "scale"; "x" ];
  test_invalid [ "scale"; "y" ];
  test_invalid [ "scale"; "z" ];

  (* Invalid skew - missing value *)
  test_invalid [ "skew"; "x" ];
  test_invalid [ "skew"; "y" ];

  (* Invalid perspective *)
  test_invalid [ "perspective" ];
  test_invalid [ "perspective"; "123" ];
  test_invalid [ "perspective"; "potato" ];

  (* Invalid perspective origin *)
  test_invalid [ "perspective"; "origin" ];
  test_invalid [ "perspective"; "origin"; "invalid" ];

  (* Invalid transform style *)
  test_invalid [ "transform"; "style" ];
  test_invalid [ "transform"; "style"; "invalid" ];

  (* Invalid prefixes *)
  test_invalid [ "translate" ];
  (* Missing axis *)
  test_invalid [ "scale"; "invalid"; "100" ];
  (* Invalid axis *)
  test_invalid []
(* Empty *)

let suborder_matches_tailwind () =
  let open Tw in
  let shuffled =
    Test_helpers.shuffle [ translate_x 4; translate_y 2; rotate 90; scale 50 ]
  in

  Test_helpers.check_ordering_matches
    ~test_name:"transforms suborder matches Tailwind" shuffled

(* skew_x/skew_y (int) and the transform-origin constructors are newly exposed
   in tw.mli; check they agree with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "skew-x-3" (Tw.skew_x 3);
  Test_helpers.check_typed_class "skew-y-6" (Tw.skew_y 6);
  Test_helpers.check_typed_class "origin-center" Tw.origin_center;
  Test_helpers.check_typed_class "origin-top-right" Tw.origin_top_right;
  Test_helpers.check_typed_class "origin-bottom-left" Tw.origin_bottom_left

let tests =
  [
    test_case "translate+rotate" `Quick test_translate_rotate;
    test_case "translate-px and negative arbitrary" `Quick
      test_translate_px_and_neg_arbitrary;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "typed constructors" `Quick test_typed;
    test_case "transforms suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("transforms", tests)
