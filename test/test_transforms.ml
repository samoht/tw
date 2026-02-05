open Alcotest

let check class_name =
  match Tw.Transforms.Handler.of_class class_name with
  | Ok t ->
      check string "transforms class" class_name
        (Tw.Transforms.Handler.to_class t)
  | Error (`Msg msg) -> fail msg

let test_translate_rotate () =
  check "translate-x-4";
  check "rotate-90"

let test_of_string_invalid () =
  (* Invalid transform utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match Tw.Transforms.Handler.of_class class_name with
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

let all_utilities () =
  let open Tw in
  [ translate_x 4; translate_y 2; rotate 90; scale 50 ]

let suborder_matches_tailwind () =
  let shuffled = Test_helpers.shuffle (all_utilities ()) in

  Test_helpers.check_ordering_matches
    ~test_name:"transforms suborder matches Tailwind" shuffled

let tests =
  [
    test_case "translate+rotate" `Quick test_translate_rotate;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "transforms suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("transforms", tests)
