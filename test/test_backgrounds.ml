open Alcotest

let test_gradient_direction () =
  let u = Tw.Backgrounds.bg_gradient_to Tw.Backgrounds.Bottom in
  let t = Tw.Utility.to_style u in
  check string "bg-gradient-to-b" "bg-gradient-to-b" (Tw.Style.pp t)

let test_gradient_colors () =
  let open Tw in
  let from = Utility.to_style (Backgrounds.from_color Color.red) in
  let via = Utility.to_style (Backgrounds.via_color Color.blue ~shade:600) in
  let to_ = Utility.to_style (Backgrounds.to_color Color.green) in
  check string "from-red-500" "from-red-500" (Style.pp from);
  check string "via-blue-600" "via-blue-600" (Style.pp via);
  check string "to-green-500" "to-green-500" (Style.pp to_)

let test_of_string_invalid () =
  (* Invalid background utilities *)
  let test_invalid =
    Test_helpers.check_invalid_input (module Tw.Backgrounds.Handler)
  in

  (* Invalid gradient direction *)
  test_invalid [ "bg"; "gradient"; "to" ];
  (* Missing direction *)
  test_invalid [ "bg"; "gradient"; "to"; "invalid" ];
  (* Invalid direction *)
  test_invalid [ "bg"; "gradient"; "to"; "x" ];

  (* Invalid direction *)

  (* Invalid from/via/to colors *)
  test_invalid [ "from" ];
  (* Missing color *)
  test_invalid [ "from"; "invalid" ];
  (* Invalid color *)
  test_invalid [ "via" ];
  (* Missing color *)
  test_invalid [ "via"; "notacolor" ];
  (* Invalid color *)
  test_invalid [ "to" ];
  (* Missing color *)
  test_invalid [ "to"; "xyz" ];

  (* Invalid color *)

  (* Invalid prefixes *)
  test_invalid [ "bg" ];
  (* Incomplete *)
  test_invalid [ "bg"; "gradient" ];
  (* Incomplete *)
  test_invalid [ "unknown"; "red" ]
(* Unknown prefix *)

let tests =
  [
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "gradient colors" `Quick test_gradient_colors;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
  ]

let suite = ("backgrounds", tests)
