open Alcotest

let test_gradient_direction () =
  let t = Tw.Backgrounds.bg_gradient_to Tw.Backgrounds.Bottom in
  check string "bg-gradient-to-b" "bg-gradient-to-b" (Tw.Style.pp t)

let test_gradient_colors () =
  let open Tw in
  let from = Backgrounds.from_color Color.red in
  let via = Backgrounds.via_color Color.blue ~shade:600 in
  let to_ = Backgrounds.to_color Color.green in
  check string "from-red-500" "from-red-500" (Style.pp from);
  check string "via-blue-600" "via-blue-600" (Style.pp via);
  check string "to-green-500" "to-green-500" (Style.pp to_)

let tests =
  [
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "gradient colors" `Quick test_gradient_colors;
  ]

let suite = ("backgrounds", tests)
