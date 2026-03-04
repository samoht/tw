let test_default () =
  let s = Tw.Scheme.default in
  Alcotest.(check int) "default ring width" 1 s.default_ring_width;
  Alcotest.(check int) "default border width" 1 s.default_border_width;
  Alcotest.(check int) "default outline width" 1 s.default_outline_width

let test_find_color () =
  let s : Tw.Scheme.t =
    { Tw.Scheme.default with colors = [ ("red-500", Hex "#ef4444") ] }
  in
  Alcotest.(check bool)
    "finds defined color" true
    (Tw.Scheme.hex_color s "red-500" <> None);
  Alcotest.(check bool)
    "missing color returns none" true
    (Tw.Scheme.hex_color s "blue-500" = None)

let tests =
  Alcotest.
    [
      test_case "default scheme" `Quick test_default;
      test_case "find color" `Quick test_find_color;
    ]

let suite = ("scheme", tests)
