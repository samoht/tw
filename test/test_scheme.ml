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

let test_breakpoint_override () =
  let s =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("breakpoint-10xl", "1600px") ]
  in
  Alcotest.(check (option (float 0.)))
    "breakpoint token populates the typed theme" (Some 1600.)
    (Tw.Scheme.breakpoint s "10xl")

let tests =
  Alcotest.
    [
      test_case "default scheme" `Quick test_default;
      test_case "find color" `Quick test_find_color;
      test_case "breakpoint override" `Quick test_breakpoint_override;
    ]

let suite = ("scheme", tests)
