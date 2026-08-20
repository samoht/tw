open Alcotest

let check_class = Test_helpers.check_class

let basic_svg () =
  check_class "fill-none" Tw.Svg.fill_none;
  check_class "stroke-2" Tw.Svg.stroke_2

(* Shadeless stroke colours (stroke-white/black) used to be swallowed by the
   stroke-width case and rejected; the width case now only matches integers, so
   they reach the colour parse. *)
let stroke_shadeless_colors () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "stroke-white references --color-white" true
    (Astring.String.is_infix ~affix:"stroke:var(--color-white)"
       (css "stroke-white"));
  Alcotest.(check bool)
    "stroke-black references --color-black" true
    (Astring.String.is_infix ~affix:"stroke:var(--color-black)"
       (css "stroke-black"));
  (* integer widths still parse as widths, not colours *)
  Alcotest.(check bool)
    "stroke-2 stays a width" true
    (Astring.String.is_infix ~affix:"stroke-width:2px" (css "stroke-2"))

let stroke_light_dark_color () =
  let cls = "stroke-[light-dark(red,blue)]" in
  match Tw.of_string cls with
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  | Ok u ->
      Alcotest.(check string) "class" cls (Tw.pp u);
      let css =
        Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
      in
      Alcotest.(check bool)
        "light-dark() is routed as a stroke colour" true
        (Astring.String.is_infix
           ~affix:"stroke:light-dark(red,blue)" css)

let tests =
  [
    test_case "basic svg" `Quick basic_svg;
    test_case "stroke shadeless colors" `Quick stroke_shadeless_colors;
    test_case "stroke light-dark color" `Quick stroke_light_dark_color;
  ]

let suite = ("svg", tests)
