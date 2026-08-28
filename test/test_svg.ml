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
      let css = Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true in
      Alcotest.(check bool)
        "light-dark() is routed as a stroke colour" true
        (Astring.String.is_infix ~affix:"stroke:light-dark(red,blue)" css)

(* An arbitrary stroke width is read with the whole CSS length grammar, so a
   unit the reader does not name is not silently rendered as a zero width. *)
let stroke_arbitrary_width_units () =
  let width cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls value =
    Alcotest.(check bool)
      (cls ^ " emits " ^ value)
      true
      (Astring.String.is_infix ~affix:("stroke-width:" ^ value) (width cls))
  in
  emits "stroke-[1.5rem]" "1.5rem";
  emits "stroke-[2em]" "2em";
  emits "stroke-[3pt]" "3pt";
  emits "stroke-[1.5vw]" "1.5vw";
  emits "stroke-[calc(1rem_+_2px)]" "calc(1rem + 2px)";
  (* the units the reader already named keep their value *)
  emits "stroke-[12px]" "12px";
  emits "stroke-[50%]" "50%";
  emits "stroke-[1.5]" "1.5px";
  (* the class name is spelled as it was written *)
  Alcotest.(check string)
    "stroke-[1.5rem] round-trips" "stroke-[1.5rem]"
    (Tw.pp (Result.get_ok (Tw.of_string "stroke-[1.5rem]")))

(* A bracket that is not a length is refused, rather than accepted and rendered
   as a zero width. *)
let stroke_arbitrary_width_invalid () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u ->
        Alcotest.failf "expected %s to be rejected, got %s" cls
          (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
    | Error _ -> ()
  in
  rejected "stroke-[1zz]";
  rejected "stroke-[12px3]";
  rejected "stroke-[.]";
  rejected "stroke-[-]"

(* A bracket colour CSS names without spelling it as a function - a named
   colour, a keyword - is a stroke colour too. The stroke reader told colours
   from widths by looking for a [#] or a colour function, so
   [stroke-[rebeccapurple]] fell through to the width reader and was refused. An
   opacity modifier then folds into the colour the bracket named: fill and
   stroke read the bracket text back as a hex and answered black for every
   colour with no hex spelling. *)
let bracket_named_color () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  (* the value a class sets for [prop], so two spellings of one colour compare
     without their class names *)
  let value prop cls =
    let sheet = css cls in
    let key = prop ^ ":" in
    match Astring.String.find_sub ~sub:key sheet with
    | None -> Alcotest.failf "%s sets no %s: %s" cls prop sheet
    | Some i ->
        let first = i + String.length key in
        Astring.String.with_range ~first sheet
        |> Astring.String.take ~sat:(fun c -> c <> ';' && c <> '}')
  in
  let same prop cls other =
    Alcotest.(check string)
      (cls ^ " is " ^ other)
      (value prop other) (value prop cls)
  in
  emits "stroke:rebeccapurple" "stroke-[rebeccapurple]";
  emits "stroke:currentColor" "stroke-[currentColor]";
  same "stroke" "stroke-[rebeccapurple]/50" "stroke-[#663399]/50";
  same "fill" "fill-[rebeccapurple]/50" "fill-[#663399]/50";
  (* a bracket naming neither a colour nor a width is still not a class *)
  Alcotest.(check bool)
    "stroke-[notacolour] is not a class" true
    (Result.is_error (Tw.of_string "stroke-[notacolour]"))

let tests =
  [
    test_case "basic svg" `Quick basic_svg;
    test_case "bracket named colour" `Quick bracket_named_color;
    test_case "stroke shadeless colors" `Quick stroke_shadeless_colors;
    test_case "stroke light-dark color" `Quick stroke_light_dark_color;
    test_case "stroke arbitrary width units" `Quick stroke_arbitrary_width_units;
    test_case "stroke arbitrary width invalid" `Quick
      stroke_arbitrary_width_invalid;
  ]

let suite = ("svg", tests)
