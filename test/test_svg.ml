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

(* A stroke width is written in plain decimal. Read as an OCaml literal,
   [stroke-0x4] parsed and then named itself [.stroke-4]: a rule the author
   never wrote, matching nothing in the markup. *)
let stroke_width_rejects_ocaml_literals () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "expected %s to be rejected, got %s" cls
            (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
      | Error _ -> ())
    [ "stroke-0x4"; "stroke-04"; "stroke-1_0" ]

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
  emits "stroke:rebeccapurple" "stroke-[rebeccapurple]";
  emits "stroke:currentColor" "stroke-[currentColor]";
  (* the modifier mixes into the colour the bracket named, not into black *)
  Test_helpers.check_declarations ~minify:false "stroke-[rebeccapurple]/50"
    [ "stroke: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
  Test_helpers.check_declarations ~minify:false "fill-[rebeccapurple]/50"
    [ "fill: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
  (* a bracket naming neither a colour nor a width is still not a class *)
  Alcotest.(check bool)
    "stroke-[notacolour] is not a class" true
    (Result.is_error (Tw.of_string "stroke-[notacolour]"))

(* fill and stroke share a priority with object-fit and object-position, and
   Tailwind emits them first of the two. They sorted last instead, the whole svg
   family landing after the object utilities, and no canonical comparison could
   see it: the two write disjoint properties, so nothing about the pair is
   cascade-significant and the differ folds the reorder away. Reading the
   positions back out of the sheet is what catches it. *)
let svg_sorts_before_object () =
  Test_helpers.check_class_order ~test_name:"fill and stroke before object"
    [
      "bg-cover";
      "mask-cover";
      "fill-blue-200";
      "fill-none";
      "stroke-current";
      "stroke-2";
      "object-cover";
      "object-center";
      "p-4";
    ]

(* A data-type hint says how to read the value written after it; it does not
   make that value the name of a custom property. [stroke-[length:2px]] wrote
   [stroke-width: var(--2px)] where Tailwind writes [stroke-width: 2px]. *)
let bracket_data_type_hint_reads_the_value () =
  Test_helpers.check_declarations "stroke-[length:2px]" [ "stroke-width:2px" ];
  Test_helpers.check_declarations "stroke-[percentage:50%]"
    [ "stroke-width:50%" ];
  Test_helpers.check_declarations "stroke-[color:red]" [ "stroke:red" ];
  Test_helpers.check_declarations "fill-[color:red]" [ "fill:red" ];
  (* a var() reference after the hint still names a custom property *)
  Test_helpers.check_declarations "stroke-[length:var(--my-width)]"
    [ "stroke-width:var(--my-width)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "stroke-[length:2px] round-trips" "stroke-[length:2px]"
    (Tw.pp (Result.get_ok (Tw.of_string "stroke-[length:2px]")));
  (* A value the width reader refuses is held open, not settled: Tailwind writes
     the bracket out whatever it says, so refusing is an intermediate. *)
  Test_helpers.check_invalid_input
    ~why:
      (Test_helpers.Diverges
         "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Svg.Handler)
    "stroke-[length:notawidth]"

let tests =
  [
    test_case "bracket data-type hint reads the value" `Quick
      bracket_data_type_hint_reads_the_value;
    test_case "basic svg" `Quick basic_svg;
    test_case "svg sorts before object" `Quick svg_sorts_before_object;
    test_case "bracket named colour" `Quick bracket_named_color;
    test_case "stroke shadeless colors" `Quick stroke_shadeless_colors;
    test_case "stroke light-dark color" `Quick stroke_light_dark_color;
    test_case "stroke arbitrary width units" `Quick stroke_arbitrary_width_units;
    test_case "stroke arbitrary width invalid" `Quick
      stroke_arbitrary_width_invalid;
    test_case "stroke width rejects OCaml literals" `Quick
      stroke_width_rejects_ocaml_literals;
  ]

let suite = ("svg", tests)
