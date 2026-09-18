open Alcotest

let check_class = Test_helpers.check_class

let basic_svg () =
  check_class "fill-none" Tw.Svg.fill_none;
  check_class "stroke-2" Tw.Svg.stroke_2

(* Shadeless stroke colours (stroke-white/black) used to be swallowed by the
   stroke-width case and rejected; the width case now only matches integers, so
   they reach the colour parse. *)
let stroke_shadeless_colors () =
  (* The whole list, which is what "parses as a colour, not a width" means: the
     affix on [stroke:] left open whether a [stroke-width] was written beside
     it. *)
  Test_helpers.check_declarations "stroke-white" [ "stroke:var(--color-white)" ];
  Test_helpers.check_declarations "stroke-black" [ "stroke:var(--color-black)" ];
  (* Integer widths still parse as widths, not colours. The [px] is Tailwind's
     minifier reading its generator's bare [2], and it is the spelling the
     upstream corpus records and [--diff] compares. *)
  Test_helpers.check_declarations "stroke-2" [ "stroke-width:2px" ]

let stroke_light_dark_color () =
  let cls = "stroke-[light-dark(red,blue)]" in
  match Tw.of_string cls with
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  | Ok u ->
      Alcotest.(check string) "class" cls (Tw.pp u);
      Test_helpers.check_declarations cls [ "stroke:light-dark(red,blue)" ]

(* An arbitrary stroke width is read with the whole CSS length grammar, so a
   unit the reader does not name is not silently rendered as a zero width. *)
let stroke_arbitrary_width_units () =
  let emits cls value =
    Test_helpers.check_declarations cls [ "stroke-width:" ^ value ]
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

(* A bracket that is not a length is not a width, and it is not rendered as a
   zero width either: the colour is this family's last resort, so it reaches
   [stroke] verbatim, which is what the CLI writes. Each of these used to be
   refused, which dropped the selector. *)
let stroke_arbitrary_width_invalid () =
  let strokes cls value =
    Test_helpers.check_declarations cls [ "stroke:" ^ value ]
  in
  strokes "stroke-[1zz]" "1zz";
  strokes "stroke-[12px3]" "12px3";
  strokes "stroke-[.]" ".";
  strokes "stroke-[-]" "-"

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
  let emits decl cls = Test_helpers.check_declarations cls [ decl ] in
  emits "stroke:rebeccapurple" "stroke-[rebeccapurple]";
  emits "stroke:currentColor" "stroke-[currentColor]";
  (* the modifier mixes into the colour the bracket named, not into black *)
  Test_helpers.check_declarations ~minify:false "stroke-[rebeccapurple]/50"
    [ "stroke: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
  Test_helpers.check_declarations ~minify:false "fill-[rebeccapurple]/50"
    [ "fill: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
  (* a bracket naming neither a colour nor a width lands on the colour, which is
     this family's last resort *)
  Test_helpers.check_declarations "stroke-[notacolour]" [ "stroke:notacolour" ];
  Test_helpers.check_declarations "fill-[notacolour]" [ "fill:notacolour" ]

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
  (* The hint says the bracket is a width whatever the value turns out to be, so
     a value no length grammar reads is still a width, forwarded verbatim under
     the token-stream contract. The browser discards the declaration; what
     matters is that the rule, and so the selector, exists. *)
  Test_helpers.check_declarations "stroke-[length:notawidth]"
    [ "stroke-width:notawidth" ];
  Test_helpers.check_declarations "stroke-[number:red]" [ "stroke-width:red" ];
  Test_helpers.check_declarations "stroke-[percentage:red]"
    [ "stroke-width:red" ];
  Alcotest.(check string)
    "stroke-[length:notawidth] round-trips" "stroke-[length:notawidth]"
    (Tw.pp (Result.get_ok (Tw.of_string "stroke-[length:notawidth]")))

(* A modifier reading a custom property mixes that property into the guarded
   value on a bracket [var()] as on a palette colour. The bracket-var arms
   folded the modifier to a percentage, which a var() has none of, so the mix
   said [100%] and the modifier was dropped. *)
let bracket_var_opacity_var () =
  let mixed cls property =
    Test_helpers.check_declarations ~minify:false cls
      [
        property ^ ": var(--c)";
        property ^ ": color-mix(in oklab, var(--c) var(--o), transparent)";
      ]
  in
  mixed "fill-[var(--c)]/(--o)" "fill";
  mixed "fill-[color:var(--c)]/[var(--o)]" "fill";
  mixed "stroke-[var(--c)]/(--o)" "stroke";
  mixed "stroke-[color:var(--c)]/[var(--o)]" "stroke"

(* A named [--opacity-*] token is read off the theme before the bracket is read,
   or the modifier stays glued to the bracket: [fill-[var(--c)]/half] painted
   the text [[var(--c)]/half] and [fill-[#123456]/half] a declaration no reader
   takes, where Tailwind mixes [var(--opacity-half)] into both. *)
let bracket_named_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  let mixed cls property fallback colour =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        property ^ ": " ^ fallback;
        property ^ ": color-mix(in oklab, " ^ colour
        ^ " var(--opacity-half), transparent)";
      ]
  in
  mixed "fill-[var(--c)]/half" "fill" "var(--c)" "var(--c)";
  mixed "stroke-[var(--c)]/half" "stroke" "var(--c)" "var(--c)";
  (* a hex takes the percentage the token resolves to in its fallback, in the
     sRGB mix Tailwind writes before its minifier folds it to a hex *)
  mixed "fill-[#123456]/half" "fill"
    "color-mix(in srgb, #123456 50%, transparent)" "#123456";
  mixed "stroke-[#123456]/half" "stroke"
    "color-mix(in srgb, #123456 50%, transparent)" "#123456"

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
    test_case "bracket var opacity from a var" `Quick bracket_var_opacity_var;
    test_case "bracket named opacity" `Quick bracket_named_opacity;
  ]

let suite = ("svg", tests)
