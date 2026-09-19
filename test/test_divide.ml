let check = Test_helpers.check_handler_roundtrip (module Tw.Divide.Handler)

let test_roundtrip () =
  check "divide-x";
  check "divide-y";
  check "divide-x-2";
  check "divide-y-4";
  check "divide-x-reverse";
  check "divide-y-reverse";
  check "divide-solid";
  check "divide-dashed";
  check "divide-dotted";
  check "divide-double";
  check "divide-none";
  check "divide-transparent";
  check "divide-current";
  check "divide-inherit"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide";
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-foo"

(* divide-x-[2em] and divide-y-[3vw] used to be refused: the width reader only
   knew px and rem, so an em or a vw stop fell through to "not a divide utility"
   instead of reading as the length it is. *)
let test_arbitrary_width_units () =
  check "divide-x-[2em]";
  check "divide-y-[3vw]";
  (* [calc(2em*] said only that the unit reached a calc. The whole list says
     which sides carry the width, which carries the reverse factor, and that the
     style longhand travels with them. *)
  Test_helpers.check_declarations ~minify:false "divide-x-[2em]"
    [
      "--tw-divide-x-reverse: 0";
      "border-inline-style: var(--tw-border-style)";
      "border-inline-start-width: calc(2em * var(--tw-divide-x-reverse))";
      "border-inline-end-width: calc(2em * calc(1 - \
       var(--tw-divide-x-reverse)))";
    ];
  Test_helpers.check_declarations ~minify:false "divide-y-[3vw]"
    [
      "--tw-divide-y-reverse: 0";
      "border-bottom-style: var(--tw-border-style)";
      "border-top-style: var(--tw-border-style)";
      "border-top-width: calc(3vw * var(--tw-divide-y-reverse))";
      "border-bottom-width: calc(3vw * calc(1 - var(--tw-divide-y-reverse)))";
    ]

(* Every arbitrary width the reader accepts is spelled back exactly as it was
   written, so the selector matches the class in the markup. A width the reader
   accepts but [to_class] cannot spell collides with every other such width on
   one class name. *)
let test_arbitrary_width_roundtrip () =
  check "divide-x-[4px]";
  check "divide-y-[4px]";
  check "divide-x-[1rem]";
  check "divide-y-[0.5rem]";
  check "divide-x-[0.5rem]";
  (* The whole selector. An affix of it says the class name appears somewhere,
     which [.divide-x-\[1rem\]x] would satisfy too; what this test is about is
     that each width names its own rule. *)
  let selects cls sel =
    match Tw.of_string cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u ->
        Alcotest.(check bool)
          (cls ^ " selects the class the author wrote")
          true
          (List.mem sel (Test_helpers.selectors_of_utility u))
  in
  selects "divide-x-[1rem]" {|:where(.divide-x-\[1rem\] > :not(:last-child))|};
  (* Two rem widths are two class names, not one. *)
  selects "divide-x-[2rem]" {|:where(.divide-x-\[2rem\] > :not(:last-child))|}

(* The typed constructor spells the width itself, and builds exactly the classes
   the bracket reader accepts. Tailwind takes a line-width keyword there -
   [divide-x-[thin]] emits [calc(thin * var(--tw-divide-x-reverse))] - so
   refusing [Thin] refused a class the parser already read. A sizing keyword is
   not a width and stays refused. *)
let test_typed_arbitrary_width () =
  let open Tw in
  Test_helpers.check_typed_class "divide-x-[4px]" (divide_x_length (Css.Px 4.));
  Test_helpers.check_typed_class "divide-y-[1rem]"
    (divide_y_length (Css.Rem 1.));
  Test_helpers.check_typed_class "divide-x-[thin]" (divide_x_length Css.Thin);
  match divide_x_length Css.Auto with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected divide_x_length Auto to be refused"

(* Every unit a border width names has a bracket spelling, so a typed
   constructor handed one produces a class the parser reads back. *)
let test_typed_width_units () =
  let open Tw in
  let spelled (expected, width) =
    Test_helpers.check_typed_class
      ("divide-x-[" ^ expected ^ "]")
      (divide_x_length width)
  in
  List.iter spelled
    [
      ("4px", (Css.Px 4. : Css.border_width));
      ("1cm", Css.Cm 1.);
      ("2mm", Css.Mm 2.);
      ("3q", Css.Q 3.);
      ("1in", Css.In 1.);
      ("12pt", Css.Pt 12.);
      ("1pc", Css.Pc 1.);
      ("1.5rem", Css.Rem 1.5);
      ("2em", Css.Em 2.);
      ("1ex", Css.Ex 1.);
      ("1cap", Css.Cap 1.);
      ("1ic", Css.Ic 1.);
      ("1ric", Css.Ric 1.);
      ("1rlh", Css.Rlh 1.);
      ("2ch", Css.Ch 2.);
      ("1lh", Css.Lh 1.);
      ("3vh", Css.Vh 3.);
      ("3vw", Css.Vw 3.);
      ("3vmin", Css.Vmin 3.);
      ("3vmax", Css.Vmax 3.);
      ("50%", Css.Pct 50.);
      ("0px", Css.Zero);
    ]

(* Every divide utility the parser accepts also has a typed constructor, and the
   two agree on the class name (issue #5). *)
let test_typed () =
  let open Tw in
  Test_helpers.check_typed_class "divide-x-2" (divide_x 2);
  Test_helpers.check_typed_class "divide-y-4" (divide_y 4);
  Test_helpers.check_typed_class "divide-x-reverse" divide_x_reverse;
  Test_helpers.check_typed_class "divide-y-reverse" divide_y_reverse;
  Test_helpers.check_typed_class "divide-blue-500" (divide_color blue);
  Test_helpers.check_typed_class "divide-gray-300"
    (divide_color ~shade:300 gray);
  Test_helpers.check_typed_class "divide-transparent" divide_transparent;
  Test_helpers.check_typed_class "divide-current" divide_current;
  Test_helpers.check_typed_class "divide-inherit" divide_inherit;
  Test_helpers.check_typed_class "divide-dashed" (divide_style Dashed)

(* The widths lead the family and divide-x-reverse trails it, which is
   Tailwind's own order; the styles used to sort first, putting divide-dashed
   ahead of divide-y-4. *)
let divide_classes =
  [
    "divide-x";
    "divide-x-2";
    "divide-y";
    "divide-y-4";
    "divide-y-reverse";
    "divide-x-reverse";
    "divide-solid";
    "divide-dashed";
    "divide-current";
    "divide-gray-200";
    "border-2";
    "border-gray-500";
    "border-dashed";
  ]

let divide_utilities () =
  List.map (fun c -> Result.get_ok (Tw.of_string c)) divide_classes

let order_matches_tailwind () =
  Test_helpers.check_ordering_matches ~test_name:"divide order matches Tailwind"
    (Test_helpers.shuffle (divide_utilities ()))

(* divide's rules continue past the class name into a combinator, so where they
   land in the sheet is a shape check_ordering_matches cannot see: it pairs
   rules by key, and a layer holding each of them in the wrong place still
   compares equal. *)
let class_order_matches_tailwind () =
  Test_helpers.check_class_order
    ~test_name:"divide class order matches Tailwind"
    (List.filter
       (fun c -> Astring.String.is_prefix ~affix:"divide-" c)
       divide_classes)

(* [divide-x-reverse] sorts last in Tailwind's sheet, after every border and
   even after unrelated utilities, while [divide-y-reverse] stays with the
   divide family. tw gave the whole family one priority, so both came first. The
   rule only sets [--tw-divide-x-reverse], so the reorder is cascade-neutral and
   the canonical differ passes it; reading the emitted positions is what sees
   it. *)
let reverse_class_order_matches_tailwind () =
  Test_helpers.check_class_order
    ~test_name:"divide-x-reverse sorts after the border family"
    [
      "divide-x-reverse";
      "divide-y-reverse";
      "divide-x-2";
      "border-2";
      "border-gray-500";
      "rounded-lg";
      "p-4";
    ]

(* divide-* and border-* write the same border properties, so what an element is
   actually bordered with is a rendering question, not only an ordering one. *)
let rendering_matches_tailwind () =
  Test_helpers.check_rendering_matches ~test_name:"divide renders like Tailwind"
    (divide_utilities ())

(* A [#] bracket is only a divide colour when what follows is a hex spelling.
   The divide reader handed everything after the [#] to the raising constructor
   from inside [of_class], so a malformed hex escaped the parser as an exception
   instead of failing the match. *)
let test_arbitrary_bracket_color_token_stream () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter
    (fun cls -> ignore (css cls))
    [ "divide-[#zz]"; "divide-[#]"; "divide-[#12345]"; "divide-[#zz]/50" ];
  Test_helpers.check_declarations "divide-[#ff0000]" [ "border-color:#f00" ]

(* A bracket colour CSS names without spelling it as a function - a named
   colour, a keyword - is a divide colour too. The reader admitted only a [#]
   hex and a colour function, so [divide-[rebeccapurple]] was an unknown class,
   with or without an opacity modifier. *)
let test_bracket_named_color () =
  let emits decl cls = Test_helpers.check_declarations cls [ decl ] in
  emits "border-color:rebeccapurple" "divide-[rebeccapurple]";
  emits "border-color:currentColor" "divide-[currentColor]";
  (* the modifier mixes into the colour the bracket named, not into black *)
  Test_helpers.check_declarations ~minify:false "divide-[rebeccapurple]/50"
    [ "border-color: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
  (* Tailwind forwards a safe token stream even when the browser will not
     recognise it as a colour. *)
  emits "border-color:notacolour" "divide-[notacolour]"

(* A modifier reading a custom property mixes that property into the guarded
   value, on [currentcolor] and on a bracket [var()] however the bracket spells
   it. The [color:] hint sent the var to the raw arm, which folded the modifier
   to a percentage, which a var() has none of, so the mix said [100%] with no
   fallback beside it; the bare [var()] beside it took the typed arm and was
   right. *)
let test_colour_opacity_var () =
  let mixed cls fallback colour =
    Test_helpers.check_declarations ~minify:false cls
      [
        "border-color: " ^ fallback;
        "border-color: color-mix(in oklab, " ^ colour
        ^ " var(--o), transparent)";
      ]
  in
  mixed "divide-current/(--o)" "currentColor" "currentcolor";
  mixed "divide-current/[var(--o)]" "currentColor" "currentcolor";
  mixed "divide-[var(--c)]/(--o)" "var(--c)" "var(--c)";
  mixed "divide-[color:var(--c)]/(--o)" "var(--c)" "var(--c)";
  mixed "divide-[color:var(--c)]/[var(--o)]" "var(--c)" "var(--c)";
  Test_helpers.check_declarations ~minify:false "divide-[color:var(--c)]/50"
    [
      "border-color: var(--c)";
      "border-color: color-mix(in oklab, var(--c) 50%, transparent)";
    ]

(* The divide width suffix is a plain decimal integer. [divide-x-0x10] was read
   as 16 and emitted a rule selecting [.divide-x-16], a class the author never
   wrote; Tailwind emits nothing for it. *)
let test_non_decimal_widths () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "divide-x-0x10";
  rejected "divide-y-0x10";
  rejected "divide-x-1_0"

(* A token-stream colour under a modifier read from a custom property is the
   pair Tailwind's polyfill writes: the bare value in the open and the mix,
   reading the property the class named, behind the colour-mix guard. A named
   token reads the theme's percentage into an sRGB mix in the open. The raw arm
   folded either modifier to [100%] and wrote nothing beside it. *)
let test_raw_colour_opacity_var () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  let pair cls ~open_ ~alpha =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        "border-color: " ^ open_;
        "border-color: color-mix(in oklab, foo(1) " ^ alpha ^ ", transparent)";
      ]
  in
  pair "divide-[foo(1)]/(--o)" ~open_:"foo(1)" ~alpha:"var(--o)";
  pair "divide-[foo(1)]/[var(--o)]" ~open_:"foo(1)" ~alpha:"var(--o)";
  pair "divide-[foo(1)]/half"
    ~open_:"color-mix(in srgb, foo(1) 50%, transparent)"
    ~alpha:"var(--opacity-half)";
  (* A percentage needs no guard. *)
  Test_helpers.check_declarations ~minify:false "divide-[foo(1)]/50"
    [ "border-color: color-mix(in oklab, foo(1) 50%, transparent)" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "token-stream colour opacity from a var" `Quick
        test_raw_colour_opacity_var;
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "arbitrary width roundtrip" `Quick
        test_arbitrary_width_roundtrip;
      Alcotest.test_case "typed arbitrary width" `Quick
        test_typed_arbitrary_width;
      Alcotest.test_case "typed width units" `Quick test_typed_width_units;
      Alcotest.test_case "arbitrary width units" `Quick
        test_arbitrary_width_units;
      Alcotest.test_case "order matches Tailwind" `Slow order_matches_tailwind;
      Alcotest.test_case "reverse class order matches Tailwind" `Slow
        reverse_class_order_matches_tailwind;
      Alcotest.test_case "class order matches Tailwind" `Slow
        class_order_matches_tailwind;
      Alcotest.test_case "renders like Tailwind" `Slow
        rendering_matches_tailwind;
      Alcotest.test_case "arbitrary bracket color token stream" `Quick
        test_arbitrary_bracket_color_token_stream;
      Alcotest.test_case "non-decimal widths" `Quick test_non_decimal_widths;
      Alcotest.test_case "bracket named colour" `Quick test_bracket_named_color;
      Alcotest.test_case "colour opacity from a var" `Quick
        test_colour_opacity_var;
    ]

let suite = ("divide", tests)
