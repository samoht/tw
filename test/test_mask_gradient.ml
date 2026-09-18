let check =
  Test_helpers.check_handler_roundtrip (module Tw.Mask_gradient.Handler)

let test_roundtrip () =
  check "mask-t-from-0%";
  check "mask-t-to-100%";
  check "mask-b-from-50%";
  check "mask-b-to-50%";
  check "mask-l-from-0%";
  check "mask-r-to-100%";
  check "mask-x-from-0%";
  check "mask-y-to-100%";
  check "mask-radial";
  check "mask-linear-0";
  check "mask-linear-1";
  check "mask-linear-45";
  check "-mask-linear-1";
  check "mask-conic-0";
  check "mask-conic-1";
  check "mask-conic-45";
  check "mask-linear-[3rad]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Mask_gradient.Handler) "mask-foo"

(* A mask stop is a <length-percentage>: the zero spacing step keeps its unit
   (0px), not a bare 0, which is what Tailwind emits. *)
let test_from_zero_keeps_unit () =
  let css =
    match Tw.of_string "mask-t-from-0" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "mask-t-from-0: %s" m
  in
  Alcotest.(check bool)
    "from-position is 0px, not bare 0" true
    (Astring.String.is_infix ~affix:"--tw-mask-top-from-position:0px" css)

(* A stop takes a colour as well as a position. A palette entry points at its
   theme token and brings the token's declaration with it; the transparent and
   current keywords go in as themselves. *)
let test_stop_colors () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " sets " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "mask-r-from-black" "--tw-mask-right-from-color:var(--color-black)";
  has "mask-r-from-black" "--color-black:";
  has "mask-b-to-red-500" "--tw-mask-bottom-to-color:var(--color-red-500)";
  has "mask-radial-from-transparent" "--tw-mask-radial-from-color:transparent";
  has "mask-r-from-current" "--tw-mask-right-from-color:currentcolor";
  check "mask-r-from-black";
  check "mask-y-to-white";
  check "mask-conic-from-red-500";
  check "mask-r-to-transparent";
  check "mask-r-from-current"

let pretty cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:false
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

let has cls affix =
  Alcotest.(check bool)
    (cls ^ " sets " ^ affix)
    true
    (Astring.String.is_infix ~affix (pretty cls))

(* The mask-image slots hold gradients, and the utilities build them as
   gradients: a directional utility layers all four edges so the three it did
   not touch keep masking nothing, and the linear and conic families put the
   whole stop list in a variable of its own. *)
let test_gradient_composition () =
  has "mask-t-from-50%"
    "--tw-mask-linear: var(--tw-mask-left), var(--tw-mask-right), \
     var(--tw-mask-bottom), var(--tw-mask-top);";
  has "mask-t-from-50%"
    "--tw-mask-top: linear-gradient(to top, var(--tw-mask-top-from-color) \
     var(--tw-mask-top-from-position), var(--tw-mask-top-to-color) \
     var(--tw-mask-top-to-position));";
  has "mask-linear-from-50%"
    "--tw-mask-linear-stops: var(--tw-mask-linear-position), \
     var(--tw-mask-linear-from-color) var(--tw-mask-linear-from-position), \
     var(--tw-mask-linear-to-color) var(--tw-mask-linear-to-position);";
  has "mask-linear-from-50%"
    "--tw-mask-linear: linear-gradient(var(--tw-mask-linear-stops));";
  has "mask-conic-from-50%"
    "--tw-mask-conic-stops: from var(--tw-mask-conic-position), \
     var(--tw-mask-conic-from-color) var(--tw-mask-conic-from-position), \
     var(--tw-mask-conic-to-color) var(--tw-mask-conic-to-position);";
  has "mask-conic-from-50%"
    "--tw-mask-conic: conic-gradient(var(--tw-mask-conic-stops));";
  has "mask-linear-45"
    "--tw-mask-linear: linear-gradient(var(--tw-mask-linear-stops, \
     var(--tw-mask-linear-position)));"

(* A bracket stop is a <length-percentage>, so it reaches the sheet spelled the
   way CSSOM spells one. *)
let test_arbitrary_stop_position () =
  has "mask-t-from-[0.5rem]" "--tw-mask-top-from-position: .5rem;";
  has "mask-t-from-[50px]" "--tw-mask-top-from-position: 50px;";
  has "mask-t-from-[12%]" "--tw-mask-top-from-position: 12%;"

let invalid = Test_helpers.check_invalid_input (module Tw.Mask_gradient.Handler)

(* A bare stop is a spacing step or a whole percentage. Tailwind reads the step
   in quarters, so 2.25 is one where tw stopped at halves, and a percentage has
   to be a whole number spelled without a sign, bracketed or not: 12.5% is no
   stop at all. *)
let test_steps_and_percentages () =
  has "mask-linear-from-2.25"
    "--tw-mask-linear-from-position: calc(var(--spacing) * 2.25);";
  has "mask-linear-from-0.25"
    "--tw-mask-linear-from-position: calc(var(--spacing) * .25);";
  has "mask-linear-to-2.5"
    "--tw-mask-linear-to-position: calc(var(--spacing) * 2.5);";
  has "mask-t-from-0%" "--tw-mask-top-from-position: 0%;";
  has "mask-t-from-[0%]" "--tw-mask-top-from-position: 0%;";
  check "mask-linear-from-2.25";
  check "mask-linear-from-0.25";
  invalid "mask-linear-from-3.125";
  invalid "mask-linear-from-.5";
  invalid "mask-linear-from-2.0";
  invalid "mask-t-from-12.5%";
  invalid "mask-t-from-[12.5%]";
  invalid "mask-t-from-[percentage:12.5%]";
  invalid "mask-t-from-[05%]";
  invalid "mask-l-from-[-25%]";
  invalid "mask-t-from--0";
  invalid "mask-t-from--0%"

(* A bracket stop is an arbitrary value, so its [_] is a space and the binary
   operators of a math function take the spaces CSS needs around them: Tailwind
   writes [calc(1px + 2px)] for [calc(1px+2px)], and a browser drops the
   unspaced spelling. The value reached the sheet as the class wrote it. *)
let test_arbitrary_stop_decodes () =
  has "mask-linear-from-[calc(1px+2px)]"
    "--tw-mask-linear-from-position: calc(1px + 2px);";
  has "mask-linear-to-[calc(100%-10px)]"
    "--tw-mask-linear-to-position: calc(100% - 10px);";
  has "mask-radial-from-[calc(1px_+_2px)]"
    "--tw-mask-radial-from-position: calc(1px + 2px);";
  has "mask-linear-from-[10px_20px]"
    "--tw-mask-linear-from-position: 10px 20px;"

(* The angle variable takes degrees, so a radian or turn angle is converted;
   every other unit goes in as the author spelled it. *)
let test_arbitrary_angle () =
  has "mask-linear-[3rad]" "--tw-mask-linear-position: 171.887deg;";
  has "-mask-linear-[3rad]" "--tw-mask-linear-position: calc(171.887deg * -1);";
  has "mask-conic-[0.5turn]" "--tw-mask-conic-position: 180deg;";
  has "mask-linear-[30deg]" "--tw-mask-linear-position: 30deg;";
  has "mask-linear-[100grad]" "--tw-mask-linear-position: 100grad;";
  has "mask-linear-45" "--tw-mask-linear-position: calc(1deg * 45);";
  has "mask-linear-1" "--tw-mask-linear-position: 1deg;"

(* A radial size is an arbitrary value: [_] is a space and [\_] a literal
   underscore, so a variable name carrying one keeps the character. *)
let test_radial_size_underscore_escape () =
  has {|mask-radial-[var(--a\_b)]|} "--tw-mask-radial-size: var(--a_b);"

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "radial size underscore escape" `Quick
        test_radial_size_underscore_escape;
      Alcotest.test_case "from-0 keeps its px unit" `Quick
        test_from_zero_keeps_unit;
      Alcotest.test_case "colour stops" `Quick test_stop_colors;
      Alcotest.test_case "gradient composition" `Quick test_gradient_composition;
      Alcotest.test_case "arbitrary stop position" `Quick
        test_arbitrary_stop_position;
      Alcotest.test_case "steps and percentages" `Quick
        test_steps_and_percentages;
      Alcotest.test_case "arbitrary angle" `Quick test_arbitrary_angle;
      Alcotest.test_case "arbitrary stop decodes" `Quick
        test_arbitrary_stop_decodes;
    ]

let suite = ("mask_gradient", tests)
