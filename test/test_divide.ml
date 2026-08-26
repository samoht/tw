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
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-foo";
  (* Units the width reader does not read are refused rather than accepted and
     spelled as something else. *)
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-x-[2em]";
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-y-[3vw]";
  Test_helpers.check_invalid_input (module Tw.Divide.Handler) "divide-x-[rem]"

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
  let selector cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "divide-x-[1rem] selects the class the author wrote" true
    (Astring.String.is_infix ~affix:".divide-x-\\[1rem\\]"
       (selector "divide-x-[1rem]"));
  (* Two rem widths are two class names, not one. *)
  Alcotest.(check bool)
    "divide-x-[2rem] is its own class" true
    (Astring.String.is_infix ~affix:".divide-x-\\[2rem\\]"
       (selector "divide-x-[2rem]"))

(* The typed constructor spells the width itself, and refuses a width that has
   no spelling inside a class name. *)
let test_typed_arbitrary_width () =
  let open Tw in
  Test_helpers.check_typed_class "divide-x-[4px]" (divide_x_length (Css.Px 4.));
  Test_helpers.check_typed_class "divide-y-[1rem]"
    (divide_y_length (Css.Rem 1.));
  match divide_x_length Css.Thin with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "expected divide_x_length Thin to be refused"

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

(* divide-* and border-* write the same border properties, so what an element is
   actually bordered with is a rendering question, not only an ordering one. *)
let rendering_matches_tailwind () =
  Test_helpers.check_rendering_matches ~test_name:"divide renders like Tailwind"
    (divide_utilities ())

(* A [#] bracket is only a divide colour when what follows is a hex spelling.
   The divide reader handed everything after the [#] to the raising constructor
   from inside [of_class], so a malformed hex escaped the parser as an exception
   instead of failing the match. *)
let test_invalid_bracket_hex () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "divide-[#zz]";
  rejected "divide-[#]";
  rejected "divide-[#12345]";
  rejected "divide-[#zz]/50";
  Alcotest.(check bool)
    "divide-[#ff0000] still emits the colour" true
    (Astring.String.is_infix ~affix:"border-color:#f00" (css "divide-[#ff0000]"))

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "arbitrary width roundtrip" `Quick
        test_arbitrary_width_roundtrip;
      Alcotest.test_case "typed arbitrary width" `Quick
        test_typed_arbitrary_width;
      Alcotest.test_case "order matches Tailwind" `Slow order_matches_tailwind;
      Alcotest.test_case "renders like Tailwind" `Slow
        rendering_matches_tailwind;
      Alcotest.test_case "invalid bracket hex" `Quick test_invalid_bracket_hex;
    ]

let suite = ("divide", tests)
