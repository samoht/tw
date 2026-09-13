let check = Test_helpers.check_handler_roundtrip (module Tw.Scroll.Handler)

let test_roundtrip () =
  check "scroll-m-0";
  check "scroll-m-4";
  check "scroll-mx-2";
  check "scroll-my-4";
  check "scroll-mt-8";
  check "scroll-mr-2";
  check "scroll-mb-4";
  check "scroll-ml-6";
  check "scroll-p-0";
  check "scroll-p-4";
  check "scroll-px-2";
  check "scroll-py-4";
  check "scroll-pt-8";
  check "scroll-pr-2";
  check "scroll-pb-4";
  check "scroll-pl-6"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll";
  Test_helpers.check_invalid_input (module Tw.Scroll.Handler) "scroll-foo"

(* Typed scroll-margin/padding constructors (newly exposed in tw.mli) take an
   [int]; check they agree with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "scroll-m-4" (Tw.scroll_m 4);
  Test_helpers.check_typed_class "scroll-mx-2" (Tw.scroll_mx 2);
  Test_helpers.check_typed_class "scroll-mt-8" (Tw.scroll_mt 8);
  Test_helpers.check_typed_class "scroll-me-6" (Tw.scroll_me 6);
  Test_helpers.check_typed_class "scroll-p-4" (Tw.scroll_p 4);
  Test_helpers.check_typed_class "scroll-py-2" (Tw.scroll_py 2);
  Test_helpers.check_typed_class "scroll-pt-8" (Tw.scroll_pt 8);
  Test_helpers.check_typed_class "scroll-ps-6" (Tw.scroll_ps 6)

(* The [']-suffixed sibling takes a half-step float instead of an int, same
   convention as [p]/[p']. *)
let test_typed_prime () =
  Test_helpers.check_typed_class "scroll-m-0.5" (Tw.scroll_m' 0.5);
  Test_helpers.check_typed_class "scroll-mx-2.5" (Tw.scroll_mx' 2.5);
  Test_helpers.check_typed_class "scroll-p-0.5" (Tw.scroll_p' 0.5);
  Test_helpers.check_typed_class "scroll-py-2.5" (Tw.scroll_py' 2.5)

(* An arbitrary scroll offset is any CSS length. A value the parser cannot read
   is not a utility: it used to be reinterpreted as a variable name, so
   [scroll-m-[2vh]] emitted [scroll-margin: var(--2vh)]. *)
let test_arbitrary_length () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "scroll-margin: 2vh" "scroll-m-[2vh]";
  emits "scroll-padding-top: 3ch" "scroll-pt-[3ch]";
  emits "scroll-margin: var(--gap)" "scroll-m-[var(--gap)]";
  match Tw.of_string "scroll-m-[bogus]" with
  | Ok _ -> Alcotest.fail "expected scroll-m-[bogus] to be rejected"
  | Error _ -> ()

(* Every scroll margin writes a property another one writes too, so their order
   decides which one wins. Tailwind sorts them by side - all, the two axes, the
   four logical sides, then the four physical ones - and puts the negative of a
   side before its positive. The sign was weighed ahead of the side, so
   [-scroll-ml-6] sorted before [scroll-mt-4]; the logical sides also trailed
   the physical ones. *)
let test_margin_order () =
  Test_helpers.check_class_order ~test_name:"scroll margins"
    [
      "-scroll-m-4";
      "scroll-m-4";
      "scroll-mx-4";
      "scroll-my-4";
      "scroll-ms-4";
      "scroll-me-4";
      "scroll-mbs-4";
      "scroll-mbe-4";
      "-scroll-mt-4";
      "-scroll-mt-8";
      "scroll-mt-4";
      "scroll-mr-4";
      "scroll-mb-4";
      "-scroll-ml-6";
      "scroll-ml-6";
    ]

(* Scroll padding takes no negative, and sorts by the same side order. *)
let test_padding_order () =
  Test_helpers.check_class_order ~test_name:"scroll paddings"
    [
      "scroll-p-4";
      "scroll-px-4";
      "scroll-py-4";
      "scroll-ps-4";
      "scroll-pe-4";
      "scroll-pbs-4";
      "scroll-pbe-4";
      "scroll-pt-4";
      "scroll-pr-4";
      "scroll-pb-4";
      "scroll-pl-4";
    ]

(* A data-type hint chooses the longhand and says nothing about the value. A
   scroll family writes one longhand per side, so every hint reaches it and the
   length reader sees only what follows the hint, which stays in the class
   name. *)
let test_data_type_hint_before_the_length_reader () =
  Test_helpers.check_declarations "scroll-m-[length:4px]"
    [ "scroll-margin:4px" ];
  Test_helpers.check_declarations "scroll-p-[foo:4px]" [ "scroll-padding:4px" ];
  Test_helpers.check_declarations "scroll-m-[length:var(--x)]"
    [ "scroll-margin:var(--x)" ];
  List.iter check
    [
      "scroll-m-[length:4px]";
      "scroll-p-[foo:4px]";
      "scroll-m-[length:var(--x)]";
    ];
  List.iter
    (Test_helpers.check_invalid_input (module Tw.Scroll.Handler))
    [ "scroll-m-[:4px]"; "scroll-m-[length:]" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "data-type hint before the length reader" `Quick
        test_data_type_hint_before_the_length_reader;
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "typed constructors: half-step" `Quick test_typed_prime;
      Alcotest.test_case "arbitrary length" `Quick test_arbitrary_length;
      Alcotest.test_case "scroll margin order" `Slow test_margin_order;
      Alcotest.test_case "scroll padding order" `Slow test_padding_order;
    ]

let suite = ("scroll", tests)
