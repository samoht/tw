module Css = Cascade.Css
open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Padding.Handler)

let of_string_valid () =
  check "p-0";
  check "p-1";
  check "p-4";
  check "p-px";
  check "p-0.5";
  check "p-1.5";

  check "px-4";
  check "px-0";
  check "px-8";
  check "py-2";
  check "py-6";

  check "pt-2";
  check "pr-4";
  check "pb-6";
  check "pl-8"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    check_invalid_input (module Tw.Padding.Handler) class_name
  in

  fail_maybe [ "p" ];
  (* Missing value *)
  fail_maybe [ "p"; "!!!" ];
  (* Invalid value - non-alphanumeric *)
  fail_maybe [ "p"; "-1" ];
  (* Negative not allowed for padding *)
  fail_maybe [ "px"; "auto" ];
  (* Auto not valid for px *)
  fail_maybe [ "py"; "auto" ]
(* Auto not valid for py *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    List.concat_map
      (fun n -> [ p n; px n; py n; pt n; pb n; pl n; pr n ])
      Test_helpers.spacing_values
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"padding suborder matches Tailwind" shuffled

let candidate_order () =
  Test_helpers.check_class_order ~test_name:"padding candidate order"
    [ "py-2"; "py-1.25"; "py-1.5"; "py-1" ]

(* Each step multiplies the spacing scale by its own number. The substring
   [*64)] this used to look for is in [calc(var(--spacing)*64)] and in a dozen
   other values, so it said little about what the class writes; the whole
   declaration is what the CLI is held to. *)
let test_css_values () =
  Test_helpers.check_declarations "p-64" [ "padding:calc(var(--spacing)*64)" ];
  Test_helpers.check_declarations "p-4" [ "padding:calc(var(--spacing)*4)" ];
  Test_helpers.check_declarations "px-10"
    [ "padding-inline:calc(var(--spacing)*10)" ]

(* Arbitrary paddings accept the full length grammar (percent, calc), not just
   px/rem, and round-trip verbatim. *)
let test_arbitrary_length_grammar () =
  Test_helpers.check_declarations "p-[calc(var(--spacing-6)-1px)]"
    [ "padding:calc(var(--spacing-6) - 1px)" ];
  Test_helpers.check_declarations "pl-[calc(100%-21.5rem)]"
    [ "padding-left:calc(100% - 21.5rem)" ];
  Test_helpers.check_declarations "px-[50%]" [ "padding-inline:50%" ];
  let check c =
    match Tw.Padding.Handler.of_class Tw.Scheme.default c with
    | Ok u ->
        Alcotest.check string "roundtrip" c (Tw.Padding.Handler.to_class u)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" c m
  in
  check "px-[50%]";
  check "p-[calc(var(--spacing-6)-1px)]"

(* Tailwind's [--spacing(N)] shorthand can appear inside an arbitrary value; it
   is not CSS, so the whole utility used to drop out. Expanding it also has to
   pull [--spacing] into the theme layer, which only colour tokens reached. *)
let test_arbitrary_spacing_fn () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Test_helpers.check_declarations "py-[calc(--spacing(2)+1px)]"
    [ "padding-block:calc(calc(var(--spacing)*2) + 1px)" ];
  (* The theme binding is a [:root] declaration, which [declarations_of_class]
     leaves out by design, so this one stays a substring. *)
  Alcotest.(check bool)
    "declares --spacing" true
    (Astring.String.is_infix ~affix:"--spacing:.25rem"
       (css "py-[calc(--spacing(2)+1px)]"))

(* The [']-suffixed sibling of each int constructor takes a half-step float; the
   int base keeps emitting what it always did. *)
let typed_prime () =
  let open Tw in
  check_typed_class "p-0.5" (p' 0.5);
  check_typed_class "px-1.5" (px' 1.5);
  check_typed_class "py-0.5" (py' 0.5);
  check_typed_class "pt-0.5" (pt' 0.5);
  check_typed_class "pr-0.5" (pr' 0.5);
  check_typed_class "pb-0.5" (pb' 0.5);
  check_typed_class "pl-0.5" (pl' 0.5);
  check_typed_class "p-4" (p 4);
  check_typed_class "px-8" (px 8)

(* A data-type hint chooses the longhand and says nothing about the value.
   Padding writes one longhand per side, so every hint reaches it and the length
   reader sees only what follows the hint, which stays in the class name. *)
let test_data_type_hint_before_the_length_reader () =
  check_declarations "p-[length:4px]" [ "padding:4px" ];
  check_declarations "px-[foo:4px]" [ "padding-inline:4px" ];
  check_declarations "p-[length:var(--x)]" [ "padding:var(--x)" ];
  List.iter check [ "p-[length:4px]"; "px-[foo:4px]"; "p-[length:var(--x)]" ];
  let reject c = check_invalid_input (module Tw.Padding.Handler) c in
  reject "p-[:4px]";
  reject "p-[length:]"

let tests =
  [
    test_case "data-type hint before the length reader" `Quick
      test_data_type_hint_before_the_length_reader;
    test_case "arbitrary --spacing()" `Quick test_arbitrary_spacing_fn;
    test_case "padding of_string - valid values" `Quick of_string_valid;
    test_case "padding of_string - invalid values" `Quick of_string_invalid;
    test_case "padding suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "padding candidate order" `Quick candidate_order;
    test_case "padding CSS values" `Quick test_css_values;
    test_case "arbitrary length grammar" `Quick test_arbitrary_length_grammar;
    test_case "typed constructors: half-step" `Quick typed_prime;
  ]

let suite = ("padding", tests)
