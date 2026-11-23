open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.List_style.Handler)

let test_list_style () =
  check "list-none";
  check "list-disc";
  check "list-decimal";
  check "list-inside";
  check "list-outside";
  check "list-image-none"

(* A bracket list-style value is read with the CSS parser, so list-[square] and
   list-image-[url(...)] are utilities. A name that is no list-style keyword is
   a [<counter-style-name>], which CSS Lists 3 sec. 3.3 admits as any
   [<custom-ident>], so it reads as the author's own [@counter-style]. *)
let test_bracket_list_style () =
  check_declarations "list-[square]" [ "list-style-type:square" ];
  check_declarations "list-image-[url(/carrot.png)]"
    [ "list-style-image:url(/carrot.png)" ];
  check_declarations "list-[nonsense-style]"
    [ "list-style-type:nonsense-style" ]

(* A bracket no list-style-type grammar reads is still a utility: Tailwind
   writes it into list-style-type as the author spelled it, and the browser
   drops it. *)
let test_bracket_falls_through () =
  let has cls value =
    check_declarations ~minify:false cls [ "list-style-type: " ^ value ]
  in
  has "list-[10px]" "10px";
  has "list-[calc(1+2)]" "calc(1 + 2)";
  has "list-[url(x.png)_center]" "url(x.png) center";
  has "list-[<value>]" "<value>";
  (* a hint says nothing about the value *)
  has "list-[length:10px]" "10px";
  Alcotest.(check string)
    "the class keeps its hint" "list-[length:10px]"
    (Tw.pp (Result.get_ok (Tw.of_string "list-[length:10px]")));
  check_invalid_input (module Tw.List_style.Handler) "list-[a;b]"

(* List position, type, and image are three property bands; candidates inside
   each band use their natural class-name order. *)
let test_list_style_property_bands () =
  Test_helpers.check_class_order ~test_name:"list-style property bands"
    [
      "list-image-none";
      "list-image-[url(/carrot.png)]";
      "list-none";
      "list-disc";
      "list-decimal";
      "list-[square]";
      "list-outside";
      "list-inside";
    ]

(* A bracket that opens with a var() and goes on past it is no custom property
   reference: Tailwind writes it as it stands. *)
let test_var_prefixed_bracket () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "list-[var(--a)_1px]";
      "list-[var(--a),foo]";
      "list-image-[var(--a)_1px]";
      "list-image-[var(--a),foo]";
    ]

(* A hint the family does not know names no reading of its own, so Tailwind
   writes the value after it the way it writes the unhinted bracket. *)
let test_unknown_hint () =
  List.iter Test_helpers.check_class_matches_tailwind
    [ "list-[foo:disc]"; "list-[foo:square]" ]

let tests =
  [
    Alcotest.test_case "unknown hint" `Quick test_unknown_hint;
    Alcotest.test_case "var-prefixed bracket" `Quick test_var_prefixed_bracket;
    test_case "list style" `Quick test_list_style;
    test_case "bracket list-style" `Quick test_bracket_list_style;
    test_case "bracket falls through" `Quick test_bracket_falls_through;
    test_case "list-style property bands" `Slow test_list_style_property_bands;
  ]

let suite = ("list_style", tests)
