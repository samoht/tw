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

(* A bracket list-style value is read with the CSS parser rather than a
   hand-rolled keyword table: list-[square] and list-image-[url(...)] used to be
   unknown classes. A name the keyword table does not know is a
   [<counter-style-name>], which CSS Lists 3 sec. 3.3 admits as any
   [<custom-ident>], so it reads as the author's own [@counter-style]. *)
let test_bracket_list_style () =
  check_declarations "list-[square]" [ "list-style-type:square" ];
  check_declarations "list-image-[url(/carrot.png)]"
    [ "list-style-image:url(/carrot.png)" ];
  check_declarations "list-[nonsense-style]"
    [ "list-style-type:nonsense-style" ]

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

let tests =
  [
    test_case "list style" `Quick test_list_style;
    test_case "bracket list-style" `Quick test_bracket_list_style;
    test_case "list-style property bands" `Slow test_list_style_property_bands;
  ]

let suite = ("list_style", tests)
