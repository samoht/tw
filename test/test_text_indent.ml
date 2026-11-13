open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Text_indent.Handler)

let test_text_indent () =
  check "indent-4";
  check "-indent-8";
  check "indent-px";
  check "-indent-px"

(* [indent'] takes a half-step float; the int base keeps emitting what it always
   did. *)
let test_text_indent_prime () =
  check_typed_class "indent-0.5" (Tw.indent' 0.5);
  check_typed_class "indent-4" (Tw.indent 4)

(* A negated arbitrary length is [calc(<value> * -1)], the spelling Tailwind
   writes for every unit. Folding the sign into the number instead was done from
   a unit table that listed a few of them, so [4px] came out [-4px] while [2em]
   and [10vh], which the table missed, already carried the calc. The two compute
   the same length, so only the raw sheets show it; what makes it worth settling
   is that the upstream fixtures record the CLI's spelling. *)
let test_typography_negated_arbitrary_is_a_calc () =
  List.iter
    (fun (cls, decl) ->
      Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("-indent-[4px]", "text-indent: calc(4px * -1)");
      ("-indent-[2rem]", "text-indent: calc(2rem * -1)");
    ]

let tests =
  [
    test_case "text indent" `Quick test_text_indent;
    test_case "text indent half-step" `Quick test_text_indent_prime;
    test_case "negated arbitrary length is a calc" `Quick
      test_typography_negated_arbitrary_is_a_calc;
  ]

let suite = ("text_indent", tests)
