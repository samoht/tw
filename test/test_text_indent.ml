open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Text_indent.Handler)

let test_text_indent () =
  check "indent-4";
  check "-indent-8";
  check "indent-px";
  check "-indent-px"

(* [indent'] takes a half-step float beside the int [indent]. *)
let test_text_indent_prime () =
  check_typed_class "indent-0.5" (Tw.indent' 0.5);
  check_typed_class "indent-4" (Tw.indent 4)

(* A negated arbitrary length is [calc(<value> * -1)], the spelling Tailwind
   writes for every unit, never a sign folded into the number. Both compute the
   same length, but the upstream fixtures record the CLI's spelling. *)
let test_typography_negated_arbitrary_is_a_calc () =
  List.iter
    (fun (cls, decl) ->
      Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("-indent-[4px]", "text-indent: calc(4px * -1)");
      ("-indent-[2rem]", "text-indent: calc(2rem * -1)");
    ]

(* A negated bracket the length reader cannot read is [calc(<value> * -1)], with
   the value after any hint, as Tailwind writes it. *)
let test_negated_token_stream () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "-indent-[foo]";
      "-indent-[45deg]";
      "-indent-[foo:red]";
      "-indent-[url(x.png)_center]";
    ]

let tests =
  [
    test_case "text indent" `Quick test_text_indent;
    test_case "text indent half-step" `Quick test_text_indent_prime;
    test_case "negated arbitrary length is a calc" `Quick
      test_typography_negated_arbitrary_is_a_calc;
    test_case "negated token stream" `Quick test_negated_token_stream;
  ]

let suite = ("text_indent", tests)
