let check = Test_helpers.check_handler_roundtrip (module Tw.Tab.Handler)

let test_roundtrip () =
  check "tab-2";
  check "tab-8";
  check "tab-[3]";
  check "tab-[12px]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-2.5";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-unknown";
  (* A tab size is written in plain decimal: [tab-0x4] named itself [.tab-4]. *)
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-0x4";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-04";
  Test_helpers.check_invalid_input (module Tw.Tab.Handler) "tab-1_0"

(* A bracket is a token stream Tailwind hands to the declaration unvalidated, so
   it goes through the arbitrary-value pipeline rather than OCaml's number
   reader. [calc()] reaches the property, and a spelling only OCaml reads as a
   number ([0x4], [1_0]) is emitted as written instead of folded to [4] and
   [10]. The bare suffix keeps rejecting both, above. *)
let test_arbitrary_token_stream () =
  Test_helpers.check_declarations "tab-[calc(1+2)]" [ "tab-size:calc(1 + 2)" ];
  Test_helpers.check_declarations "tab-[0x4]" [ "tab-size:0x4" ];
  Test_helpers.check_declarations "tab-[1_0]" [ "tab-size:1 0" ];
  Test_helpers.check_declarations "tab-[0x4px]" [ "tab-size:0x4px" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "arbitrary token stream" `Quick
        test_arbitrary_token_stream;
    ]

let suite = ("tab", tests)
