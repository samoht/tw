let check = Test_helpers.check_handler_roundtrip (module Tw.Columns.Handler)

let test_roundtrip () =
  check "columns-auto";
  check "columns-1";
  check "columns-2";
  check "columns-3";
  check "columns-12";
  check "columns-3xs";
  check "columns-2xs";
  check "columns-xs";
  check "columns-sm";
  check "columns-md";
  check "columns-lg";
  check "columns-xl";
  check "columns-2xl";
  check "columns-3xl";
  check "columns-4xl";
  check "columns-5xl";
  check "columns-6xl";
  check "columns-7xl";
  (* Arbitrary count ([3]) and width ([16rem]/[200px]) forms. *)
  check "columns-[3]";
  check "columns-[16rem]";
  check "columns-[200px]"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns";
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns-abc";
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns-0x10";
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns-1_0"

(* A bare suffix is written in plain decimal, so [columns-0x10] stays rejected.
   A bracket is not a suffix: it is a token stream Tailwind hands to the
   declaration unvalidated, and the pinned CLI emits [columns: 0x10] for
   [columns-[0x10]]. tw emits it as written rather than reading it with OCaml's
   number reader, which folded [0x10] to [16] and named the rule
   [.columns-\[16\]], a selector the markup can never match. *)
let test_arbitrary_token_stream () =
  Test_helpers.check_declarations "columns-[calc(1+2)]"
    [ "columns:calc(1 + 2)" ];
  Test_helpers.check_declarations "columns-[0x10]" [ "columns:0x10" ];
  Test_helpers.check_declarations "columns-[1_0]" [ "columns:1 0" ];
  Test_helpers.check_declarations "columns-[+3]" [ "columns:+3" ];
  Test_helpers.check_declarations "columns-[0x4rem]" [ "columns:0x4rem" ]

(* columns-[16rem] is a column-WIDTH (columns: 16rem), distinct from the integer
   count form (columns-[3]). *)
let test_columns_arbitrary_width () =
  Test_helpers.check_declarations "columns-[16rem]" [ "columns:16rem" ]

(* The typed constructors (newly exposed in tw.mli) must agree with the parser
   on class names, including the [int] argument of [columns]. *)
let test_typed () =
  Test_helpers.check_typed_class "columns-3" (Tw.columns 3);
  Test_helpers.check_typed_class "columns-auto" Tw.columns_auto;
  Test_helpers.check_typed_class "columns-xs" Tw.columns_xs;
  Test_helpers.check_typed_class "columns-2xl" Tw.columns_2xl;
  Test_helpers.check_typed_class "columns-7xl" Tw.columns_7xl

(* Tailwind orders the values of one utility by a natural (digit-aware) compare
   of the class name, so [columns-9] precedes [columns-10] and the theme names
   interleave with the counts. *)
let test_numeric_order () =
  Test_helpers.check_class_order ~test_name:"columns numeric order"
    [
      "columns-1";
      "columns-2";
      "columns-2xl";
      "columns-2xs";
      "columns-3";
      "columns-3xs";
      "columns-7xl";
      "columns-10";
      "columns-11";
      "columns-12";
      "columns-auto";
      "columns-lg";
    ]

(* Arbitrary widths sort by the same natural compare: a four-character prefix is
   not enough to separate [100px] from [100rem], and the counts still order
   numerically. *)
let test_arbitrary_order () =
  Test_helpers.check_class_order ~test_name:"columns arbitrary order"
    [
      "columns-[9rem]";
      "columns-[10rem]";
      "columns-[100px]";
      "columns-[100rem]";
      "columns-[1000px]";
      "columns-[var(--a)]";
      "columns-[var(--b)]";
    ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "columns arbitrary width" `Quick
        test_columns_arbitrary_width;
      Alcotest.test_case "arbitrary token stream" `Quick
        test_arbitrary_token_stream;
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "numeric order" `Quick test_numeric_order;
      Alcotest.test_case "arbitrary order" `Quick test_arbitrary_order;
    ]

let suite = ("columns", tests)
