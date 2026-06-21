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
  Test_helpers.check_invalid_input (module Tw.Columns.Handler) "columns-abc"

(* columns-[16rem] is a column-WIDTH (columns: 16rem), distinct from the integer
   count form (columns-[3]). *)
let test_columns_arbitrary_width () =
  let css =
    match Tw.of_string "columns-[16rem]" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.fail "could not parse columns-[16rem]"
  in
  Alcotest.(check bool)
    "columns-[16rem] sets columns: 16rem" true
    (Astring.String.is_infix ~affix:"columns:16rem" css)

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "columns arbitrary width" `Quick
        test_columns_arbitrary_width;
    ]

let suite = ("columns", tests)
