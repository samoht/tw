(* Tests for Pp module *)
module Pp = Tw__Pp (* Access internal module *)

let test_str () =
  let result = Pp.str [ "Hello"; " "; "World" ] in
  Alcotest.(check string) "concatenates strings" "Hello World" result

let test_sep () =
  let result = Pp.sep ", " [ "a"; "b"; "c" ] in
  Alcotest.(check string) "joins with separator" "a, b, c" result;

  let empty = Pp.sep ", " [] in
  Alcotest.(check string) "handles empty list" "" empty

let test_lines () =
  let result = Pp.lines [ "line1"; "line2"; "line3" ] in
  Alcotest.(check string) "joins with newlines" "line1\nline2\nline3" result

let test_structural () =
  let kv_result = Pp.kv "key" "value" in
  Alcotest.(check string) "formats key-value" "key: value" kv_result;

  let field_result = Pp.field "name" "John" in
  Alcotest.(check string) "formats field" "name = John;" field_result;

  let braces_result = Pp.braces "content" in
  Alcotest.(check string) "wraps in braces" "{content}" braces_result;

  let parens_result = Pp.parens "content" in
  Alcotest.(check string) "wraps in parens" "(content)" parens_result;

  let quote_result = Pp.quote "hello" in
  Alcotest.(check string) "wraps in quotes" "\"hello\"" quote_result

let test_indent () =
  let result = Pp.indent 4 "code" in
  Alcotest.(check string) "indents with spaces" "    code" result;

  let no_indent = Pp.indent 0 "code" in
  Alcotest.(check string) "no indent for 0" "code" no_indent

let test_option () =
  let some_result = Pp.option Pp.string (Some "value") in
  Alcotest.(check string) "formats Some" "Some (value)" some_result;

  let none_result = Pp.option Pp.string None in
  Alcotest.(check string) "formats None" "None" none_result

let test_list () =
  let default_sep = Pp.list Pp.string [ "a"; "b"; "c" ] in
  Alcotest.(check string) "default separator" "[a, b, c]" default_sep;

  let custom_sep = Pp.list ~sep:" | " Pp.string [ "x"; "y"; "z" ] in
  Alcotest.(check string) "custom separator" "[x | y | z]" custom_sep;

  let empty = Pp.list Pp.string [] in
  Alcotest.(check string) "empty list" "[]" empty

let test_record () =
  let result = Pp.record [ ("name", "Alice"); ("age", "30") ] in
  (* Fields are space-separated, each ending with semicolon *)
  Alcotest.(check string) "formats record" "{ name = Alice; age = 30; }" result;

  let empty = Pp.record [] in
  Alcotest.(check string) "empty record" "{ }" empty

let test_primitives () =
  Alcotest.(check string) "bool true" "true" (Pp.bool true);
  Alcotest.(check string) "bool false" "false" (Pp.bool false);
  Alcotest.(check string) "int" "42" (Pp.int 42);
  Alcotest.(check string) "int negative" "-5" (Pp.int (-5));

  (* Test float formatting without trailing dot *)
  Alcotest.(check string) "float whole" "10" (Pp.float 10.0);
  Alcotest.(check string) "float decimal" "3.14" (Pp.float 3.14)

let suite =
  [
    ( "pp",
      [
        Alcotest.test_case "str" `Quick test_str;
        Alcotest.test_case "sep" `Quick test_sep;
        Alcotest.test_case "lines" `Quick test_lines;
        Alcotest.test_case "structural formatters" `Quick test_structural;
        Alcotest.test_case "indent" `Quick test_indent;
        Alcotest.test_case "option" `Quick test_option;
        Alcotest.test_case "list" `Quick test_list;
        Alcotest.test_case "record" `Quick test_record;
        Alcotest.test_case "primitives" `Quick test_primitives;
      ] );
  ]
