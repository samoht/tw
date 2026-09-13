(* Tests for the shared substring scan. *)
module Strings = Tw.Strings

let check_index msg expected ~sub s =
  Alcotest.(check (option int)) msg expected (Strings.index ~sub s)

let test_index_found () =
  check_index "at the start" (Some 0) ~sub:"ab" "abc";
  check_index "in the middle" (Some 1) ~sub:"b" "abc";
  check_index "at the end" (Some 1) ~sub:"bc" "abc";
  check_index "the whole string" (Some 0) ~sub:"abc" "abc";
  check_index "the first of two overlapping matches" (Some 0) ~sub:"aa" "aaaa";
  check_index "the wildcard the utility parser splits on" (Some 6) ~sub:"-*"
    "--text-*--line-height"

let test_index_absent () =
  check_index "a missing pattern" None ~sub:"d" "abc";
  check_index "a pattern longer than the string" None ~sub:"abcd" "abc";
  check_index "a pattern whose characters are scattered" None ~sub:"ac" "abc";
  check_index "any pattern in the empty string" None ~sub:"a" ""

(* The empty pattern occurs at every position, so the first one is [0]. *)
let test_index_empty_pattern () =
  check_index "the empty pattern in a string" (Some 0) ~sub:"" "abc";
  check_index "the empty pattern in the empty string" (Some 0) ~sub:"" ""

let check_contains msg expected ~sub s =
  Alcotest.(check bool) msg expected (Strings.contains ~sub s)

let test_contains_found () =
  check_contains "at the start" true ~sub:"ab" "abc";
  check_contains "in the middle" true ~sub:"b" "abc";
  check_contains "at the end" true ~sub:"bc" "abc";
  check_contains "the whole string" true ~sub:"abc" "abc";
  check_contains "an escaped modifier prefix" true ~sub:"before\\:"
    ".before\\:absolute::before"

let test_contains_absent () =
  check_contains "a missing pattern" false ~sub:"d" "abc";
  check_contains "a pattern longer than the string" false ~sub:"abcd" "abc";
  check_contains "a pattern whose characters are scattered" false ~sub:"ac"
    "abc";
  check_contains "any pattern in the empty string" false ~sub:"a" ""

(* The empty string occurs in every string, at every position. Answering [false]
   would break the rule that every prefix of an occurring pattern also
   occurs. *)
let test_contains_empty_pattern () =
  check_contains "the empty pattern in a string" true ~sub:"" "abc";
  check_contains "the empty pattern in the empty string" true ~sub:"" ""

let test_is_digit () =
  Alcotest.(check bool) "zero" true (Strings.is_digit '0');
  Alcotest.(check bool) "nine" true (Strings.is_digit '9');
  Alcotest.(check bool) "five" true (Strings.is_digit '5');
  Alcotest.(check bool) "a letter" false (Strings.is_digit 'a');
  Alcotest.(check bool) "the slash below zero" false (Strings.is_digit '/');
  Alcotest.(check bool) "the colon above nine" false (Strings.is_digit ':')

let suite =
  ( "strings",
    [
      Alcotest.test_case "index finds a pattern" `Quick test_index_found;
      Alcotest.test_case "index rejects an absent pattern" `Quick
        test_index_absent;
      Alcotest.test_case "index accepts the empty pattern" `Quick
        test_index_empty_pattern;
      Alcotest.test_case "contains finds a pattern" `Quick test_contains_found;
      Alcotest.test_case "contains rejects an absent pattern" `Quick
        test_contains_absent;
      Alcotest.test_case "contains accepts the empty pattern" `Quick
        test_contains_empty_pattern;
      Alcotest.test_case "is_digit" `Quick test_is_digit;
    ] )
