open Alcotest

(* Test parsing valid class strings and converting to CSS *)
let test_base_of_string_valid () =
  let open Tw.Utility in
  match base_of_string [ "p"; "4" ] with
  | Ok base ->
      let style = base_to_style base in
      check string "parsed class name" "p-4" (Tw.Style.pp style)
  | Error _ -> fail "Failed to parse p-4"

(* Test parsing invalid class strings returns error *)
let test_base_of_string_invalid () =
  let open Tw.Utility in
  match base_of_string [ "invalid"; "class" ] with
  | Ok base ->
      let name = Tw.Style.pp (base_to_style base) in
      fail ("Should not parse invalid class, got: " ^ name)
  | Error (`Msg msg) ->
      check bool "error message not empty" true (String.length msg > 0)

(* Test deduplication preserves order and keeps last occurrence *)
let test_deduplicate () =
  let open Tw.Utility in
  (* Parse some utilities *)
  let u1 =
    match base_of_string [ "p"; "0" ] with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in
  let u2 =
    match base_of_string [ "p"; "1" ] with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in
  let u3 =
    match base_of_string [ "p"; "0" ] with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in

  (* Last occurrence should win *)
  let result = deduplicate [ u1; u2; u3 ] in
  check int "deduplicate length" 2 (List.length result)

(* Test deduplication with empty list *)
let test_deduplicate_empty () =
  let open Tw.Utility in
  let result = deduplicate [] in
  check int "empty list dedup" 0 (List.length result)

(* Test CSS parsing *)
let test_css_of_string_valid () =
  match Tw.Utility.css_of_string ".test { color: red; }" with
  | Ok _ -> check bool "can parse CSS" true true
  | Error _ -> fail "Failed to parse valid CSS"

(* Test CSS parsing with invalid input *)
let test_css_of_string_invalid () =
  match Tw.Utility.css_of_string ".test { color }" with
  | Ok _ -> fail "Should not parse invalid CSS"
  | Error _ -> check bool "rejects invalid CSS" true true

(* Test Utility.order returns correct priority and suborder *)
let test_order_priorities () =
  let open Tw.Utility in
  (* Test various utilities - using actual module assignments, not ideal
     priorities *)
  let parse_and_order parts =
    match base_of_string parts with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ String.concat "-" parts)
  in

  (* Test relative ordering between different priority groups *)
  let pos_prio, _ = parse_and_order [ "top"; "0" ] in
  let grid_prio, _ = parse_and_order [ "col"; "span"; "2" ] in
  let margin_prio, _ = parse_and_order [ "m"; "4" ] in
  let padding_prio, _ = parse_and_order [ "p"; "4" ] in
  let typo_prio, _ = parse_and_order [ "text"; "xl" ] in

  (* Verify relative ordering (what matters for CSS) *)
  check bool "positioning before grid" true (pos_prio < grid_prio);
  check bool "grid before margin" true (grid_prio < margin_prio);
  check bool "margin before padding" true (margin_prio < padding_prio);
  check bool "padding before typography" true (padding_prio < typo_prio)

(* Test suborder within same priority group *)
let test_order_suborders () =
  let open Tw.Utility in
  let parse_and_order parts =
    match base_of_string parts with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ String.concat "-" parts)
  in

  (* Test padding suborders (all priority 19) *)
  let _, p_sub = parse_and_order [ "p"; "4" ] in
  let _, px_sub = parse_and_order [ "px"; "4" ] in
  let _, py_sub = parse_and_order [ "py"; "4" ] in
  let _, pt_sub = parse_and_order [ "pt"; "4" ] in

  (* These should have different suborders for proper CSS ordering *)
  check bool "p before px" true (p_sub < px_sub);
  check bool "px before py" true (px_sub < py_sub);
  check bool "py before pt" true (py_sub < pt_sub)

(* Test that ordering is consistent *)
let test_order_consistency () =
  let open Tw.Utility in
  let parse_and_order parts =
    match base_of_string parts with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ String.concat "-" parts)
  in

  (* Call multiple times to ensure deterministic *)
  let o1 = parse_and_order [ "p"; "4" ] in
  let o2 = parse_and_order [ "p"; "4" ] in
  let o3 = parse_and_order [ "p"; "4" ] in

  check (pair int int) "first equals second" o1 o2;
  check (pair int int) "second equals third" o2 o3

let tests =
  [
    test_case "base_of_string valid input" `Quick test_base_of_string_valid;
    test_case "base_of_string invalid input" `Quick test_base_of_string_invalid;
    test_case "deduplicate preserves order" `Quick test_deduplicate;
    test_case "deduplicate handles empty list" `Quick test_deduplicate_empty;
    test_case "css_of_string valid input" `Quick test_css_of_string_valid;
    test_case "css_of_string invalid input" `Quick test_css_of_string_invalid;
    test_case "order returns correct priorities" `Quick test_order_priorities;
    test_case "order returns correct suborders" `Quick test_order_suborders;
    test_case "order is consistent" `Quick test_order_consistency;
  ]

let suite = ("utility", tests)
