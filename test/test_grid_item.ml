open Alcotest

let check = Test_helpers.check_handler_roundtrip (module Tw.Grid_item.Handler)

let of_string_valid () =
  (* Column placement *)
  check "col-auto";
  check "col-span-1";
  check "col-span-2";
  check "col-span-3";
  check "col-span-6";
  check "col-span-12";
  check "col-span-full";
  check "col-start-1";
  check "col-start-2";
  check "col-start-auto";
  check "col-end-1";
  check "col-end-7";
  check "col-end-auto";

  (* Row placement *)
  check "row-auto";
  check "row-span-1";
  check "row-span-2";
  check "row-span-3";
  check "row-span-6";
  check "row-span-12";
  check "row-span-full";
  check "row-start-1";
  check "row-start-2";
  check "row-start-auto";
  check "row-end-1";
  check "row-end-7";
  check "row-end-auto"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Grid_item.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "col" ];
  (* Missing value *)
  fail_maybe [ "col"; "span" ];
  (* Missing span value *)
  (* Note: Tailwind v4 accepts col-span-0 and values beyond 12 *)
  fail_maybe [ "col"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "col"; "start" ];
  (* Missing start value *)
  fail_maybe [ "col"; "end" ];

  (* Missing end value *)
  fail_maybe [ "row" ];
  (* Missing value *)
  fail_maybe [ "row"; "span" ];
  (* Missing span value *)
  (* Note: Tailwind v4 accepts row-span-0 and values beyond 12 *)
  fail_maybe [ "row"; "span"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "row"; "start" ];
  (* Missing start value *)
  fail_maybe [ "row"; "end" ]
(* Missing end value *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      col_auto;
      col_span 1;
      col_span 2;
      col_span 6;
      col_span_full;
      col_start 1;
      col_start_auto;
      col_end 1;
      col_end_auto;
      row_auto;
      row_span 1;
      row_span 3;
      row_span_full;
      row_start 2;
      row_start_auto;
      row_end 7;
      row_end_auto;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"grid_item suborder matches Tailwind" shuffled

(* The three shapes an arbitrary span is usually written in: a count, a named
   line and a var(). Anything else the bracket holds is passed through; see
   {!test_arbitrary_span_token_stream}. *)
let test_arbitrary_span_accepted () =
  let accepted cls =
    match Tw.of_string cls with
    | Ok _ -> ()
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  accepted "col-span-[3]";
  accepted "col-span-[mycol]";
  accepted "col-span-[var(--my-variable)]"

let css cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* A var() reference is not a <custom-ident>: it substitutes before the
   <grid-line> grammar applies, so its parentheses must reach the output
   unescaped. *)
let test_arbitrary_span_var () =
  let has affix cls =
    Alcotest.(check bool)
      (Fmt.str "%s emits %s" cls affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "grid-column: span var(--my-variable) / span var(--my-variable)"
    "col-span-[var(--my-variable)]";
  has "grid-row: span var(--my-variable) / span var(--my-variable)"
    "row-span-[var(--my-variable)]"

(* The bracket is a token stream Tailwind hands to the declaration unvalidated.
   It goes through the arbitrary-value pipeline, not OCaml's number reader, so
   [calc()] reaches the property and a spelling only OCaml reads as a number
   ([0x4], [1_0]) is emitted as written rather than folded to a span count. The
   docs' [<value>] placeholder is passed through the same way, as the pinned CLI
   does. *)
let test_arbitrary_span_token_stream () =
  Test_helpers.check_declarations "col-span-[calc(1+2)]"
    [ "grid-column:span calc(1 + 2)/span calc(1 + 2)" ];
  Test_helpers.check_declarations "col-span-[0x4]"
    [ "grid-column:span 0x4/span 0x4" ];
  Test_helpers.check_declarations "col-span-[1_0]"
    [ "grid-column:span 1 0/span 1 0" ];
  Test_helpers.check_declarations "row-span-[calc(1+2)]"
    [ "grid-row:span calc(1 + 2)/span calc(1 + 2)" ];
  Test_helpers.check_declarations "col-span-[<value>]"
    [ "grid-column:span <value>/span <value>" ]

(* [col-[...]] and [row-start-[...]] take grid lines. A bracket the grid-line
   grammar cannot read is accepted and then raises out of [to_css], a pure
   conversion, so the rejection belongs at parse time. *)
let test_invalid_arbitrary_grid_line () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "col-[50%]";
  rejected "row-[1.5]";
  rejected "col-start-[1fr]";
  rejected "row-end-[50%]";
  renders "col-[2]";
  renders "col-[span_2/span_3]";
  renders "col-[var(--x)]";
  renders "col-start-[7]";
  renders "row-end-[3]"

(* A grid line is an arbitrary value: [_] is a space and [\_] a literal
   underscore, so a variable name carrying one keeps the character. *)
let test_grid_line_underscore_escape () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an escaped underscore stays in the variable name" true
    (Astring.String.is_infix ~affix:"grid-column-start: var(--a_b)"
       (css {|col-start-[var(--a\_b)]|}))

let tests =
  [
    test_case "grid line underscore escape" `Quick
      test_grid_line_underscore_escape;
    test_case "grid_item of_string - valid values" `Quick of_string_valid;
    test_case "grid_item of_string - invalid values" `Quick of_string_invalid;
    test_case "arbitrary span accepted" `Quick test_arbitrary_span_accepted;
    test_case "arbitrary span var()" `Quick test_arbitrary_span_var;
    test_case "arbitrary span token stream" `Quick
      test_arbitrary_span_token_stream;
    test_case "grid_item suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "invalid arbitrary grid line" `Quick
      test_invalid_arbitrary_grid_line;
  ]

let suite = ("grid_item", tests)
