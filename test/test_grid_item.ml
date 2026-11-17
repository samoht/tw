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
  let fail_maybe =
    Test_helpers.check_invalid_parts ~why:Test_helpers.Not_a_utility
      (module Tw.Grid_item.Handler)
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

(* A var() reference is not a <custom-ident>: it substitutes before the
   <grid-line> grammar applies, so its parentheses must reach the output
   unescaped. *)
let test_arbitrary_span_var () =
  let has decl cls =
    Test_helpers.check_declarations ~minify:false cls [ decl ]
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

(* A bracket the grid-line grammar declines is still a utility: Tailwind writes
   it into the longhand the class names, as the author spelled it, and leaves
   the browser to drop it. *)
let test_grid_line_bracket_falls_through () =
  let has cls decl = Test_helpers.check_declarations cls [ decl ] in
  List.iter
    (fun (family, property) ->
      has (family ^ "-[10px]") (property ^ ":10px");
      has (family ^ "-[50%]") (property ^ ":50%");
      has (family ^ "-[1fr]") (property ^ ":1fr");
      has (family ^ "-[<value>]") (property ^ ":<value>");
      has (family ^ "-[url(x.png)_center]") (property ^ ":url(x.png) center");
      (* a hint chooses the longhand and is not part of the value *)
      has (family ^ "-[length:10px]") (property ^ ":10px"))
    [
      ("col", "grid-column");
      ("col-start", "grid-column-start");
      ("col-end", "grid-column-end");
      ("row", "grid-row");
      ("row-start", "grid-row-start");
      ("row-end", "grid-row-end");
    ];
  has "row-[1.5]" "grid-row:1.5";
  (* the brackets the grammar reads keep their typed reading *)
  has "col-[span_2/span_3]" "grid-column:span 2/span 3";
  has "col-start-[7]" "grid-column-start:7";
  Alcotest.(check string)
    "the class keeps its hint" "col-start-[length:10px]"
    (Tw.pp (Result.get_ok (Tw.of_string "col-start-[length:10px]")))

(* A bracket that would end the declaration is the one thing a grid line cannot
   hold, and Tailwind writes nothing for it either. *)
let test_invalid_arbitrary_grid_line () =
  List.iter
    (Test_helpers.check_invalid_input (module Tw.Grid_item.Handler))
    [ "col-[a;b]"; "col-start-[a;b]"; "row-[a;b]"; "row-end-[a;b]" ]

(* A grid line is an arbitrary value: [_] is a space and [\_] a literal
   underscore, so a variable name carrying one keeps the character. *)
let test_grid_line_underscore_escape () =
  Test_helpers.check_declarations ~minify:false {|col-start-[var(--a\_b)]|}
    [ "grid-column-start: var(--a_b)" ]

(* A data-type hint chooses which longhand a bracket lands in and says nothing
   about the value. The grid-item families each write one longhand, so every
   hint lands there and the value reader is handed what follows it; the hint
   stays in the class name, which is what the markup carries.

   [col-span-] is the one that did not merely refuse: it wrote the hint into the
   value, so [col-span-[foo:2]] emitted [span foo:2 / span foo:2]. *)
let test_grid_item_brackets_peel_a_hint () =
  List.iter
    (fun (cls, decl) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("col-[foo:2]", "grid-column: 2");
      ("col-span-[foo:2]", "grid-column: span 2 / span 2");
      ("col-start-[foo:2]", "grid-column-start: 2");
      ("row-[foo:2]", "grid-row: 2");
    ]

(* Tailwind negates any grid-line bracket as [calc(<value> * -1)], with the
   value after any hint, and sorts it with the negated steps. *)
let test_negated_bracket_token_stream () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "-col-[foo]";
      "-col-[2]";
      "-col-start-[foo:2]";
      "-col-end-[var(--x)]";
      "-row-[url(x.png)_center]";
      "-row-start-[3px]";
      "-row-end-[red]";
    ];
  Test_helpers.check_ordering_matches ~test_name:"negated grid brackets"
    (List.map
       (fun c -> Result.get_ok (Tw.of_string c))
       [
         "col-2";
         "-col-[foo]";
         "-col-2";
         "col-[3]";
         "-col-start-[2]";
         "col-start-2";
         "-row-end-[x]";
         "row-end-1";
         "-row-[a]";
         "-row-3";
       ])

(* The class name is what the markup carries, so it keeps the bracket as the
   author wrote it, even where the value is respelled: [calc(1+2)] is spaced out
   for the declaration and not for the selector. *)
let test_grid_bracket_class_as_written () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u -> Alcotest.(check string) "class round-trips" cls (Tw.pp u))
    [
      "col-[calc(1+2)]";
      "col-start-[calc(1+2)]";
      "row-end-[calc(1+2)]";
      "col-span-[calc(1+2)]";
      "row-[span_2/span_3]";
      {|col-start-[var(--a\_b)]|};
    ]

let tests =
  [
    test_case "negated bracket token stream" `Quick
      test_negated_bracket_token_stream;
    test_case "grid bracket class as written" `Quick
      test_grid_bracket_class_as_written;
    test_case "grid-item brackets peel a data-type hint" `Quick
      test_grid_item_brackets_peel_a_hint;
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
    test_case "grid line bracket falls through" `Quick
      test_grid_line_bracket_falls_through;
    test_case "invalid arbitrary grid line" `Quick
      test_invalid_arbitrary_grid_line;
  ]

let suite = ("grid_item", tests)
