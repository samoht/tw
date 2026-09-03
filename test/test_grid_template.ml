open Alcotest

let check =
  Test_helpers.check_handler_roundtrip (module Tw.Grid_template.Handler)

let of_string_valid () =
  (* Grid template columns *)
  check "grid-cols-1";
  check "grid-cols-2";
  check "grid-cols-3";
  check "grid-cols-4";
  check "grid-cols-6";
  check "grid-cols-12";
  check "grid-cols-none";
  check "grid-cols-subgrid";

  (* Grid template rows *)
  check "grid-rows-1";
  check "grid-rows-2";
  check "grid-rows-3";
  check "grid-rows-4";
  check "grid-rows-6";
  check "grid-rows-12";
  check "grid-rows-none";
  check "grid-rows-subgrid";

  (* Grid auto flow *)
  check "grid-flow-row";
  check "grid-flow-col";
  check "grid-flow-dense";
  check "grid-flow-row-dense";
  check "grid-flow-col-dense";

  (* Grid auto columns *)
  check "auto-cols-auto";
  check "auto-cols-min";
  check "auto-cols-max";
  check "auto-cols-fr";

  (* Grid auto rows *)
  check "auto-rows-auto";
  check "auto-rows-min";
  check "auto-rows-max";
  check "auto-rows-fr";

  (* Arbitrary values with mixed units (was crashing on '%' before) *)
  check "grid-cols-[1fr_40%]";
  check "grid-cols-[200px]";
  check "grid-cols-[1fr_2fr]";
  check "grid-cols-[auto_1fr_auto]";
  check "grid-rows-[min-content_1fr_max-content]";
  check "auto-cols-[50%]";
  check "auto-rows-[1.5rem]";

  (* Arbitrary grid functions: repeat()/minmax()/fit-content(), incl. nesting
     and auto-fill/auto-fit (used to be rejected as unknown classes). *)
  check "grid-cols-[minmax(0,1fr)]";
  check "grid-cols-[repeat(3,minmax(0,1fr))]";
  check "grid-cols-[repeat(2,1fr_2fr)]";
  check "grid-cols-[repeat(auto-fill,minmax(0,1fr))]";
  check "grid-cols-[fit-content(200px)]";
  check "grid-rows-[repeat(4,minmax(100px,auto))]";
  (* a math-function track (min/max/clamp) is a length track *)
  check "grid-cols-[min(50%,20rem)]";
  check "grid-cols-[min(50%,20rem)_auto]";
  check "grid-cols-[max(200px,50%)_1fr]"

let of_string_invalid () =
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Grid_template.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "grid" ];
  (* Missing subcommand *)
  fail_maybe [ "grid"; "cols" ];
  (* Missing value *)
  fail_maybe [ "grid"; "cols"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "grid"; "rows" ];
  (* Missing value *)
  fail_maybe [ "grid"; "rows"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "grid"; "flow" ];
  (* Missing flow direction *)
  fail_maybe [ "grid"; "flow"; "invalid" ];

  (* Invalid flow direction *)
  fail_maybe [ "auto" ];
  (* Missing subcommand *)
  fail_maybe [ "auto"; "cols" ];
  (* Missing value *)
  fail_maybe [ "auto"; "cols"; "invalid" ];
  (* Invalid value *)
  fail_maybe [ "auto"; "rows" ];
  (* Missing value *)
  fail_maybe [ "auto"; "rows"; "invalid" ];

  (* Invalid value *)
  ()

(* A bracket the track grammar cannot read is not refused: Tailwind hands it to
   the declaration as written, and so does tw. Reading it must not raise, which
   [grid-cols-[1fr_40%]] once did mid-run. *)
let test_unreadable_tracks_pass_through () =
  Test_helpers.check_declarations "grid-cols-[totally_garbage]"
    [ "grid-template-columns:totally garbage" ];
  Test_helpers.check_declarations "grid-cols-[1xyz]"
    [ "grid-template-columns:1xyz" ];
  Test_helpers.check_declarations "grid-rows-[abc_def]"
    [ "grid-template-rows:abc def" ];
  Test_helpers.check_declarations "auto-cols-[nope]"
    [ "grid-auto-columns:nope" ];
  Test_helpers.check_declarations "grid-cols-[1fr_40%]"
    [ "grid-template-columns:1fr 40%" ]

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    List.init 12 (fun i -> grid_cols (i + 1))
    @ List.init 6 (fun i -> grid_rows (i + 1))
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"grid_template suborder matches Tailwind" shuffled

(* Arbitrary grid functions emit their values verbatim, including bare 0 inside
   minmax (not 0px) and nested repeat()/minmax(). *)
let test_grid_functions_css () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " contains " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "grid-cols-[repeat(3,minmax(0,1fr))]"
    "grid-template-columns:repeat(3,minmax(0,1fr))";
  has "grid-rows-[repeat(4,minmax(100px,auto))]"
    "grid-template-rows:repeat(4,minmax(100px,auto))";
  has "grid-cols-[fit-content(200px)]"
    "grid-template-columns:fit-content(200px)"

(* An arbitrary track can name the spacing scale or hold a var(), including as
   the repeat() count. The token has to be declared alongside the value. *)
let test_arbitrary_track_values () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "grid-cols-[repeat(auto-fit,--spacing(42))]"
    "grid-template-columns:repeat(auto-fit,calc(var(--spacing)*42))";
  has "grid-cols-[repeat(auto-fit,--spacing(42))]" "--spacing:.25rem";
  has "grid-cols-[repeat(var(--columns),var(--width))]"
    "grid-template-columns:repeat(var(--columns),var(--width))"

(* The bracket is a token stream Tailwind hands to the declaration unvalidated.
   It goes through the arbitrary-value pipeline, not OCaml's number reader, so
   [calc()] reaches the property and a spelling only OCaml reads as a number
   ([0x4], [1_0]) is emitted as written rather than folded to a pixel length.
   The bare integer [123] keeps tw's established [123px] reading, which the
   upstream fixture pins. *)
let test_arbitrary_token_stream () =
  Test_helpers.check_declarations "grid-cols-[calc(1+2)]"
    [ "grid-template-columns:calc(1 + 2)" ];
  Test_helpers.check_declarations "grid-cols-[0x4]"
    [ "grid-template-columns:0x4" ];
  Test_helpers.check_declarations "grid-cols-[1_0]"
    [ "grid-template-columns:1 0" ];
  Test_helpers.check_declarations "grid-cols-[0x4rem]"
    [ "grid-template-columns:0x4rem" ];
  Test_helpers.check_declarations "grid-cols-[123]"
    [ "grid-template-columns:123px" ]

let tests =
  [
    test_case "arbitrary token stream" `Quick test_arbitrary_token_stream;
    test_case "unreadable tracks pass through" `Quick
      test_unreadable_tracks_pass_through;
    test_case "arbitrary track values" `Quick test_arbitrary_track_values;
    test_case "grid_template of_string - valid values" `Quick of_string_valid;
    test_case "grid_template of_string - invalid values" `Quick
      of_string_invalid;
    test_case "grid_template suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "grid_template arbitrary functions css" `Quick
      test_grid_functions_css;
  ]

let suite = ("grid_template", tests)
