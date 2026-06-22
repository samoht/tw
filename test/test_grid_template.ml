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
  check "grid-rows-[repeat(4,minmax(100px,auto))]"

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

  (* Arbitrary values with unparseable contents: should reject, not crash.
     Regression: grid-cols-[1fr_40%] used to raise Invalid_argument mid-run. *)
  let bad input =
    match Tw.Grid_template.Handler.of_class Tw.Scheme.default input with
    | Ok _ -> fail ("Expected error for: " ^ input)
    | Error _ -> ()
  in
  bad "grid-cols-[totally_garbage]";
  bad "grid-cols-[1xyz]";
  bad "grid-rows-[abc_def]";
  bad "auto-cols-[nope]"

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

let tests =
  [
    test_case "grid_template of_string - valid values" `Quick of_string_valid;
    test_case "grid_template of_string - invalid values" `Quick
      of_string_invalid;
    test_case "grid_template suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "grid_template arbitrary functions css" `Quick
      test_grid_functions_css;
  ]

let suite = ("grid_template", tests)
