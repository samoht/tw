module Css = Cascade.Css
open Alcotest

let check_class = Test_helpers.check_class
let check = Test_helpers.check_handler_roundtrip (module Tw.Layout.Handler)

let check_overflow =
  Test_helpers.check_handler_roundtrip (module Tw.Overflow.Handler)

let test_display_utilities () =
  check "block";
  check "inline-block";
  check "inline";
  check "hidden"

let test_visibility () =
  check "visible";
  check "invisible";
  check "collapse"

(* box-decoration-break: the v4 box-decoration-* names and the legacy v3
   decoration-clone/slice aliases, which keep their own class name. *)
let test_box_decoration_break () =
  check "box-decoration-clone";
  check "box-decoration-slice";
  check "decoration-clone";
  check "decoration-slice"

let test_z_index () =
  check "z-0";
  check "z-10";
  check "z-20";
  check "z-30";
  check "z-40";
  check "z-50";
  check "z-auto"

let test_overflow () =
  check_overflow "overflow-auto";
  check_overflow "overflow-hidden";
  check_overflow "overflow-clip";
  check_overflow "overflow-visible";
  check_overflow "overflow-scroll";
  check_overflow "overflow-x-auto";
  check_overflow "overflow-y-hidden"

let of_string_invalid () =
  (* Invalid layout values *)
  let fail_maybe input =
    let class_name = String.concat "-" input in
    match Tw.Layout.Handler.of_class Tw.Scheme.default class_name with
    | Ok _ -> fail ("Expected error for: " ^ String.concat "-" input)
    | Error _ -> ()
  in

  fail_maybe [ "inline"; "invalid" ];
  (* Invalid display *)
  fail_maybe [ "table"; "invalid" ];
  (* Invalid table display *)
  (* Note: z-60 is valid in Tailwind v4 (dynamic z-index), so not tested here *)
  fail_maybe [ "z"; ""; "10" ];
  (* Double dash z--10 is invalid *)
  fail_maybe [ "unknown" ]
(* Unknown layout type *)

let test_screen_reader () =
  check_class "sr-only" Tw.Layout.sr_only;
  check_class "not-sr-only" Tw.Layout.not_sr_only

(* Tailwind emits sr-only's declarations in a fixed order (position first,
   clip-path/white-space/border-width last). Keep byte parity so the optimized
   output matches Tailwind. *)
let test_sr_only_declaration_order () =
  let css = Tw.to_css [ Tw.sr_only ] in
  let full = Tw.Css.to_string ~minify:true css in
  (* Scope the search to the .sr-only rule body (the preflight base layer has
     its own width/position declarations). *)
  let body =
    match Astring.String.find_sub ~sub:".sr-only{" full with
    | None -> Alcotest.fail "sr-only rule not found"
    | Some start -> (
        let rest = String.sub full start (String.length full - start) in
        match String.index_opt rest '}' with
        | Some e -> String.sub rest 0 e
        | None -> rest)
  in
  let idx name =
    match Astring.String.find_sub ~sub:name body with
    | Some i -> i
    | None -> Alcotest.failf "sr-only missing declaration %S" name
  in
  let order =
    [
      "position:";
      "width:";
      "height:";
      "padding:";
      "margin:";
      "overflow:";
      "clip-path:";
      "white-space:";
      "border-width:";
    ]
  in
  ignore
    (List.fold_left
       (fun prev name ->
         let i = idx name in
         if i <= prev then
           Alcotest.failf "sr-only declaration %S out of order" name;
         i)
       (-1) order)

let test_layout_container () =
  (* Test that container generates .container rule with width: 100% *)
  let css = Tw.to_css [ Tw.container ] in
  let css_string = Tw.Css.to_string ~minify:true css in
  Alcotest.(check bool)
    "has .container class" true
    (String.contains css_string 'c');
  Alcotest.(check bool) "has width:100%" true (String.contains css_string 'w')

let test_container_vs_at_container () =
  (* Test the difference between layout .container and @container query *)
  let layout = Tw.to_css [ Tw.container ] in
  let query = Tw.to_css [ Tw.at_container ] in
  let layout_str = Tw.Css.to_string ~minify:true layout in
  let query_str = Tw.Css.to_string ~minify:true query in
  Alcotest.(check bool)
    "layout container has .container class" true
    (String.contains layout_str '.');
  Alcotest.(check bool)
    "@container has container-type" true
    (String.contains query_str 't')

let test_layout_container_matches_tailwind () =
  (* This test documents that our implementation matches Tailwind ordering *)
  Test_helpers.check_ordering_matches ~forms:false
    ~test_name:"container matches Tailwind .container ordering" [ Tw.container ]

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      block;
      inline;
      inline_block;
      hidden;
      object_contain;
      object_cover;
      object_fill;
      sr_only;
      not_sr_only;
      table_auto;
      table_fixed;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"layout suborder matches Tailwind" shuffled

(* isolation sits between inset and z-index in Tailwind's order, and float and
   clear between the grid-column/grid-row group and .container -- not with the
   display family their module groups them into. *)
let isolation_and_float_slots () =
  let classes =
    [
      "inset-0";
      "isolate";
      "isolation-auto";
      "z-10";
      "col-span-2";
      "row-span-2";
      "float-left";
      "clear-both";
      "ml-auto";
      "box-border";
      "block";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css =
    Cascade.Css.to_string ~minify:true (Tw.to_css ~base:false utilities)
  in
  let positions =
    List.map
      (fun c ->
        let needle = "." ^ c ^ "{" in
        let n = String.length needle and h = String.length css in
        let rec go i =
          if i + n > h then -1
          else if String.sub css i n = needle then i
          else go (i + 1)
        in
        go 0)
      classes
  in
  Alcotest.check bool "every utility is emitted" true
    (List.for_all (fun p -> p >= 0) positions);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "the utilities come out in Tailwind's order"
    (List.sort Int.compare positions)
    positions

(* Visibility, float and break-* typed constructors are newly exposed in tw.mli;
   check they agree with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "visible" Tw.visible;
  Test_helpers.check_typed_class "invisible" Tw.invisible;
  Test_helpers.check_typed_class "collapse" Tw.collapse;
  Test_helpers.check_typed_class "float-left" Tw.float_left;
  Test_helpers.check_typed_class "float-start" Tw.float_start;
  Test_helpers.check_typed_class "float-none" Tw.float_none;
  Test_helpers.check_typed_class "break-after-page" Tw.break_after_page;
  Test_helpers.check_typed_class "break-before-column" Tw.break_before_column;
  Test_helpers.check_typed_class "break-inside-avoid" Tw.break_inside_avoid

(* [object-[50%]] is a position, not a variable name: it used to come out as
   [object-position: var(--50)]. [z-auto] writes the keyword, since no theme
   declares [--z-index-auto]. *)
let test_object_and_z_values () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "object-[50%]" "object-position:50%";
  has "object-[10px_20px]" "object-position:10px 20px";
  has "object-[var(--x)]" "object-position:var(--x)";
  (* not a position and not a var: not a utility *)
  (match Tw.of_string "object-[<value>]" with
  | Ok _ -> Alcotest.fail "expected object-[<value>] to be rejected"
  | Error _ -> ());
  has "z-auto" "z-index:auto"

(* [z-[...]] takes a z-index. A bracket the z-index grammar cannot read is
   accepted and then raises out of [to_css], a pure conversion, so the rejection
   belongs at parse time. *)
let test_invalid_arbitrary_z_index () =
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
  (* The negated form builds [calc(<inner> * -1)] from a typed z-index, so a
     bracket the grammar cannot read still has nowhere to go. *)
  rejected "-z-[foo]";
  renders "z-[13]";
  renders "z-[auto]";
  renders "z-[var(--x)]";
  renders "-z-[13]";
  renders "-z-[var(--x)]"

(* The bracket is a token stream Tailwind hands to the declaration unvalidated.
   It goes through the arbitrary-value pipeline, not OCaml's number reader, so
   [calc()] reaches the property and a spelling only OCaml reads as a number
   ([0x4], [1_0]) is emitted as written. A word and a percentage are not
   z-indexes and are passed through the same way, as the pinned CLI does. *)
let test_arbitrary_token_stream () =
  (* The z-index grammar reads a math function, so this one is typed rather than
     opaque, and cascade folds a calc() over constants to the number it
     computes. Tailwind writes [calc(1 + 2)]; the two are the same z-index. *)
  Test_helpers.check_declarations "z-[calc(1+2)]" [ "z-index:3" ];
  Test_helpers.check_declarations "z-[calc(var(--x)+2)]"
    [ "z-index:calc(var(--x) + 2)" ];
  Test_helpers.check_declarations "z-[0x4]" [ "z-index:0x4" ];
  Test_helpers.check_declarations "z-[1_0]" [ "z-index:1 0" ];
  Test_helpers.check_declarations "z-[foo]" [ "z-index:foo" ];
  Test_helpers.check_declarations "z-[50%]" [ "z-index:50%" ];
  Test_helpers.check_declarations "z-[1.5]" [ "z-index:1.5" ]

let tests =
  [
    test_case "display utilities" `Quick test_display_utilities;
    test_case "visibility" `Quick test_visibility;
    test_case "box-decoration-break" `Quick test_box_decoration_break;
    test_case "typed constructors" `Quick test_typed;
    test_case "z-index" `Quick test_z_index;
    test_case "object-position and z-auto values" `Quick
      test_object_and_z_values;
    test_case "overflow" `Quick test_overflow;
    test_case "screen reader utilities" `Quick test_screen_reader;
    test_case "sr-only declaration order" `Quick test_sr_only_declaration_order;
    test_case "layout container" `Quick test_layout_container;
    test_case "layout container vs @container" `Quick
      test_container_vs_at_container;
    test_case "layout container matches Tailwind" `Quick
      test_layout_container_matches_tailwind;
    test_case "layout of_string - invalid values" `Quick of_string_invalid;
    test_case "isolation and float slots" `Quick isolation_and_float_slots;
    test_case "layout suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "invalid arbitrary z-index" `Quick test_invalid_arbitrary_z_index;
    test_case "arbitrary z-index token stream" `Quick
      test_arbitrary_token_stream;
  ]

let suite = ("layout", tests)
