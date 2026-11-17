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
  let fail_maybe =
    Test_helpers.check_invalid_parts ~why:Test_helpers.Not_a_utility
      (module Tw.Layout.Handler)
  in

  fail_maybe [ "inline"; "invalid" ];
  fail_maybe [ "table"; "invalid" ];
  (* z-60 is valid in Tailwind v4 (dynamic z-index); the double dash of z--10 is
     not *)
  fail_maybe [ "z"; ""; "10" ];
  fail_maybe [ "unknown" ]

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
  Test_helpers.check_declarations "container"
    [
      "width:100%";
      "max-width:40rem";
      "max-width:48rem";
      "max-width:64rem";
      "max-width:80rem";
      "max-width:96rem";
    ]

(* [container] is the layout utility and [@container] marks a query container;
   the two share a word and nothing else. *)
let test_container_vs_at_container () =
  Test_helpers.check_declarations "@container" [ "container-type:inline-size" ]

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

(* The visibility, float and break-* typed constructors agree with the parser on
   class names. *)
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

(* [object-[50%]] is a position, not a variable name. [z-auto] writes the
   keyword, since no theme declares [--z-index-auto]. *)
let test_object_and_z_values () =
  let has cls decl = Test_helpers.check_declarations cls [ decl ] in
  has "object-[50%]" "object-position:50%";
  has "object-[10px_20px]" "object-position:10px 20px";
  has "object-[var(--x)]" "object-position:var(--x)";
  (* Not a position and not a var: the family's own longhand takes it as the
     token stream it is, which is what the CLI writes. *)
  Test_helpers.check_declarations "object-[<value>]"
    [ "object-position:<value>" ];
  has "z-auto" "z-index:auto"

(* [to_css] is a pure conversion, so a bracket [of_class] accepts renders
   without raising. *)
let test_arbitrary_z_index_renders () =
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  renders "z-[13]";
  renders "z-[auto]";
  renders "z-[var(--x)]";
  renders "-z-[13]";
  renders "-z-[var(--x)]"

(* A negated bracket the z-index grammar cannot read is [calc(<value> * -1)],
   with the value after any hint, as Tailwind writes it. *)
let test_negated_token_stream () =
  List.iter Test_helpers.check_class_matches_tailwind
    [
      "-z-[foo]"; "-z-[red]"; "-z-[3px]"; "-z-[foo:2]"; "-z-[url(x.png)_center]";
    ]

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

(* A data-type hint chooses which longhand a bracket lands in and says nothing
   about the value. [object-] writes one longhand, so every hint lands there and
   the position reader is handed what follows it; the hint stays in the class
   name, which is what the markup carries. Tailwind writes the value through.

   A percentage rather than a keyword, because [object-[top]] is refused with no
   hint involved: the position reader here takes no keyword, which is a separate
   gap. *)
let test_object_bracket_peels_a_hint () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) "class round-trips" cls (Tw.pp u);
          Test_helpers.check_declarations ~minify:false cls
            [ "object-position: 50%" ])
    [ "object-[position:50%]"; "object-[foo:50%]" ]

(* [object-[...]] takes the whole CSS <position> grammar, as Tailwind does and
   as the [bg-position-] bracket does: an edge keyword, a pair of them, a
   keyword with an offset, a var() and a math function. *)
let test_object_bracket_position_grammar () =
  List.iter
    (fun (cls, decl) ->
      Test_helpers.check_declarations ~minify:false cls [ decl ])
    [
      ("object-[top]", "object-position: top");
      ("object-[center]", "object-position: center");
      ("object-[left_top]", "object-position: left top");
      ("object-[bottom_right]", "object-position: bottom right");
      (* cascade resolves an edge keyword carrying an offset to its percentage,
         so this is [100% 2rem] where the CLI writes [right 2rem]. The two
         compute the same position and the canonical differ reports no
         difference; [bg-position-] has read it this way all along. *)
      ("object-[right_2rem]", "object-position: 100% 2rem");
      ("object-[50%_50%]", "object-position: 50% 50%");
    ]

(* A bracket that opens with a var() and goes on past it is no custom property
   reference: Tailwind writes it as it stands. *)
let test_var_prefixed_bracket () =
  List.iter Test_helpers.check_class_matches_tailwind
    [ "object-[var(--a)_1px]"; "object-[var(--a),foo]" ]

let tests =
  [
    Alcotest.test_case "var-prefixed bracket" `Quick test_var_prefixed_bracket;
    test_case "object bracket position grammar" `Quick
      test_object_bracket_position_grammar;
    test_case "object bracket peels a data-type hint" `Quick
      test_object_bracket_peels_a_hint;
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
    test_case "arbitrary z-index renders" `Quick test_arbitrary_z_index_renders;
    test_case "negated z-index token stream" `Quick test_negated_token_stream;
    test_case "arbitrary z-index token stream" `Quick
      test_arbitrary_token_stream;
  ]

let suite = ("layout", tests)
