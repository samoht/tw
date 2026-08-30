(* The harness decides whether every other suite passes, so what its predicates
   report is worth pinning on its own. Each case here feeds a predicate an input
   whose answer is known by construction and checks the answer, rather than
   checking anything the library emits. *)

(* Two selector shapes a real utility has: divide's rule continues past the
   class name into a combinator, and a longer class shares a shorter one's
   prefix. Written the way a minified sheet spells them. *)
let sheet =
  ".divide-x>:not(:last-child){border-inline-width:1px}"
  ^ ".bg-top{background-position:top}"
  ^ ".divide-x-2>:not(:last-child){border-inline-width:2px}"
  ^ ".bg-top-left{background-position:left top}"

let position cls = Test_helpers.class_position sheet cls

let index_of affix =
  match Astring.String.find_sub ~sub:affix sheet with
  | Some i -> i
  | None -> Alcotest.failf "%s is not in the fixture sheet" affix

(* [.divide-x] is followed by a combinator, not by [{] or [,]. A predicate that
   only accepts those two delimiters reports the class as absent from a sheet
   that declares it, and [check_class_order] then blames the generated CSS. *)
let test_position_of_continuing_selector () =
  Alcotest.(check (option int))
    "divide-x is where its rule starts"
    (Some (index_of ".divide-x>"))
    (position "divide-x")

(* The same shape one level down: the match has to end where the class name
   ends, so [.divide-x] must not be read off [.divide-x-2]. *)
let test_position_ignores_longer_class_with_combinator () =
  Alcotest.(check (option int))
    "divide-x-2 is its own rule"
    (Some (index_of ".divide-x-2>"))
    (position "divide-x-2")

let test_position_ignores_longer_class () =
  Alcotest.(check (option int))
    "bg-top is not bg-top-left"
    (Some (index_of ".bg-top{"))
    (position "bg-top")

let test_position_absent_class () =
  Alcotest.(check (option int))
    "flex is not in the sheet" None (position "flex")

(* Every declaration a class emits, theme bindings included. *)
let declarations_of cls =
  match Tw.of_string cls with
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  | Ok u ->
      Tw.to_css ~base:false [ u ]
      |> Cascade.Css.fold
           (fun acc stmt ->
             match Cascade.Css.as_rule stmt with
             | Some (_, decls, _) -> decls @ acc
             | None -> acc)
           []

let references_var cls =
  Test_helpers.has_var_in_declarations (declarations_of cls)

(* A [var()] under calc() is still a variable reference. A predicate that only
   reads the head of the value calls the spacing scale no reference at all, and
   every assertion that a sheet holds none passes on it by accident. *)
let test_var_under_calc () =
  Alcotest.(check bool)
    "p-4 references the spacing variable" true (references_var "p-4")

(* Same shape one function further in: the colour sits inside color-mix. *)
let test_var_under_color_mix () =
  Alcotest.(check bool)
    "bg-black/50 references the palette variable" true
    (references_var "bg-black/50")

let test_no_var_when_none_referenced () =
  Alcotest.(check bool)
    "text-left references no variable" false
    (references_var "text-left")

(* Where an ordering difference becomes observable: an element carrying two
   classes that write on each other. *)
let paired a b =
  let pairs = Test_helpers.interacting_pairs [ a; b ] in
  List.exists (fun p -> p = (a, b) || p = (b, a)) pairs

(* [inset] and [top] are one property after shorthand expansion, so which of the
   two rules comes last decides where the element sits. They share no property
   name, so pairing on the name alone never puts them on one element. The [px]
   step rather than a scale step: [inset-4] and [top-4] each write [--spacing]
   too, which pairs them for a reason that says nothing about the two
   properties. *)
let test_pairs_shorthand_with_longhand () =
  Alcotest.(check bool)
    "inset-px and top-px share an element" true
    (paired "inset-px" "top-px")

(* The same relation one family over: [flex] writes [flex-grow]. *)
let test_pairs_shorthand_with_its_own_longhand () =
  Alcotest.(check bool)
    "flex-1 and grow share an element" true (paired "flex-1" "grow")

(* A pair that interacts through a variable rather than a slot: shadow-current
   feeds the colour the size utility's shadow reads, which neither class shows
   on its own. *)
let test_pairs_composed_through_a_variable () =
  Alcotest.(check bool)
    "shadow-lg and shadow-current share an element" true
    (paired "shadow-lg" "shadow-current")

let test_unrelated_classes_are_not_paired () =
  Alcotest.(check bool)
    "text-left and italic do not share an element" false
    (paired "text-left" "italic")

(* The list handed to the browser: singles first, then the pairs, with the
   repeat of a class dropped rather than rendered twice. *)
let test_render_elements_are_unique_and_ordered () =
  Alcotest.(check (list string))
    "m-2 twice renders once"
    [ "m-2"; "ml-2"; "m-2 ml-2" ]
    (Test_helpers.render_elements [ "m-2"; "ml-2"; "m-2" ])

(* The whole-sheet reader, on a sheet spelling every shape it has to survive: a
   [;]-terminated [@layer] list, a layer that is not the wanted one, a selector
   list, a nested at-rule block, and a [}] inside a string. *)
let layered =
  "@layer theme,utilities;" ^ "@layer theme{:root{--spacing:.25rem}}"
  ^ "@layer utilities{" ^ ".flex{display:flex}" ^ ".a,.b{color:red}"
  ^ ".q:before{content:\"}\"}"
  ^ "@media (min-width:40rem){.md\\:flex{display:flex}}" ^ "}"

let test_layer_keys_expand_and_stop_at_the_layer () =
  Alcotest.(check (list string))
    "the utilities layer's own statements, selector lists expanded"
    [ ".flex"; ".a"; ".b"; ".q:before"; "@media (min-width:40rem)" ]
    (Test_helpers.layer_statement_keys layered ~layer:"utilities")

let test_layer_keys_absent_layer () =
  Alcotest.(check (list string))
    "a layer the sheet does not declare has no statements" []
    (Test_helpers.layer_statement_keys layered ~layer:"components")

let utilities_layer selectors =
  "@layer utilities{"
  ^ String.concat "" (List.map (fun s -> s ^ "{color:red}") selectors)
  ^ "}"

let gap ~tailwind ~tw =
  Test_helpers.sheet_order_gap ~layer:"utilities"
    ~tailwind:(utilities_layer tailwind) ~tw:(utilities_layer tw)

(* One swapped pair is one statement out of place, not two: moving either half
   past the other settles it, and the gate reports the minimum. *)
let test_order_gap_counts_the_minimum_move () =
  let g =
    gap ~tailwind:[ ".a"; ".b"; ".c"; ".d" ] ~tw:[ ".b"; ".a"; ".c"; ".d" ]
  in
  Alcotest.(check (pair int int))
    "four pairs, one has to move" (4, 1)
    (g.Test_helpers.pairs, g.Test_helpers.moves)

(* A key either side spells twice could be paired with either occurrence, so it
   is left out rather than counted under a guess. *)
let test_order_gap_drops_a_repeated_key () =
  let g = gap ~tailwind:[ ".x"; ".y"; ".z" ] ~tw:[ ".z"; ".y"; ".x"; ".x" ] in
  Alcotest.(check (pair int int))
    "the repeated key leaves two pairs, one of them moved" (2, 1)
    (g.Test_helpers.pairs, g.Test_helpers.moves)

let test_order_gap_agreeing_sheets () =
  let g = gap ~tailwind:[ ".a"; ".b"; ".c" ] ~tw:[ ".a"; ".b"; ".c" ] in
  Alcotest.(check (pair int int))
    "nothing moves when the orders agree" (3, 0)
    (g.Test_helpers.pairs, g.Test_helpers.moves)

(* Mode [`Tree] is the only mode that reports a rule written twice or a
   custom-property binding nothing reads: mode [`Canonical] folds the second
   copy away in its optimizer, and every caller of it prunes unreferenced
   bindings off both sides first. The cases below feed {!Test_helpers.surplus}
   sheets that carry one of those by construction, and sheets that carry only
   the differences the check is not asking about. *)
let surplus_of ~expected ~actual =
  Test_helpers.surplus (Test_helpers.tree_diff_css ~expected ~actual)

let test_surplus_reports_a_rule_written_twice () =
  Alcotest.(check int)
    "the second copy is surplus" 1
    (List.length
       (surplus_of ~expected:"@layer utilities{.a{color:red}.b{color:blue}}"
          ~actual:"@layer utilities{.a{color:red}.b{color:blue}.a{color:red}}"))

let test_surplus_reports_a_binding_nothing_reads () =
  Alcotest.(check int)
    "the dead binding is surplus" 1
    (List.length
       (surplus_of
          ~expected:
            "@layer theme{:root{--a:1px}}@layer utilities{.x{width:var(--a)}}"
          ~actual:
            "@layer theme{:root{--a:1px;--dead:2px}}@layer \
             utilities{.x{width:var(--a)}}"))

(* Two rules writing different properties can be emitted in either order without
   changing what any element computes. That is a difference tree mode reports
   and this check deliberately does not: ordering has its own oracles in
   {!Test_helpers.check_class_order} and {!Test_helpers.sheet_order_gap}. *)
let test_surplus_ignores_a_cascade_neutral_reorder () =
  Alcotest.(check int)
    "a reorder adds nothing" 0
    (List.length
       (surplus_of ~expected:"@layer utilities{.a{color:red}.b{width:1px}}"
          ~actual:"@layer utilities{.b{width:1px}.a{color:red}}"))

(* What respelling both sheets through one printer buys: the two minifiers write
   this value differently and CSS reads the two spellings as the same tokens, so
   the comparison must not see a difference here. Without the respelling it
   does, and mode [`Tree] cannot gate anything against the real CLI. *)
let test_respelling_settles_a_minifier_disagreement () =
  let diff =
    Test_helpers.tree_diff_css ~expected:":root{--c:oklch(63.7% .237 25.331)}"
      ~actual:":root{--c:oklch(63.7%.237 25.331)}"
  in
  match diff.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff -> ()
  | _ ->
      let buf = Buffer.create 256 in
      Cascade_diff.Css_compare.pp ~expected:"Tailwind" ~actual:"tw" buf diff;
      Alcotest.failf "one value, two spellings, reported as a difference:\n%s"
        (Buffer.contents buf)

(* One class from every family tw implements, plus one of every variant shape,
   so the sheet the check reads spans the generator rather than a corner of it.
   Kept as class names because that is what both sides are asked for. *)
let broad_class_set =
  List.concat_map Tw_tools.Source_scan.split_whitespace
    [
      (* Layout, box model, sizing. *)
      "p-4 px-2 m-4 -m-2 mt-8 gap-4 gap-x-2 block flex grid hidden w-4 h-8 \
       max-w-4xl z-10 top-0 absolute relative container aspect-video columns-3 \
       box-border isolate overflow-hidden overscroll-contain contain-layout";
      (* Flex and grid. *)
      "flex-col basis-1/2 grow shrink-0 order-2 items-center justify-between \
       place-items-center content-center self-end justify-self-start \
       grid-cols-2 col-span-2 row-start-2 auto-cols-fr grid-flow-col";
      (* Borders, backgrounds, effects. *)
      "border border-2 border-b border-gray-200 border-solid divide-x-2 \
       divide-gray-200 rounded-sm rounded-t-lg bg-white bg-blue-600 shadow-md \
       inset-shadow-sm ring-2 inset-ring-2 opacity-50 outline-none outline-2 \
       mix-blend-multiply bg-blend-overlay mask-none";
      (* Typography. *)
      "text-lg text-gray-900 font-bold leading-relaxed tracking-wide indent-4 \
       align-middle whitespace-nowrap break-words hyphens-auto antialiased \
       list-disc underline uppercase truncate line-clamp-3 text-shadow-lg \
       tab-4";
      (* Filters and transforms. *)
      "blur-sm brightness-125 contrast-50 grayscale invert saturate-150 sepia \
       hue-rotate-90 drop-shadow-md backdrop-blur-sm translate-x-4 rotate-90 \
       scale-50 skew-x-6 origin-top-left perspective-normal transform-3d \
       backface-hidden";
      (* Transitions, interactivity, tables, SVG, scrolling. *)
      "transition-all duration-150 animate-spin will-change-transform \
       cursor-pointer select-none appearance-none resize-none caret-red-500 \
       accent-blue-500 field-sizing-content touch-pan-x scroll-mt-4 \
       scroll-smooth table-fixed fill-current stroke-2 sr-only";
      (* One of every variant shape. *)
      "hover:bg-blue-500 focus:outline-none active:bg-blue-700 \
       disabled:opacity-50 first:pt-0 last:pb-0 odd:bg-gray-50 before:block \
       after:block marker:text-gray-500 placeholder:text-gray-400 \
       dark:bg-gray-900 sm:p-2 md:grid-cols-2 lg:flex xl:hidden max-md:block \
       min-lg:flex motion-safe:animate-pulse motion-reduce:transition-none \
       contrast-more:border-4 group-hover:text-white peer-checked:bg-blue-500 \
       peer-focus:ring-2 aria-checked:bg-blue-500 data-[state=open]:block \
       not-hover:opacity-50 has-[:focus]:border-2 supports-grid:flex \
       starting:opacity-0 @container @sm:flex dark:hover:bg-gray-800 \
       dark:focus:outline-none md:hover:bg-blue-500 sm:dark:p-4 \
       lg:group-hover:text-white";
      (* Arbitrary values, including one holding a colon. *)
      "bg-[color:var(--brand)] hover:bg-[color:var(--brand)] \
       w-[calc(100%-1rem)] text-[14px] rotate-[10deg]";
    ]

let test_no_surplus_over_a_broad_class_set () =
  let utilities =
    List.map
      (fun cls ->
        match Tw.of_string cls with
        | Ok u -> u
        | Error (`Msg m) -> Alcotest.failf "%s does not parse: %s" cls m)
      broad_class_set
  in
  let diff = Test_helpers.tree_diff utilities in
  let test_name = "broad class set" in
  Test_helpers.check_no_dropped_declarations ~test_name diff;
  Test_helpers.check_no_surplus ~test_name diff

let tests =
  [
    Alcotest.test_case "class position: continuing selector" `Quick
      test_position_of_continuing_selector;
    Alcotest.test_case "class position: longer class with combinator" `Quick
      test_position_ignores_longer_class_with_combinator;
    Alcotest.test_case "class position: longer class" `Quick
      test_position_ignores_longer_class;
    Alcotest.test_case "class position: absent class" `Quick
      test_position_absent_class;
    Alcotest.test_case "var under calc" `Quick test_var_under_calc;
    Alcotest.test_case "var under color-mix" `Quick test_var_under_color_mix;
    Alcotest.test_case "no var referenced" `Quick
      test_no_var_when_none_referenced;
    Alcotest.test_case "pairs shorthand with longhand" `Quick
      test_pairs_shorthand_with_longhand;
    Alcotest.test_case "pairs shorthand with its own longhand" `Quick
      test_pairs_shorthand_with_its_own_longhand;
    Alcotest.test_case "pairs classes composed through a variable" `Quick
      test_pairs_composed_through_a_variable;
    Alcotest.test_case "unrelated classes are not paired" `Quick
      test_unrelated_classes_are_not_paired;
    Alcotest.test_case "render elements are unique and ordered" `Quick
      test_render_elements_are_unique_and_ordered;
    Alcotest.test_case "layer keys: expanded and layer-bounded" `Quick
      test_layer_keys_expand_and_stop_at_the_layer;
    Alcotest.test_case "layer keys: absent layer" `Quick
      test_layer_keys_absent_layer;
    Alcotest.test_case "order gap: minimum move" `Quick
      test_order_gap_counts_the_minimum_move;
    Alcotest.test_case "order gap: repeated key" `Quick
      test_order_gap_drops_a_repeated_key;
    Alcotest.test_case "order gap: agreeing sheets" `Quick
      test_order_gap_agreeing_sheets;
    Alcotest.test_case "surplus: a rule written twice" `Quick
      test_surplus_reports_a_rule_written_twice;
    Alcotest.test_case "surplus: a binding nothing reads" `Quick
      test_surplus_reports_a_binding_nothing_reads;
    Alcotest.test_case "surplus: a cascade-neutral reorder" `Quick
      test_surplus_ignores_a_cascade_neutral_reorder;
    Alcotest.test_case "respelling settles a minifier disagreement" `Quick
      test_respelling_settles_a_minifier_disagreement;
    Alcotest.test_case "no surplus over a broad class set" `Slow
      test_no_surplus_over_a_broad_class_set;
  ]

let suite = ("test_helpers", tests)
