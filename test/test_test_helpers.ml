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
  ]

let suite = ("test_helpers", tests)
