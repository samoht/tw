open Alcotest
open Tw_tools.Routing

(* Tailwind emits a declared utility as one block, its own nesting intact:
   [.line-y { padding: 5px; &::before { color: red } }]. Flattened into two
   rules, the second sorts by the property it writes and an unrelated utility
   can land between them. *)
let test_declared_utility_keeps_its_nesting () =
  let udefs = [ ("line-y", " padding: 5px; &::before { color: red } ") ] in
  let count, entries, _ =
    custom_routed_utilities ~theme:Tw.Scheme.default ~defs:[] ~udefs
      [ "line-y" ]
  in
  check int "one candidate generated" 1 count;
  match entries with
  | [ (cls, _, statements) ] ->
      check string "the utility's own class" "line-y" cls;
      check int "one block, not one rule per selector" 1
        (List.length statements);
      check string "the nested rule is still nested"
        ".line-y{padding:5px;&:before{color:red}}"
        (Cascade.Css.to_string ~minify:true (Cascade.Css.v statements))
  | _ -> Alcotest.failf "expected one entry, got %d" (List.length entries)

(* A compound ancestor variant puts [:where(...)] before the candidate class in
   the expanded selector. The routed block still belongs to the candidate that
   produced it; recovering ownership from the selector must not send all of its
   custom-variant branches to an unplaced utilities layer at the end. *)
let test_complex_custom_variant_keeps_candidate () =
  let defs =
    [
      ( "dark",
        "&:where(.dark,.dark \
         *){@slot;}@media(prefers-color-scheme:dark){&:where(.system,.system \
         *){@slot;}}" );
    ]
  in
  let candidate = "dark:in-[figure]:outline-1" in
  let count, entries, unplaced =
    custom_routed_utilities ~theme:Tw.Scheme.default ~defs ~udefs:[]
      [ candidate ]
  in
  check int "one candidate generated" 1 count;
  check bool "no trailing unplaced utilities layer" false
    (List.exists
       (fun stmt ->
         match Cascade.Css.layer_block_name stmt with
         | Some name ->
             Cascade.Css.Stylesheet.equal_layer_name name [ "utilities" ]
         | None -> false)
       unplaced);
  match entries with
  | [ (cls, _, statements) ] ->
      check string "the originating candidate is retained" candidate cls;
      check int "both custom branches stay in its block" 2
        (List.length statements)
  | _ -> Alcotest.failf "expected one entry, got %d" (List.length entries)

(* For utilities in the same property slot, Tailwind sorts the one with more
   declarations first. The modifier contributes the second declaration here, so
   lexical candidate order is deliberately the wrong answer. *)
let test_functional_property_count_order () =
  let udefs =
    [
      ( "example-*",
        " --resolved-value: --value([length]); --resolved-modifier: \
         --modifier([length]) " );
    ]
  in
  let _, entries, _ =
    custom_routed_utilities ~theme:Tw.Scheme.default ~defs:[] ~udefs
      [ "example-[12px]"; "example-[12px]/[16px]" ]
  in
  let css =
    Tw.to_css ~theme:Tw.Scheme.default ~base:false ~layers:false ~extra:entries
      []
    |> Cascade.Css.to_string ~minify:true
  in
  check string "more declarations sort first"
    ".example-\\[12px\\]\\/\\[16px\\]{--resolved-value:12px;--resolved-modifier:16px}.example-\\[12px\\]{--resolved-value:12px}"
    css

(* A candidate a project's functional declaration resolves is theirs to
   generate: [Tw.of_string] does not know it. One the declaration resolves
   nothing for is left to the built-in generator, as Tailwind tries every
   utility registered for a root: [tab-[13]] under [@utility tab-* { tab-size:
   --value(integer) }] is the built-in [tab-*]'s, and a root nothing built in
   knows declines it the same way. *)
let test_functional_routing () =
  let udefs = [ ("example-*", " --resolved-value: --value(integer) ") ] in
  let routed cls =
    is_custom_routed ~theme:Tw.Scheme.default ~defs:[] ~udefs cls
  in
  check bool "a candidate of the root" true (routed "example-4");
  check bool "the root on its own, which carries no value" false
    (routed "example");
  check bool "one the declaration resolves nothing for" false
    (routed "example-foo");
  check bool "a built-in utility" false (routed "flex");
  let tab = [ ("tab-*", " tab-size: --value(integer) ") ] in
  check bool "a bracket the declaration declines" false
    (is_custom_routed ~theme:Tw.Scheme.default ~defs:[] ~udefs:tab "tab-[13]")

(* A [@utility] body is author text tw does not validate. An unclosed brace in
   one must cost that class alone: assembled into a single sheet, the block
   swallows every utility written after it, and the sheet as a whole no longer
   parses, which dropped the lot. *)
let test_malformed_utility_spares_the_others () =
  let udefs =
    [
      ("line-bad", " color: red; &::before { content: \"x\" ");
      ("line-ok", " padding: 5px ");
    ]
  in
  let _, entries, _ =
    custom_routed_utilities ~theme:Tw.Scheme.default ~defs:[] ~udefs
      [ "line-bad"; "line-ok" ]
  in
  let is_ok (cls, _, _) = String.equal cls "line-ok" in
  let name (cls, _, _) = cls in
  match List.find_opt is_ok entries with
  | None ->
      Alcotest.failf "line-ok dropped, entries: %s"
        (String.concat ", " (List.map name entries))
  | Some (_, _, statements) ->
      check string "the good utility stands on its own" ".line-ok{padding:5px}"
        (Cascade.Css.to_string ~minify:true (Cascade.Css.v statements))

let tests =
  [
    test_case "declared utility keeps its nesting" `Quick
      test_declared_utility_keeps_its_nesting;
    test_case "complex custom variant keeps its candidate" `Quick
      test_complex_custom_variant_keeps_candidate;
    test_case "functional property count order" `Quick
      test_functional_property_count_order;
    test_case "functional routing" `Quick test_functional_routing;
    test_case "malformed utility spares the others" `Quick
      test_malformed_utility_spares_the_others;
  ]

let suite = ("routing", tests)
