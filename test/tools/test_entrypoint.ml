open Alcotest
open Tw_tools.Entrypoint

let string_list = list string
let pair_list = list (pair string string)

(* Class-name scanners. *)

let test_variant_segments () =
  check string_list "plain class" [ "flex" ] (variant_segments "flex");
  check string_list "stacked variants" [ "lg"; "dark"; "flex" ]
    (variant_segments "lg:dark:flex");
  check string_list "colon inside brackets" [ "[&>*]"; "flex" ]
    (variant_segments "[&>*]:flex");
  check string_list "colon inside parens"
    [ "supports-(--x)"; "flex" ]
    (variant_segments "supports-(--x):flex")

let test_split_declared_variants () =
  let defs = [ ("dark", "&:where(.dark, .dark *) { @slot; }") ] in
  check (pair string_list string) "declared variant taken from the middle"
    ([ "dark" ], "lg:flex")
    (split_declared_variants defs "lg:dark:flex");
  check (pair string_list string) "nothing declared" ([], "lg:hover:flex")
    (split_declared_variants defs "lg:hover:flex")

(* Theme extraction. *)

let theme_src =
  "@import \"tailwindcss\" theme(static);\n\
   @theme {\n\
  \  --color-*: initial;\n\
  \  --color-brand: red;\n\
  \  --animate-flash: flash 2s;\n\
  \  @keyframes flash { to { opacity: 0 } }\n\
  \  --spacing: 0.3rem;\n\
   }\n\
   @theme inline {\n\
  \  --font-x: var(--font-ext), system-ui;\n\
   }\n"

let test_theme_overrides () =
  let tokens, inline = theme_overrides_of_css theme_src in
  check pair_list "every token each block declares, in source order"
    [
      ("color-*", "initial");
      ("color-brand", "red");
      ("animate-flash", "flash 2s");
      ("spacing", "0.3rem");
      ("font-x", "var(--font-ext), system-ui");
    ]
    tokens;
  check string_list "only the inline block's names" [ "font-x" ] inline

(* [--<ns>-*: initial] takes a whole namespace out of the theme. The name is not
   a <dashed-ident>, so no CSS parser can build a declaration from it and the
   reset has to be read off the token stream; without it a project's reset never
   reaches the renderer, which then keeps the built-in scale. *)
let test_theme_namespace_reset () =
  let tokens, _ =
    theme_overrides_of_css
      "@theme {\n  --breakpoint-*: initial;\n  --breakpoint-tablet: 800px;\n}"
  in
  check pair_list "the reset stands among the tokens beside it"
    [ ("breakpoint-*", "initial"); ("breakpoint-tablet", "800px") ]
    tokens;
  let tokens, _ =
    theme_overrides_of_css "@theme { --example-*: [x); y]; --example-one: ok; }"
  in
  check pair_list "a mismatched closer does not end the open square block"
    [ ("example-*", "[x); y]"); ("example-one", "ok") ]
    tokens

let test_imports_static_theme () =
  check bool "declared" true (imports_static_theme theme_src);
  check bool "plain import" false
    (imports_static_theme "@import \"tailwindcss\";")

(* Nesting a utility under the rule that applied it. *)

let nested ~classes s =
  Cascade.Selector.to_string ~minify:true
    (nest_on_ampersand ~classes (Cascade.Selector.of_string s))

let test_nest_on_ampersand () =
  check string "class escaped as a hex code point" "&"
    (nested ~classes:[ "2xl:flex" ]
       (Cascade.Selector.to_string (Cascade.Selector.class_ "2xl:flex")));
  check string "utility wrapped in :where" ":where(&>:not(:last-child))"
    (nested ~classes:[ "divide-x" ] ":where(.divide-x > :not(:last-child))");
  check string "group class left alone" "&:is(:where(.group):hover *)"
    (nested ~classes:[ "group-hover:flex" ]
       ".group-hover\\:flex:is(:where(.group):hover *)");
  check string "ancestor class heads the selector" ":where(.group) &"
    (nested ~classes:[ "in-[.group]:flex" ]
       ":where(.group) .in-\\[\\.group\\]\\:flex");
  check string "every arm of a list" "&,&"
    (nested ~classes:[ "a"; "b" ] ".a, .b");
  check string "no class of ours: the leftmost goes" "& .other"
    (nested ~classes:[ "nothing" ] ".first .other")

(* Text passes over the source. *)

let test_strip_tailwind_import_options () =
  check string "options dropped from the import" "@import \"tailwindcss\" ;"
    (strip_tailwind_import_options "@import \"tailwindcss\" theme(static);")

let test_fill_slots () =
  check string "slot replaced by the body"
    "&:where(.dark, .dark *) { color: red }"
    (fill_slots "&:where(.dark, .dark *) { @slot; }" "color: red")

let test_take_custom_variants () =
  let css, defs =
    take_custom_variants
      "@custom-variant dark { &:where(.dark, .dark *) { @slot; } }\n\
       .a { color: red }\n"
  in
  check string "declaration removed" "\n.a { color: red }\n" css;
  check pair_list "name and template"
    [ ("dark", " &:where(.dark, .dark *) { @slot; } ") ]
    defs

let test_take_custom_utilities () =
  let css, defs =
    take_custom_utilities
      "@utility line-y { border-block: 1px solid }\n\
       @utility tab-* { tab-size: --value(integer) }\n"
  in
  check string "declarations removed" "\n\n" css;
  (* The functional form keeps its [-*]: that is what tells a candidate's root
     from the whole name a static declaration spells. *)
  check pair_list "both forms are read"
    [
      ("tab-*", " tab-size: --value(integer) ");
      ("line-y", " border-block: 1px solid ");
    ]
    defs

(* [--spacing(N)] is a reference to the spacing scale, except under an [@theme
   inline] [--spacing]: there the token has no declaration to reference, so the
   step is multiplied out. *)
let test_spacing_shorthand () =
  let scheme inline =
    Tw.Scheme.with_overrides ~inline Tw.Scheme.default [ ("spacing", "4px") ]
  in
  check string "a declared token is referenced"
    ".a { margin: calc(var(--spacing) * 12) }"
    (apply_variants ~theme:(scheme []) ".a { margin: --spacing(12) }");
  check string "an inline token is worked out" ".a { margin: 48px }"
    (apply_variants ~theme:(scheme [ "spacing" ]) ".a { margin: --spacing(12) }")

let test_drop_directives () =
  check string "directives gone, author CSS kept" "\n\n.a { color: red }\n"
    (drop_directives
       "@tailwind utilities;\n@theme { --x: 1 }\n.a { color: red }\n")

let test_hoist_theme_keyframes () =
  check string "keyframes lifted out of the theme block"
    "@theme {\n\
    \  --animate-flash: flash 2s;\n\
    \  \n\
     }\n\
     @keyframes flash { to { opacity: 0 } }"
    (hoist_theme_keyframes
       "@theme {\n\
       \  --animate-flash: flash 2s;\n\
       \  @keyframes flash { to { opacity: 0 } }\n\
        }\n")

(* One [@apply] pulls in a rule per utility, each decorating the same [&]. They
   are merged on selector equality, so the author's rule comes back once holding
   every declaration rather than once per utility. *)
let test_apply_merges_one_rule () =
  let path = "apply-entry.css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc "@import \"tailwindcss\";\n.btn { @apply p-4 m-4; }\n");
  let out =
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () ->
        splice_into_entrypoint ~theme:Tw.Scheme.default ~path (Cascade.Css.v []))
  in
  let btn = Cascade.Selector.class_ "btn" in
  let rules =
    Cascade.Css.statements out
    |> List.filter_map Cascade.Css.statement_selector
    |> List.filter (Cascade.Selector.equal btn)
  in
  check int "one .btn rule" 1 (List.length rules);
  check string "holding both declarations"
    ".btn{margin:calc(var(--spacing)*4);padding:calc(var(--spacing)*4)}"
    (Cascade.Css.to_string ~minify:true out)

(* Each utility an [@apply] pulls in hoists an [@property] block for every
   variable it sets, and the two shadow utilities set the same ones. The hoisted
   blocks are deduplicated on statement identity, so the sheet declares each
   property once however many rules applied a utility that sets it. *)
let test_apply_hoists_each_property_once () =
  let path = "apply-property-entry.css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc
        "@import \"tailwindcss\";\n\
         .a { @apply shadow-md; }\n\
         .b { @apply shadow-lg; }\n");
  let out =
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () ->
        splice_into_entrypoint ~theme:Tw.Scheme.default ~path (Cascade.Css.v []))
  in
  let names =
    Cascade.Css.statements out
    |> List.filter_map (fun stmt ->
        match Cascade.Css.as_property stmt with
        | Some (Cascade.Css.Property_info { name; _ }) -> Some name
        | None -> None)
  in
  check bool "the shadow properties are hoisted" true (names <> []);
  check string_list "each declared once"
    (List.sort_uniq String.compare names)
    (List.sort String.compare names)

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

(* {2 Functional utilities} *)

(* The CSS each candidate gets from the declarations, as [class -> minified
   rule], so a candidate the declarations resolve nothing for shows up as an
   absent entry rather than an empty one. *)
let routed_css ~theme ~udefs candidates =
  let _, entries, _ =
    custom_routed_utilities ~theme ~defs:[] ~udefs candidates
  in
  List.map
    (fun (cls, _, statements) ->
      (cls, Cascade.Css.to_string ~minify:true (Cascade.Css.v statements)))
    entries

let check_routed ~udefs ?(theme = Tw.Scheme.default) msg expected candidates =
  check pair_list msg expected
    (List.sort compare (routed_css ~theme ~udefs candidates))

(* [@utility example-* { ... }] declares a utility whose candidate carries a
   value, read back in the body with [--value(...)]. A candidate the reads do
   not all resolve is no utility of that declaration and produces nothing. *)
let test_functional_value () =
  let udefs = [ ("example-*", " --resolved-value: --value(integer) ") ] in
  check_routed ~udefs "the integer the candidate spells"
    [
      ("example-1", ".example-1{--resolved-value:1}");
      ("example-76", ".example-76{--resolved-value:76}");
    ]
    [ "example-1"; "example-76"; "example-foo"; "example"; "example-2.5" ]

(* A [--value(--namespace)] reads the theme entry the candidate names. How it is
   spelled follows the block the token was declared in: a plain one is a
   reference the theme layer declares, an [@theme reference] one carries its
   value as the fallback, an [@theme inline] one stands for the value. *)
let test_functional_theme_value () =
  let udefs = [ ("example-*", " --resolved-value: --value(--example) ") ] in
  let theme ?inline ?reference () =
    Tw.Scheme.with_overrides ?inline ?reference Tw.Scheme.default
      [ ("example-a", "8") ]
  in
  check_routed ~udefs ~theme:(theme ()) "a declared token is referenced"
    [ ("example-a", ".example-a{--resolved-value:var(--example-a)}") ]
    [ "example-a"; "example-b" ];
  check_routed ~udefs
    ~theme:(theme ~reference:[ "example-a" ] ())
    "a reference token carries its value"
    [ ("example-a", ".example-a{--resolved-value:var(--example-a,8)}") ]
    [ "example-a" ];
  check_routed ~udefs
    ~theme:(theme ~inline:[ "example-a" ] ())
    "an inline token stands for its value"
    [ ("example-a", ".example-a{--resolved-value:8}") ]
    [ "example-a" ]

(* An arbitrary value resolves against [--value([type])], which reads the hint
   the candidate spelled when there is one and infers the type otherwise. *)
let test_functional_arbitrary_value () =
  let udefs = [ ("example-*", " --resolved-value: --value([integer]) ") ] in
  check_routed ~udefs "only the values that read as integers"
    [
      ("example-[1]", ".example-\\[1\\]{--resolved-value:1}");
      ( "example-[integer:var(--my-value)]",
        ".example-\\[integer\\:var\\(--my-value\\)\\]{--resolved-value:var(--my-value)}"
      );
    ]
    [
      "example-[1]";
      "example-[1px]";
      "example-[integer:var(--my-value)]";
      "example-[color:var(--my-value)]";
      "example-(--my-value)";
    ]

(* [--modifier(...)] reads the [/half] of the candidate, and [--default(...)]
   answers for the half - or the value - the candidate left out. A modifier the
   body never resolves makes the whole candidate invalid. *)
let test_functional_modifier () =
  let udefs =
    [
      ( "example-*",
        " --resolved-value: --value(integer, --default(12)); \
         --resolved-modifier: --modifier(integer) " );
    ]
  in
  check_routed ~udefs "the modifier, and the default for the value"
    [
      ("example", ".example{--resolved-value:12}");
      ("example-1/1", ".example-1\\/1{--resolved-value:1;--resolved-modifier:1}");
      ("example/25", ".example\\/25{--resolved-value:12;--resolved-modifier:25}");
    ]
    [ "example"; "example/25"; "example-1/1"; "example/foo" ]

(* A candidate the project's functional declarations root is theirs to generate:
   [Tw.of_string] does not know it, so the routing has to claim it even when the
   declarations end up resolving nothing for it. *)
let test_functional_routing () =
  let udefs = [ ("example-*", " --resolved-value: --value(integer) ") ] in
  let routed cls = is_custom_routed ~defs:[] ~udefs cls in
  check bool "a candidate of the root" true (routed "example-4");
  check bool "the root on its own" true (routed "example");
  check bool "one the declaration resolves nothing for" true
    (routed "example-foo");
  check bool "a built-in utility" false (routed "flex")

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
    test_case "variant segments" `Quick test_variant_segments;
    test_case "declared variants split out" `Quick test_split_declared_variants;
    test_case "theme overrides" `Quick test_theme_overrides;
    test_case "theme namespace reset" `Quick test_theme_namespace_reset;
    test_case "static theme import" `Quick test_imports_static_theme;
    test_case "nest on ampersand" `Quick test_nest_on_ampersand;
    test_case "import options stripped" `Quick
      test_strip_tailwind_import_options;
    test_case "slots filled" `Quick test_fill_slots;
    test_case "custom variants taken" `Quick test_take_custom_variants;
    test_case "custom utilities taken" `Quick test_take_custom_utilities;
    test_case "spacing shorthand" `Quick test_spacing_shorthand;
    test_case "directives dropped" `Quick test_drop_directives;
    test_case "theme keyframes hoisted" `Quick test_hoist_theme_keyframes;
    test_case "@apply merges into one rule" `Quick test_apply_merges_one_rule;
    test_case "@apply hoists each property once" `Quick
      test_apply_hoists_each_property_once;
    test_case "declared utility keeps its nesting" `Quick
      test_declared_utility_keeps_its_nesting;
    test_case "complex custom variant keeps its candidate" `Quick
      test_complex_custom_variant_keeps_candidate;
    test_case "functional value" `Quick test_functional_value;
    test_case "functional theme value" `Quick test_functional_theme_value;
    test_case "functional arbitrary value" `Quick
      test_functional_arbitrary_value;
    test_case "functional modifier" `Quick test_functional_modifier;
    test_case "functional routing" `Quick test_functional_routing;
    test_case "malformed utility spares the others" `Quick
      test_malformed_utility_spares_the_others;
  ]

let suite = ("entrypoint", tests)
