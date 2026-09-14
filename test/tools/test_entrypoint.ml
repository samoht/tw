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
  (* The theme block comes with them: both declarations read [--spacing], and a
     sheet that does not declare it resolves them to nothing. *)
  check string "holding both declarations and the token they read"
    ".btn{margin:calc(var(--spacing)*4);padding:calc(var(--spacing)*4)}@layer \
     theme{:root,:host{--spacing:.25rem}}"
    (Cascade.Css.to_string ~minify:true out)

(* Lightning CSS lowers a dynamic [color-mix()] in author CSS as progressive
   enhancement. A palette token can be resolved to a legacy colour; a custom
   property declared by the rule cannot, so its first colour operand is the
   fallback. Custom-property values and ordinary colour properties follow the
   same rule. *)
let test_authored_color_mix_fallbacks () =
  let path = "color-mix-entry.css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc
        "@import \"tailwindcss\";\n\
         @theme { --color-brand: oklch(63.7% 0.237 25.331); }\n\
         .article {\n\
        \  --prose-color: var(--color-brand);\n\
        \  --marker-color: color-mix(in oklab, var(--color-brand) 25%, \
         transparent);\n\
        \  color: color-mix(in oklab, var(--color-brand) 25%, transparent);\n\
        \  em { color: color-mix(in oklab, var(--prose-color) 75%, \
         transparent); }\n\
         }\n\
         .existing {\n\
        \  background-color: #fb2c3640;\n\
        \  background-color: color-mix(in oklab, var(--color-brand) 25%, \
         transparent);\n\
         }\n\
         .applied {\n\
        \  @apply bg-gray-700/40;\n\
         }\n");
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("color-brand", "oklch(63.7% 0.237 25.331)") ]
  in
  let out =
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () -> splice_into_entrypoint ~theme ~path (Cascade.Css.v []))
  in
  check string "fallback immediately precedes each guarded authored value"
    "@layer theme{:root,:host{--color-brand:oklch(63.7%.237 \
     25.331)}:root,:host{--color-gray-700:oklch(37.3%.034 \
     259.733)}}.article{--prose-color:var(--color-brand);--marker-color:#fb2c3640}@supports(color:color-mix(in \
     lab,red,red)){.article{--marker-color:color-mix(in \
     oklab,var(--color-brand) \
     25%,transparent)}}.article{color:#fb2c3640}@supports(color:color-mix(in \
     lab,red,red)){.article{color:color-mix(in oklab,var(--color-brand) \
     25%,transparent)}}.article \
     em{color:var(--prose-color)}@supports(color:color-mix(in \
     lab,red,red)){.article em{color:color-mix(in oklab,var(--prose-color) \
     75%,transparent)}}.existing{background-color:#fb2c3640}@supports(color:color-mix(in \
     lab,red,red)){.existing{background-color:color-mix(in \
     oklab,var(--color-brand) \
     25%,transparent)}}.applied{background-color:#36415366}@supports(color:color-mix(in \
     lab,red,red)){.applied{background-color:color-mix(in \
     oklab,var(--color-gray-700) 40%,transparent)}}"
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

(* The initial values of the variables the utilities set. [@layer properties]
   holds them, on the universal selector, under one browser-detection
   [@supports] condition. *)
let property_fallbacks sheet =
  Cascade.Css.statements sheet
  |> List.concat_map (fun stmt ->
      match Cascade.Css.as_layer stmt with
      | Some (Some name, inner)
        when Cascade.Css.Stylesheet.equal_layer_name name [ "properties" ] ->
          inner
      | _ -> [])

let fallback_names stmts =
  List.concat_map
    (fun stmt ->
      match Cascade.Css.as_supports stmt with
      | None -> []
      | Some (_, inner) ->
          List.concat_map
            (fun stmt ->
              match Cascade.Css.as_rule stmt with
              | Some (_, decls, _) ->
                  List.map Cascade.Css.Declaration.property_name decls
              | None -> [])
            inner)
    stmts

(* The generated sheet hoists a [@layer properties] block and so does every
   [@apply], and the two overlap whenever they name utilities that set the same
   variables. Folded into the single layer Tailwind writes, they arrive as a run
   of [@supports] blocks over one condition and one universal selector, each
   repeating what the others already declare. Tailwind v4.3.3 writes one block
   for the same input and declares each variable once. *)
let test_property_fallbacks_in_one_block () =
  let path = "property-fallback-entry.css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc "@import \"tailwindcss\";\n.a { @apply shadow-md; }\n");
  let generated =
    Tw.to_css ~theme:Tw.Scheme.default ~base:false ~forms:false
      [ Tw.shadow_lg; Tw.blur_sm ]
  in
  let out =
    Fun.protect
      ~finally:(fun () -> Sys.remove path)
      (fun () ->
        splice_into_entrypoint ~theme:Tw.Scheme.default ~path generated)
  in
  let fallbacks = property_fallbacks out in
  check int "one block, the way Tailwind writes it" 1 (List.length fallbacks);
  let names = fallback_names fallbacks in
  check bool "the fallbacks are there" true (names <> []);
  check string_list "each variable initialised once"
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

(* Curly blocks are declaration boundaries rather than opaque values: a nested
   rule may itself contain a functional declaration that needs resolving. *)
let test_functional_nested_rule () =
  let udefs =
    [ ("example-*", " &::before { --resolved-value: --value(integer); } ") ]
  in
  check_routed ~udefs "the declaration under the nested rule is resolved"
    [ ("example-7", ".example-7:before{--resolved-value:7}") ]
    [ "example-7" ]

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

(* An [@apply] must leave no [var()] the sheet does not declare.

   This is the invariant a parity comparison misses: the rule [@apply] emits is
   byte-identical to the reference in every one of these cases, so a diff of
   the utilities layer passes while the page renders unstyled, because nothing
   declares the token the rule reads. Colours and spacing hid it for a long
   time - those two are rescued downstream by [Build.referenced_theme_decls],
   so the families that carry their own namespace were the only ones broken.

   The check is self-contained rather than a comparison, so it needs no CLI and
   holds for any utility added later. *)
(* Only a read with no fallback has to be declared. [var(--tw-leading,
   var(--text-lg--line-height))] is the documented shape of a channel variable
   a utility reads but never sets, and the fallback is what covers it. *)
let var_re =
  Re.compile
    (Re.seq
       [
         Re.str "var(--";
         Re.group (Re.rep1 (Re.compl [ Re.set ",) " ]));
         Re.char ')';
       ])

let declared_re =
  Re.compile
    (Re.seq
       [
         Re.char '-';
         Re.char '-';
         Re.group (Re.rep1 (Re.compl [ Re.set ":;{} " ]));
         Re.char ':';
       ])

let names re s =
  Re.all re s |> List.map (fun g -> Re.Group.get g 1) |> List.sort_uniq compare

let applied_sheet body =
  let path = Filename.temp_file "apply-token" ".css" in
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let oc = open_out path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () ->
          output_string oc
            (String.concat "" [ "@import \"tailwindcss\";\n"; body; "\n" ]));
      splice_into_entrypoint ~theme:Tw.Scheme.default ~path (Cascade.Css.v [])
      |> Cascade.Css.to_string ~minify:true)

let check_no_dangling_var label css =
  let read = names var_re css and set = names declared_re css in
  let dangling = List.filter (fun v -> not (List.mem v set)) read in
  match dangling with
  | [] -> ()
  | missing ->
      Alcotest.failf "%s reads --%s with nothing declaring it@.%s" label
        (String.concat ", --" missing)
        css

(* One per theme namespace a utility reads through [var()]. The six spellings
   that were always right are here too, so a fix that trades one for the other
   cannot pass. *)
let applied_utilities =
  [
    "rounded-lg";
    "text-lg";
    "blur-sm";
    "ease-in-out";
    "animate-spin";
    "max-w-md";
    "perspective-near";
    "tracking-wide";
    "leading-relaxed";
    "p-6";
    "gap-4";
    "text-blue-500";
    "bg-red-200";
    "border-gray-300";
    "ring-blue-400";
    "shadow-sm";
    "font-mono";
    "duration-200";
  ]

let test_apply_declares_every_token_it_reads () =
  List.iter
    (fun cls ->
      check_no_dangling_var
        (String.concat "" [ ".card { @apply "; cls; " }" ])
        (applied_sheet (String.concat "" [ ".card { @apply "; cls; "; }" ]));
      check_no_dangling_var
        (String.concat "" [ "@utility card { @apply "; cls; " }" ])
        (applied_sheet
           (String.concat "" [ "@utility card { @apply "; cls; "; }" ])))
    applied_utilities

(* An animation utility names a [@keyframes] block, which is not a declaration
   and so not covered by the [var()] invariant above. *)
(* The same invariant for author CSS that never mentions [@apply]. The value
   shorthands read a theme token as surely as a utility does, and a token the
   sheet does not declare leaves the declaration resolving to nothing.

   A project [@theme] token is not covered here: the theme it declares is built
   by the caller, and [splice_into_entrypoint] is handed one rather than
   reading it back. That path is measured against the CLI instead. *)
let test_author_css_declares_every_token_it_reads () =
  List.iter
    (fun body -> check_no_dangling_var body (applied_sheet body))
    [
      ".btn { padding: --spacing(4); }";
      ".btn { margin: --spacing(2.5); }";
      ".btn { color: theme(--color-red-500); }";
      ".btn { color: theme(colors.red.500); }";
    ]

(* [prefix(tw)] is read off the import the way [theme(static)] is. A [prefix()]
   outside an import statement is not the option, and neither is one in a later
   import that the first did not carry. *)
let test_import_prefix () =
  let prefix s = import_prefix s in
  check (option string) "the option on the import" (Some "tw")
    (prefix "@import \"tailwindcss\" prefix(tw);");
  check (option string) "beside other options" (Some "app")
    (prefix "@import \"tailwindcss\" source(none) prefix(app) theme(static);");
  check (option string) "no option" None (prefix "@import \"tailwindcss\";");
  check (option string) "a call outside an import" None
    (prefix ".a { width: prefix(tw) }");
  check (option string) "an empty option names nothing" None
    (prefix "@import \"tailwindcss\" prefix();")

let test_apply_keeps_the_keyframes () =
  let css = applied_sheet ".card { @apply animate-spin; }" in
  check bool "the @keyframes the animation names survives" true
    (Astring.String.is_infix ~affix:"@keyframes spin" css)

(* A utility the stylesheet declares for itself is what an [@apply] beside it
   names most often: a component class built from a project utility. Static and
   functional alike, with a modifier or without, each comes back carrying the
   declarations the pinned CLI gives it. *)
let test_apply_declared_utility () =
  let css =
    applied_sheet
      "@utility card { tab-size: 8; }\n\
       @utility bar-* { tab-size: --value(integer); line-clamp: \
       --modifier(integer); }\n\
       .a { @apply bar-2; }\n\
       .b { @apply bar-2/3; }\n\
       .c { @apply card; }"
  in
  List.iter
    (fun rule -> check bool rule true (Astring.String.is_infix ~affix:rule css))
    [ ".a{tab-size:2}"; ".b{tab-size:2;line-clamp:3}"; ".c{tab-size:8}" ]

(* A sweep over Tailwind's CSS dialect: each case is an entrypoint, compiled by
   tw the way the [tw] CLI compiles a project and by the pinned CLI, and the two
   sheets compared whole.

   Every defect found in this area since the corpus went in has been invisible
   to the markup sweeps: the utility rules were byte-identical and only the
   theme layer differed, or a value shorthand passed through unexpanded and the
   browser dropped the declaration. A class list cannot reach any of it, so the
   entrypoint is the unit under test.

   A case declares what the CLI says about it: parity, or a divergence with the
   reason it is one. Either verdict can fail. A divergence that starts matching
   fails as surely as a parity case that stops, so a gap that gets fixed reports
   itself rather than going quiet, and the change that fixes it moves the case
   to parity. *)
type verdict = Parity | Diverges of string

type case = {
  name : string;
  entry : string;
  classes : string list;
      (** The markup's classes. tw is handed them; the CLI reads them from an
          [@source inline] the sweep appends to the entrypoint. *)
  files : (string * string) list;
      (** Files the entrypoint reads, written beside it. *)
  verdict : verdict;
}

let case ?(classes = []) ?(files = []) ?why name entry =
  let verdict = match why with None -> Parity | Some why -> Diverges why in
  { name; entry; classes; files; verdict }

(* An entrypoint that imports all of Tailwind and fences its sources, so nothing
   is scanned and what is generated is what the case asks for. *)
let fenced body =
  String.concat "" [ "@import \"tailwindcss\" source(none);\n"; body; "\n" ]

let cases =
  [
    case "apply-plain" (fenced ".btn { @apply p-4 rounded-lg; }");
    case "apply-colour" (fenced ".btn { @apply bg-blue-500 text-white; }");
    case "apply-important" (fenced ".btn { @apply bg-blue-500!; }");
    case "apply-namespaced"
      (fenced ".btn { @apply blur-sm ease-in-out tracking-wide; }");
    case "apply-animation" (fenced ".btn { @apply animate-spin; }");
    case "apply-nested" (fenced ".btn { &:hover { @apply underline; } }");
    case "apply-in-layer" (fenced "@layer components { .btn { @apply p-4; } }");
    case "utility-apply" (fenced "@utility card { @apply rounded-lg; }");
    case "apply-declared-utility"
      (fenced "@utility card { tab-size: 8; } .btn { @apply card; }");
    case "utility-functional"
      ~classes:[ "foo-2"; "foo-2/3"; "foo-2/[7]" ]
      (fenced
         "@utility foo-* { z-index: --value(integer); order: \
          --modifier(integer, [integer]); }");
    case "spacing-fn" (fenced ".btn { padding: --spacing(4); }");
    case "spacing-fn-fraction" (fenced ".btn { margin: --spacing(2.5); }");
    case "alpha-fn"
      (fenced ".btn { color: --alpha(var(--color-red-500) / 50%); }");
    case "theme-fn" (fenced ".btn { color: theme(--color-red-500); }");
    case "theme-fn-dashed" (fenced ".btn { color: --theme(--color-red-500); }");
    case "theme-fn-v3" (fenced ".btn { color: theme(colors.red.500); }");
    case "theme-in-media"
      (fenced
         "@media (width >= theme(--breakpoint-md)) { .btn { display: flex; } }");
    case "theme-block"
      (fenced
         "@theme { --color-brand: #1da1f2; } .btn { color: var(--color-brand); \
          }");
    case "theme-inline"
      (fenced
         "@theme inline { --color-brand: #1da1f2; } .btn { color: \
          var(--color-brand); }");
    case "theme-reference"
      ~why:"a reference token author CSS reads is declared, where none may be"
      (fenced
         "@theme reference { --color-brand: #1da1f2; } .btn { color: \
          var(--color-brand); }");
    case "theme-default"
      (fenced
         "@theme default { --color-brand: #1da1f2; } .btn { color: \
          var(--color-brand); }");
    case "theme-static" (fenced "@theme static { --color-brand: #1da1f2; }");
    case "theme-keyframes"
      (fenced
         "@theme { --animate-wiggle: wiggle 1s; @keyframes wiggle { to { \
          transform: rotate(3deg); } } } .btn { animation: \
          var(--animate-wiggle); }");
    case "custom-variant"
      (fenced
         "@custom-variant dark (&:where(.dark, .dark *)); .btn { @apply p-4; }");
    case "custom-variant-slot"
      (fenced
         "@custom-variant hocus { &:hover, &:focus { @slot; } } .btn { \
          @variant hocus { color: red; } }");
    case "variant-at-rule" (fenced ".btn { @variant dark { color: white; } }");
    case "colour-mix-author"
      (fenced
         "@theme { --color-brand: #1da1f2; } .btn { color: color-mix(in oklab, \
          var(--color-brand) 25%, transparent); }");
    case "property-author"
      ~why:
        "cascade's optimizer keeps initial-value: 0px in the @property where \
         the reference minifies it to 0"
      (fenced
         "@property --my-x { syntax: \"<length>\"; inherits: false; \
          initial-value: 0px; } .a { --my-x: 2px; }");
    case "plugin-typography"
      ~why:
        "@apply prose swaps the plugin's inner .prose for the applying class \
         too"
      (fenced "@plugin \"@tailwindcss/typography\"; .btn { @apply prose; }");
    case "config-js"
      ~files:
        [
          ( "sweep-config.js",
            "module.exports = { theme: { extend: { colors: { brand: '#1da1f2' \
             } } } };\n" );
        ]
      ~why:
        "tw does not evaluate a JavaScript config, so a colour it adds \
         resolves to nothing"
      (fenced
         "@config \"./sweep-config.js\"; .btn { color: theme(colors.brand); }");
    case "import-important"
      ~classes:[ "p-4"; "hover:underline" ]
      "@import \"tailwindcss\" important source(none);\n";
    case "import-prefix" ~classes:[ "tw:p-4" ]
      "@import \"tailwindcss\" prefix(tw) source(none);\n";
    case "import-theme-static"
      ~why:
        "the static theme leaves out the --text-*--line-height tokens and the \
         keyframes"
      "@import \"tailwindcss\" theme(static) source(none);\n";
    case "reference"
      ~why:
        "tw emits the theme layer a reference exists to leave out, and each \
         var() loses its fallback"
      "@reference \"tailwindcss\";\n\
       .btn { @apply rounded-lg bg-blue-600 p-4; }\n";
    case "source-inline"
      (fenced "@source inline(\"underline hover:bg-red-500\");");
    case "source-not-inline" ~classes:[ "p-4"; "m-2" ]
      (fenced "@source not inline(\"m-2\");");
    case "tailwind-utilities" ~classes:[ "p-4" ]
      "@import \"tailwindcss/theme.css\" layer(theme);\n\
       @tailwind utilities source(none);\n";
    case "sub-imports" ~classes:[ "p-4" ]
      "@import \"tailwindcss/theme.css\" layer(theme);\n\
       @import \"tailwindcss/utilities.css\" layer(utilities) source(none);\n";
    case "transition-discrete" ~classes:[ "transition-discrete" ] (fenced "");
  ]

let write_file path contents =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let inline_source = function
  | [] -> ""
  | classes ->
      String.concat ""
        [ "@source inline(\""; String.concat " " classes; "\");\n" ]

(* The sheet the [tw] CLI prints for the project, rendered the way the sweep has
   always rendered it. *)
let tw_sheet ~path classes =
  let theme =
    Tw_tools.Entrypoint.theme_of_css (Tw_tools.Entrypoint.read_file path)
  in
  let _, sheet =
    Tw_tools.Project.stylesheet ~theme ~entrypoint:path ~base:true classes
  in
  let rename_custom_property = Tw.theme_token_rename ~theme in
  Cascade.Css.optimize sheet
  |> Cascade.Css.to_string ~minify:true ?rename_custom_property

let matches_cli case =
  (* Beside the other entrypoints these tests write, so the CLI resolves
     [@import "tailwindcss"] against the project's own node_modules. *)
  let path = "sweep-" ^ case.name ^ ".css" in
  let written = path :: List.map fst case.files in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun p -> if Sys.file_exists p then Sys.remove p) written)
    (fun () ->
      List.iter (fun (p, contents) -> write_file p contents) case.files;
      write_file path (case.entry ^ inline_source case.classes);
      let tw = tw_sheet ~path case.classes in
      let cli = Tw_tools.Tailwind_gen.generate_entrypoint ~minify:true path in
      match (Tw_tools.Parity_compare.diff ~mode:`Canonical cli tw).result with
      | Cascade_diff.Css_compare.No_diff -> true
      | _ -> false)

let misjudged case =
  match (case.verdict, matches_cli case) with
  | Parity, true | Diverges _, false -> None
  | Parity, false -> Some (case.name ^ " diverges from the CLI")
  | Diverges why, true ->
      Some
        (String.concat ""
           [
             case.name;
             " matches the CLI now, so move it to parity (it diverged because ";
             why;
             ")";
           ])

let test_dialect_sweep () =
  Test_helpers.require_tailwind_cli ();
  match List.filter_map misjudged cases with
  | [] -> ()
  | wrong -> Alcotest.fail (String.concat "; " wrong)

(* The inventory is read off the pinned bundle rather than written from memory:
   a list its author wrote can only hold what its author already knew. The
   bundle quotes every at-keyword it handles, CSS's own included, and keys its
   value functions by name, so what is left once CSS's at-rules are set aside is
   the dialect. A directive a Tailwind upgrade adds then fails here until a case
   covers it. *)
let css_at_rules =
  [
    "@charset";
    "@container";
    "@custom-media";
    "@keyframes";
    "@layer";
    "@media";
    "@namespace";
    "@page";
    "@property";
    "@starting-style";
    "@supports";
    "@view-transition";
    (* Not at-rules: the bundle quotes the [@min-*] and [@max-*] container
       variant roots the same way. *)
    "@max";
    "@min";
  ]

let quoted_at_keyword_re =
  Re.compile
    (Re.seq
       [
         Re.char '"';
         Re.group
           (Re.seq
              [ Re.char '@'; Re.rep1 (Re.alt [ Re.rg 'a' 'z'; Re.char '-' ]) ]);
         Re.char '"';
       ])

let value_function_key_re =
  Re.compile
    (Re.seq
       [
         Re.char '"';
         Re.group (Re.seq [ Re.str "--"; Re.rep1 (Re.rg 'a' 'z') ]);
         Re.str "\":";
       ])

let rec dir_above name dir =
  let candidate = Filename.concat dir name in
  if Sys.file_exists candidate then Some candidate
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else dir_above name parent

let bundle_sources () =
  match dir_above "node_modules/tailwindcss/dist" (Sys.getcwd ()) with
  | None ->
      Test_helpers.require_tailwind_cli ();
      Alcotest.fail "the CLI runs but its bundle is not under node_modules"
  | Some dist ->
      Sys.readdir dist |> Array.to_list
      |> List.filter (fun f -> Filename.check_suffix f ".mjs")
      |> List.map (fun f ->
          Tw_tools.Entrypoint.read_file (Filename.concat dist f))

let uses_at_keyword keyword entry =
  Re.execp
    (Re.compile
       (Re.seq [ Re.str keyword; Re.alt [ Re.set " ;{(\"'\n"; Re.eos ] ]))
    entry

let uses_function name entry = Astring.String.is_infix ~affix:(name ^ "(") entry

let test_dialect_surface_is_covered () =
  let bundle = bundle_sources () in
  let named re =
    List.concat_map
      (fun src -> List.map (fun g -> Re.Group.get g 1) (Re.all re src))
      bundle
    |> List.sort_uniq String.compare
  in
  let entries = List.map (fun case -> case.entry) cases in
  let uncovered uses names =
    List.filter
      (fun name -> not (List.exists (fun entry -> uses name entry) entries))
      names
  in
  let directives =
    List.filter
      (fun name -> not (List.mem name css_at_rules))
      (named quoted_at_keyword_re)
  in
  check string_list "every directive the bundle handles has a case" []
    (uncovered uses_at_keyword directives);
  check string_list "every value function the bundle defines has a case" []
    (uncovered uses_function (named value_function_key_re))

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
    test_case "authored color-mix fallbacks" `Quick
      test_authored_color_mix_fallbacks;
    test_case "@apply hoists each property once" `Quick
      test_apply_hoists_each_property_once;
    test_case "property fallbacks in one block" `Quick
      test_property_fallbacks_in_one_block;
    test_case "declared utility keeps its nesting" `Quick
      test_declared_utility_keeps_its_nesting;
    test_case "complex custom variant keeps its candidate" `Quick
      test_complex_custom_variant_keeps_candidate;
    test_case "functional value" `Quick test_functional_value;
    test_case "functional property count order" `Quick
      test_functional_property_count_order;
    test_case "functional nested rule" `Quick test_functional_nested_rule;
    test_case "functional theme value" `Quick test_functional_theme_value;
    test_case "functional arbitrary value" `Quick
      test_functional_arbitrary_value;
    test_case "functional modifier" `Quick test_functional_modifier;
    test_case "functional routing" `Quick test_functional_routing;
    test_case "malformed utility spares the others" `Quick
      test_malformed_utility_spares_the_others;
    test_case "@apply declares every token it reads" `Quick
      test_apply_declares_every_token_it_reads;
    test_case "@apply keeps the keyframes" `Quick test_apply_keeps_the_keyframes;
    test_case "@apply of a declared utility" `Quick test_apply_declared_utility;
    test_case "prefix() on the import" `Quick test_import_prefix;
    test_case "dialect sweep against the CLI" `Slow test_dialect_sweep;
    test_case "dialect surface is covered" `Quick
      test_dialect_surface_is_covered;
    test_case "author CSS declares every token it reads" `Quick
      test_author_css_declares_every_token_it_reads;
  ]

let suite = ("entrypoint", tests)
