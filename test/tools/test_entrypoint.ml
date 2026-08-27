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
  check pair_list "the functional one is dropped"
    [ ("line-y", " border-block: 1px solid ") ]
    defs

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
    test_case "directives dropped" `Quick test_drop_directives;
    test_case "theme keyframes hoisted" `Quick test_hoist_theme_keyframes;
    test_case "@apply merges into one rule" `Quick test_apply_merges_one_rule;
  ]

let suite = ("entrypoint", tests)
