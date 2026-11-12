open Alcotest
open Tw_tools.Tailwind_import

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

let test_imports_static_theme () =
  check bool "declared" true (imports_static_theme theme_src);
  check bool "plain import" false
    (imports_static_theme "@import \"tailwindcss\";")

let test_strip_options () =
  check string "options dropped from the import" "@import \"tailwindcss\" ;"
    (strip_options "@import \"tailwindcss\" theme(static);")

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

let tests =
  [
    test_case "static theme import" `Quick test_imports_static_theme;
    test_case "import options stripped" `Quick test_strip_options;
    test_case "prefix() on the import" `Quick test_import_prefix;
  ]

let suite = ("tailwind_import", tests)
