open Alcotest

(* The sheet a project compiles to, from an entrypoint written beside the other
   entrypoints these tests write. Nothing is scanned: the import fences its
   sources, so a class reaches the sheet from [classes] or from the entrypoint
   itself. *)
let compiled_with ?(base = false) ?(classes = []) ~import name body =
  let path = "project-" ^ name ^ ".css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc import;
      output_string oc body);
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      (* The theme is the one the entrypoint asks for, as the CLI builds it: an
         option on the import is part of what is being compiled. *)
      let theme =
        Tw_tools.Entrypoint.theme_of_css (Tw_tools.Entrypoint.read_file path)
      in
      let _, sheet =
        Tw_tools.Project.stylesheet ~theme ~entrypoint:path ~base classes
      in
      Cascade.Css.to_string ~minify:true sheet)

let compiled ?classes name body =
  compiled_with ?classes ~import:"@import \"tailwindcss\" source(none);\n" name
    body

let check_rules css ~present ~absent =
  List.iter
    (fun rule ->
      check bool ("emits " ^ rule) true
        (Astring.String.is_infix ~affix:rule css))
    present;
  List.iter
    (fun rule ->
      check bool ("omits " ^ rule) false
        (Astring.String.is_infix ~affix:rule css))
    absent

(* [@source inline] is Tailwind's safelist: the classes it names are generated
   whether or not any markup carries them. *)
let test_source_inline () =
  check_rules
    (compiled "inline" "@source inline(\"underline hover:bg-red-500\");\n")
    ~present:[ ".underline{"; ".hover\\:bg-red-500:hover{" ]
    ~absent:[]

(* A pattern expands the way the bundle's brace expansion does: a comma list,
   the empty alternative included, and a numeric range, stepped or descending.
   Each candidate is expanded on its own. *)
let test_source_inline_braces () =
  check_rules
    (compiled "braces"
       "@source inline(\"{hover:,}bg-red-{500,600} p-{0..4..2} mt-{3..1}\");\n")
    ~present:
      [
        ".bg-red-500{";
        ".bg-red-600{";
        ".hover\\:bg-red-500:hover{";
        ".hover\\:bg-red-600:hover{";
        ".p-0{";
        ".p-2{";
        ".p-4{";
        ".mt-1{";
        ".mt-2{";
        ".mt-3{";
      ]
    ~absent:[ ".p-1{"; ".p-3{" ]

(* [@source not inline] takes a class out of the build wherever it came from:
   the safelist beside it and the markup alike. *)
let test_source_not_inline () =
  check_rules
    (compiled ~classes:[ "m-2"; "m-4" ] "not-inline"
       "@source inline(\"p-2 p-3\");\n@source not inline(\"m-2 p-3\");\n")
    ~present:[ ".p-2{"; ".m-4{" ] ~absent:[ ".p-3{"; ".m-2{" ]

(* Only the directive's own argument is a safelist: a path source names files,
   and an [inline()] anywhere else is not the option. *)
let test_source_inline_scope () =
  check_rules
    (compiled "scope" "@source \"./nowhere\";\n.a { --x: inline(\"flex\"); }\n")
    ~present:[] ~absent:[ ".flex{" ]

(* [important] on the import marks every declaration a utility emits, whatever
   dresses it: a variant, the [!] suffix, an arbitrary property, a utility the
   project declares. The author's own CSS is not a utility and neither is what
   an [@apply] pulls into it, so both keep their declarations as written. *)
let test_import_important () =
  check_rules
    (compiled_with
       ~classes:
         [ "p-4"; "hover:underline"; "bg-blue-500!"; "[color:red]"; "card" ]
       ~import:"@import \"tailwindcss\" important source(none);\n" "important"
       "@utility card { tab-size: 8; }\n\
        .btn { @apply m-2; }\n\
        .x { color: blue; }\n")
    ~present:
      [
        ".p-4{padding:calc(var(--spacing)*4)!important}";
        ".hover\\:underline:hover{text-decoration-line:underline!important}";
        ".bg-blue-500\\!{background-color:var(--color-blue-500)!important}";
        ".\\[color\\:red\\]{color:red!important}";
        ".card{tab-size:8!important}";
        ".btn{margin:calc(var(--spacing)*2)}";
        ".x{color:";
      ]
    ~absent:
      [ "margin:calc(var(--spacing)*2)!important"; "color:blue!important" ]

(* The word is an option only where the import carries it. *)
let test_import_important_scope () =
  check_rules
    (compiled ~classes:[ "p-4" ] "important-scope" ".a { --flag: important; }\n")
    ~present:[ ".p-4{padding:calc(var(--spacing)*4)}" ]
    ~absent:[ "!important" ]

(* A [@theme static] block's tokens are declared whether or not anything reads
   them, which is what the modifier is for: a token JavaScript or an inline
   style reads at runtime has no reader in the sheet. An override of a default
   token is declared with the project's value, and a plain block beside it still
   declares only what is read. *)
let test_theme_static_block () =
  check_rules
    (compiled "theme-static"
       "@theme static { --color-brand: #123457; --spacing-huge: 10rem; \
        --color-red-500: #fe0102; }\n\
        @theme { --color-plain: #234568; }\n")
    ~present:
      [
        "--color-brand:#123457";
        "--spacing-huge:10rem";
        "--color-red-500:#fe0102";
      ]
    ~absent:[ "--color-plain" ]

(* A browser without [@property] never gives a registered property its initial
   value, so Tailwind declares each one in the same [@supports]-guarded [@layer
   properties] block it writes for its own: a non-inheriting property on every
   element, an inheriting one on the root, [initial] where the rule names no
   value. Only a top-level [@property] is shimmed, and the first of two with one
   name is the one that counts. *)
let test_author_property_fallback () =
  check_rules
    (compiled "author-property"
       "@property --a { syntax: \"<length>\"; inherits: false; initial-value: \
        0px; }\n\
        @property --b { syntax: \"<color>\"; inherits: true; initial-value: \
        red; }\n\
        @property --c { syntax: \"*\"; inherits: false; }\n\
        @property --e { syntax: \"<length>\"; inherits: false; initial-value: \
        1px; }\n\
        @property --e { syntax: \"<length>\"; inherits: false; initial-value: \
        2px; }\n\
        @layer components { @property --d { syntax: \"<length>\"; inherits: \
        false; initial-value: 0px; } }\n")
    ~present:
      [
        "@layer properties{";
        "--a:0px";
        ":root,:host{--b:red}";
        "--c:initial";
        "--e:1px";
      ]
    ~absent:[ "--e:2px"; "--d:" ]

(* [--theme()] reads a theme token from author CSS: a reference the theme layer
   then declares, with a fallback threaded into it; the value itself where the
   call says [inline] or stands where [var()] cannot, in a media query; and the
   fallback alone when the token does not exist. None of it is CSS, so a call
   left in place is a declaration the browser drops. *)
let test_dashed_theme_function () =
  check_rules
    (compiled "dashed-theme"
       "@theme { --color-brand: #123457; }\n\
        .a { color: --theme(--color-brand); }\n\
        .b { color: --theme(--color-red-500, #fe0102); }\n\
        .c { padding: calc(--theme(--spacing) * 2); }\n\
        .d { color: --theme(--color-nope, #fe0102); }\n\
        .e { width: --theme(--breakpoint-md inline); }\n\
        .g { color: --theme(--color-blue-500 inline); }\n\
        @media (width >= --theme(--breakpoint-md)) { .f { display: flex; } }\n")
    ~present:
      [
        ".a{color:var(--color-brand)}";
        "--color-brand:#123457";
        ".b{color:var(--color-red-500,#fe0102)}";
        "--color-red-500:";
        ".c{padding:calc(var(--spacing)*2)}";
        "--spacing:.25rem";
        ".d{color:#fe0102}";
        ".e{width:48rem}";
        ".g{color:oklch(";
        "@media(width>=48rem){.f{display:flex}}";
      ]
    ~absent:[ "--theme("; "--color-blue-500" ]

(* Each file under [tailwindcss/] is one part of the sheet, placed where it is
   imported, inside the layer [layer()] names or unlayered without one:
   [theme.css] the theme tokens a utility read, [utilities.css] and [@tailwind
   utilities] the utilities. A project importing those two asked for no
   preflight, so it gets neither the reset nor the font tokens only the reset
   reads, and no layer it did not declare. *)
let test_sub_imports () =
  let theme_import = "@import \"tailwindcss/theme.css\" layer(theme);\n" in
  check_rules
    (compiled_with ~classes:[ "p-4" ] "sub-layered"
       ~import:
         (theme_import
        ^ "@import \"tailwindcss/utilities.css\" layer(utilities) source(none);\n"
         )
       "")
    ~present:
      [
        "@layer theme{:root,:host{--spacing:.25rem}}";
        "@layer utilities{.p-4{padding:calc(var(--spacing)*4)}}";
      ]
    ~absent:[ "@layer base"; "@layer components"; "--font-sans"; "box-sizing" ];
  check_rules
    (compiled_with ~classes:[ "p-4" ] "sub-unlayered"
       ~import:
         "@import \"tailwindcss/theme.css\";\n\
          @import \"tailwindcss/utilities.css\" source(none);\n"
       "")
    ~present:
      [
        ":root,:host{--spacing:.25rem}"; ".p-4{padding:calc(var(--spacing)*4)}";
      ]
    ~absent:[ "@layer"; "--font-sans"; "box-sizing" ];
  check_rules
    (compiled_with ~classes:[ "p-4" ] "sub-tailwind-directive"
       ~import:(theme_import ^ "@tailwind utilities source(none);\n")
       "")
    ~present:
      [
        "@layer theme{:root,:host{--spacing:.25rem}}";
        ".p-4{padding:calc(var(--spacing)*4)}";
      ]
    ~absent:[ "@layer utilities"; "@layer base"; "--font-sans"; "box-sizing" ]

(* An [@theme reference] block says the tokens are declared somewhere else, so
   the sheet declares none of them, a default one included. A utility reading
   one, directly or through [@apply], carries the block's value as the fallback
   of its reference so it still resolves; the author's own [var()] is the
   author's and stays as written. *)
let test_theme_reference_block () =
  check_rules
    (compiled ~classes:[ "bg-brand" ] "theme-reference"
       "@theme reference { --color-brand: #123457; --color-red-500: #fe0102; }\n\
        .btn { color: var(--color-brand); }\n\
        .apply { @apply bg-red-500; }\n")
    ~present:
      [
        ".bg-brand{background-color:var(--color-brand,#123457)}";
        ".btn{color:var(--color-brand)}";
        ".apply{background-color:var(--color-red-500,#fe0102)}";
      ]
    ~absent:[ "--color-brand:"; "--color-red-500:" ]

(* [@reference "tailwindcss"] is what a component's own stylesheet starts with:
   the theme is in scope for [@apply] and none of it is emitted, so fifty
   components do not ship fifty copies of it. What an [@apply] pulls in carries
   each token's value as the fallback of its reference, variants included, so
   the rule resolves standalone. The author's own [var()] is the author's. *)
let test_reference_tailwindcss () =
  check_rules
    (compiled_with "reference-tailwindcss"
       ~import:"@reference \"tailwindcss\";\n"
       ".a { @apply text-lg; }\n\
        .b { @apply hover:bg-red-500 md:p-4; }\n\
        .x { color: var(--color-red-500); }\n")
    ~present:
      [
        "font-size:var(--text-lg,1.125rem)";
        "var(--text-lg--line-height,calc(";
        "background-color:var(--color-red-500,oklch(";
        "padding:calc(var(--spacing,.25rem)*4)";
        ".x{color:var(--color-red-500)}";
      ]
    ~absent:
      [
        "@layer theme";
        "@layer base";
        "@layer utilities";
        "--spacing:";
        "--text-lg:";
        "box-sizing";
      ]

let occurrences needle hay =
  let n = String.length needle and len = String.length hay in
  let rec go i acc =
    if i + n > len then acc
    else if String.sub hay i n = needle then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

(* [theme(static)] declares the whole theme once, in the generated sheet. An
   [@apply] renders its utilities on its own, and an expansion that carried the
   static theme along put another copy of every token, and of the default
   animations' keyframes, beside each rule that applied something: the
   tailwindcss.com sheet came out with nine theme blocks where the reference has
   one. *)
let test_static_theme_declared_once () =
  let css =
    compiled_with ~classes:[ "card" ] "static-once"
      ~import:"@import \"tailwindcss\" theme(static) source(none);\n"
      "@utility card { @apply p-4 rounded-lg; }\n\
       .a { @apply m-2; }\n\
       .b { @apply rounded-lg shadow-sm; }\n"
  in
  check int "one theme block" 1 (occurrences ":root,:host{" css);
  check int "one @keyframes spin" 1 (occurrences "@keyframes spin" css)

(* A class under a project's [@custom-variant] is routed around the generated
   sheet: its utility renders on its own and the variant wraps what comes back.
   That render carried the static theme too, so a [dark:] class brought a second
   copy of the default keyframes, as tailwindcss.com's [dark:*] classes did. *)
let test_static_theme_routed_once () =
  let css =
    compiled_with ~classes:[ "dark:p-4"; "dark:m-2" ] "static-routed"
      ~import:"@import \"tailwindcss\" theme(static) source(none);\n"
      "@custom-variant dark (&:where(.dark, .dark *));\n"
  in
  check int "one theme block" 1 (occurrences ":root,:host{" css);
  check int "one @keyframes spin" 1 (occurrences "@keyframes spin" css)

(* Tailwind applies a candidate's variants left to right, each putting the
   selector so far in its [&], so a declared variant sits where it was written
   among the built-in ones: [hover:dark:] is [.x:hover:where(...)] and
   [dark:hover:] is [.x:where(...):hover]. The routed block wrapped the declared
   variants outermost whatever their place, so both spelled
   [:where(...):hover]. *)
let test_declared_variant_keeps_its_place () =
  let css =
    compiled
      ~classes:
        [
          "hover:dark:text-white";
          "dark:hover:text-white";
          "first:dark:flex";
          "dark:before:flex";
          "dark:marker:text-red-500";
          "in-data-stack:dark:flex";
        ]
      "declared-place" "@custom-variant dark (&:where(.dark, .dark *));\n"
  in
  List.iter
    (fun selector ->
      check bool selector true (Astring.String.is_infix ~affix:selector css))
    [
      ".hover\\:dark\\:text-white:hover:where(.dark,.dark *)";
      ".dark\\:hover\\:text-white:where(.dark,.dark *):hover";
      ".first\\:dark\\:flex:first-child:where(.dark,.dark *)";
      ".dark\\:before\\:flex:where(.dark,.dark *):before";
      (* A built-in variant of several rules, [marker:]'s three, slots each of
         them; the probe declaration the template is derived from stays in
         none. *)
      ".dark\\:marker\\:text-red-500:where(.dark,.dark *)::marker{color:";
      ".dark\\:marker\\:text-red-500:where(.dark,.dark \
       *)::-webkit-details-marker{color:";
      (* A template whose [&] ends a longer selector keeps that selector. *)
      ":where([data-stack]) .in-data-stack\\:dark\\:flex:where(.dark,.dark *)";
    ];
  check bool "the probe declaration stays out" false
    (Astring.String.is_infix ~affix:"float:none" css);
  (* What the built-in prefix hoists, [before:]'s content registration, still
     arrives when every prefix is routed as a variant. *)
  check bool "before: still registers --tw-content" true
    (Astring.String.is_infix ~affix:"@property --tw-content" css)

(* Tailwind tries every utility registered for a root, so a candidate a
   project's functional declaration declines falls to the built-in one:
   [tab-[13]] under [@utility tab-* { tab-size: --value(integer) }] is the
   built-in [tab-*]'s, which takes the bracket. The declaration claimed every
   candidate of its root, and the bracket came out as nothing. *)
let test_functional_declaration_falls_back () =
  let css =
    compiled ~classes:[ "tab-4"; "tab-[13]" ] "functional-fallback"
      "@utility tab-* { tab-size: --value(integer); }\n"
  in
  check_rules css
    ~present:[ ".tab-4{tab-size:4}"; ".tab-\\[13\\]{tab-size:13}" ]
    ~absent:[]

(* A [!] on a declared utility marks its declarations [!important], under a
   variant and in the v3 prefix form alike, as Tailwind marks them. The mark
   stayed on the name, which named no declaration, so the class came out as
   nothing. *)
let test_declared_utility_important () =
  let css =
    compiled
      ~classes:[ "content-auto!"; "!content-auto"; "dark:content-auto!" ]
      "declared-important"
      "@utility content-auto { content-visibility: auto; }\n\
       @custom-variant dark (&:where(.dark, .dark *));\n"
  in
  check_rules css
    ~present:
      [
        ".content-auto\\!{content-visibility:auto!important}";
        ".\\!content-auto{content-visibility:auto!important}";
        ".dark\\:content-auto\\!:where(.dark,.dark \
         *){content-visibility:auto!important}";
      ]
    ~absent:[]

(* The bare [--*: initial] takes the [--default-*] tokens away. Tailwind spells
   each read of one [--theme(--default-<x>, <fallback>)]: preflight then writes
   the font stack and [normal] themselves, and the transition family the
   property's own initial value, [ease] and [0s], where tw kept referencing a
   token nothing declared. *)
let test_whole_theme_reset_resolves_the_defaults () =
  let css =
    compiled_with ~base:true
      ~classes:[ "transition"; "transition-colors" ]
      "whole-reset" ~import:"@import \"tailwindcss\" source(none);\n"
      "@theme { --*: initial; --spacing: 4px; }\n"
  in
  List.iter
    (fun affix -> check bool affix true (Astring.String.is_infix ~affix css))
    [
      "transition-timing-function:var(--tw-ease,ease)";
      "transition-duration:var(--tw-duration,0s)";
      "font-feature-settings:normal";
      "font-family:-apple-system,";
      "font-family:ui-monospace,";
    ];
  List.iter
    (fun affix ->
      check bool ("no " ^ affix) false (Astring.String.is_infix ~affix css))
    [ "--default-"; "--font-sans"; "--font-mono" ]

(* [@plugin "@tailwindcss/forms"] resets native form controls in the base layer
   unless its options ask for [strategy: "class"]. Tailwind 4.3.3 writes
   [input:where([type=text])] and the [select] rules there for the default
   strategy and for [strategy: "base"]; the plugin line never reached tw's
   sheet, so every input kept the browser's own look and nothing said so. *)
let test_forms_plugin_base () =
  let reset = "input:where([type=text])" in
  let forms name plugin =
    compiled_with ~base:true ~classes:[ "p-2" ] name
      ~import:"@import \"tailwindcss\" source(none);\n" plugin
  in
  let resets name plugin =
    Astring.String.is_infix ~affix:reset (forms name plugin)
  in
  check bool "default strategy resets inputs" true
    (resets "forms-default" "@plugin \"@tailwindcss/forms\";\n");
  check bool "base strategy resets inputs" true
    (resets "forms-base"
       "@plugin \"@tailwindcss/forms\" { strategy: \"base\"; }\n");
  check bool "class strategy leaves inputs" false
    (resets "forms-class"
       "@plugin \"@tailwindcss/forms\" { strategy: \"class\"; }\n");
  check bool "no plugin, no reset" false (resets "forms-none" "")

(* The forms reset is the plugin's, not preflight's: an entrypoint importing
   Tailwind in parts without [tailwindcss/preflight.css] still gets it from
   4.3.3, in [@layer base] and with no preflight beside it. tw built the reset
   into the base layer it writes only with preflight, and placed base content
   only for a preflight import, so the sheet had neither. *)
let test_forms_plugin_base_without_preflight () =
  let css =
    compiled_with ~base:true ~classes:[ "p-2" ] "forms-no-preflight"
      ~import:
        "@layer theme, base, components, utilities;\n\
         @import \"tailwindcss/theme.css\" layer(theme);\n\
         @import \"tailwindcss/utilities.css\" layer(utilities) source(none);\n"
      "@plugin \"@tailwindcss/forms\";\n"
  in
  check bool "the reset is written" true
    (Astring.String.is_infix ~affix:"input:where([type=text])" css);
  check bool "in the base layer" true
    (Astring.String.is_infix ~affix:"@layer base{" css);
  check bool "without preflight" false
    (Astring.String.is_infix ~affix:"box-sizing:border-box" css)

let suite =
  ( "project",
    [
      test_case "@source inline" `Quick test_source_inline;
      test_case "@source inline braces" `Quick test_source_inline_braces;
      test_case "@source not inline" `Quick test_source_not_inline;
      test_case "@source inline scope" `Quick test_source_inline_scope;
      test_case "important on the import" `Quick test_import_important;
      test_case "important elsewhere" `Quick test_import_important_scope;
      test_case "@theme static block" `Quick test_theme_static_block;
      test_case "author @property fallback" `Quick test_author_property_fallback;
      test_case "--theme() in author CSS" `Quick test_dashed_theme_function;
      test_case "tailwindcss sub-imports" `Quick test_sub_imports;
      test_case "@theme reference block" `Quick test_theme_reference_block;
      test_case "@reference tailwindcss" `Quick test_reference_tailwindcss;
      test_case "static theme declared once" `Quick
        test_static_theme_declared_once;
      test_case "static theme routed once" `Quick test_static_theme_routed_once;
      test_case "a declared variant keeps its place" `Quick
        test_declared_variant_keeps_its_place;
      test_case "a whole-theme reset resolves the defaults" `Quick
        test_whole_theme_reset_resolves_the_defaults;
      test_case "a functional declaration falls back" `Quick
        test_functional_declaration_falls_back;
      test_case "a declared utility takes the important mark" `Quick
        test_declared_utility_important;
      test_case "forms plugin base" `Quick test_forms_plugin_base;
      test_case "forms plugin base without preflight" `Quick
        test_forms_plugin_base_without_preflight;
    ] )
