open Alcotest

(* The sheet a project compiles to, from an entrypoint written beside the other
   entrypoints these tests write. Nothing is scanned: the import fences its
   sources, so a class reaches the sheet from [classes] or from the entrypoint
   itself. *)
let compiled_with ?(classes = []) ~import name body =
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
        Tw_tools.Project.stylesheet ~theme ~entrypoint:path ~base:false classes
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
    ] )
