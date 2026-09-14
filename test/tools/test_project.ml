open Alcotest

(* The sheet a project compiles to, from an entrypoint written beside the other
   entrypoints these tests write. Nothing is scanned: the import fences its
   sources, so a class reaches the sheet from [classes] or from the entrypoint
   itself. *)
let compiled ?(classes = []) name body =
  let path = "project-" ^ name ^ ".css" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc "@import \"tailwindcss\" source(none);\n";
      output_string oc body);
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () ->
      let _, sheet =
        Tw_tools.Project.stylesheet ~theme:Tw.Scheme.default ~entrypoint:path
          ~base:false classes
      in
      Cascade.Css.to_string ~minify:true sheet)

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

let suite =
  ( "project",
    [
      test_case "@source inline" `Quick test_source_inline;
      test_case "@source inline braces" `Quick test_source_inline_braces;
      test_case "@source not inline" `Quick test_source_not_inline;
      test_case "@source inline scope" `Quick test_source_inline_scope;
    ] )
