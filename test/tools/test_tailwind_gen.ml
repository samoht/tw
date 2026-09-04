open Alcotest
open Tw_tools.Tailwind_gen

(* Every test here needs the real CLI, so each opens on the gate: a missing tool
   skips, and TW_TAILWIND_TESTS=1 fails instead. A CLI that is present and fails
   to produce CSS raises [Failure] past the gate, and that must reach the
   runner: catching it would report a broken harness as a pass. *)
let test_check_available () =
  Test_helpers.require_tailwind_cli ();
  check_tailwindcss_available ();
  check bool "tailwindcss available" true true

let rec dir_containing name dir =
  if Sys.file_exists (Filename.concat dir name) then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else dir_containing name parent

(* Every worktree points node_modules at the same installed tree. Going through
   npx makes concurrent gates coordinate through npm's shared cache even though
   the exact executable is already present and immutable. *)
let test_prefers_project_tailwindcss () =
  let relative = "node_modules/.bin/tailwindcss" in
  let root =
    match dir_containing relative (Sys.getcwd ()) with
    | Some root -> root
    | None -> Alcotest.skip ()
  in
  let expected = Filename.quote (Filename.concat root relative) in
  check string "uses the pinned executable directly" expected
    (tailwindcss_command ())

let test_parity_preserves_author_custom_properties () =
  let diff =
    Tw_tools.Parity_compare.diff ~mode:`Canonical ".x { --author-token: yes }"
      ".x {}"
  in
  match diff.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff ->
      Alcotest.fail
        "parity comparison erased an author custom property missing from tw"
  | _ -> ()

let test_generate_simple () =
  Test_helpers.require_tailwind_cli ();
  let css = generate ~minify:true [ "p-4"; "bg-blue-500" ] in
  check bool "generated CSS not empty" true (String.length css > 0);
  check bool "contains p-4 class" true
    (Astring.String.is_infix ~affix:".p-4" css)

(* The upstream runner keeps a tiny allowlist for the single arbitrary colour
   ([#0088cc] at 25%/50%) that Tailwind's frozen [*.test.ts] fixtures store as
   stale LightningCSS-rounded oklab ([#0288cc40]/[#0288cc80]) where tw and the
   real v4 CLI emit the true [#0088cc40]/[#0088cc80]. That allowlist hardcodes
   tw's "true" value, so pin it to the live CLI here: tw and the real
   tailwindcss must agree (canonically) on these classes. If upstream ever
   changes/fixes the rounding, this fails loudly instead of the allowlist
   silently masking it. *)
let test_arbitrary_color_opacity_matches_cli () =
  Test_helpers.require_tailwind_cli ();
  let check_class cls =
    let cli = generate ~minify:true ~optimize:true ~forms:true [ cls ] in
    let tw =
      match Tw.of_string cls with
      | Ok u ->
          Tw.to_css ~base:true [ u ]
          |> Tw.Css.optimize ~prune_unused_custom_props:true
          |> Tw.Css.to_string ~minify:true
      | Error _ -> Alcotest.failf "tw could not parse %s" cls
    in
    let diff = Tw_tools.Parity_compare.diff ~mode:`Canonical cli tw in
    match diff.Cascade_diff.Css_compare.result with
    | Cascade_diff.Css_compare.No_diff ->
        check bool (cls ^ ": tw matches live Tailwind CLI") true true
    | _ -> Alcotest.failf "%s: tw diverges from the live Tailwind CLI" cls
  in
  check_class "accent-[#0088cc]/50";
  check_class "accent-[#0088cc]/25"

(* Regression: candidates are fed to the real CLI verbatim, not inside an
   escaped HTML class attribute. The attribute forced single quotes to the HTML
   entity [&#39;], which the extractor read literally into the selector
   ([.bg-\[url\(\&\#39\;...\)\]]), diverging from tw's [.bg-\[url\(\'...\'\)\]].
   Arbitrary url() values with single quotes must round-trip with no entity
   mangling. *)
let test_arbitrary_url_matches_cli () =
  Test_helpers.require_tailwind_cli ();
  let cls = "bg-[url('/img/x.svg')]" in
  let cli = generate ~minify:true ~optimize:true [ cls ] in
  check bool
    (cls ^ ": CLI reference is not HTML-entity mangled")
    false
    (Astring.String.is_infix ~affix:"&#39;" cli);
  let tw =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~base:true [ u ]
        |> Tw.Css.optimize ~prune_unused_custom_props:true
        |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "tw could not parse %s" cls
  in
  let diff = Tw_tools.Parity_compare.diff ~mode:`Canonical cli tw in
  match diff.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff ->
      check bool (cls ^ ": tw matches live Tailwind CLI") true true
  | _ -> Alcotest.failf "%s: tw diverges from the live Tailwind CLI" cls

(* The reference sheet is generated from a class list, and the route that list
   takes decides what the CLI can answer with. Text in a source file has to
   survive Tailwind's candidate extractor first, and the extractor declines
   spellings the engine compiles: [@apply group-hover/-2a:underline] resolves,
   while the same class written into a scanned file yields nothing. Extraction
   failure costs the whole rule rather than changing a value, so the comparison
   reports a class Tailwind supports as tw's invention. *)
let test_extractor_hostile_class_matches_cli () =
  Test_helpers.require_tailwind_cli ();
  let cls = "group-hover/-2a:underline" in
  let cli = generate ~minify:true ~optimize:true [ cls ] in
  let tw =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~base:true [ u ]
        |> Tw.Css.optimize ~prune_unused_custom_props:true
        |> Tw.Css.to_string ~minify:true
    | Error _ -> Alcotest.failf "tw could not parse %s" cls
  in
  let diff = Tw_tools.Parity_compare.diff ~mode:`Canonical cli tw in
  match diff.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff ->
      check bool (cls ^ ": tw matches live Tailwind CLI") true true
  | _ -> Alcotest.failf "%s: tw diverges from the live Tailwind CLI" cls

(* The inline route carries every class name Tailwind can produce, quotes and
   all, because the quote style is chosen per candidate. What it cannot carry
   keeps the extractor, and naming that residue is what stops a comparison from
   mixing the two oracles without saying so. *)
let test_scanned_candidates_names_the_residue () =
  check (list string) "a class Tailwind can produce goes inline" []
    (scanned_candidates
       [
         "p-4";
         "group-hover/-2a:underline";
         {|content-["hello_world"]|};
         "data-[foo$='bar'_i]:flex";
         "bg-[url('/img/x.svg')]";
       ]);
  check (list string) "brace expansion and both quote styles keep the extractor"
    [ "p-{1,2}"; {|content-['"x"']|} ]
    (scanned_candidates [ "p-4"; "p-{1,2}"; {|content-['"x"']|}; "underline" ])

let with_cwd dir f =
  let saved = Sys.getcwd () in
  Sys.chdir dir;
  Fun.protect ~finally:(fun () -> Sys.chdir saved) f

(* The harness is reached from wherever the calling binary happens to stand:
   dune runs the suites from inside [_build], [tw] runs from whatever directory
   the command was typed in, and a worktree's executable is routinely invoked
   from the shared checkout. None of that may reach the sheet, and two things
   carry it there: an entrypoint that leaves Tailwind to choose its own sources,
   which is how a probe compiling nothing once returned the whole repository,
   and a scratch root resolved against the caller, which puts the entrypoint
   where [@import "tailwindcss"] has no package to resolve against. *)
let test_generate_ignores_the_working_directory () =
  Test_helpers.require_tailwind_cli ();
  (* Settle which executable answers before moving: that is a separate
     working-directory question, and pinning it keeps this test on the sheet. *)
  check_tailwindcss_available ();
  let classes = [ "p-4"; "underline" ] in
  let here = generate ~minify:true classes in
  check bool "the reference sheet carries the classes asked for" true
    (Astring.String.is_infix ~affix:".p-4" here
    && Astring.String.is_infix ~affix:".underline" here);
  let elsewhere =
    with_cwd (Filename.get_temp_dir_name ()) (fun () ->
        generate ~minify:true classes)
  in
  check string "the working directory does not reach the sheet" here elsewhere

let test_entrypoint_names_its_sources () =
  check string "the version probe names no source at all"
    "@import \"tailwindcss\" source(none);\n" (entrypoint []);
  check string "the generated entrypoint names each source once"
    "@import \"tailwindcss\" source(none);\n\
     @plugin \"@tailwindcss/typography\";\n\
     @config \"./tailwind.config.js\";\n\
     @source \"./input.html\";\n\
     @source inline(\"p-4\");\n"
    (entrypoint
       ~plugins:[ "@tailwindcss/typography" ]
       ~config:"./tailwind.config.js" ~scanned_files:[ "./input.html" ]
       [ "p-4"; "p-{1,2}" ]);
  check string "a project entrypoint keeps its own head"
    "@import \"tailwindcss\";\n@source \"./input.html\";\n"
    (entrypoint ~project_css:"@import \"tailwindcss\";"
       ~scanned_files:[ "./input.html" ] [])

(* The CLI resolves [@import "tailwindcss"] against the nearest node_modules
   above the entrypoint, so a scratch directory outside the project is no
   scratch directory at all. *)
let in_project_tmp_dir f =
  let root =
    match dir_containing "node_modules" (Sys.getcwd ()) with
    | Some root -> root
    | None -> Alcotest.skip ()
  in
  let tmp = Filename.concat root "tmp" in
  if not (Sys.file_exists tmp) then Sys.mkdir tmp 0o755;
  let dir = Filename.temp_file ~temp_dir:tmp "tw_fence" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote dir)))
    (fun () -> f dir)

(* [source(none)] is what stops the CLI choosing sources for itself, and this is
   the failure it prevents: compiled from the directory it sits in, an
   entrypoint must read the file it names and not the one beside it. *)
let test_entrypoint_fences_source_detection () =
  Test_helpers.require_tailwind_cli ();
  check_tailwindcss_available ();
  in_project_tmp_dir @@ fun dir ->
  let write name content =
    let oc = open_out (Filename.concat dir name) in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc content)
  in
  write "named.txt" "underline";
  write "unnamed.html" "text-red-500";
  write "entry.css" (entrypoint ~scanned_files:[ "./named.txt" ] [ "p-4" ]);
  let css =
    with_cwd dir (fun () ->
        generate_entrypoint (Filename.concat dir "entry.css"))
  in
  check bool "the inline candidate reaches the engine" true
    (Astring.String.is_infix ~affix:".p-4" css);
  check bool "the named file reaches the extractor" true
    (Astring.String.is_infix ~affix:".underline" css);
  check bool "a file the entrypoint does not name is left alone" false
    (Astring.String.is_infix ~affix:".text-red-500" css)

let tests =
  [
    test_case "check tailwindcss available" `Quick test_check_available;
    test_case "prefer the project Tailwind executable" `Quick
      test_prefers_project_tailwindcss;
    test_case "parity preserves author custom properties" `Quick
      test_parity_preserves_author_custom_properties;
    test_case "generate simple CSS" `Quick test_generate_simple;
    test_case "arbitrary colour opacity matches live CLI" `Quick
      test_arbitrary_color_opacity_matches_cli;
    test_case "arbitrary url() round-trips through the CLI harness" `Quick
      test_arbitrary_url_matches_cli;
    test_case "a class the extractor declines matches the live CLI" `Quick
      test_extractor_hostile_class_matches_cli;
    test_case "the extractor residue is named" `Quick
      test_scanned_candidates_names_the_residue;
    test_case "generation ignores the working directory" `Quick
      test_generate_ignores_the_working_directory;
    test_case "the entrypoint names its sources" `Quick
      test_entrypoint_names_its_sources;
    test_case "the entrypoint fences source detection" `Quick
      test_entrypoint_fences_source_detection;
  ]

let suite = ("tailwind_gen", tests)
