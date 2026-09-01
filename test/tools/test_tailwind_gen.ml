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
    let diff =
      Cascade_diff.Css_compare.diff ~mode:`Canonical
        ~prune_unused_custom_props:true cli tw
    in
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
  let diff =
    Cascade_diff.Css_compare.diff ~mode:`Canonical
      ~prune_unused_custom_props:true cli tw
  in
  match diff.Cascade_diff.Css_compare.result with
  | Cascade_diff.Css_compare.No_diff ->
      check bool (cls ^ ": tw matches live Tailwind CLI") true true
  | _ -> Alcotest.failf "%s: tw diverges from the live Tailwind CLI" cls

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
  ]

let suite = ("tailwind_gen", tests)
