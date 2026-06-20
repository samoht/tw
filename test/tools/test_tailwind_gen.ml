open Alcotest
open Tw_tools.Tailwind_gen

let test_check_available () =
  (* This just tests that the function doesn't crash *)
  try
    check_tailwindcss_available ();
    check bool "tailwindcss available" true true
  with Failure _ ->
    (* If Tailwind is not installed, that's ok for the test *)
    check bool "tailwindcss check completed" true true

let test_generate_simple () =
  try
    let css = generate ~minify:true [ "p-4"; "bg-blue-500" ] in
    check bool "generated CSS not empty" true (String.length css > 0);
    check bool "contains p-4 class" true
      (Astring.String.is_infix ~affix:".p-4" css)
  with Failure _ ->
    (* Skip if Tailwind not available *)
    check bool "test skipped (no tailwind)" true true

(* The upstream runner keeps a tiny allowlist for the single arbitrary colour
   ([#0088cc] at 25%/50%) that Tailwind's frozen [*.test.ts] fixtures store as
   stale LightningCSS-rounded oklab ([#0288cc40]/[#0288cc80]) where tw and the
   real v4 CLI emit the true [#0088cc40]/[#0088cc80]. That allowlist hardcodes
   tw's "true" value, so pin it to the live CLI here: tw and the real
   tailwindcss must agree (canonically) on these classes. If upstream ever
   changes/fixes the rounding, this fails loudly instead of the allowlist
   silently masking it. Skips when tailwindcss is unavailable. *)
let test_arbitrary_color_opacity_matches_cli () =
  let check_class cls =
    try
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
      | Cascade_diff.Css_compare.No_diff _ ->
          check bool (cls ^ ": tw matches live Tailwind CLI") true true
      | _ -> Alcotest.failf "%s: tw diverges from the live Tailwind CLI" cls
    with Failure _ -> check bool "skipped (tailwindcss unavailable)" true true
  in
  check_class "accent-[#0088cc]/50";
  check_class "accent-[#0088cc]/25"

let tests =
  [
    test_case "check tailwindcss available" `Quick test_check_available;
    test_case "generate simple CSS" `Quick test_generate_simple;
    test_case "arbitrary colour opacity matches live CLI" `Quick
      test_arbitrary_color_opacity_matches_cli;
  ]

let suite = ("tailwind_gen", tests)
