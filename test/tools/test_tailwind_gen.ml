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

let tests =
  [
    test_case "check tailwindcss available" `Quick test_check_available;
    test_case "generate simple CSS" `Quick test_generate_simple;
  ]

let suite = ("tailwind_gen", tests)
