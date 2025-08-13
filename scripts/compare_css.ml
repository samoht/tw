(** Script to compare two CSS files Usage: compare_css.exe <css_file1>
    <css_file2> *)

open Fmt

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content
  with Sys_error msg ->
    epr "Error reading %s: %s@." path msg;
    None

let strip_tailwind_header css =
  (* Strip the Tailwind header comment if present *)
  let header_re =
    Re.compile
      (Re.seq
         [
           Re.str "/*! tailwindcss";
           Re.rep (Re.compl [ Re.char '*' ]);
           Re.str "*/";
         ])
  in
  Re.replace_string header_re ~by:"" css

let compare_css css1 css2 =
  (* Direct byte-for-byte comparison after trimming *)
  String.equal (String.trim css1) (String.trim css2)

let () =
  (* Parse command line arguments *)
  let args = Array.to_list Sys.argv |> List.tl in

  match args with
  | [ css_file1; css_file2 ] ->
      (* Check if files exist *)
      if not (Sys.file_exists css_file1) then (
        epr "Error: CSS file '%s' does not exist@." css_file1;
        exit 1);

      if not (Sys.file_exists css_file2) then (
        epr "Error: CSS file '%s' does not exist@." css_file2;
        exit 1);

      pr "Comparing %s with %s@." css_file1 css_file2;

      (* Read first CSS file *)
      let css1 =
        match read_file css_file1 with Some css -> css | None -> exit 1
      in

      (* Read second CSS file *)
      let css2 =
        match read_file css_file2 with Some css -> css | None -> exit 1
      in

      (* Strip Tailwind header comment from both files *)
      let css1 = strip_tailwind_header css1 in
      let css2 = strip_tailwind_header css2 in

      (* Compare *)
      let identical = compare_css css1 css2 in

      if identical then pr "✓ CSS files are identical@."
      else (
        pr "✗ CSS files differ@.";
        pr "@.To see the differences, run:@.";
        pr "  diff -u %s %s@." css_file1 css_file2);

      (* Exit with appropriate code *)
      exit (if identical then 0 else 1)
  | [ "--help" ] | [ "-h" ] ->
      pr "Usage: %s <css_file1> <css_file2>@.@." Sys.argv.(0);
      pr "Compare two CSS files for byte-for-byte equality.@.@.";
      pr "Arguments:@.";
      pr "  css_file1    First CSS file@.";
      pr "  css_file2    Second CSS file@.@.";
      pr "Note: Tailwind header comments are automatically stripped.@.@.";
      pr "Example:@.";
      pr "  %s examples/simple.css examples/simple.tailwind.css@." Sys.argv.(0);
      exit 0
  | _ ->
      epr "Error: Invalid arguments@.";
      epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      epr "Run '%s --help' for more information@." Sys.argv.(0);
      exit 1
