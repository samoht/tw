(** Script to compare our generated CSS with Tailwind v4 *)

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content
  with _ -> None

let normalize_css css =
  (* Remove comments and normalize whitespace *)
  css
  |> Re.replace_string
       (Re.compile
          (Re.seq
             [ Re.str "/*"; Re.rep (Re.compl [ Re.char '*' ]); Re.str "*/" ]))
       ~by:""
  |> Re.replace_string (Re.compile (Re.rep1 Re.space)) ~by:" "
  |> String.trim

let compare_files our_file tw_file name =
  match (read_file our_file, read_file tw_file) with
  | Some our_css, Some tw_css ->
      let our_normalized = normalize_css our_css in
      let tw_normalized = normalize_css tw_css in
      if our_normalized = tw_normalized then (
        Printf.printf "  ✓ %s matches Tailwind v4\n" name;
        true)
      else (
        Printf.printf "  ✗ %s differs from Tailwind v4\n" name;
        Printf.printf "    To debug: diff %s %s\n" our_file tw_file;
        false)
  | None, _ ->
      Printf.printf "  ✗ Could not read %s\n" our_file;
      false
  | _, None ->
      Printf.printf "  ⚠ Tailwind CSS not generated for %s\n" name;
      true

let list_files dir pattern =
  try
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun f -> Re.execp (Re.compile (Re.Perl.re pattern)) f)
  with _ -> []

let get_base_name file =
  (* Extract base name from file, e.g., "simple_page.html" -> "simple_page" *)
  try
    let dot_pos = String.rindex file '.' in
    String.sub file 0 dot_pos
  with Not_found -> file

let find_matching_css html_file css_files =
  (* Find CSS file that matches the HTML file by base name *)
  let html_base = get_base_name html_file in
  List.find_opt
    (fun css_file ->
      let css_base = get_base_name css_file in
      css_base = html_base)
    css_files

let () =
  print_endline "Comparing generated CSS with Tailwind v4...";

  (* Check if Tailwind is available *)
  let has_tailwind =
    Sys.command
      "command -v npx >/dev/null 2>&1 && npx tailwindcss --version >/dev/null \
       2>&1"
    = 0
  in

  if not has_tailwind then (
    print_endline "  ⚠ Skipping comparison (Tailwind CSS not installed)";
    exit 0);

  (* Discover all HTML and CSS files in examples directory *)
  let html_files = list_files "examples" "\\.html$" in
  let css_files = list_files "examples" "\\.css$" in

  if html_files = [] then (
    print_endline "  ⚠ No HTML files found to compare";
    exit 0);

  let all_match = ref true in

  (* Process each HTML file *)
  List.iter
    (fun html_file ->
      match find_matching_css html_file css_files with
      | Some css_file ->
          let base = get_base_name html_file in
          let tw_css = base ^ "_tailwind.css" in

          (* Generate Tailwind CSS *)
          let cmd =
            Printf.sprintf
              "cd examples && npx tailwindcss --content './%s' -o %s \
               2>/dev/null"
              html_file tw_css
          in

          if Sys.command cmd = 0 then
            let result =
              compare_files ("examples/" ^ css_file) ("examples/" ^ tw_css) base
            in
            all_match := !all_match && result
          else (
            Printf.printf "  ✗ Failed to generate Tailwind CSS for %s\n" base;
            all_match := false)
      | None ->
          Printf.printf "  ⚠ No matching CSS file found for %s (skipping)\n"
            html_file)
    html_files;

  if !all_match then (
    print_endline "";
    print_endline "✓ All CSS outputs match Tailwind v4!")
  else (
    print_endline "";
    print_endline "✗ Some CSS outputs differ from Tailwind v4";
    exit 1)
