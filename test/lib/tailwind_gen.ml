(** Tailwind CSS generation utilities for testing - extracted from test_tw.ml *)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let tailwind_files temp_dir classnames =
  let html_content =
    Fmt.str
      {|<!DOCTYPE html>
<html>
<head></head>
<body>
  <div class="%s"></div>
</body>
</html>|}
      (String.concat " " classnames)
  in
  let input_css_content = "@import \"tailwindcss\";" in
  write_file (Filename.concat temp_dir "input.html") html_content;
  write_file (Filename.concat temp_dir "input.css") input_css_content

let version_checked = ref false

let check_tailwindcss_available () =
  if !version_checked then ()
  else (
    version_checked := true;
    let binary_check = Sys.command "which npx > /dev/null 2>&1" in
    if binary_check <> 0 then
      failwith "npx not found in PATH.\nPlease install Node.js and npm.";
    let temp_file = Filename.temp_file "tw_version" ".txt" in
    let version_cmd = "npx tailwindcss --help 2>&1 | head -1 > " ^ temp_file in
    let exit_code = Sys.command version_cmd in
    if exit_code = 0 then (
      let ic = open_in temp_file in
      let version_line = input_line ic in
      close_in ic;
      Sys.remove temp_file;
      if not (String.contains version_line '4') then
        failwith
          (Fmt.str
             "Expected Tailwind CSS v4.x but found: %s\n\
              Please install v4:\n\
              npm install -D tailwindcss"
             version_line))
    else failwith "Failed to check tailwindcss version.")

let generate ?(minify = false) classnames =
  check_tailwindcss_available ();
  let temp_dir = "temp_tailwind_test" in
  let cleanup () = ignore (Sys.command "rm -rf temp_tailwind_test") in
  try
    let _ = Sys.command (Fmt.str "mkdir -p %s" temp_dir) in
    tailwind_files temp_dir classnames;
    let minify_flag = if minify then " --minify" else "" in
    let cmd =
      Printf.sprintf
        "cd %s && npx tailwindcss -i input.css -o output.css --content \
         input.html%s --optimize 2>/dev/null"
        temp_dir minify_flag
    in
    let exit_code = Sys.command cmd in
    if exit_code = 0 then (
      let output_file = Filename.concat temp_dir "output.css" in
      let ic = open_in output_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      cleanup ();
      content)
    else (
      cleanup ();
      failwith
        ("Failed to generate Tailwind CSS for classes: "
        ^ String.concat " " classnames))
  with e ->
    cleanup ();
    raise e
