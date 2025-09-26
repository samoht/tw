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
  let input_css_content =
    "@import \"tailwindcss\";\n@plugin \"@tailwindcss/typography\";"
  in
  write_file (Filename.concat temp_dir "input.html") html_content;
  write_file (Filename.concat temp_dir "input.css") input_css_content

let availability_result = ref None
let tailwind_command = ref None

let check_tailwindcss_available () =
  match !availability_result with
  | Some (Ok ()) -> () (* Already checked and available *)
  | Some (Error e) -> raise e (* Already checked and failed *)
  | None -> (
      let result =
        try
          (* First try native tailwindcss (much faster) *)
          let native_check = Sys.command "which tailwindcss > /dev/null 2>&1" in
          let use_npx = native_check <> 0 in

          let cmd =
            if use_npx then (
              (* Fall back to npx if native not found *)
              let npx_check = Sys.command "which npx > /dev/null 2>&1" in
              if npx_check <> 0 then
                failwith
                  "Test setup failed: neither tailwindcss nor npx found in PATH.\n\
                   Please install tailwindcss directly or install Node.js and \
                   npm.";
              "npx tailwindcss")
            else "tailwindcss"
          in

          (* Store the command for future use *)
          tailwind_command := Some cmd;

          (* Log which command we're using *)
          if use_npx then
            Fmt.epr
              "Using npx tailwindcss (slower). For faster tests, install \
               native tailwindcss.@."
          else Fmt.epr "Using native tailwindcss (fast).@.";

          let temp_file = Filename.temp_file "tw_version" ".txt" in
          let version_cmd = cmd ^ " --help 2>&1 | head -1 > " ^ temp_file in
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
                   version_line);
            Ok ())
          else failwith "Failed to check tailwindcss version."
        with e -> Error e
      in
      availability_result := Some result;
      match result with Ok () -> () | Error e -> raise e)

(* Statistics tracking *)
module Stats = struct
  let total_time = ref 0.0
  let total_calls = ref 0
  let test_start_time = ref 0.0
  let start_timer () = Unix.gettimeofday ()

  let record_call elapsed_time =
    incr total_calls;
    total_time := !total_time +. elapsed_time

  let reset () =
    total_time := 0.0;
    total_calls := 0;
    test_start_time := Unix.gettimeofday ()

  let print_stats () =
    let total_test_time = Unix.gettimeofday () -. !test_start_time in
    Fmt.epr "@.=== Tailwind CSS Generation Statistics ===@.";

    (* Show which tailwindcss is being used *)
    (match !tailwind_command with
    | Some cmd when String.contains cmd ' ' ->
        Fmt.epr "Using: npx tailwindcss (slower)@."
    | Some _ -> Fmt.epr "Using: native tailwindcss (fast)@."
    | None -> Fmt.epr "Tailwindcss: not initialized@.");

    if !total_calls > 0 then (
      let avg_time = !total_time /. float_of_int !total_calls in
      let percentage = !total_time /. total_test_time *. 100.0 in
      Fmt.epr "Total calls: %d@." !total_calls;
      Fmt.epr "Time in tailwindcss: %.2fs@." !total_time;
      Fmt.epr "Total test time: %.2fs@." total_test_time;
      Fmt.epr "Percentage in tailwindcss: %.1f%%@." percentage;
      Fmt.epr "Average time per call: %.3fs@." avg_time)
    else Fmt.epr "No tailwindcss calls recorded@.";
    Fmt.epr "=========================================="
end

let with_stats f =
  Stats.reset ();
  match f () with
  | v ->
      Stats.print_stats ();
      v
  | exception exn ->
      Stats.print_stats ();
      raise exn

let generate ?(minify = false) ?(optimize = true) classnames =
  check_tailwindcss_available ();
  let temp_dir = "temp_tailwind_test" in
  let cleanup () = ignore (Sys.command "rm -rf temp_tailwind_test") in
  try
    let start_time = Stats.start_timer () in

    let _ = Sys.command (Fmt.str "mkdir -p %s" temp_dir) in
    tailwind_files temp_dir classnames;
    let minify_flag = if minify then " --minify" else "" in
    let optimize_flag = if optimize then " --optimize" else "" in
    let tailwind_cmd =
      match !tailwind_command with
      | Some cmd -> cmd
      | None -> "tailwindcss" (* Fallback, should be set by check *)
    in
    let cmd =
      Fmt.str
        "cd %s && %s -i input.css -o output.css --content input.html%s%s \
         2>/dev/null"
        temp_dir tailwind_cmd minify_flag optimize_flag
    in
    let exit_code = Sys.command cmd in

    let elapsed = Unix.gettimeofday () -. start_time in
    Stats.record_call elapsed;
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
