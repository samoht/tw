(** Tailwind CSS generation utilities for testing - extracted from test_tw.ml *)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let tailwind_files ?(forms = false) ?input_css temp_dir classnames =
  (* Feed candidates to the extractor verbatim, space-separated, as raw file
     text rather than inside an HTML class attribute. An attribute forces
     escaping one quote style into an HTML entity, and Tailwind's extractor
     reads that entity literally into the selector (e.g. bg-[url('x')] ->
     .bg-\[url\(\&\#39\;x\&\#39\;\)\]), diverging from tw's selector. Raw text
     preserves both single and double quotes exactly as a real source file
     would, so arbitrary url() and content-["..."] values round-trip. *)
  let html_content = String.concat " " classnames in
  (* When the caller supplies a project CSS entrypoint, use it verbatim so the
     real Tailwind reads the project's @theme/@plugin/@config; otherwise
     synthesise the default import. *)
  let input_css_content =
    match input_css with
    | Some content -> content
    | None ->
        if forms then
          (* forms plugin with strategy: 'class' requires a config file *)
          "@import \"tailwindcss\";\n\
           @plugin \"@tailwindcss/typography\";\n\
           @config \"./tailwind.config.js\";"
        else "@import \"tailwindcss\";\n@plugin \"@tailwindcss/typography\";"
  in
  (* Generate tailwind.config.js when forms plugin is needed (only for the
     synthesised input; a supplied entrypoint carries its own config). *)
  (if forms && input_css = None then
     let config_content =
       {|import forms from '@tailwindcss/forms'

export default {
  plugins: [
    forms({ strategy: 'class' })
  ]
}
|}
     in
     write_file (Filename.concat temp_dir "tailwind.config.js") config_content);
  write_file (Filename.concat temp_dir "input.html") html_content;
  write_file (Filename.concat temp_dir "input.css") input_css_content

let availability_result = ref None
let tailwind_command = ref None

(* The reference build must be the exact Tailwind version tw tracks: a different
   release changes default tokens (e.g. the v4.2 -> v4.3 unit-spacing change,
   [calc(var(--spacing) * 1)] to a bare [var(--spacing)]), so an off-version
   binary yields a stale reference. We use a native tailwindcss only when it
   matches this version, and otherwise fall back to the pinned node_modules
   build. *)
let required_version = (4, 3, 3)

let parse_version v =
  let to_int s =
    let buf = Buffer.create 4 in
    String.iter (fun c -> if c >= '0' && c <= '9' then Buffer.add_char buf c) s;
    int_of_string_opt (Buffer.contents buf)
  in
  match String.split_on_char '.' v with
  | maj :: min :: rest -> (
      let patch = match rest with p :: _ -> p | [] -> "0" in
      match (to_int maj, to_int min, to_int patch) with
      | Some a, Some b, Some c -> Some (a, b, c)
      | _ -> None)
  | _ -> None

let tailwindcss_version cmd =
  (* Try --version first, fall back to --help if needed *)
  let temp_file = Filename.temp_file "tw_version" ".txt" in
  let version_cmd = cmd ^ " --version 2>/dev/null > " ^ temp_file in
  let exit_code = Sys.command version_cmd in
  let version_line, fallback_used =
    if exit_code = 0 then (
      let ic = open_in temp_file in
      let line = input_line ic in
      close_in ic;
      (line, false))
    else
      let help_cmd = cmd ^ " --help 2>&1 | head -1 > " ^ temp_file in
      let help_exit = Sys.command help_cmd in
      if help_exit = 0 then (
        let ic = open_in temp_file in
        let line = input_line ic in
        close_in ic;
        (line, true))
      else failwith "Failed to check tailwindcss version."
  in
  Sys.remove temp_file;
  (version_line, fallback_used)

let extract_version_number line =
  (* Extract version number from strings like "tailwindcss v4.0.0" or "4.0.0" *)
  let parts = String.split_on_char ' ' (String.trim line) in
  let version_candidates =
    List.filter
      (fun s ->
        let trimmed = String.trim s in
        String.length trimmed > 0
        && (Char.code trimmed.[0] >= Char.code '0'
            && Char.code trimmed.[0] <= Char.code '9'
           || String.length trimmed > 1
              && trimmed.[0] = 'v'
              && Char.code trimmed.[1] >= Char.code '0'
              && Char.code trimmed.[1] <= Char.code '9'))
      parts
  in
  match version_candidates with
  | [] -> None
  | v :: _ ->
      let clean_v =
        if String.length v > 0 && v.[0] = 'v' then
          String.sub v 1 (String.length v - 1)
        else v
      in
      Some clean_v

let version_string (a, b, c) =
  string_of_int a ^ "." ^ string_of_int b ^ "." ^ string_of_int c

let command_version cmd =
  let line, fallback_used = tailwindcss_version cmd in
  if fallback_used then None else extract_version_number line

(* The reference must be EXACTLY the pinned version: a different release, even a
   newer one, can change the emitted CSS and silently diverge from the snapshot
   fixtures (e.g. the v4.2 -> v4.3 unit-spacing change). *)
let command_is_required cmd =
  match command_version cmd with
  | Some v -> parse_version v = Some required_version
  | None -> false

let tailwindcss_command () =
  let have cmd = Sys.command ("which " ^ cmd ^ " > /dev/null 2>&1") = 0 in
  let native = have "tailwindcss" in
  if native && command_is_required "tailwindcss" then "tailwindcss"
  else if have "npx" && command_is_required "npx tailwindcss" then
    "npx tailwindcss"
  else
    let found =
      if native then
        match command_version "tailwindcss" with
        | Some v -> "v" ^ v
        | None -> "unknown version"
      else "not installed"
    in
    failwith
      ("Test setup failed: tailwindcss v"
      ^ version_string required_version
      ^ " is required (native: " ^ found
      ^ ").\nInstall it with: npm install -g @tailwindcss/cli@"
      ^ version_string required_version)

let check_tailwindcss_available () =
  match !availability_result with
  | Some (Ok ()) -> ()
  | Some (Error e) -> raise e
  | None -> (
      let result =
        try
          tailwind_command := Some (tailwindcss_command ());
          Ok ()
        with e -> Error e
      in
      availability_result := Some result;
      match result with Ok () -> () | Error e -> raise e)

let available () =
  try
    check_tailwindcss_available ();
    true
  with Failure _ -> false

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

let temp_dir () =
  (* Ensure tmp directory exists in project root *)
  if not (Sys.file_exists "tmp") then Unix.mkdir "tmp" 0o755;
  (* Create unique temp directory in project root so tailwindcss can resolve
     imports *)
  let dir = Filename.temp_file ~temp_dir:"tmp" "tw_gen_" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  dir

(** Detect if any class names use forms utilities (form-input, form-select,
    etc.) *)
let has_forms_class classnames =
  List.exists
    (fun cls -> String.length cls >= 5 && String.sub cls 0 5 = "form-")
    classnames

let generate ?(minify = false) ?(optimize = true) ?forms ?input_css classnames =
  check_tailwindcss_available ();

  let dir = temp_dir () in
  let cleanup () = ignore (Sys.command ("rm -rf " ^ Filename.quote dir)) in

  try
    let start_time = Stats.start_timer () in

    (* Auto-detect forms plugin usage if not explicitly specified *)
    let use_forms =
      match forms with
      | Some f -> f && classnames <> []
      | None -> has_forms_class classnames
    in
    tailwind_files ~forms:use_forms ?input_css dir classnames;

    let minify_flag = if minify then " --minify" else "" in
    let optimize_flag = if optimize then " --optimize" else "" in
    let tailwind_cmd =
      match !tailwind_command with Some cmd -> cmd | None -> "tailwindcss"
    in

    let cmd =
      Fmt.str
        "cd %s && %s -i input.css -o output.css --content input.html%s%s \
         2>/dev/null"
        dir tailwind_cmd minify_flag optimize_flag
    in

    let exit_code = Sys.command cmd in
    let elapsed = Unix.gettimeofday () -. start_time in
    Stats.record_call elapsed;

    if exit_code = 0 then (
      let output_file = Filename.concat dir "output.css" in
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
