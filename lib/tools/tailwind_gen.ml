(** Tailwind CSS generation utilities for testing - extracted from test_tw.ml *)

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

(* A candidate reaches the CLI by one of two routes, and they answer different
   questions. An [@source inline] entry goes to the engine, which is what tw's
   own reader models. Text in a scanned file has to satisfy the candidate
   extractor first, and the extractor declines spellings the engine compiles: a
   [/name] opening on [-] or [_] is one, so [group-hover/-2a:underline] yields
   no rule from a file although [@apply] resolves it. A declined candidate costs
   the whole rule rather than changing a value, so a sheet built that way is
   short a rule that tw emits, and the comparison reads as a class tw invented.

   The scanned route answers what inline cannot. A [theme(--x)] read counts as a
   theme dependency only when the candidate came from a file, so a class reading
   one that way binds the token in [@layer theme] and the same class declared
   inline leaves it unbound. The spelling inlines the token's value and leaves
   no [var()] behind, so nothing else recovers the binding, and the comparison
   reads as a binding tw invented.

   Every candidate therefore takes both routes, which costs nothing because the
   engine compiles it once either way. [inline_quote] says which of them the
   inline string can hold at all: [{] and [}] are its expansion syntax, a [\]
   opens a string escape, and one carrying both quote characters fits inside
   neither string form. No class name Tailwind can produce is spelled that
   way. *)
let inline_quote candidate =
  let unusable = function '{' | '}' | '\\' -> true | _ -> false in
  if String.exists unusable candidate then None
  else if not (String.contains candidate '"') then Some '"'
  else if not (String.contains candidate '\'') then Some '\''
  else None

(* Every entrypoint the harness writes names the sources Tailwind may read,
   because the CLI decides for itself otherwise: an [@import "tailwindcss"]
   without [source(none)] scans the working directory, which under dune is the
   whole build tree and by hand is the checkout. A run that chooses its own
   sources reads tw's own output back in, so a reference built from an empty
   class list can arrive carrying thousands of selectors and a comparison
   against it measures nothing. The fence therefore belongs to the one function
   that spells an entrypoint rather than to each caller's command line.

   [project_css] is the single exception: a caller comparing against a real
   project supplies that project's own entrypoint, which carries its own source
   decisions and cannot be rewritten without changing what is being measured.
   Such a run needs its detection rooted some other way. *)
let entrypoint ?project_css ?(plugins = []) ?config ?(scanned_files = [])
    candidates =
  let head =
    match project_css with
    | Some css -> css
    | None -> "@import \"tailwindcss\" source(none);"
  in
  let plugin name = Fmt.str "@plugin \"%s\";" name in
  let config_line path = Fmt.str "@config \"%s\";" path in
  let scan path = Fmt.str "@source \"%s\";" path in
  let inline candidate =
    Option.map
      (fun quote -> Fmt.str "@source inline(%c%s%c);" quote candidate quote)
      (inline_quote candidate)
  in
  String.concat "\n"
    ((head :: List.map plugin plugins)
    @ Option.to_list (Option.map config_line config)
    @ List.map scan scanned_files
    @ List.filter_map inline candidates)
  ^ "\n"

(* [-s "p-4 flex"] hands the harness one entry holding two candidates, and both
   routes split it, so what either compiles is the whitespace split of what the
   caller passed. *)
let candidates classnames = List.concat_map Tw.split_whitespace classnames

(* What the inline string cannot hold reaches the CLI through the extractor and
   nowhere else, which is the one case where a dropped candidate costs a rule
   outright. *)
let scanned_candidates classnames =
  List.filter (fun c -> Option.is_none (inline_quote c)) (candidates classnames)

(* The generated directory holds one file per role, and the entrypoint names two
   of them, so the names are written once. *)
let scanned_file = "input.html"
let config_file = "tailwind.config.js"

let tailwind_files ?(forms = false) ?input_css temp_dir classnames =
  (* Candidates are fed to the extractor verbatim, space-separated, as raw file
     text rather than inside an HTML class attribute. An attribute forces
     escaping one quote style into an HTML entity, and Tailwind's extractor
     reads that entity literally into the selector (e.g. bg-[url('x')] ->
     .bg-\[url\(\&\#39\;x\&\#39\;\)\]), diverging from tw's selector. Raw text
     preserves both single and double quotes exactly as a real source file
     would. *)
  let candidates = candidates classnames in
  let html_content = String.concat " " candidates in
  let scanned_files = [ "./" ^ scanned_file ] in
  (* When the caller supplies a project CSS entrypoint, use it verbatim so the
     real Tailwind reads the project's @theme/@plugin/@config; otherwise
     synthesise the default import. *)
  let input_css_content =
    match input_css with
    | Some project_css -> entrypoint ~project_css ~scanned_files candidates
    | None ->
        (* The forms plugin with strategy: 'class' requires a config file. *)
        let config = if forms then Some ("./" ^ config_file) else None in
        entrypoint
          ~plugins:[ "@tailwindcss/typography" ]
          ?config ~scanned_files candidates
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
     write_file (Filename.concat temp_dir config_file) config_content);
  write_file (Filename.concat temp_dir scanned_file) html_content;
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

let rec dir_containing name dir =
  if Sys.file_exists (Filename.concat dir name) then Some dir
  else
    let parent = Filename.dirname dir in
    if String.equal parent dir then None else dir_containing name parent

let pinned_cli_relative = "node_modules/.bin/tailwindcss"

(* Scratch files stay inside a project, never in a system temp directory: that
   one is shared with every other user and run on the machine, and the CLI
   resolves [@import "tailwindcss"] against the nearest [node_modules] above the
   entrypoint, which a system directory has none of.

   The project is the tree the running executable was built in when it carries
   the pinned CLI, and the scratch root its [tmp/], which the checkout ignores.
   A worktree's binary is routinely invoked from another checkout, and a
   caller-relative root sends its generated entrypoints into that checkout
   instead.

   An installed binary has no such tree and writes into the caller's project, so
   it must not leave anything there. It uses the [.cache/tw] of the nearest
   [node_modules] above the working directory, where JavaScript tools keep their
   scratch files and which a project already ignores; resolution from there
   walks up to that same [node_modules]. With none above there is no project
   tree for the CLI to resolve against, and a hidden [.tw-scratch] in the
   working directory serves. Either way the directories tw creates are removed
   when it exits, as each file in them is removed after its run; a directory
   still holding another run's files is left to that run. *)
let executable_dir () =
  let self = Sys.executable_name in
  let self =
    if Filename.is_relative self then Filename.concat (Sys.getcwd ()) self
    else self
  in
  Filename.dirname self

(* Where scratch files go, and whether the directory outlives the run. *)
let scratch_root () =
  match dir_containing pinned_cli_relative (executable_dir ()) with
  | Some root -> `Checkout (Filename.concat root "tmp")
  | None -> (
      let cwd = Sys.getcwd () in
      match dir_containing "node_modules" cwd with
      | Some project ->
          `Project
            (List.fold_left Filename.concat project
               [ "node_modules"; ".cache"; "tw" ])
      | None -> `Project (Filename.concat cwd ".tw-scratch"))

let tmp_root () = match scratch_root () with `Checkout d | `Project d -> d

(* The directories tw created, innermost first, so that removing them in order
   empties each parent before it is reached. *)
let created_dirs = ref []

let remove_created_dirs () =
  List.iter
    (fun dir -> try Sys.rmdir dir with Sys_error _ -> ())
    !created_dirs;
  created_dirs := []

let rec mkdir_tracked dir =
  if not (Sys.file_exists dir) then begin
    mkdir_tracked (Filename.dirname dir);
    (* Another run may have created it between the two calls. *)
    match Sys.mkdir dir 0o755 with
    | () ->
        if !created_dirs = [] then at_exit remove_created_dirs;
        created_dirs := dir :: !created_dirs
    | exception Sys_error _ -> ()
  end

let tmp_file prefix suffix =
  let root =
    match scratch_root () with
    | `Checkout root ->
        (* The checkout's own [tmp/] outlives the run. Another test binary may
           have created it between the two calls. *)
        (if not (Sys.file_exists root) then
           try Sys.mkdir root 0o755 with Sys_error _ -> ());
        root
    | `Project root ->
        mkdir_tracked root;
        root
  in
  (* A concurrent run removes the directory once it holds nothing, so it may
     have gone between creating it here and the file going in. *)
  try Filename.temp_file ~temp_dir:root prefix suffix
  with Sys_error _ when not (Sys.file_exists root) ->
    mkdir_tracked root;
    Filename.temp_file ~temp_dir:root prefix suffix

let first_line path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> try Some (input_line ic) with End_of_file -> None)

(* The CLI names itself in the banner it puts at the top of every stylesheet it
   compiles, and it has no version flag: [--version] compiles a stylesheet like
   any other run. So the probe compiles the smallest thing there is, an
   entrypoint with no sources at all. An empty file will not do, because the
   scan comes from what [@import "tailwindcss"] pulls in rather than from the
   file's contents. *)
let version_probe_entrypoint = entrypoint []

let tailwindcss_version cmd =
  (* A command that exits 0 and prints nothing has answered nothing, so it
     counts as no answer and the [--help] fallback runs. *)
  let temp_file = tmp_file "tw_version" ".txt" in
  (* The entrypoint has to sit where the [tailwindcss] package resolves, which
     means inside the project: from a system temp directory the CLI exits 1 with
     no output. *)
  let entrypoint = tmp_file "tw_probe" ".css" in
  write_file entrypoint version_probe_entrypoint;
  let remove path = try Sys.remove path with Sys_error _ -> () in
  let cleanup () =
    remove temp_file;
    remove entrypoint
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let probe args =
    if Sys.command (cmd ^ args ^ temp_file) = 0 then first_line temp_file
    else None
  in
  match probe (" -i " ^ Filename.quote entrypoint ^ " -o - 2>/dev/null > ") with
  | Some line -> (line, false)
  | None -> (
      match probe " --help 2>&1 | head -1 > " with
      | Some line -> (line, true)
      | None -> failwith "Failed to check tailwindcss version.")

let extract_version_number line =
  (* Extract version number from strings like "tailwindcss v4.0.0" or "4.0.0" *)
  let digit c = c >= '0' && c <= '9' in
  let version_candidates =
    List.filter
      (fun s ->
        digit s.[0] || (String.length s > 1 && s.[0] = 'v' && digit s.[1]))
      (Tw.split_whitespace line)
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

(* What a candidate answered. A CLI that compiles the probe is usable, and its
   banner names its version; one that only answers [--help] is of a known
   version but cannot compile from here, which a global install is from any
   directory with no [node_modules] above it, since [@import "tailwindcss"]
   resolves against that tree. *)
type answer =
  | Absent
  | Compiles of string option
  | Help_only of string option
  | Silent

let probe_candidate cmd present =
  if not present then Absent
  else
    match tailwindcss_version cmd with
    | line, false -> Compiles (extract_version_number line)
    | line, true -> Help_only (extract_version_number line)
    | exception Failure _ -> Silent

(* The reference must be EXACTLY the pinned version: a different release, even a
   newer one, can change the emitted CSS and silently diverge from the snapshot
   fixtures (e.g. the v4.2 -> v4.3 unit-spacing change). *)
let is_required = function
  | Compiles (Some v) -> parse_version v = Some required_version
  | Compiles None | Help_only _ | Silent | Absent -> false

(* Naming the native binary alone reports "not installed" on a machine that
   reaches the CLI through npx, which is every CI runner and the case a lockfile
   bump breaks. A CLI that answered [--help] and compiled nothing is named with
   the entrypoint it refused, which is what the caller has to move. *)
let describe_candidate label = function
  | Absent -> label ^ ": not installed"
  | Compiles (Some v) -> label ^ ": v" ^ v
  | Compiles None | Silent -> label ^ ": unknown version"
  | Help_only version ->
      let version =
        match version with
        | Some v -> ": v" ^ v ^ " by its --help banner"
        | None -> ": answers --help"
      in
      label ^ version ^ ", but it did not compile the probe entrypoint under "
      ^ tmp_root ()

(* A program resolved the way opam resolves one, with no shell involved: a name
   holding a slash is that file, and a bare name is the first executable match
   in [PATH], a match without the execute bit skipped as the shell skips it.
   Asking a shell instead needs a [which] binary, and Arch Linux installs none,
   or [command -v], and dash answers that for any file named by path. *)
let resolve_command name =
  let executable path =
    (try not (Sys.is_directory path) with Sys_error _ -> false)
    &&
      try
        Unix.access path [ Unix.X_OK ];
        true
      with Unix.Unix_error _ -> false
  in
  if String.contains name '/' then if executable name then Some name else None
  else
    let path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
    List.find_map
      (fun dir ->
        if dir = "" then None
        else
          let candidate = Filename.concat dir name in
          if executable candidate then Some candidate else None)
      (String.split_on_char ':' path)

(* Worktrees can safely share an installed node_modules tree: the packages are
   read-only while tests run. Do not put npx between the harness and the pinned
   binary, because concurrent npx processes coordinate through npm's shared
   cache and can starve one another for minutes. *)
(* The caller's directory is searched first, and the tree the running executable
   was built in only when that finds nothing. The order is what lets a caller
   point tw at the CLI pinned beside the project being measured, which the
   option tests rely on to stand a stub in front of the real one; reversing it
   would make the binary's own pin unconditional and take that away.

   The fallback is for the case that used to fail outright: run from a directory
   with no [node_modules] above it, discovery found nothing and the run died
   reporting the pinned CLI "not installed", even though the binary was built in
   a tree carrying it. Note this is the opposite order from [tmp_root], which
   writes scratch files and must not put them in a checkout the caller merely
   happens to stand in. Either answer still has to satisfy
   [command_is_required], so a tree pinning the wrong version is declined rather
   than measured against. *)
let project_tailwindcss_command () =
  let relative = pinned_cli_relative in
  let root =
    match dir_containing relative (Sys.getcwd ()) with
    | Some root -> Some root
    | None -> dir_containing relative (executable_dir ())
  in
  match root with
  | None -> None
  | Some root ->
      Option.map Filename.quote
        (resolve_command (Filename.concat root relative))

(* Candidates in the order they are tried, and the search stops at the first
   usable one, so a project pin costs one probe and npx is never run behind it.
   The answers gathered on the way to a failure are the failure message. The
   project pin is labelled by its relative path, since the absolute one says
   nothing a reader of the message does not already know. *)
let tailwindcss_command () =
  let have cmd = Option.is_some (resolve_command cmd) in
  let project = project_tailwindcss_command () in
  let candidates =
    [
      ( pinned_cli_relative,
        Option.value ~default:pinned_cli_relative project,
        Option.is_some project );
      ("tailwindcss", "tailwindcss", have "tailwindcss");
      ("npx tailwindcss", "npx tailwindcss", have "npx");
    ]
  in
  let rec search answers = function
    | [] -> Error (List.rev answers)
    | (label, cmd, present) :: rest ->
        let answer = probe_candidate cmd present in
        if is_required answer then Ok cmd
        else search ((label, answer) :: answers) rest
  in
  match search [] candidates with
  | Ok cmd -> cmd
  | Error answers ->
      let required = version_string required_version in
      (* A global install of the right version is not what is missing, so
         suggesting one would send the caller round again. *)
      let advice =
        if
          List.exists
            (function
              | _, Help_only (Some v) -> parse_version v = Some required_version
              | _ -> false)
            answers
        then
          "Run tw from a project with tailwindcss@" ^ required
          ^ " installed: @import \"tailwindcss\" resolves against the nearest \
             node_modules above the entrypoint tw writes."
        else "Install it with: npm install -g @tailwindcss/cli@" ^ required
      in
      failwith
        ("tailwindcss v" ^ required ^ " is required ("
        ^ String.concat ", "
            (List.map
               (fun (label, answer) -> describe_candidate label answer)
               answers)
        ^ ").\n" ^ advice)

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

(* Establishing the reference build runs commands and reads their output, so it
   fails on a missing CLI and on an unusable filesystem alike; both mean the
   same thing here, that there is nothing to compare against. Generation is
   separate: once the CLI is known good, a [generate] that produces no CSS still
   raises. *)
let availability () =
  match check_tailwindcss_available () with
  | () -> Ok ()
  | exception Failure reason -> Error reason
  | exception Sys_error reason -> Error reason

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
  (* One directory per generation, in the project root so tailwindcss can
     resolve imports. *)
  let dir = tmp_file "tw_gen_" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  dir

(** Detect if any class names use forms utilities (form-input, form-select,
    etc.) *)
let has_forms_class classnames =
  List.exists (fun cls -> String.starts_with ~prefix:"form-" cls) classnames

(* What the CLI said is the reason a generation failed; the class list only says
   what was asked of it. *)
let failure_message ~errors classnames =
  let said =
    try String.trim (In_channel.with_open_bin errors In_channel.input_all)
    with Sys_error _ -> ""
  in
  String.concat ""
    [
      "Failed to generate Tailwind CSS";
      (if said = "" then "" else ": " ^ said);
      "\nfor classes: ";
      String.concat " " classnames;
    ]

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

    let output_file = Filename.concat dir "output.css" in

    (* The CLI's own [--cwd] rather than a shell [cd], and it is asked only to
       root automatic source detection: the entrypoint and the output are named
       absolutely, so what the run reads and writes does not depend on the
       flag's reach. The synthesised entrypoint has named its sources and needs
       no rooting at all, but a caller-supplied project entrypoint carries its
       own source decisions and may well leave detection on, and this is what
       keeps it off whatever directory the calling binary stands in. *)
    let errors = Filename.concat dir "stderr.txt" in
    let cmd =
      Fmt.str "%s --cwd %s -i %s -o %s%s%s 2>%s" tailwind_cmd
        (Filename.quote dir)
        (Filename.quote (Filename.concat dir "input.css"))
        (Filename.quote output_file)
        minify_flag optimize_flag (Filename.quote errors)
    in

    let exit_code = Sys.command cmd in
    let elapsed = Unix.gettimeofday () -. start_time in
    Stats.record_call elapsed;

    if exit_code = 0 then (
      let ic = open_in output_file in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      cleanup ();
      content)
    else
      let message = failure_message ~errors classnames in
      cleanup ();
      failwith message
  with e ->
    cleanup ();
    raise e

(* A project entrypoint is generated where it sits. Tailwind resolves its
   [@import]s, its [@source] and its [@plugin]s relative to the file, and
   [@import "tailwindcss"] against the nearest node_modules above it, so copying
   the file elsewhere breaks all three; only the output moves. An entrypoint
   that pins [source(none)] plus an explicit [@source] therefore scans exactly
   what it names, which is what keeps a comparison against tw from reading tw's
   own output back in. *)
let generate_entrypoint ?(minify = true) entry =
  check_tailwindcss_available ();
  let out = tmp_file "tw_entry" ".css" in
  let remove () = try Sys.remove out with Sys_error _ -> () in
  Fun.protect ~finally:remove @@ fun () ->
  let cmd =
    match !tailwind_command with Some c -> c | None -> "tailwindcss"
  in
  let start_time = Stats.start_timer () in
  let status =
    Fmt.kstr Sys.command "%s -i %s -o %s%s 2>/dev/null" cmd
      (Filename.quote entry) (Filename.quote out)
      (if minify then " --minify" else "")
  in
  Stats.record_call (Unix.gettimeofday () -. start_time);
  if status <> 0 then
    failwith ("Failed to generate Tailwind CSS from the entrypoint " ^ entry);
  let ic = open_in out in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))
