(* The parity corpus: every entrypoint under [corpus/] compiled by tw and by the
   Tailwind CLI, the two sheets compared canonically, and both rendered over a
   page carrying the classes. Dropping a [NAME.css] into the directory is the
   whole act of adding a case; the rule that runs this reads the directory.

   A case is one Tailwind entrypoint, [@import "tailwindcss"] included, with
   whatever [@theme], [@custom-variant], [@utility], [@apply] or [@source
   inline("...")] the idiom under test needs. The page rendered is [NAME.html]
   beside it when there is one, and otherwise [Test_helpers.classes_page] over
   the classes its [@source inline] directives name, so a case that tests
   classes needs no markup of its own, and one that tests markup - a variant
   reading an ancestor's attribute, [@apply] inside a component - brings its
   page.

   Each case runs the documented command, [tw --input-css NAME.css PAGE --diff
   --html PAGE], so what is checked is what a reader of docs/parity.md would
   run: the canonical diff first, the render after it, exit 0 when both agree.
   Without node and a headless Chromium the render is skipped and the case is
   judged on the canonical diff alone, with a line saying so; TW_BROWSER_TESTS=1
   turns that into a failure, as TW_TAILWIND_TESTS=1 does for a missing CLI.

   Usage: corpus_check TW_BINARY DIR *)

module Entrypoint = Tw_tools.Entrypoint

let ( // ) = Filename.concat
let required name = Sys.getenv_opt name = Some "1"
let read_file path = In_channel.with_open_bin path In_channel.input_all

let write_file path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let cases dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".css")
  |> List.sort compare

(* The page a case renders over: its own, or one built from the classes its
   entrypoint safelists. *)
let page_of_case dir name css =
  let own = dir // (Filename.chop_suffix name ".css" ^ ".html") in
  if Sys.file_exists own then `Own own
  else
    match Entrypoint.source_inline css with
    | [], _ -> `None
    | classes, _ -> `Built (Test_helpers.classes_page classes)

(* [tw --input-css CSS PAGE --diff [--html PAGE]], its output kept for the
   report and its exit status read: 0 agrees, 1 differs, anything else did not
   compare. *)
let run_case ~tw ~render ~work dir name =
  let css_path = dir // name in
  let css = read_file css_path in
  let page =
    match page_of_case dir name css with
    | `Own path -> Some path
    | `Built html ->
        let path = work // (Filename.chop_suffix name ".css" ^ ".html") in
        write_file path html;
        Some path
    | `None -> None
  in
  match page with
  | None ->
      Fmt.pr
        "SKIP %s: no %s.html beside it and no @source inline to build one@."
        name
        (Filename.chop_suffix name ".css");
      `Skipped
  | Some page ->
      let out = work // (name ^ ".out") in
      let cmd =
        String.concat " "
          ([
             Filename.quote tw;
             "--input-css";
             Filename.quote css_path;
             Filename.quote page;
             "--diff";
           ]
          @ (if render then [ "--html"; Filename.quote page ] else [])
          @ [ ">"; Filename.quote out; "2>&1" ])
      in
      let status = Sys.command cmd in
      let report = read_file out in
      if status = 0 then (
        Fmt.pr "ok   %s%s@." name (if render then "" else " (canonical only)");
        `Ok)
      else (
        Fmt.pr "FAIL %s: exit %d@.%s@." name status report;
        `Failed)

let () =
  let tw, dir =
    match Sys.argv with
    | [| _; tw; dir |] -> (tw, dir)
    | _ ->
        prerr_endline "usage: corpus_check TW_BINARY DIR";
        exit 2
  in
  (match Tw_tools.Tailwind_gen.availability () with
  | Ok () -> ()
  | Error reason when required "TW_TAILWIND_TESTS" ->
      Fmt.epr "corpus_check: TW_TAILWIND_TESTS=1 but %s@." reason;
      exit 1
  | Error reason ->
      Fmt.pr "corpus_check: skipped, %s@." reason;
      exit 0);
  let render =
    match (Browser.node_binary (), Browser.chrome_binary ()) with
    | Some _, Some _ -> true
    | _ when required "TW_BROWSER_TESTS" ->
        Fmt.epr
          "corpus_check: TW_BROWSER_TESTS=1 but no node or headless Chromium@.";
        exit 1
    | _ ->
        Fmt.pr "corpus_check: no browser, the render is skipped@.";
        false
  in
  let work = Filename.get_temp_dir_name () // "tw-corpus" in
  (try Sys.mkdir work 0o755 with Sys_error _ -> ());
  let results = List.map (run_case ~tw ~render ~work dir) (cases dir) in
  let count r = List.length (List.filter (( = ) r) results) in
  Fmt.pr "corpus_check: %d case(s), %d ok, %d failed, %d skipped@."
    (List.length results) (count `Ok) (count `Failed) (count `Skipped);
  if results = [] then (
    Fmt.pr "FAIL: no case under %s@." dir;
    exit 1);
  if count `Failed > 0 then exit 1
