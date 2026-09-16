module Css = Cascade.Css
module Entrypoint = Tw_tools.Entrypoint
open Cascade_diff
open Cmdliner

(* Probe spans for [obs run], on the [add-observe] branch only. *)
let span name = Probe.span name Probe.Fields.unit
let with_span span f = Probe.with_span span () f
let scan_span = span "tw.scan"
let theme_span = span "tw.entrypoint.theme"
let prefixes_span = span "tw.cascade.optimize_or_prefix"
let print_span = span "tw.cascade.print"

(* Parse a whitespace-separated string of classes *)
let parse_classes ?(warn = true) ?(theme = Tw.Scheme.default) classes_str =
  let class_names = Tw_tools.Source_scan.split_whitespace classes_str in
  List.filter_map
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok style -> Some style
      | Error (`Msg msg) ->
          (* The parser says why - a v3 spelling, a malformed arbitrary property
             - and repeating "Unknown class" here threw that away. *)
          if warn then Fmt.epr "Warning: %s@." msg;
          None)
    class_names

let ignored_scan_entry name =
  name = "_build" || name = "node_modules" || name = ".git"
  || String.starts_with ~prefix:"." name

let scan_warning path message =
  Fmt.epr "Warning: cannot scan %s: %s@." path message

(* Recursively get content files without following directory symlinks or
   descending into generated/dependency/metadata trees. A bad subtree is local
   to that path: readable siblings still contribute their candidates. *)
let rec files path keep =
  let regular_file () = if keep path then [ path ] else [] in
  try
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.to_list
        |> List.filter (fun entry -> not (ignored_scan_entry entry))
        |> List.concat_map (fun entry ->
            files (Filename.concat path entry) keep)
    | Unix.S_LNK -> (
        match (Unix.stat path).st_kind with
        | Unix.S_DIR -> []
        | _ -> regular_file ())
    | _ -> regular_file ()
  with
  | Sys_error message ->
      scan_warning path message;
      []
  | Unix.Unix_error (error, _, _) ->
      scan_warning path (Unix.error_message error);
      []

(* Generation backend - determines which tool to use *)
type backend =
  | Native (* Use our tw implementation *)
  | Tailwind (* Use real tailwindcss tool *)
  | Diff (* Compare tw vs tailwindcss *)

(* Main command implementation *)
type gen_opts = {
  minify : bool;
  optimize : bool;
  quiet : bool;
  css_mode : Tw.Css.mode;
  backend : backend;
  theme : Tw.Scheme.t;
      (** Theme used by tw's renderer, built from the project's --input-css so a
          --diff over a real repo compares against the same [@theme] Tailwind
          uses. Defaults to {!Tw.Scheme.default}. *)
  input_css : string option;
      (** Path to the project's CSS entrypoint, fed verbatim to the real
          Tailwind backend so both sides share the project config. *)
  input_css_path : string option;
      (** The entrypoint's own path, so tw can compile it (its rules and its
          relative [@import]s), not just read its [@theme]. *)
  diff_mode : Cascade_diff.Css_compare.mode;
      (** Comparison mode for --diff. [`Canonical] (default) ignores selector
          regrouping/reordering and is right for real-world parity sweeps;
          [`Auto]/[`Tree] (structural) reports regrouping, for tests that target
          it. *)
  output : string option;
      (** The file [-o] names, or standard output for [None] and ["-"]. *)
  html : string option;
      (** The document [--diff --html] renders both sheets over. *)
}

(* The entrypoint the Tailwind reference compiles. It is written into a scratch
   directory, so its relative paths are rooted where the entrypoint sits. *)
let reference_entrypoint ~(opts : gen_opts) =
  match (opts.input_css_path, opts.input_css) with
  | Some path, Some css ->
      let dir = Filename.dirname path in
      let dir =
        if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir
        else dir
      in
      Some (Entrypoint.rooted ~dir css)
  | _, css -> css

let eval_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    try Sys.mkdir dir 0o755 with Sys_error _ when Sys.file_exists dir -> ()
  end

(* The sheet goes to the file [-o] names, its directory created first as
   Tailwind's CLI creates it, or to standard output. *)
let emit ~(opts : gen_opts) css =
  match opts.output with
  | None | Some "-" -> print_string css
  | Some path ->
      mkdir_p (Filename.dirname path);
      Out_channel.with_open_bin path (fun oc -> output_string oc css)

(* The reference sheet hands each class to Tailwind's engine and to its source
   extractor both, so the two sides answer the same question. A class the
   [@source inline] string cannot hold has only the extractor, and the extractor
   drops what it cannot read, so a rule missing on the Tailwind side may be the
   harness rather than tw. Name those classes: a comparison whose provenance is
   unknown is worse than one that is merely narrower. *)
let print_oracle_note classes =
  match Tw_tools.Tailwind_gen.scanned_candidates classes with
  | [] -> ()
  | scanned ->
      Fmt.pr
        "Note: %s reached Tailwind through its source extractor alone, which \
         drops a candidate it cannot read rather than compiling it.@."
        (String.concat ", " scanned)

(* The exit status a comparison answers with, so a CI job can gate on it: 0 when
   the two sheets are equivalent, 1 when they differ, 2 when one of them could
   not be read and so nothing was compared. *)
let print_diff_result label diff =
  match diff.Css_compare.result with
  | Css_compare.No_diff ->
      Fmt.pr "✓ No differences found%s@." label;
      0
  | result -> (
      Fmt.pr "Differences found%s:@.@." label;
      let buf = Buffer.create 256 in
      Css_compare.pp ~expected:"Tailwind" ~actual:"tw" buf diff;
      print_string (Buffer.contents buf);
      Fmt.pr "@.";
      match result with
      | Css_compare.Both_errors _ | Expected_error _ | Actual_error _ -> 2
      | Tree_diff _ | String_diff _ | No_diff -> 1)

(* The generator reports a missing or unusable CLI as a [Failure] carrying the
   whole diagnosis, and [Printexc] would print that as a quoted literal with its
   newlines escaped. *)
let tailwind_error = function
  | Failure reason | Sys_error reason -> reason
  | e -> Printexc.to_string e

let calc_re = Re.compile (Re.str "calc(")

let render_css ~(opts : gen_opts) stylesheet =
  let stylesheet =
    match opts.css_mode with
    | Inline ->
        (* The spacing token is a runtime override point and its references stay
           live by default; inline mode is the request to resolve them, and the
           arithmetic a substitution leaves static, [calc(.25rem * 4)], is
           resolved with them. Only a declaration holding a [calc()] with no
           variable left is folded, and only exactly: the pass is not a
           minifier, so a colour or a keyword keeps the spelling it was
           generated with. *)
        let resolved = Tw.Css.inline_vars ~inline_runtime:true stylesheet in
        let static_calc d =
          Tw.Css.vars_of_declarations [ d ] = []
          && Re.execp calc_re (Tw.Css.declaration_value ~minify:true d)
        in
        let fold d =
          if static_calc d then Tw.Css.Declaration.normalize ~lossless:true d
          else d
        in
        let fold_rule selector decls =
          Tw.Css.rule ~selector (List.map fold decls)
        in
        Tw.Css.v (Tw.Css.map fold_rule (Tw.Css.statements resolved))
    | Variables -> stylesheet
  in
  let stylesheet =
    with_span prefixes_span @@ fun () ->
    if opts.optimize then
      (* Custom properties are an open runtime API: JavaScript, inline styles,
         and separately loaded sheets can read declarations that have no local
         var() reference. *)
      Tw.Css.optimize stylesheet
    else
      (* Prefixing is an output compatibility contract, independent of the
         structural optimizations controlled by [--optimize]. *)
      Tw.Css.Optimize.add_compatibility_prefixes
        ~targets:Tw.Css.Optimize.evergreen_targets stylesheet
  in
  (* A prefixed project spells its theme tokens [--tw-spacing], on the
     declaration and at every [var()] alike. *)
  let rename_custom_property = Tw.theme_token_rename ~theme:opts.theme in
  with_span print_span (fun () ->
      Tw.Css.to_string ~minify:opts.minify ?rename_custom_property stylesheet)

(* Surface of_string's specific message (e.g. the actionable arbitrary-property
   feedback) for a single unknown class; fall back to a generic message. *)
let unknown_class_error ~theme class_str =
  match Tw.of_string ~theme class_str with
  | Error (`Msg m) -> Fmt.str "Error: %s" m
  | Ok _ -> Fmt.str "Error: Unknown class: %s" class_str

(* The sheet for the class string of [-s]. With an entrypoint, a utility it
   declares is routed the way the scanning form routes it, and none of the
   entrypoint's own CSS is spliced in: this path answers for the classes alone.
   [None] when nothing reads any of them. *)
let single_class_sheet ~(opts : gen_opts) ~base class_str =
  match opts.input_css_path with
  | None -> (
      match parse_classes ~warn:false ~theme:opts.theme class_str with
      | [] when class_str <> "" -> None
      | styles -> Some (Tw.to_css ~theme:opts.theme ~base styles))
  | Some _ as entrypoint ->
      let count, sheet =
        Tw_tools.Project.utilities ~theme:opts.theme ?entrypoint ~base
          (Tw_tools.Source_scan.split_whitespace class_str)
      in
      if count = 0 && class_str <> "" then None else Some sheet

(* A class the document does not carry is refused before either sheet is
   compiled: the browser would compare it on no element. *)
let uncovered_refusal ~(opts : gen_opts) classes =
  match opts.html with
  | None -> None
  | Some path -> (
      let html = Entrypoint.read_file path in
      match Tw_tools.Parity_compare.uncovered ~html classes with
      | [] -> None
      | missing ->
          Some
            (String.concat ""
               [
                 "no element of ";
                 path;
                 " carries ";
                 String.concat ", " missing;
                 ", so the browser would compare nothing for it";
               ]))

(* The browser half of [--diff --html]: both sheets rendered over the document,
   reported after the canonical comparison. It runs whatever the canonical
   comparison said, so machinery the two sheets share cannot hide a difference
   from it. *)
let browser_verdict ~(opts : gen_opts) ~classes ~tailwind ~tw =
  match opts.html with
  | None -> 0
  | Some path -> (
      let html = Entrypoint.read_file path in
      match Tw_tools.Parity_compare.browser ~html ~classes ~tailwind ~tw with
      | Error reason ->
          Fmt.epr "Error: %s@." reason;
          2
      | Ok report ->
          print_string
            (Browser_compare.to_string ~first:"Tailwind" ~second:"tw" ~html:path
               report);
          if report.differences = [] then 0 else 1)

let diff_single_class class_str ~(opts : gen_opts) =
  let classes = Tw_tools.Source_scan.split_whitespace class_str in
  match uncovered_refusal ~opts classes with
  | Some reason ->
      Fmt.epr "Error: %s@." reason;
      `Ok 2
  | None -> (
      try
        let legacy_css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true
            ?input_css:(reference_entrypoint ~opts)
            [ class_str ]
        in
        match single_class_sheet ~opts ~base:true class_str with
        | None -> `Error (false, unknown_class_error ~theme:opts.theme class_str)
        | Some stylesheet ->
            let our_css = render_css ~opts stylesheet in
            let diff =
              Tw_tools.Parity_compare.diff ~mode:opts.diff_mode legacy_css
                our_css
            in
            let code =
              if class_str = "" then print_diff_result " (empty/base only)" diff
              else (
                print_oracle_note [ class_str ];
                print_diff_result
                  (Fmt.str " between Tailwind and tw for '%s'" class_str)
                  diff)
            in
            (* The exit statuses rank by gravity, so the graver of the two
               wins. *)
            `Ok
              (max code
                 (browser_verdict ~opts ~classes ~tailwind:legacy_css
                    ~tw:our_css))
      with e ->
        `Error
          (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e)))

let process_single_class class_str flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_single_class class_str ~opts
  | Tailwind -> (
      try
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true
            ?input_css:(reference_entrypoint ~opts)
            [ class_str ]
        in
        emit ~opts css;
        `Ok 0
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (tailwind_error e) ))
  | Native -> (
      let include_base = eval_flag flag ~default:false in
      match single_class_sheet ~opts ~base:include_base class_str with
      | None -> `Error (false, unknown_class_error ~theme:opts.theme class_str)
      | Some stylesheet ->
          emit ~opts (render_css ~opts stylesheet);
          `Ok 0)

(* Classes live outside component sources too: a docs site keeps most of its
   markup in .md/.mdx, and plain .ts/.js hold class strings just as .tsx does.
   Skipping them emits a fraction of the utilities the project uses, with
   nothing to say so. *)
let is_content path =
  List.exists
    (Filename.check_suffix path)
    [
      ".html";
      ".eml";
      ".ml";
      ".re";
      ".js";
      ".jsx";
      ".ts";
      ".tsx";
      ".vue";
      ".svelte";
      ".md";
      ".mdx";
    ]

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then files path is_content else [ path ]
      else [])
    paths

let print_stats ~quiet ~candidate_count ~known_count =
  if (not quiet) && known_count = 0 && candidate_count > 0 then (
    Fmt.epr "@.--- Statistics ---%@.";
    Fmt.epr "Candidate tokens scanned: %d@." candidate_count;
    Fmt.epr "Successfully parsed: %d@." known_count)

let relative_to root file =
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  if String.starts_with ~prefix file then
    String.sub file (String.length prefix)
      (String.length file - String.length prefix)
  else file

(* What one [@source] path names, resolved against the stylesheet's directory
   the way Tailwind resolves it. A directory is walked like a path on the
   command line, a glob keeps the files under its root that match it, and a path
   naming nothing is skipped, as Tailwind skips it. *)
let source_files ~base path =
  let path =
    if Filename.is_relative path then Filename.concat base path else path
  in
  if Tw_tools.Source_scan.is_glob path then
    let root, pattern = Tw_tools.Source_scan.glob_root path in
    if Sys.file_exists root then
      files root (fun file ->
          Tw_tools.Source_scan.glob_matches ~pattern (relative_to root file))
    else []
  else collect_files [ path ]

(* The files the entrypoint's [@source] directives name, less those its [@source
   not] directives take back out. *)
let entrypoint_files ~(opts : gen_opts) =
  match (opts.input_css_path, opts.input_css) with
  | Some path, Some css ->
      let base = Filename.dirname path in
      let included, excluded = Tw_tools.Entrypoint.source_paths css in
      let dropped = List.concat_map (source_files ~base) excluded in
      List.concat_map (source_files ~base) included
      |> List.filter (fun file -> not (List.exists (String.equal file) dropped))
  | _ -> []

let scanned_classes ~opts paths =
  with_span scan_span @@ fun () ->
  collect_files paths @ entrypoint_files ~opts
  |> List.concat_map Tw_tools.Source_scan.candidates_from_file
  |> List.sort_uniq String.compare

let native_stylesheet ~(opts : gen_opts) ~include_base all_classes =
  Tw_tools.Project.stylesheet ~theme:opts.theme ?entrypoint:opts.input_css_path
    ~base:include_base all_classes

let diff_files paths ~(opts : gen_opts) =
  let all_classes = scanned_classes ~opts paths in
  (* Only a candidate that names a utility is a class the document has to carry:
     scanning sources reads every word that could be one. *)
  let classes =
    List.filter
      (fun cls -> Result.is_ok (Tw.of_string ~theme:opts.theme cls))
      all_classes
  in
  match uncovered_refusal ~opts classes with
  | Some reason ->
      Fmt.epr "Error: %s@." reason;
      `Ok 2
  | None -> (
      try
        let legacy_css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true
            ?input_css:(reference_entrypoint ~opts)
            all_classes
        in
        let _, stylesheet =
          native_stylesheet ~opts ~include_base:true all_classes
        in
        let our_css = render_css ~opts stylesheet in
        let diff =
          Tw_tools.Parity_compare.diff ~mode:opts.diff_mode legacy_css our_css
        in
        print_oracle_note all_classes;
        let code = print_diff_result "" diff in
        `Ok
          (max code
             (browser_verdict ~opts ~classes ~tailwind:legacy_css ~tw:our_css))
      with e ->
        `Error
          (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e)))

let native_files paths flag ~(opts : gen_opts) =
  let include_base = eval_flag flag ~default:true in
  try
    let all_classes = scanned_classes ~opts paths in
    let known_count, stylesheet =
      native_stylesheet ~opts ~include_base all_classes
    in
    emit ~opts (render_css ~opts stylesheet);
    print_stats ~quiet:opts.quiet ~candidate_count:(List.length all_classes)
      ~known_count;
    `Ok 0
  with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))

let process_files paths flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_files paths ~opts
  | Tailwind -> (
      try
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true
            ?input_css:(reference_entrypoint ~opts)
            (scanned_classes ~opts paths)
        in
        emit ~opts css;
        `Ok 0
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (tailwind_error e) ))
  | Native -> native_files paths flag ~opts

(* A v3 [@config] names a JavaScript config, which tw does not evaluate, so an
   entrypoint carrying one is refused rather than compiled without the theme it
   would add. The Tailwind backend reads the config itself, so only tw's own
   compile refuses it. *)
let js_config_refusal ~backend ~input_css css_content =
  match (backend, input_css, css_content) with
  | (Native | Diff), Some path, Some css -> (
      match Entrypoint.config_directives css with
      | [] -> None
      | config :: _ ->
          Some
            (String.concat ""
               [
                 "Error: ";
                 path;
                 ": @config ";
                 config;
                 " loads a JavaScript config, which tw does not evaluate; \
                  declare its theme in an @theme block instead";
               ]))
  | _ -> None

(* With no path on the command line the sources are detected as Tailwind's CLI
   detects them: from the working directory, from the directory the import's
   [source("dir")] names relative to the stylesheet, or not at all under
   [source(none)], which leaves what the [@source] directives name. *)
let detected_roots ~input_css css_content =
  match (input_css, css_content) with
  | Some path, Some css -> (
      match Entrypoint.source_root css with
      | `None -> []
      | `Detect -> [ Filename.current_dir_name ]
      | `Dir dir when Filename.is_relative dir ->
          [ Filename.concat (Filename.dirname path) dir ]
      | `Dir dir -> [ dir ])
  | _ -> [ Filename.current_dir_name ]

(* Tailwind's CLI takes no path to scan, so a stylesheet named as one was meant
   as the entrypoint; read as markup it holds no class. *)
let stylesheet_refusal path =
  Fmt.str
    "%s is a stylesheet, not markup to scan; pass it with -i to use it as the \
     entrypoint"
    path

let stylesheet_among paths =
  List.find_opt
    (fun path ->
      Filename.check_suffix path ".css"
      && Sys.file_exists path
      && not (Sys.is_directory path))
    paths

(* What a build read, each file with what [stat] says of it, so a file that
   changes, appears or goes makes a different stamp. *)
let stamp files =
  List.map
    (fun file ->
      match Unix.stat file with
      | st -> (file, st.Unix.st_mtime, st.Unix.st_size)
      | exception Unix.Unix_error _ -> (file, 0., -1))
    files

(* Whether standard input has closed, waiting up to [timeout] seconds for it to
   say so. Whatever arrives on it is read and dropped. *)
let stdin_closed timeout =
  match Unix.select [ Unix.stdin ] [] [] timeout with
  | [], _, _ -> false
  | _ -> (
      let buf = Bytes.create 4096 in
      match Unix.read Unix.stdin buf 0 (Bytes.length buf) with
      | 0 -> true
      | _ -> false
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
      | exception Unix.Unix_error _ -> true)
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> false
  | exception Unix.Unix_error _ -> true

let report_failure = function
  | `Error (_, message) -> Fmt.epr "tw: %s@." message
  | _ -> ()

(* [--watch] rebuilds whenever what the build read changes, polling every
   [interval] seconds, and stops once standard input closes unless it was asked
   to watch always, as Tailwind's CLI does. A failed rebuild is reported and the
   watch goes on. *)
let rec watch ~always ~interval ~inputs ~build previous =
  let closed =
    if always then begin
      Unix.sleepf interval;
      false
    end
    else stdin_closed interval
  in
  if closed then `Ok 0
  else
    let current = stamp (inputs ()) in
    if current <> previous then begin
      report_failure (build ());
      watch ~always ~interval ~inputs ~build current
    end
    else watch ~always ~interval ~inputs ~build previous

let tw_main single_class base_flag ~css_mode ~minify ~optimize ~quiet ~backend
    ~input_css ~output ~watching ~diff_mode ~html paths =
  (* Resolve default CSS mode based on operation kind when not provided *)
  let resolved_css_mode : Css.mode =
    match (single_class, backend, css_mode) with
    | _, Diff, _ -> Variables (* Diff always uses variables mode *)
    | _, _, `Inline -> Inline
    | _, _, `Variables -> Variables
    | Some _, _, `Default -> Inline (* single-class defaults to inline mode *)
    | None, _, `Default -> Variables (* files/scan default to variables *)
  in
  (* Diff mode forces minified output; Cascade handles semantic comparison. *)
  let resolved_minify = match backend with Diff -> true | _ -> minify in
  let resolved_optimize = optimize in
  (* Build the renderer theme from the project's CSS entrypoint (its @theme), so
     a --diff over a real repo compares against the same tokens Tailwind uses.
     It is read again for every build, so a watch sees an edit to it. *)
  let load () =
    let css_content = Option.map Entrypoint.read_file input_css in
    let theme =
      match css_content with
      | None -> Tw.Scheme.default
      | Some css -> with_span theme_span (fun () -> Entrypoint.theme_of_css css)
    in
    let opts : gen_opts =
      {
        minify = resolved_minify;
        optimize = resolved_optimize;
        quiet;
        css_mode = resolved_css_mode;
        backend;
        theme;
        input_css = css_content;
        input_css_path = input_css;
        diff_mode;
        output;
        html;
      }
    in
    let roots =
      match paths with
      | [] -> detected_roots ~input_css css_content
      | paths -> paths
    in
    (opts, roots)
  in
  let build () =
    let opts, roots = load () in
    match js_config_refusal ~backend ~input_css opts.input_css with
    | Some message -> `Error (false, message)
    | None -> (
        match single_class with
        | Some class_str -> process_single_class class_str base_flag ~opts
        | None -> process_files roots base_flag ~opts)
  in
  match (stylesheet_among paths, watching) with
  | Some path, _ -> `Error (false, stylesheet_refusal path)
  | None, None -> build ()
  | None, Some _ when backend <> Native || Option.is_some single_class ->
      `Error
        ( true,
          "--watch rebuilds from sources and takes no -s, --tailwind or --diff"
        )
  | None, Some (always, interval) ->
      let inputs () =
        let opts, roots = load () in
        Option.to_list input_css @ collect_files roots @ entrypoint_files ~opts
      in
      let before = stamp (inputs ()) in
      report_failure (build ());
      watch ~always ~interval ~inputs ~build before

(* Command-line arguments *)
let single_flag =
  let doc = "Generate CSS for a single Tailwind class" in
  Arg.(
    value & opt (some string) None & info [ "s"; "single" ] ~docv:"CLASS" ~doc)

let base_flag =
  Arg.(
    value
    & vflag `Default
        [
          ( `Enable,
            info [ "base" ]
              ~doc:
                "Include the Base layer (Preflight CSS reset and semantic \
                 defaults)" );
          (`Disable, info [ "no-base" ] ~doc:"Exclude the Base layer");
        ])

let minify_flag =
  let doc = "Minify the generated CSS output" in
  Arg.(value & flag & info [ "m"; "minify" ] ~doc)

let optimize_flag =
  let doc =
    "Optimize the generated CSS by merging and deduplicating rules. Also \
     passed to the Tailwind backend under --tailwind and --diff."
  in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "silent" ] ~doc)

let input_css_arg =
  let doc =
    "The project's CSS entrypoint. Its @theme configures tw's renderer, its \
     @source directives and source() option say what to scan, and --tailwind \
     and --diff hand it to Tailwind."
  in
  Arg.(
    value
    & opt (some string) None
    & info [ "i"; "input"; "input-css" ] ~docv:"CSS" ~doc)

let output_arg =
  let doc =
    "Write the generated CSS to $(docv), creating its directory, rather than \
     to standard output, which - also names."
  in
  Arg.(
    value & opt (some string) None & info [ "o"; "output" ] ~docv:"FILE" ~doc)

let watch_flag =
  let doc =
    "Rebuild whenever the entrypoint or a scanned file changes, until standard \
     input closes; $(b,--watch=always) keeps watching after it closes."
  in
  Arg.(value & flag & info [ "w"; "watch" ] ~doc)

(* [--watch=always] arrives here: an optional value on [--watch] itself would
   take a path written after it as the value. *)
let watch_always_flag =
  Arg.(value & flag & info [ "watch-always" ] ~docs:Manpage.s_none)

let poll_arg =
  let doc =
    "How often $(b,--watch) looks for changes, in milliseconds. tw always \
     polls, every 250ms unless told otherwise."
  in
  Arg.(value & opt (some int) None & info [ "poll" ] ~docv:"MS" ~doc)

(* Whether to watch, whether to go on after standard input closes, and how often
   to look, in seconds. *)
let watching_term =
  let watching watch always poll =
    match poll with
    | Some ms when ms <= 0 ->
        Error "--poll takes a positive number of milliseconds"
    | _ when not (watch || always) -> Ok None
    | _ ->
        let ms = Option.value poll ~default:250 in
        Ok (Some (always, float_of_int ms /. 1000.))
  in
  Term.(
    term_result' ~usage:true
      (const watching $ watch_flag $ watch_always_flag $ poll_arg))

let cwd_arg =
  let doc =
    "Run from $(docv): the paths given are read against it, and sources are \
     detected from it."
  in
  Arg.(value & opt (some dir) None & info [ "cwd" ] ~docv:"DIR" ~doc)

let tailwind_flag =
  let doc_tailwind = "Use the real tailwindcss tool to generate CSS" in
  Arg.(value & flag & info [ "tailwind" ] ~doc:doc_tailwind)

let diff_flag =
  let doc = "Compare tw output with real Tailwind CSS." in
  Arg.(value & flag & info [ "diff" ] ~doc)

let diff_mode_arg =
  let doc =
    "CSS comparison mode for --diff: canonical (default, ignores selector \
     regrouping/reordering, right for real-world parity sweeps), auto, \
     tree/structural (reports regrouping), or string."
  in
  let mode_conv =
    Arg.enum
      [
        ("canonical", `Canonical);
        ("auto", `Auto);
        ("tree", `Tree);
        ("structural", `Tree);
        ("string", `String);
      ]
  in
  Arg.(
    value & opt mode_conv `Canonical & info [ "diff-mode" ] ~docv:"MODE" ~doc)

(* Which tool generates, and how a comparison reads the two sheets. *)
let html_arg =
  let doc =
    "With $(b,--diff), also render Tailwind's sheet and tw's over the HTML \
     document $(docv) in a headless Chromium and report every computed-style \
     value they disagree on, whatever the canonical comparison says. Every \
     class compared must appear in $(docv). Needs node and a headless \
     Chromium, and exits 2 without them."
  in
  Arg.(value & opt (some file) None & info [ "html" ] ~docv:"FILE" ~doc)

let backend_term =
  let backend tailwind diff diff_mode html =
    match (tailwind, diff, html) with
    | true, true, _ -> Error "--tailwind and --diff are mutually exclusive"
    | _, false, Some _ ->
        Error "--html renders the two sheets --diff compares; it needs --diff"
    | _, true, html -> Ok (Diff, diff_mode, html)
    | true, false, None -> Ok (Tailwind, `Canonical, None)
    | false, false, None -> Ok (Native, `Canonical, None)
  in
  Term.(
    term_result' ~usage:true
      (const backend $ tailwind_flag $ diff_flag $ diff_mode_arg $ html_arg))

let css_mode_vflag =
  let doc_inline = "Inline mode: resolve values (no variables), no layers." in
  let doc_vars = "Variables mode: emit CSS variables and layered output." in
  Arg.(
    value
    & vflag `Default
        [
          (`Inline, info [ "inline" ] ~doc:doc_inline);
          (`Variables, info [ "variables" ] ~doc:doc_vars);
        ])

let paths_arg =
  let doc = "Files or directories to scan for Tailwind classes" in
  Arg.(value & pos_all string [] & info [] ~docv:"PATH" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P "tw is a tool that generates CSS from Tailwind-like utility classes.";
    `P
      "It can generate CSS for a single class using -s (no base styles by \
       default), or scan files/directories and generate a complete stylesheet \
       (with base styles by default).";
    `S Manpage.s_examples;
    `P
      "Build a project the way Tailwind's CLI does, detecting its sources from \
       the working directory:";
    `Pre "  tw -i src/app.css -o dist/app.css --watch";
    `P "Generate CSS for a single class (no Base layer by default):";
    `Pre "  tw -s bg-blue-500";
    `P "Generate CSS for a single class with the Base layer:";
    `Pre "  tw -s bg-blue-500 --base";
    `P "Scan files and generate CSS (with the Base layer by default):";
    `Pre "  tw index.html src/";
    `P "Scan files and generate CSS without the Base layer:";
    `Pre "  tw --no-base index.html src/";
    `P "Generate inline mode (no variables, no layers):";
    `Pre "  tw --inline index.html src/";
    `P "Generate minified CSS:";
    `Pre "  tw --minify index.html src/";
    `P "Generate optimized CSS (rule merging/deduplication):";
    `Pre "  tw --optimize index.html src/";
    `P "Generate both minified and optimized CSS:";
    `Pre "  tw --minify --optimize index.html src/";
    `P "Use real Tailwind CSS:";
    `Pre "  tw -s bg-blue-500 --tailwind";
    `P "Compare tw output with real Tailwind CSS:";
    `Pre "  tw -s prose-sm --diff --diff-mode=canonical";
    `P "Use structural diff output when regrouping/order is relevant:";
    `Pre "  tw -s prose-sm --diff --diff-mode=tree";
    `S Manpage.s_see_also;
    `P "https://tailwindcss.com";
  ]

(* [--cwd] moves the process before any path is read, so the entrypoint, the
   output and the paths are all read against it, as Tailwind's CLI reads [-i]
   and [-o]. *)
let run ~minify ~optimize ~quiet s b css_m (backend, diff_mode, html) input_css
    output watching cwd paths =
  Option.iter Sys.chdir cwd;
  let missing =
    List.find_opt
      (fun path -> not (Sys.file_exists path))
      (Option.to_list input_css @ paths)
  in
  match missing with
  | Some path -> `Error (true, Fmt.str "no '%s' file or directory" path)
  | None ->
      tw_main s b ~css_mode:css_m ~minify ~optimize ~quiet ~backend ~diff_mode
        ~html ~input_css ~output ~watching paths

let cmd =
  let doc = "A Tailwind CSS-like utility class generator for OCaml" in
  let exits =
    Cmd.Exit.info 1
      ~doc:"when $(b,--diff) finds a difference between the two sheets."
    :: Cmd.Exit.info 2
         ~doc:"when $(b,--diff) cannot read one of the two sheets."
    :: Cmd.Exit.defaults
  in
  let info = Cmd.info "tw" ~version:Tw_info.version ~doc ~man ~exits in
  Cmd.v info
    Term.(
      ret
        (const (fun () s b css_m m o q ->
             run ~minify:m ~optimize:o ~quiet:q s b css_m)
        $ Observe.setup ~json_reporter:None "tw"
        $ single_flag $ base_flag $ css_mode_vflag $ minify_flag $ optimize_flag
        $ quiet_flag $ backend_term $ input_css_arg $ output_arg $ watching_term
        $ cwd_arg $ paths_arg))

(* Spellings cmdliner has no form for: [--diff=MODE], Tailwind's
   [--watch=always] (or [--watch always]) and a bare [--poll]. *)
let normalize_argv argv =
  let rec go = function
    | [] -> []
    | ("-w" | "--watch") :: "always" :: rest
    | ("-w=always" | "--watch=always") :: rest ->
        "--watch" :: "--watch-always" :: go rest
    | "--poll" :: ms :: rest when Option.is_some (int_of_string_opt ms) ->
        ("--poll=" ^ ms) :: go rest
    | "--poll" :: rest -> "--poll=250" :: go rest
    | arg :: rest when String.starts_with ~prefix:"--diff=" arg ->
        let mode = String.sub arg 7 (String.length arg - 7) in
        "--diff" :: ("--diff-mode=" ^ mode) :: go rest
    | arg :: rest -> arg :: go rest
  in
  Array.of_list (go (Array.to_list argv))

let () = exit (Cmd.eval' ~argv:(normalize_argv Sys.argv) cmd)
