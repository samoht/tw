module Css = Cascade.Css
module Entrypoint = Tw_tools.Entrypoint
open Cascade_diff
open Cmdliner

(* Parse a whitespace-separated string of classes *)
let parse_classes ?(warn = true) ?(theme = Tw.Scheme.default) classes_str =
  let class_names = Tw_tools.Source_scan.split_whitespace classes_str in
  List.filter_map
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok style -> Some style
      | Error _ ->
          if warn then Fmt.epr "Warning: Unknown class '%s'@." cls;
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
let rec files path patterns =
  let regular_file () =
    if List.exists (fun pattern -> Filename.check_suffix path pattern) patterns
    then [ path ]
    else []
  in
  try
    match (Unix.lstat path).st_kind with
    | Unix.S_DIR ->
        Sys.readdir path |> Array.to_list
        |> List.filter (fun entry -> not (ignored_scan_entry entry))
        |> List.concat_map (fun entry ->
            files (Filename.concat path entry) patterns)
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
}

let eval_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

(* The reference sheet is built by handing each class to Tailwind's engine, so
   the two sides answer the same question. A class the [@source inline] string
   cannot hold reaches the CLI through its source extractor instead, and the
   extractor drops what it cannot read, so a rule missing on the Tailwind side
   may be the harness rather than tw. Name those classes: a comparison whose
   provenance is unknown is worse than one that is merely narrower. *)
let print_oracle_note classes =
  match Tw_tools.Tailwind_gen.scanned_candidates classes with
  | [] -> ()
  | scanned ->
      Fmt.pr
        "Note: %s reached Tailwind through its source extractor, which drops a \
         candidate it cannot read rather than compiling it.@."
        (String.concat ", " scanned)

let print_diff_result label diff =
  match diff.Css_compare.result with
  | Css_compare.No_diff -> Fmt.pr "✓ No differences found%s@." label
  | _ ->
      Fmt.pr "Differences found%s:@.@." label;
      let buf = Buffer.create 256 in
      Css_compare.pp ~expected:"Tailwind" ~actual:"tw" buf diff;
      print_string (Buffer.contents buf);
      Fmt.pr "@."

let render_css ~(opts : gen_opts) stylesheet =
  let stylesheet =
    match opts.css_mode with
    | Inline -> Tw.Css.inline_vars stylesheet
    | Variables -> stylesheet
  in
  let stylesheet =
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
  Tw.Css.to_string ~minify:opts.minify stylesheet

(* Surface of_string's specific message (e.g. the actionable arbitrary-property
   feedback) for a single unknown class; fall back to a generic message. *)
let unknown_class_error ~theme class_str =
  match Tw.of_string ~theme class_str with
  | Error (`Msg m) -> Fmt.str "Error: %s" m
  | Ok _ -> Fmt.str "Error: Unknown class: %s" class_str

let diff_single_class class_str ~(opts : gen_opts) =
  try
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true ?input_css:opts.input_css [ class_str ]
    in
    let tw_styles = parse_classes ~warn:false ~theme:opts.theme class_str in
    let styles = match tw_styles with [] -> [] | s -> s in
    let stylesheet = Tw.to_css ~theme:opts.theme ~base:true styles in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Tw_tools.Parity_compare.diff ~mode:opts.diff_mode legacy_css our_css
    in
    match tw_styles with
    | [] when class_str = "" ->
        print_diff_result " (empty/base only)" diff;
        `Ok ()
    | [] -> `Error (false, unknown_class_error ~theme:opts.theme class_str)
    | _ ->
        print_oracle_note [ class_str ];
        print_diff_result
          (Fmt.str " between Tailwind and tw for '%s'" class_str)
          diff;
        `Ok ()
  with e ->
    `Error (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e))

let process_single_class class_str flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_single_class class_str ~opts
  | Tailwind -> (
      try
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true ?input_css:opts.input_css
            [ class_str ]
        in
        print_string css;
        `Ok ()
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (Printexc.to_string e)
          ))
  | Native -> (
      let include_base = eval_flag flag ~default:false in
      let tw_styles = parse_classes ~warn:false ~theme:opts.theme class_str in
      let styles = match tw_styles with [] -> [] | s -> s in
      match tw_styles with
      | [] when class_str <> "" ->
          `Error (false, unknown_class_error ~theme:opts.theme class_str)
      | _ ->
          let stylesheet =
            Tw.to_css ~theme:opts.theme ~base:include_base styles
          in
          print_string (render_css ~opts stylesheet);
          `Ok ())

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then
          (* Classes live outside component sources too: a docs site keeps most
             of its markup in .md/.mdx, and plain .ts/.js hold class strings
             just as .tsx does. Skipping them emits a fraction of the utilities
             the project uses, with nothing to say so. *)
          files path
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
        else [ path ]
      else [])
    paths

let print_stats ~quiet ~candidate_count ~known_count =
  if (not quiet) && known_count = 0 && candidate_count > 0 then (
    Fmt.epr "@.--- Statistics ---%@.";
    Fmt.epr "Candidate tokens scanned: %d@." candidate_count;
    Fmt.epr "Successfully parsed: %d@." known_count)

(* [prose] comes from @tailwindcss/typography, which Tailwind only applies when
   the entrypoint asks for it. A project that styles [.prose] itself, as
   tailwindcss.com does, gets the plugin's whole stylesheet on top otherwise. *)
let declares_plugin css name =
  match css with
  | None -> false
  | Some css ->
      let needle = "@tailwindcss/" ^ name in
      let n = String.length needle and l = String.length css in
      let rec go i =
        i + n <= l && (String.sub css i n = needle || go (i + 1))
      in
      go 0

let is_prose_class cls =
  cls = "prose"
  || String.starts_with ~prefix:"prose-" cls
  ||
  (* variants keep the utility at the end: [lg:prose-sm] *)
  match String.rindex_opt cls ':' with
  | Some i ->
      let bare = String.sub cls (i + 1) (String.length cls - i - 1) in
      bare = "prose" || String.starts_with ~prefix:"prose-" bare
  | None -> false

let parse_known_candidates ?(theme = Tw.Scheme.default) ?input_css candidates =
  let typography = declares_plugin input_css "typography" in
  List.filter_map
    (fun cls ->
      if (not typography) && is_prose_class cls then None
      else
        match Tw.of_string ~theme cls with
        | Ok style -> (
            (* A handler may accept a class at parse yet raise when it renders
               an arbitrary value it cannot serialise, as the docs'
               [prop-[<value>]] placeholders do. Such a class produces no rule,
               so drop it rather than let it abort the whole sheet. *)
            match Tw.to_css ~theme [ style ] with
            | (_ : Css.t) -> Some (cls, style)
            | exception
                (Invalid_argument _ | Failure _ | Cascade.Error.Parse_error _)
              ->
                None)
        | Error _ -> None)
    candidates

let scanned_classes paths =
  collect_files paths
  |> List.concat_map Tw_tools.Source_scan.candidates_from_file
  |> List.sort_uniq String.compare

(* The whole sheet tw generates for a scanned project: the built-in utilities,
   the classes the project's own [@utility] and [@custom-variant] declarations
   route, and the entrypoint all of it is spliced into. A comparison against the
   real Tailwind has to be made against this, not against the built-in utilities
   alone: Tailwind reads the same entrypoint, so every declared utility would
   otherwise read as a rule tw failed to emit. *)
let native_stylesheet ~(opts : gen_opts) ~include_base all_classes =
  let defs = Entrypoint.entry_variant_defs opts.input_css_path in
  let udefs = Entrypoint.entry_utility_defs opts.input_css_path in
  let routed, normal =
    List.partition (Entrypoint.is_custom_routed ~defs ~udefs) all_classes
  in
  let known =
    parse_known_candidates ~theme:opts.theme ?input_css:opts.input_css normal
  in
  let routed_count, routed_extra, routed_stmts =
    Entrypoint.custom_routed_utilities ~theme:opts.theme ~defs ~udefs routed
  in
  (* Routed custom variants no longer pass through the typed modifier parser,
     but the sorter still needs their exact names so a declaration such as
     [not-dark] is not mistaken for the built-in [not-] compound slot. Dummy
     selector values are sufficient here: routed candidates already carry the
     expanded author CSS in [extra], and only the registered names are read. *)
  let sort_theme =
    let custom = Tw.Scheme.{ values = [ ("", "&") ]; template = "{}" } in
    let custom_variants =
      List.fold_left
        (fun variants (name, _) ->
          if List.mem_assoc name variants then variants
          else (name, custom) :: variants)
        opts.theme.custom_variants defs
    in
    { opts.theme with custom_variants }
  in
  let stylesheet =
    Tw.to_css ~theme:sort_theme ~base:include_base ~extra:routed_extra
      (List.map snd known)
  in
  let stylesheet = Entrypoint.place_routed routed_stmts stylesheet in
  let stylesheet =
    match opts.input_css_path with
    | Some path ->
        Entrypoint.splice_into_entrypoint ~theme:opts.theme ~path stylesheet
    | None -> stylesheet
  in
  (List.length known + routed_count, stylesheet)

let diff_files paths ~(opts : gen_opts) =
  try
    let all_classes = scanned_classes paths in
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true ?input_css:opts.input_css all_classes
    in
    let _, stylesheet =
      native_stylesheet ~opts ~include_base:true all_classes
    in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Tw_tools.Parity_compare.diff ~mode:opts.diff_mode legacy_css our_css
    in
    print_oracle_note all_classes;
    print_diff_result "" diff;
    `Ok ()
  with e ->
    `Error (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e))

let native_files paths flag ~(opts : gen_opts) =
  let include_base = eval_flag flag ~default:true in
  try
    let all_classes = scanned_classes paths in
    let known_count, stylesheet =
      native_stylesheet ~opts ~include_base all_classes
    in
    print_string (render_css ~opts stylesheet);
    print_stats ~quiet:opts.quiet ~candidate_count:(List.length all_classes)
      ~known_count;
    `Ok ()
  with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))

let process_files paths flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_files paths ~opts
  | Tailwind -> (
      try
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true ?input_css:opts.input_css
            (scanned_classes paths)
        in
        print_string css;
        `Ok ()
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (Printexc.to_string e)
          ))
  | Native -> native_files paths flag ~opts

let tw_main single_class base_flag ~css_mode ~minify ~optimize ~quiet ~backend
    ~input_css ~diff_mode paths =
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
     a --diff over a real repo compares against the same tokens Tailwind
     uses. *)
  let css_content = Option.map Entrypoint.read_file input_css in
  let theme =
    match css_content with
    | None -> Tw.Scheme.default
    | Some css ->
        let overrides, inline = Entrypoint.theme_overrides_of_css css in
        let base =
          if Entrypoint.imports_static_theme css then
            { Tw.Scheme.default with static_theme = true }
          else Tw.Scheme.default
        in
        Tw.Scheme.with_overrides ~inline base overrides
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
    }
  in
  match single_class with
  | Some class_str -> process_single_class class_str base_flag ~opts
  | None -> (
      match paths with
      | [] -> `Error (true, "Either provide -s <class> or file/directory paths")
      | paths -> process_files paths base_flag ~opts)

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
  Arg.(value & flag & info [ "minify" ] ~doc)

let optimize_flag =
  let doc =
    "Optimize the generated CSS by merging and deduplicating rules. Also \
     passed to the Tailwind backend under --tailwind and --diff."
  in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let input_css_arg =
  let doc =
    "Project CSS entrypoint to feed to Tailwind during --diff. @theme blocks \
     are also used to configure tw's renderer."
  in
  Arg.(value & opt (some file) None & info [ "input-css" ] ~docv:"CSS" ~doc)

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
  Arg.(value & pos_all file [] & info [] ~docv:"PATH" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P "tw is a tool that generates CSS from Tailwind-like utility classes.";
    `P
      "It can generate CSS for a single class using -s (no base styles by \
       default), or scan files/directories and generate a complete stylesheet \
       (with base styles by default).";
    `S Manpage.s_examples;
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

let cmd =
  let doc = "A Tailwind CSS-like utility class generator for OCaml" in
  let info = Cmd.info "tw" ~version:Tw_info.version ~doc ~man in
  Cmd.v info
    Term.(
      ret
        (const (fun s b css_m m o q tailwind diff diff_mode input_css paths ->
             if tailwind && diff then
               `Error (true, "--tailwind and --diff are mutually exclusive")
             else
               let backend, diff_mode =
                 if diff then (Diff, diff_mode)
                 else
                   let backend = if tailwind then Tailwind else Native in
                   (backend, `Canonical)
               in
               tw_main s b ~css_mode:css_m ~minify:m ~optimize:o ~quiet:q
                 ~backend ~diff_mode ~input_css paths)
        $ single_flag $ base_flag $ css_mode_vflag $ minify_flag $ optimize_flag
        $ quiet_flag $ tailwind_flag $ diff_flag $ diff_mode_arg $ input_css_arg
        $ paths_arg))

let normalize_argv argv =
  argv |> Array.to_list
  |> List.concat_map (fun arg ->
      let prefix = "--diff=" in
      let prefix_len = String.length prefix in
      if String.length arg > prefix_len && String.sub arg 0 prefix_len = prefix
      then
        let mode = String.sub arg prefix_len (String.length arg - prefix_len) in
        [ "--diff"; "--diff-mode=" ^ mode ]
      else [ arg ])
  |> Array.of_list

let () = exit (Cmd.eval ~argv:(normalize_argv Sys.argv) cmd)
