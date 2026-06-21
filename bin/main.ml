module Css = Cascade.Css
open Cascade_diff
open Cmdliner

(* Parse a whitespace-separated string of classes *)
let parse_classes ?(warn = true) classes_str =
  let class_names = Tw_tools.Source_scan.split_whitespace classes_str in
  List.filter_map
    (fun cls ->
      match Tw.of_string cls with
      | Ok style -> Some style
      | Error _ ->
          if warn then Fmt.epr "Warning: Unknown class '%s'@." cls;
          None)
    class_names

(* Recursively get files in directories *)
let rec files path patterns =
  if Sys.is_directory path then
    let entries = Sys.readdir path |> Array.to_list in
    List.concat_map
      (fun entry -> files (Filename.concat path entry) patterns)
      entries
  else if
    List.exists (fun pattern -> Filename.check_suffix path pattern) patterns
  then [ path ]
  else []

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
  test_scheme : bool;
}

(* The test scheme matching Tailwind's test @theme: hex colors and explicit
   spacing variables. Threaded into to_css via ~theme under --test-scheme. *)
let test_scheme : Tw.Scheme.t =
  {
    colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
    spacing = [ (4, Css.Rem 1.0) ];
    radius = [];
    default_ring_width = 1;
    default_border_width = 1;
    default_outline_width = 1;
    breakpoints = [];
    token_overrides = [];
  }

let theme_of (opts : gen_opts) =
  if opts.test_scheme then test_scheme else Tw.Scheme.default

let eval_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

let print_diff_result label diff =
  match diff.Css_compare.result with
  | Css_compare.No_diff _ -> Fmt.pr "✓ No differences found%s@." label
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
      (* The generated sheet is a closed author stylesheet, so prune theme
         tokens nothing references -- e.g. --spacing once p-0 folds to 0, which
         Tailwind also drops. *)
      Tw.Css.optimize ~prune_unused_custom_props:true stylesheet
    else stylesheet
  in
  Tw.Css.to_string ~minify:opts.minify stylesheet

let diff_single_class class_str ~(opts : gen_opts) =
  try
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true [ class_str ]
    in
    let tw_styles = parse_classes ~warn:false class_str in
    let styles = match tw_styles with [] -> [] | s -> s in
    let stylesheet = Tw.to_css ~theme:(theme_of opts) ~base:true styles in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Css_compare.diff ~mode:`Canonical ~prune_unused_custom_props:true
        legacy_css our_css
    in
    match tw_styles with
    | [] when class_str = "" ->
        print_diff_result " (empty/base only)" diff;
        `Ok ()
    | [] -> `Error (false, Fmt.str "Error: Unknown class: %s" class_str)
    | _ ->
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
            ~optimize:opts.optimize ~forms:true [ class_str ]
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
      let tw_styles = parse_classes ~warn:false class_str in
      let styles = match tw_styles with [] -> [] | s -> s in
      match tw_styles with
      | [] when class_str <> "" ->
          `Error (false, Fmt.str "Error: Unknown class: %s" class_str)
      | _ ->
          let stylesheet =
            Tw.to_css ~theme:(theme_of opts) ~base:include_base styles
          in
          print_string (render_css ~opts stylesheet);
          `Ok ())

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then
          files path
            [ ".html"; ".eml"; ".ml"; ".re"; ".jsx"; ".tsx"; ".vue"; ".svelte" ]
        else [ path ]
      else [])
    paths

let print_stats ~quiet ~candidate_count ~known_count =
  if (not quiet) && known_count = 0 && candidate_count > 0 then (
    Fmt.epr "@.--- Statistics ---%@.";
    Fmt.epr "Candidate tokens scanned: %d@." candidate_count;
    Fmt.epr "Successfully parsed: %d@." known_count)

let parse_known_candidates candidates =
  List.filter_map
    (fun cls ->
      match Tw.of_string cls with
      | Ok style -> Some (cls, style)
      | Error _ -> None)
    candidates

let diff_files paths ~(opts : gen_opts) =
  try
    let all_files = collect_files paths in
    let all_classes =
      List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
      |> List.sort_uniq String.compare
    in
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true all_classes
    in
    let tw_styles = parse_known_candidates all_classes |> List.map snd in
    let stylesheet = Tw.to_css ~theme:(theme_of opts) ~base:true tw_styles in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Css_compare.diff ~mode:`Canonical ~prune_unused_custom_props:true
        legacy_css our_css
    in
    print_diff_result "" diff;
    `Ok ()
  with e ->
    `Error (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e))

let native_files paths flag ~(opts : gen_opts) =
  let include_base = eval_flag flag ~default:true in
  try
    let all_files = collect_files paths in
    let all_classes =
      List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
      |> List.sort_uniq String.compare
    in
    let known = parse_known_candidates all_classes in
    let tw_styles = List.map snd known in
    let stylesheet =
      Tw.to_css ~theme:(theme_of opts) ~base:include_base tw_styles
    in
    print_string (render_css ~opts stylesheet);
    print_stats ~quiet:opts.quiet ~candidate_count:(List.length all_classes)
      ~known_count:(List.length known);
    `Ok ()
  with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))

let process_files paths flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_files paths ~opts
  | Tailwind -> (
      try
        let all_files = collect_files paths in
        let all_classes =
          List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
          |> List.sort_uniq String.compare
        in
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true all_classes
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
    ~test_scheme paths =
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
  let opts : gen_opts =
    {
      minify = resolved_minify;
      optimize = resolved_optimize;
      quiet;
      css_mode = resolved_css_mode;
      backend;
      test_scheme;
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
    "Pass CSS optimization through to the Tailwind backend. tw output is not \
     pre-optimized."
  in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let backend_vflag =
  let doc_tailwind = "Use the real tailwindcss tool to generate CSS" in
  let doc_diff =
    "Compare tw output with real Tailwind CSS (shows differences). Forces \
     --variables --base --minify for comparison."
  in
  Arg.(
    value
    & vflag Native
        [
          (Tailwind, info [ "tailwind" ] ~doc:doc_tailwind);
          (Diff, info [ "diff" ] ~doc:doc_diff);
        ])

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

let test_scheme_flag =
  let doc =
    "Use Tailwind test scheme (hex colors like #ef4444 for red-500, explicit \
     spacing variables). Matches Tailwind's test @theme configuration."
  in
  Arg.(value & flag & info [ "test-scheme" ] ~doc)

let cmd =
  let doc = "A Tailwind CSS-like utility class generator for OCaml" in
  let man =
    [
      `S Manpage.s_description;
      `P "tw is a tool that generates CSS from Tailwind-like utility classes.";
      `P
        "It can generate CSS for a single class using -s (no base styles by \
         default), or scan files/directories and generate a complete \
         stylesheet (with base styles by default).";
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
      `P "Compare tw output with real Tailwind CSS (minified and optimized):";
      `Pre "  tw -s prose-sm --diff";
      `S Manpage.s_see_also;
      `P "https://tailwindcss.com";
    ]
  in
  let info = Cmd.info "tw" ~version:"0.1.0" ~doc ~man in
  Cmd.v info
    Term.(
      ret
        (const (fun s b css_m m o q backend test_scheme paths ->
             tw_main s b ~css_mode:css_m ~minify:m ~optimize:o ~quiet:q ~backend
               ~test_scheme paths)
        $ single_flag $ base_flag $ css_mode_vflag $ minify_flag $ optimize_flag
        $ quiet_flag $ backend_vflag $ test_scheme_flag $ paths_arg))

let () = exit (Cmd.eval cmd)
