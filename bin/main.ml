open Cmdliner

(* Parse a space-separated string of classes *)
let parse_classes ?(warn = true) classes_str =
  let class_names =
    String.split_on_char ' ' classes_str
    |> List.filter (fun s -> String.length s > 0)
  in
  List.filter_map
    (fun cls ->
      match Tw.of_string cls with
      | Ok style -> Some style
      | Error _ ->
          if warn then Fmt.epr "Warning: Unknown class '%s'@." cls;
          None)
    class_names

(* Extract class names from files *)
let extract_classes_from_file filename =
  let ic = open_in filename in
  let classes = ref [] in
  try
    while true do
      let line = input_line ic in
      (* Simple regex-like pattern matching for class attributes *)
      (* This is a simplified version - in production you'd use a proper HTML parser *)
      let rec extract_from_line line =
        try
          let class_start = String.index line '"' in
          let class_end = String.index_from line (class_start + 1) '"' in
          let class_str =
            String.sub line (class_start + 1) (class_end - class_start - 1)
          in
          if
            String.length class_str > 0
            && (String.contains line '=' || String.contains line ':')
          then classes := class_str :: !classes;
          extract_from_line
            (String.sub line (class_end + 1)
               (String.length line - class_end - 1))
        with Not_found -> ()
      in
      extract_from_line line
    done;
    !classes
  with End_of_file ->
    close_in ic;
    !classes

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

(** Set up the test scheme matching Tailwind's test @theme.
    This uses hex colors and explicit spacing variables. *)
let setup_test_scheme () =
  let scheme : Tw.Scheme.t =
    {
      colors = [ ("red-500", Tw.Scheme.Hex "#ef4444") ];
      spacing = [ (4, Css.Rem 1.0) ];
      radius = [];
    }
  in
  Tw.Color.Handler.set_scheme scheme;
  Tw.Theme.set_scheme scheme;
  Tw.Borders.set_scheme scheme

let eval_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

let process_single_class class_str flag ~(opts : gen_opts) =
  (* Set up test scheme if requested *)
  if opts.test_scheme then setup_test_scheme ();
  match opts.backend with
  | Diff -> (
      (* For diff mode: always use variables mode and include base layer *)
      try
        (* Generate legacy Tailwind CSS *)
        let legacy_css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true [ class_str ]
        in

        (* Generate our CSS with variables mode and base layer *)
        let include_base = true in
        let tw_styles = parse_classes ~warn:false class_str in
        let styles = match tw_styles with [] -> [] | s -> s in
        let stylesheet = Tw.to_css ~base:include_base ~mode:Variables styles in
        let our_css =
          Tw.Css.to_string ~minify:opts.minify ~optimize:opts.optimize
            stylesheet
        in
        match tw_styles with
        | [] when class_str = "" ->
            (* Empty class string - just output base layer *)
            let diff =
              Tw_tools.Css_compare.diff ~expected:legacy_css ~actual:our_css
            in
            (match diff with
            | Tw_tools.Css_compare.No_diff ->
                Fmt.pr "✓ No differences found (empty/base only)@."
            | _ ->
                Fmt.pr "Differences found (empty/base only):@.@.";
                Tw_tools.Css_compare.pp ~expected:"Tailwind" ~actual:"tw"
                  Fmt.stdout diff;
                Fmt.pr "@.");
            `Ok ()
        | [] -> `Error (false, Fmt.str "Error: Unknown class: %s" class_str)
        | _ ->
            (* Compare the two outputs *)
            let diff =
              Tw_tools.Css_compare.diff ~expected:legacy_css ~actual:our_css
            in
            (match diff with
            | Tw_tools.Css_compare.No_diff ->
                Fmt.pr
                  "✓ No differences found between Tailwind and tw for '%s'@."
                  class_str
            | _ ->
                Fmt.pr "Differences found for '%s':@.@." class_str;
                Tw_tools.Css_compare.pp ~expected:"Tailwind" ~actual:"tw"
                  Fmt.stdout diff;
                Fmt.pr "@.");
            `Ok ()
      with e ->
        `Error
          (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e)))
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
            Tw.to_css ~base:include_base ~mode:opts.css_mode styles
          in
          print_string
            (Tw.Css.to_string ~minify:opts.minify ~optimize:opts.optimize
               stylesheet);
          `Ok ())

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then
          files path [ ".html"; ".ml"; ".re"; ".jsx"; ".tsx" ]
        else [ path ]
      else [])
    paths

let print_stats ~quiet ~known ~unknown =
  if (not quiet) && unknown <> [] then (
    let total = List.length known + List.length unknown in
    let unknown_count = List.length unknown in
    let unique_unknown = unknown |> List.sort_uniq String.compare in
    Fmt.epr "@.--- Statistics ---%@.";
    Fmt.epr "Total classes found: %d@." total;
    Fmt.epr "Successfully parsed: %d@." (List.length known);
    Fmt.epr "Unknown classes: %d (%.1f%%)@." unknown_count
      (float_of_int unknown_count /. float_of_int total *. 100.0);
    if List.length unique_unknown <= 20 then
      Fmt.epr "Unknown: %s@." (String.concat ", " unique_unknown)
    else
      Fmt.epr "Unknown (first 20): %s...@."
        (String.concat ", " (List.filteri (fun i _ -> i < 20) unique_unknown)))

let process_files paths flag ~(opts : gen_opts) =
  (* Set up test scheme if requested *)
  if opts.test_scheme then setup_test_scheme ();
  match opts.backend with
  | Diff -> (
      (* For diff mode: always use variables mode and include base layer *)
      try
        let all_files = collect_files paths in
        let all_classes_raw =
          List.concat_map extract_classes_from_file all_files
          |> List.sort_uniq String.compare
        in
        let all_classes =
          all_classes_raw
          |> List.concat_map (fun classes_str ->
              String.split_on_char ' ' classes_str
              |> List.filter (fun s -> String.length s > 0))
          |> List.sort_uniq String.compare
        in

        (* Generate legacy Tailwind CSS *)
        let legacy_css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true all_classes
        in

        (* Generate our CSS with variables mode and base layer *)
        let include_base = true in
        let tw_styles =
          List.concat_map
            (fun classes_str ->
              let class_names =
                String.split_on_char ' ' classes_str
                |> List.filter (fun s -> String.length s > 0)
              in
              List.filter_map
                (fun cls ->
                  match Tw.of_string cls with
                  | Ok style -> Some style
                  | Error _ -> None)
                class_names)
            all_classes_raw
        in
        let stylesheet =
          Tw.to_css ~base:include_base ~mode:Variables tw_styles
        in
        let our_css =
          Tw.Css.to_string ~minify:opts.minify ~optimize:opts.optimize
            stylesheet
        in

        (* Compare the two outputs *)
        let diff =
          Tw_tools.Css_compare.diff ~expected:legacy_css ~actual:our_css
        in
        (match diff with
        | Tw_tools.Css_compare.No_diff ->
            Fmt.pr "✓ No differences found between Tailwind and tw@."
        | _ ->
            Fmt.pr "Differences found:@.@.";
            Tw_tools.Css_compare.pp ~expected:"Tailwind" ~actual:"tw" Fmt.stdout
              diff;
            Fmt.pr "@.");
        `Ok ()
      with e ->
        `Error
          (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e)))
  | Tailwind -> (
      try
        let all_files = collect_files paths in
        let all_classes =
          List.concat_map extract_classes_from_file all_files
          |> List.sort_uniq String.compare
          |> List.concat_map (fun classes_str ->
              String.split_on_char ' ' classes_str
              |> List.filter (fun s -> String.length s > 0))
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
  | Native -> (
      let include_base = eval_flag flag ~default:true in
      try
        let all_files = collect_files paths in
        let all_classes =
          List.concat_map extract_classes_from_file all_files
          |> List.sort_uniq String.compare
        in
        let unknown_classes = ref [] in
        let known_classes = ref [] in
        let tw_styles =
          List.concat_map
            (fun classes_str ->
              let class_names =
                String.split_on_char ' ' classes_str
                |> List.filter (fun s -> String.length s > 0)
              in
              List.filter_map
                (fun cls ->
                  match Tw.of_string cls with
                  | Ok style ->
                      known_classes := cls :: !known_classes;
                      Some style
                  | Error _ ->
                      unknown_classes := cls :: !unknown_classes;
                      if not opts.quiet then
                        Fmt.epr "Warning: Unknown class '%s'@." cls;
                      None)
                class_names)
            all_classes
        in
        let stylesheet =
          Tw.to_css ~base:include_base ~mode:opts.css_mode tw_styles
        in
        print_string
          (Tw.Css.to_string ~minify:opts.minify ~optimize:opts.optimize
             stylesheet);

        (* Print statistics to stderr *)
        print_stats ~quiet:opts.quiet ~known:!known_classes
          ~unknown:!unknown_classes;

        `Ok ()
      with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e)))

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
  (* Diff mode defaults to minify=true and optimize=true for production
     comparison *)
  let resolved_minify = match backend with Diff -> true | _ -> minify in
  let resolved_optimize = match backend with Diff -> true | _ -> optimize in
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
    "Apply CSS optimizations (deduplicate, merge consecutive rules, combine\n\n\
    \     identical rules)"
  in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let backend_vflag =
  let doc_tailwind = "Use the real tailwindcss tool to generate CSS" in
  let doc_diff =
    "Compare tw output with real Tailwind CSS (shows differences). Always uses \
     --variables --base mode and defaults to --minify --optimize for \
     production comparison."
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
