open Cmdliner

(* Parse a space-separated string of classes *)
let parse_classes classes_str =
  let class_names =
    String.split_on_char ' ' classes_str
    |> List.filter (fun s -> String.length s > 0)
  in
  List.filter_map
    (fun cls ->
      match Tw.of_string cls with Ok style -> Some style | Error _ -> None)
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

(* Main command implementation *)
let tw_main single_class reset_flag minify paths =
  match single_class with
  | Some class_str -> (
      (* Generate CSS for a single class - no reset by default unless
         overridden *)
      let reset =
        match reset_flag with
        | `Enable -> true
        | `Disable -> false
        | `Default -> false (* no reset by default for single class *)
      in
      let tw_styles = parse_classes class_str in
      match tw_styles with
      | [] -> `Error (false, Fmt.str "Error: Unknown class: %s" class_str)
      | styles ->
          let stylesheet = Tw.to_css ~reset styles in
          print_endline (Tw.stylesheet_to_string ~minify stylesheet);
          `Ok ())
  | None -> (
      (* Scan files and directories - reset enabled by default unless
         overridden *)
      let reset =
        match reset_flag with
        | `Enable -> true
        | `Disable -> false
        | `Default -> true (* reset by default for file scanning *)
      in
      match paths with
      | [] -> `Error (true, "Either provide -s <class> or file/directory paths")
      | paths -> (
          try
            let all_files =
              List.concat_map
                (fun path ->
                  if Sys.file_exists path then
                    if Sys.is_directory path then
                      files path [ ".html"; ".ml"; ".re"; ".jsx"; ".tsx" ]
                    else [ path ]
                  else [])
                paths
            in

            let all_classes =
              List.concat_map extract_classes_from_file all_files
              |> List.sort_uniq String.compare
            in

            let tw_styles =
              List.concat_map
                (fun classes_str ->
                  try parse_classes classes_str with Failure _ -> [])
                all_classes
            in

            let stylesheet = Tw.to_css ~reset tw_styles in
            print_endline (Tw.stylesheet_to_string ~minify stylesheet);
            `Ok ()
          with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))))

(* Command-line arguments *)
let single_flag =
  let doc = "Generate CSS for a single Tailwind class" in
  Arg.(
    value & opt (some string) None & info [ "s"; "single" ] ~docv:"CLASS" ~doc)

let reset_flag =
  Arg.(
    value
    & vflag `Default
        [
          (`Enable, info [ "reset" ] ~doc:"Include CSS reset/normalize rules");
          ( `Disable,
            info [ "no-reset" ] ~doc:"Do not include CSS reset/normalize rules"
          );
        ])

let minify_flag =
  let doc = "Minify the generated CSS output" in
  Arg.(value & flag & info [ "minify" ] ~doc)

let paths_arg =
  let doc = "Files or directories to scan for Tailwind classes" in
  Arg.(value & pos_all file [] & info [] ~docv:"PATH" ~doc)

let cmd =
  let doc = "A Tailwind CSS-like utility class generator for OCaml" in
  let man =
    [
      `S Manpage.s_description;
      `P "tw is a tool that generates CSS from Tailwind-like utility classes.";
      `P
        "It can either generate CSS for a single class using -s (without \
         reset), or scan files/directories for classes and generate a complete \
         stylesheet (with reset by default).";
      `S Manpage.s_examples;
      `P "Generate CSS for a single class (no reset by default):";
      `Pre "  tw -s bg-blue-500";
      `P "Generate CSS for a single class with reset:";
      `Pre "  tw -s bg-blue-500 --reset";
      `P "Scan files and generate CSS (with reset by default):";
      `Pre "  tw index.html src/";
      `P "Scan files and generate CSS without reset:";
      `Pre "  tw --no-reset index.html src/";
      `P "Generate minified CSS:";
      `Pre "  tw --minify index.html src/";
      `S Manpage.s_see_also;
      `P "https://tailwindcss.com";
    ]
  in
  let info = Cmd.info "tw" ~version:"0.1.0" ~doc ~man in
  Cmd.v info
    Term.(
      ret (const tw_main $ single_flag $ reset_flag $ minify_flag $ paths_arg))

let () = exit (Cmd.eval cmd)
