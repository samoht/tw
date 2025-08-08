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
          if warn then 
            Printf.eprintf "Warning: Unknown class '%s'\n" cls;
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

(* Main command implementation *)
let reset_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

let process_single_class class_str flag minify =
  let reset = reset_flag flag ~default:false in
  let tw_styles = parse_classes ~warn:false class_str in
  match tw_styles with
  | [] -> `Error (false, Fmt.str "Error: Unknown class: %s" class_str)
  | styles ->
      let stylesheet = Tw.to_css ~reset styles in
      print_endline (Tw.Css.to_string ~minify stylesheet);
      `Ok ()

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then
          files path [ ".html"; ".ml"; ".re"; ".jsx"; ".tsx" ]
        else [ path ]
      else [])
    paths

let process_files paths flag minify quiet =
  let reset = reset_flag flag ~default:true in
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
                  if not quiet then
                    Printf.eprintf "Warning: Unknown class '%s'\n" cls;
                  None)
            class_names)
        all_classes
    in
    let stylesheet = Tw.to_css ~reset tw_styles in
    print_endline (Tw.Css.to_string ~minify stylesheet);
    
    (* Print statistics to stderr *)
    if not quiet && !unknown_classes <> [] then begin
      let total = List.length !known_classes + List.length !unknown_classes in
      let unknown_count = List.length !unknown_classes in
      let unique_unknown = !unknown_classes |> List.sort_uniq String.compare in
      Printf.eprintf "\n--- Statistics ---\n";
      Printf.eprintf "Total classes found: %d\n" total;
      Printf.eprintf "Successfully parsed: %d\n" (List.length !known_classes);
      Printf.eprintf "Unknown classes: %d (%.1f%%)\n" 
        unknown_count 
        (float_of_int unknown_count /. float_of_int total *. 100.0);
      if List.length unique_unknown <= 20 then
        Printf.eprintf "Unknown: %s\n" (String.concat ", " unique_unknown)
      else
        Printf.eprintf "Unknown (first 20): %s...\n" 
          (String.concat ", " (List.filteri (fun i _ -> i < 20) unique_unknown))
    end;
    
    `Ok ()
  with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))

let tw_main single_class reset_flag minify quiet paths =
  match single_class with
  | Some class_str -> process_single_class class_str reset_flag minify
  | None -> (
      match paths with
      | [] -> `Error (true, "Either provide -s <class> or file/directory paths")
      | paths -> process_files paths reset_flag minify quiet)

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

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

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
      ret (const tw_main $ single_flag $ reset_flag $ minify_flag $ quiet_flag $ paths_arg))

let () = exit (Cmd.eval cmd)
