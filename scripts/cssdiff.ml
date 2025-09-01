(** Script to compare two CSS files with structural parsing for better diffs
    Usage: cssdiff.exe <css_file1> <css_file2> *)

let read_file path =
  try
    let ic = open_in path in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content
  with Sys_error msg ->
    Fmt.epr "Error reading %s: %s@." path msg;
    None

let () =
  (* Parse command line arguments *)
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ file1; file2 ] -> (
      match (read_file file1, read_file file2) with
      | Some css1, Some css2 ->
          if Tw_tools.Css_compare.compare_css css1 css2 then (
            Fmt.pr "✓ CSS files are structurally identical@.";
            exit 0)
          else (
            Fmt.pr "✗ CSS files differ structurally@.@.";
            (* Show detailed diff *)
            let diff = Tw_tools.Css_compare.format_diff css1 css2 in
            Fmt.pr "%s@." diff;
            exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
