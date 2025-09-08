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
            let css1 = Tw_tools.Css_compare.strip_header css1 in
            let css2 = Tw_tools.Css_compare.strip_header css2 in
            (match (Css.of_string css1, Css.of_string css2) with
            | Ok ast1, Ok ast2 ->
                let diff = Tw_tools.Css_compare.diff ast1 ast2 in
                Fmt.pr "%a@." Tw_tools.Css_compare.pp diff
            | Error msg1, Error msg2 ->
                if msg1 = msg2 then
                  Fmt.pr "Same parse error in both CSS files: %s@." msg1
                else
                  Fmt.pr "Parse errors in both CSS:@.First: %s@.Second: %s@."
                    msg1 msg2
            | Error msg, _ -> Fmt.pr "Parse error in first CSS file: %s@." msg
            | _, Error msg -> Fmt.pr "Parse error in second CSS file: %s@." msg);
            exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
