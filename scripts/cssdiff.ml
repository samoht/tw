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
      | Some css1, Some css2 -> (
          if css1 = css2 then (
            Fmt.pr "✓ CSS files are identical@.";
            exit 0)
          else
            (* Use css_compare for detailed structural comparison *)
            let result =
              Tw_tools.Css_compare.diff ~expected:css2 ~actual:css1
            in
            match result with
            | No_diff ->
                Fmt.pr "✓ CSS files are identical@.";
                exit 0
            | String_diff _ ->
                Fmt.pr "%a@,@,"
                  (Tw_tools.Css_compare.pp_stats ~expected_str:css2
                     ~actual_str:css1)
                  result;
                Fmt.pr "%a@,"
                  (Tw_tools.Css_compare.pp_diff_result ~expected:"File 2"
                     ~actual:"File 1")
                  result;
                exit 1
            | Diff _ | Both_errors _ | Expected_error _ | Actual_error _ ->
                Fmt.pr "%a@,@,"
                  (Tw_tools.Css_compare.pp_stats ~expected_str:css2
                     ~actual_str:css1)
                  result;
                Fmt.pr "%a@,"
                  (Tw_tools.Css_compare.pp_diff_result ~expected:"File 2"
                     ~actual:"File 1")
                  result;
                exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
