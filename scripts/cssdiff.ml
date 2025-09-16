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
          if css1 = css2 then (
            Fmt.pr "✓ CSS files are identical@.";
            exit 0)
          else (
            Fmt.pr "✗ CSS files differ@.@.";
            (* Just do simple string comparison *)
            (match (Css.of_string css1, Css.of_string css2) with
            | Ok _, Ok _ ->
                Fmt.pr
                  "Both CSS files parsed successfully but differ in content@."
            | Error msg1, Error msg2 ->
                let fmt_err (err : Css.parse_error) = Css.pp_parse_error err in
                if msg1.message = msg2.message && msg1.position = msg2.position
                then
                  Fmt.pr "Same parse error in both CSS files: %s@."
                    (fmt_err msg1)
                else
                  Fmt.pr "Parse errors in both CSS:@.First: %s@.Second: %s@."
                    (fmt_err msg1) (fmt_err msg2)
            | Error msg, _ ->
                Fmt.pr "Parse error in first CSS file: %s@."
                  (Css.pp_parse_error msg)
            | _, Error msg ->
                Fmt.pr "Parse error in second CSS file: %s@."
                  (Css.pp_parse_error msg));
            exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
