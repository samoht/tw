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
            | Diff d
              when Tw_tools.Css_compare.(
                     d.rules = [] && d.media_queries = [] && d.layers = []
                     && d.supports_queries = [] && d.container_queries = []
                     && d.custom_properties = []) ->
                (* Check for string differences and show context *)
                if css1 <> css2 then (
                  match
                    Tw_tools.Css_compare.show_string_diff_context ~expected:css2
                      ~actual:css1
                  with
                  | Some (exp_ctx, act_ctx, (_diff_line, char_pos), pos) ->
                      Fmt.pr "✓ CSS files have no structural differences@.@.";
                      Fmt.pr "String difference at position %d:@." pos;
                      (* Format each line with prefix *)
                      let exp_lines = String.split_on_char '\n' exp_ctx in
                      let act_lines = String.split_on_char '\n' act_ctx in
                      List.iter (fun line -> Fmt.pr "- %s@." line) exp_lines;
                      List.iter (fun line -> Fmt.pr "+ %s@." line) act_lines;
                      (* Create caret line *)
                      Fmt.pr "  %s^@." (String.make char_pos ' ');
                      exit 0
                  | None ->
                      Fmt.pr
                        "✓ CSS files have no structural differences \
                         (whitespace/formatting only)@.";
                      exit 0)
                else (
                  Fmt.pr "✓ CSS files have no structural differences@.";
                  exit 0)
            | _ ->
                Fmt.pr "✗ CSS files differ@.@.";
                Tw_tools.Css_compare.pp_diff_result ~expected:"File 2"
                  ~actual:"File 1" ~expected_str:css2 ~actual_str:css1
                  Fmt.stdout result;
                Fmt.pr "@.";
                exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
