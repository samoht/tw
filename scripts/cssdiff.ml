(** Script to compare two CSS files with structural parsing for better diffs
    Usage: cssdiff.exe <css_file1> <css_file2> *)

let pp_parse_error ppf (Css_parser.Parse_error (msg, reader)) =
  let before, after = Css_parser.Reader.context_string reader in
  let pos = Css_parser.Reader.position reader in
  let len = Css_parser.Reader.length reader in
  Fmt.pf ppf "%s@.  Position %d/%d: %s[HERE]%s" msg pos len before after

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
            (match (Css_parser.of_string css1, Css_parser.of_string css2) with
            | Ok ast1, Ok ast2 ->
                let diff = Tw_tools.Css_compare.diff ast1 ast2 in
                Fmt.pr "%a@." Tw_tools.Css_compare.pp diff
            | Error e1, Error e2 ->
                (* Check if contexts are similar *)
                let (Css_parser.Parse_error (msg1, r1)) = e1 in
                let (Css_parser.Parse_error (msg2, r2)) = e2 in
                let b1, a1 = Css_parser.Reader.context_string r1 in
                let b2, a2 = Css_parser.Reader.context_string r2 in
                if msg1 = msg2 && b1 = b2 && a1 = a2 then
                  Fmt.pr "Parse error in both CSS files: %a@." pp_parse_error e1
                else
                  Fmt.pr "Parse errors in both CSS:@.First: %a@.Second: %a@."
                    pp_parse_error e1 pp_parse_error e2
            | Error e, _ ->
                Fmt.pr "Failed to parse first CSS: %a@." pp_parse_error e
            | _, Error e ->
                Fmt.pr "Failed to parse second CSS: %a@." pp_parse_error e);
            exit 1)
      | _ -> exit 1)
  | _ ->
      Fmt.epr "Usage: %s <css_file1> <css_file2>@." Sys.argv.(0);
      exit 1
