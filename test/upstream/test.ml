(** Test runner for upstream Tailwind CSS tests

    Compares our tw output against expected CSS extracted from tailwindcss test
    snapshots. No external tools needed at runtime. *)

open Alcotest

type test_case = { name : string; classes : string list; expected : string }

let read_test_cases filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let tests = ref [] in
    let lines = String.split_on_char '\n' content in
    let rec parse lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if String.length line > 2 && line.[0] = '#' && line.[1] = ' ' then
            let name = String.sub line 2 (String.length line - 2) in
            parse_classes name rest
          else parse rest
    and parse_classes name lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "<<<>>>" then parse rest
          else if line = "---" then
            (* No classes line before ---, skip *)
            parse_expected name [] (Buffer.create 256) rest
          else if String.length line > 2 && line.[0] = '#' && line.[1] = ' '
          then
            (* New test without classes *)
            let new_name = String.sub line 2 (String.length line - 2) in
            parse_classes new_name rest
          else
            let classes =
              String.split_on_char ' ' line
              |> List.filter (fun s -> String.length s > 0)
            in
            parse_after_classes name classes rest
    and parse_after_classes name classes lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "---" then
            parse_expected name classes (Buffer.create 256) rest
          else if line = "<<<>>>" then
            (* No expected CSS, skip this test *)
            parse rest
          else parse rest
    and parse_expected name classes buf lines =
      match lines with
      | [] ->
          let expected = Buffer.contents buf |> String.trim in
          if classes <> [] && expected <> "" then
            tests := { name; classes; expected } :: !tests
      | line :: rest ->
          if String.trim line = "<<<>>>" then (
            let expected = Buffer.contents buf |> String.trim in
            if classes <> [] && expected <> "" then
              tests := { name; classes; expected } :: !tests;
            parse rest)
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf '\n';
            Buffer.add_string buf line;
            parse_expected name classes buf rest)
    in
    parse lines;
    List.rev !tests

let run_test_case test () =
  if test.classes = [] then ()
  else
    (* Parse classes and generate our CSS *)
    let utilities =
      List.filter_map
        (fun cls ->
          match Tw.of_string cls with Ok u -> Some u | Error _ -> None)
        test.classes
    in
    if utilities = [] then
      (* If we can't parse any classes, but expected CSS exists, that's a
         failure *)
      if String.trim test.expected <> "" then
        Alcotest.failf "Could not parse any classes: %s"
          (String.concat " " test.classes)
      else ()
    else
      let our_css =
        Tw.to_css ~base:false ~layers:false ~optimize:true utilities
        |> Css.to_string ~minify:false
      in
      (* Compare *)
      let diff =
        Tw_tools.Css_compare.diff ~expected:test.expected ~actual:our_css
      in
      match diff with
      | Tw_tools.Css_compare.No_diff -> ()
      | Tw_tools.Css_compare.String_diff _ ->
          (* Minor string differences are acceptable *)
          ()
      | Tw_tools.Css_compare.Tree_diff d ->
          if Tw_tools.Tree_diff.is_empty d then ()
          else
            let diff_str =
              Fmt.str "%a"
                (Tw_tools.Css_compare.pp ~expected:"Tailwind" ~actual:"tw")
                diff
            in
            Alcotest.fail
              (Fmt.str "CSS mismatch:\nClasses: %s\n%s"
                 (String.concat " " test.classes)
                 diff_str)
      | Tw_tools.Css_compare.Expected_error e ->
          Alcotest.fail
            (Fmt.str "Failed to parse expected CSS: %s" (Css.pp_parse_error e))
      | Tw_tools.Css_compare.Actual_error e ->
          Alcotest.fail
            (Fmt.str "Failed to parse our CSS: %s" (Css.pp_parse_error e))
      | Tw_tools.Css_compare.Both_errors (e1, e2) ->
          Alcotest.fail
            (Fmt.str "Parse errors:\nExpected: %s\nOurs: %s"
               (Css.pp_parse_error e1) (Css.pp_parse_error e2))

let () =
  let test_files = [ "utilities.txt"; "test/upstream/utilities.txt" ] in
  let test_file =
    List.find_opt Sys.file_exists test_files
    |> Option.value ~default:"utilities.txt"
  in

  if not (Sys.file_exists test_file) then (
    Fmt.epr "No test file found. Run extract_tests.exe first.@.";
    exit 0);

  let test_cases = read_test_cases test_file in
  if test_cases = [] then (
    Fmt.epr "No test cases with expected CSS found.@.";
    exit 0);

  Fmt.epr "Running %d upstream tests...@." (List.length test_cases);

  let tests =
    List.map (fun tc -> test_case tc.name `Quick (run_test_case tc)) test_cases
  in
  Alcotest.run "upstream" [ ("utilities", tests) ]
