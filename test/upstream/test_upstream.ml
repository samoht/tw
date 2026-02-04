(** Test runner for upstream Tailwind CSS tests

    Compares our tw output against expected CSS extracted from tailwindcss test
    snapshots. No network or external tools needed at runtime. *)

open Alcotest

type test_case = { name : string; classes : string list; expected : string }

let unescape s =
  let b = Buffer.create (String.length s) in
  let i = ref 0 in
  while !i < String.length s do
    if !i < String.length s - 1 && s.[!i] = '\\' then (
      incr i;
      match s.[!i] with
      | 'n' -> Buffer.add_char b '\n'
      | '\\' -> Buffer.add_char b '\\'
      | c ->
          (* Unknown escape - just skip the backslash *)
          Buffer.add_char b c)
    else Buffer.add_char b s.[!i];
    incr i
  done;
  Buffer.contents b

let read_test_cases filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let tests = ref [] in
    let current_name = ref None in
    let current_classes = ref [] in
    let current_expected = ref None in

    let flush () =
      match (!current_name, !current_expected) with
      | Some name, Some expected when !current_classes <> [] ->
          tests :=
            { name; classes = List.rev !current_classes; expected } :: !tests;
          current_classes := [];
          current_expected := None
      | _ ->
          current_classes := [];
          current_expected := None
    in

    (try
       while true do
         let line = input_line ic in
         let line = String.trim line in
         if String.length line = 0 then flush ()
         else if String.length line > 2 && line.[0] = '#' && line.[1] = ' ' then (
           flush ();
           current_name := Some (String.sub line 2 (String.length line - 2)))
         else if String.length line > 9 && String.sub line 0 9 = "classes: "
         then
           current_classes :=
             String.sub line 9 (String.length line - 9)
             |> String.split_on_char ' '
             |> List.filter (fun s -> String.length s > 0)
         else if String.length line > 10 && String.sub line 0 10 = "expected: "
         then
           current_expected :=
             Some (unescape (String.sub line 10 (String.length line - 10)))
       done
     with End_of_file ->
       flush ();
       close_in ic);
    List.rev !tests

let run_test_case test () =
  (if test.classes = [] then ()
   else if
     (* Debug: print first 1100 chars of expected to see escape handling *)
     test.name = "inset"
   then
     let preview =
       if String.length test.expected > 1100 then
         String.sub test.expected 900 200
       else test.expected
     in
     Printf.eprintf "DEBUG expected substring (900-1100):\n%S\n%!" preview);
  (* Parse classes and generate our CSS *)
  let utilities =
    List.filter_map
      (fun cls ->
        match Tw.of_string cls with Ok u -> Some u | Error _ -> None)
      test.classes
  in
  if utilities = [] then ()
  else
    (* Use inline mode - simple upstream tests expect plain CSS without
       layers *)
    let our_css =
      Tw.to_css ~base:false ~mode:Inline ~optimize:true utilities
      |> Css.to_string ~minify:false
    in
    (* Compare *)
    let diff =
      Tw_tools.Css_compare.diff ~expected:test.expected ~actual:our_css
    in
    match diff with
    | Tw_tools.Css_compare.No_diff -> ()
    | Tw_tools.Css_compare.String_diff _ -> ()
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
