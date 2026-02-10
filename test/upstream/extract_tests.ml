(** Extract test cases from tailwindcss utilities.test.ts

    This script parses the upstream Tailwind CSS test file and extracts:
    - Test names
    - Utility class names
    - Expected CSS output from toMatchInlineSnapshot

    Usage: dune exec test/upstream/extract_tests.exe -- <utilities.test.ts> *)

let is_valid_class s =
  String.length s > 0
  && s.[0] <> '@'
  && (not (String.contains s '('))
  && (not (Astring.String.is_suffix ~affix:"/foo" s))
  && (not (Astring.String.is_suffix ~affix:"/bar" s))
  && (not (Astring.String.is_suffix ~affix:"/sidebar" s))
  && (not (Astring.String.is_suffix ~affix:"/main" s))
  (* Filter bare utility names that need values *)
  && not
       (List.mem s
          [
            "m";
            "mx";
            "my";
            "mt";
            "mr";
            "mb";
            "ml";
            "ms";
            "me";
            "mbs";
            "mbe";
            "p";
            "px";
            "py";
            "pt";
            "pr";
            "pb";
            "pl";
            "ps";
            "pe";
            "pbs";
            "pbe";
            "w";
            "h";
            "size";
            "gap";
            "gap-x";
            "gap-y";
            "inset";
            "inset-x";
            "inset-y";
            "top";
            "right";
            "bottom";
            "left";
            "start";
            "end";
            "z";
            "order";
            "col";
            "row";
            "basis";
            "grow";
            "shrink";
            "border";
            "border-x";
            "border-y";
            "border-t";
            "border-r";
            "border-b";
            "border-l";
            "border-s";
            "border-e";
            "divide-x";
            "divide-y";
            "rounded";
            "rounded-t";
            "rounded-r";
            "rounded-b";
            "rounded-l";
            "rounded-tl";
            "rounded-tr";
            "rounded-br";
            "rounded-bl";
            "rounded-ss";
            "rounded-se";
            "rounded-es";
            "rounded-ee";
            "opacity";
            "shadow";
            "inset-shadow";
            "ring";
            "inset-ring";
            "ring-offset";
            "blur";
            "brightness";
            "contrast";
            "grayscale";
            "hue-rotate";
            "invert";
            "saturate";
            "sepia";
            "drop-shadow";
            "backdrop-blur";
            "backdrop-brightness";
            "backdrop-contrast";
            "backdrop-grayscale";
            "backdrop-hue-rotate";
            "backdrop-invert";
            "backdrop-opacity";
            "backdrop-saturate";
            "backdrop-sepia";
            "transition";
            "duration";
            "ease";
            "delay";
            "scale";
            "scale-x";
            "scale-y";
            "scale-z";
            "rotate";
            "rotate-x";
            "rotate-y";
            "rotate-z";
            "translate";
            "translate-x";
            "translate-y";
            "translate-z";
            "skew";
            "skew-x";
            "skew-y";
            "origin";
            "perspective";
            "perspective-origin";
            "accent";
            "caret";
            "scroll-m";
            "scroll-p";
            "scroll-mx";
            "scroll-my";
            "scroll-px";
            "scroll-py";
            "scroll-mt";
            "scroll-mr";
            "scroll-mb";
            "scroll-ml";
            "scroll-ms";
            "scroll-me";
            "scroll-pt";
            "scroll-pr";
            "scroll-pb";
            "scroll-pl";
            "scroll-ps";
            "scroll-pe";
            "columns";
            "aspect";
            "grid-cols";
            "grid-rows";
            "auto-cols";
            "auto-rows";
            "col-span";
            "col-start";
            "col-end";
            "row-span";
            "row-start";
            "row-end";
            "text";
            "font";
            "tracking";
            "leading";
            "decoration";
            "underline-offset";
            "indent";
            "bg";
            "from";
            "via";
            "to";
            "fill";
            "stroke";
            "outline";
            "outline-offset";
            "cursor";
            "list";
            "list-image";
            "min-w";
            "max-w";
            "min-h";
            "max-h";
            "space-x";
            "space-y";
            "line-clamp";
          ])

let extract_quoted_strings line =
  let pattern = Re.Pcre.regexp {|'([^']+)'|} in
  List.map (fun m -> Re.Group.get m 1) (Re.all pattern line)

type test_case = {
  name : string;
  classes : string list;
  expected : string option;
}

type parse_state =
  | Outside
  | InTest of string
  | InArray of string
  | InSnapshot of string * string list * Buffer.t

let parse_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let tests = ref [] in
  let lines = String.split_on_char '\n' content in
  let test_pattern = Re.Pcre.regexp {|^test\('([^']+)'|} in
  let run_pattern = Re.Pcre.regexp {|run\(\[([^\]]*)\]|} in
  let snapshot_start = Re.Pcre.regexp {|toMatchInlineSnapshot\(`|} in
  let snapshot_end = Re.Pcre.regexp {|`\)|} in
  (* Pattern for .toEqual('') which means the classes should produce empty
     output *)
  let empty_expect = Re.Pcre.regexp {|\.toEqual\s*\(\s*['"]{2}\s*\)|} in

  let state = ref Outside in
  let current_classes = ref [] in

  let flush_test name expected =
    let classes =
      !current_classes |> List.rev |> List.filter is_valid_class
      |> List.sort_uniq String.compare
    in
    if classes <> [] then tests := { name; classes; expected } :: !tests;
    current_classes := []
  in

  List.iter
    (fun line ->
      match !state with
      | Outside -> (
          match Re.exec_opt test_pattern line with
          | Some groups -> state := InTest (Re.Group.get groups 1)
          | None -> ())
      | InTest name ->
          (* Check for run([...]) *)
          (match Re.exec_opt run_pattern line with
          | Some groups ->
              let content = Re.Group.get groups 1 in
              current_classes :=
                List.rev_append
                  (extract_quoted_strings content)
                  !current_classes
          | None -> ());
          (* Check for .toEqual('') which means classes should produce empty
             output. Flush test with empty expected, then clear classes. *)
          if Re.execp empty_expect line then (
            flush_test name (Some "");
            current_classes := [] (* Check for array continuation *))
          else if
            Astring.String.is_infix ~affix:"[" line
            && not (Astring.String.is_infix ~affix:"]" line)
          then state := InArray name (* Check for snapshot start *)
          else if Re.execp snapshot_start line then
            state := InSnapshot (name, !current_classes, Buffer.create 256)
            (* Check for new test *)
          else if Astring.String.is_prefix ~affix:"test('" line then (
            flush_test name None;
            current_classes := [];
            match Re.exec_opt test_pattern line with
            | Some groups -> state := InTest (Re.Group.get groups 1)
            | None -> state := Outside
            (* Check for test end without snapshot *))
          else if Astring.String.is_prefix ~affix:"})" line then (
            flush_test name None;
            current_classes := [];
            state := Outside)
      | InArray name ->
          current_classes :=
            List.rev_append (extract_quoted_strings line) !current_classes;
          if Astring.String.is_infix ~affix:"]" line then state := InTest name
      | InSnapshot (name, classes, buf) ->
          if Re.execp snapshot_end line then (
            current_classes := List.rev classes;
            let expected = Buffer.contents buf |> String.trim in
            (* Remove surrounding quotes if present *)
            let expected =
              if String.length expected >= 2 && expected.[0] = '"' then
                String.sub expected 1 (String.length expected - 2)
              else expected
            in
            (* Unescape template literal backslashes: \\\\ -> \\ *)
            let expected =
              Astring.String.concat ~sep:{|\|}
                (Astring.String.cuts ~sep:{|\\|} expected)
            in
            flush_test name (Some expected);
            current_classes := [];
            state := InTest name)
          else (
            Buffer.add_string buf line;
            Buffer.add_char buf '\n'))
    lines;

  List.rev !tests

let () =
  if Array.length Sys.argv < 2 then (
    Fmt.epr "Usage: %s <path-to-utilities.test.ts>@." Sys.argv.(0);
    exit 1);

  let filename = Sys.argv.(1) in
  Fmt.epr "Parsing %s...@." filename;
  let tests = parse_file filename in

  (* Output in simple delimiter format: # test-name class1 class2 --- css output
     here with newlines preserved <<<>>> *)
  List.iter
    (fun test ->
      Fmt.pr "# %s@." test.name;
      Fmt.pr "%s@." (String.concat " " test.classes);
      (match test.expected with
      | Some css ->
          Fmt.pr "---@.";
          Fmt.pr "%s@." css
      | None -> ());
      Fmt.pr "<<<>>>@.")
    tests;

  let with_expected = List.filter (fun t -> t.expected <> None) tests in
  Fmt.epr "Extracted %d test cases (%d with expected CSS)@." (List.length tests)
    (List.length with_expected)
