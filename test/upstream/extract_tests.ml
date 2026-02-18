(** Extract test cases from tailwindcss utilities.test.ts

    This script parses the upstream Tailwind CSS test file and extracts:
    - Test names
    - Theme configuration ({!theme_config})
    - Utility class names
    - Expected CSS output from toMatchInlineSnapshot

    {2 Generating utilities.txt}

    The [utilities.txt] file is generated from the upstream Tailwind CSS test
    suite. To regenerate it:

    {v
    # Clone tailwindcss repo (or use existing clone)
    git clone https://github.com/tailwindlabs/tailwindcss.git /tmp/tailwindcss
    cd /tmp/tailwindcss && git checkout v4.1.8  # or desired version

    # Extract tests
    dune exec test/upstream/extract_tests.exe -- \
      /tmp/tailwindcss/packages/tailwindcss/src/utilities.test.ts \
      > test/upstream/utilities.txt
    v}

    {b Do NOT edit utilities.txt directly.} If test expectations need updating,
    regenerate from a newer Tailwind version or fix the extraction script.

    Usage: dune exec test/upstream/extract_tests.exe -- <utilities.test.ts> *)

(** Check if a string looks like a class name (vs a directive or invalid
    syntax). We only filter out things that aren't class names at all - bare
    utility names and invalid suffixes are kept as negative tests. *)
let is_valid_class s =
  String.length s > 0
  (* Filter out @layer, @apply, etc. - these are directives, not classes *)
  && s.[0] <> '@'
  (* Filter out function-like syntax - not valid class names *)
  && not (String.contains s '(')

let extract_quoted_strings line =
  let pattern = Re.Pcre.regexp {|'([^']+)'|} in
  List.map (fun m -> Re.Group.get m 1) (Re.all pattern line)

(* Strip quoted strings from a line so we can detect unquoted brackets. E.g.
   ['z-\[123\]', 'foo'] becomes [, ] — the ] inside quotes is removed. *)
let strip_quoted s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let rec loop i =
    if i >= len then Buffer.contents buf
    else
      match s.[i] with
      | '\'' ->
          let j = ref (i + 1) in
          while !j < len && s.[!j] <> '\'' do
            incr j
          done;
          loop (min (!j + 1) len)
      | '"' ->
          let j = ref (i + 1) in
          while !j < len && s.[!j] <> '"' do
            incr j
          done;
          loop (min (!j + 1) len)
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

(** Theme configuration detected from the CSS template passed to compileCss.
    Different configurations produce different CSS output for the same utility
    classes (e.g., [Theme_inline] inlines variable values instead of using
    [var()] references). *)
type theme_config =
  | Theme  (** [@theme { ... }] — standard theme with variable references *)
  | Theme_inline
      (** [@theme inline { ... }] — values inlined at compile time *)
  | Theme_reference
      (** [@theme reference { ... }] — reference-only, no [@property] rules *)
  | Theme_inline_reference
      (** [@theme inline reference { ... }] — inlined + reference *)
  | No_theme  (** No [@theme] block, just [@tailwind utilities;] *)
  | Run  (** Uses [run()] helper instead of [compileCss()] *)

let config_to_string = function
  | Theme -> "theme"
  | Theme_inline -> "theme-inline"
  | Theme_reference -> "theme-reference"
  | Theme_inline_reference -> "theme-inline-reference"
  | No_theme -> "none"
  | Run -> "run"

type test_case = {
  name : string;
  config : theme_config;
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
  (* Match array content between [ and ], handling ] inside quoted strings *)
  let run_pattern =
    Re.Pcre.regexp {|run\(\[((?:[^\]'"]|'[^']*'|"[^"]*")*)\]|}
  in
  (* Pattern for standalone array lines like ['class1', 'class2'], *)
  let standalone_array_pattern =
    Re.Pcre.regexp {|^\s*\[((?:[^\]'"]|'[^']*'|"[^"]*")*)\]|}
  in
  let snapshot_start = Re.Pcre.regexp {|toMatchInlineSnapshot\(`|} in
  let snapshot_end = Re.Pcre.regexp {|`\)|} in
  (* Pattern for .toEqual('') which means the classes should produce empty
     output *)
  let empty_expect = Re.Pcre.regexp {|\.toEqual\s*\(\s*['"]{2}\s*\)|} in

  (* Config detection patterns — order matters: most specific first *)
  let compile_css_re = Re.Pcre.regexp {|compileCss\s*\(|} in
  let run_call_re = Re.Pcre.regexp {|\brun\s*\(|} in
  let theme_inline_ref_re =
    Re.Pcre.regexp {|@theme\s+inline\s+reference\s*\{|}
  in
  let theme_inline_re = Re.Pcre.regexp {|@theme\s+inline\s*\{|} in
  let theme_ref_re = Re.Pcre.regexp {|@theme\s+reference\s*\{|} in
  let theme_re = Re.Pcre.regexp {|@theme\s*\{|} in

  let state = ref Outside in
  let current_classes = ref [] in
  let current_config = ref No_theme in

  let flush_test name expected =
    let classes =
      !current_classes |> List.rev |> List.filter is_valid_class
      |> List.sort_uniq String.compare
    in
    if classes <> [] then
      tests := { name; config = !current_config; classes; expected } :: !tests;
    current_classes := []
  in

  List.iter
    (fun line ->
      match !state with
      | Outside -> (
          match Re.exec_opt test_pattern line with
          | Some groups ->
              current_config := No_theme;
              state := InTest (Re.Group.get groups 1)
          | None -> ())
      | InTest name ->
          (* Detect compileCss/run calls to track theme configuration.
             compileCss() resets config (each call has its own @theme). run()
             uses built-in defaults. *)
          if Re.execp compile_css_re line then current_config := No_theme
          else if Re.execp run_call_re line then current_config := Run;

          (* Detect @theme variants within compileCss CSS templates. Most
             specific patterns checked first to avoid partial matches. *)
          if Re.execp theme_inline_ref_re line then
            current_config := Theme_inline_reference
          else if Re.execp theme_inline_re line then
            current_config := Theme_inline
          else if Re.execp theme_ref_re line then
            current_config := Theme_reference
          else if Re.execp theme_re line then current_config := Theme;

          (* Check for run([...]) *)
          (match Re.exec_opt run_pattern line with
          | Some groups ->
              let content = Re.Group.get groups 1 in
              current_classes :=
                List.rev_append
                  (extract_quoted_strings content)
                  !current_classes
          | None -> (
              (* Also check for standalone array lines like ['class1',
                 'class2'], *)
              match Re.exec_opt standalone_array_pattern line with
              | Some groups ->
                  let content = Re.Group.get groups 1 in
                  current_classes :=
                    List.rev_append
                      (extract_quoted_strings content)
                      !current_classes
              | None -> ()));
          (* Check for .toEqual('') which means classes should produce empty
             output. Flush test with empty expected, then clear classes. *)
          if Re.execp empty_expect line then (
            flush_test name (Some "");
            current_classes := [] (* Check for array continuation *))
          else if
            let stripped = strip_quoted line in
            Astring.String.is_infix ~affix:"[" stripped
            && not (Astring.String.is_infix ~affix:"]" stripped)
          then state := InArray name (* Check for snapshot start *)
          else if Re.execp snapshot_start line then
            state := InSnapshot (name, !current_classes, Buffer.create 256)
            (* Check for new test *)
          else if Astring.String.is_prefix ~affix:"test('" line then (
            flush_test name None;
            current_classes := [];
            current_config := No_theme;
            match Re.exec_opt test_pattern line with
            | Some groups -> state := InTest (Re.Group.get groups 1)
            | None -> state := Outside
            (* Check for test end without snapshot *))
          else if Astring.String.is_prefix ~affix:"})" line then (
            flush_test name None;
            current_classes := [];
            current_config := No_theme;
            state := Outside)
      | InArray name ->
          current_classes :=
            List.rev_append (extract_quoted_strings line) !current_classes;
          (* Check for unquoted ] to detect array end — don't be fooled by ]
             inside quoted class names like 'z-[123]' *)
          let stripped = strip_quoted line in
          if Astring.String.is_infix ~affix:"]" stripped then
            state := InTest name
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

  (* Output format: # test-name @config <theme-config> class1 class2 --- css
     output here with newlines preserved <<<>>> *)
  List.iter
    (fun test ->
      Fmt.pr "# %s@." test.name;
      Fmt.pr "@config %s@." (config_to_string test.config);
      Fmt.pr "%s@." (String.concat " " test.classes);
      (match test.expected with
      | Some css ->
          Fmt.pr "---@.";
          Fmt.pr "%s@." css
      | None -> ());
      Fmt.pr "<<<>>>@.")
    tests;

  let with_expected = List.filter (fun t -> t.expected <> None) tests in
  let config_counts =
    List.fold_left
      (fun acc t ->
        let key = config_to_string t.config in
        let count = try List.assoc key acc with Not_found -> 0 in
        (key, count + 1) :: List.filter (fun (k, _) -> k <> key) acc)
      [] tests
  in
  Fmt.epr "Extracted %d test cases (%d with expected CSS)@." (List.length tests)
    (List.length with_expected);
  Fmt.epr "Config breakdown:@.";
  List.iter
    (fun (config, count) -> Fmt.epr "  %s: %d@." config count)
    (List.sort compare config_counts)
