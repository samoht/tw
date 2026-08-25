(** Extract test cases from tailwindcss utilities.test.ts

    This script parses the upstream Tailwind CSS test file and extracts:
    - Test names
    - Theme configuration ({!theme_config})
    - Utility class names
    - Expected CSS output from toMatchInlineSnapshot

    {2 Generating the fixtures}

    The parity target is Tailwind CSS {b v4.3.3} (the version enforced by
    [Tailwind_gen.required_version]). The committed [utilities.txt] and
    [variants.txt] were generated from the [v4.3.3] tag of the upstream test
    suite. To regenerate:

    {v
    # Clone tailwindcss (or use an existing clone) at the v4.3.3 tag
    git clone https://github.com/tailwindlabs/tailwindcss.git /tmp/tailwindcss
    cd /tmp/tailwindcss && git checkout v4.3.3

    # Extract both fixtures
    dune exec test/upstream/extract_tests.exe -- \
      /tmp/tailwindcss/packages/tailwindcss/src/utilities.test.ts \
      > test/upstream/utilities.txt
    dune exec test/upstream/extract_tests.exe -- \
      /tmp/tailwindcss/packages/tailwindcss/src/variants.test.ts \
      > test/upstream/variants.txt
    v}

    {b Do NOT edit the .txt fixtures directly.} If test expectations need
    updating, regenerate from the pinned Tailwind version or fix the extraction
    script.

    Usage: dune exec test/upstream/extract_tests.exe -- <utilities.test.ts> *)

(** Check if a string looks like a class name (vs a directive or invalid
    syntax). We only filter out things that aren't class names at all - bare
    utility names and invalid suffixes are kept as negative tests. *)
let is_valid_class s =
  String.length s > 0
  (* Filter out @layer, @apply, etc. - these are directives, not classes *)
  && s.[0] <> '@'
  (* Filter out function-like syntax (e.g. theme(...)) - but allow parens inside
     arbitrary brackets like z-[var(--value)] or bg-[rgb(0,0,0)], and also allow
     parenthesized bracket notation like mask-t-from-(color:--my-var) where the
     paren is preceded by a dash *)
  && ((not (String.contains s '('))
     || String.contains s '['
     || Re.execp (Re.Pcre.regexp {|-\(|}) s)

(* Candidates are quoted with single or double quotes; JS uses double quotes
   when the candidate itself contains a single quote (e.g.
   ["data-[foo$='bar'_i]:flex"]). Match either quote style so the outer quote
   wins instead of capturing an inner single-quoted fragment. *)
let extract_quoted_strings line =
  let pattern = Re.Pcre.regexp {|"([^"]*)"|'([^']*)'|} in
  Re.all pattern line
  |> List.map (fun m ->
      if Re.Group.test m 1 then Re.Group.get m 1 else Re.Group.get m 2)

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
  variants : string list;
      (** [matchVariant] directives, e.g. ["is-data ..."]. *)
  theme_vars : (string * string) list;
      (** [@theme] token declarations ([--name: value]) from the test's CSS
          template. Captures tokens (e.g. [text-shadow-2xs]) that Tailwind
          inlines into utilities rather than emitting to [:root], so the runner
          can reconstruct the test's theme as token overrides. *)
}

(* Parse [matchVariant('name', (value) => `template`, { values: {...} })] calls
   into directive strings: "name <template-with-{}> KEY=value ...". The DEFAULT
   key is kept verbatim and mapped to the default slot by the test runner. *)
let parse_match_variants content =
  let re =
    Re.Pcre.regexp
      {|matchVariant\(\s*'([^']+)'\s*,\s*\(value\)\s*=>\s*`([^`]*)`\s*,\s*\{[^{]*values:\s*\{([^}]*)\}|}
  in
  let value_re = Re.Pcre.regexp {|([A-Za-z_][A-Za-z0-9_-]*)\s*:\s*'([^']*)'|} in
  let tbl = Hashtbl.create 4 in
  List.iter
    (fun m ->
      let name = Re.Group.get m 1 in
      let template =
        (* `${value}` -> `{}` placeholder *)
        Re.replace
          (Re.Pcre.regexp {|\$\{value\}|})
          ~f:(fun _ -> "{}")
          (Re.Group.get m 2)
      in
      let pairs =
        Re.all value_re (Re.Group.get m 3)
        |> List.map (fun p -> Re.Group.get p 1 ^ "=" ^ Re.Group.get p 2)
      in
      Hashtbl.replace tbl name (String.concat " " (name :: template :: pairs)))
    (Re.all re content);
  tbl

(* Parse [@custom-variant <name> { @container <header> { @slot } }] blocks into
   directive strings ["container <name> <header>"], e.g. ["container has-c foo
   style(--c)"]. The runner registers these as structural container-query
   variants. *)
let parse_custom_variant_containers content =
  let re =
    Re.Pcre.regexp
      {|@custom-variant\s+([A-Za-z0-9_-]+)\s*\{\s*@container\s+([^{\n]+?)\s*\{\s*@slot|}
  in
  let tbl = Hashtbl.create 4 in
  List.iter
    (fun m ->
      let name = Re.Group.get m 1 in
      let header = Re.Group.get m 2 |> String.trim in
      Hashtbl.replace tbl name ("container " ^ name ^ " " ^ header))
    (Re.all re content);
  tbl

(* A [@theme] block is read as a character stream, not line by line: a
   declaration value runs to the [;] that ends it at the block's own nesting
   level, and Prettier wraps a long value (a font stack, a transition list) over
   several lines. A [;] [{] [}] inside a string, a comment or a nested bracket
   ends nothing. Whitespace outside strings folds to a single space so the
   captured value fits the one-line [@theme-var] fixture format. *)
type theme_scanner = {
  decl : Buffer.t;  (** the declaration read so far *)
  mutable quote : char option;  (** the open string delimiter, if any *)
  mutable in_comment : bool;
  mutable depth : int;  (** bracket nesting; [0] is the block's own level *)
  mutable pending_space : bool;
}

let theme_scanner () =
  {
    decl = Buffer.create 128;
    quote = None;
    in_comment = false;
    depth = 0;
    pending_space = false;
  }

(* [--spacing-*: initial] resets a whole namespace, so [*] belongs in a token
   name: without it the reset reads as no declaration at all and the test that
   sets it silently loses its theme. *)
let theme_decl_re = Re.Pcre.regexp {|^--([A-Za-z0-9*-]+)\s*:\s*(.*)$|}

let scanner_add s c =
  if s.pending_space && Buffer.length s.decl > 0 then Buffer.add_char s.decl ' ';
  s.pending_space <- false;
  Buffer.add_char s.decl c

(* Emit the declaration read so far, if it is a [--name: value] one. *)
let scanner_flush s emit =
  let text = String.trim (Buffer.contents s.decl) in
  Buffer.clear s.decl;
  s.pending_space <- false;
  match Re.exec_opt theme_decl_re text with
  | Some g -> emit (Re.Group.get g 1, String.trim (Re.Group.get g 2))
  | None -> ()

(* Read one line of an open [@theme] block from [start]. Returns [true] while
   the block is still open. *)
let scanner_feed s line ~start emit =
  let len = String.length line in
  let closed = ref false in
  let i = ref start in
  while (not !closed) && !i < len do
    let c = line.[!i] in
    (match s.quote with
    | Some q ->
        scanner_add s c;
        if c = '\\' && !i + 1 < len then (
          scanner_add s line.[!i + 1];
          incr i)
        else if c = q then s.quote <- None
    | None -> (
        if s.in_comment then (
          if c = '*' && !i + 1 < len && line.[!i + 1] = '/' then (
            s.in_comment <- false;
            s.pending_space <- true;
            incr i))
        else if c = '/' && !i + 1 < len && line.[!i + 1] = '*' then (
          s.in_comment <- true;
          incr i)
        else
          match c with
          | ' ' | '\t' | '\r' -> s.pending_space <- true
          | '\'' | '"' ->
              s.quote <- Some c;
              scanner_add s c
          | '(' | '[' | '{' ->
              s.depth <- s.depth + 1;
              scanner_add s c
          | ')' | ']' ->
              if s.depth > 0 then s.depth <- s.depth - 1;
              scanner_add s c
          | '}' ->
              if s.depth = 0 then (
                scanner_flush s emit;
                closed := true)
              else (
                s.depth <- s.depth - 1;
                scanner_add s c)
          | ';' -> if s.depth = 0 then scanner_flush s emit else scanner_add s c
          | c -> scanner_add s c));
    incr i
  done;
  (* The line break itself is whitespace within the value. *)
  if not !closed then s.pending_space <- true;
  not !closed

(* [.toEqual('')] asserts that a candidate list compiles to nothing. Like a
   [@theme] declaration, the assertion is read as a character stream: Prettier
   wraps the call when the [expect(...)] head is long, leaving the argument and
   the closing [)] on later lines. A bracket inside a string closes nothing. *)
type expect_scanner = {
  arg : Buffer.t;  (** the argument text read so far *)
  mutable arg_quote : char option;  (** the open string delimiter, if any *)
  mutable arg_depth : int;  (** bracket nesting; [0] is the call's own level *)
}

let expect_scanner () =
  { arg = Buffer.create 32; arg_quote = None; arg_depth = 0 }

(* Read one line of an open [.toEqual(] call from [start]. Returns [Some arg]
   with the argument text once the closing [)] is read, [None] while the call is
   still open. *)
let expect_feed s line ~start =
  let len = String.length line in
  let closed = ref None in
  let i = ref start in
  while Option.is_none !closed && !i < len do
    let c = line.[!i] in
    (match s.arg_quote with
    | Some q ->
        Buffer.add_char s.arg c;
        if c = '\\' && !i + 1 < len then (
          Buffer.add_char s.arg line.[!i + 1];
          incr i)
        else if c = q then s.arg_quote <- None
    | None -> (
        match c with
        | '\'' | '"' | '`' ->
            s.arg_quote <- Some c;
            Buffer.add_char s.arg c
        | '(' | '[' | '{' ->
            s.arg_depth <- s.arg_depth + 1;
            Buffer.add_char s.arg c
        | ']' | '}' ->
            if s.arg_depth > 0 then s.arg_depth <- s.arg_depth - 1;
            Buffer.add_char s.arg c
        | ')' ->
            if s.arg_depth = 0 then closed := Some (Buffer.contents s.arg)
            else (
              s.arg_depth <- s.arg_depth - 1;
              Buffer.add_char s.arg c)
        | c -> Buffer.add_char s.arg c));
    incr i
  done;
  !closed

(* The empty assertion is [.toEqual('')]; wrapping it leaves the argument on its
   own line, and Prettier gives that line a trailing comma. *)
let is_empty_string_arg arg =
  let arg = String.trim arg in
  let arg =
    let n = String.length arg in
    if n > 0 && arg.[n - 1] = ',' then String.trim (String.sub arg 0 (n - 1))
    else arg
  in
  arg = "''" || arg = {|""|}

(* A [test(...)] block opens with the call's own indentation and closes at the
   [})] sitting at that same indentation: Prettier indents every line of the
   body further, so the pair is unambiguous, and a block nested in one or more
   [describe(...)] blocks is read like any other. The name is a character stream
   from the opening paren, so a [(], a [)] or a quote of another style inside
   the name closes nothing, and the three quote styles are all read.

   [test.each([...])(name, fn)] is not a [test(] head and stays out: the three
   such blocks in v4.3.3 assert on Tailwind's own JS helpers
   ([isValidStaticUtilityName], [compoundsForSelectors]), never call [run] and
   compile no CSS. *)
let test_open_re = Re.Pcre.regexp {|^([ \t]*)test[ \t]*\(|}

(* Read the quoted first argument of a [test(] call from [start], just past the
   opening paren, and return it as it is spelled in the source. [None] when the
   argument is not a string literal closed on this line. *)
let test_name line ~start =
  let len = String.length line in
  let i = ref start in
  while !i < len && (line.[!i] = ' ' || line.[!i] = '\t') do
    incr i
  done;
  if !i >= len then None
  else
    match line.[!i] with
    | ('\'' | '"' | '`') as quote ->
        let buf = Buffer.create 64 in
        let closed = ref false in
        incr i;
        while (not !closed) && !i < len do
          let c = line.[!i] in
          if c = '\\' && !i + 1 < len then (
            Buffer.add_char buf c;
            Buffer.add_char buf line.[!i + 1];
            i := !i + 2)
          else if c = quote then (
            closed := true;
            incr i)
          else (
            Buffer.add_char buf c;
            incr i)
        done;
        if !closed then Some (Buffer.contents buf) else None
    | _ -> None

(* [Some (indent, name)] when [line] opens a test block. *)
let test_open line =
  match Re.exec_opt test_open_re line with
  | None -> None
  | Some g ->
      let start = snd (Re.Group.offset g 0) in
      Option.map (fun name -> (Re.Group.get g 1, name)) (test_name line ~start)

(* [true] when [line] is the [})] closing a block opened at [indent]. *)
let test_close ~indent line =
  let n = String.length indent in
  String.length line >= n + 2
  && String.sub line 0 n = indent
  && line.[n] = '}'
  && line.[n + 1] = ')'

type parse_state =
  | Outside
  | In_test of string
  | In_array of string
  | In_snapshot of string * string list * Buffer.t

let parse_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let tests = ref [] in
  let lines = String.split_on_char '\n' content in
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
  (* Head of the [.toEqual(...)] assertion; the argument is read from there to
     the [)] that closes the call. *)
  let expect_open = Re.Pcre.regexp {|\.toEqual\s*\(|} in

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
  (* Indentation of the [test(] opening the block being read, so its own [})] is
     told apart from one closing a [describe(...)] around it. *)
  let test_indent = ref "" in
  (* Cases produced by the block being read, held back until it ends. A block
     that defines its own [@utility] is dropped whole: the extractor carries a
     candidate list and the [@theme] tokens, not the CSS around them, so the
     expected snapshot of such a block holds rules for utilities the runner is
     never told about and no run can produce. In v4.3.3 that is the
     [describe('custom utilities')] block and the one [@utility container]
     case. *)
  let block_tests = ref [] in
  let saw_custom_utility = ref false in
  let utility_def_re = Re.Pcre.regexp {|@utility\s|} in
  let current_classes = ref [] in
  let current_config = ref No_theme in
  (* A single test can mix a keep [@theme { ... }] block with an inline [@theme
     inline { ... }] block (e.g. filter: keep the sizes, inline
     [--drop-shadow-multi]). The keep block wins the config so the runner keeps
     those tokens (deriving the keep-set from the expected CSS) and inlines only
     the ones the fixture actually inlined; tagging such a case [theme-inline]
     would hand the runner an empty keep-set and inline everything. *)
  let saw_keep_theme = ref false in
  let variant_defs = parse_match_variants content in
  Hashtbl.iter
    (fun k v -> Hashtbl.replace variant_defs k v)
    (parse_custom_variant_containers content);
  let match_variant_use = Re.Pcre.regexp {|matchVariant\(\s*'([^']+)'|} in
  let custom_variant_use =
    Re.Pcre.regexp {|@custom-variant\s+([A-Za-z0-9_-]+)|}
  in
  let current_variant_names = ref [] in
  (* Capture [@theme] token declarations [--name: value;]. [in_theme] holds the
     scanner of the active [@theme {...}] block within a compileCss template. *)
  let current_theme_vars = ref [] in
  let in_theme = ref None in
  let theme_open_re = Re.Pcre.regexp {|@theme\b[^{]*\{|} in
  (* Scanner of the [.toEqual(] call still being read, if any. *)
  let in_expect = ref None in

  let flush_test name expected =
    let classes =
      !current_classes |> List.rev |> List.filter is_valid_class
      |> List.sort_uniq String.compare
    in
    let variants =
      !current_variant_names |> List.rev
      |> List.filter_map (fun n -> Hashtbl.find_opt variant_defs n)
    in
    if classes <> [] then
      block_tests :=
        {
          name;
          config = !current_config;
          classes;
          expected;
          variants;
          theme_vars = List.rev !current_theme_vars;
        }
        :: !block_tests;
    current_classes := [];
    current_variant_names := [];
    current_theme_vars := [];
    in_theme := None;
    in_expect := None
  in

  let close_block () =
    if not !saw_custom_utility then tests := !block_tests @ !tests;
    block_tests := [];
    saw_custom_utility := false
  in

  List.iter
    (fun line ->
      if Re.execp utility_def_re line then saw_custom_utility := true;
      match !state with
      | Outside -> (
          match test_open line with
          | Some (indent, name) ->
              current_config := No_theme;
              saw_keep_theme := false;
              saw_custom_utility := false;
              test_indent := indent;
              state := In_test name
          | None -> ())
      | In_test name -> (
          (* Detect compileCss/run calls to track theme configuration.
             compileCss() resets config (each call has its own @theme). run()
             uses built-in defaults. *)
          if Re.execp compile_css_re line then (
            current_config := No_theme;
            saw_keep_theme := false)
          else if Re.execp run_call_re line then current_config := Run;

          (* Record any matchVariant plugin used by this test so the runner can
             register it before compiling. *)
          (match Re.exec_opt match_variant_use line with
          | Some g ->
              current_variant_names :=
                Re.Group.get g 1 :: !current_variant_names
          | None -> ());

          (* Record [@custom-variant <name>] definitions used by this test. *)
          (match Re.exec_opt custom_variant_use line with
          | Some g ->
              current_variant_names :=
                Re.Group.get g 1 :: !current_variant_names
          | None -> ());

          (* Detect @theme variants within compileCss CSS templates. Most
             specific patterns checked first to avoid partial matches. *)
          if Re.execp theme_inline_ref_re line then (
            if not !saw_keep_theme then current_config := Theme_inline_reference)
          else if Re.execp theme_inline_re line then (
            if not !saw_keep_theme then current_config := Theme_inline)
          else if Re.execp theme_ref_re line then (
            saw_keep_theme := true;
            current_config := Theme_reference)
          else if Re.execp theme_re line then (
            saw_keep_theme := true;
            current_config := Theme);

          (* Capture [@theme] token declarations. Enter just after the opener's
             [{] and read on until the block's closing [}]. *)
          let scan_from =
            match !in_theme with
            | Some s -> Some (s, 0)
            | None -> (
                match Re.exec_opt theme_open_re line with
                | Some g ->
                    let s = theme_scanner () in
                    in_theme := Some s;
                    Some (s, snd (Re.Group.offset g 0))
                | None -> None)
          in
          (match scan_from with
          | None -> ()
          | Some (s, start) ->
              let emit v = current_theme_vars := v :: !current_theme_vars in
              if not (scanner_feed s line ~start emit) then in_theme := None);

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
          let saw_empty_expect =
            let scan =
              match !in_expect with
              | Some s -> Some (s, 0)
              | None -> (
                  match Re.exec_opt expect_open line with
                  | Some g ->
                      let s = expect_scanner () in
                      in_expect := Some s;
                      Some (s, snd (Re.Group.offset g 0))
                  | None -> None)
            in
            match scan with
            | None -> false
            | Some (s, start) -> (
                match expect_feed s line ~start with
                | None -> false
                | Some arg ->
                    in_expect := None;
                    is_empty_string_arg arg)
          in
          if saw_empty_expect then (
            flush_test name (Some "");
            current_classes := [] (* Check for array continuation *))
          else if
            let stripped = strip_quoted line in
            Astring.String.is_infix ~affix:"[" stripped
            && not (Astring.String.is_infix ~affix:"]" stripped)
          then state := In_array name (* Check for snapshot start *)
          else if Re.execp snapshot_start line then
            state := In_snapshot (name, !current_classes, Buffer.create 256)
            (* Check for new test *)
          else
            match test_open line with
            | Some (indent, next) ->
                flush_test name None;
                close_block ();
                current_classes := [];
                current_config := No_theme;
                test_indent := indent;
                state := In_test next
            (* Check for test end without snapshot *)
            | None ->
                if test_close ~indent:!test_indent line then (
                  flush_test name None;
                  close_block ();
                  current_classes := [];
                  current_config := No_theme;
                  state := Outside))
      | In_array name ->
          current_classes :=
            List.rev_append (extract_quoted_strings line) !current_classes;
          (* Check for unquoted ] to detect array end — don't be fooled by ]
             inside quoted class names like 'z-[123]' *)
          let stripped = strip_quoted line in
          if Astring.String.is_infix ~affix:"]" stripped then
            state := In_test name
      | In_snapshot (name, classes, buf) ->
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
            state := In_test name)
          else (
            Buffer.add_string buf line;
            Buffer.add_char buf '\n'))
    lines;
  close_block ();

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
      List.iter (fun v -> Fmt.pr "@variant %s@." v) test.variants;
      List.iter (fun (n, v) -> Fmt.pr "@theme-var %s %s@." n v) test.theme_vars;
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
