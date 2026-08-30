(** Reader for the generated upstream fixtures ([utilities.txt],
    [variants.txt]).

    The fixtures are one block per upstream test, separated by [<<<>>>]:

    {v
    # <test name>
    @config <theme config>
    @variant <matchVariant directive>     (optional, repeatable)
    @theme-var <name> <value>             (optional, repeatable)
    @theme-mode <name> <modifiers>        (optional, repeatable)
    @utility-def <name> <body>            (optional, repeatable)
    @layer-wrap <layer>                   (optional)
    <space-separated class list>
    ---
    <expected CSS>
    v}

    Both the fixture replay in [test/upstream/test.ml] and the parse-parity gate
    in [test/test_tw.ml] read the same corpus, so they read it here: two readers
    drifting apart is two different corpora behind one set of fixtures. See
    [extract_tests.ml] for how the fixtures are generated. *)

type config =
  | Theme
  | Theme_inline
  | Theme_reference
  | Theme_inline_reference
  | No_theme
  | Run

exception Malformed of string

let fail_malformed fmt = Fmt.kstr (fun msg -> raise (Malformed msg)) fmt

let config_of_string = function
  | "theme" -> Theme
  | "theme-inline" -> Theme_inline
  | "theme-reference" -> Theme_reference
  | "theme-inline-reference" -> Theme_inline_reference
  | "none" -> No_theme
  | "run" -> Run
  | name -> fail_malformed "unknown @config name %S" name

type case = {
  source : string;  (** Fixture the case was read from. *)
  name : string;
  config : config;
  classes : string list;
  expected : string option;
      (** The [---] section, or [None] for a block the extractor wrote without
          one: an upstream test that asserts the compile throws has no CSS to
          replay. *)
  variants : string list;  (** [matchVariant] directive lines for this test. *)
  theme_vars : (string * string) list;
      (** [@theme] token overrides (name, value) captured from the test's CSS
          template by the extractor (e.g. text-shadow sizes Tailwind inlines).
      *)
  theme_modes : (string * string list) list;
      (** The modifiers of the [@theme] block each token was declared in, for
          the tokens whose block had any. [inline] and [reference] change how a
          token reads, and one test can declare two tokens of a namespace in
          blocks that differ, so the mode belongs to the token. *)
  utility_defs : (string * string) list;
      (** [@utility] declarations (name, body) the test's CSS template makes. A
          case that has any is compiled through the declared-utility path rather
          than class by class. *)
  layer_wrap : string option;
      (** The layer the test's CSS template compiles [@tailwind utilities] into,
          when it names one. Tailwind puts the generated utilities in it and
          everything else beside it. *)
}

(** Split a class line by spaces, but don't split inside brackets. *)
let split_classes line =
  let len = String.length line in
  let buf = Buffer.create 64 in
  let acc = ref [] in
  let depth = ref 0 in
  for i = 0 to len - 1 do
    let c = line.[i] in
    if c = '[' then (
      incr depth;
      Buffer.add_char buf c)
    else if c = ']' then (
      decr depth;
      Buffer.add_char buf c)
    else if c = ' ' && !depth = 0 then (
      let s = Buffer.contents buf in
      if s <> "" then acc := s :: !acc;
      Buffer.clear buf)
    else Buffer.add_char buf c
  done;
  let s = Buffer.contents buf in
  if s <> "" then acc := s :: !acc;
  List.rev !acc

let lines_of filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    String.split_on_char '\n' content

(* A line the reader cannot place: the fixtures are machine-written, so a line
   outside the grammar means the extractor and the reader have drifted apart, or
   the fixture was edited by hand. Either way it is a bug in this repository and
   not an input to tolerate -- a reader that resumes its scan on such a line
   drops the surrounding block with the case count as its only trace. *)
let malformed filename line text expectation =
  fail_malformed
    "%s:%d: expected %s, read %S. The upstream fixtures are generated: \
     regenerate with extract_tests.exe rather than editing them."
    filename line expectation text

let after_prefix prefix s =
  if String.starts_with ~prefix s then
    let n = String.length prefix in
    Some (String.sub s n (String.length s - n))
  else None

let split_first_space s =
  Option.map
    (fun i ->
      (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)))
    (String.index_opt s ' ')

(* A directive line is "<keyword> <name> <rest>"; the name runs to the first
   space and the rest is kept as written. A keyword carrying neither is
   malformed rather than a class list that happens to start with an [@]. *)
let named_pair filename line keyword arg =
  match split_first_space arg with
  | Some pair -> pair
  | None -> malformed filename line arg (keyword ^ "<name> <value>")

(* One block, in the shape [extract_tests.ml] writes it: a [# <name>] header, an
   [@config] line, the repeatable directives, the class list, then [---] and the
   expected CSS. An upstream test that asserts the compile throws is written
   with no [---] and nothing after the class list.

   The expected CSS is free-form and runs to the block's end, so it is the one
   region a stray line can hide in; everything above it is checked. *)
let parse_block filename block =
  let header_line, name, body =
    match block with
    | [] -> fail_malformed "%s: an empty block (two <<<>>> in a row)" filename
    | (n, header) :: rest -> (
        let header = String.trim header in
        match after_prefix "# " header with
        | Some name -> (n, name, rest)
        | None -> malformed filename n header "a block header (# <name>)")
  in
  let config = ref No_theme in
  let variants = ref [] in
  let theme_vars = ref [] in
  let theme_modes = ref [] in
  let utility_defs = ref [] in
  let layer_wrap = ref None in
  let read_config n arg =
    match config_of_string arg with
    | c -> config := c
    | exception Malformed _ ->
        malformed filename n arg
          "one of @config theme, theme-inline, theme-reference, \
           theme-inline-reference, none, run"
  in
  let handlers =
    [
      ("@config ", read_config);
      ("@variant ", fun _ arg -> variants := arg :: !variants);
      ( "@theme-var ",
        fun n arg ->
          theme_vars := named_pair filename n "@theme-var " arg :: !theme_vars
      );
      ( "@theme-mode ",
        fun n arg ->
          let token, modes = named_pair filename n "@theme-mode " arg in
          theme_modes := (token, String.split_on_char ' ' modes) :: !theme_modes
      );
      ( "@utility-def ",
        fun n arg ->
          utility_defs :=
            named_pair filename n "@utility-def " arg :: !utility_defs );
      ("@layer-wrap ", fun _ arg -> layer_wrap := Some arg);
    ]
  in
  let rec directives lines =
    match lines with
    | [] -> []
    | (n, line) :: rest -> (
        let line = String.trim line in
        let hit =
          List.find_map
            (fun (prefix, handle) ->
              Option.map (fun arg -> (handle, arg)) (after_prefix prefix line))
            handlers
        in
        match hit with
        | Some (handle, arg) ->
            handle n arg;
            directives rest
        | None -> lines)
  in
  match directives body with
  | [] ->
      malformed filename header_line ("# " ^ name)
        "a class list after the block header"
  | (n, classes_line) :: after ->
      let classes_line = String.trim classes_line in
      if classes_line = "---" then
        malformed filename n classes_line "a class list before ---";
      let expected =
        match after with
        | [] -> None
        | (n, separator) :: css ->
            let separator = String.trim separator in
            if separator <> "---" then
              malformed filename n separator "--- or the end of the block";
            Some (String.trim (String.concat "\n" (List.map snd css)))
      in
      {
        source = filename;
        name;
        config = !config;
        classes = split_classes classes_line;
        expected;
        variants = List.rev !variants;
        theme_vars = List.rev !theme_vars;
        theme_modes = List.rev !theme_modes;
        utility_defs = List.rev !utility_defs;
        layer_wrap = !layer_wrap;
      }

let read filename =
  let numbered = List.mapi (fun i line -> (i + 1, line)) (lines_of filename) in
  (* The [#!] banner is provenance rather than a block, and the extractor writes
     it first or not at all. *)
  let numbered =
    match numbered with
    | (_, first) :: rest
      when String.starts_with ~prefix:"#!" (String.trim first) ->
        rest
    | lines -> lines
  in
  let rec split current blocks lines =
    match lines with
    | [] ->
        (* The extractor closes every block, so what follows the last separator
           is the file's trailing newline and nothing else. *)
        List.iter
          (fun (n, line) ->
            if String.trim line <> "" then
              malformed filename n line "a block closed by <<<>>>")
          (List.rev current);
        List.rev blocks
    | (n, line) :: rest when String.trim line = "<<<>>>" -> (
        match List.rev current with
        | [] -> malformed filename n line "a block before <<<>>>"
        | block -> split [] (block :: blocks) rest)
    | entry :: rest -> split (entry :: current) blocks rest
  in
  List.map (parse_block filename) (split [] [] numbered)

(* Blocks are counted on the separator the reader splits on, so the count is the
   reader's own notion of a block rather than a second one beside it. *)
let blocks filename =
  List.length
    (List.filter (fun line -> String.trim line = "<<<>>>") (lines_of filename))

(* The banner the extractor writes: "#! <n> blocks extracted from ...". It
   starts with [#!] rather than [# ] so the block scan walks past it. *)
let declared_blocks filename =
  match lines_of filename with
  | [] -> None
  | first :: _ ->
      let first = String.trim first in
      let prefix = "#! " in
      let n = String.length prefix in
      if String.length first <= n || String.sub first 0 n <> prefix then None
      else
        let tail = String.sub first n (String.length first - n) in
        let count =
          match String.index_opt tail ' ' with
          | Some i -> String.sub tail 0 i
          | None -> tail
        in
        int_of_string_opt count

(* A fixture is found beside the executable under the dune sandbox, and under
   [upstream/] or [test/upstream/] when the test runs from a parent
   directory. *)
let path basename =
  List.find_opt Sys.file_exists
    [
      basename;
      Filename.concat "upstream" basename;
      Filename.concat "test/upstream" basename;
    ]

(* The [:root] custom properties a fixture's expected CSS declares, so a case's
   theme tokens can be read back out of the output Tailwind produced for it. *)
let extract_root_vars expected =
  let pattern = Re.Pcre.regexp {|--([a-zA-Z0-9_-]+):\s*([^;}]+)|} in
  let matches = Re.all pattern expected in
  List.filter_map
    (fun m ->
      try
        let name = Re.Group.get m 1 in
        let value = String.trim (Re.Group.get m 2) in
        Some (name, value)
      with Not_found | Failure _ -> None)
    matches

(* The [var(--name, fallback)] pairs in a fixture's expected CSS. A [@theme
   reference] block declares no [:root] value, so the fallback is the only place
   its token appears. *)
let extract_var_fallbacks expected =
  let pattern =
    Re.Pcre.regexp
      {|var\(--([a-zA-Z0-9_-]+),\s*(var\(--[a-zA-Z0-9_-]+\)|[^)]+)\)|}
  in
  let matches = Re.all pattern expected in
  List.filter_map
    (fun m ->
      try
        let name = Re.Group.get m 1 in
        let fallback = String.trim (Re.Group.get m 2) in
        Some (name, fallback)
      with Not_found | Failure _ -> None)
    matches

(* A [--tw-*] variable is a utility's own output rather than a theme token the
   test declared, so it must not become a token override. *)
let is_runtime_var name = String.length name > 3 && String.sub name 0 3 = "tw-"
