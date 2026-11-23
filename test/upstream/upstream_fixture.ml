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
  source : string;
  name : string;
  config : config;
  classes : string list;
  expected : string option;
  variants : string list;
  theme_vars : (string * string) list;
  theme_modes : (string * string list) list;
  utility_defs : (string * string) list;
  layer_wrap : string option;
  layer_before_theme : bool;
}

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
    (* Text mode, so a fixture written on Windows reads back with LF line ends;
       [in_channel_length] counts the CRs text mode drops, so it cannot size the
       read. *)
    String.split_on_char '\n'
      (In_channel.with_open_text filename In_channel.input_all)

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
  let layer_before_theme = ref false in
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
      ( "@layer-before-theme ",
        fun n arg ->
          match arg with
          | "true" -> layer_before_theme := true
          | "false" -> layer_before_theme := false
          | _ -> malformed filename n arg "true or false" );
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
        layer_before_theme = !layer_before_theme;
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
  | first :: _ -> (
      let first = String.trim first in
      (* A banner that is only the prefix names no count; the empty tail reads
         as none below. *)
      match after_prefix "#! " first with
      | None -> None
      | Some tail ->
          let count =
            match String.index_opt tail ' ' with
            | Some i -> String.sub tail 0 i
            | None -> tail
          in
          int_of_string_opt count)

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
let is_runtime_var name =
  String.starts_with ~prefix:"tw-" name && String.length name > 3
