(** Extract test cases from tailwindcss utilities.test.ts

    This script parses the upstream Tailwind CSS test file and extracts:
    - Test names
    - Theme configuration ({!theme_config})
    - [@utility] declarations and the layer the template compiles into
    - Utility class names
    - Expected CSS output from toMatchInlineSnapshot

    A block that declares its own [@utility] is carried only when the runner
    reproduces it; {!unreplayable} says which shapes it does not, and each drop
    is named on stderr with its reason.

    {2 Generating the fixtures}

    The parity target is Tailwind CSS {b v4.3.3} (the version enforced by
    [Tailwind_gen.required_version]). The committed [utilities.txt] and
    [variants.txt] were generated from the [v4.3.3] tag of the upstream test
    suite. To regenerate:

    {v
    # Clone tailwindcss (or use an existing clone) at the v4.3.3 tag
    git clone https://github.com/tailwindlabs/tailwindcss.git tmp/tailwindcss
    cd tmp/tailwindcss && git checkout v4.3.3

    # Confirm the tag before extracting: another version rewrites values
    # (v4.3.2 spells 22 of the lengths v4.3.3 spells 0px as 0).
    head -3 packages/tailwindcss/package.json

    # Extract both fixtures
    dune exec test/upstream/extract_tests.exe -- \
      tmp/tailwindcss/packages/tailwindcss/src/utilities.test.ts \
      > test/upstream/utilities.txt
    dune exec test/upstream/extract_tests.exe -- \
      tmp/tailwindcss/packages/tailwindcss/src/variants.test.ts \
      > test/upstream/variants.txt
    v}

    {b Do NOT edit the .txt fixtures directly.} If test expectations need
    updating, regenerate from the pinned Tailwind version or fix the extraction
    script. A case upstream does not have is a tw regression test and belongs in
    its own [test_<module>.ml], never here: the next regeneration would drop it
    silently.

    {2 The provenance banner}

    Each generated fixture opens with a [#!] line naming this script and the
    number of blocks it wrote. [test/upstream/test.ml] counts the blocks back
    and fails when the two disagree, so a block added to a generated fixture by
    hand is reported at once instead of being dropped, unremarked, by the next
    regeneration.

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
  theme_modes : (string * string list) list;
      (** The modifiers of the [@theme] block each token was declared in, for
          the tokens whose block had any. [inline] and [reference] change how a
          token reads rather than what it is, and a test can put two tokens of
          one namespace in blocks that differ, so the mode belongs to the token
          rather than to the test. *)
  utility_defs : (string * string) list;
      (** [@utility <name> { <body> }] declarations from the test's CSS
          template, so the runner can compile the case through the same
          declared-utility path the CLI uses for a project entrypoint. *)
  layer_wrap : string option;
      (** The layer the test's CSS template puts [@tailwind utilities] in, when
          it puts it in one at all. Tailwind emits the generated utilities
          inside that layer and everything else beside it. *)
  layer_before_theme : bool;
      (** Whether that layer appeared before a later [@theme] block. Source
          order decides whether Tailwind writes the generated layer before or
          after the theme declarations beside it. *)
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

(* An [@utility <name> { ... }] body is read the same way, but whole: a nested
   block, a [;] or a string inside it ends nothing, and only the [}] at the
   block's own level closes it. Whitespace outside strings folds to a single
   space so the body fits the one-line [@utility-def] fixture format. *)
type block_scanner = {
  text : Buffer.t;  (** the body read so far *)
  mutable delim : char option;  (** the open string delimiter, if any *)
  mutable commented : bool;
  mutable nesting : int;  (** bracket nesting; [0] is the block's own level *)
  mutable folded_space : bool;
}

let block_scanner () =
  {
    text = Buffer.create 128;
    delim = None;
    commented = false;
    nesting = 0;
    folded_space = false;
  }

let block_add s c =
  if s.folded_space && Buffer.length s.text > 0 then Buffer.add_char s.text ' ';
  s.folded_space <- false;
  Buffer.add_char s.text c

(* Read one line of an open [@utility] body from [start]. Returns [true] while
   the body is still open. *)
let block_feed s line ~start =
  let len = String.length line in
  let closed = ref false in
  let i = ref start in
  while (not !closed) && !i < len do
    let c = line.[!i] in
    (match s.delim with
    | Some q ->
        block_add s c;
        if c = '\\' && !i + 1 < len then (
          block_add s line.[!i + 1];
          incr i)
        else if c = q then s.delim <- None
    | None -> (
        if s.commented then (
          if c = '*' && !i + 1 < len && line.[!i + 1] = '/' then (
            s.commented <- false;
            s.folded_space <- true;
            incr i))
        else if c = '/' && !i + 1 < len && line.[!i + 1] = '*' then (
          s.commented <- true;
          incr i)
        else
          match c with
          | ' ' | '\t' | '\r' -> s.folded_space <- true
          | '\'' | '"' ->
              s.delim <- Some c;
              block_add s c
          | '(' | '[' | '{' ->
              s.nesting <- s.nesting + 1;
              block_add s c
          | ')' | ']' ->
              if s.nesting > 0 then s.nesting <- s.nesting - 1;
              block_add s c
          | '}' ->
              if s.nesting = 0 then closed := true
              else (
                s.nesting <- s.nesting - 1;
                block_add s c)
          | c -> block_add s c));
    incr i
  done;
  if not !closed then s.folded_space <- true;
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

let contains_apply body = Astring.String.is_infix ~affix:"@apply" body

(* The theme tokens a definition body reads with [var(--name)]. *)
let var_reference_re = Re.Pcre.regexp {|var\(\s*--([A-Za-z0-9_-]+)|}

let var_references body =
  List.map (fun g -> Re.Group.get g 1) (Re.all var_reference_re body)

(* The static theme-token catalog is populated by the families that own it.
   Building the base sheet once loads the same complete snapshot the replay
   runner uses; the extractor can then distinguish a missing theme token from an
   arbitrary runtime custom property. *)
let init_theme_tokens = lazy (ignore (Tw.to_css ~base:true []))

(* [@utility example-*] declares a functional utility: its body is a template
   whose [--value(...)] and [--modifier(...)] reads stand for the candidate's
   own value, so no CSS parser reads it as written. *)
let is_functional (name, _) = String.contains name '*'

(* The property of a declaration cascade rejects the value of, if the body has
   one. *)
let unreadable_declaration body =
  match Cascade.Css.of_string (String.concat "" [ ".x{"; body; "}" ]) with
  | Error _ -> Some "the definition body"
  | Ok { warnings; _ } ->
      List.find_map
        (fun (w : Cascade.Error.t) ->
          match w.kind with
          | Cascade.Error.Bad_value { property; _ } -> Some property
          | _ -> None)
        warnings

(* The declarations a snapshot gives one class: the [;] inside every [.name {
   ... }] block it holds, nested at-rules included. A block whose selector lists
   several classes is not matched, so it counts as none, which reads as "no more
   than the definition wrote" and keeps the case. *)
let snapshot_declarations expected name =
  let opener = Re.Pcre.regexp (Re.Pcre.quote ("." ^ name) ^ {|\s*\{|}) in
  let len = String.length expected in
  let count = ref 0 in
  List.iter
    (fun g ->
      let i = ref (snd (Re.Group.offset g 0)) in
      let depth = ref 0 in
      let closed = ref false in
      while (not !closed) && !i < len do
        (match expected.[!i] with
        | '{' -> incr depth
        | '}' -> if !depth = 0 then closed := true else decr depth
        | ';' -> incr count
        | _ -> ());
        incr i
      done)
    (Re.all opener expected);
  !count

let semicolons s =
  String.fold_left (fun n c -> if c = ';' then n + 1 else n) 0 s

(* Tailwind's own breakpoint variants. A candidate carrying one needs a
   [--breakpoint-<name>] the case's template has to declare. *)
let breakpoint_names = [ "sm"; "md"; "lg"; "xl"; "2xl" ]

let variant_prefixes cls =
  match String.rindex_opt cls ':' with
  | None -> []
  | Some i -> String.split_on_char ':' (String.sub cls 0 i)

(* Why the runner cannot reproduce a block that declares its own [@utility], or
   [None] when it can. Each reason is a gap to close, not a property of the
   corpus: the check goes when the gap does.

   An [@utility] the scanner did not read would leave the case compiled without
   its definitions, so a scanner gap shows up as a missing case.

   A body cascade cannot read loses the same declaration on both sides of the
   comparison, which then compares less than it appears to.

   A snapshot that gives a declared class more declarations than its definition
   wrote is one Tailwind merged with a built-in utility of the same name; tw's
   routing replaces the built-in instead of appending to it. An [@apply] body
   writes its declarations by expansion, so it is counted out of this one.

   [@theme reference] declares a token elsewhere: Tailwind inlines its value as
   a [var()] fallback and emits no [:root] declaration. A declared utility
   spells that fallback itself, so a case whose classes the declarations all
   govern replays; every other class still goes through the built-in generator,
   which has no reference-token mode.

   A [var(--name)] a definition reads and the snapshot retains is rendered
   against tw's own theme when the fixture carries no value for it, which is
   either a token Tailwind never emitted or a value it emitted from the case's
   theme. A dead declaration, or an arbitrary runtime custom property, is not a
   missing theme dependency.

   [run()] compiles against an empty theme, so a named breakpoint variant the
   template never declares resolves to nothing upstream; tw's [Scheme.t] has no
   way to say "no breakpoints" (an empty list means the built-in ones). *)
let unreplayable ~defs ~config ~theme_vars ~classes expected =
  let routed =
    List.filter
      (Tw_tools.Entrypoint.is_custom_routed ~defs:[] ~udefs:defs)
      classes
  in
  let unreadable =
    List.filter_map
      (fun def ->
        if is_functional def then None else unreadable_declaration (snd def))
      defs
  in
  let () = Lazy.force init_theme_tokens in
  let unresolved_reference name =
    (not (List.mem_assoc name theme_vars))
    && Option.is_some (Tw.Scheme.token_default name)
  in
  let reads_unknown_token =
    List.exists
      (fun (_, body) ->
        List.exists
          (fun name ->
            unresolved_reference name
            && Astring.String.is_infix ~affix:("var(--" ^ name) expected)
          (var_references body))
      defs
  in
  let applies = List.exists (fun (_, b) -> contains_apply b) defs in
  let undeclared_breakpoint =
    List.exists
      (fun cls ->
        List.exists
          (fun v ->
            List.mem v breakpoint_names
            && not (List.mem_assoc ("breakpoint-" ^ v) theme_vars))
          (variant_prefixes cls))
      classes
  in
  let merged_with_builtin =
    (not applies)
    && List.exists
         (fun (name, _) ->
           let wrote =
             List.fold_left
               (fun n (m, body) -> if m = name then n + semicolons body else n)
               0 defs
           in
           snapshot_declarations expected name > wrote)
         defs
  in
  let some = Fmt.kstr (fun s -> Some s) in
  (* [some] is fixed at the first format it is used with, so a reason carrying a
     count needs its own. *)
  let counted = Fmt.kstr (fun s -> Some s) in
  if defs = [] then Some "an @utility declaration the extractor could not read"
  else if unreadable <> [] then
    some "cascade cannot read the declared %s" (String.concat ", " unreadable)
  else if merged_with_builtin then
    Some "Tailwind merged the declared utility with a built-in of the same name"
  else if
    (config = Theme_reference || config = Theme_inline_reference)
    && expected <> ""
    && List.length routed < List.length classes
  then
    counted
      "@theme reference and %d class(es) the declarations do not govern, which \
       tw has no reference-token mode for"
      (List.length classes - List.length routed)
  else if reads_unknown_token && routed <> [] then
    Some
      "a declared utility reads a theme token the fixture does not carry, so \
       tw cannot render it against the case's own theme"
  else if undeclared_breakpoint then
    Some "a breakpoint variant the case's own theme does not declare"
  else None

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
  (* Cases produced by the block being read, held back until it ends: a test's
     [@utility] declarations are stamped on all of them at once, because a test
     can declare them in a template it reuses across several snapshots (the
     definition sits above the first [expect], the candidate list below the
     second).

     A block that declares its own [@utility] is carried only when the runner's
     declared-utility path reproduces it; {!unreplayable} names the shapes it
     does not, and every drop is reported on stderr. *)
  let block_tests = ref [] in
  let saw_custom_utility = ref false in
  let utility_def_re = Re.Pcre.regexp {|@utility\s|} in
  (* Capture [@utility <name> { <body> }] declarations. [in_utility] holds the
     name and the scanner of the block still being read, if any. The name runs
     to the [{] and is kept as spelled: an invalid one ([~push], [@push]) is a
     test's own subject, not something to normalise away. *)
  let current_utility_defs = ref [] in
  let in_utility = ref None in
  let utility_open_re = Re.Pcre.regexp {|@utility\s+([^{\n]+?)\s*\{|} in
  (* [@layer <name> { @tailwind utilities; }] in a template puts the generated
     utilities in [<name>]. The block is read whole, then kept only if that is
     what it holds: an [@layer] wrapping the author's own rules names nothing
     for the generator. *)
  let current_layer_wrap = ref None in
  let current_layer_before_theme = ref false in
  let in_layer = ref None in
  let layer_open_re = Re.Pcre.regexp {|@layer\s+([A-Za-z0-9_-]+)\s*\{|} in
  let tailwind_utilities_re = Re.Pcre.regexp {|@tailwind\s+utilities|} in
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
  let current_theme_modes = ref [] in
  let in_theme = ref None in
  let theme_open_re = Re.Pcre.regexp {|@theme\b([^{]*)\{|} in
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
    let emitted = classes <> [] in
    if emitted then
      block_tests :=
        {
          name;
          config = !current_config;
          classes;
          expected;
          variants;
          theme_vars = List.rev !current_theme_vars;
          theme_modes = List.rev !current_theme_modes;
          utility_defs = [];
          layer_wrap = !current_layer_wrap;
          layer_before_theme = !current_layer_before_theme;
        }
        :: !block_tests;
    current_classes := [];
    current_variant_names := [];
    (* A call with no candidates produces no fixture case, so it has not
       consumed metadata read from a let-bound CSS template. Tailwind tests
       commonly compile that template once to show it emits nothing, then pass
       the same binding to [run] with candidates. Keep its theme input for that
       later call; a real emitted case consumes it as before. *)
    if emitted then (
      current_theme_vars := [];
      current_theme_modes := []);
    in_theme := None;
    in_expect := None
  in

  let close_block () =
    let defs = List.rev !current_utility_defs in
    let drop =
      if not !saw_custom_utility then None
      else
        List.find_map
          (fun t ->
            unreplayable ~defs ~config:t.config ~theme_vars:t.theme_vars
              ~classes:t.classes
              (Option.value ~default:"" t.expected))
          (List.rev !block_tests)
    in
    (match drop with
    | Some why ->
        List.iter
          (fun t -> Fmt.epr "dropped '%s': %s@." t.name why)
          (List.rev !block_tests)
    | None ->
        tests :=
          List.map (fun t -> { t with utility_defs = defs }) !block_tests
          @ !tests);
    block_tests := [];
    (* Metadata an empty call left available for another use of the same CSS
       binding belongs only to this [test(...)] block. Do not let the final
       empty assertion in one test seed the first case in the next. *)
    current_theme_vars := [];
    current_theme_modes := [];
    current_utility_defs := [];
    current_layer_wrap := None;
    current_layer_before_theme := false;
    in_theme := None;
    in_utility := None;
    in_layer := None;
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

          (* The generated utilities and the theme declarations remain beside
             one another in source order. [current_layer_wrap] is set only after
             its block closes, so seeing a later [@theme] records the one
             ordering the replay cannot recover from the two statements
             alone. *)
          if
            Option.is_some !current_layer_wrap
            && (Re.execp theme_inline_ref_re line
               || Re.execp theme_inline_re line
               || Re.execp theme_ref_re line || Re.execp theme_re line)
          then current_layer_before_theme := true;

          (* Capture [@theme] token declarations. Enter just after the opener's
             [{] and read on until the block's closing [}]. *)
          let scan_from =
            match !in_theme with
            | Some (modes, s) -> Some (modes, s, 0)
            | None -> (
                match Re.exec_opt theme_open_re line with
                | Some g ->
                    let modes =
                      String.split_on_char ' ' (String.trim (Re.Group.get g 1))
                      |> List.filter (fun m -> m <> "")
                    in
                    let s = theme_scanner () in
                    in_theme := Some (modes, s);
                    Some (modes, s, snd (Re.Group.offset g 0))
                | None -> None)
          in
          (match scan_from with
          | None -> ()
          | Some (modes, s, start) ->
              let emit ((name, _) as v) =
                current_theme_vars := v :: !current_theme_vars;
                if modes <> [] then
                  current_theme_modes := (name, modes) :: !current_theme_modes
              in
              if not (scanner_feed s line ~start emit) then in_theme := None);

          (* Capture [@utility <name> { ... }] declarations the same way. *)
          let utility_scan =
            match !in_utility with
            | Some (name, s) -> Some (name, s, 0)
            | None -> (
                match Re.exec_opt utility_open_re line with
                | Some g ->
                    let name = String.trim (Re.Group.get g 1) in
                    let s = block_scanner () in
                    in_utility := Some (name, s);
                    Some (name, s, snd (Re.Group.offset g 0))
                | None -> None)
          in
          (match utility_scan with
          | None -> ()
          | Some (name, s, start) ->
              if not (block_feed s line ~start) then (
                in_utility := None;
                current_utility_defs :=
                  (name, Buffer.contents s.text) :: !current_utility_defs));

          (* Note the layer a template compiles [@tailwind utilities] into. *)
          let layer_scan =
            match !in_layer with
            | Some (name, s) -> Some (name, s, 0)
            | None -> (
                match Re.exec_opt layer_open_re line with
                | Some g ->
                    let name = Re.Group.get g 1 in
                    let s = block_scanner () in
                    in_layer := Some (name, s);
                    Some (name, s, snd (Re.Group.offset g 0))
                | None -> None)
          in
          (match layer_scan with
          | None -> ()
          | Some (name, s, start) ->
              if not (block_feed s line ~start) then (
                in_layer := None;
                if Re.execp tailwind_utilities_re (Buffer.contents s.text) then
                  current_layer_wrap := Some name));

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

  (* The provenance banner: [#!] keeps it out of the block scan, which reads a
     [#] and a space as a block header, and the count is what
     [test/upstream/test.ml] holds the file to. *)
  let count = List.length tests in
  Fmt.pr "#! %d %s extracted from %s by extract_tests.exe -- do not edit@."
    count
    (if count = 1 then "block" else "blocks")
    (Filename.basename filename);

  (* Output format: # test-name @config <theme-config> class1 class2 --- css
     output here with newlines preserved <<<>>> *)
  List.iter
    (fun test ->
      Fmt.pr "# %s@." test.name;
      Fmt.pr "@config %s@." (config_to_string test.config);
      List.iter (fun v -> Fmt.pr "@variant %s@." v) test.variants;
      List.iter (fun (n, v) -> Fmt.pr "@theme-var %s %s@." n v) test.theme_vars;
      List.iter
        (fun (n, modes) ->
          Fmt.pr "@theme-mode %s %s@." n (String.concat " " modes))
        test.theme_modes;
      List.iter
        (fun (n, body) -> Fmt.pr "@utility-def %s %s@." n body)
        test.utility_defs;
      Option.iter (Fmt.pr "@layer-wrap %s@.") test.layer_wrap;
      if test.layer_before_theme then Fmt.pr "@layer-before-theme true@.";
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
