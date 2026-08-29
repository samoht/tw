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

let config_of_string = function
  | "theme" -> Theme
  | "theme-inline" -> Theme_inline
  | "theme-reference" -> Theme_reference
  | "theme-inline-reference" -> Theme_inline_reference
  | "none" -> No_theme
  | "run" -> Run
  | _ -> No_theme

type case = {
  source : string;  (** Fixture the case was read from. *)
  name : string;
  config : config;
  classes : string list;
  expected : string;
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

let read filename =
  if not (Sys.file_exists filename) then []
  else
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let tests = ref [] in
    let current_variants = ref [] in
    let current_theme_vars = ref [] in
    let current_theme_modes = ref [] in
    let current_utility_defs = ref [] in
    let current_layer_wrap = ref None in
    let lines = String.split_on_char '\n' content in
    let parse_config_line line =
      let line = String.trim line in
      if String.length line > 8 && String.sub line 0 8 = "@config " then
        Some (config_of_string (String.sub line 8 (String.length line - 8)))
      else None
    in
    (* A directive line is "<keyword> <name> <rest>"; the name runs to the first
       space and the rest is kept as written. *)
    let named_pair keyword line =
      let n = String.length keyword in
      if String.length line < n || String.sub line 0 n <> keyword then None
      else
        let tail = String.sub line n (String.length line - n) in
        Option.map
          (fun i ->
            ( String.sub tail 0 i,
              String.sub tail (i + 1) (String.length tail - i - 1) ))
          (String.index_opt tail ' ')
    in
    let rec parse lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if String.length line > 2 && line.[0] = '#' && line.[1] = ' ' then (
            let name = String.sub line 2 (String.length line - 2) in
            current_variants := [];
            current_theme_vars := [];
            current_theme_modes := [];
            current_utility_defs := [];
            current_layer_wrap := None;
            parse_config name No_theme rest)
          else parse rest
    and parse_config name default_config lines =
      match lines with
      | [] -> ()
      | line :: rest -> (
          match parse_config_line line with
          | Some config -> parse_variants name config rest
          | None -> parse_variants name default_config (line :: rest))
    and parse_variants name config lines =
      match lines with
      | [] -> ()
      | line :: rest -> (
          let tl = String.trim line in
          if String.length tl >= 9 && String.sub tl 0 9 = "@variant " then (
            current_variants :=
              String.sub tl 9 (String.length tl - 9) :: !current_variants;
            parse_variants name config rest)
          else
            match named_pair "@theme-var " tl with
            | Some pair ->
                current_theme_vars := pair :: !current_theme_vars;
                parse_variants name config rest
            | None -> (
                match named_pair "@theme-mode " tl with
                | Some (n, modes) ->
                    current_theme_modes :=
                      (n, String.split_on_char ' ' modes)
                      :: !current_theme_modes;
                    parse_variants name config rest
                | None -> (
                    match named_pair "@utility-def " tl with
                    | Some pair ->
                        current_utility_defs := pair :: !current_utility_defs;
                        parse_variants name config rest
                    | None ->
                        if
                          String.length tl >= 12
                          && String.sub tl 0 12 = "@layer-wrap "
                        then (
                          current_layer_wrap :=
                            Some (String.sub tl 12 (String.length tl - 12));
                          parse_variants name config rest)
                        else parse_classes name config (line :: rest))))
    and parse_classes name config lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "<<<>>>" then parse rest
          else if line = "---" then
            (* No classes line before ---, skip *)
            parse_expected name config [] (Buffer.create 256) rest
          else if String.length line > 2 && line.[0] = '#' && line.[1] = ' '
          then (
            (* New test without classes *)
            let new_name = String.sub line 2 (String.length line - 2) in
            current_variants := [];
            current_theme_vars := [];
            current_theme_modes := [];
            current_utility_defs := [];
            current_layer_wrap := None;
            parse_config new_name No_theme rest)
          else
            let classes = split_classes line in
            parse_after_classes name config classes rest
    and parse_after_classes name config classes lines =
      match lines with
      | [] -> ()
      | line :: rest ->
          let line = String.trim line in
          if line = "---" then
            parse_expected name config classes (Buffer.create 256) rest
          else if line = "<<<>>>" then
            (* No expected CSS, skip this test *)
            parse rest
          else parse rest
    and parse_expected name config classes buf lines =
      match lines with
      | [] ->
          let expected = Buffer.contents buf |> String.trim in
          if classes <> [] then
            tests :=
              {
                source = filename;
                name;
                config;
                classes;
                expected;
                variants = List.rev !current_variants;
                theme_vars = List.rev !current_theme_vars;
                theme_modes = List.rev !current_theme_modes;
                utility_defs = List.rev !current_utility_defs;
                layer_wrap = !current_layer_wrap;
              }
              :: !tests
      | line :: rest ->
          if String.trim line = "<<<>>>" then (
            let expected = Buffer.contents buf |> String.trim in
            if classes <> [] then
              tests :=
                {
                  source = filename;
                  name;
                  config;
                  classes;
                  expected;
                  variants = List.rev !current_variants;
                  theme_vars = List.rev !current_theme_vars;
                  theme_modes = List.rev !current_theme_modes;
                  utility_defs = List.rev !current_utility_defs;
                  layer_wrap = !current_layer_wrap;
                }
                :: !tests;
            parse rest)
          else (
            if Buffer.length buf > 0 then Buffer.add_char buf '\n';
            Buffer.add_string buf line;
            parse_expected name config classes buf rest)
    in
    parse lines;
    List.rev !tests

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
