module Css = Cascade.Css

(* The patterns are literal, so they are compiled once here rather than per
   call. [Re.str] quotes its argument, which matters for the wildcard ones: [-*]
   is text the utility grammar spells, not a repetition operator. *)
let wildcard_re = Re.compile (Re.str "-*")
let double_wildcard_re = Re.compile (Re.str "-*-*")
let bracket_opener_re = Re.compile (Re.str "-[")
let paren_opener_re = Re.compile (Re.str "-(")

let value_or_modifier_re =
  Re.compile (Re.alt [ Re.str "--value("; Re.str "--modifier(" ])

let re_index re s =
  match Re.exec_opt re s with
  | Some group -> Some (fst (Re.Group.offset group 0))
  | None -> None

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

module Components = struct
  open Cascade

  let array css =
    Parser.list_of_component_values (Reader.of_string css)
    |> (fun out -> out.Parser.value)
    |> Array.of_list

  let start component = (Component.source_loc component).Loc.start_pos
  let stop component = (Component.source_loc component).Loc.end_pos

  let rec after_whitespace components i =
    if i >= Array.length components then i
    else
      match components.(i) with
      | Component.Preserved { kind = Token.Whitespace _; _ } ->
          after_whitespace components (i + 1)
      | _ -> i

  (* Component values already group every function and bracket block, so only a
     top-level semicolon can close this declaration. Mismatched closers stay
     inside the group whose matching closer is still outstanding. *)
  let rec semicolon_from components i =
    if i >= Array.length components then i
    else
      match components.(i) with
      | Component.Preserved { kind = Token.Semicolon; _ } -> i
      | _ -> semicolon_from components (i + 1)

  (* Source ranges of punctuation that separates declarations. Curly blocks are
     walked because a nested rule may itself contain functional utility
     declarations; parentheses, square blocks and functions remain atomic. *)
  let punctuation components =
    let rec add acc = function
      | [] -> acc
      | Component.Preserved
          { kind = Token.Semicolon | Token.Close Token.Curly; loc; _ }
        :: rest ->
          add ((loc.Loc.start_pos, loc.Loc.end_pos) :: acc) rest
      | Component.Block { node = { opening = Token.Curly; value; closed }; loc }
        :: rest ->
          let acc = (loc.Loc.start_pos, loc.Loc.start_pos + 1) :: acc in
          let acc = add acc value in
          let acc =
            if closed then (loc.Loc.end_pos - 1, loc.Loc.end_pos) :: acc
            else acc
          in
          add acc rest
      | _ :: rest -> add acc rest
    in
    List.rev (add [] components)
end

(* [--<ns>-*: initial] takes a whole [@theme] namespace out of the theme, and
   [--<ns>-*] is not a custom-property name: the [*] ends the ident, so "consume
   a declaration" (CSS Syntax 3 sec. 5.5.15) finds no [:] where it wants one and
   the block loses the reset. The token stream still has it, so it is read from
   there, keyed by where it starts so it can go back among the declarations in
   the order the block wrote them. *)
let namespace_resets body =
  let components = Components.array body in
  let text from upto = String.trim (String.sub body from (upto - from)) in
  (* The reset an ident at [i] opens, and the index to carry on from. *)
  let reset i name acc =
    let star = Components.after_whitespace components (i + 1) in
    let colon = Components.after_whitespace components (star + 1) in
    if
      colon < Array.length components
      && (match components.(star) with
        | Cascade.Component.Preserved { kind = Cascade.Token.Delim "*"; _ } ->
            true
        | _ -> false)
      &&
      match components.(colon) with
      | Cascade.Component.Preserved { kind = Cascade.Token.Colon; _ } -> true
      | _ -> false
    then
      let last = Components.semicolon_from components (colon + 1) in
      let upto =
        if last < Array.length components then
          Components.start components.(last)
        else String.length body
      in
      let namespace = String.sub name 2 (String.length name - 2) ^ "*" in
      ( last + 1,
        ( Components.start components.(i),
          (namespace, text (Components.stop components.(colon)) upto) )
        :: acc )
    else (i + 1, acc)
  in
  (* Only the component values of the block itself are visited. Nested rules and
     bracket groups are each one component and therefore stepped past. *)
  let rec go i acc =
    if i >= Array.length components then List.rev acc
    else
      match components.(i) with
      | Cascade.Component.Preserved { kind = Cascade.Token.Ident name; _ }
        when String.length name >= 2 && String.equal (String.sub name 0 2) "--"
        ->
          let i, acc = reset i name acc in
          go i acc
      | _ -> go (i + 1) acc
  in
  go 0 []

(* Tailwind's [@theme] is not a CSS at-rule, so cascade keeps it whole rather
   than interpreting it (CSS Syntax 3 sec. 5.5.2, "consume an at-rule"): the
   prelude carries the modifiers, the block its declarations, both as the source
   text they were written as. *)
let theme_block = function
  | Cascade.Stylesheet.Unknown_at_rule
      { name = "theme"; prelude; block = Some body } ->
      Some (prelude, body)
  | _ -> None

(* The [(bare-name, value)] pairs a [@theme] block declares. Its body is a
   declaration list, read here the way a rule's body is read, so reading carries
   on past a nested at-rule such as the [@keyframes] a project writes beside its
   [--animate-*] token. The namespace resets come off the token stream and are
   merged back in by where each one starts. *)
let theme_tokens body =
  let declared =
    Cascade.Reader.of_string body
    |> Cascade.Parser.block_contents
    |> (fun out -> out.Cascade.Parser.value)
    |> List.concat_map (function `Decls decls -> decls | `Rule _ -> [])
    |> List.filter_map (fun (decl : Cascade.Component.declaration) ->
        let { Cascade.Component.name; value; _ } =
          decl.Cascade.Component.node
        in
        if String.length name > 2 && String.sub name 0 2 = "--" then
          Some
            ( decl.Cascade.Component.loc.Cascade.Loc.start_pos,
              ( String.sub name 2 (String.length name - 2),
                Cascade.Parser.string_of_components value ) )
        else None)
  in
  List.stable_sort
    (fun (a, _) (b, _) -> Int.compare a b)
    (declared @ namespace_resets body)
  |> List.map snd

(* Extract @theme token overrides from a project CSS entrypoint, so tw renders
   with the same tokens Tailwind reads from it: the pairs every block declares,
   and the names among them that came from an [@theme inline] block. The
   resulting strings feed Scheme.with_overrides. *)
let theme_blocks css =
  match Css.of_string css with
  | Error _ -> []
  | Ok parse ->
      Css.statements parse.Css.stylesheet
      |> List.filter_map theme_block
      |> List.map (fun (prelude, body) ->
          (String.split_on_char ' ' (String.trim prelude), theme_tokens body))

(* The names the blocks carrying the modifier [option] declare. *)
let theme_tokens_with option blocks =
  List.concat_map
    (fun (options, tokens) ->
      if List.mem option options then List.map fst tokens else [])
    blocks

let theme_overrides_of_css css =
  let blocks = theme_blocks css in
  (List.concat_map snd blocks, theme_tokens_with "inline" blocks)

(* [@import "tailwindcss"] and its subpath forms are the package, not a file on
   disk: each marks where part of the generated sheet belongs. The package's own
   [index.css] imports [theme.css], [preflight.css] and [utilities.css] in turn,
   so the bare import is all three, each file is its own part, and any other
   subpath is read as the package. *)
type tailwind_part = Whole | Theme_part | Preflight_part | Utilities_part

let tailwind_import_part url =
  let u = Css.decode_import_url url in
  match Filename.remove_extension u with
  | "tailwindcss" -> Some Whole
  | "tailwindcss/theme" -> Some Theme_part
  | "tailwindcss/preflight" -> Some Preflight_part
  | "tailwindcss/utilities" -> Some Utilities_part
  | _ when String.starts_with ~prefix:"tailwindcss/" u -> Some Whole
  | _ -> None

let is_tailwind_import url = Option.is_some (tailwind_import_part url)

(* Tailwind's [@custom-variant NAME { ... @slot; ... }] and selector shorthand
   [@custom-variant NAME (...);] declare variants, and [@variant NAME { decls }]
   applies one inside author CSS. These are Tailwind syntax, so a CSS parser
   drops them and the declarations they guard vanish. Expanding here keeps the
   [&] nesting for cascade to flatten.

   Only the built-in [dark] is known without a declaration; other names need one
   in the entrypoint. *)

let builtin_variants =
  [ ("dark", "@media (prefers-color-scheme: dark) { @slot; }") ]

(* Where the entrypoint's blocks, at-rule headers and function calls start and
   end, keyed by the offset each one starts at.

   The Tailwind at-rules below are not CSS, so a stylesheet parser drops them
   and they have to be located in the text first -- but counting braces over the
   raw bytes counts the ones inside a string, a comment or an escape too. The
   block then ends in the wrong place and everything after it is silently
   dropped. cascade's component values come out of CSS Syntax 3 sec. 5.4 already
   matched, and each carries the range of source it was parsed from, so the
   offsets are read off those. *)
(* Offsets into the entrypoint source, so a rewrite hands back the author's own
   bytes. cascade owns the index; the callers below are the Tailwind-specific
   part. *)
module Index = Cascade.Source_index

(* Whether [css] spells [name] anywhere. *)
let mentions name css =
  let n = String.length name and len = String.length css in
  let rec at i j = j = n || (css.[i + j] = name.[j] && at i (j + 1)) in
  let rec from i = i + n <= len && (at i 0 || from (i + 1)) in
  from 0

(* [css] copied with [step] rewriting it. [step index buf i] runs at each offset
   nothing has been copied from yet: it writes to [buf] what stands for the
   source at [i] and answers where to resume, or [None] to have the byte at [i]
   copied. [names] is what the pass matches; the index finds a name only as the
   source spells it, so a source spelling none of them is handed back before the
   parse the index costs. *)
let rewrite ~names css step =
  if not (List.exists (fun name -> mentions name css) names) then css
  else
    let index = Index.v css in
    let len = String.length css in
    let buf = Buffer.create len in
    let step = step index buf in
    let rec go i =
      if i >= len then ()
      else
        match step i with
        | Some next -> go next
        | None ->
            Buffer.add_char buf css.[i];
            go (i + 1)
    in
    go 0;
    Buffer.contents buf

(* [@import "tailwindcss" theme(static)] asks for the whole theme, not only the
   variables a utility used. The option is not CSS, so it is read off the
   import's own [theme()] call rather than from a parsed stylesheet. *)
let imports_static_theme css =
  let index = Index.v css in
  let static =
    Index.calls index ~name:"theme"
    |> List.filter (fun (_, (block : Index.block)) ->
        String.trim block.body = "static")
  in
  Index.at_statements index ~name:"@import"
  |> List.exists (fun (at, (import : Index.statement)) ->
      List.exists (fun (i, _) -> at < i && i < import.next) static)

(* [@import "tailwindcss" prefix(tw)] asks for [tw:] in front of every candidate
   and [--tw-] in front of every theme token. Read the same way [theme(static)]
   is: the option function sits inside the import statement. *)
let import_prefix css =
  let index = Index.v css in
  let prefixes = Index.calls index ~name:"prefix" in
  Index.at_statements index ~name:"@import"
  |> List.filter_map (fun (at, (import : Index.statement)) ->
      List.find_map
        (fun (i, (block : Index.block)) ->
          if at < i && i < import.next then
            match String.trim block.body with "" -> None | name -> Some name
          else None)
        prefixes)
  |> function
  | name :: _ -> Some name
  | [] -> None

(* Split [s] on [sep] where no bracket is open, so a [,] or a space inside
   [[...]], [(...)] or a nested [{...}] stays in its segment. *)
let split_top_level sep s =
  let len = String.length s in
  let rec go depth start i acc =
    if i >= len then List.rev (String.sub s start (len - start) :: acc)
    else
      match s.[i] with
      | '(' | '[' | '{' -> go (depth + 1) start (i + 1) acc
      | ')' | ']' | '}' -> go (max 0 (depth - 1)) start (i + 1) acc
      | c when c = sep && depth = 0 ->
          go depth (i + 1) (i + 1) (String.sub s start (i - start) :: acc)
      | _ -> go depth start (i + 1) acc
  in
  go 0 0 0 []

let brace_bound = Re.seq [ Re.opt (Re.char '-'); Re.rep1 Re.digit ]

let brace_range_re =
  Re.compile
    (Re.whole_string
       (Re.seq
          [
            Re.group brace_bound;
            Re.str "..";
            Re.group brace_bound;
            Re.opt (Re.seq [ Re.str ".."; Re.group brace_bound ]);
          ]))

(* [{from..to..step}] counts from [from] to [to] inclusive. The step defaults to
   one, and a step pointing away from [to] is turned round rather than never
   arriving, as the bundle does. A zero step is an error there, so the pattern
   names nothing here. *)
let brace_range inner =
  let bound g k = Option.bind (Re.Group.get_opt g k) int_of_string_opt in
  match Re.exec_opt brace_range_re inner with
  | None -> None
  | Some g -> (
      match (bound g 1, bound g 2) with
      | Some first, Some last ->
          let step = Option.fold ~none:1 ~some:abs (bound g 3) in
          let step = if first <= last then step else -step in
          let past n = if step > 0 then n > last else n < last in
          let rec count n acc =
            if past n then List.rev acc
            else count (n + step) (string_of_int n :: acc)
          in
          Some (if step = 0 then [] else count first [])
      | _ -> None)

(* Tailwind's brace expansion: the first [{...}] is a comma list or a range,
   each alternative is expanded in turn, and so is what follows the brace. An
   unbalanced brace is refused by the bundle, so it names nothing. *)
let rec expand_braces pattern =
  match String.index_opt pattern '{' with
  | None -> [ pattern ]
  | Some opening -> (
      let len = String.length pattern in
      let rec closing depth i =
        if i >= len then None
        else
          match pattern.[i] with
          | '{' -> closing (depth + 1) (i + 1)
          | '}' when depth = 1 -> Some i
          | '}' -> closing (depth - 1) (i + 1)
          | _ -> closing depth (i + 1)
      in
      match closing 0 opening with
      | None -> []
      | Some close ->
          let prefix = String.sub pattern 0 opening in
          let inner = String.sub pattern (opening + 1) (close - opening - 1) in
          let suffix = String.sub pattern (close + 1) (len - close - 1) in
          let alternatives =
            match brace_range inner with
            | Some range -> range
            | None -> List.concat_map expand_braces (split_top_level ',' inner)
          in
          List.concat_map
            (fun rest ->
              List.map
                (fun alt -> String.concat "" [ prefix; alt; rest ])
                alternatives)
            (expand_braces suffix))

(* The text of a quoted argument. The bundle refuses an unquoted one. *)
let quoted_contents body =
  let body = String.trim body in
  let n = String.length body in
  if n >= 2 && (body.[0] = '"' || body.[0] = '\'') && body.[n - 1] = body.[0]
  then Some (String.sub body 1 (n - 2))
  else None

(* [@source inline("...")] is the safelist and [@source not inline("...")] the
   blocklist. The argument is read off the [inline()] call inside the statement,
   and what stands between the at-keyword and that call says which of the two it
   is; anything else there is not the option. *)
let source_inline css =
  let index = Index.v css in
  let calls = Index.calls index ~name:"inline" in
  let keyword = String.length "@source" in
  let argument at (source : Index.statement) =
    List.find_map
      (fun (i, (call : Index.block)) ->
        if at + keyword <= i && call.next <= source.next then
          match
            String.trim (String.sub css (at + keyword) (i - at - keyword))
          with
          | "" -> Some (false, call.body)
          | "not" -> Some (true, call.body)
          | _ -> None
        else None)
      calls
  in
  Index.at_statements index ~name:"@source"
  |> List.fold_left
       (fun (safelist, blocklist) (at, source) ->
         match argument at source with
         | None -> (safelist, blocklist)
         | Some (negated, body) -> (
             match quoted_contents body with
             | None -> (safelist, blocklist)
             | Some patterns ->
                 let candidates =
                   split_top_level ' ' patterns
                   |> List.filter (fun p -> p <> "")
                   |> List.concat_map expand_braces
                 in
                 if negated then (safelist, blocklist @ candidates)
                 else (safelist @ candidates, blocklist)))
       ([], [])

(* [@source "<path>"] names files to scan for candidates, relative to the
   stylesheet, and [@source not "<path>"] takes files back out. The [inline()]
   forms are the safelist above: their argument is a call, not a quoted path. *)
let source_paths css =
  let index = Index.v css in
  let blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  Index.at_statements index ~name:"@source"
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.fold_left
       (fun (included, excluded) (_, (source : Index.statement)) ->
         let prelude = String.trim source.prelude in
         let n = String.length prelude in
         let negated, argument =
           if n > 3 && String.sub prelude 0 3 = "not" && blank prelude.[3] then
             (true, String.sub prelude 3 (n - 3))
           else (false, prelude)
         in
         match quoted_contents argument with
         | None -> (included, excluded)
         | Some path when negated -> (included, excluded @ [ path ])
         | Some path -> (included @ [ path ], excluded))
       ([], [])

(* The directives whose argument is a path the CLI resolves against the
   stylesheet. [@source not "../x"] names one too; [@source inline("...")] names
   candidates, not a path. *)
let path_directives =
  [ "@import"; "@source"; "@plugin"; "@config"; "@reference" ]

let is_relative_path path =
  String.starts_with ~prefix:"./" path || String.starts_with ~prefix:"../" path

(* Where the quoted path of the directive at [at] opens and closes, when what
   stands before the quote is blank or, for [@source], the [not] keyword. *)
let quoted_path_span css ~name ~at ~next =
  let len = String.length css in
  let blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec skip_blank i =
    if i < next && blank css.[i] then skip_blank (i + 1) else i
  in
  let i = skip_blank (at + String.length name) in
  let i =
    if
      String.equal name "@source"
      && i + 3 < len
      && String.sub css i 3 = "not"
      && blank css.[i + 3]
    then skip_blank (i + 3)
    else i
  in
  if i < next && (css.[i] = '"' || css.[i] = '\'') then
    match String.index_from_opt css (i + 1) css.[i] with
    | Some close when close < next -> Some (i + 1, close)
    | _ -> None
  else None

let rooted ~dir css =
  let index = Index.v css in
  let spans =
    List.concat_map
      (fun name ->
        Index.at_statements index ~name
        |> List.filter_map (fun (at, (statement : Index.statement)) ->
            match quoted_path_span css ~name ~at ~next:statement.next with
            | Some (start, stop) ->
                let path = String.sub css start (stop - start) in
                if is_relative_path path then Some (start, stop, path) else None
            | None -> None))
      path_directives
    |> List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b)
  in
  let buf = Buffer.create (String.length css) in
  let last =
    List.fold_left
      (fun pos (start, stop, path) ->
        Buffer.add_string buf (String.sub css pos (start - pos));
        Buffer.add_string buf (Filename.concat dir path);
        stop)
      0 spans
  in
  Buffer.add_string buf (String.sub css last (String.length css - last));
  Buffer.contents buf

(* [@import "tailwindcss" source(none)] turns automatic source detection off,
   and [source("../src")] moves where it starts. The option sits inside the
   import statement, or inside [@tailwind utilities] for a sheet importing the
   parts. *)
let source_root css =
  let index = Index.v css in
  let sources = Index.calls index ~name:"source" in
  let option (at, (statement : Index.statement)) =
    List.find_map
      (fun (i, (block : Index.block)) ->
        if at < i && i < statement.next then
          match String.trim block.body with
          | "none" -> Some `None
          | body -> Option.map (fun dir -> `Dir dir) (quoted_contents body)
        else None)
      sources
  in
  Index.at_statements index ~name:"@import"
  @ Index.at_statements index ~name:"@tailwind"
  |> List.find_map option
  |> Option.value ~default:`Detect

(* [@import "tailwindcss" important] marks every utility declaration
   [!important]. The option is a bare word in the import's prelude rather than a
   call, so it is read off the prelude's top-level words. *)
let imports_important css =
  let index = Index.v css in
  let blank = function '\n' | '\t' | '\r' | '\012' -> ' ' | c -> c in
  Index.at_statements index ~name:"@import"
  |> List.exists (fun (_, (import : Index.statement)) ->
      List.mem "important"
        (split_top_level ' ' (String.map blank import.prelude)))

(* [@reference "tailwindcss"] brings the theme into scope for [@apply] without
   emitting any of it. The prelude is the URL alone. *)
let references_tailwind css =
  let index = Index.v css in
  Index.at_statements index ~name:"@reference"
  |> List.exists (fun (_, (statement : Index.statement)) ->
      is_tailwind_import (String.trim statement.prelude))

(* [@plugin "@tailwindcss/forms"] resets native form controls in the base layer,
   which is the plugin's default strategy. An options block naming [strategy:
   "class"] leaves the controls alone and styles only the [form-*] classes. *)
let forms_base css =
  let index = Index.v css in
  let unquote s =
    let s = String.trim s in
    let n = String.length s in
    if n >= 2 && (s.[0] = '"' || s.[0] = '\'') && s.[n - 1] = s.[0] then
      String.sub s 1 (n - 2)
    else s
  in
  let names_forms prelude =
    String.equal (unquote prelude) "@tailwindcss/forms"
  in
  let class_strategy body =
    split_top_level ';' body
    |> List.exists (fun option ->
        match String.index_opt option ':' with
        | None -> false
        | Some i ->
            let value =
              String.sub option (i + 1) (String.length option - i - 1)
            in
            String.equal (String.trim (String.sub option 0 i)) "strategy"
            && String.equal (unquote value) "class")
  in
  let len = String.length css in
  let rec block i =
    if i >= len then false
    else
      match Index.at_rule index ~name:"@plugin" i with
      | Some { prelude; block = { body; next }; _ } when names_forms prelude ->
          (not (class_strategy body)) || block next
      | _ -> block (i + 1)
  in
  Index.at_statements index ~name:"@plugin"
  |> List.exists (fun (_, (statement : Index.statement)) ->
      names_forms statement.prelude)
  || block 0

(* The JavaScript configs the entrypoint's [@config] directives name, as written
   and in source order. *)
let config_directives css =
  let index = Index.v css in
  Index.at_statements index ~name:"@config"
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map (fun (_, (statement : Index.statement)) ->
      String.trim statement.prelude)

(* A project can declare [@keyframes] inside its [@theme] block, beside the
   [--animate-*] token that names it. [@theme] is a build-time directive, so
   [drop_directives] takes the whole block out of the emitted CSS; lift actual
   keyframe at-rules to the top level first, where Tailwind emits them. *)
let hoist_theme_keyframes css =
  let lifted = Buffer.create 0 in
  let css =
    rewrite ~names:[ "@theme" ] css (fun index buf i ->
        match Index.at_rule index ~name:"@theme" i with
        | Some { brace; block = { next = stop; _ }; _ } ->
            Buffer.add_string buf (String.sub css i (brace + 1 - i));
            let rec go_theme i =
              if i >= stop then ()
              else
                match Index.at_rule index ~name:"@keyframes" i with
                | Some { block = { next; _ }; _ } when next <= stop ->
                    Buffer.add_string lifted (String.sub css i (next - i));
                    go_theme next
                | Some _ | None ->
                    Buffer.add_char buf css.[i];
                    go_theme (i + 1)
            in
            go_theme (brace + 1);
            Some stop
        | None -> None)
  in
  css ^ Buffer.contents lifted

(* Tailwind extends [@import] with options CSS has no grammar for
   ([theme(static)], [source(none)], [prefix(tw)]). Strip actual option function
   tokens from actual import statements so quoted parentheses and comments do
   not alter their boundaries. *)
let strip_tailwind_import_options css =
  rewrite ~names:[ "@import" ] css (fun index buf i ->
      let option_at i =
        List.find_map
          (fun name -> Index.call index ~name i)
          [ "theme"; "source"; "prefix" ]
      in
      let rec copy_import i stop =
        if i >= stop then ()
        else
          match option_at i with
          | Some { next; _ } when next <= stop -> copy_import next stop
          | Some _ | None ->
              Buffer.add_char buf css.[i];
              copy_import (i + 1) stop
      in
      match Index.at_statement index ~name:"@import" i with
      | Some { next; _ } ->
          copy_import i next;
          Some next
      | None -> None)

(* Pull out the [@KEYWORD NAME { ... }] declarations, dropping them from the
   CSS: they declare something for the generator, and Tailwind does not emit
   them either. *)
let take_named_defs keyword css =
  let defs = ref [] in
  let css =
    rewrite ~names:[ keyword ] css (fun index _ i ->
        match Index.at_rule index ~name:keyword i with
        | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
            defs := (prelude, body) :: !defs;
            Some next
        | _ -> None)
  in
  (css, !defs)

let shorthand_variant prelude =
  let prelude = String.trim prelude in
  let len = String.length prelude in
  let whitespace = function
    | ' ' | '\t' | '\n' | '\r' | '\012' -> true
    | _ -> false
  in
  let rec name_end i =
    if i >= len || prelude.[i] = '(' || whitespace prelude.[i] then i
    else name_end (i + 1)
  in
  let i = name_end 0 in
  let name = String.sub prelude 0 i in
  let selector = String.trim (String.sub prelude i (len - i)) in
  if
    name = ""
    || String.length selector < 2
    || selector.[0] <> '('
    || selector.[String.length selector - 1] <> ')'
  then None
  else
    let selector = String.sub selector 1 (String.length selector - 2) in
    Some (name, selector ^ " { @slot; }")

let take_custom_variants css =
  let defs = ref [] in
  let css =
    rewrite ~names:[ "@custom-variant" ] css (fun index _ i ->
        match Index.at_rule index ~name:"@custom-variant" i with
        | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
            defs := (prelude, body) :: !defs;
            Some next
        | _ -> (
            match Index.at_statement index ~name:"@custom-variant" i with
            | Some { prelude; next } -> (
                match shorthand_variant prelude with
                | Some def ->
                    defs := def :: !defs;
                    Some next
                | None -> None)
            | None -> None))
  in
  (css, !defs)

(* {2 Functional [@utility NAME-*] declarations}

   A functional declaration is a template rather than a rule: its body reads the
   candidate's own value back with [--value(...)] and the [/half] after it with
   [--modifier(...)]. A declaration whose reads do not all resolve is dropped,
   and a candidate that resolved no [--value(...)] at all is not a utility. This
   is Tailwind's [createCssUtility], and the arguments a read takes are its
   documented API: a quoted literal, a bare data type, a [[data-type]] for an
   arbitrary value, a [--namespace] to look up in the theme, and
   [--default(...)] for the candidate that spelled no value. *)

(* Tailwind's [segment]: split [s] on [sep] at the top level, so a separator
   inside a bracket group or a quoted string belongs to the piece around it. *)
let segment sep s =
  let len = String.length s in
  let pieces = ref [] in
  let buf = Buffer.create len in
  let depth = ref 0 in
  let quote = ref None in
  let i = ref 0 in
  while !i < len do
    let c = s.[!i] in
    (match !quote with
    | Some q ->
        Buffer.add_char buf c;
        if c = '\\' && !i + 1 < len then (
          Buffer.add_char buf s.[!i + 1];
          incr i)
        else if c = q then quote := None
    | None -> (
        match c with
        | '\'' | '"' ->
            quote := Some c;
            Buffer.add_char buf c
        | '(' | '[' | '{' ->
            incr depth;
            Buffer.add_char buf c
        | ')' | ']' | '}' ->
            if !depth > 0 then decr depth;
            Buffer.add_char buf c
        | c when c = sep && !depth = 0 ->
            pieces := Buffer.contents buf :: !pieces;
            Buffer.clear buf
        | c -> Buffer.add_char buf c));
    incr i
  done;
  List.rev (Buffer.contents buf :: !pieces)

let is_digit c = c >= '0' && c <= '9'

(* A [<number>] as a candidate spells one: an optional sign, digits with at most
   one decimal point, and an optional exponent. *)
let is_number text =
  let n = String.length text in
  let i = ref 0 in
  if !i < n && (text.[!i] = '+' || text.[!i] = '-') then incr i;
  let digits () =
    let from = !i in
    while !i < n && is_digit text.[!i] do
      incr i
    done;
    !i - from
  in
  let whole = digits () in
  let fraction =
    if !i < n && text.[!i] = '.' then (
      incr i;
      digits ())
    else -1
  in
  let mantissa = if fraction < 0 then whole >= 1 else fraction >= 1 in
  let exponent =
    if !i < n && (text.[!i] = 'e' || text.[!i] = 'E') then begin
      incr i;
      if !i < n && (text.[!i] = '+' || text.[!i] = '-') then incr i;
      digits () >= 1
    end
    else true
  in
  mantissa && exponent && !i = n

let is_percentage text =
  let n = String.length text in
  n > 1 && text.[n - 1] = '%' && is_number (String.sub text 0 (n - 1))

let is_fraction text =
  match segment '/' text with
  | [ left; right ] ->
      is_number (String.trim left) && is_number (String.trim right)
  | _ -> false

(* The spelling a round trip through a number leaves unchanged, which is what
   Tailwind's [String(Number(value)) === String(value)] asks for: no sign, no
   redundant leading zero, no trailing zero in the fraction, no bare [.]. *)
let is_canonical_number text =
  let whole, fraction =
    match String.index_opt text '.' with
    | None -> (text, None)
    | Some i ->
        ( String.sub text 0 i,
          Some (String.sub text (i + 1) (String.length text - i - 1)) )
  in
  let digits s = s <> "" && String.for_all is_digit s in
  digits whole
  && (String.length whole = 1 || whole.[0] <> '0')
  &&
  match fraction with
  | None -> true
  | Some f -> digits f && f.[String.length f - 1] <> '0'

let is_positive_integer text =
  is_canonical_number text && not (String.contains text '.')

(* Tailwind's spacing multiplier: a canonical non-negative number that is a
   whole multiple of [0.25]. *)
let is_spacing_multiplier text =
  is_canonical_number text
  &&
  match float_of_string_opt text with
  | None -> false
  | Some value -> Float.rem value 0.25 = 0.

(* MDN's list of CSS length units, the one Tailwind reads a bare length
   against. *)
let length_units =
  [
    "cm";
    "mm";
    "Q";
    "in";
    "pc";
    "pt";
    "px";
    "em";
    "ex";
    "ch";
    "rem";
    "lh";
    "rlh";
    "vw";
    "vh";
    "vmin";
    "vmax";
    "vb";
    "vi";
    "svw";
    "svh";
    "lvw";
    "lvh";
    "dvw";
    "dvh";
    "cqw";
    "cqh";
    "cqi";
    "cqb";
    "cqmin";
    "cqmax";
  ]

let math_functions =
  [
    "calc";
    "min";
    "max";
    "clamp";
    "round";
    "mod";
    "rem";
    "sin";
    "cos";
    "tan";
    "asin";
    "acos";
    "atan";
    "atan2";
    "pow";
    "sqrt";
    "hypot";
    "log";
    "exp";
    "abs";
    "sign";
  ]

(* A math function stands for the value it computes, so it reads as every
   numeric type. *)
let math_fn_re =
  Re.compile
    (Re.alt (List.map (fun name -> Re.str (name ^ "(")) math_functions))

let has_math_fn text = Re.execp math_fn_re text

let is_length text =
  has_math_fn text
  || List.exists
       (fun unit ->
         let n = String.length text and u = String.length unit in
         n > u
         && String.sub text (n - u) u = unit
         && is_number (String.sub text 0 (n - u)))
       length_units

(* Whether [text] reads as the CSS data type [kind]. A [var()] is opaque, so it
   reads as nothing at all. The kinds beyond these are ones no [--value([kind])]
   here asks for; one that does resolves to nothing rather than to a guess. *)
let infer_data_type text kind =
  if String.starts_with ~prefix:"var(" text then false
  else
    match kind with
    | "color" -> Option.is_some (Css.parse_color text)
    | "length" -> is_length text
    | "percentage" -> is_percentage text || has_math_fn text
    | "ratio" -> is_fraction text || has_math_fn text
    | "number" -> is_number text || has_math_fn text
    | "integer" -> is_positive_integer text
    | _ -> false

(* Only these four data types are read from a bare candidate value, so no
   [--value(color)] can turn [example-red] into a utility. *)
let bare_value_data_types = [ "number"; "integer"; "ratio"; "percentage" ]

(** The value a candidate carries: a bare word, or an arbitrary value with the
    data-type hint it was spelled with. The modifier takes the same two shapes.
*)
type candidate_value =
  | Bare of string
  | Bracketed of { hint : string option; text : string }

type functional_candidate = {
  root : string;  (** the [@utility] name without its [-*] *)
  value : candidate_value option;
  fraction : string option;
      (** [2/3] when the value and the modifier read as one fraction, which is
          what a [--value(ratio)] resolves against. *)
  modifier : candidate_value option;
}

(* [example-*] declares a functional utility rooted at [example], and
   [border--*] one rooted at [border-]. *)
let functional_root name =
  let n = String.length name in
  if n > 2 && String.sub name (n - 2) 2 = "-*" then
    Some (String.sub name 0 (n - 2))
  else None

let is_named_value s =
  s <> ""
  && String.for_all
       (fun c ->
         is_digit c
         || (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || c = '_' || c = '.' || c = '%' || c = '-')
       s

(* [color:var(--x)] carries the hint [color]. A [:] after anything but lower
   case letters and dashes is part of the value, not the end of a hint. *)
let split_hint text =
  let n = String.length text in
  let rec go i =
    if i >= n then (None, text)
    else
      match text.[i] with
      | ':' -> (Some (String.sub text 0 i), String.sub text (i + 1) (n - i - 1))
      | c when (c >= 'a' && c <= 'z') || c = '-' -> go (i + 1)
      | _ -> (None, text)
  in
  go 0

let parse_modifier raw =
  let n = String.length raw in
  let inner () = String.sub raw 1 (n - 2) in
  if n < 1 then None
  else if n >= 2 && raw.[0] = '[' && raw.[n - 1] = ']' then
    let text = Tw.Parse.decode_arbitrary_value (inner ()) in
    if String.trim text = "" then None
    else Some (Bracketed { hint = None; text })
  else if n >= 2 && raw.[0] = '(' && raw.[n - 1] = ')' then
    let name = inner () in
    if String.length name >= 2 && String.sub name 0 2 = "--" then
      Some
        (Bracketed
           { hint = None; text = String.concat "" [ "var("; name; ")" ] })
    else None
  else if is_named_value raw then Some (Bare raw)
  else None

(* The roots a candidate could have, as Tailwind's [findRoots] yields them: the
   whole base when it is a root, then every prefix ending before a [-]. A prefix
   that leaves no value behind ends the search. *)
let candidate_roots is_root base =
  let found = ref (if is_root base then [ (base, None) ] else []) in
  let rec go idx =
    if idx > 0 then begin
      let head = String.sub base 0 idx in
      let rest = String.sub base (idx + 1) (String.length base - idx - 1) in
      let next () =
        go (Option.value ~default:0 (String.rindex_from_opt base (idx - 1) '-'))
      in
      if not (is_root head) then next ()
      else if rest <> "" then begin
        found := (head, Some rest) :: !found;
        next ()
      end
    end
  in
  Option.iter go (String.rindex_opt base '-');
  List.rev !found

(* The [(--x)] and [(color:--x)] shorthands stand for an arbitrary [var()]. *)
let paren_shorthand base idx =
  let n = String.length base in
  let inner = String.sub base (idx + 2) (n - idx - 3) in
  let hint, name =
    match segment ':' inner with [ h; v ] -> (Some h, v) | _ -> (None, inner)
  in
  if String.length name < 2 || String.sub name 0 2 <> "--" then None
  else
    let reference = String.concat "" [ "var("; name; ")" ] in
    Some (Bracketed { hint; text = reference })

let bracket_value raw =
  match String.index_opt raw '[' with
  | None -> None
  | Some from ->
      let n = String.length raw in
      if raw.[n - 1] <> ']' then None
      else
        let text =
          Tw.Parse.decode_arbitrary_value
            (String.sub raw (from + 1) (n - from - 2))
        in
        let hint, text = split_hint text in
        if String.trim text = "" || hint = Some "" then None
        else Some (Bracketed { hint; text })

(* The candidates [cls] reads as, given the roots the project declared. *)
let parse_functional_candidates ~is_root cls =
  match segment '/' cls with
  | [] | _ :: _ :: _ :: _ -> []
  | base :: rest ->
      let modifier_raw = match rest with [ m ] -> Some m | _ -> None in
      let modifier = Option.bind modifier_raw parse_modifier in
      let n = String.length base in
      (* A candidate carrying a modifier the reader refuses is no candidate at
         all, and neither is an empty base. *)
      if n = 0 || (modifier_raw <> None && modifier = None) then []
      else
        let candidate root value fraction =
          { root; value; fraction; modifier }
        in
        (* The base spells a value after the root; the whole base is a root of
           its own only when the utility takes no value. *)
        let with_value (root, raw) =
          match raw with
          | None -> Some (candidate root None None)
          | Some raw when String.contains raw '[' ->
              Option.map
                (fun value -> candidate root (Some value) None)
                (bracket_value raw)
          | Some raw when is_named_value raw ->
              (* A named value and a named modifier also read as the one
                 fraction a [--value(ratio)] resolves against. *)
              let fraction =
                match (modifier_raw, modifier) with
                | Some m, Some (Bare _) ->
                    Some (String.concat "" [ raw; "/"; m ])
                | _ -> None
              in
              Some (candidate root (Some (Bare raw)) fraction)
          | Some _ -> None
        in
        (* An arbitrary value ends the base, so what stands before its opener is
           the root outright rather than one of several the base could name. *)
        let arbitrary opener read =
          match re_index opener base with
          | Some idx when is_root (String.sub base 0 idx) ->
              read (String.sub base 0 idx) idx
          | _ -> []
        in
        if base.[n - 1] = ']' then
          arbitrary bracket_opener_re (fun root idx ->
              Option.to_list
                (with_value
                   (root, Some (String.sub base (idx + 1) (n - idx - 1)))))
        else if base.[n - 1] = ')' then
          arbitrary paren_opener_re (fun root idx ->
              Option.to_list
                (Option.map
                   (fun value -> candidate root (Some value) None)
                   (paren_shorthand base idx)))
        else List.filter_map with_value (candidate_roots is_root base)

(* Normalise one [--value(...)] argument the way Tailwind's preprocessing does,
   so the spellings a formatter leaves behind all name the same thing:
   [--text-\* --line-height], [--text- * --line-height] and [--text
   --line-height] are all [--text-*--line-height], and a bare namespace grows
   the [-*] it left out. *)
let normalize_value_arg arg =
  let unescape s =
    let len = String.length s in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      if s.[!i] = '\\' && !i + 1 < len && s.[!i + 1] = '*' then (
        Buffer.add_char buf '*';
        i := !i + 2)
      else (
        Buffer.add_char buf s.[!i];
        incr i)
    done;
    Buffer.contents buf
  in
  (* Whitespace before a [--] separates a namespace from a sub-key: it stands
     for the wildcard the sub-key hangs off. Every other run of it goes. *)
  let wildcard_or_drop_space s =
    let len = String.length s in
    let space c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      if space s.[!i] then
        if !i + 2 < len && s.[!i + 1] = '-' && s.[!i + 2] = '-' then
          Buffer.add_string buf "-*"
        else ()
      else Buffer.add_char buf s.[!i];
      incr i
    done;
    Buffer.contents buf
  in
  let rec collapse s =
    match re_index double_wildcard_re s with
    | None -> s
    | Some i ->
        collapse
          (String.concat ""
             [
               String.sub s 0 i; String.sub s (i + 2) (String.length s - i - 2);
             ])
  in
  let arg = collapse (wildcard_or_drop_space (unescape arg)) in
  if
    String.length arg >= 2
    && String.sub arg 0 2 = "--"
    && (not (String.contains arg '('))
    && not (Re.execp wildcard_re arg)
  then arg ^ "-*"
  else arg

(* The CSS a declared utility writes for the theme token [name]: an inline token
   stands for its own value, a reference token carries that value as the
   fallback of its own reference because nothing declares it in the generated
   sheet, and any other token is a plain reference the theme layer declares.

   Written as text, like the [--spacing()] expansion above and for the same
   reason: this pass runs over the dialect before cascade parses any of it, and
   the token's value is whatever the [@theme] block wrote, with no value type to
   build a typed reference at. What comes out is read back by cascade. *)
let theme_token_css ~theme name =
  Option.map
    (fun value ->
      if Tw.Scheme.is_inline_token theme name then value
      else if Tw.Scheme.is_reference_token theme name then
        String.concat "" [ "var(--"; name; ", "; value; ")" ]
      else String.concat "" [ "var(--"; name; ")" ])
    (Tw.Scheme.token theme name)

(* Split [arg] on the wildcard [-*]: [--text-*--line-height] is the namespace
   [--text] and the sub-key [--line-height], and [--example-*] is a namespace
   with nothing behind it. *)
let split_wildcard arg = Re.split_delim wildcard_re arg

(* A [--value] argument naming a theme namespace reads the entry the candidate
   names in it, and one naming a sub-key ([--text-*--line-height]) reads that
   sub-key of the entry, which is there only when the entry itself is. *)
let theme_arg_css ~theme arg name =
  let bare s = String.sub s 2 (String.length s - 2) in
  if String.length arg < 2 || String.sub arg 0 2 <> "--" then None
  else
    match split_wildcard arg with
    | [ namespace; "" ] ->
        theme_token_css ~theme (String.concat "-" [ bare namespace; name ])
    | namespace :: (_ :: _ as subs) ->
        let entry = String.concat "-" [ bare namespace; name ] in
        if Option.is_none (Tw.Scheme.token theme entry) then None
        else
          theme_token_css ~theme (entry ^ List.nth subs (List.length subs - 1))
    | _ -> None

type resolution = { css : string; is_ratio : bool }
(** What one [--value(...)] or [--modifier(...)] resolved to, and whether it
    read the candidate as a ratio - which rules out its modifier and takes every
    declaration that read it as something else out of the utility. *)

let plain css = Some { css; is_ratio = false }

(* A bare data type reads the candidate's own word, except [ratio], which reads
   the value and the modifier back as the one fraction they spell. *)
let resolve_bare_arg ~fraction text kind =
  let read = if kind = "ratio" then fraction else Some text in
  match read with
  | None -> None
  | Some read -> (
      if not (infer_data_type read kind) then None
      else
        match kind with
        | "ratio" -> (
            match List.map String.trim (segment '/' read) with
            | [ left; right ]
              when is_positive_integer left && is_positive_integer right ->
                Some
                  {
                    css = String.concat " " [ left; "/"; right ];
                    is_ratio = true;
                  }
            | _ -> None)
        | "number" -> if is_spacing_multiplier read then plain read else None
        | "percentage" ->
            if is_positive_integer (String.sub read 0 (String.length read - 1))
            then plain read
            else None
        | _ -> plain read)

(* One argument of a read, against the value the candidate spelled. *)
let resolve_arg ~theme ~fraction value arg =
  let arg = normalize_value_arg arg in
  let n = String.length arg in
  let quoted =
    n >= 2 && (arg.[0] = '\'' || arg.[0] = '"') && arg.[n - 1] = arg.[0]
  in
  let theme_arg = n >= 2 && String.sub arg 0 2 = "--" in
  let bracketed = n >= 2 && arg.[0] = '[' && arg.[n - 1] = ']' in
  match value with
  | Bare text when quoted ->
      if String.sub arg 1 (n - 2) = text then plain text else None
  | Bare text when theme_arg ->
      Option.bind (theme_arg_css ~theme arg text) plain
  | Bare text when List.mem arg bare_value_data_types ->
      resolve_bare_arg ~fraction text arg
  | Bare _ -> None
  | Bracketed _ when not bracketed -> None
  | Bracketed { hint; text } -> (
      let kind = String.sub arg 1 (n - 2) in
      if kind = "*" then plain text
      else
        match hint with
        | Some spelled -> if spelled = kind then plain text else None
        | None -> if infer_data_type text kind then plain text else None)

(* [--default(4)] answers for a candidate that spelled no value at all. *)
let default_arg arg =
  let arg = String.trim arg in
  let n = String.length arg in
  let head = "--default(" in
  let m = String.length head in
  if n > m && String.sub arg 0 m = head && arg.[n - 1] = ')' then
    Some (String.trim (String.sub arg m (n - m - 1)))
  else None

let resolve_read ~theme ~value ~fraction args =
  match value with
  | None -> Option.bind (List.find_map default_arg args) plain
  | Some value -> List.find_map (resolve_arg ~theme ~fraction value) args

type read_state = {
  mutable used_value : bool;
  mutable resolved_value : bool;
  mutable used_modifier : bool;
  mutable resolved_modifier : bool;
  mutable ratio : bool;
}
(** What the whole body's reads did, which is what decides whether the candidate
    is a utility at all. *)

(** A declaration once its reads are resolved: kept, dropped because a read did
    not resolve, or kept unless a [--value(ratio)] resolved somewhere else. *)
type declaration_result = Keep of string | Drop | Ratio_drop of string

(* Resolve every read in one declaration. A read that does not resolve takes the
   declaration with it, and stops the rest of it from being read at all. *)
let resolve_declaration ~theme ~candidate ~state text =
  let index = Index.v text in
  let len = String.length text in
  let buf = Buffer.create len in
  let dropped = ref false in
  let non_ratio = ref false in
  let read ~value ~fraction (block : Index.block) =
    match
      resolve_read ~theme ~value ~fraction
        (List.map String.trim (segment ',' block.body))
    with
    | None ->
        dropped := true;
        None
    | Some resolved ->
        Buffer.add_string buf resolved.css;
        Some (resolved, block.next)
  in
  let rec go i =
    if i >= len || !dropped then ()
    else
      match Index.call index ~name:"--value" i with
      | Some block -> (
          state.used_value <- true;
          match
            read ~value:candidate.value ~fraction:candidate.fraction block
          with
          | None -> ()
          | Some (resolved, next) ->
              state.resolved_value <- true;
              if resolved.is_ratio then state.ratio <- true
              else non_ratio := true;
              go next)
      | None -> (
          match Index.call index ~name:"--modifier" i with
          | Some block -> (
              state.used_modifier <- true;
              match read ~value:candidate.modifier ~fraction:None block with
              | None -> ()
              | Some (_, next) ->
                  state.resolved_modifier <- true;
                  go next)
          | None ->
              Buffer.add_char buf text.[i];
              go (i + 1))
  in
  go 0;
  if !dropped then Drop
  else if !non_ratio then Ratio_drop (Buffer.contents buf)
  else Keep (Buffer.contents buf)

(** A body read as declarations and the punctuation between them, so one whose
    reads did not resolve can be dropped without disturbing the rest. *)
type body_piece = Declaration of string | Punctuation of string

(* A [;] inside a bracket group is part of the declaration around it; a [;], [{]
   or [}] outside one ends it, nested rules included. Component values supply
   those already-matched boundaries. *)
let body_pieces body =
  let pieces = ref [] in
  let from = ref 0 in
  let cut (at, next) =
    pieces :=
      Punctuation (String.sub body at (next - at))
      :: Declaration (String.sub body !from (at - !from))
      :: !pieces;
    from := next
  in
  Components.array body |> Array.to_list |> Components.punctuation
  |> List.iter cut;
  List.rev
    (Declaration (String.sub body !from (String.length body - !from)) :: !pieces)

let rec rebuild_body ~ratio = function
  | [] -> []
  | (Declaration _, Keep text) :: rest -> text :: rebuild_body ~ratio rest
  | (Declaration _, Ratio_drop text) :: rest when not ratio ->
      text :: rebuild_body ~ratio rest
  | (Declaration _, _) :: (Punctuation ";", _) :: rest ->
      rebuild_body ~ratio rest
  | (Declaration _, _) :: rest -> rebuild_body ~ratio rest
  | (Punctuation text, _) :: rest -> text :: rebuild_body ~ratio rest

(* The body one [@utility NAME-*] declaration gives [candidate], or nothing when
   the candidate is no utility of that declaration. *)
let functional_body ~theme ~body candidate =
  let state =
    {
      used_value = false;
      resolved_value = false;
      used_modifier = false;
      resolved_modifier = false;
      ratio = false;
    }
  in
  let resolved =
    List.map
      (fun piece ->
        match piece with
        | Punctuation _ -> (piece, Keep "")
        | Declaration text ->
            if Re.execp value_or_modifier_re text then
              (piece, resolve_declaration ~theme ~candidate ~state text)
            else (piece, Keep text))
      (body_pieces body)
  in
  let modifier = candidate.modifier <> None in
  if not (state.used_value && state.resolved_value) then None
  else if state.used_modifier && modifier && not state.resolved_modifier then
    None
  else if state.ratio && state.resolved_modifier then None
  else if modifier && (not state.ratio) && not state.resolved_modifier then None
  else Some (String.concat "" (rebuild_body ~ratio:state.ratio resolved))

let functional_roots udefs =
  List.filter_map (fun (n, _) -> functional_root n) udefs

(* The candidates [cls] reads as against the functional [@utility] names of
   [udefs]. *)
let functional_candidates udefs cls =
  match functional_roots udefs with
  | [] -> []
  | roots ->
      parse_functional_candidates ~is_root:(fun r -> List.mem r roots) cls

(* The body [cls] gets from the functional [@utility] declarations of [udefs]:
   every declaration whose root it names, resolved against it, in the order they
   were written. Tailwind registers each of them and applies them all. *)
let functional_utility_body ~theme ~udefs cls =
  List.find_map
    (fun candidate ->
      match
        List.filter_map
          (fun (name, body) ->
            if functional_root name = Some candidate.root then
              functional_body ~theme ~body candidate
            else None)
          udefs
      with
      | [] -> None
      | bodies -> Some (String.concat "" bodies))
    (functional_candidates udefs cls)

(* [@utility NAME { ... }] declares a project's own utility class, and [@utility
   NAME-* { ... }] one whose candidate carries a value its body reads back with
   [--value()] and [--modifier()]. Both forms are read. *)
let take_custom_utilities css = take_named_defs "@utility" css

(* The at-keywords Tailwind's dialect adds to CSS. They are input for the
   generator, which reads each of them above -- for a definition, for an
   expansion, or for the theme and the plugins the entrypoint asks for -- and
   Tailwind emits none of them. This list is the whole of what makes an at-rule
   one of Tailwind's; anything else is the author's CSS, and passes through
   whether or not tw or a parser knows what it means. *)
let tailwind_directives =
  [
    "@apply";
    "@config";
    "@custom-variant";
    "@plugin";
    "@reference";
    "@slot";
    "@source";
    "@tailwind";
    "@theme";
    "@utility";
    "@variant";
  ]

(* Drop them, so none reaches a browser that has no meaning for it. What is
   still here declared nothing usable -- an [@utility] with no name, a variant
   tw cannot expand -- or names something outside the stylesheet, so there is
   nothing to salvage from the text either. *)
let drop_directives css =
  rewrite ~names:tailwind_directives css (fun index _ i ->
      List.find_map
        (fun name ->
          match Index.at_rule index ~name i with
          | Some { block = { next; _ }; _ } -> Some next
          | None ->
              Option.map
                (fun ({ next; _ } : Index.statement) -> next)
                (Index.at_statement index ~name i))
        tailwind_directives)

let fill_slots template body =
  rewrite ~names:[ "@slot" ] template (fun index buf i ->
      match Index.at_statement index ~name:"@slot" i with
      | Some { next; _ } ->
          Buffer.add_string buf body;
          Some next
      | None -> None)

(* Replace [@variant NAME { decls }] with the variant's body, substituting the
   declarations at each [@slot]. Nested [@variant]s expand outermost-first, so
   the recursion re-runs over the result. *)
let rec expand_variants ~depth defs css =
  if depth > 8 then css
  else
    let changed = ref false in
    let out =
      rewrite ~names:[ "@variant" ] css (fun index buf i ->
          match Index.at_rule index ~name:"@variant" i with
          | Some { prelude; block = { body; next }; _ }
            when List.mem_assoc prelude defs ->
              let template = List.assoc prelude defs in
              changed := true;
              Buffer.add_string buf (fill_slots template body);
              Some next
          | _ -> None)
    in
    if !changed then expand_variants ~depth:(depth + 1) defs out else out

(* {2 [not-] over a declared variant}

   Tailwind's [not-] negates what a variant's body builds, not the variant's
   name, so a project that declares [dark] itself gets the negation of its own
   body. Tailwind rewrites one node in place: sibling branches would have to be
   nested into a single conjunction, which it does not build, so a body of more
   than one refuses the candidate. Under that node the first leaf decides: the
   style rule on the way to it goes under [:not()] with [&] read as [*], and the
   condition takes a [not]. Tailwind also refuses a path through two rules or
   two conditions, a pseudo-element, which [:not()] cannot hold, and an at-rule
   other than [@media], [@supports] and [@container]. A compound [and]/[or]
   condition is refused here as well: Tailwind puts a [not] in front of it that
   makes the query invalid, so its rule never applies either. *)

let body_rules body =
  let items =
    (Cascade.Parser.block_contents (Cascade.Reader.of_string body)).value
  in
  List.fold_right
    (fun item (decls, rules) ->
      match item with
      | `Decls [] -> (decls, rules)
      | `Decls _ -> (true, rules)
      | `Rule (Cascade.Component.At { node = { name = "slot"; _ }; _ }) ->
          (decls, rules)
      | `Rule rule -> (decls, rule :: rules))
    items (false, [])

let rule_body = function
  | Cascade.Component.Qualified { node = { block; _ }; _ } ->
      Some (Cascade.Parser.string_of_components block.node.value)
  | At { node = { block; _ }; _ } ->
      Option.map
        (fun (b : Cascade.Component.block Cascade.Component.node) ->
          Cascade.Parser.string_of_components b.node.value)
        block

(* The path from [rule] down to the first node holding nothing but the slot. *)
let rec first_leaf path rule =
  let path = rule :: path in
  match rule_body rule with
  | None -> Some (List.rev path)
  | Some body -> (
      match body_rules body with
      | false, [] -> Some (List.rev path)
      | _, rules -> List.find_map (first_leaf path) rules)

let negated_selector sel =
  if Css.Selector.has_pseudo_element sel then None
  else
    let arms = Option.value ~default:[ sel ] (Css.Selector.as_list sel) in
    (* A universal beside another simple selector matches nothing more, and the
       CLI's minifier drops it there. *)
    let implied = function Css.Selector.Universal None -> false | _ -> true in
    let universal =
      Css.Selector.map (function
        | Css.Selector.Nesting -> Css.Selector.Universal None
        | Css.Selector.Compound parts -> (
            match List.filter implied parts with
            | [] -> Css.Selector.Universal None
            | [ part ] -> part
            | parts -> Css.Selector.Compound parts)
        | node -> node)
    in
    Some
      (Css.Selector.Compound
         [ Css.Selector.Nesting; Css.Selector.Not (List.map universal arms) ])

let rec negated_media : Css.Media.t -> Css.Media.t option = function
  | Cond (Not condition) -> Some (Cond condition)
  | Cond (Feature _ as condition) -> Some (Cond (Not condition))
  | Cond (And _ | Or _) | Type { prefix = Some Only; _ } -> None
  | Type ({ prefix = Some Not; _ } as media) ->
      Some (Type { media with prefix = None })
  | Type ({ prefix = None; _ } as media) ->
      Some (Type { media with prefix = Some Not })
  | List [ media ] -> negated_media media
  | List _ -> None

let negated_supports : Css.Supports.t -> Css.Supports.t option = function
  | Not condition -> Some condition
  | And _ | Or _ -> None
  | condition -> Some (Not condition)

let rec negated_container : Css.Container.t -> Css.Container.t option = function
  | Named (name, condition) ->
      Option.map
        (fun condition -> Css.Container.Named (name, condition))
        (negated_container condition)
  | Not condition -> Some condition
  | And _ | Or _ -> None
  | condition -> Some (Not condition)

(* The negated condition as the header of a template block. *)
let negated_at_rule name prelude =
  let prelude = String.trim prelude in
  let header keyword to_string negate of_string =
    Option.map
      (fun condition -> String.concat "" [ keyword; " "; to_string condition ])
      (negate (of_string prelude))
  in
  match name with
  | "media" ->
      header "@media"
        (Css.Media.to_string ~minify:false)
        negated_media Css.Media.of_string_strict
  | "supports" ->
      header "@supports"
        (Css.Supports.to_string ~minify:false)
        negated_supports Css.Supports.of_string
  | "container" ->
      header "@container"
        (Css.Container.to_string ~minify:false)
        negated_container Css.Container.of_string
  | _ -> None

let all_some options =
  List.fold_right
    (fun option acc ->
      match (option, acc) with Some x, Some xs -> Some (x :: xs) | _ -> None)
    options (Some [])

let negated_path path =
  let selectors, conditions =
    List.partition_map
      (function
        | Cascade.Component.Qualified { node = { prelude; _ }; _ } ->
            Left (Cascade.Parser.string_of_components prelude)
        | At { node = { name; prelude; _ }; _ } ->
            Right (name, Cascade.Parser.string_of_components prelude))
      path
  in
  match (selectors, conditions) with
  | _ :: _ :: _, _ | _, _ :: _ :: _ -> None
  | selectors, conditions -> (
      let selectors =
        all_some
          (List.map
             (fun s -> negated_selector (Css.Selector.of_string s))
             selectors)
      in
      let conditions =
        all_some (List.map (fun (name, p) -> negated_at_rule name p) conditions)
      in
      match (selectors, conditions) with
      | Some selectors, Some headers ->
          let slot header = header ^ " { @slot; }" in
          Some
            (String.concat " "
               (List.map
                  (fun s -> slot (Css.Selector.to_string ~minify:false s))
                  selectors
               @ List.map slot headers))
      | _ -> None)

let negated_variant ~defs template =
  match body_rules (expand_variants ~depth:0 defs template) with
  | false, [ node ] -> (
      match Option.bind (first_leaf [] node) negated_path with
      | negation -> negation
      | exception (Cascade.Cursor.Parse_error _ | Invalid_argument _ | Failure _)
        ->
          None)
  | _ -> None

let with_negated_variants defs =
  List.fold_left
    (fun (defs, refused) (name, template) ->
      let negation = "not-" ^ name in
      if List.mem_assoc negation defs then (defs, refused)
      else
        match negated_variant ~defs template with
        | Some body -> (defs @ [ (negation, body) ], refused)
        | None -> (defs, refused @ [ negation ]))
    (defs, []) defs

(* A project that declared [--spacing] in an [@theme inline] block has no
   variable to reference, so the step is multiplied out here instead, the way
   Tailwind's inline theme does. *)
let inline_spacing ~theme multiple =
  if not (Tw.Scheme.is_inline_token theme "spacing") then None
  else
    let scaled (step : Css.length) times : Css.length option =
      match step with
      | Css.Px v -> Some (Css.Px (v *. times))
      | Css.Rem v -> Some (Css.Rem (v *. times))
      | Css.Em v -> Some (Css.Em (v *. times))
      | _ -> None
    in
    match
      ( Option.bind (Tw.Scheme.token theme "spacing") Css.parse_length,
        float_of_string_opt (String.trim multiple) )
    with
    | Some step, Some times ->
        Option.map (Css.Pp.to_string Css.pp_length) (scaled step times)
    | _ -> None

(* Tailwind's [--spacing(N)] is shorthand for the spacing scale. It is not CSS,
   so a parser rejects the declaration and it drops out of the output. *)
let expand_spacing_fn ~theme css =
  rewrite ~names:[ "--spacing(" ] css (fun index buf i ->
      match Index.call index ~name:"--spacing" i with
      | Some { body; next } ->
          Buffer.add_string buf
            (match inline_spacing ~theme body with
            | Some value -> value
            | None -> String.concat "" [ "calc(var(--spacing) * "; body; ")" ]);
          Some next
      | None -> None)

(* The value a theme token carries. The palette is not in [Scheme]'s table: a
   colour is catalogued, and [theme_color_decl] is what reads it back. Asking
   [Scheme.token] alone left [theme(--color-red-500)] unresolved, and an
   unresolved [theme()] is not CSS, so the parser dropped the declaration and
   the author's rule with it. *)
let theme_token_value theme name =
  match Tw.Scheme.token theme name with
  | Some _ as value -> value
  | None ->
      Option.map
        (fun decl -> String.trim (Cascade.Css.declaration_value decl))
        (Tw.Color.Handler.theme_color_decl ~theme name)

(* Tailwind's [--alpha(<color> / <percentage>)] is shorthand for compositing a
   colour with its own alpha. It is not CSS, so a parser rejects the declaration
   and it drops out of the output, leaving the element with no colour at all. It
   spells the same [color-mix()] the author could have written, which is what
   [authored_color_mix_fallbacks] then gives a legacy fallback and an
   [@supports] arm - the shape the reference emits. *)
let expand_alpha_fn css =
  rewrite ~names:[ "--alpha(" ] css (fun index buf i ->
      match Index.call index ~name:"--alpha" i with
      | Some { body; next } -> (
          match String.index_opt body '/' with
          | None -> None
          | Some slash ->
              let colour = String.trim (String.sub body 0 slash) in
              let alpha =
                String.trim
                  (String.sub body (slash + 1) (String.length body - slash - 1))
              in
              Buffer.add_string buf
                (String.concat ""
                   [
                     "color-mix(in oklab, ";
                     colour;
                     " ";
                     alpha;
                     ", transparent)";
                   ]);
              Some next)
      | None -> None)

(* [theme()] also takes the dotted path of a v3 config ([theme(fontSize.sm)]),
   which names the same token under its old namespace. *)
let v3_theme_namespaces =
  [
    ("fontSize", "text");
    ("lineHeight", "leading");
    ("letterSpacing", "tracking");
    ("fontWeight", "font-weight");
    ("fontFamily", "font");
    ("colors", "color");
    ("borderRadius", "radius");
    ("boxShadow", "shadow");
    ("dropShadow", "drop-shadow");
    ("screens", "breakpoint");
    ("spacing", "spacing");
    ("transitionTimingFunction", "ease");
    ("animation", "animate");
    ("blur", "blur");
  ]

let v3_theme_token theme path =
  let unquote s =
    let n = String.length s in
    if n >= 2 && (s.[0] = '"' || s.[0] = '\'') && s.[n - 1] = s.[0] then
      String.sub s 1 (n - 2)
    else s
  in
  match String.split_on_char '.' (unquote path) with
  | [] | [ _ ] -> None
  | ns :: rest -> (
      match List.assoc_opt ns v3_theme_namespaces with
      | None -> None
      | Some prefix -> (
          let key = String.concat "-" rest in
          match theme_token_value theme (prefix ^ "-" ^ key) with
          | Some _ as v -> v
          | None -> (
              match (ns, float_of_string_opt key) with
              | ("spacing" | "lineHeight"), Some n -> Tw.Theme.spacing_times n
              | _ -> None)))

(* Tailwind's [theme(--token)] inlines the token's value. It is not CSS, and it
   appears in places a [var()] could not stand anyway, such as a media query
   condition. An unknown token is left alone rather than guessed at. *)
let resolve_theme_fn ~theme css =
  rewrite ~names:[ "theme(" ] css (fun index buf i ->
      match Index.call index ~name:"theme" i with
      | None -> None
      | Some { body; next } -> (
          let name = String.trim body in
          let bare =
            if String.length name > 2 && String.sub name 0 2 = "--" then
              String.sub name 2 (String.length name - 2)
            else name
          in
          match
            match theme_token_value theme bare with
            | Some _ as v -> v
            | None -> v3_theme_token theme name
          with
          | Some value ->
              Buffer.add_string buf value;
              Some next
          | None -> None))

(* Where an at-rule prelude runs, for the at-rules whose prelude can hold a
   [--theme()] call. A [var()] is not read there, so such a call gives the value
   itself. *)
let at_rule_preludes index css =
  let names = [ "@media"; "@container"; "@supports"; "@custom-media" ] in
  let prelude_at acc i name =
    match Index.at_rule index ~name i with
    | Some { brace; _ } -> (i, brace) :: acc
    | None -> (
        match Index.at_statement index ~name i with
        | Some { next; _ } -> (i, next) :: acc
        | None -> acc)
  in
  let rec scan from acc =
    match String.index_from_opt css from '@' with
    | None -> acc
    | Some i ->
        scan (i + 1)
          (List.fold_left (fun acc n -> prelude_at acc i n) acc names)
  in
  scan 0 []

(* [--theme(--token)] reads a theme token from author CSS. It is a reference the
   theme layer then declares, with any fallback threaded into it; the value
   itself where the call says [inline] or stands in an at-rule prelude; and the
   fallback alone when the theme has no such token. A call naming no token is
   left alone, as the reference refuses to compile one. *)
let dashed_theme_value ~theme ~in_prelude body =
  match split_top_level ',' body with
  | [] -> None
  | first :: fallback -> (
      let first = String.trim first in
      let inline = String.ends_with ~suffix:" inline" first in
      let token =
        if inline then
          String.trim (String.sub first 0 (String.length first - 7))
        else first
      in
      let fallback = String.concat ", " (List.map String.trim fallback) in
      if not (String.length token > 2 && String.sub token 0 2 = "--") then None
      else
        let bare = String.sub token 2 (String.length token - 2) in
        match theme_token_value theme bare with
        | Some value when inline || in_prelude -> Some value
        | Some _ when fallback = "" ->
            Some (String.concat "" [ "var("; token; ")" ])
        | Some _ ->
            Some (String.concat "" [ "var("; token; ", "; fallback; ")" ])
        | None when fallback = "" -> None
        | None -> Some fallback)

let resolve_dashed_theme_fn ~theme css =
  rewrite ~names:[ "--theme(" ] css (fun index buf ->
      let preludes = at_rule_preludes index css in
      fun i ->
        match Index.call index ~name:"--theme" i with
        | Some { body; next } -> (
            let in_prelude =
              List.exists (fun (start, stop) -> start < i && i < stop) preludes
            in
            match dashed_theme_value ~theme ~in_prelude body with
            | Some value ->
                Buffer.add_string buf value;
                Some next
            | None -> None)
        | None -> None)

(* [to_css] heads a utility's selector with the utility's own class, and a
   variant decorates it in place, as [.dark\:fill-gray-400:where(.dark, ...)].
   Swapping that class for [&] turns the rule into a nested one the author's
   selector can host, so a variant survives [@apply] without being reimplemented
   here. The class is not always the leftmost one in the selector: [divide-*]
   wraps it in [:where(.divide-x > :not(:last-child))] and [in-*] heads the
   selector with the ancestor's class instead, so it is picked out by name among
   the classes the [@apply] asked for. A selector naming none of them keeps the
   leftmost class, which is what the variants tw generates put there. *)
let rec heads_with_class pick = function
  | Cascade.Selector.Class name -> pick name
  | Compound parts -> List.exists (heads_with_class pick) parts
  | Combined (left, _, right) ->
      heads_with_class pick left || heads_with_class pick right
  | List arms -> List.exists (heads_with_class pick) arms
  | _ -> false

let rec swap_heading pick = function
  | Cascade.Selector.Class name when pick name -> Cascade.Selector.Nesting
  | Compound parts -> Compound (List.map (swap_heading pick) parts)
  | Combined (left, combinator, right) ->
      Combined (swap_heading pick left, combinator, swap_heading pick right)
  | List arms -> List (List.map (swap_heading pick) arms)
  | node -> node

(* The utility's class can also stand inside a pseudo-class argument, as the
   typography plugin's [:where(.prose > ul > li p)] under [.prose] does. Where
   the class also stands outside every argument, that occurrence is the one the
   applying rule takes the place of, and the one inside is a descendant test
   that stays. Only a class found nowhere else, as [divide-*] puts it in
   [:where()], is swapped where it sits. *)
let nest_on_ampersand ~classes sel =
  let swap pick =
    Cascade.Selector.map (function
      | Cascade.Selector.Class name when pick name -> Cascade.Selector.Nesting
      | node -> node)
  in
  let own name = List.mem name classes in
  let arm a =
    if heads_with_class own a then swap_heading own a
    else if Cascade.Selector.exists_class own a then swap own a
    else
      match Cascade.Selector.first_class a with
      | Some name -> swap (String.equal name) a
      | None -> a
  in
  match Cascade.Selector.as_list sel with
  | Some arms -> Cascade.Selector.list (List.map arm arms)
  | None -> arm sel

(* Split a class name on its variant separators. A [:] inside [[&>*]] or [(--x)]
   is part of the segment, not a separator. *)
let variant_segments name =
  let len = String.length name in
  let rec seg_end i depth =
    if i >= len then i
    else
      match name.[i] with
      | '[' | '(' -> seg_end (i + 1) (depth + 1)
      | ']' | ')' -> seg_end (i + 1) (depth - 1)
      | ':' when depth = 0 -> i
      | _ -> seg_end (i + 1) depth
  in
  let rec go i acc =
    let stop = seg_end i 0 in
    let seg = String.sub name i (stop - i) in
    if stop >= len then List.rev (seg :: acc) else go (stop + 1) (seg :: acc)
  in
  go 0 []

(* Separate the variants a project declared with [@custom-variant] from the rest
   of the class. They cannot go through [Tw.of_string], which only knows the
   built-in variants, so they are re-emitted as [@variant] blocks for
   [expand_variants] to expand. A declared variant is picked out wherever it
   sits in the chain — [lg:dark:flex] as much as [dark:lg:flex] — and the
   built-in prefixes stay attached to the utility, which keeps their media
   queries wrapped around the declared variant's selector. *)
let split_declared_variants defs name =
  match variant_segments name with
  | [] | [ _ ] -> ([], name)
  | segs ->
      let bare = List.nth segs (List.length segs - 1) in
      let prefix = List.filteri (fun i _ -> i < List.length segs - 1) segs in
      let declared, builtin =
        List.partition (fun s -> List.mem_assoc s defs) prefix
      in
      (declared, String.concat ":" (builtin @ [ bare ]))

(* Whether [seen] already holds [stmt], recording it when it does not.
   [Css.hash_statement] buckets a statement and [Css.equal_statement] settles
   the bucket, so identity is decided on the statement rather than on the CSS
   text it renders to. *)
let seen_statement seen stmt =
  let bucket = Css.hash_statement stmt in
  if List.exists (Css.equal_statement stmt) (Hashtbl.find_all seen bucket) then
    true
  else begin
    Hashtbl.add seen bucket stmt;
    false
  end

let dedup_statements stmts =
  let seen = Hashtbl.create 8 in
  List.filter (fun stmt -> not (seen_statement seen stmt)) stmts

(* A utility's selector names a class, the theme block's [:root, :host] does
   not. Asked of the selector itself rather than of its text, where a '.' also
   comes from an attribute value or a decimal inside a pseudo argument. *)
let rec merge_same_selector = function
  | a :: b :: rest -> (
      match (Css.as_rule a, Css.as_rule b) with
      | Some (sa, da, []), Some (sb, db, []) when Css.Selector.equal sa sb ->
          merge_same_selector (Css.rule ~selector:sa (da @ db) :: rest)
      | _ -> a :: merge_same_selector (b :: rest))
  | stmts -> stmts

let render_nested_utilities ~classes stmts =
  stmts
  |> Css.map (fun sel decls ->
      Css.rule ~selector:(nest_on_ampersand ~classes sel) decls)
  |> merge_same_selector |> Css.v |> Css.to_string ~minify:true

(* The declarations of [names], rewritten to nest under the [&] of whatever rule
   applies them, plus the statements that must stay at the top of the sheet. A
   [@layer properties] block holds the initial value of the variables the
   utilities set, on the universal selector: nested under an author rule it
   would come out as [.box *], so it is hoisted instead. *)
(* What goes to the top of the sheet rather than under the [&] that applied the
   utilities: a rule naming no class, which is the [:root] block declaring the
   theme tokens they read, and the three at-rules that stand on their own -
   [@layer properties] (nested it would come out as [.box *]), [@property], and
   the [@keyframes] an animation utility names. An at-rule that wraps utility
   rules - [@supports], [@media] - is not one of those: it nests with what it
   holds.

   The theme block used to be dropped here rather than hoisted, so
   [@apply rounded-lg] emitted a rule reading [var(--radius-lg)] with nothing
   in the sheet declaring it. Colours and spacing hid it for a long time: those
   two are rescued downstream by [Build.referenced_theme_decls], so only the
   families carrying their own namespace showed the defect. *)
let stands_at_top stmt =
  match Css.statement_selector stmt with
  | Some sel -> not (Css.Selector.exists_class (fun _ -> true) sel)
  | None ->
      Css.as_layer stmt <> None
      || Css.as_property stmt <> None
      || Css.as_keyframes stmt <> None

(* The theme block goes back inside [@layer theme], where [merge_named_layers]
   folds it into the generated sheet's own block. An at-rule stands at top
   level, which is where Tailwind emits it. *)
let at_top_of_sheet stmt =
  match Css.statement_selector stmt with
  | None -> stmt
  | Some _ -> Css.layer ~name:[ "theme" ] [ stmt ]

let nested_utilities ~theme names =
  (* The generated sheet already declares a [theme(static)] theme whole, with
     every keyframe, so a render that only lends its declarations to author CSS
     or a routed variant must not declare it again. *)
  let theme = { theme with Tw.Scheme.static_theme = false } in
  let of_name n =
    match Tw.of_string ~theme n with Ok s -> Some s | Error _ -> None
  in
  match List.filter_map of_name names with
  | [] -> ("", [])
  | styles ->
      let sheet =
        Tw.to_css ~theme ~base:false ~forms:false ~layers:false styles
      in
      (* The class each utility carries in its own selector, spelled the way
         [to_css] spells it rather than the way the [@apply] did. *)
      let classes = String.split_on_char ' ' (Tw.to_classes styles) in
      let top, nestable = List.partition stands_at_top (Css.statements sheet) in
      let hoisted = List.map at_top_of_sheet top in
      (* One [@apply] pulls in several utilities, each with a rule of its own.
         They all decorate the same [&], so they belong in one rule, the way
         Tailwind emits them; left apart, each is a rule of the author's
         selector holding one declaration. *)
      (* The hoisted statements go back one by one, not as one block: two
         utilities bring overlapping [@property] sets, and deduping the blocks
         whole re-emits every property they do not share. *)
      (render_nested_utilities ~classes nestable, hoisted)

(* Append each statement unless it is already there: every utility that sets the
   same variable brings back the same hoisted [@property]. *)
let add_once buf seen items =
  List.iter
    (fun stmt ->
      if not (seen_statement seen stmt) then
        Buffer.add_string buf (Css.to_string ~minify:true (Css.v [ stmt ])))
    items

let apply_names css start stop =
  String.sub css start (stop - start)
  |> String.split_on_char ' '
  |> List.concat_map (String.split_on_char '\n')
  |> List.map String.trim
  |> List.filter (fun name -> name <> "")

let emit_apply_name ~theme ~defs ~udefs ~buf ~hoisted ~seen name =
  let variants, bare = split_declared_variants defs name in
  (* [@apply line-t] names a utility the project declared, whose body is author
     CSS in its own right. *)
  let body, top =
    match List.assoc_opt bare udefs with
    | Some decls -> (decls, [])
    | None -> (
        match functional_utility_body ~theme ~udefs bare with
        | Some decls -> (decls, [])
        | None -> nested_utilities ~theme [ bare ])
  in
  add_once hoisted seen top;
  if body <> "" then begin
    List.iter
      (fun variant ->
        Buffer.add_string buf (String.concat "" [ "@variant "; variant; "{" ]))
      variants;
    Buffer.add_string buf body;
    List.iter (fun _ -> Buffer.add_char buf '}') variants
  end

let plain_apply_name ~defs ~udefs name =
  match split_declared_variants defs name with
  | [], bare
    when (not (List.mem_assoc bare udefs))
         && functional_candidates udefs bare = [] ->
      Some bare
  | _ -> None

let rec take_plain_apply_run ~defs ~udefs acc = function
  | name :: rest when Option.is_some (plain_apply_name ~defs ~udefs name) ->
      take_plain_apply_run ~defs ~udefs (name :: acc) rest
  | rest -> (List.rev acc, rest)

let rec emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen = function
  | [] -> ()
  | name :: rest as names -> (
      match plain_apply_name ~defs ~udefs name with
      | None ->
          emit_apply_name ~theme ~defs ~udefs ~buf ~hoisted ~seen name;
          emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen rest
      | Some _ ->
          let run, rest = take_plain_apply_run ~defs ~udefs [] names in
          let bare = List.filter_map (plain_apply_name ~defs ~udefs) run in
          let body, top = nested_utilities ~theme bare in
          add_once hoisted seen top;
          Buffer.add_string buf body;
          emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen rest)

(* Tailwind's [@apply] pulls a utility's declarations into an author rule. It is
   not CSS, so the at-rule drops out and takes the whole rule with it once the
   rule is left empty. *)
let expand_apply ~theme ~defs ?(udefs = []) css =
  let hoisted = Buffer.create 0 in
  let seen = Hashtbl.create 64 in
  let css =
    rewrite ~names:[ "@apply" ] css (fun index buf i ->
        match Index.at_statement index ~name:"@apply" i with
        | Some { prelude; next } ->
            (* A utility with no declared variant and no body of its own
               decorates the applying rule's [&] directly. A run of those
               renders in one call, so their declarations land in a single rule
               the way Tailwind emits them, rather than one rule of the author's
               selector per utility. *)
            let names = apply_names prelude 0 (String.length prelude) in
            emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen names;
            Some next
        | None -> None)
  in
  (* The hoisted blocks go last: their layer is ordered by the sheet's [@layer]
     statement, not by where they sit. *)
  if Buffer.length hoisted = 0 then css else css ^ Buffer.contents hoisted

(* The names an [@variant NAME {] header uses inside a body. *)
let variant_names_in css =
  if not (mentions "@variant" css) then []
  else
    let index = Index.v css in
    let len = String.length css in
    let rec go i acc =
      if i >= len then List.rev acc
      else
        match Index.at_rule index ~name:"@variant" i with
        | Some { prelude; brace; _ } when prelude <> "" ->
            go (brace + 1) (prelude :: acc)
        | _ -> go (i + 1) acc
    in
    go 0 []

(* Replace the first occurrence of [needle] in [hay]. *)
let replace_first ~needle ~by hay =
  let n = String.length needle and h = String.length hay in
  let rec at i =
    if i + n > h then None
    else if String.sub hay i n = needle then Some i
    else at (i + 1)
  in
  match at 0 with
  | None -> None
  | Some i ->
      Some
        (String.concat ""
           [ String.sub hay 0 i; by; String.sub hay (i + n) (h - i - n) ])

(* The [@variant] body a built-in variant expands to. [Tw.of_string] knows the
   variants, but only as part of a whole utility, so derive the wrapper from
   what it emits around a probe with a single declaration and put [@slot] where
   that declaration was. This is what lets a project's [@utility] carry a
   built-in prefix, which the [@variant] machinery otherwise only has templates
   for when the project declared it. *)
let builtin_variant_template ~theme name =
  let body, _ = nested_utilities ~theme [ name ^ ":float-none" ] in
  if body = "" then None
  else
    (* A media variant wraps the probe in a bare [&], which would add a nesting
       level the utility's own body cannot survive: its [@variant before] and
       the [@supports] an opacity colour emits end up three deep and the sheet
       no longer parses. Drop that level by putting the slot in its place. *)
    match replace_first ~needle:"&{float:none}" ~by:"@slot;" body with
    | Some t -> Some t
    | None -> replace_first ~needle:"float:none" ~by:"@slot;" body

let apply_variants ?(extra_defs = []) ?(udefs = []) ~theme css =
  (* What an [@apply] pulls into the author's CSS is not a utility, so the
     import's [important] does not reach it. *)
  let theme = { theme with Tw.Scheme.important = false } in
  (* The [@utility] declarations taken out here are the ones an [@apply] in this
     same file names. Dropping them left [@apply card] resolving against an
     empty table, so it named no utility and the rule it decorated came out
     without the declarations the custom utility carries - silently, since an
     [@apply] that names nothing is not an error. *)
  let css, own_udefs = take_custom_utilities css in
  let udefs = udefs @ own_udefs in
  let css, defs = take_custom_variants css in
  let defs = defs @ extra_defs in
  (* A project declaration wins over the built-in of the same name. Any other
     built-in the CSS names has its template derived from tw's own output for a
     probe utility, so [@variant sm] is not silently dropped along with the
     declarations it guards. *)
  let derived =
    variant_names_in css
    |> List.sort_uniq String.compare
    |> List.filter (fun n -> not (List.mem_assoc n defs))
    |> List.filter_map (fun n ->
        Option.map (fun t -> (n, t)) (builtin_variant_template ~theme n))
  in
  let defs = defs @ builtin_variants @ derived in
  (* A declared utility's body may [@apply] another one, so keep expanding until
     nothing is left (bounded, in case two reference each other). *)
  let rec expand depth css =
    let out = expand_apply ~theme ~defs ~udefs css in
    if depth >= 4 || String.equal out css then out else expand (depth + 1) out
  in
  drop_directives
    (resolve_theme_fn ~theme
       (expand_alpha_fn
          (resolve_dashed_theme_fn ~theme
             (expand_spacing_fn ~theme
                (expand_variants ~depth:0 defs (expand 0 css))))))

(* Preload every transitively-referenced stylesheet, keyed by the URL resolved
   against its importer, which is what the inliner looks up. Mirrors cascade's
   own filesystem loader. A package import has no file and stays unresolved on
   purpose, so the splice below can find it. *)
let preload_imports ~transform ~base_url stylesheet =
  let imports = Hashtbl.create 16 in
  let rec scan_under base sheet =
    let loader = Css.Context.loader ~base_url:base () in
    Css.fold (scan_stmt loader) () sheet
  and scan_stmt loader () stmt =
    match Css.as_import stmt with
    | Some ir when not (is_tailwind_import ir.url) ->
        handle loader (Css.decode_import_url ir.url)
    | _ -> ()
  and handle loader url =
    match Css.Context.resolve_url loader url with
    | Error _ -> ()
    | Ok resolved -> (
        if not (Hashtbl.mem imports resolved) then
          match read_file resolved with
          | exception Sys_error _ -> ()
          | content -> (
              let content = transform content in
              Hashtbl.add imports resolved content;
              match Css.of_string content with
              | Ok inner -> scan_under resolved inner.Css.stylesheet
              | Error _ -> ()))
  in
  scan_under base_url stylesheet;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) imports []

(* Compile the project's CSS entrypoint instead of only reading its [@theme].
   Tailwind treats that file as the stylesheet: its own rules and its relative
   [@import]s are part of the output, and [@import "tailwindcss"] is where the
   generated sheet goes. Reading it for tokens alone silently dropped every rule
   the project wrote. *)
(* A [@property] the author's [@apply] hoisted and one the generated sheet sets
   name the same custom property, and a second [@property] for a name is
   redundant. Keep the first, and put them all at the end of the document, where
   Tailwind emits them: spliced at the [@import] instead, they sit ahead of the
   author's own rules and shift every one of them. *)
let equal_layer = Css.Stylesheet.equal_layer_name

(* An empty [@layer name] block says only that the name has a slot, and with
   nothing in it the fold below reads it as no occurrence at all and leaves it
   standing. The generated sheet writes an empty utilities layer whenever every
   utility in the sheet is a declared one, and [hoist_layer_blocks] fills a slot
   from the first block of its name, so an empty block in front of the real one
   hides the rules. Write what it means, a slot, and both passes then see the
   block that has them. Tailwind emits the same [@layer name;] for it. *)
let declare_empty_layers stmts =
  let has_content name =
    List.exists
      (fun st ->
        match Css.as_layer st with
        | Some (Some n, _ :: _) -> equal_layer n name
        | _ -> false)
      stmts
  in
  List.map
    (fun st ->
      match Css.as_layer st with
      | Some (Some n, []) when has_content n -> Css.layer_decl [ n ]
      | _ -> st)
    stmts

(* A named layer appears once in Tailwind's output, so fold every repeat of a
   name into the first. The generated sheet and the [@layer properties] block
   each applied utility hoists say the same thing, and the fold takes no hook to
   re-optimize the body it joins the way the [merge_consecutive_*] passes do, so
   drop what the joined body now holds twice. *)
let merge_named_layers stmts =
  let stmts = declare_empty_layers stmts in
  let merged = Css.Optimize.merge_named_layers_by_name stmts in
  if List.compare_lengths merged stmts = 0 then stmts
  else
    List.map
      (fun stmt ->
        match Css.as_layer stmt with
        | Some (Some name, inner) -> Css.layer ~name (dedup_statements inner)
        | _ -> stmt)
      merged

(* Every [@apply] and every declared utility hoists a [@layer properties] block
   of its own, holding the initial values of the variables its utilities set,
   and the generated sheet hoists one too. Folded into the single layer the
   sheet declares, they line up as a run of [@supports] blocks over one
   condition and one universal selector, each repeating variables the others
   already declare. Tailwind writes one block. Joining the run leaves every
   variable on the value the last block in it gave it, which is the value it
   held before. *)
let join_fallback_rules stmts =
  merge_same_selector stmts
  |> List.map (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, decls, []) ->
          Css.rule ~selector (Css.Optimize.deduplicate_declarations decls)
      | _ -> stmt)

let collapse_property_fallbacks stmts =
  List.map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when equal_layer name [ "properties" ] ->
          Css.layer ~name
            (Css.Optimize.merge_consecutive_supports
               ~optimize_merged_block:join_fallback_rules inner)
      | _ -> stmt)
    stmts

(* A token declared twice keeps the place its first declaration holds, so the
   theme's own order survives, and takes the value its last one gives, which is
   the value the cascade settled on. *)
let join_theme_declarations decls =
  let last = Hashtbl.create 16 in
  List.iter
    (fun d ->
      Option.iter
        (fun name -> Hashtbl.replace last name d)
        (Css.custom_declaration_name d))
    decls;
  let placed = Hashtbl.create 16 in
  List.filter_map
    (fun d ->
      match Css.custom_declaration_name d with
      | None -> Some d
      | Some name when Hashtbl.mem placed name -> None
      | Some name ->
          Hashtbl.add placed name ();
          Some (Hashtbl.find last name))
    decls

(* Every [@apply] hoists a [:root, :host] rule of its own, declaring the tokens
   its utilities read, and the generated sheet declares the theme too. Folded
   into the one [@layer theme], they line up as a run of rules Tailwind writes
   as one; under [theme(static)] every token in the later ones is already in the
   first. *)
let join_theme_rules stmts =
  List.map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when equal_layer name [ "theme" ] ->
          Css.layer ~name
            (merge_same_selector inner
            |> List.map (fun stmt ->
                match Css.as_rule stmt with
                | Some (selector, decls, []) ->
                    Css.rule ~selector (join_theme_declarations decls)
                | _ -> stmt))
      | _ -> stmt)
    stmts

let layer_block_name stmt =
  match Css.layer_block_name stmt with Some [] | None -> None | name -> name

(* A layer is declared by the first mention of its name, so a later declaration
   of the same slot says nothing. Expanding a layer list writes a slot where its
   name sat and the sheet carries a standalone declaration of that same slot
   further on, so track what has been declared and keep only the first. *)
let slot_registry () =
  let declared = Hashtbl.create 8 in
  let key n = Css.Stylesheet.string_of_layer_name n in
  let declare n = Hashtbl.replace declared (key n) () in
  let is_declared n = Hashtbl.mem declared (key n) in
  (declare, is_declared)

(* [@layer components;] on its own declares the layer's slot; the block that
   fills it can come much later, from an imported file. Tailwind emits the block
   in the slot, so move it there. The declared order already makes this
   cascade-neutral; it is the document shape that differs. *)
let movable_layer_slots stmts =
  let is_block_of n st =
    match layer_block_name st with Some m -> equal_layer m n | None -> false
  in
  (* Two layer names never share their printed text, so it keys them. *)
  let by_text a b =
    String.compare
      (Css.Stylesheet.string_of_layer_name a)
      (Css.Stylesheet.string_of_layer_name b)
  in
  List.filter_map Css.layer_statement_name_list stmts
  |> List.concat |> List.sort_uniq by_text
  |> List.filter (fun n -> List.exists (is_block_of n) stmts)

let expand_layer_list ~movable ~emitted ~declare ~is_declared ~block_for names =
  List.filter_map
    (fun n ->
      let repeat = is_declared n in
      declare n;
      if List.exists (equal_layer n) movable then
        if Hashtbl.mem emitted n then None
        else begin
          Hashtbl.add emitted n ();
          block_for n
        end
      else if repeat then None
      else Some (Css.layer_decl [ n ]))
    names

let fresh_layer_decl ~declare ~is_declared names =
  match List.filter (fun n -> not (is_declared n)) names with
  | [] -> []
  | fresh ->
      List.iter declare fresh;
      [ Css.layer_decl fresh ]

(* [@layer properties] carries the initial values of the variables the utilities
   set, so it has to be declared ahead of the layers that read them, which is
   where Tailwind emits it. The generated sheet places its own; one an [@apply]
   hoisted arrives at the end of the entrypoint text instead, and would order
   after every other layer. [Place_routed] does the same for the
   declared-utility path. *)
let lead_properties_layer stmts =
  let is_properties stmt =
    match Css.layer_block_name stmt with
    | Some name -> Css.Stylesheet.equal_layer_name name [ "properties" ]
    | None -> false
  in
  match List.partition is_properties stmts with
  | [], _ -> stmts
  | lead, rest -> lead @ rest

let hoist_layer_blocks stmts =
  let movable = movable_layer_slots stmts in
  if movable = [] then stmts
  else
    let is_block_of n st =
      match layer_block_name st with Some m -> equal_layer m n | None -> false
    in
    let block_for n = List.find_opt (is_block_of n) stmts in
    let emitted = Hashtbl.create 8 in
    let declare, is_declared = slot_registry () in
    List.concat_map
      (fun stmt ->
        match Css.layer_statement_name_list stmt with
        | Some names
          when List.exists (fun n -> List.exists (equal_layer n) movable) names
          ->
            expand_layer_list ~movable ~emitted ~declare ~is_declared ~block_for
              names
        | Some names -> fresh_layer_decl ~declare ~is_declared names
        | None -> (
            match layer_block_name stmt with
            | Some n when List.exists (equal_layer n) movable -> []
            | Some n ->
                declare n;
                [ stmt ]
            | None -> [ stmt ]))
      stmts

(* A token the project declared in an [@theme inline] block has no declaration
   of its own — the value goes into the utility instead. Two exceptions: one
   that refers to itself, where inlining would leave the reference dangling, and
   one some other rule still reads. That has to be judged over the whole
   document: the typography plugin's [.prose code] reads [--font-mono] from the
   components layer. *)
let drop_unread_inline_tokens ~theme stmts =
  if Tw.Scheme.(theme.inline_tokens) = [] then stmts
  else
    let reads =
      Css.vars_of_stylesheet (Css.v stmts) |> List.map Css.any_var_name
    in
    let keep decl =
      match Css.custom_declaration_name decl with
      | Some n when String.length n > 2 ->
          let bare = String.sub n 2 (String.length n - 2) in
          (not (Tw.Scheme.is_inline_token theme bare)) || List.mem n reads
      | _ -> true
    in
    let rec go stmts =
      List.map
        (fun stmt ->
          match Css.as_layer stmt with
          | Some (name, inner) -> Css.layer ?name (go inner)
          | None -> (
              match Css.as_rule stmt with
              | Some (sel, decls, nested) ->
                  Css.rule ~selector:sel ~nested (List.filter keep decls)
              | None -> stmt))
        stmts
    in
    go stmts

let collect_properties_at_end stmts =
  let seen = Hashtbl.create 64 in
  let keep, props =
    List.partition_map
      (fun stmt ->
        match Css.as_property stmt with
        | None ->
            if Css.as_keyframes stmt <> None then Right stmt
            else Left (Some stmt)
        | Some (Css.Property_info { name; _ }) ->
            if Hashtbl.mem seen name then Left None
            else begin
              Hashtbl.add seen name ();
              Right stmt
            end)
      stmts
  in
  let at_end, keyframes =
    List.partition (fun st -> Css.as_keyframes st = None) props
  in
  List.filter_map Fun.id keep @ at_end @ keyframes

(* Tailwind runs authored CSS through Lightning CSS, which writes a legacy value
   before a dynamic [color-mix()] and keeps the authored value behind a feature
   query. Utility output already carries that pair; this pass is only applied to
   the parsed entrypoint before the generated sheet is spliced in.

   [flatten_nesting] has made every element rule flat by the time this runs, so
   splitting one declaration into a rule/query pair preserves its exact place
   among the author's declarations. *)
let fallback_declaration ~theme declaration =
  let value = Css.declaration_value ~minify:true declaration in
  match Css.parse_color value with
  | None -> None
  | Some color -> (
      match Tw.Color.pre_color_mix_fallback theme color with
      | None -> None
      | Some fallback ->
          let fallback_value =
            Css.color fallback |> Css.Declaration.normalize
            |> Css.declaration_value ~minify:true
          in
          let layer = Css.Declaration.custom_declaration_layer declaration in
          Css.Declaration.parse_declaration ?layer
            (Css.declaration_name declaration)
            fallback_value
          |> Option.map (fun fallback ->
              if Css.declaration_is_important declaration then
                Css.important fallback
              else fallback))

let lower_color_mix_rule ~theme selector declarations =
  let rec loop pending emitted = function
    | [] ->
        let emitted =
          match pending with
          | [] -> emitted
          | _ -> Css.rule ~selector (List.rev pending) :: emitted
        in
        List.rev emitted
    | declaration :: rest -> (
        match fallback_declaration ~theme declaration with
        | None -> loop (declaration :: pending) emitted rest
        | Some fallback ->
            let base_declarations =
              match pending with
              | previous :: _
                when Css.Declaration.equal_declaration previous fallback ->
                  List.rev pending
              | _ -> List.rev (fallback :: pending)
            in
            let base = Css.rule ~selector base_declarations in
            let enhanced = Css.rule ~selector [ declaration ] in
            let supports =
              Css.supports ~condition:Tw.Color.color_mix_supports_condition
                [ enhanced ]
            in
            loop [] (supports :: base :: emitted) rest)
  in
  loop [] [] declarations

let authored_color_mix_fallbacks ~theme stmts =
  let rec lower_block stmts =
    List.concat_map
      (fun statement ->
        match Css.as_supports statement with
        | Some (condition, _)
          when Css.Supports.equal condition
                 Tw.Color.color_mix_supports_condition ->
            [ statement ]
        | _ -> (
            let statement =
              Css.Stylesheet.map_statement_children lower_block statement
            in
            match Css.as_rule statement with
            | Some (selector, declarations, []) ->
                lower_color_mix_rule ~theme selector declarations
            | _ -> [ statement ]))
      stmts
  in
  lower_block stmts

(* A theme token the author's own CSS reads has to be declared, the way one a
   utility reads is. [padding: --spacing(4)] expands to [calc(var(--spacing) *
   4)], and a sheet declaring no [--spacing] resolves it to nothing - the rule
   is right and the page renders unstyled, which is the shape every author-CSS
   defect here has taken.

   Only the author's own statements are scanned. The generated sheet already
   declares what it reads, and re-deriving it from the whole result is what
   over-emits: a [--default-*] token nothing set would arrive in the theme layer
   and move a fallback the utilities layer spells. *)
let author_theme_tokens ~theme stmts =
  Css.vars_of_rules stmts
  |> List.filter_map (fun (Css.V var) ->
      (* [var_name] is the bare name, without the [--] a declaration carries. *)
      let bare = Cascade.Css.var_name var in
      (* A reference token is declared somewhere else, by definition. *)
      if Tw.Scheme.is_reference_token theme bare then None
      else
        Option.map
          (fun value -> ("--" ^ bare, value))
          (theme_token_value theme bare))
  |> List.sort_uniq compare

(* Declared anywhere in [stmts], at any depth. *)
let declared_custom_properties stmts =
  Cascade.Css.Stylesheet.fold_declarations
    (fun names decls ->
      List.filter_map Cascade.Css.custom_declaration_name decls @ names)
    [] stmts

let declare_author_theme_tokens tokens stmts =
  let declared = declared_custom_properties stmts in
  match List.filter (fun (name, _) -> not (List.mem name declared)) tokens with
  | [] -> stmts
  | missing ->
      (* An [@layer theme] block, so [merge_named_layers] folds it into the
         generated sheet's own rather than leaving a second one behind. *)
      let decls =
        List.map
          (fun (name, value) -> Css.custom_property ~layer:"theme" name value)
          missing
      in
      let selector =
        Css.Selector.List [ Css.Selector.Root; Css.Selector.host () ]
      in
      Css.layer ~name:[ "theme" ] [ Css.rule ~selector decls ] :: stmts

(* The author's own top-level [@property] rules get the fallback block the
   generated sheet writes for its own. Appended, so [merge_named_layers] folds
   it into the sheet's [@layer properties] after the utilities' variables. *)
let add_author_property_fallbacks author stmts =
  match List.filter (fun s -> Option.is_some (Css.as_property s)) author with
  | [] -> stmts
  | rules -> stmts @ Tw.property_fallbacks rules

(* [@tailwind utilities] is all [tailwindcss/utilities.css] holds, so it is read
   as that import, where the directive stands. *)
let tailwind_utilities_as_import css =
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let asks_for_utilities (statement : Index.statement) =
    match split_top_level ' ' (String.trim statement.prelude) with
    | "utilities" :: _ -> true
    | _ -> false
  in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_statement index ~name:"@tailwind" i with
      | Some statement when asks_for_utilities statement ->
          Buffer.add_string buf "@import \"tailwindcss/utilities\";";
          go statement.next
      | _ ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

let tailwind_parts css =
  match
    Css.of_string
      (strip_tailwind_import_options (tailwind_utilities_as_import css))
  with
  | Error _ -> []
  | Ok parse ->
      List.filter_map
        (fun stmt ->
          Option.bind (Css.as_import stmt)
            (fun (ir : Css.Stylesheet.import_rule) ->
              tailwind_import_part ir.url))
        (Css.statements parse.Css.stylesheet)

let imports_preflight css =
  match tailwind_parts css with
  | [] -> true
  | parts ->
      List.exists
        (function
          | Whole | Preflight_part -> true
          | Theme_part | Utilities_part -> false)
        parts

(* The statements of [generated] one import asks for, in the layer its [layer()]
   names or unlayered without one. The utilities bring what their rules need
   beside them: the [@layer properties] fallbacks and the [@property] and
   [@keyframes] rules. No part brings the components layer, which the generated
   sheet only ever declares empty. *)
let generated_part ?(with_base = false) part ~layer generated =
  let stmts = Css.statements generated in
  let layer_named name stmt =
    match Css.as_layer stmt with
    | Some (Some n, inner) when equal_layer n [ name ] -> Some inner
    | _ -> None
  in
  let contents name = List.concat (List.filter_map (layer_named name) stmts) in
  let place = function
    | [] -> []
    | inner -> (
        match layer with
        | None -> inner
        | Some name -> [ Css.layer ~name inner ])
  in
  let is_layer stmt =
    Option.is_some (Css.as_layer stmt)
    || Option.is_some (Css.layer_statement_name_list stmt)
  in
  match part with
  | Whole -> stmts
  | Theme_part -> place (contents "theme")
  | Preflight_part -> place (contents "base")
  | Utilities_part ->
      (* Without a preflight import the generated base layer holds only what a
         plugin adds, the forms reset, and Tailwind writes that in [@layer base]
         whichever part the entrypoint imports. *)
      let base =
        match contents "base" with
        | [] -> []
        | inner ->
            if with_base then [ Css.layer ~name:[ "base" ] inner ] else []
      in
      List.filter (fun s -> Option.is_some (layer_named "properties" s)) stmts
      @ base
      @ place (contents "utilities")
      @ List.filter (fun s -> not (is_layer s)) stmts

(* An imported file under [layer(...)] is expanded before its layer wraps it, so
   what its [@apply]s hoisted - a [@layer theme] or [@layer properties] block,
   an [@property] registration, an [@keyframes] - arrives inside that layer.
   Tailwind keeps them at the top of the sheet, where [merge_named_layers] folds
   the blocks into the generated sheet's own. A class-less rule the author wrote
   in the import stays where it is: the expansion never leaves one bare, it
   wraps its theme rule in [@layer theme]. *)
let lift_hoisted_out_of_layers stmts =
  let hoisted stmt =
    (match Css.as_layer stmt with
      | Some (Some name, _) ->
          equal_layer name [ "theme" ] || equal_layer name [ "properties" ]
      | _ -> false)
    || Css.as_property stmt <> None
    || Css.as_keyframes stmt <> None
  in
  List.concat_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when List.exists hoisted inner ->
          let lifted, kept = List.partition hoisted inner in
          lifted @ [ Css.layer ~name kept ]
      | _ -> [ stmt ])
    stmts

let splice_into_entrypoint ~theme ~path generated =
  match read_file path with
  | exception Sys_error _ -> generated
  | raw -> (
      let raw = tailwind_utilities_as_import raw in
      let with_base = not (imports_preflight raw) in
      let css =
        apply_variants ~theme
          (hoist_theme_keyframes (strip_tailwind_import_options raw))
      in
      match Css.of_string css with
      | Error _ -> generated
      | Ok p ->
          (* An imported file uses the same Tailwind syntax, and its [@variant]s
             may be declared in the entrypoint, so it gets the same treatment
             with those declarations in scope. *)
          let _, entry_defs =
            take_custom_variants (strip_tailwind_import_options raw)
          in
          let transform body =
            apply_variants ~extra_defs:entry_defs ~theme
              (strip_tailwind_import_options
                 (tailwind_utilities_as_import body))
          in
          let imports =
            preload_imports ~transform ~base_url:path p.Css.stylesheet
          in
          let loader = Css.Context.loader ~base_url:path ~imports () in
          (* Tailwind flattens the author's nesting, including what the expanded
             variants introduce, so match that shape. *)
          let inlined =
            Css.flatten_nesting (Css.inline_imports loader p.Css.stylesheet)
          in
          Css.statements inlined
          |> authored_color_mix_fallbacks ~theme
          |> List.concat_map (fun stmt ->
              match stmt with
              | Cascade.Stylesheet.Import { url; layer; _ } as s -> (
                  match tailwind_import_part url with
                  | Some part -> generated_part ~with_base part ~layer generated
                  | None -> [ s ])
              | s -> [ s ])
          (* A stylesheet that only references the package declares no token,
             its own [@theme]'s included. *)
          |> declare_author_theme_tokens
               (if theme.Tw.Scheme.reference_theme then []
                else author_theme_tokens ~theme (Css.statements inlined))
          |> add_author_property_fallbacks (Css.statements inlined)
          |> lift_hoisted_out_of_layers |> merge_named_layers
          |> collapse_property_fallbacks |> join_theme_rules
          |> hoist_layer_blocks |> lead_properties_layer
          |> drop_unread_inline_tokens ~theme
          |> collect_properties_at_end |> Css.v)

(* Read the entrypoint's [@custom-variant] and [@utility] declarations. A
   project can redefine a built-in variant here (e.g. class-based [dark]) or
   declare a utility of its own; both govern the whole utility set, not only the
   author's own CSS. *)
let entry_defs take = function
  | None -> []
  | Some path -> (
      match read_file path with
      | exception Sys_error _ -> []
      | raw -> snd (take (strip_tailwind_import_options raw)))

(* The theme a project's entrypoint asks for: its [\@theme] overrides, whether
   it imported [theme(static)], and the prefix it named. One function so the CLI
   and anything measuring the CLI build the same theme from the same file rather
   than two that drift. *)
let theme_of_css css =
  let overrides, inline = theme_overrides_of_css css in
  let base =
    if imports_static_theme css then
      { Tw.Scheme.default with static_theme = true }
    else Tw.Scheme.default
  in
  let base =
    {
      base with
      prefix = import_prefix css;
      important = imports_important css;
      reference_theme = references_tailwind css && tailwind_parts css = [];
    }
  in
  let blocks = theme_blocks css in
  let static = theme_tokens_with "static" blocks in
  let reference = theme_tokens_with "reference" blocks in
  Tw.Scheme.with_overrides ~inline ~reference ~static base overrides

let entry_variant_defs = entry_defs take_custom_variants
let entry_utility_defs = entry_defs take_custom_utilities

(* The escaped class with its declarations under the [@variant]s that wrap
   it. *)
let wrapped_block cls variants body =
  let class_sel = Css.Selector.to_string (Css.Selector.Class cls) in
  let wrapped =
    List.fold_right
      (fun v acc -> String.concat "" [ "@variant "; v; "{"; acc; "}" ])
      variants body
  in
  String.concat "" [ class_sel; "{"; wrapped; "}" ]

(* A candidate the project's own declarations govern: it carries a declared
   variant, it is a declared utility, or it reads as one of a declared
   functional utility's candidates. *)
let is_custom_routed ~defs ~udefs cls =
  let variants, bare = split_declared_variants defs cls in
  let segs = variant_segments bare in
  let name = List.nth segs (List.length segs - 1) in
  variants <> [] || List.mem_assoc name udefs
  || functional_candidates udefs name <> []

(* Candidates the built-in generator cannot produce: a variant the project
   redefined via [@custom-variant] (e.g. a class-based [dark:]), which
   [Tw.of_string] only knows in its built-in [@media (prefers-color-scheme:
   dark)] form, and a class the project declared with [@utility], which
   [Tw.of_string] does not know at all. Both go through the same expansion the
   author CSS uses: the declarations land under the escaped class, wrapped in
   the declared variants, and cascade flattens the nesting into the project's
   selector. *)
(* The class a routed rule belongs to: its selector's first class, which is the
   declared utility itself for both [.line-y] and [.line-y:before]. *)
let rec first_class_of_statement stmt =
  match Css.as_rule stmt with
  | Some (selector, _, _) -> Css.Selector.first_class selector
  | None -> (
      match Css.as_media stmt with
      | Some (_, inner) -> List.find_map first_class_of_statement inner
      | None -> (
          match Css.as_supports stmt with
          | Some (_, inner) -> List.find_map first_class_of_statement inner
          | None -> None))

(* Where a declared utility sorts: the slot of the property it writes first. *)
let rec slot_of_statement stmt =
  match Css.as_rule stmt with
  | Some (_, d :: _, _) ->
      Tw.Utility.order_of_property (Css.Declaration.property_key d)
  | Some (_, [], _) -> None
  | None -> (
      match Css.as_media stmt with
      | Some (_, inner) -> List.find_map slot_of_statement inner
      | None -> (
          match Css.as_supports stmt with
          | Some (_, inner) -> List.find_map slot_of_statement inner
          | None -> None))

let routed_template ~theme derived name =
  match Hashtbl.find_opt derived name with
  | Some template -> template
  | None ->
      let template = builtin_variant_template ~theme name in
      Hashtbl.add derived name template;
      template

let record_routed_order ~theme own_order cls name =
  match Tw.Utility.base_of_class theme name with
  | Ok base -> Hashtbl.replace own_order cls (Tw.Utility.order base)
  | Error _ -> ()

let routed_block ~theme ~defs ~udefs ~hoisted ~seen ~derived ~own_order cls =
  let variants, bare = split_declared_variants defs cls in
  (* [bare] still carries the built-in prefixes; the utility itself is its last
     segment. *)
  let segments = variant_segments bare in
  let last = List.length segments - 1 in
  let name = List.nth segments last in
  let builtin = List.filteri (fun index _ -> index < last) segments in
  (* Every body declared for the name, in the order they were written: Tailwind
     registers each [@utility] of a name and applies them all, so a second
     declaration adds to the first rather than replacing it. *)
  let declared =
    match List.filter (fun (n, _) -> n = name) udefs with
    | _ :: _ as declared -> Some (String.concat "" (List.map snd declared))
    | [] -> functional_utility_body ~theme ~udefs name
  in
  match declared with
  | Some body ->
      (* A declared utility means nothing to [Tw.of_string], so every prefix has
         to become a [@variant], the built-in ones included. *)
      if
        List.for_all
          (fun variant ->
            Option.is_some (routed_template ~theme derived variant))
          builtin
      then Some (wrapped_block cls (variants @ builtin) body)
      else None
  | None when variants = [] -> None
  | None ->
      let body, top = nested_utilities ~theme [ bare ] in
      add_once hoisted seen top;
      if body = "" then None
      else begin
        record_routed_order ~theme own_order cls name;
        Some (wrapped_block cls variants body)
      end

let collect_routed_templates ~theme derived udefs =
  List.iter
    (fun (_, body) ->
      List.iter
        (fun name -> ignore (routed_template ~theme derived name))
        (variant_names_in body))
    udefs

let routed_variant_defs defs derived =
  defs
  @ Hashtbl.fold
      (fun name template acc ->
        match template with Some body -> (name, body) :: acc | None -> acc)
      derived []

(* A parsed routed statement keeps the candidate that produced its block.
   Selector recovery remains the fallback for independently hoisted statements,
   but cannot own a compound selector whose leading [:where(...)] hides the
   candidate class from [first_class_of_statement]. *)
let routed_owner owner stmt =
  match owner with Some _ -> owner | None -> first_class_of_statement stmt

let group_routed_rules ~own_order rules =
  let group = Hashtbl.create 8 in
  let order_of = Hashtbl.create 8 in
  let classless = ref [] in
  List.iter
    (fun (owner, stmt) ->
      match routed_owner owner stmt with
      | None -> classless := stmt :: !classless
      | Some cls -> (
          let prev =
            Stdlib.Option.value ~default:[] (Hashtbl.find_opt group cls)
          in
          Hashtbl.replace group cls (prev @ [ stmt ]);
          if (not (Hashtbl.mem own_order cls)) && not (Hashtbl.mem order_of cls)
          then
            match slot_of_statement stmt with
            | Some order -> Hashtbl.add order_of cls order
            | None -> ()))
    rules;
  (group, order_of, List.rev !classless)

let routed_slot ~own_order ~order_of cls =
  match Hashtbl.find_opt own_order cls with
  | Some order -> order
  | None ->
      Stdlib.Option.value ~default:(max_int, max_int)
        (Hashtbl.find_opt order_of cls)

(* Tailwind counts every declaration in a utility's AST, including ones in
   nested rules and at-rules. [Css.fold] follows every kind of nested block, so
   this does not need a brittle list of the block at-rules cascade knows. *)
let routed_declaration_count stmts =
  Css.fold
    (fun count stmt ->
      match Css.as_rule stmt with
      | Some (_, declarations, _) -> count + List.length declarations
      | None -> (
          match Css.as_declarations stmt with
          | Some declarations -> count + List.length declarations
          | None -> count))
    0 (Css.v stmts)

let compare_routed_entries ~own_order ~order_of (c1, stmts1) (c2, stmts2) =
  let p1, s1 = routed_slot ~own_order ~order_of c1 in
  let p2, s2 = routed_slot ~own_order ~order_of c2 in
  let priority = Int.compare p1 p2 in
  let suborder = if priority <> 0 then priority else Int.compare s1 s2 in
  if suborder <> 0 then suborder
  else
    let count =
      Int.compare
        (routed_declaration_count stmts2)
        (routed_declaration_count stmts1)
    in
    if count <> 0 then count else String.compare c1 c2

(* Within one declared utility, the rules it writes outright come before the
   ones a variant wrapped in an at-rule, the order the generator gives a
   built-in utility and its own media queries. *)
let unwrapped_first stmts =
  let plain, wrapped =
    List.partition (fun stmt -> Option.is_some (Css.as_rule stmt)) stmts
  in
  plain @ wrapped

let ordered_routed_entries ~own_order ~order_of group =
  Hashtbl.fold
    (fun cls stmts acc -> (cls, unwrapped_first stmts) :: acc)
    group []
  |> List.sort (compare_routed_entries ~own_order ~order_of)

let routed_statements ~block_count ~own_order stmts =
  (* [@layer properties], [@property] and [@keyframes] sit beside the utilities
     layer, not in it: nested, the first would become [utilities.properties],
     and the [@keyframes] an applied animation utility names would go into the
     classless bucket and be wrapped in a utilities layer of its own, which is
     not where Tailwind puts it. *)
  let hoisted, rules =
    List.partition
      (fun (_, stmt) ->
        Css.as_layer stmt <> None
        || Css.as_property stmt <> None
        || Css.as_keyframes stmt <> None)
      stmts
  in
  let group, order_of, classless = group_routed_rules ~own_order rules in
  (* A declared utility whose first property tw has no slot for still belongs
     among the utilities, at the end, so it goes over with the rest: sorted with
     them, and read by the theme layer for the tokens it names. A statement
     naming no class at all has nothing to sort by, and gets a utilities layer
     of its own after them. *)
  let ordered =
    ordered_routed_entries ~own_order ~order_of group
    |> List.map (fun (cls, stmts) ->
        (cls, routed_slot ~own_order ~order_of cls, stmts))
  in
  let unplaced =
    if classless = [] then [] else [ Css.layer ~name:[ "utilities" ] classless ]
  in
  (block_count, ordered, unplaced @ dedup_statements (List.map snd hoisted))

(* Flattening is what turns the wrappers a variant builds around a [@utility]
   body into selectors: [.focus\:line-y { &:focus { ... } }] has to become
   [.focus\:line-y:focus]. The body's own nesting is not a wrapper, and Tailwind
   keeps it - [.line-y { padding: 5px; &::before { color: red } }] is one block
   in its output. A rule already carrying declarations of its own is the utility
   rather than a wrapper, so it goes through as written; flattening it would
   split the utility into a rule per selector, each sorting by the property it
   writes. *)
let flattened_statement stmt =
  match Css.as_rule stmt with
  | Some (_, _ :: _, nested)
    when List.for_all (fun st -> Css.as_rule st <> None) nested ->
      [ stmt ]
  | _ -> Css.statements (Css.flatten_nesting (Css.v [ stmt ]))

(* The statements of one generated block, or none when it will not parse at all.
   A malformed body has to cost its own class and nothing else: read as one
   assembled sheet, an unclosed brace nests every block written after it inside
   the broken one, and a parse the recovery cannot save loses the lot. *)
let parse_routed_block css =
  match Css.of_string css with
  | Error _ -> None
  | Ok parsed ->
      Some
        (Css.statements parsed.Css.stylesheet
        |> List.concat_map flattened_statement)

let parse_routed_blocks ~own_order ~hoisted blocks =
  let parsed =
    List.filter_map
      (fun (cls, block) ->
        Option.map
          (List.map (fun stmt -> (Some cls, stmt)))
          (parse_routed_block block))
      blocks
  in
  let hoisted =
    Option.value ~default:[] (parse_routed_block hoisted)
    |> List.map (fun stmt -> (None, stmt))
  in
  List.concat parsed @ hoisted
  |> routed_statements ~block_count:(List.length parsed) ~own_order

(* A declared utility hoists the same [@layer properties] fallback block the
   generated sheet emits, and that block belongs where the sheet puts its own:
   ahead of the theme, not after the utilities it initialises. The rest of what
   it hoists follows the sheet. *)
let place_routed stmts sheet =
  let is_properties_layer stmt =
    match Css.layer_block_name stmt with
    | Some name -> Css.Stylesheet.equal_layer_name name [ "properties" ]
    | None -> false
  in
  match List.partition is_properties_layer stmts with
  | [], [] -> sheet
  | lead, trail -> Css.v (lead @ Css.statements sheet @ trail)

let custom_routed_utilities ~theme ~defs ~udefs candidates =
  let hoisted = Buffer.create 0 in
  let seen = Hashtbl.create 64 in
  let derived = Hashtbl.create 8 in
  let own_order = Hashtbl.create 8 in
  let blocks =
    List.filter_map
      (fun cls ->
        Option.map
          (fun block -> (cls, block))
          (routed_block ~theme ~defs ~udefs ~hoisted ~seen ~derived ~own_order
             cls))
      candidates
  in
  match blocks with
  | [] -> (0, [], [])
  | _ ->
      collect_routed_templates ~theme derived udefs;
      let extra_defs = routed_variant_defs defs derived in
      (* A [@utility] body is author CSS: it may hold [@apply], [@variant] and
         the [--spacing()]/[theme()] shorthands. Each block is expanded and read
         on its own so one unparseable body cannot take the others down. *)
      let expand = apply_variants ~extra_defs ~udefs ~theme in
      parse_routed_blocks ~own_order
        ~hoisted:(expand (Buffer.contents hoisted))
        (List.map (fun (cls, block) -> (cls, expand block)) blocks)
