module Css = Cascade.Css

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* [@import "tailwindcss" theme(static)] asks for the whole theme, not only the
   variables a utility used. The option is stripped before parsing, so it is
   read from the raw text. *)
let imports_static_theme css =
  let len = String.length css in
  let rec go i =
    i + 13 <= len && (String.sub css i 13 = "theme(static)" || go (i + 1))
  in
  go 0

(* Where a token begins and ends in the source it was read from. *)
let start (t : Cascade.Token.t) = t.loc.Cascade.Loc.start_pos
let stop (t : Cascade.Token.t) = t.loc.Cascade.Loc.end_pos

(* Every token of [css], in order, without the terminating [Eof]. *)
let tokens css =
  let lexer = Cascade.Lexer.of_string css in
  let rec go acc =
    let token = Cascade.Lexer.next lexer in
    match token.Cascade.Token.kind with
    | Cascade.Token.Eof -> Array.of_list (List.rev acc)
    | _ -> go (token :: acc)
  in
  go []

(* The first token from [i] on that is not whitespace, or the end of [toks]. *)
let rec after_whitespace (toks : Cascade.Token.t array) i =
  if
    i < Array.length toks
    && Cascade.Token.equal_kind toks.(i).kind Cascade.Token.Whitespace
  then after_whitespace toks (i + 1)
  else i

(* Index of the [;] closing the declaration whose value starts at [i], or of the
   [}] that ends the block first. A bracket group inside the value is stepped
   over whole, so a [;] within one does not end the declaration. *)
let rec declaration_end (toks : Cascade.Token.t array) i depth =
  if i >= Array.length toks then i
  else
    match toks.(i).kind with
    | Cascade.Token.Semicolon when depth = 0 -> i
    | Cascade.Token.Open _ | Cascade.Token.Function _ ->
        declaration_end toks (i + 1) (depth + 1)
    | Cascade.Token.Close _ when depth = 0 -> i
    | Cascade.Token.Close _ -> declaration_end toks (i + 1) (depth - 1)
    | _ -> declaration_end toks (i + 1) depth

(* [--<ns>-*: initial] takes a whole [@theme] namespace out of the theme, and
   [--<ns>-*] is not a custom-property name: the [*] ends the ident, so "consume
   a declaration" (CSS Syntax 3 sec. 5.5.15) finds no [:] where it wants one and
   the block loses the reset. The token stream still has it, so it is read from
   there, keyed by where it starts so it can go back among the declarations in
   the order the block wrote them. *)
let namespace_resets body =
  let toks = tokens body in
  let text from upto = String.trim (String.sub body from (upto - from)) in
  (* The reset an ident at [i] opens, and the index to carry on from. *)
  let reset i name acc =
    let star = after_whitespace toks (i + 1) in
    let colon = after_whitespace toks (star + 1) in
    if
      colon < Array.length toks
      && Cascade.Token.equal_kind toks.(star).kind (Cascade.Token.Delim "*")
      && Cascade.Token.equal_kind toks.(colon).kind Cascade.Token.Colon
    then
      let last = declaration_end toks (colon + 1) 0 in
      let upto =
        if last < Array.length toks then start toks.(last)
        else String.length body
      in
      let namespace = String.sub name 2 (String.length name - 2) ^ "*" in
      ( last + 1,
        (start toks.(i), (namespace, text (stop toks.(colon)) upto)) :: acc )
    else (i + 1, acc)
  in
  (* Only a declaration of the block itself is a reset, so a [*] under a nested
     rule or inside a bracket group is stepped past. *)
  let rec go i depth acc =
    if i >= Array.length toks then List.rev acc
    else
      match toks.(i).kind with
      | Cascade.Token.Open _ | Cascade.Token.Function _ ->
          go (i + 1) (depth + 1) acc
      | Cascade.Token.Close _ -> go (i + 1) (max 0 (depth - 1)) acc
      | Cascade.Token.Ident name
        when depth = 0
             && String.length name >= 2
             && String.equal (String.sub name 0 2) "--" ->
          let i, acc = reset i name acc in
          go i depth acc
      | _ -> go (i + 1) depth acc
  in
  go 0 0 []

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
                Cascade.Parser.to_string_custom value ) )
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
let theme_overrides_of_css css =
  match Css.of_string css with
  | Error _ -> ([], [])
  | Ok parse ->
      let block (prelude, body) =
        let names = theme_tokens body in
        let inline =
          List.mem "inline" (String.split_on_char ' ' (String.trim prelude))
        in
        (names, if inline then List.map fst names else [])
      in
      let blocks =
        Css.statements parse.Css.stylesheet
        |> List.filter_map theme_block
        |> List.map block
      in
      (List.concat_map fst blocks, List.concat_map snd blocks)

(* [@import "tailwindcss"] (and its subpath forms) is the package entry, not a
   file on disk: it marks where the generated theme/base/utilities belong. *)
let is_tailwind_import url =
  let u = Css.decode_import_url url in
  u = "tailwindcss" || String.starts_with ~prefix:"tailwindcss/" u

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
   end, read off cascade's tokenizer.

   The Tailwind at-rules below are not CSS, so they have to be found in the text
   before a parser sees it -- but counting braces over the raw bytes counts the
   ones inside a string, a comment or an escape too. The block then ends in the
   wrong place and everything after it is silently dropped. A tokenizer knows
   which brace is a delimiter, so the offsets come from there. *)
module Scan = struct
  open Cascade

  type block = { body : string; next : int }
  (** A [{ ... }] or [( ... )] body, and the offset just past its close. *)

  type header = { prelude : string; brace : int; block : block }
  (** An [@name PRELUDE { ... }] header: what stands between the at-keyword and
      the [{], where that [{] is, and the block it opens. *)

  type statement = { prelude : string; next : int }
  (** A blockless at-rule's prelude and the offset after its semicolon. When a
      closing brace terminates it, [next] points at that brace. *)

  type t = {
    call : (int, string * block) Hashtbl.t;
        (** function-token offset -> the name it calls and its arguments *)
    at : (int, string * header) Hashtbl.t;
        (** at-keyword offset -> its at-name and its header *)
    statement : (int, string * statement) Hashtbl.t;
        (** at-keyword offset -> its at-name and blockless statement *)
  }

  (* For each token opening a bracket, the index of the token closing it, or
     [-1] when the source ends first. Per CSS Syntax 3 sec. 5.4.7 a close
     bracket also ends any group left open inside the one it matches, and a
     stray one that matches nothing is dropped. *)
  let close_of (toks : Token.t array) =
    let close = Array.make (Array.length toks) (-1) in
    let stack = ref [] in
    let rec pop bracket i = function
      | [] -> None
      | (opener, b) :: rest when b = bracket ->
          close.(opener) <- i;
          Some rest
      | _ :: rest -> pop bracket i rest
    in
    Array.iteri
      (fun i (token : Token.t) ->
        match token.kind with
        | Token.Open bracket -> stack := (i, bracket) :: !stack
        | Token.Function _ -> stack := (i, Token.Paren) :: !stack
        | Token.Close bracket -> (
            match pop bracket i !stack with
            | Some rest -> stack := rest
            | None -> ())
        | _ -> ())
      toks;
    close

  (* What the bracket the token at [i] opens holds. An unclosed one runs to the
     end of the source, which is where the parser ends it too. *)
  let block css (toks : Token.t array) close i =
    let from = stop toks.(i) in
    let stop_at, next =
      if close.(i) < 0 then (String.length css, String.length css)
      else (start toks.(close.(i)), stop toks.(close.(i)))
    in
    { body = String.sub css from (stop_at - from); next }

  (* Index of the [{] an at-rule's prelude leads to. A bracket group in the
     prelude is stepped over whole, so a [;] inside one does not end the
     at-rule. *)
  let rec brace_of (toks : Token.t array) close i =
    if i >= Array.length toks then None
    else
      match toks.(i).Token.kind with
      | Token.Open Token.Curly -> Some i
      | Token.Semicolon | Token.Close _ -> None
      | Token.Open _ | Token.Function _ ->
          if close.(i) < 0 then None else brace_of toks close (close.(i) + 1)
      | _ -> brace_of toks close (i + 1)

  (* The semicolon or enclosing close brace that terminates a blockless at-rule.
     Bracket groups in its prelude are stepped over whole. *)
  let rec statement_end (toks : Token.t array) close i source_end =
    if i >= Array.length toks then (source_end, source_end)
    else
      match toks.(i).Token.kind with
      | Token.Semicolon -> (start toks.(i), stop toks.(i))
      | Token.Close Token.Curly ->
          let at = start toks.(i) in
          (at, at)
      | Token.Open _ | Token.Function _ ->
          if close.(i) < 0 then (source_end, source_end)
          else statement_end toks close (close.(i) + 1) source_end
      | _ -> statement_end toks close (i + 1) source_end

  let v css =
    let toks = tokens css in
    let close = close_of toks in
    let t =
      {
        call = Hashtbl.create 16;
        at = Hashtbl.create 16;
        statement = Hashtbl.create 16;
      }
    in
    let header i (token : Token.t) name =
      match brace_of toks close (i + 1) with
      | None -> ()
      | Some j ->
          let from = stop token and upto = start toks.(j) in
          let prelude = String.trim (String.sub css from (upto - from)) in
          let block = block css toks close j in
          Hashtbl.replace t.at (start token)
            (name, { prelude; brace = upto; block })
    in
    let statement i (token : Token.t) name =
      let from = stop token in
      let upto, next = statement_end toks close (i + 1) (String.length css) in
      let prelude = String.sub css from (upto - from) in
      Hashtbl.replace t.statement (start token) (name, { prelude; next })
    in
    Array.iteri
      (fun i (token : Token.t) ->
        match token.kind with
        | Token.Function name ->
            Hashtbl.replace t.call (start token) (name, block css toks close i)
        | Token.At_keyword name ->
            header i token name;
            statement i token name
        | _ -> ())
      toks;
    t

  (* [name( ... )] starting at [i]. *)
  let call t ~name i =
    match Hashtbl.find_opt t.call i with
    | Some (n, block) when n = name -> Some block
    | _ -> None

  (* [@name PRELUDE { ... }] starting at [i]. An at-rule with no block of its
     own is not one of these. *)
  let at_rule t ~name i =
    match Hashtbl.find_opt t.at i with
    | Some (n, header) when "@" ^ n = name -> Some header
    | _ -> None

  (* A blockless [@name PRELUDE;] starting at [i]. *)
  let at_statement t ~name i =
    match Hashtbl.find_opt t.statement i with
    | Some (n, statement) when "@" ^ n = name -> Some statement
    | _ -> None
end

(* A project can declare [@keyframes] inside its [@theme] block, beside the
   [--animate-*] token that names it. [@theme] is a build-time directive, so
   [drop_directives] takes the whole block out of the emitted CSS; lift actual
   keyframe at-rules to the top level first, where Tailwind emits them. *)
let hoist_theme_keyframes css =
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let lifted = Buffer.create 0 in
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_rule scan ~name:"@theme" i with
      | Some { brace; block = { next; _ }; _ } ->
          Buffer.add_string buf (String.sub css i (brace + 1 - i));
          go_theme (brace + 1) next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  and go_theme i stop =
    if i >= stop then go i
    else
      match Scan.at_rule scan ~name:"@keyframes" i with
      | Some { block = { next; _ }; _ } when next <= stop ->
          Buffer.add_string lifted (String.sub css i (next - i));
          go_theme next stop
      | Some _ | None ->
          Buffer.add_char buf css.[i];
          go_theme (i + 1) stop
  in
  go 0;
  Buffer.add_buffer buf lifted;
  Buffer.contents buf

(* Tailwind extends [@import] with options CSS has no grammar for
   ([theme(static)], [source(none)], [prefix(tw)]). Strip actual option function
   tokens from actual import statements so quoted parentheses and comments do
   not alter their boundaries. *)
let strip_tailwind_import_options css =
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let option_at i =
    List.find_map
      (fun name -> Scan.call scan ~name i)
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
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_statement scan ~name:"@import" i with
      | Some { next; _ } ->
          copy_import i next;
          go next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

(* Pull out the [@KEYWORD NAME { ... }] declarations, dropping them from the
   CSS: they declare something for the generator, and Tailwind does not emit
   them either. *)
let take_named_defs keyword css =
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let defs = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_rule scan ~name:keyword i with
      | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
          defs := (prelude, body) :: !defs;
          go next
      | _ ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  (Buffer.contents buf, !defs)

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
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let defs = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_rule scan ~name:"@custom-variant" i with
      | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
          defs := (prelude, body) :: !defs;
          go next
      | _ -> (
          match Scan.at_statement scan ~name:"@custom-variant" i with
          | Some { prelude; next } -> (
              match shorthand_variant prelude with
              | Some def ->
                  defs := def :: !defs;
                  go next
              | None ->
                  Buffer.add_char buf css.[i];
                  go (i + 1))
          | None ->
              Buffer.add_char buf css.[i];
              go (i + 1))
  in
  go 0;
  (Buffer.contents buf, !defs)

(* [@utility NAME { ... }] declares a project's own utility class. Only the
   static form is read here; the functional [@utility NAME-* { ... }] one needs
   [--value()]/[--modifier()] resolution. *)
let take_custom_utilities css =
  let css, defs = take_named_defs "@utility" css in
  (css, List.filter (fun (n, _) -> not (String.contains n '*')) defs)

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
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let directive_at i =
    List.find_map
      (fun name ->
        match Scan.at_rule scan ~name i with
        | Some { block = { next; _ }; _ } -> Some next
        | None ->
            Option.map
              (fun ({ next; _ } : Scan.statement) -> next)
              (Scan.at_statement scan ~name i))
      tailwind_directives
  in
  let rec go i =
    if i >= len then ()
    else
      match directive_at i with
      | Some next -> go next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

let fill_slots template body =
  let scan = Scan.v template in
  let len = String.length template in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_statement scan ~name:"@slot" i with
      | Some { next; _ } ->
          Buffer.add_string buf body;
          go next
      | None ->
          Buffer.add_char buf template.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

(* Replace [@variant NAME { decls }] with the variant's body, substituting the
   declarations at each [@slot]. Nested [@variant]s expand outermost-first, so
   the recursion re-runs over the result. *)
let rec expand_variants ~depth defs css =
  if depth > 8 then css
  else
    let scan = Scan.v css in
    let len = String.length css in
    let buf = Buffer.create len in
    let changed = ref false in
    let rec go i =
      if i >= len then ()
      else
        match Scan.at_rule scan ~name:"@variant" i with
        | Some { prelude; block = { body; next }; _ }
          when List.mem_assoc prelude defs ->
            let template = List.assoc prelude defs in
            changed := true;
            Buffer.add_string buf (fill_slots template body);
            go next
        | _ ->
            Buffer.add_char buf css.[i];
            go (i + 1)
    in
    go 0;
    let out = Buffer.contents buf in
    if !changed then expand_variants ~depth:(depth + 1) defs out else out

(* Tailwind's [--spacing(N)] is shorthand for the spacing scale. It is not CSS,
   so a parser rejects the declaration and it drops out of the output. *)
let expand_spacing_fn css =
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else
      match Scan.call scan ~name:"--spacing" i with
      | Some { body; next } ->
          Buffer.add_string buf
            (String.concat "" [ "calc(var(--spacing) * "; body; ")" ]);
          go next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

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
          match Tw.Scheme.token theme (prefix ^ "-" ^ key) with
          | Some _ as v -> v
          | None -> (
              match (ns, float_of_string_opt key) with
              | ("spacing" | "lineHeight"), Some n -> Tw.Theme.spacing_times n
              | _ -> None)))

(* Tailwind's [theme(--token)] inlines the token's value. It is not CSS, and it
   appears in places a [var()] could not stand anyway, such as a media query
   condition. An unknown token is left alone rather than guessed at. *)
let resolve_theme_fn ~theme css =
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let skip i =
    Buffer.add_char buf css.[i];
    i + 1
  in
  let rec go i =
    if i >= len then ()
    else
      match Scan.call scan ~name:"theme" i with
      | None -> go (skip i)
      | Some { body; next } -> (
          let name = String.trim body in
          let bare =
            if String.length name > 2 && String.sub name 0 2 = "--" then
              String.sub name 2 (String.length name - 2)
            else name
          in
          match
            match Tw.Scheme.token theme bare with
            | Some _ as v -> v
            | None -> v3_theme_token theme name
          with
          | Some value ->
              Buffer.add_string buf value;
              go next
          | None -> go (skip i))
  in
  go 0;
  Buffer.contents buf

(* [to_css] heads a utility's selector with the utility's own class, and a
   variant decorates it in place, as [.dark\:fill-gray-400:where(.dark, ...)].
   Swapping that class for [&] turns the rule into a nested one the author's
   selector can host, so a variant survives [@apply] without being reimplemented
   here. The class is not always the leftmost one in the selector: [divide-*]
   wraps it in [:where(.divide-x > :not(:last-child))] and [in-*] heads the
   selector with the ancestor's class instead, so it is picked out by name among
   the classes the [@apply] asked for. A selector naming none of them keeps the
   leftmost class, which is what the variants tw generates put there. *)
let nest_on_ampersand ~classes sel =
  let swap pick =
    Cascade.Selector.map (function
      | Cascade.Selector.Class name when pick name -> Cascade.Selector.Nesting
      | node -> node)
  in
  let own name = List.mem name classes in
  let arm a =
    if Cascade.Selector.exists_class own a then swap own a
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

let is_utility_statement stmt =
  match Css.statement_selector stmt with
  | None -> true
  | Some sel ->
      let s = Cascade.Selector.to_string ~minify:true sel in
      String.contains s '.'

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
let nested_utilities ~theme names =
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
      (* [to_css] also emits the theme block the utilities read from, whose
         selector is [:root]. It belongs at the top of the sheet, not inside the
         rule that applied them, and [is_utility_statement] tells the two apart
         by whether the selector names a class at all. *)
      (* [@property] belongs beside the utilities too, not inside the rule that
         applied them: nested there it is emitted once per applying rule, and
         the same property comes back for every utility that sets it. *)
      let hoisted, nestable =
        Css.statements sheet
        |> List.filter is_utility_statement
        |> List.partition (fun stmt ->
            Css.as_layer stmt <> None || Css.as_property stmt <> None)
      in
      (* One [@apply] pulls in several utilities, each with a rule of its own.
         They all decorate the same [&], so they belong in one rule, the way
         Tailwind emits them; left apart, each is a rule of the author's
         selector holding one declaration. *)
      (* One string per hoisted statement, not one for the whole block: two
         utilities bring overlapping [@property] sets, and deduping the blocks
         whole re-emits every property they do not share. *)
      ( render_nested_utilities ~classes nestable,
        List.map (fun st -> Css.to_string ~minify:true (Css.v [ st ])) hoisted
      )

(* Append each statement unless it is already there: every utility that sets the
   same variable brings back the same hoisted [@property]. *)
let add_once buf seen items =
  List.iter
    (fun s ->
      if s <> "" && not (List.mem s !seen) then begin
        seen := s :: !seen;
        Buffer.add_string buf s
      end)
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
    | None -> nested_utilities ~theme [ bare ]
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
  | [], bare when not (List.mem_assoc bare udefs) -> Some bare
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
  let scan = Scan.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let hoisted = Buffer.create 0 in
  let seen = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match Scan.at_statement scan ~name:"@apply" i with
      | Some { prelude; next } ->
          (* A utility with no declared variant and no body of its own decorates
             the applying rule's [&] directly. A run of those renders in one
             call, so their declarations land in a single rule the way Tailwind
             emits them, rather than one rule of the author's selector per
             utility. *)
          let names = apply_names prelude 0 (String.length prelude) in
          emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen names;
          go next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  (* The hoisted blocks go last: their layer is ordered by the sheet's [@layer]
     statement, not by where they sit. *)
  Buffer.add_buffer buf hoisted;
  Buffer.contents buf

(* The names an [@variant NAME {] header uses inside a body. *)
let variant_names_in css =
  let scan = Scan.v css in
  let len = String.length css in
  let rec go i acc =
    if i >= len then List.rev acc
    else
      match Scan.at_rule scan ~name:"@variant" i with
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
  let css, _ = take_custom_utilities css in
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
       (expand_spacing_fn (expand_variants ~depth:0 defs (expand 0 css))))

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

(* A named layer appears once in Tailwind's output. The generated sheet and the
   [@layer properties] blocks each applied utility hoists arrive separately, so
   fold every repeat of a name into the first, dropping content the first
   already has. *)
let named_layer stmt =
  match Css.as_layer stmt with Some (Some name, _) -> Some name | _ -> None

let layer_statements stmt =
  match Css.as_layer stmt with Some (_, inner) -> inner | None -> []

let repeated_layer_names stmts =
  let counts = Hashtbl.create 8 in
  List.iter
    (fun stmt ->
      match named_layer stmt with
      | Some name ->
          Hashtbl.replace counts name
            (1 + Option.value ~default:0 (Hashtbl.find_opt counts name))
      | None -> ())
    stmts;
  Hashtbl.fold
    (fun name count acc -> if count > 1 then name :: acc else acc)
    counts []

let merge_named_layers stmts =
  let repeated = repeated_layer_names stmts in
  if repeated = [] then stmts
  else
    let merged = Hashtbl.create 8 in
    List.iter
      (fun n ->
        let seen = Hashtbl.create 64 in
        let body =
          List.concat_map
            (fun stmt ->
              match named_layer stmt with
              | Some m when equal_layer m n -> layer_statements stmt
              | _ -> [])
            stmts
          |> List.filter (fun st ->
              let key = Css.to_string ~minify:true (Css.v [ st ]) in
              if Hashtbl.mem seen key then false
              else begin
                Hashtbl.add seen key ();
                true
              end)
        in
        Hashtbl.add merged n body)
      repeated;
    let emitted = Hashtbl.create 8 in
    List.filter_map
      (fun stmt ->
        match named_layer stmt with
        | Some n when List.exists (equal_layer n) repeated ->
            if Hashtbl.mem emitted n then None
            else begin
              Hashtbl.add emitted n ();
              Some (Css.layer ~name:n (Hashtbl.find merged n))
            end
        | _ -> Some stmt)
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

let splice_into_entrypoint ~theme ~path generated =
  match read_file path with
  | exception Sys_error _ -> generated
  | raw -> (
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
              (strip_tailwind_import_options body)
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
          |> List.concat_map (fun stmt ->
              match stmt with
              | Cascade.Stylesheet.Import { url; _ } when is_tailwind_import url
                ->
                  Css.statements generated
              | s -> [ s ])
          |> merge_named_layers |> hoist_layer_blocks
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
   variant, or it is a declared utility. *)
let is_custom_routed ~defs ~udefs cls =
  let variants, bare = split_declared_variants defs cls in
  let segs = variant_segments bare in
  variants <> [] || List.mem_assoc (List.nth segs (List.length segs - 1)) udefs

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
  match List.assoc_opt name udefs with
  | Some decls ->
      (* A declared utility means nothing to [Tw.of_string], so every prefix has
         to become a [@variant], the built-in ones included. *)
      if
        List.for_all
          (fun variant ->
            Option.is_some (routed_template ~theme derived variant))
          builtin
      then Some (wrapped_block cls (variants @ builtin) decls)
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

let dedup_statements stmts =
  let seen = Hashtbl.create 8 in
  List.filter
    (fun stmt ->
      let key = Css.to_string ~minify:true (Css.v [ stmt ]) in
      if Hashtbl.mem seen key then false
      else begin
        Hashtbl.add seen key ();
        true
      end)
    stmts

let group_routed_rules ~own_order rules =
  let group = Hashtbl.create 8 in
  let order_of = Hashtbl.create 8 in
  let classless = ref [] in
  List.iter
    (fun stmt ->
      match first_class_of_statement stmt with
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

let compare_routed_entries ~own_order ~order_of (c1, _) (c2, _) =
  let p1, s1 = routed_slot ~own_order ~order_of c1 in
  let p2, s2 = routed_slot ~own_order ~order_of c2 in
  let priority = Int.compare p1 p2 in
  let suborder = if priority <> 0 then priority else Int.compare s1 s2 in
  if suborder <> 0 then suborder else String.compare c1 c2

let ordered_routed_entries ~own_order ~order_of group =
  Hashtbl.fold (fun cls stmts acc -> (cls, stmts) :: acc) group []
  |> List.sort (compare_routed_entries ~own_order ~order_of)

let place_routed_entries ~own_order ~order_of ~classless entries =
  let ordered, unordered =
    List.fold_left
      (fun (ordered, unordered) (cls, stmts) ->
        match Hashtbl.find_opt own_order cls with
        | Some order -> ((cls, order, stmts) :: ordered, unordered)
        | None -> (
            match Hashtbl.find_opt order_of cls with
            | Some (priority, suborder) ->
                ((cls, (priority, suborder), stmts) :: ordered, unordered)
            | None -> (ordered, unordered @ stmts)))
      ([], classless) entries
  in
  (List.rev ordered, unordered)

let routed_statements ~block_count ~own_order stmts =
  (* [@layer properties] and [@property] sit beside the utilities layer, not in
     it: nested, the first would become [utilities.properties]. *)
  let hoisted, rules =
    List.partition
      (fun stmt -> Css.as_layer stmt <> None || Css.as_property stmt <> None)
      stmts
  in
  let group, order_of, classless = group_routed_rules ~own_order rules in
  let entries = ordered_routed_entries ~own_order ~order_of group in
  let ordered, unordered =
    place_routed_entries ~own_order ~order_of ~classless entries
  in
  let unplaced =
    if unordered = [] then [] else [ Css.layer ~name:[ "utilities" ] unordered ]
  in
  (block_count, ordered, unplaced @ dedup_statements hoisted)

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

let parse_routed_blocks ~block_count ~own_order css =
  match Css.of_string css with
  | Error _ -> (0, [], [])
  | Ok parsed ->
      Css.statements parsed.Css.stylesheet
      |> List.concat_map flattened_statement
      |> routed_statements ~block_count ~own_order

let custom_routed_utilities ~theme ~defs ~udefs candidates =
  let hoisted = Buffer.create 0 in
  let seen = ref [] in
  let derived = Hashtbl.create 8 in
  let own_order = Hashtbl.create 8 in
  let blocks =
    List.filter_map
      (routed_block ~theme ~defs ~udefs ~hoisted ~seen ~derived ~own_order)
      candidates
  in
  match blocks with
  | [] -> (0, [], [])
  | _ ->
      collect_routed_templates ~theme derived udefs;
      let extra_defs = routed_variant_defs defs derived in
      (* A [@utility] body is author CSS: it may hold [@apply], [@variant] and
         the [--spacing()]/[theme()] shorthands. *)
      String.concat "" (blocks @ [ Buffer.contents hoisted ])
      |> apply_variants ~extra_defs ~udefs ~theme
      |> parse_routed_blocks ~block_count:(List.length blocks) ~own_order
