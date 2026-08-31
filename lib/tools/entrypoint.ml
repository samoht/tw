module Css = Cascade.Css

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
      | Component.Preserved { kind = Token.Whitespace; _ } ->
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
          { kind = Token.Semicolon | Token.Close Token.Curly; loc }
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
   end, keyed by the offset each one starts at.

   The Tailwind at-rules below are not CSS, so a stylesheet parser drops them
   and they have to be located in the text first -- but counting braces over the
   raw bytes counts the ones inside a string, a comment or an escape too. The
   block then ends in the wrong place and everything after it is silently
   dropped. cascade's component values come out of CSS Syntax 3 sec. 5.4 already
   matched, and each carries the range of source it was parsed from, so the
   offsets are read off those. *)
module Index = struct
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

  (* Where the contents of a group end. One the source left open ends with the
     source, which is where the parser ends it too. *)
  let inner_end ~closed (loc : Loc.t) =
    if closed then loc.end_pos - 1 else loc.end_pos

  let group css ~from ~upto ~next =
    { body = String.sub css from (upto - from); next }

  let block_of css (b : Component.block Component.node) =
    let upto = inner_end ~closed:b.node.closed b.loc in
    group css ~from:(b.loc.start_pos + 1) ~upto ~next:b.loc.end_pos

  (* A call's arguments start where its first one does; one that takes none has
     an empty body against its own closer. *)
  let call_of css (f : Component.func Component.node) =
    let upto = inner_end ~closed:f.node.terminated f.loc in
    let from =
      match f.node.arguments with
      | argument :: _ -> (Component.source_loc argument).start_pos
      | [] -> upto
    in
    group css ~from ~upto ~next:f.loc.end_pos

  (* The [{] an at-rule's prelude leads to. A group in the prelude is a single
     component, so a [;] inside one does not end the at-rule. *)
  let rec brace_of = function
    | [] -> None
    | Component.Block ({ node = { opening = Token.Curly; _ }; _ } as b) :: _ ->
        Some b
    | Component.Preserved { kind = Token.Semicolon | Token.Close _; _ } :: _ ->
        None
    | _ :: rest -> brace_of rest

  (* The semicolon or enclosing closer that terminates a blockless at-rule. *)
  let rec statement_end ~closer = function
    | [] -> (closer, closer)
    | Component.Preserved { kind = Token.Semicolon; loc } :: _ ->
        (loc.start_pos, loc.end_pos)
    | Component.Preserved { kind = Token.Close Token.Curly; loc } :: _ ->
        (loc.start_pos, loc.start_pos)
    | _ :: rest -> statement_end ~closer rest

  let v css =
    let t =
      {
        call = Hashtbl.create 16;
        at = Hashtbl.create 16;
        statement = Hashtbl.create 16;
      }
    in
    let header name (at : Loc.t) rest =
      match brace_of rest with
      | None -> ()
      | Some b ->
          let brace = b.Component.loc.start_pos in
          let prelude =
            String.trim (String.sub css at.end_pos (brace - at.end_pos))
          in
          Hashtbl.replace t.at at.start_pos
            (name, { prelude; brace; block = block_of css b })
    in
    let statement ~closer name (at : Loc.t) rest =
      let upto, next = statement_end ~closer rest in
      let prelude = String.sub css at.end_pos (upto - at.end_pos) in
      Hashtbl.replace t.statement at.start_pos (name, { prelude; next })
    in
    let rec walk ~closer = function
      | [] -> ()
      | item :: rest ->
          (match item with
          | Component.Preserved { kind = Token.At_keyword name; loc } ->
              header name loc rest;
              statement ~closer name loc rest
          | Component.Preserved _ -> ()
          | Component.Block b ->
              walk ~closer:(inner_end ~closed:b.node.closed b.loc) b.node.value
          | Component.Func f ->
              Hashtbl.replace t.call f.loc.start_pos (f.node.name, call_of css f);
              walk
                ~closer:(inner_end ~closed:f.node.terminated f.loc)
                f.node.arguments);
          walk ~closer rest
    in
    let parsed = Parser.list_of_component_values (Reader.of_string css) in
    walk ~closer:(String.length css) parsed.value;
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

  (* Every [name( ... )] in the source, with the offset each starts at. *)
  let calls t ~name =
    Hashtbl.fold
      (fun i (n, block) acc -> if n = name then (i, block) :: acc else acc)
      t.call []

  (* Every blockless [@name PRELUDE;], the same way. *)
  let at_statements t ~name =
    Hashtbl.fold
      (fun i (n, s) acc -> if "@" ^ n = name then (i, s) :: acc else acc)
      t.statement []
end

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

(* A project can declare [@keyframes] inside its [@theme] block, beside the
   [--animate-*] token that names it. [@theme] is a build-time directive, so
   [drop_directives] takes the whole block out of the emitted CSS; lift actual
   keyframe at-rules to the top level first, where Tailwind emits them. *)
let hoist_theme_keyframes css =
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let lifted = Buffer.create 0 in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_rule index ~name:"@theme" i with
      | Some { brace; block = { next; _ }; _ } ->
          Buffer.add_string buf (String.sub css i (brace + 1 - i));
          go_theme (brace + 1) next
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  and go_theme i stop =
    if i >= stop then go i
    else
      match Index.at_rule index ~name:"@keyframes" i with
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
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
  let rec go i =
    if i >= len then ()
    else
      match Index.at_statement index ~name:"@import" i with
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let defs = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_rule index ~name:keyword i with
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let defs = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_rule index ~name:"@custom-variant" i with
      | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
          defs := (prelude, body) :: !defs;
          go next
      | _ -> (
          match Index.at_statement index ~name:"@custom-variant" i with
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

(* Index of the first [sub] in [s]. *)
let sub_index s sub =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go 0

let has_sub s sub = Option.is_some (sub_index s sub)
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
let has_math_fn text =
  List.exists (fun name -> has_sub text (name ^ "(")) math_functions

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
          match sub_index base opener with
          | Some idx when is_root (String.sub base 0 idx) ->
              read (String.sub base 0 idx) idx
          | _ -> []
        in
        if base.[n - 1] = ']' then
          arbitrary "-[" (fun root idx ->
              Option.to_list
                (with_value
                   (root, Some (String.sub base (idx + 1) (n - idx - 1)))))
        else if base.[n - 1] = ')' then
          arbitrary "-(" (fun root idx ->
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
    match sub_index s "-*-*" with
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
    && not (has_sub arg "-*")
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
let split_wildcard arg =
  let rec go acc s =
    match sub_index s "-*" with
    | None -> List.rev (s :: acc)
    | Some i ->
        go (String.sub s 0 i :: acc)
          (String.sub s (i + 2) (String.length s - i - 2))
  in
  go [] arg

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
            if has_sub text "--value(" || has_sub text "--modifier(" then
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let directive_at i =
    List.find_map
      (fun name ->
        match Index.at_rule index ~name i with
        | Some { block = { next; _ }; _ } -> Some next
        | None ->
            Option.map
              (fun ({ next; _ } : Index.statement) -> next)
              (Index.at_statement index ~name i))
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
  let index = Index.v template in
  let len = String.length template in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_statement index ~name:"@slot" i with
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
    let index = Index.v css in
    let len = String.length css in
    let buf = Buffer.create len in
    let changed = ref false in
    let rec go i =
      if i >= len then ()
      else
        match Index.at_rule index ~name:"@variant" i with
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else
      match Index.call index ~name:"--spacing" i with
      | Some { body; next } ->
          Buffer.add_string buf
            (match inline_spacing ~theme body with
            | Some value -> value
            | None -> String.concat "" [ "calc(var(--spacing) * "; body; ")" ]);
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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let skip i =
    Buffer.add_char buf css.[i];
    i + 1
  in
  let rec go i =
    if i >= len then ()
    else
      match Index.call index ~name:"theme" i with
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
let is_utility_statement stmt =
  match Css.statement_selector stmt with
  | None -> true
  | Some sel -> Css.Selector.exists_class (fun _ -> true) sel

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
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let hoisted = Buffer.create 0 in
  let seen = Hashtbl.create 64 in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_statement index ~name:"@apply" i with
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
       (expand_spacing_fn ~theme (expand_variants ~depth:0 defs (expand 0 css))))

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

let compare_routed_entries ~own_order ~order_of (c1, _) (c2, _) =
  let p1, s1 = routed_slot ~own_order ~order_of c1 in
  let p2, s2 = routed_slot ~own_order ~order_of c2 in
  let priority = Int.compare p1 p2 in
  let suborder = if priority <> 0 then priority else Int.compare s1 s2 in
  if suborder <> 0 then suborder else String.compare c1 c2

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
  (* [@layer properties] and [@property] sit beside the utilities layer, not in
     it: nested, the first would become [utilities.properties]. *)
  let hoisted, rules =
    List.partition
      (fun (_, stmt) ->
        Css.as_layer stmt <> None || Css.as_property stmt <> None)
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
