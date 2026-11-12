(* {2 Functional [@utility NAME-*] declarations}

   A functional declaration is a template rather than a rule: its body reads the
   candidate's own value back with [--value(...)] and the [/half] after it with
   [--modifier(...)]. A declaration whose reads do not all resolve is dropped,
   and a candidate that resolved no [--value(...)] at all is not a utility. This
   is Tailwind's [createCssUtility], and the arguments a read takes are its
   documented API: a quoted literal, a bare data type, a [[data-type]] for an
   arbitrary value, a [--namespace] to look up in the theme, and
   [--default(...)] for the candidate that spelled no value. *)

module Index = Cascade.Source_index

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

(* Pull out the [@KEYWORD NAME { ... }] declarations, dropping them from the
   CSS: they declare something for the generator, and Tailwind does not emit
   them either. *)
let take_named_defs keyword css =
  let defs = ref [] in
  let css =
    Text_pass.rewrite ~names:[ keyword ] css (fun index _ i ->
        match Index.at_rule index ~name:keyword i with
        | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
            defs := (prelude, body) :: !defs;
            Some next
        | _ -> None)
  in
  (css, !defs)

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
         Data_type.is_digit c
         || (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || c = '_' || c = '.' || c = '%' || c = '-')
       s

(* [color:var(--x)] carries the hint [color]. A [:] after anything but lower
   case letters and dashes is part of the value, not the end of a hint. *)
let split_hint text =
  match Tw.Parse.data_type_hint text with
  | Some (hint, value) -> (Some hint, value)
  | None -> (None, text)

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
    if String.starts_with ~prefix:"--" name then
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
    match Data_type.segment ':' inner with
    | [ h; v ] -> (Some h, v)
    | _ -> (None, inner)
  in
  if not (String.starts_with ~prefix:"--" name) then None
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
  match Data_type.segment '/' cls with
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
    String.starts_with ~prefix:"--" arg
    && (not (String.contains arg '('))
    && not (Re.execp wildcard_re arg)
  then arg ^ "-*"
  else arg

(* The CSS a declared utility writes for the theme token [name]: an inline token
   stands for its own value, a reference token carries that value as the
   fallback of its own reference because nothing declares it in the generated
   sheet, and any other token is a plain reference the theme layer declares.

   Written as text, like [Value_fn.expand_spacing] and for the same reason: this
   pass runs over the dialect before cascade parses any of it, and the token's
   value is whatever the [@theme] block wrote, with no value type to build a
   typed reference at. What comes out is read back by cascade. *)
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
  if not (String.starts_with ~prefix:"--" arg) then None
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
      if not (Data_type.infer read kind) then None
      else
        match kind with
        | "ratio" -> (
            match List.map String.trim (Data_type.segment '/' read) with
            | [ left; right ]
              when Data_type.is_positive_integer left
                   && Data_type.is_positive_integer right ->
                Some
                  {
                    css = String.concat " " [ left; "/"; right ];
                    is_ratio = true;
                  }
            | _ -> None)
        | "number" ->
            if Data_type.is_spacing_multiplier read then plain read else None
        | "percentage" ->
            if
              Data_type.is_positive_integer
                (String.sub read 0 (String.length read - 1))
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
  let theme_arg = String.starts_with ~prefix:"--" arg in
  let bracketed = n >= 2 && arg.[0] = '[' && arg.[n - 1] = ']' in
  match value with
  | Bare text when quoted ->
      if String.sub arg 1 (n - 2) = text then plain text else None
  | Bare text when theme_arg ->
      Option.bind (theme_arg_css ~theme arg text) plain
  | Bare text when List.mem arg Data_type.bare_kinds ->
      resolve_bare_arg ~fraction text arg
  | Bare _ -> None
  | Bracketed _ when not bracketed -> None
  | Bracketed { hint; text } -> (
      let kind = String.sub arg 1 (n - 2) in
      if kind = "*" then plain text
      else
        match hint with
        | Some spelled -> if spelled = kind then plain text else None
        | None -> if Data_type.infer text kind then plain text else None)

(* [--default(4)] answers for a candidate that spelled no value at all. *)
let default_arg arg =
  Option.map String.trim (Tw.Parse.call_body "--default" (String.trim arg))

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
        (List.map String.trim (Data_type.segment ',' block.body))
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
  Text_pass.Components.array body
  |> Array.to_list |> Text_pass.Components.punctuation |> List.iter cut;
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
let body_for ~theme ~body candidate =
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

let is_functional_candidate udefs cls = functional_candidates udefs cls <> []

(* The body [cls] gets from the functional [@utility] declarations of [udefs]:
   every declaration whose root it names, resolved against it, in the order they
   were written. Tailwind registers each of them and applies them all. *)
let functional_body ~theme ~udefs cls =
  List.find_map
    (fun candidate ->
      match
        List.filter_map
          (fun (name, body) ->
            if functional_root name = Some candidate.root then
              body_for ~theme ~body candidate
            else None)
          udefs
      with
      | [] -> None
      | bodies -> Some (String.concat "" bodies))
    (functional_candidates udefs cls)

(* [@utility NAME { ... }] declares a project's own utility class, and [@utility
   NAME-* { ... }] one whose candidate carries a value its body reads back with
   [--value()] and [--modifier()]. Both forms are read. *)
let take css = take_named_defs "@utility" css
