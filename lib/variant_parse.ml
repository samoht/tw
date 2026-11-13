module Css = Cascade.Css
open Style

(* Bracket content uses underscores for spaces, e.g. "2n+1_of_.foo" reads as nth
   = 2n+1, of_ = Some [Class "foo"]. *)
let nth_selector expr =
  let spaced = Parse.decode_underscores expr in
  Css.Selector.read_nth_selector (Cascade.Cursor.of_string spaced)

(* Substitute the resolved value into a template's [{}] placeholder. *)
let custom_variant_apply template value =
  let tlen = String.length template in
  let rec find i =
    if i + 2 > tlen then None
    else if template.[i] = '{' && template.[i + 1] = '}' then Some i
    else find (i + 1)
  in
  match find 0 with
  | Some i ->
      String.sub template 0 i ^ value
      ^ String.sub template (i + 2) (tlen - i - 2)
  | None -> template

(* Parse [is-data], [is-data-foo], or [is-data-[potato]] against the theme's own
   variants, returning the (token, resolved-selector) for a [Custom_variant]. *)
let try_custom_variant (theme : Scheme.t) s =
  let resolve name (cv : Scheme.custom_variant) =
    let make v = Custom_variant (s, custom_variant_apply cv.template v) in
    if s = name then Option.map make (List.assoc_opt "" cv.values)
    else
      let prefix = name ^ "-" in
      if String.starts_with ~prefix s && String.length s > String.length prefix
      then
        let rest =
          String.sub s (String.length prefix)
            (String.length s - String.length prefix)
        in
        let value =
          if
            String.length rest >= 2
            && rest.[0] = '['
            && rest.[String.length rest - 1] = ']'
          then Some (String.sub rest 1 (String.length rest - 2))
          else List.assoc_opt rest cv.values
        in
        Option.map make value
      else None
  in
  List.find_map (fun (name, cv) -> resolve name cv) theme.custom_variants

let is_custom_variant theme s = Option.is_some (try_custom_variant theme s)

(* Negate a container condition, cancelling double-negation and pushing through
   a named container: [not (not C)] = [C]; [name (C)] -> [name (not C)]. *)
let rec negate_container (c : Css.Container.t) : Css.Container.t =
  match c with
  | Css.Container.Not x -> x
  | Css.Container.Named (name, x) ->
      Css.Container.Named (name, negate_container x)
  | x -> Css.Container.Not x

(* Parse [has-a] (declared) or [not-has-a] (negated) against the theme's
   container variants, yielding a [Container_style] modifier. *)
let try_container_variant (theme : Scheme.t) s =
  let lookup name = List.assoc_opt name theme.container_variants in
  match lookup s with
  | Some cond -> Some (Container_style (s, cond))
  | None ->
      if String.starts_with ~prefix:"not-" s && String.length s > 4 then
        let inner = String.sub s 4 (String.length s - 4) in
        Option.map
          (fun cond -> Container_style (s, negate_container cond))
          (lookup inner)
      else None

(* Modifiers whose whole effect is a media query. A group or peer negation
   builds a selector, and a media query has no negated selector form, so one of
   these cannot be its inner: [rule.ml] answers such a pair with no rules at
   all, which reaches the author as a class that silently does nothing. The list
   is the one [Rule.media_condition_of_modifier] answers for, plus [Hover] and
   [Device_hocus], which gate on [\@media (hover: hover)]. *)
let renders_as_media = function
  | Hover | Device_hocus | Dark | Motion_safe | Motion_reduce | Contrast_more
  | Contrast_less | Print | Portrait | Landscape | Forced_colors
  | Inverted_colors | Pointer_none | Pointer_coarse | Pointer_fine
  | Any_pointer_none | Any_pointer_coarse | Any_pointer_fine | Noscript ->
      true
  | _ -> false

(* A variant whose rendering gates on [\@media (hover: hover)]: [hover] itself,
   its group, peer, named, ancestor and [has-] forms. [has-hover] holds the
   state as a name. *)
let rec involves_hover = function
  | Hover | Group_hover | Peer_hover | Device_hocus | Has "hover" -> true
  | Named_group (m, _) | Named_peer (m, _) | In_state (m, _) | Has_variant m ->
      involves_hover m
  | _ -> false

let group_state_modifiers = Style.group_state_modifiers
let is_has_shorthand = Style.is_has_shorthand

(* Find matching closing bracket, handling nested brackets *)
let matching_bracket s =
  let len = String.length s in
  let rec loop i depth =
    if i >= len then None
    else
      match s.[i] with
      | '[' -> loop (i + 1) (depth + 1)
      | ']' when depth = 0 -> Some i
      | ']' -> loop (i + 1) (depth - 1)
      | '(' -> loop (i + 1) (depth + 1)
      | ')' -> loop (i + 1) (depth - 1)
      | _ -> loop (i + 1) depth
  in
  loop 0 0

(* Extract bracketed content from a string like "prefix-[content]" *)
let extract_bracket_content ~prefix s =
  if String.starts_with ~prefix s then
    let rest =
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    in
    match matching_bracket rest with
    | Some i when i = String.length rest - 1 ->
        (* Closing bracket must be at end - reject trailing chars like /foo *)
        Some (String.sub rest 0 i)
    | _ -> None
  else None

(* Extract bracketed content allowing an optional /name suffix *)
(* Extract name suffix from "rest" after prefix, e.g. "checked/name" ->
   ("checked", Some "name") *)
let split_name rest =
  match String.index_opt rest '/' with
  | Some i ->
      let base = String.sub rest 0 i in
      let name = String.sub rest (i + 1) (String.length rest - i - 1) in
      if name <> "" then (base, Some name) else (rest, None)
  | None -> (rest, None)

(* Try parsing has-shorthand modifiers like has-checked,
   group-has-checked/name *)
(* Known aria shorthand names that map to [aria-X=true] *)
let is_aria_shorthand = function
  | "busy" | "checked" | "disabled" | "expanded" | "hidden" | "pressed"
  | "readonly" | "required" | "selected" ->
      true
  | _ -> false

(* Try parsing group-aria-*/peer-aria-* shorthand with optional /name *)
let try_aria_shorthand s =
  if String.starts_with ~prefix:"group-aria-" s && String.length s > 11 then
    let rest = String.sub s 11 (String.length s - 11) in
    let base, name = split_name rest in
    if is_aria_shorthand base then Some (Group_aria (base, name)) else None
  else if String.starts_with ~prefix:"peer-aria-" s && String.length s > 10 then
    let rest = String.sub s 10 (String.length s - 10) in
    let base, name = split_name rest in
    if is_aria_shorthand base then Some (Peer_aria (base, name)) else None
  else None

let try_has_shorthand s =
  if String.starts_with ~prefix:"has-" s && String.length s > 4 then
    let rest = String.sub s 4 (String.length s - 4) in
    if is_has_shorthand rest then Some (Has rest) else None
  else if String.starts_with ~prefix:"group-has-" s && String.length s > 10 then
    let rest = String.sub s 10 (String.length s - 10) in
    let base, name = split_name rest in
    if is_has_shorthand base then Some (Group_has (base, name)) else None
  else if String.starts_with ~prefix:"peer-has-" s && String.length s > 9 then
    let rest = String.sub s 9 (String.length s - 9) in
    let base, name = split_name rest in
    if is_has_shorthand base then Some (Peer_has (base, name)) else None
  else None

(* A bare arbitrary variant with no [&] anchor compounds onto the utility's own
   class, so it has to be a single compound selector: [[code]] and [[.line]] are
   fine, [[>img]] and [[@media_print]] are not. The bracket is read as a
   selector rather than scanned for combinator characters, because [~] and [|]
   spell an attribute operator as well as a combinator: [[data-size~=large]] is
   a compound and [[p_~_span]] is not, and only the grammar separates them. [_]
   stands for a space here, the decoding {!Modifiers.nest_selector} applies. *)
let is_compound_selector inner =
  let s = Parse.decode_underscores inner in
  let cursor = Cascade.Cursor.of_string s in
  match Css.Selector.read_strict_selector_list cursor with
  | exception (Cascade.Cursor.Parse_error _ | Invalid_argument _) -> false
  | List _ | Combined _ | Relative _ -> false
  | _ ->
      (* The reader stops at the first thing it cannot use, so trailing rubbish
         has to be refused here: it is not part of the compound. *)
      Cascade.Cursor.ws cursor;
      Cascade.Cursor.is_done cursor

(* A plain identifier: what a group/peer name or a bare data attribute may be
   spelled with. *)
let is_plain_ident str =
  str <> ""
  && String.for_all
       (fun c ->
         (c >= 'a' && c <= 'z')
         || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9')
         || c = '-' || c = '_')
       str

(* An aria- or data- variant names an attribute: [aria-[modal]] matches
   [[aria-modal]]. An argument that leaves the name empty gives [[aria-]], a
   selector browsers parse and keep although nothing ever matches it, so it is
   not a variant. An underscore stands for a space, so it is empty as well. *)
let names_attribute expr =
  let decoded = Parse.decode_underscores expr in
  decoded <> "" && String.equal decoded (String.trim decoded)

(* [group-data-dragging] is [group-data-[dragging]] with a different spelling,
   so it keeps its own class name. *)
let try_bare_data s prefix make =
  let plen = String.length prefix in
  if not (String.starts_with ~prefix s && String.length s > plen) then None
  else
    let base, name = split_name (String.sub s plen (String.length s - plen)) in
    if is_plain_ident base && names_attribute base then Some (make base name)
    else None

let extract_bracket_content_with_name ~prefix s =
  if String.starts_with ~prefix s then
    let rest =
      String.sub s (String.length prefix)
        (String.length s - String.length prefix)
    in
    match matching_bracket rest with
    | Some i when i = String.length rest - 1 ->
        (* No suffix after bracket *)
        Some (String.sub rest 0 i, None)
    | Some i when i < String.length rest - 1 && rest.[i + 1] = '/' ->
        (* /name suffix after bracket *)
        let content = String.sub rest 0 i in
        let name = String.sub rest (i + 2) (String.length rest - i - 2) in
        if name <> "" then Some (content, Some name) else None
    | _ -> None
  else None

(* Parse a CSS length from a string like "600px", "40rem", "100vh". Arbitrary
   value syntax is decoded first, so [min-[calc(1000px+12em)]] becomes a
   spec-valid [calc(1000px + 12em)] the length reader accepts. *)
let parse_css_length s : Style.arbitrary_length option =
  Option.map
    (fun len : Style.arbitrary_length -> { len; text = s })
    (Parse.arbitrary_length s)

(* Parse a pixel value from a string like "600px" or "600", keeping the
   spelling: it is what the variant's own class is named after. The digits are a
   CSS number, which is not OCaml's: [0x600px] is neither a number nor a length,
   and reading it as an OCaml float turned [min-[0x600px]] into a live 1536px
   breakpoint out of a value Tailwind leaves for the browser to drop. *)
let parse_px_value s =
  let digits =
    if String.ends_with ~suffix:"px" s then String.sub s 0 (String.length s - 2)
    else s
  in
  match
    Cascade.Cursor.try_parse_full_err Css.Values.read_number
      (Cascade.Cursor.of_string digits)
  with
  | Ok (Num px) -> Some ({ px; text = s } : Style.arbitrary_px)
  | Ok _ | Error _ -> None

(* Parse a has-selector string as a relative CSS selector ([:has()] accepts a
   bare leading combinator, e.g. [>div] or [~img]), with [&] resolved to the
   universal selector via {!Cascade.Nest.substitute}, the same substitution
   {!Modifiers.nest_selector} performs for anchored templates. *)
let has_relative_selector s =
  let sel =
    Css.Selector.read_relative
      (Cascade.Cursor.of_string (Parse.decode_underscores s))
  in
  Cascade.Nest.substitute ~parent:Css.Selector.universal sel

(* Validate that a has-selector string can be parsed as a CSS selector. Rejects
   invalid selectors like "@media_print" at parse time. *)
let is_valid_has_selector sel =
  try
    ignore (has_relative_selector sel);
    true
  with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> false

(* Find Tailwind's match operator ($=, ^=, *=, ~=, |= or bare =) in the raw,
   not-yet-underscore-decoded bracket content, skipping over anything of that
   shape written inside quotes. Cascade's own selector reader cannot do this
   step: an unquoted operator can only be told apart from the same character
   inside a quoted value by scanning for the quotes, and that scan has to run
   before the value is handed to the reader at all. *)
let data_match_operator raw_expr =
  let len = String.length raw_expr in
  let in_quotes = ref false in
  let rec go i =
    if i >= len then None
    else
      match raw_expr.[i] with
      | '"' | '\'' ->
          in_quotes := not !in_quotes;
          go (i + 1)
      | ('$' | '^' | '*' | '~' | '|')
        when (not !in_quotes) && i + 1 < len && raw_expr.[i + 1] = '=' ->
          Some (i, 2)
      | '=' when not !in_quotes -> Some (i, 1)
      | _ -> go (i + 1)
  in
  go 0

(* Split a trailing case-sensitivity flag off a decoded value remainder:
   Selectors 4 requires whitespace before [i]/[s], which is what Tailwind's own
   underscore convention spells too once the underscore before the flag decodes
   to a space (e.g. [data-[foo=bar_i]] -> "bar i"). A value that is itself
   quoted never matches this: its last character is the closing quote, and the
   flag can only follow that, as in [data-[foo='bar'_i]] -> "'bar' i" decoding
   to a value of "'bar'" and a flag. *)
let split_trailing_flag value =
  let n = String.length value in
  if n >= 2 && value.[n - 1] = 'i' && value.[n - 2] = ' ' then
    (String.trim (String.sub value 0 (n - 2)), " i")
  else if n >= 2 && value.[n - 1] = 's' && value.[n - 2] = ' ' then
    (String.trim (String.sub value 0 (n - 2)), " s")
  else (value, "")

let is_already_quoted value =
  let n = String.length value in
  n >= 2
  && ((value.[0] = '"' && value.[n - 1] = '"')
     || (value.[0] = '\'' && value.[n - 1] = '\''))

(* Wrap a decoded, not-already-quoted value in quotes CSS requires but
   Tailwind's own bracket sugar lets the author skip, e.g. the space in
   [data-[foo=bar_baz]] -> "bar baz". {!Css.Selector.pp_attribute_match} drops
   the quotes again when minified and the value does not need them, so adding
   them here never changes what a plain identifier value renders as. *)
let quote_value value =
  let q = if String.contains value '"' then '\'' else '"' in
  String.make 1 q ^ value ^ String.make 1 q

(* Read the reconstructed [[data-<expr>]] through cascade's own selector reader
   and pull the pieces [parse_data_expr] hands back out of it. *)
let data_attribute_of_bracket bracket =
  match Css.Selector.of_string bracket with
  | Css.Selector.Attribute (_, Css.Selector.Data name, m, flag) ->
      ("data-" ^ name, m, flag)
  | _ -> invalid_arg ("not a data attribute: " ^ bracket)

(* Parse a [data-[...]] bracket body (the interior text, no brackets) into an
   attribute name, match operator and optional case-sensitivity flag. Tailwind
   underscore-decodes the whole expression and only then reads it, so the value
   and flag - and, once a value is quoted, the quotes themselves - are valid CSS
   only after that decoding; {!Css.Selector.of_string} reads the reconstructed
   [[data-<expr>]] and does the actual work: choosing the match operator,
   validating the attribute name, and reading a quoted value's escapes the same
   way a browser would. Raises on a malformed expression; callers that see
   arbitrary user input should validate through {!is_valid_data_attr_expr}
   first. *)
let parse_data_expr raw_expr =
  match data_match_operator raw_expr with
  | None ->
      let name = String.trim (Parse.decode_underscores raw_expr) in
      data_attribute_of_bracket ("[data-" ^ name ^ "]")
  | Some (op_pos, op_len) ->
      let len = String.length raw_expr in
      let name =
        String.trim (Parse.decode_underscores (String.sub raw_expr 0 op_pos))
      in
      (* Validated here, on its own: folded straight into the reconstructed text
         below, a name ending in one of [$ ^ * ~ |] would recombine with the
         operator that follows it into a different, two-character one ([foo^]
         then [=] reads back as [foo] then [^=]). None of those characters is
         one {!Css.Selector.attribute} accepts in a name, so this rules the
         recombination out before it can happen. *)
      ignore (Css.Selector.attribute ("data-" ^ name) Css.Selector.Presence);
      let op_text = String.sub raw_expr op_pos op_len in
      let rest =
        String.trim
          (Parse.decode_underscores
             (String.sub raw_expr (op_pos + op_len) (len - op_pos - op_len)))
      in
      let value, flag_text = split_trailing_flag rest in
      let value_text =
        if is_already_quoted value then value else quote_value value
      in
      data_attribute_of_bracket
        ("[data-" ^ name ^ op_text ^ value_text ^ flag_text ^ "]")

(* Validate a data-[...] bracket expression by running it through the same
   reading [parse_data_expr] does at render time. *)
let is_valid_data_attr_expr expr =
  match parse_data_expr expr with
  | _ -> true
  | exception (Invalid_argument _ | Cascade.Cursor.Parse_error _) -> false

(* The query a bracket [@media] at-rule names, [[@media_print]] or
   [[@media(width>=600px)]], with underscores read as spaces; [None] when what
   follows [@media] is no query the grammar reads, so a half-written one is
   refused where the modifier is parsed rather than raised from the render. *)
let bracket_media_condition inner =
  let n = String.length inner in
  if
    String.starts_with ~prefix:"@media" inner
    && n > 7
    && (inner.[6] = '_' || inner.[6] = '(')
  then
    let cond =
      String.trim (Parse.decode_underscores (String.sub inner 6 (n - 6)))
    in
    match Css.Media.of_string_strict cond with
    | media -> Some media
    | exception Cascade.Cursor.Parse_error _ -> None
  else None

(* An at-rule in brackets is a variant too: [[@supports(display:grid)]],
   [[@media_print]] and [[@starting-style]] wrap the utility rather than select
   it. *)
let try_bracket_at_rule s =
  if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then
    let inner = String.sub s 1 (String.length s - 2) in
    if
      inner = "@starting-style"
      || String.starts_with ~prefix:"@supports" inner
         && String.length inner > 9
      || Option.is_some (bracket_media_condition inner)
    then Some (At_rule inner)
    else None
  else None

let is_supports_ident_char c =
  (c >= 'a' && c <= 'z')
  || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9')
  || c = '-'

(* The [@supports] keywords a condition joins or negates with. Written against a
   parenthesis, [not(display:grid)], the keyword is the name of a function
   token, which the grammar reads as an unknown feature that never holds, so
   each gets the spaces Tailwind gives it. Only at the top level: inside
   [selector(...)] a [:not(a)] is a pseudo-class, not the keyword. *)
let space_supports_keywords cond =
  let n = String.length cond in
  let buf = Buffer.create (n + 8) in
  let keyword_at i =
    List.find_opt
      (fun k ->
        let l = String.length k in
        i + l <= n
        && String.sub cond i l = k
        && (i = 0 || not (is_supports_ident_char cond.[i - 1]))
        && (i + l = n || not (is_supports_ident_char cond.[i + l])))
      [ "and"; "not"; "or" ]
  in
  let rec go i depth =
    if i < n then
      match cond.[i] with
      | '(' ->
          Buffer.add_char buf '(';
          go (i + 1) (depth + 1)
      | ')' ->
          Buffer.add_char buf ')';
          go (i + 1) (depth - 1)
      | c when depth = 0 -> (
          match keyword_at i with
          | Some k ->
              let len = Buffer.length buf in
              if len > 0 && Buffer.nth buf (len - 1) <> ' ' then
                Buffer.add_char buf ' ';
              Buffer.add_string buf k;
              let next = i + String.length k in
              if next < n && cond.[next] <> ' ' then Buffer.add_char buf ' ';
              go next depth
          | None ->
              Buffer.add_char buf c;
              go (i + 1) depth)
      | c ->
          Buffer.add_char buf c;
          go (i + 1) depth
  in
  go 0 0;
  String.trim (Buffer.contents buf)

(* [prop: value] is a property test only when what stands before the colon is a
   property name. In [not(display:grid)] it is [not(display], and splitting
   there handed the property reader [grid)] for a value. *)
let supports_property_test cond =
  match String.index_opt cond ':' with
  | None -> None
  | Some i ->
      let prop = String.trim (String.sub cond 0 i) in
      if prop <> "" && String.for_all is_supports_ident_char prop then
        Some
          ( prop,
            String.trim (String.sub cond (i + 1) (String.length cond - i - 1))
          )
      else None

let normalize_supports_condition condition_str =
  let cond = Parse.decode_underscores condition_str in
  if
    String.starts_with ~prefix:"--" cond
    && String.length cond > 2
    && not (String.contains cond ':')
  then
    (* Bare custom property: --test tests against var(--tw), built directly
       rather than assembled as "(--test: var(--tw))" and re-parsed. *)
    Css.Supports.property cond "var(--tw)"
  else if
    String.starts_with ~prefix:"(" cond && String.ends_with ~suffix:")" cond
  then
    (* Already parenthesised, so it is the condition the author wrote; only the
       real grammar reader can make sense of arbitrary authored text. *)
    Css.Supports.of_string (space_supports_keywords cond)
  else
    match supports_property_test cond with
    | Some (prop, value) ->
        (* [prop: value], the property test Tailwind emits, built directly. *)
        Css.Supports.property prop value
    | None when String.contains cond '(' ->
        (* A negation, a function such as [selector(...)] or
           [font-format(opentype)], or a condition joined with [and]/[or]; again
           arbitrary authored text, so the real grammar reader parses it. *)
        Css.Supports.of_string (space_supports_keywords cond)
    | None ->
        (* Bare property name: backdrop-filter tests against var(--tw), the same
           expansion as the bare custom property above. A lone identifier is not
           a condition the CSS grammar has a production for, so leaving it
           raised a parse error out of the scanner. *)
        Css.Supports.property cond "var(--tw)"

(* Validate that a supports-[...] bracket normalizes to a condition the
   [@supports] grammar has a production for. An empty or half-written one used
   to reach the condition reader and raise from there, and a property test whose
   value is no declaration value raised [Failure] out of the property reader. *)
let is_valid_supports_condition cond =
  cond <> ""
  &&
    try
      ignore (normalize_supports_condition cond);
      true
    with Cascade.Cursor.Parse_error _ | Invalid_argument _ | Failure _ ->
      false

(* Which spelling an [nth-*] argument was written in. A bare number reads both
   ways and they are different classes, so the reader records the one it saw
   rather than deciding again when it prints. *)
let bracketed expr : Style.nth_expr = { expr; bracketed = true }
let bare expr : Style.nth_expr = { expr; bracketed = false }

(* Validate that an nth-*-[...] bracket holds an An+B expression (Selectors 4
   sec. 9.3). The reader raises on anything else, so the expression is checked
   where the modifier is parsed, like the has- selector above. *)
let is_valid_nth_expr expr =
  try
    ignore (nth_selector expr);
    true
  with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> false

(* Try parsing a bracketed modifier, returning Some if matched *)
(* Build the list of bracket pattern matchers for a given input string *)
let bracket_named_patterns s =
  let ( let* ) = Option.bind in
  (* Every aria- and data- bracket here takes an attribute expression, so the
     argument has to name an attribute. *)
  let try_attr ?(extra = fun _ -> true) prefix make =
    let* expr = extract_bracket_content ~prefix s in
    if names_attribute expr && extra expr then Some (make expr) else None
  in
  let try_named ?(extra = fun _ -> true) prefix make =
    let* expr, name = extract_bracket_content_with_name ~prefix s in
    if names_attribute expr && extra expr then Some (make expr name) else None
  in
  let try_named_has prefix make =
    let* sel, name = extract_bracket_content_with_name ~prefix s in
    if is_valid_has_selector sel then Some (make sel name) else None
  in
  [
    (fun () -> try_named "group-aria-[" (fun e n -> Group_aria (e, n)));
    (fun () -> try_named "peer-aria-[" (fun e n -> Peer_aria (e, n)));
    (fun () -> try_attr "aria-[" (fun expr -> Aria_bracket expr));
    (fun () ->
      try_named "group-data-[" ~extra:is_valid_data_attr_expr (fun e n ->
          Group_data ("[" ^ e ^ "]", n)));
    (fun () ->
      try_named "peer-data-[" ~extra:is_valid_data_attr_expr (fun e n ->
          Peer_data ("[" ^ e ^ "]", n)));
    (* Bare shorthand: [group-data-dragging] is [group-data-[dragging]] with a
       different spelling, so it keeps its own class name. *)
    (fun () -> try_bare_data s "group-data-" (fun e n -> Group_data (e, n)));
    (fun () -> try_bare_data s "peer-data-" (fun e n -> Peer_data (e, n)));
    (fun () ->
      try_attr "data-[" ~extra:is_valid_data_attr_expr (fun expr ->
          Data_bracket expr));
    (fun () -> try_named_has "group-has-[" (fun s n -> Group_has (s, n)));
    (fun () -> try_named_has "peer-has-[" (fun s n -> Peer_has (s, n)));
    (fun () ->
      let* sel = extract_bracket_content ~prefix:"has-[" s in
      if is_valid_has_selector sel then Some (Has sel) else None);
  ]

(* Every [nth-*-[...]] arm goes through [try_nth], so the bracket flag is set in
   one place. *)
let bracket_value_patterns s =
  let ( let* ) = Option.bind in
  let try_with prefix parse make =
    let* content = extract_bracket_content ~prefix s in
    let* value = parse content in
    Some (make value)
  in
  let try_nth prefix make =
    let* expr = extract_bracket_content ~prefix s in
    if is_valid_nth_expr expr then Some (make (bracketed expr)) else None
  in
  [
    (fun () -> try_with "min-[" parse_px_value (fun px -> Min_arbitrary px));
    (fun () -> try_with "max-[" parse_px_value (fun px -> Max_arbitrary px));
    (fun () ->
      try_with "min-[" parse_css_length (fun l -> Min_arbitrary_length l));
    (fun () ->
      try_with "max-[" parse_css_length (fun l -> Max_arbitrary_length l));
    (fun () -> try_nth "nth-last-of-type-[" (fun e -> Nth_last_of_type e));
    (fun () -> try_nth "nth-of-type-[" (fun e -> Nth_of_type e));
    (fun () -> try_nth "nth-last-[" (fun e -> Nth_last e));
    (fun () -> try_nth "nth-[" (fun e -> Nth e));
    (fun () ->
      let* cond = extract_bracket_content ~prefix:"supports-[" s in
      if is_valid_supports_condition cond then Some (Supports_condition cond)
      else None);
    (fun () ->
      try_with "group-["
        (fun sel ->
          if sel <> "" && (String.contains sel '&' || is_compound_selector sel)
          then Some sel
          else None)
        (fun sel -> Group_arbitrary sel));
    (fun () ->
      try_with "peer-["
        (fun sel ->
          if sel <> "" && (String.contains sel '&' || is_compound_selector sel)
          then Some sel
          else None)
        (fun sel -> Peer_arbitrary sel));
    (fun () -> try_bracket_at_rule s);
    (fun () ->
      if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']'
      then
        let inner = String.sub s 1 (String.length s - 2) in
        if String.contains inner '&' || is_compound_selector inner then
          Some (Arbitrary_selector inner)
        else None
      else None);
  ]

let bracket_patterns s = bracket_named_patterns s @ bracket_value_patterns s

(* Try supports-<property> shorthand: supports-grid → Supports_property
   "grid" *)
let try_supports_shorthand s =
  if
    String.starts_with ~prefix:"supports-" s
    && String.length s > 9
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then
    let prop = String.sub s 9 (String.length s - 9) in
    Some (Supports_property prop)
  else None

let try_bracketed_modifier s =
  match List.find_map (fun f -> f ()) (bracket_patterns s) with
  | Some _ as r -> r
  | None -> try_supports_shorthand s

(* Try to parse numeric nth patterns: nth-N, nth-last-N, nth-of-type-N,
   nth-last-of-type-N where N is a positive integer *)
let try_numeric_nth s =
  let try_prefix prefix make =
    let plen = String.length prefix in
    if String.starts_with ~prefix s && String.length s > plen then
      let rest = String.sub s plen (String.length s - plen) in
      if Style.is_numeric rest then Some (make rest) else None
    else None
  in
  match try_prefix "nth-last-of-type-" (fun n -> Nth_last_of_type (bare n)) with
  | Some _ as r -> r
  | None -> (
      match try_prefix "nth-of-type-" (fun n -> Nth_of_type (bare n)) with
      | Some _ as r -> r
      | None -> (
          match try_prefix "nth-last-" (fun n -> Nth_last (bare n)) with
          | Some _ as r -> r
          | None -> try_prefix "nth-" (fun n -> Nth (bare n))))

(* Simple modifiers - direct string to modifier mapping *)
let simple_modifiers =
  [
    (* Responsive breakpoints *)
    ("sm", Responsive `Sm);
    ("md", Responsive `Md);
    ("lg", Responsive `Lg);
    ("xl", Responsive `Xl);
    ("2xl", Responsive `Xl_2);
    (* Min responsive breakpoints (explicit min-width, same as unprefixed) *)
    ("min-sm", Min_responsive `Sm);
    ("min-md", Min_responsive `Md);
    ("min-lg", Min_responsive `Lg);
    ("min-xl", Min_responsive `Xl);
    ("min-2xl", Min_responsive `Xl_2);
    (* Max responsive breakpoints *)
    ("max-sm", Max_responsive `Sm);
    ("max-md", Max_responsive `Md);
    ("max-lg", Max_responsive `Lg);
    ("max-xl", Max_responsive `Xl);
    ("max-2xl", Max_responsive `Xl_2);
    (* Interactive states *)
    ("hover", Hover);
    ("focus", Focus);
    ("active", Active);
    ("disabled", Disabled);
    ("focus-within", Focus_within);
    ("focus-visible", Focus_visible);
    (* Appearance *)
    ("dark", Dark);
    ("motion-safe", Motion_safe);
    ("motion-reduce", Motion_reduce);
    ("contrast-more", Contrast_more);
    ("contrast-less", Contrast_less);
    ("forced-colors", Forced_colors);
    ("inverted-colors", Inverted_colors);
    ("pointer-none", Pointer_none);
    ("pointer-coarse", Pointer_coarse);
    ("pointer-fine", Pointer_fine);
    ("any-pointer-none", Any_pointer_none);
    ("any-pointer-coarse", Any_pointer_coarse);
    ("any-pointer-fine", Any_pointer_fine);
    ("noscript", Noscript);
    ("print", Print);
    ("portrait", Portrait);
    ("landscape", Landscape);
    ("ltr", Ltr);
    ("rtl", Rtl);
    (* Group states *)
    ("group-hover", Group_hover);
    ("group-focus", Group_focus);
    ("group-active", Group_active);
    ("group-visited", Group_visited);
    ("group-disabled", Group_disabled);
    ("group-checked", Group_checked);
    ("group-empty", Group_empty);
    ("group-required", Group_required);
    ("group-valid", Group_valid);
    ("group-invalid", Group_invalid);
    ("group-indeterminate", Group_indeterminate);
    ("group-default", Group_default);
    ("group-open", Group_open);
    ("group-target", Group_target);
    ("group-first", Group_first);
    ("group-last", Group_last);
    ("group-only", Group_only);
    ("group-odd", Group_odd);
    ("group-even", Group_even);
    ("group-first-of-type", Group_first_of_type);
    ("group-last-of-type", Group_last_of_type);
    ("group-only-of-type", Group_only_of_type);
    ("group-optional", Group_optional);
    ("group-read-only", Group_read_only);
    ("group-read-write", Group_read_write);
    ("group-inert", Group_inert);
    ("group-user-valid", Group_user_valid);
    ("group-user-invalid", Group_user_invalid);
    ("group-placeholder-shown", Group_placeholder_shown);
    ("group-autofill", Group_autofill);
    ("group-in-range", Group_in_range);
    ("group-out-of-range", Group_out_of_range);
    ("group-focus-within", Group_focus_within);
    ("group-focus-visible", Group_focus_visible);
    ("group-enabled", Group_enabled);
    ("group-hocus", Group_hocus);
    (* Peer states *)
    ("peer-hover", Peer_hover);
    ("peer-focus", Peer_focus);
    ("peer-checked", Peer_checked);
    ("peer-active", Peer_active);
    ("peer-visited", Peer_visited);
    ("peer-disabled", Peer_disabled);
    ("peer-empty", Peer_empty);
    ("peer-required", Peer_required);
    ("peer-valid", Peer_valid);
    ("peer-invalid", Peer_invalid);
    ("peer-indeterminate", Peer_indeterminate);
    ("peer-default", Peer_default);
    ("peer-open", Peer_open);
    ("peer-target", Peer_target);
    ("peer-first", Peer_first);
    ("peer-last", Peer_last);
    ("peer-only", Peer_only);
    ("peer-odd", Peer_odd);
    ("peer-even", Peer_even);
    ("peer-first-of-type", Peer_first_of_type);
    ("peer-last-of-type", Peer_last_of_type);
    ("peer-only-of-type", Peer_only_of_type);
    ("peer-optional", Peer_optional);
    ("peer-read-only", Peer_read_only);
    ("peer-read-write", Peer_read_write);
    ("peer-inert", Peer_inert);
    ("peer-user-valid", Peer_user_valid);
    ("peer-user-invalid", Peer_user_invalid);
    ("peer-placeholder-shown", Peer_placeholder_shown);
    ("peer-autofill", Peer_autofill);
    ("peer-in-range", Peer_in_range);
    ("peer-out-of-range", Peer_out_of_range);
    ("peer-focus-within", Peer_focus_within);
    ("peer-focus-visible", Peer_focus_visible);
    ("peer-enabled", Peer_enabled);
    ("peer-hocus", Peer_hocus);
    (* ARIA variants *)
    ("aria-checked", Aria_checked);
    ("aria-expanded", Aria_expanded);
    ("aria-selected", Aria_selected);
    ("aria-disabled", Aria_disabled);
    (* Structural pseudo-classes *)
    ("first", First);
    ("last", Last);
    ("only", Only);
    ("odd", Odd);
    ("even", Even);
    ("first-of-type", First_of_type);
    ("last-of-type", Last_of_type);
    ("only-of-type", Only_of_type);
    ("empty", Empty);
    (* Form states *)
    ("checked", Checked);
    ("indeterminate", Indeterminate);
    ("default", Default);
    ("required", Required);
    ("valid", Valid);
    ("invalid", Invalid);
    ("in-range", In_range);
    ("out-of-range", Out_of_range);
    ("placeholder-shown", Placeholder_shown);
    ("autofill", Autofill);
    ("read-only", Read_only);
    ("read-write", Read_write);
    ("optional", Optional);
    ("open", Open);
    ("enabled", Enabled);
    ("target", Target);
    ("visited", Visited);
    ("inert", Inert);
    ("user-valid", User_valid);
    ("user-invalid", User_invalid);
    (* Pseudo-elements *)
    ("before", Pseudo_before);
    ("after", Pseudo_after);
    ("marker", Pseudo_marker);
    ("selection", Pseudo_selection);
    ("placeholder", Pseudo_placeholder);
    ("backdrop", Pseudo_backdrop);
    ("file", Pseudo_file);
    ("first-letter", Pseudo_first_letter);
    ("first-line", Pseudo_first_line);
    ("details-content", Pseudo_details_content);
    (* Other *)
    ("starting", Starting);
    ("*", Children);
    ("**", Descendants);
    ("hocus", Hocus);
    ("device-hocus", Device_hocus);
    (* Container queries *)
    ("@3xs", Container Container_3xs);
    ("@2xs", Container Container_2xs);
    ("@xs", Container Container_xs);
    ("@sm", Container Container_sm);
    ("@md", Container Container_md);
    ("@lg", Container Container_lg);
    ("@xl", Container Container_xl);
    ("@2xl", Container Container_2xl);
    ("@3xl", Container Container_3xl);
    ("@4xl", Container Container_4xl);
    ("@5xl", Container Container_5xl);
    ("@6xl", Container Container_6xl);
    ("@7xl", Container Container_7xl);
  ]

(* [simple_modifiers] holds the built-in breakpoints as a fixed set, so a
   [@theme] block that removed one is only visible through the scheme: [md:]
   then names a breakpoint the project does not have, and stops resolving the
   way an unknown variant does. *)
let modifier_breakpoint_is_defined theme = function
  | Responsive bp | Min_responsive bp | Max_responsive bp ->
      Scheme.has_breakpoint theme (Style.pp_modifier (Responsive bp))
  | _ -> true

(* Look a variant up in [simple_modifiers], honouring the theme's
   breakpoints. *)
let lookup_simple theme s =
  match List.assoc_opt s simple_modifiers with
  | Some m when modifier_breakpoint_is_defined theme m -> Some m
  | Some _ | None -> None

(* Try looking up a custom breakpoint (e.g., "10xl", "min-10xl", "max-10xl") *)
let try_custom_breakpoint breakpoints s =
  (* Direct name: e.g., "10xl" → Custom_responsive *)
  if List.mem s breakpoints then Some (Custom_responsive s)
  else if String.starts_with ~prefix:"min-" s && String.length s > 4 then
    (* min-<name>: e.g., "min-10xl" → Min_custom *)
    let name = String.sub s 4 (String.length s - 4) in
    if List.mem name breakpoints then Some (Min_custom name) else None
  else if String.starts_with ~prefix:"max-" s && String.length s > 4 then
    let name = String.sub s 4 (String.length s - 4) in
    if List.mem name breakpoints then Some (Max_custom name) else None
  else None

(** Try not-* shorthand patterns that aren't in simple_modifiers or bracket
    patterns. These are modifiers like data-foo, has-checked, nth-2 that work as
    shorthands in the not-* context. *)
let try_not_shorthand inner =
  match try_supports_shorthand inner with
  | Some m -> Some (Not m)
  | None ->
      if String.starts_with ~prefix:"data-" inner && String.length inner > 5
      then
        (* data-X shorthand — attribute presence check *)
        let attr = String.sub inner 5 (String.length inner - 5) in
        Some (Not (Data_custom (attr, "")))
      else if String.starts_with ~prefix:"nth-" inner && String.length inner > 4
      then
        (* nth-X shorthand — :nth-child(X) *)
        let expr = String.sub inner 4 (String.length inner - 4) in
        Some (Not (Nth (Style.nth_expr expr)))
      else None

(** Check if a modifier is compatible with not-* negation. Pseudo-elements,
    starting style and children/descendants cannot be negated; a container query
    can, as [\@container not (width >= 28rem)]. *)
let is_not_compatible = function
  | Pseudo_before | Pseudo_after | Pseudo_marker | Pseudo_selection
  | Pseudo_placeholder | Pseudo_backdrop | Pseudo_file | Pseudo_first_letter
  | Pseudo_first_line | Pseudo_details_content | Starting | Children
  | Descendants | Prose_element _ ->
      false
  (* A bracket [@media] at-rule wraps the utility in a query and has no selector
     to negate or to look for; [not-[@media ...]] has its own reading, and
     Tailwind compiles nothing for [has-[@media_print]]. *)
  | At_rule content when Option.is_some (bracket_media_condition content) ->
      false
  (* A bracket [@supports] condition has its own [not-] reading too, which
     negates the condition; read as a plain variant it negated the class. *)
  | At_rule content
    when String.starts_with ~prefix:"@supports" content
         && String.length content > 9 ->
      false
  | _ -> true

(* A variant that is an at-rule and nothing else: a media, breakpoint, supports
   or container query, or the negation of one. *)
let rec at_rule_only = function
  | Responsive _ | Min_responsive _ | Max_responsive _ | Min_arbitrary _
  | Max_arbitrary _ | Min_arbitrary_length _ | Max_arbitrary_length _
  | Custom_responsive _ | Min_custom _ | Max_custom _ | Supports_property _
  | Supports_condition _ | Container _ | Starting ->
      true
  | At_rule content | Not_bracket content ->
      Option.is_some (bracket_media_condition content)
      || String.starts_with ~prefix:"@supports" content
         && String.length content > 9
  | Not m -> at_rule_only m
  | m -> renders_as_media m && not (involves_hover m)

(* The variants a [group-not-] or [peer-not-] holds. A plain [not-hover] is fine
   - it negates the selector and keeps the hover media gate - but a group or
   peer negation has only the selector, so an inner with an at-rule half, a
   breakpoint or a container as much as [hover], leaves it nothing to emit, and
   Tailwind compiles nothing for it. *)
let scoped_negation_holds m =
  is_not_compatible m && (not (at_rule_only m)) && not (involves_hover m)

(* The variants a [has-] or [in-] holds: those with a selector of their own. An
   at-rule variant has none. A negation has one unless it negates an at-rule,
   and [not-hover] pairs its selector with [\@media not (hover: hover)], which a
   compound cannot carry: Tailwind compiles nothing for [has-md], [has-not-dark]
   or [in-not-group-hover]. [hover] itself keeps its gate around the compound,
   so [has-hover] is fine. *)
let rec has_selector = function
  | Not m -> is_not_compatible m && has_selector m && not (involves_hover m)
  | m -> is_not_compatible m && not (at_rule_only m)

(* [not-[...]] whose content is neither a media condition nor a pseudo-class
   negates the content read as a selector, so text the selector grammar cannot
   read - or can read only a prefix of - is not a negation at all. The reader
   raises rather than answering, and it stops at the first thing it cannot use,
   so the cursor has to be exhausted too: a trailing remainder would otherwise
   be dropped and the rule would negate less than the class says. *)
let reads_as_selector content =
  let s = Parse.decode_underscores content in
  let cursor = Cascade.Cursor.of_string s in
  match Css.Selector.read cursor with
  | exception (Cascade.Cursor.Parse_error _ | Invalid_argument _) -> false
  | _ ->
      Cascade.Cursor.ws cursor;
      Cascade.Cursor.is_done cursor

(* The condition a bracket [@supports] at-rule names under [not-], with
   underscores read as spaces: [(display:grid)] or [not (display:grid)]. [None]
   for a compound [and]/[or] condition, which has no single negation, and for
   text the condition grammar does not read. *)
let bracket_supports_condition content =
  let n = String.length content in
  if String.starts_with ~prefix:"@supports" content && n > 9 then
    let cond =
      String.trim (Parse.decode_underscores (String.sub content 9 (n - 9)))
    in
    match Css.Supports.of_string cond with
    | Css.Supports.And _ | Css.Supports.Or _
    | Css.Supports.Not (Css.Supports.And _ | Css.Supports.Or _) ->
        None
    | condition -> Some condition
    | exception (Cascade.Cursor.Parse_error _ | Invalid_argument _ | Failure _)
      ->
        None
  else None

(** Check if bracket content is valid for not-[...] patterns. Rejects combinator
    selectors (+, >, ~), media conditions with commas, and bare selectors. *)
let is_valid_not_bracket_content content =
  if String.length content = 0 then false
  else
    (* Reject combinator selectors: +img, >img, ~img *)
    let first = content.[0] in
    if first = '+' || first = '>' || first = '~' then false
    else if
      (* Reject media conditions with commas (complex media) *)
      (String.starts_with ~prefix:"@media" content && String.length content > 6)
      || String.starts_with ~prefix:"@media_" content
         && String.length content > 7
    then not (String.contains content ',')
    else if
      String.starts_with ~prefix:"@supports" content
      && String.length content > 9
    then Option.is_some (bracket_supports_condition content)
    else if first = ':' then true
    else reads_as_selector content

(** Try parsing a not-[...] bracket pattern. Returns the Not_bracket modifier
    for pseudo-class or media bracket content. *)
let try_not_bracket inner =
  if String.starts_with ~prefix:"[" inner then
    let rest = String.sub inner 1 (String.length inner - 1) in
    match matching_bracket rest with
    | Some i when i = String.length rest - 1 ->
        let content = String.sub rest 0 i in
        if is_valid_not_bracket_content content then Some (Not_bracket content)
        else None
    (* Content after the bracket, [not-[:checked]/foo] among it: a named
       not-bracket variant is invalid, and so is any other tail. *)
    | Some _ | None -> None
  else None

(** Parse group-not-* or peer-not-* pattern. Splits the rest into inner modifier
    and optional /name suffix. *)
let parse_group_peer_not_inner rest =
  (* Check for bracket content first: [...]/name or [...] *)
  if String.starts_with ~prefix:"[" rest then
    let after_bracket = String.sub rest 1 (String.length rest - 1) in
    match matching_bracket after_bracket with
    | Some i ->
        let content = String.sub after_bracket 0 i in
        let remainder =
          String.sub after_bracket (i + 1) (String.length after_bracket - i - 1)
        in
        let name =
          if
            String.starts_with ~prefix:"/" remainder
            && String.length remainder > 1
          then Some (String.sub remainder 1 (String.length remainder - 1))
          else None
        in
        Some (Not_bracket content, name)
    | None -> None
  else
    (* Non-bracket: split on / for name suffix *)
    match String.index_opt rest '/' with
    | Some i ->
        let inner_str = String.sub rest 0 i in
        let name = String.sub rest (i + 1) (String.length rest - i - 1) in
        let inner_mod =
          match List.assoc_opt inner_str simple_modifiers with
          | Some m when scoped_negation_holds m -> Some m
          | Some _ | None -> None
        in
        Option.map (fun m -> (m, Some name)) inner_mod
    | None -> (
        match List.assoc_opt rest simple_modifiers with
        | Some m when scoped_negation_holds m -> Some (m, None)
        | Some _ | None -> None)

(* Try to parse compound named group variants: not-group-STATE/name,
   has-group-STATE/name, in-group-STATE/name, group-peer-STATE/name *)
let try_compound_named_group s =
  let try_match prefix make =
    let plen = String.length prefix in
    if String.starts_with ~prefix s && String.length s > plen then
      let rest = String.sub s plen (String.length s - plen) in
      let base, name_opt = split_name rest in
      match name_opt with
      (* [group-hover/[]] is not a name Tailwind accepts. *)
      | Some name when is_plain_ident name -> (
          match List.assoc_opt base group_state_modifiers with
          | Some m -> Some (make m name)
          | None -> None)
      | Some _ | None -> None
    else None
  in
  (* Longest prefix first: [group-peer-X/n] must not be read as [group-] with
     the state [peer-X]. *)
  List.find_map
    (fun f -> f ())
    [
      (fun () -> try_match "not-group-" (fun m n -> Not_named_group (m, n)));
      (fun () -> try_match "has-group-" (fun m n -> Has_named_group (m, n)));
      (fun () -> try_match "in-group-" (fun m n -> In_named_group (m, n)));
      (fun () -> try_match "group-peer-" (fun m n -> Group_peer_named (m, n)));
      (fun () -> try_match "group-" (fun m n -> Named_group (m, n)));
      (fun () -> try_match "peer-" (fun m n -> Named_peer (m, n)));
    ]

(* Try in-* pattern: in-[selector] or in-data-attr *)
let try_in_modifier s =
  if not (String.starts_with ~prefix:"in-" s && String.length s > 3) then None
  else
    let rest = String.sub s 3 (String.length s - 3) in
    if String.starts_with ~prefix:"[" rest then
      let after = String.sub rest 1 (String.length rest - 1) in
      match matching_bracket after with
      | Some i when i = String.length after - 1 ->
          Some (In_bracket (String.sub after 0 i))
      | _ -> None
    else if
      String.starts_with ~prefix:"data-" rest
      && String.length rest > 5
      && not (String.contains rest '[')
    then Some (In_data (String.sub rest 5 (String.length rest - 5)))
    else
      (* [in-focus]: an ancestor in that state, the same state names the other
         variants take. *)
      match List.assoc_opt rest group_state_modifiers with
      | Some m -> Some (In_state (m, rest))
      | None -> None

(* Try not-in-[...] pattern *)
let try_not_in_modifier s =
  if not (String.starts_with ~prefix:"not-in-[" s && String.length s > 8) then
    None
  else
    let rest = String.sub s 8 (String.length s - 8) in
    match matching_bracket rest with
    | Some i when i = String.length rest - 1 ->
        Some (Not (In_bracket (String.sub rest 0 i)))
    | _ -> None

(* Try group-not-* and peer-not-* patterns *)
let try_group_peer_not s =
  let try_prefix prefix make =
    let plen = String.length prefix in
    if String.starts_with ~prefix s && String.length s > plen then
      let rest = String.sub s plen (String.length s - plen) in
      match parse_group_peer_not_inner rest with
      | Some (inner, name) -> Some (make inner name)
      | None -> None
    else None
  in
  match try_prefix "group-not-" (fun i n -> Group_not (i, n)) with
  | Some _ as r -> r
  | None -> try_prefix "peer-not-" (fun i n -> Peer_not (i, n))

(* Try not-* prefix: wrap inner modifier in Not *)
let try_not_modifier theme s =
  if not (String.starts_with ~prefix:"not-" s && String.length s > 4) then None
  else
    let inner = String.sub s 4 (String.length s - 4) in
    match List.assoc_opt inner simple_modifiers with
    | Some m when is_not_compatible m ->
        if modifier_breakpoint_is_defined theme m then Some (Not m) else None
    | Some _ -> None
    | None ->
        let fns =
          [
            (fun () -> try_not_bracket inner);
            (fun () ->
              match try_bracketed_modifier inner with
              | Some m when is_not_compatible m -> Some (Not m)
              | Some _ -> None
              | None -> None);
            (fun () -> try_not_shorthand inner);
          ]
        in
        List.find_map (fun f -> f ()) fns

(* Try bare data-X or aria-X shorthand *)
let try_bare_data_aria s =
  if
    String.starts_with ~prefix:"data-" s
    && String.length s > 5
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then Some (Data_custom (String.sub s 5 (String.length s - 5), ""))
  else if
    String.starts_with ~prefix:"aria-" s
    && String.length s > 5
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then
    let expr = String.sub s 5 (String.length s - 5) in
    if names_attribute expr then Some (Aria_bracket expr) else None
  else None

(* One name per variant @tailwindcss/typography registers. A name outside this
   set is not a variant there either, so the class is rejected rather than
   compiled into a selector Tailwind never produces. *)
let is_prose_element_name = function
  | "headings" | "p" | "a" | "strong" | "em" | "code" | "pre" | "ol" | "ul"
  | "li" | "dl" | "dt" | "dd" | "blockquote" | "h1" | "h2" | "h3" | "h4" | "h5"
  | "h6" | "img" | "picture" | "video" | "figure" | "figcaption" | "hr"
  | "table" | "tr" | "th" | "td" | "thead" | "kbd" | "lead" ->
      true
  | _ -> false

(* Try parsing prose-* element variant modifier *)
let try_prose_element s =
  if String.starts_with ~prefix:"prose-" s && String.length s > 6 then
    let name = String.sub s 6 (String.length s - 6) in
    if is_prose_element_name name then Some (Prose_element name) else None
  else None

(* Parse the container-query modifier forms that are not plain table entries:
   [@min-<size>], [@max-<size>], [@\[<len>\]], [@min-\[<len>\]], and
   [@max-\[<len>\]]. The bare [@<size>] forms are in [simple_modifiers]. *)
let container_size_of_string = function
  | "3xs" -> Some Container_3xs
  | "2xs" -> Some Container_2xs
  | "xs" -> Some Container_xs
  | "sm" -> Some Container_sm
  | "md" -> Some Container_md
  | "lg" -> Some Container_lg
  | "xl" -> Some Container_xl
  | "2xl" -> Some Container_2xl
  | "3xl" -> Some Container_3xl
  | "4xl" -> Some Container_4xl
  | "5xl" -> Some Container_5xl
  | "6xl" -> Some Container_6xl
  | "7xl" -> Some Container_7xl
  | _ -> None

(* The theme token a container-query arbitrary value reads through [theme()],
   e.g. [breakpoint-lg] for [@min-[theme(--breakpoint-lg)]]. Tailwind spells the
   lookup [theme(--x)] and [--theme(--x)]. *)
let container_theme_token content =
  let s = String.trim content in
  let s = Option.value (Parse.bare_name s) ~default:s in
  match Parse.call_body "theme" s with
  | Some body when body <> "" ->
      let inner = String.trim body in
      Some
        (if String.starts_with ~prefix:"--" inner then
           String.sub inner 2 (String.length inner - 2)
         else inner)
  | Some _ | None -> None

(* A container-query arbitrary value may reference a theme token, e.g.
   [@min-[theme(--breakpoint-lg)]]. [theme(--breakpoint-lg)] resolves to the
   [--breakpoint-lg] default (64rem) via the same token table the [lg:] variant
   publishes, so a plain length parse falls back to token resolution. *)
let parse_container_length content =
  match Css.parse_length content with
  | Some _ as len -> len
  | None ->
      Option.bind
        (Option.bind (container_theme_token content) Scheme.token_default)
        Css.parse_length

(* Resolving a [theme()] reference into the query does not consume the token:
   Tailwind writes its binding into [@layer theme] as well, so a consumer
   reading [--breakpoint-lg] off the sheet still finds it. The value is the
   registered default's own text, the way the theme layer emits every other
   token it holds; the declaration rides on the utility's properties, where the
   theme extractor collects it and the utilities layer filters it out. *)
let rec container_query_theme_decls (q : container_query) =
  let of_bracket raw =
    match container_theme_token raw with
    | None -> []
    | Some name -> (
        match Scheme.token_default name with
        | None -> []
        | Some css -> [ Css.custom_property ~layer:"theme" ("--" ^ name) css ])
  in
  match q with
  | Container_len (raw, _) | Container_len_cmp (_, raw, _) -> of_bracket raw
  | Container_size (_, inner) | Container_scoped (_, inner) ->
      container_query_theme_decls inner
  | Container_3xs | Container_2xs | Container_xs | Container_sm | Container_md
  | Container_lg | Container_xl | Container_2xl | Container_3xl | Container_4xl
  | Container_5xl | Container_6xl | Container_7xl | Container_theme _
  | Container_named _ ->
      []

(* [@sm/main] aims a size query at the container named [main]. The name is the
   tail after the last [/]; the head is any other container-query spelling. *)
(* A size the project's [@theme] declared as [--container-<name>], read the
   way the built-in scale is; the scale's own names stay built in. *)
let theme_container_size theme name =
  match container_size_of_string name with
  | Some _ -> None
  | None ->
      if
        Parse.is_valid_theme_name name
        && Option.is_some
             (Option.bind
                (Scheme.token theme ("container-" ^ name))
                Css.parse_length)
      then Some (Container_theme name)
      else None

(* Match ["<prefix>[<len>]"] and build a modifier from the parsed length. *)
let bracketed_container s prefix mk =
  let plen = String.length prefix and slen = String.length s in
  if
    String.starts_with ~prefix:(prefix ^ "[") s
    && String.ends_with ~suffix:"]" s
    && slen > plen + 2
  then
    let raw = String.sub s (plen + 1) (slen - plen - 2) in
    match parse_container_length raw with
    | Some len -> Some (mk raw len)
    | None -> None
  else None

let rec try_container_query ?(theme = Scheme.default) s =
  let bracketed = bracketed_container s in
  (* Match ["<prefix><size>"] against the named size scale, then against the
     sizes the project declared. *)
  let sized prefix cmp =
    let plen = String.length prefix in
    if String.starts_with ~prefix s && String.length s > plen then
      let name = String.sub s plen (String.length s - plen) in
      match container_size_of_string name with
      | Some q -> Some (Container (Container_size (cmp, q)))
      | None ->
          Option.map
            (fun q -> Container (Container_size (cmp, q)))
            (theme_container_size theme name)
    else None
  in
  if String.length s < 2 || s.[0] <> '@' then None
  else
    List.find_map
      (fun f -> f ())
      [
        (fun () ->
          bracketed "@" (fun raw len -> Container (Container_len (raw, len))));
        (fun () ->
          bracketed "@min-" (fun raw len ->
              Container (Container_len_cmp (Min, raw, len))));
        (fun () ->
          bracketed "@max-" (fun raw len ->
              Container (Container_len_cmp (Max, raw, len))));
        (fun () -> sized "@min-" Min);
        (fun () -> sized "@max-" Max);
        (fun () ->
          Option.map
            (fun q -> Container q)
            (theme_container_size theme (String.sub s 1 (String.length s - 1))));
        (fun () -> try_scoped_container_query ~theme s);
      ]

and try_scoped_container_query ?(theme = Scheme.default) s =
  match String.rindex_opt s '/' with
  | None -> None
  | Some i -> (
      let name = String.sub s (i + 1) (String.length s - i - 1) in
      if name = "" then None
      else
        let head = String.sub s 0 i in
        match
          match List.assoc_opt head simple_modifiers with
          | Some _ as m -> m
          | None -> try_container_query ~theme head
        with
        | Some (Container q) -> Some (Container (Container_scoped (name, q)))
        | _ -> None)

let container_query_of_token ?theme token =
  match List.assoc_opt token simple_modifiers with
  | Some (Container q) -> Some q
  | Some _ -> None
  | None -> (
      match try_container_query ?theme token with
      | Some (Container q) -> Some q
      | Some _ | None -> None)

(* The [--container-*] scale a size query reads, held the way the breakpoints
   are: a size the [@theme] block removed names nothing, under [@min-], [@max-]
   and a [/name] tail alike. *)
let rec container_size_is_defined theme = function
  | Container_size (_, inner) | Container_scoped (_, inner) ->
      container_size_is_defined theme inner
  | ( Container_3xs | Container_2xs | Container_xs | Container_sm | Container_md
    | Container_lg | Container_xl | Container_2xl | Container_3xl
    | Container_4xl | Container_5xl | Container_6xl | Container_7xl
    | Container_theme _ ) as q ->
      not (Scheme.is_removed theme ("container-" ^ Style.container_size_name q))
  | Container_named _ | Container_len _ | Container_len_cmp _ -> true

(* Parse a modifier string into a typed Style.modifier *)
let rec modifier ~(theme : Scheme.t) s : modifier option =
  let fns =
    [
      (fun () -> lookup_simple theme s);
      (fun () -> try_container_query ~theme s);
      (fun () -> try_bracketed_modifier s);
      (fun () -> try_aria_shorthand s);
      (fun () -> try_has_shorthand s);
      (fun () -> try_has_variant ~theme s);
      (fun () -> try_numeric_nth s);
      (fun () -> try_compound_named_group s);
      (fun () -> try_in_modifier s);
      (fun () -> try_not_in_modifier s);
      (fun () -> try_group_peer_not s);
      (fun () -> try_group_peer_not_variant ~theme s);
      (* Before [try_not_modifier]: a container variant handles its own [not-]
         (negating the structural condition), not the generic [Not] wrapper. *)
      (fun () -> try_container_variant theme s);
      (fun () -> try_not_modifier theme s);
      (fun () -> try_bare_data_aria s);
      (fun () -> try_prose_element s);
      (fun () -> try_custom_breakpoint (Scheme.breakpoint_names theme) s);
      (fun () -> try_custom_variant theme s);
      (* Last: a [not-] over any other variant negates the selector it produces.
         The readings above come first because several [not-] spellings need
         their own handling. *)
      (fun () -> try_not_of_modifier ~theme s);
      (fun () -> try_in_variant ~theme s);
    ]
  in
  match List.find_map (fun f -> f ()) fns with
  | Some (Container q) when not (container_size_is_defined theme q) -> None
  | m -> m

(* [group-not-has-[...]] and [peer-not-...]: the inner is any variant, read on
   its own. The reading above only knows the simple state names and a bare
   bracket. *)
and try_group_peer_not_variant ~theme s =
  let try_prefix prefix make =
    let plen = String.length prefix in
    if not (String.starts_with ~prefix s && String.length s > plen) then None
    else
      let base, name =
        split_name (String.sub s plen (String.length s - plen))
      in
      match modifier ~theme base with
      | Some m when scoped_negation_holds m -> Some (make m name)
      | Some _ | None -> None
  in
  match try_prefix "group-not-" (fun i n -> Group_not (i, n)) with
  | Some _ as r -> r
  | None -> try_prefix "peer-not-" (fun i n -> Peer_not (i, n))

(* [has-<variant>]: the argument is any variant, and that variant's own selector
   goes inside [:has()]. The shorthand reading only knows the state names and a
   bracket, so [has-peer-checked] fell through. *)
and try_has_variant ~theme s =
  if String.starts_with ~prefix:"has-" s && String.length s > 4 then
    match modifier ~theme (String.sub s 4 (String.length s - 4)) with
    | Some m when has_selector m -> Some (Has_variant m)
    | Some _ | None -> None
  else
    match try_scoped_has_variant ~theme "group-has-" s with
    | Some (m, name) -> Some (Group_has_variant (m, name))
    | None -> (
        match try_scoped_has_variant ~theme "peer-has-" s with
        | Some (m, name) -> Some (Peer_has_variant (m, name))
        | None -> None)

(* [group-has-<variant>] and [peer-has-<variant>] hold a variant the way [has-]
   does. A state name or a bracket keeps the reading [try_has_shorthand] and the
   bracket patterns give it. *)
and try_scoped_has_variant ~theme prefix s =
  let n = String.length prefix in
  if String.starts_with ~prefix s && String.length s > n then
    let base, name = split_name (String.sub s n (String.length s - n)) in
    if base = "" || base.[0] = '[' || is_has_shorthand base then None
    else
      match modifier ~theme base with
      | Some m when has_selector m -> Some (m, name)
      | Some _ | None -> None
  else None

(* A [not-] over any variant, including another negation: [not-not-md] is [md]
   again. A negation with two halves has no single negation, so [not-not-hover]
   reads as nothing, which is what Tailwind compiles for it. *)
and try_not_of_modifier ~theme s =
  if not (String.starts_with ~prefix:"not-" s && String.length s > 4) then None
  else
    match modifier ~theme (String.sub s 4 (String.length s - 4)) with
    | Some (Not inner) when involves_hover inner -> None
    | Some m when is_not_compatible m -> Some (Not m)
    | Some _ | None -> None

(* [in-<variant>]: an ancestor the variant's selector matches. The readings
   above know a bracket, a data attribute, a named group and the state names;
   any other variant with a selector reads here, as [has-] does, and
   [in-not-focus], [in-has-focus] and [in-group-hover] all read. *)
and try_in_variant ~theme s =
  if String.starts_with ~prefix:"in-" s && String.length s > 3 then
    let rest = String.sub s 3 (String.length s - 3) in
    match modifier ~theme rest with
    | Some m when has_selector m -> Some (In_state (m, rest))
    | Some _ | None -> None
  else None
