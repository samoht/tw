(** CSS rule extraction from Tailwind utilities.

    Converts modifier structures into CSS rules, handling all modifier types:
    pseudo-classes, media queries, container queries, etc. *)

module Css = Cascade.Css
open Output

(* ======================================================================== *)
(* Basic Utilities *)
(* ======================================================================== *)

let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Small memoization for escaping, as many utilities reuse the same base class
   names across modifiers. *)
let escape_cache : (string, string) Hashtbl.t = Hashtbl.create 256

let escape_class_name name =
  match Hashtbl.find_opt escape_cache name with
  | Some v -> v
  | None ->
      (* Delegate escaping to the selector printer for correctness and parity
         with the rest of the system. This covers all special characters per CSS
         rules, including ones Tailwind often uses (e.g., !, |, ^, ~, etc.) We
         strip the leading '.' from the rendered class selector. *)
      let sel = Css.Selector.class_ name in
      let rendered = Css.Selector.to_string sel in
      let escaped =
        if String.length rendered > 0 && rendered.[0] = '.' then
          String.sub rendered 1 (String.length rendered - 1)
        else rendered
      in
      Hashtbl.add escape_cache name escaped;
      escaped

(* ======================================================================== *)
(* Rule Extraction - Convert Core.t to CSS rules *)
(* ======================================================================== *)

(* Selector helpers: centralized operations for transforming selector ASTs when
   applying modifiers. This removes brittle string-based handling and keeps
   selector semantics together. *)
module Rules_selector = struct
  (* Replace every occurrence of a class name in a selector AST. *)
  let rec replace_class_in_selector ~old_class ~new_class = function
    | Css.Selector.Class cls when String.equal cls old_class ->
        Css.Selector.Class new_class
    | Css.Selector.Compound selectors ->
        Css.Selector.Compound
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Combined (a, comb, b) ->
        Css.Selector.Combined
          ( replace_class_in_selector ~old_class ~new_class a,
            comb,
            replace_class_in_selector ~old_class ~new_class b )
    | Css.Selector.Relative (comb, b) ->
        Css.Selector.Relative
          (comb, replace_class_in_selector ~old_class ~new_class b)
    | Css.Selector.List selectors ->
        Css.Selector.List
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Is selectors ->
        Css.Selector.Is
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Where selectors ->
        Css.Selector.Where
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Not selectors ->
        Css.Selector.Not
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Has selectors ->
        Css.Selector.Has
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Slotted selectors ->
        Css.Selector.Slotted
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Cue selectors ->
        Css.Selector.Cue
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Cue_region selectors ->
        Css.Selector.Cue_region
          (List.map (replace_class_in_selector ~old_class ~new_class) selectors)
    | Css.Selector.Nth_child (nth, of_) ->
        Css.Selector.Nth_child
          ( nth,
            Option.map
              (List.map (replace_class_in_selector ~old_class ~new_class))
              of_ )
    | Css.Selector.Nth_last_child (nth, of_) ->
        Css.Selector.Nth_last_child
          ( nth,
            Option.map
              (List.map (replace_class_in_selector ~old_class ~new_class))
              of_ )
    | Css.Selector.Nth_of_type (nth, of_) ->
        Css.Selector.Nth_of_type
          ( nth,
            Option.map
              (List.map (replace_class_in_selector ~old_class ~new_class))
              of_ )
    | Css.Selector.Nth_last_of_type (nth, of_) ->
        Css.Selector.Nth_last_of_type
          ( nth,
            Option.map
              (List.map (replace_class_in_selector ~old_class ~new_class))
              of_ )
    | other -> other

  (* Extract class name from a modified selector (with or without
     pseudo-class). *)
  let extract_modified_class_name modified_base_selector base_class =
    match modified_base_selector with
    | Css.Selector.Class cls -> cls
    | Css.Selector.Compound selectors ->
        List.find_map
          (function Css.Selector.Class cls -> Some cls | _ -> None)
          selectors
        |> Option.value ~default:base_class
    | _ -> base_class

  (* Transform selector by applying modifier to base class and updating
     descendants. *)
  let transform_selector_with_modifier modified_base_selector base_class
      modified_class selector =
    let replace_in_children =
      replace_class_in_selector ~old_class:base_class ~new_class:modified_class
    in
    let rec transform = function
      | Css.Selector.Class cls when String.equal cls base_class ->
          modified_base_selector
      | Css.Selector.Combined (base_sel, combinator, complex_sel) ->
          Css.Selector.Combined
            (transform base_sel, combinator, replace_in_children complex_sel)
      | Css.Selector.Compound selectors ->
          Css.Selector.Compound (List.map transform selectors)
      | Css.Selector.List selectors ->
          Css.Selector.List (List.map transform selectors)
      | other -> other
    in
    transform selector
end

let current_scheme = ref Scheme.default
let set_scheme scheme = current_scheme := scheme

let breakpoint_rem = function
  | `Sm -> 40.
  | `Md -> 48.
  | `Lg -> 64.
  | `Xl -> 80.
  | `Xl_2 -> 96.

(** Get the media condition for a breakpoint, using px from scheme if available,
    otherwise rem. *)
let breakpoint_condition bp =
  let name = string_of_breakpoint bp in
  match Scheme.breakpoint !current_scheme name with
  | Some px -> Css.Media.Min_width px
  | None -> Css.Media.Min_width_rem (breakpoint_rem bp)

(** Get the negated media condition for max-* breakpoints. *)
let breakpoint_not_condition bp =
  let name = string_of_breakpoint bp in
  match Scheme.breakpoint !current_scheme name with
  | Some px -> Css.Media.Not_min_width px
  | None -> Css.Media.Not_min_width_rem (breakpoint_rem bp)

(** Get the media condition and class prefix for a responsive modifier. *)
let responsive_modifier_condition = function
  | Style.Responsive bp ->
      let prefix = string_of_breakpoint bp in
      (breakpoint_condition bp, prefix)
  | Style.Min_responsive bp ->
      let prefix =
        match bp with
        | `Sm -> "min-sm"
        | `Md -> "min-md"
        | `Lg -> "min-lg"
        | `Xl -> "min-xl"
        | `Xl_2 -> "min-2xl"
      in
      (breakpoint_condition bp, prefix)
  | Style.Max_responsive bp ->
      let prefix =
        match bp with
        | `Sm -> "max-sm"
        | `Md -> "max-md"
        | `Lg -> "max-lg"
        | `Xl -> "max-xl"
        | `Xl_2 -> "max-2xl"
      in
      (breakpoint_not_condition bp, prefix)
  | Style.Min_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      (Css.Media.Min_width px, "min-[" ^ px_str ^ "px]")
  | Style.Max_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      (Css.Media.Not_min_width px, "max-[" ^ px_str ^ "px]")
  | Style.Min_arbitrary_length l ->
      let len_str = Modifiers.compact_length l in
      (Css.media_min_width_length l, "min-[" ^ len_str ^ "]")
  | Style.Max_arbitrary_length l ->
      let len_str = Modifiers.compact_length l in
      (Css.media_not_min_width_length l, "max-[" ^ len_str ^ "]")
  | Style.Custom_responsive name ->
      let px =
        match Scheme.breakpoint !current_scheme name with
        | Some px -> px
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.Media.Min_width px, name)
  | Style.Min_custom name ->
      let px =
        match Scheme.breakpoint !current_scheme name with
        | Some px -> px
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.Media.Min_width px, "min-" ^ name)
  | Style.Max_custom name ->
      let px =
        match Scheme.breakpoint !current_scheme name with
        | Some px -> px
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.Media.Not_min_width px, "max-" ^ name)
  | _ -> failwith "not a responsive modifier"

let selector_with_data_key selector key value =
  let attr_selector = Css.Selector.attribute key (Exact value) in
  Css.Selector.combine selector Descendant attr_selector

let responsive_rule breakpoint base_class selector props =
  let prefix = string_of_breakpoint breakpoint in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query
    ~condition:(breakpoint_condition breakpoint)
    ~selector:new_selector ~props ~base_class:modified_class ()

let min_responsive_rule breakpoint base_class selector props =
  let prefix =
    match breakpoint with
    | `Sm -> "min-sm"
    | `Md -> "min-md"
    | `Lg -> "min-lg"
    | `Xl -> "min-xl"
    | `Xl_2 -> "min-2xl"
  in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query
    ~condition:(breakpoint_condition breakpoint)
    ~selector:new_selector ~props ~base_class:modified_class ()

let max_responsive_rule breakpoint base_class selector props =
  let prefix =
    match breakpoint with
    | `Sm -> "max-sm"
    | `Md -> "max-md"
    | `Lg -> "max-lg"
    | `Xl -> "max-xl"
    | `Xl_2 -> "max-2xl"
  in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query
    ~condition:(breakpoint_not_condition breakpoint)
    ~selector:new_selector ~props ~base_class:modified_class ()

let min_arbitrary_rule px base_class selector props =
  let px_str =
    if Float.is_integer px then Int.to_string (Float.to_int px)
    else Float.to_string px
  in
  let prefix = "min-[" ^ px_str ^ "px]" in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Min_width px) ~selector:new_selector ~props
    ~base_class:modified_class ()

let max_arbitrary_rule px base_class selector props =
  let px_str =
    if Float.is_integer px then Int.to_string (Float.to_int px)
    else Float.to_string px
  in
  let prefix = "max-[" ^ px_str ^ "px]" in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Not_min_width px) ~selector:new_selector
    ~props ~base_class:modified_class ()

let min_arbitrary_length_rule l base_class selector props =
  let len_str = Modifiers.compact_length l in
  let prefix = "min-[" ^ len_str ^ "]" in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query
    ~condition:(Css.media_min_width_length l)
    ~selector:new_selector ~props ~base_class:modified_class ()

let max_arbitrary_length_rule l base_class selector props =
  let len_str = Modifiers.compact_length l in
  let prefix = "max-[" ^ len_str ^ "]" in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query
    ~condition:(Css.media_not_min_width_length l)
    ~selector:new_selector ~props ~base_class:modified_class ()

let custom_responsive_rule name base_class selector props =
  let px =
    match Scheme.breakpoint !current_scheme name with
    | Some px -> px
    | None -> failwith ("unknown custom breakpoint: " ^ name)
  in
  let modified_class = name ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Min_width px) ~selector:new_selector ~props
    ~base_class:modified_class ()

let min_custom_rule name base_class selector props =
  let px =
    match Scheme.breakpoint !current_scheme name with
    | Some px -> px
    | None -> failwith ("unknown custom breakpoint: " ^ name)
  in
  let modified_class = "min-" ^ name ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Min_width px) ~selector:new_selector ~props
    ~base_class:modified_class ()

let max_custom_rule name base_class selector props =
  let px =
    match Scheme.breakpoint !current_scheme name with
    | Some px -> px
    | None -> failwith ("unknown custom breakpoint: " ^ name)
  in
  let modified_class = "max-" ^ name ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Not_min_width px) ~selector:new_selector
    ~props ~base_class:modified_class ()

let container_rule query base_class selector props =
  let prefix = Containers.container_query_to_class_prefix query in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition = Containers.container_query_to_condition query in
  container_query ~condition ~selector:new_selector ~props ~base_class ()

(** Preprocess a has-[...] selector string for CSS parsing. Replaces & with *
    (nesting reference) and ensures combinators (+, >, ~) have proper spacing.
*)
let preprocess_has_selector s =
  let buf = Buffer.create (String.length s + 4) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    (match s.[!i] with
    | '&' -> Buffer.add_char buf '*'
    | ('+' | '>' | '~') as c ->
        (* Add space before combinator if not already present *)
        if
          Buffer.length buf = 0
          || Buffer.contents buf |> fun b -> b.[String.length b - 1] <> ' '
        then Buffer.add_char buf ' ';
        Buffer.add_char buf c;
        (* Add space after combinator if not already present *)
        if !i + 1 < len && s.[!i + 1] <> ' ' then Buffer.add_char buf ' '
    | c -> Buffer.add_char buf c);
    incr i
  done;
  Buffer.contents buf

let has_like_selector kind ?name ?shorthand ~not_order selector_str base_class
    props =
  let open Css.Selector in
  let processed = preprocess_has_selector selector_str in
  let reader = Css.Reader.of_string processed in
  let parsed_selector = Css.Selector.read_relative reader in
  let has_part s =
    match shorthand with Some sh -> sh | None -> "[" ^ s ^ "]"
  in
  match kind with
  | `Has ->
      let class_name = "has-" ^ has_part selector_str ^ ":" ^ base_class in
      let sel = compound [ class_ class_name; has [ parsed_selector ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Group_has ->
      let name_suffix = match name with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-has-" ^ has_part selector_str ^ name_suffix ^ ":" ^ base_class
      in
      let group_class =
        match name with Some n -> "group/" ^ n | None -> "group"
      in
      let rel =
        combine
          (compound [ where [ Class group_class ]; has [ parsed_selector ] ])
          Descendant universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Peer_has ->
      let name_suffix = match name with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-has-" ^ has_part selector_str ^ name_suffix ^ ":" ^ base_class
      in
      let peer_class =
        match name with Some n -> "peer/" ^ n | None -> "peer"
      in
      let rel =
        combine
          (compound [ where [ Class peer_class ]; has [ parsed_selector ] ])
          Subsequent_sibling universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()

(* Pseudo-class modifiers: transform the base selector and mark hover when
   needed. *)
let handle_pseudo_class_modifier ?(inner_has_hover = false) modifier base_class
    selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  let has_hover = Modifiers.is_hover modifier in
  if has_hover && inner_has_hover then
    (* Nested hover: wrap in @media (hover:hover) { @media (hover:hover) { }
       } *)
    let inner_rule = Css.rule ~selector:new_selector props in
    let inner_media = Css.media ~condition:Css.Media.Hover [ inner_rule ] in
    media_query ~condition:Css.Media.Hover ~selector:new_selector ~props:[]
      ~base_class:modified_class ~nested:[ inner_media ] ()
  else
    regular ~selector:new_selector ~props ~base_class:modified_class ~has_hover
      ()

(** Handle data attribute modifiers (data-state, data-variant, etc.) *)
let handle_data_modifier key value selector props base_class =
  regular
    ~selector:(selector_with_data_key selector ("data-" ^ key) value)
    ~props ~base_class ()

(* Media-like modifiers (dark, motion/contrast prefs) should transform the
   existing selector structure rather than rebuilding a flat class selector.
   When the inner rule has_hover, we need nested media queries to match
   Tailwind's structure: @media (condition) { @media (hover:hover) { ... } } *)
let handle_media_like_modifier (modifier : Style.modifier)
    ~(condition : Css.Media.t) ?(inner_has_hover = false) base_class selector
    props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  if inner_has_hover then
    (* For compound dark:hover: case, generate a Media_query with nested hover
       media. This allows the optimizer to group these rules together. The
       selector already has :hover from the inner rule transformation - do NOT
       add another one. *)
    let hover_selector =
      Rules_selector.transform_selector_with_modifier modified_base_selector
        base_class modified_class selector
    in
    (* Nested @media (hover:hover) { .dark\:hover\:X:hover { props } } *)
    let inner_hover_media =
      Css.media ~condition:Css.Media.Hover
        [ Css.rule ~selector:hover_selector props ]
    in
    media_query ~condition ~selector:hover_selector ~props:[]
      ~base_class:modified_class ~nested:[ inner_hover_media ] ()
  else
    let new_selector =
      Rules_selector.transform_selector_with_modifier modified_base_selector
        base_class modified_class selector
    in
    media_query ~condition ~selector:new_selector ~props
      ~base_class:modified_class ()

(* Route data attribute modifiers *)
let route_data_modifier modifier base_class selector props =
  match modifier with
  | Style.Data_state v ->
      handle_data_modifier "state" v selector props base_class
  | Style.Data_variant v ->
      handle_data_modifier "variant" v selector props base_class
  | Style.Data_active ->
      handle_data_modifier "active" "" selector props base_class
  | Style.Data_inactive ->
      handle_data_modifier "inactive" "" selector props base_class
  | Style.Data_custom (k, v) ->
      handle_data_modifier k v selector props base_class
  | _ -> regular ~selector ~props ~base_class ()

(* Parse a data bracket expression into attribute name, match operator, and
   optional flag. Handles $=, ^=, *=, ~=, |= operators and trailing i/s flags.
   Underscores in the value part are converted to spaces. *)
(* Parse trailing case-sensitivity flag and strip surrounding quotes *)
let parse_value_and_flag value_str =
  let vlen = String.length value_str in
  let value_str, flag =
    if vlen >= 2 && value_str.[vlen - 1] = 'i' && value_str.[vlen - 2] = ' '
    then
      ( String.trim (String.sub value_str 0 (vlen - 2)),
        Some Css.Selector.Case_insensitive )
    else if
      vlen >= 2 && value_str.[vlen - 1] = 's' && value_str.[vlen - 2] = ' '
    then
      ( String.trim (String.sub value_str 0 (vlen - 2)),
        Some Css.Selector.Case_sensitive )
    else (value_str, None)
  in
  let value =
    let vlen = String.length value_str in
    if vlen >= 2 then
      match (value_str.[0], value_str.[vlen - 1]) with
      | '"', '"' | '\'', '\'' -> String.sub value_str 1 (vlen - 2)
      | _ -> value_str
    else value_str
  in
  (value, flag)

let parse_data_expr raw_expr =
  (* Find the match operator in raw content (before underscore conversion) *)
  let len = String.length raw_expr in
  let in_quotes = ref false in
  let rec find_op i =
    if i >= len then None
    else
      match raw_expr.[i] with
      | '"' | '\'' ->
          in_quotes := not !in_quotes;
          find_op (i + 1)
      | ('$' | '^' | '*' | '~' | '|')
        when (not !in_quotes) && i + 1 < len && raw_expr.[i + 1] = '=' ->
          Some (i, 2, raw_expr.[i])
      | '=' when not !in_quotes -> Some (i, 1, '=')
      | _ -> find_op (i + 1)
  in
  let underscore_to_space s =
    String.trim (String.map (fun c -> if c = '_' then ' ' else c) s)
  in
  match find_op 0 with
  | None -> ("data-" ^ underscore_to_space raw_expr, Css.Selector.Presence, None)
  | Some (op_pos, op_len, op_char) ->
      let attr = underscore_to_space (String.sub raw_expr 0 op_pos) in
      let value_str =
        underscore_to_space
          (String.sub raw_expr (op_pos + op_len) (len - op_pos - op_len))
      in
      let value, flag = parse_value_and_flag value_str in
      let match_op =
        match op_char with
        | '$' -> Css.Selector.Suffix value
        | '^' -> Css.Selector.Prefix value
        | '*' -> Css.Selector.Substring value
        | '~' -> Css.Selector.Whitespace_list value
        | '|' -> Css.Selector.Hyphen_list value
        | _ -> Css.Selector.Exact value
      in
      ("data-" ^ attr, match_op, flag)

(* Known data shorthand names *)
let _is_data_shorthand_name = function
  | "disabled" | "active" | "inactive" -> true
  | _ -> false

(* Route data bracket variants to appropriate handler *)
let route_data_bracket_modifier modifier base_class props =
  let kind, raw_str, name_opt =
    match modifier with
    | Style.Data_bracket s -> (`Data, s, None)
    | Style.Group_data (s, n) -> (`Group_data, s, n)
    | Style.Peer_data (s, n) -> (`Peer_data, s, n)
    | _ -> failwith "Invalid data bracket modifier"
  in
  let attr_name, attr_match, attr_flag = parse_data_expr raw_str in
  let open Css.Selector in
  let class_part = "[" ^ raw_str ^ "]" in
  let not_order = 20 in
  match kind with
  | `Data ->
      let class_name = "data-" ^ class_part ^ ":" ^ base_class in
      let sel =
        compound
          [ class_ class_name; attribute ?flag:attr_flag attr_name attr_match ]
      in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Group_data ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-data-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let group_class =
        match name_opt with Some n -> "group/" ^ n | None -> "group"
      in
      let rel =
        combine
          (compound
             [
               where [ Class group_class ];
               attribute ?flag:attr_flag attr_name attr_match;
             ])
          Descendant universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Peer_data ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-data-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let peer_class =
        match name_opt with Some n -> "peer/" ^ n | None -> "peer"
      in
      let rel =
        combine
          (compound
             [
               where [ Class peer_class ];
               attribute ?flag:attr_flag attr_name attr_match;
             ])
          Subsequent_sibling universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()

(* Resolve has-shorthand names to CSS selector strings *)
let resolve_has_shorthand = function
  | "checked" -> ":checked"
  | "hocus" -> ":hover, :focus"
  | s -> s

(* Route :has() variants to appropriate handler *)
let route_has_modifier modifier base_class props =
  let kind, raw_str, name =
    match modifier with
    | Style.Has s -> (`Has, s, None)
    | Style.Group_has (s, name) -> (`Group_has, s, name)
    | Style.Peer_has (s, name) -> (`Peer_has, s, name)
    | _ -> failwith "Invalid has modifier"
  in
  (* Shorthand forms like "checked" are stored without colon prefix. Bracket
     forms like ":checked" start with colon or other CSS chars. *)
  let is_shorthand =
    raw_str <> ""
    && raw_str.[0] <> ':'
    && raw_str.[0] <> '&'
    && raw_str.[0] <> '+'
    && raw_str.[0] <> '>'
    && raw_str.[0] <> '~'
    && raw_str.[0] <> '.'
    && not (String.contains raw_str ' ')
  in
  let selector_str = resolve_has_shorthand raw_str in
  let shorthand = if is_shorthand then Some raw_str else None in
  (* Ordering: shorthands before brackets; pseudo-class brackets before
     combinator brackets. Named vs unnamed ordering handled by
     normalize_for_sort since '/' maps to '|' which sorts after ':' → '!'. *)
  let base_order =
    if is_shorthand then 10
    else if raw_str <> "" && raw_str.[0] = ':' then 20
    else 30
  in
  let not_order = base_order in
  has_like_selector kind ?name ?shorthand ~not_order selector_str base_class
    props

(* Parse an aria expression string into an attribute name and match. "modal" →
   ("aria-modal", Presence) "valuenow=1" → ("aria-valuenow", Exact "1")
   "invalid=spelling" → ("aria-invalid", Exact "spelling") Underscores in values
   are replaced with spaces. *)
let parse_aria_expr expr =
  let expr = String.map (fun c -> if c = '_' then ' ' else c) expr in
  let expr = String.trim expr in
  match String.index_opt expr '=' with
  | None -> ("aria-" ^ expr, Css.Selector.Presence)
  | Some i ->
      let attr = String.trim (String.sub expr 0 i) in
      let raw_value =
        String.trim (String.sub expr (i + 1) (String.length expr - i - 1))
      in
      (* Strip surrounding quotes if present *)
      let value =
        let len = String.length raw_value in
        if len >= 2 && raw_value.[0] = '"' && raw_value.[len - 1] = '"' then
          String.sub raw_value 1 (len - 2)
        else raw_value
      in
      ("aria-" ^ attr, Css.Selector.Exact value)

(* Route aria variants to appropriate handler *)
let route_aria_modifier modifier base_class props =
  let kind, raw_str, name_opt =
    match modifier with
    | Style.Aria_bracket s -> (`Aria, s, None)
    | Style.Group_aria (s, n) -> (`Group_aria, s, n)
    | Style.Peer_aria (s, n) -> (`Peer_aria, s, n)
    | _ -> failwith "Invalid aria modifier"
  in
  let open Css.Selector in
  let is_shorthand = Modifiers.is_aria_shorthand raw_str in
  let aria_attr, aria_match =
    if is_shorthand then ("aria-" ^ raw_str, Exact "true")
    else parse_aria_expr raw_str
  in
  let class_part = if is_shorthand then raw_str else "[" ^ raw_str ^ "]" in
  let not_order = if is_shorthand then 10 else 20 in
  match kind with
  | `Aria ->
      let class_name = "aria-" ^ class_part ^ ":" ^ base_class in
      let sel =
        compound [ class_ class_name; attribute aria_attr aria_match ]
      in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Group_aria ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-aria-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let group_class =
        match name_opt with Some n -> "group/" ^ n | None -> "group"
      in
      let rel =
        combine
          (compound
             [ where [ Class group_class ]; attribute aria_attr aria_match ])
          Descendant universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()
  | `Peer_aria ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-aria-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let peer_class =
        match name_opt with Some n -> "peer/" ^ n | None -> "peer"
      in
      let rel =
        combine
          (compound
             [ where [ Class peer_class ]; attribute aria_attr aria_match ])
          Subsequent_sibling universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ~not_order ()

(* Handle fallback for unmatched modifiers. Must extract modified_class so that
   outer modifiers like dark: can properly transform the selector. *)
let handle_fallback_modifier ?(inner_has_hover = false) modifier base_class
    selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  let has_hover = Modifiers.is_hover modifier || inner_has_hover in
  regular ~selector:new_selector ~props ~base_class:modified_class ~has_hover ()

(** Normalize a supports condition string into a valid CSS [@supports]
    condition. Converts underscores to spaces and wraps in parens as needed. *)
let normalize_supports_condition condition_str =
  (* Convert underscores to spaces (Tailwind bracket notation) *)
  let cond = String.map (fun c -> if c = '_' then ' ' else c) condition_str in
  if
    String.length cond > 2
    && cond.[0] = '-'
    && cond.[1] = '-'
    && not (String.contains cond ':')
  then
    (* Bare custom property: --test → (--test: var(--tw)) *)
    "(" ^ cond ^ ": var(--tw))"
  else if cond <> "" && cond.[0] = '(' then
    (* Already wrapped in parens *)
    cond
  else if String.contains cond ':' then
    (* Property: value → wrap in parens *)
    "(" ^ cond ^ ")"
  else
    (* Function call like font-format(opentype) or var(--test) *)
    cond

(** Handle [@supports] modifier: builds modified class name, updates selector,
    normalizes condition, and emits a supports query rule. *)
let handle_supports_modifier condition_str base_class selector props =
  (* Use shorthand class name for supports-<property> patterns, otherwise use
     bracket notation *)
  let modified_class =
    if String.ends_with ~suffix:": var(--tw)" condition_str then
      let prop_len = String.length condition_str - 11 in
      "supports-" ^ String.sub condition_str 0 prop_len ^ ":" ^ base_class
    else "supports-[" ^ condition_str ^ "]:" ^ base_class
  in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition_input = normalize_supports_condition condition_str in
  let condition = Css.Supports.of_string condition_input in
  supports_query ~condition ~selector:new_selector ~props
    ~base_class:modified_class ()

(** Map a media-like modifier to its corresponding Css.Media.t condition.
    Returns [Some condition] for modifiers that map to media queries, [None] for
    non-media modifiers. *)
let media_condition_of_modifier = function
  | Style.Dark -> Some (Css.Media.Prefers_color_scheme `Dark)
  | Style.Motion_safe -> Some (Css.Media.Prefers_reduced_motion `No_preference)
  | Style.Motion_reduce -> Some (Css.Media.Prefers_reduced_motion `Reduce)
  | Style.Contrast_more -> Some (Css.Media.Prefers_contrast `More)
  | Style.Contrast_less -> Some (Css.Media.Prefers_contrast `Less)
  | Style.Print -> Some Css.Media.Print
  | Style.Portrait -> Some (Css.Media.Orientation `Portrait)
  | Style.Landscape -> Some (Css.Media.Orientation `Landscape)
  | Style.Forced_colors -> Some (Css.Media.Forced_colors `Active)
  | Style.Inverted_colors -> Some (Css.Media.Inverted_colors `Inverted)
  | Style.Pointer_none -> Some (Css.Media.Pointer `None)
  | Style.Pointer_coarse -> Some (Css.Media.Pointer `Coarse)
  | Style.Pointer_fine -> Some (Css.Media.Pointer `Fine)
  | Style.Any_pointer_none -> Some (Css.Media.Any_pointer `None)
  | Style.Any_pointer_coarse -> Some (Css.Media.Any_pointer `Coarse)
  | Style.Any_pointer_fine -> Some (Css.Media.Any_pointer `Fine)
  | Style.Noscript -> Some (Css.Media.Scripting `None)
  | _ -> None

(** Variant order for not-* inner modifiers. Returns a large offset that encodes
    the inner modifier's position in the Tailwind v4 variant order. This ensures
    not-* rules sort by variant position, not alphabetically. The offset is
    multiplied by 100 to leave room for the base suborder. *)

(** Check if string [s] contains substring [pat]. *)
let has_substring s pat =
  let slen = String.length s and plen = String.length pat in
  let rec check i =
    i + plen <= slen && (String.sub s i plen = pat || check (i + 1))
  in
  check 0

(** Compute variant_order from base_class and selector. Extracts the modifier
    prefix from base_class (everything before the last ":") and maps it to a
    variant order number. For before/after, the base_class is the raw utility
    name without prefix, so we detect them from the selector content. *)
let compute_variant_order base_class selector =
  let from_base_class bc =
    match String.rindex_opt bc ':' with
    | Some i -> (
        let prefix = String.sub bc 0 i in
        let vo = Modifiers.variant_order_of_prefix prefix in
        if vo > 0 then vo
        else
          (* For compound prefixes like "dark:group-focus", try the outermost
             modifier (before the first colon in the prefix). *)
          match String.index_opt prefix ':' with
          | Some j -> Modifiers.variant_order_of_prefix (String.sub prefix 0 j)
          | None -> 0)
    | None -> 0
  in
  let vo = match base_class with None -> 0 | Some bc -> from_base_class bc in
  (* If no variant_order from base_class, check selector for modifier-based
     pseudo-elements (before:/after: modifiers). Only detect when the selector
     class name contains the escaped modifier prefix (e.g., "before\:absolute")
     to avoid matching utility-generated pseudo-elements like prose's
     ::before. *)
  if vo > 0 then vo
  else
    let sel_str = Css.Selector.to_string selector in
    if has_substring sel_str "before\\:" then 1600
    else if has_substring sel_str "after\\:" then 1601
    else 0

(** Build the class name prefix for a not-* inner modifier. Handles shorthand
    forms like data-foo, has-checked, nth-2 that need different class names than
    their pp_modifier representation. *)
let not_class_prefix inner_modifier =
  match inner_modifier with
  | Style.Data_custom (attr, "") -> "data-" ^ attr
  | Style.Has pseudo_str
    when String.length pseudo_str > 0 && pseudo_str.[0] = ':' ->
      "has-" ^ String.sub pseudo_str 1 (String.length pseudo_str - 1)
  | Style.Has shorthand_name -> "has-" ^ shorthand_name
  | Style.Nth expr -> Style.pp_nth "nth" expr
  | Style.Nth_last expr -> Style.pp_nth "nth-last" expr
  | Style.Nth_of_type expr -> Style.pp_nth "nth-of-type" expr
  | Style.Nth_last_of_type expr -> Style.pp_nth "nth-last-of-type" expr
  | Style.Supports cond when String.ends_with ~suffix:": var(--tw)" cond ->
      let prop_len = String.length cond - 11 in
      "supports-" ^ String.sub cond 0 prop_len
  | inner -> Modifiers.pp_modifier inner

(** Extract the pseudo-class selector(s) from a modifier for use in :not().
    Returns a list of selectors that go inside :not(sel1, sel2, ...). *)
let extract_not_conditions inner_modifier base_class =
  match inner_modifier with
  | Style.Hocus | Style.Device_hocus ->
      [ Css.Selector.Hover; Css.Selector.Focus ]
  | Style.Has selector_str ->
      let pseudo_str =
        if String.length selector_str > 0 && selector_str.[0] = ':' then
          String.sub selector_str 1 (String.length selector_str - 1)
        else selector_str
      in
      let inner_sel =
        match pseudo_str with
        | "checked" -> Css.Selector.Checked
        | "hover" -> Css.Selector.Hover
        | "focus" -> Css.Selector.Focus
        | "disabled" -> Css.Selector.Disabled
        | _ -> Css.Selector.Class (":" ^ pseudo_str)
      in
      [ Css.Selector.Has [ inner_sel ] ]
  | Style.Data_custom (attr, "") ->
      [ Css.Selector.attribute ("data-" ^ attr) Presence ]
  | Style.Data_custom (attr, value) ->
      [ Css.Selector.attribute ("data-" ^ attr) (Exact value) ]
  | Style.Aria_selected ->
      [ Css.Selector.attribute "aria-selected" (Exact "true") ]
  | Style.Aria_checked ->
      [ Css.Selector.attribute "aria-checked" (Exact "true") ]
  | Style.Aria_expanded ->
      [ Css.Selector.attribute "aria-expanded" (Exact "true") ]
  | Style.Aria_disabled ->
      [ Css.Selector.attribute "aria-disabled" (Exact "true") ]
  | _ -> (
      (* Generic extraction: get selector from to_selector and strip the leading
         Class element to get just the pseudo-class part *)
      let sel = Modifiers.to_selector inner_modifier base_class in
      match sel with
      | Css.Selector.Compound (Css.Selector.Class _ :: rest) when rest <> [] ->
          rest
      | _ -> [ sel ])

(** Build a regular rule with :not() selector for a not-* modifier. *)
let not_selector_rule ?(not_order = 0) inner_modifier modified_class base_class
    props =
  let conditions = extract_not_conditions inner_modifier base_class in
  let not_sel = Css.Selector.Not conditions in
  regular ~not_order
    ~selector:
      (Css.Selector.compound [ Css.Selector.Class modified_class; not_sel ])
    ~props ~base_class:modified_class ()

(* Create a single not-media rule *)
let not_media_rule ~nvo ~condition modified_class props =
  [
    media_query ~not_order:nvo ~condition
      ~selector:(Css.Selector.Class modified_class) ~props
      ~base_class:modified_class ();
  ]

(** Handle :not() pseudo-class modifier: dispatches to the right rule type based
    on the inner modifier. Returns a list of rules since some modifiers (like
    hover) produce both a selector rule and a media rule. *)
let handle_not_modifier inner_modifier base_class _selector props =
  let modified_class =
    "not-" ^ not_class_prefix inner_modifier ^ ":" ^ base_class
  in
  let nvo = Modifiers.not_variant_order inner_modifier in
  let sel_rule () =
    not_selector_rule ~not_order:nvo inner_modifier modified_class base_class
      props
  in
  let not_hover_media () =
    not_media_rule ~nvo ~condition:(Css.Media.Negated Css.Media.Hover)
      modified_class props
  in
  match inner_modifier with
  | Style.Hover -> [ sel_rule () ] @ not_hover_media ()
  | Style.Device_hocus -> [ sel_rule () ] @ not_hover_media ()
  | Style.Hocus -> [ sel_rule () ]
  | _ when Option.is_some (media_condition_of_modifier inner_modifier) ->
      let condition = Option.get (media_condition_of_modifier inner_modifier) in
      not_media_rule ~nvo ~condition:(Css.Media.Negated condition)
        modified_class props
  | Style.Supports condition_str ->
      let condition_input = normalize_supports_condition condition_str in
      let inner_condition = Css.Supports.of_string condition_input in
      [
        supports_query ~not_order:nvo
          ~condition:(Css.Supports.Not inner_condition)
          ~selector:(Css.Selector.Class modified_class) ~props
          ~base_class:modified_class ();
      ]
  | Style.Responsive bp | Style.Min_responsive bp ->
      not_media_rule ~nvo
        ~condition:(breakpoint_not_condition bp)
        modified_class props
  | Style.Max_responsive bp ->
      not_media_rule ~nvo ~condition:(breakpoint_condition bp) modified_class
        props
  | Style.Min_arbitrary px ->
      not_media_rule ~nvo ~condition:(Css.Media.Not_min_width px) modified_class
        props
  | Style.Max_arbitrary px ->
      not_media_rule ~nvo ~condition:(Css.Media.Min_width px) modified_class
        props
  | Style.Min_arbitrary_length l ->
      not_media_rule ~nvo
        ~condition:(Css.media_not_min_width_length l)
        modified_class props
  | Style.Max_arbitrary_length l ->
      not_media_rule ~nvo
        ~condition:(Css.media_min_width_length l)
        modified_class props
  | _ -> [ sel_rule () ]

(** Parse a bracket media condition string (from not-[@media...]) and return the
    appropriate negated media condition. Handles double negation: not of "not
    (cond)" → positive cond. *)
let parse_bracket_media content =
  (* Convert underscores to spaces (Tailwind bracket convention) *)
  let s = String.map (fun c -> if c = '_' then ' ' else c) content in
  (* Strip @media prefix *)
  let rest =
    String.trim
      (if String.length s > 7 && String.sub s 0 7 = "@media " then
         String.sub s 7 (String.length s - 7)
       else if String.length s > 6 && String.sub s 0 6 = "@media" then
         String.sub s 6 (String.length s - 6)
       else s)
  in
  (* Check for "not" prefix (double negation → positive) *)
  if String.length rest > 4 && String.sub rest 0 4 = "not " then
    let inner = String.trim (String.sub rest 4 (String.length rest - 4)) in
    (* Double negation: return the positive condition *)
    match inner with
    | "(orientation: portrait)" | "(orientation:portrait)" ->
        Css.Media.Orientation `Portrait
    | "(orientation: landscape)" | "(orientation:landscape)" ->
        Css.Media.Orientation `Landscape
    | _ -> Css.Media.Raw inner
  else
    (* Negate the condition *)
    match rest with
    | "print" -> Css.Media.Negated Css.Media.Print
    | "(orientation: portrait)" | "(orientation:portrait)" ->
        Css.Media.Negated (Css.Media.Orientation `Portrait)
    | "(orientation: landscape)" | "(orientation:landscape)" ->
        Css.Media.Negated (Css.Media.Orientation `Landscape)
    | "(hover: hover)" | "(hover:hover)" -> Css.Media.Negated Css.Media.Hover
    | _ -> Css.Media.Negated (Css.Media.Raw rest)

(** Parse a bracket pseudo-class string into a CSS selector. *)
let parse_bracket_pseudo content =
  match content with
  | ":checked" -> Css.Selector.Checked
  | ":hover" -> Css.Selector.Hover
  | ":focus" -> Css.Selector.Focus
  | ":active" -> Css.Selector.Active
  | ":disabled" -> Css.Selector.Disabled
  | ":first-child" -> Css.Selector.First_child
  | ":last-child" -> Css.Selector.Last_child
  | ":focus-within" -> Css.Selector.Focus_within
  | ":focus-visible" -> Css.Selector.Focus_visible
  | _ -> Css.Selector.Class content

(** Parse in-[...] bracket content into an ancestor selector. Class selectors
    (starting with .) are used directly; others are wrapped in :is(). *)
let in_bracket_ancestor content =
  if content <> "" && content.[0] = '.' then
    (* Class selector: .group → Class "group" inside :where() *)
    let cls = String.sub content 1 (String.length content - 1) in
    Css.Selector.Class cls
  else
    (* Element or other selector: p → :is(p) inside :where() *)
    Css.Selector.Is [ Css.Selector.Element (None, content) ]

(** Handle in-[...] bracket modifier. Returns a list of rules. *)
let handle_in_bracket content base_class props =
  let modified_class = "in-[" ^ content ^ "]:" ^ base_class in
  let ancestor = in_bracket_ancestor content in
  let sel =
    Css.Selector.combine (Css.Selector.Where [ ancestor ])
      Css.Selector.Descendant (Css.Selector.Class modified_class)
  in
  [
    regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in"
      ~not_order:300 ();
  ]

(** Handle in-data-X modifier. Returns a list of rules. *)
let handle_in_data attr base_class props =
  let modified_class = "in-data-" ^ attr ^ ":" ^ base_class in
  let sel =
    Css.Selector.combine
      (Css.Selector.Where
         [
           Css.Selector.Attribute (None, Data attr, Css.Selector.Presence, None);
         ])
      Css.Selector.Descendant (Css.Selector.Class modified_class)
  in
  [
    regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in"
      ~not_order:200 ();
  ]

(** Handle not-in-[...] bracket modifier. Returns a list of rules. *)
let handle_not_in_bracket content base_class props =
  let modified_class = "not-in-[" ^ content ^ "]:" ^ base_class in
  let ancestor = in_bracket_ancestor content in
  let sel =
    Css.Selector.compound
      [
        Css.Selector.Class modified_class;
        Css.Selector.Not
          [
            Css.Selector.combine (Css.Selector.Where [ ancestor ])
              Css.Selector.Descendant (Css.Selector.Universal None);
          ];
      ]
  in
  [
    regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in"
      ~not_order:100 ();
  ]

(** Handle not-[...] bracket modifier. Returns a list of rules. *)
let handle_not_bracket content base_class props =
  let modified_class = "not-[" ^ content ^ "]:" ^ base_class in
  let nvo = Modifiers.not_variant_order (Style.Not_bracket content) in
  if
    (String.length content > 6 && String.sub content 0 6 = "@media")
    || (String.length content > 7 && String.sub content 0 7 = "@media_")
  then
    (* Media bracket pattern: not-[@media...] → negated media query *)
    let condition = parse_bracket_media content in
    [
      media_query ~not_order:nvo ~condition
        ~selector:(Css.Selector.Class modified_class) ~props
        ~base_class:modified_class ();
    ]
  else if content <> "" && content.[0] = ':' then
    (* Pseudo-class bracket: not-[:checked] → :not(:checked) *)
    let pseudo = parse_bracket_pseudo content in
    [
      regular ~not_order:nvo
        ~selector:
          (Css.Selector.compound
             [ Css.Selector.Class modified_class; Css.Selector.Not [ pseudo ] ])
        ~props ~base_class:modified_class ();
    ]
  else
    (* Unknown bracket content - treat as raw selector *)
    [
      regular ~not_order:nvo
        ~selector:
          (Css.Selector.compound
             [
               Css.Selector.Class modified_class;
               Css.Selector.Not [ Css.Selector.Class content ];
             ])
        ~props ~base_class:modified_class ();
    ]

(** Handle group-not-X modifier. Produces selector with
    :is(:where(.group):not(...) descendant) pattern. *)
let handle_group_not_modifier inner name_opt base_class props =
  let inner_str =
    match inner with
    | Style.Not_bracket content -> "[" ^ content ^ "]"
    | m -> Modifiers.pp_modifier m
  in
  let name_suffix = match name_opt with None -> "" | Some n -> "/" ^ n in
  let modified_class =
    "group-not-" ^ inner_str ^ name_suffix ^ ":" ^ base_class
  in
  let nvo = Modifiers.not_variant_order (Style.Group_not (inner, name_opt)) in
  (* Check if inner modifier is media-based — those produce no output in group
     context *)
  let is_media_inner =
    match inner with
    | Style.Hover | Style.Device_hocus -> true
    | _ -> Option.is_some (media_condition_of_modifier inner)
  in
  if is_media_inner then []
  else
    let group_class =
      match name_opt with
      | None -> Css.Selector.Class "group"
      | Some name -> Css.Selector.Class ("group/" ^ name)
    in
    (* Get the pseudo conditions for :not() *)
    let not_conditions =
      match inner with
      | Style.Not_bracket content when content <> "" && content.[0] = ':' ->
          [ parse_bracket_pseudo content ]
      | _ -> extract_not_conditions inner base_class
    in
    let open Css.Selector in
    let rel =
      combine
        (compound [ where [ group_class ]; Not not_conditions ])
        Descendant universal
    in
    [
      regular ~not_order:nvo
        ~selector:(compound [ Class modified_class; is_ [ rel ] ])
        ~props ~base_class:modified_class ();
    ]

(** Handle peer-not-X modifier. Produces selector with
    :is(:where(.peer):not(...) sibling) pattern. *)
let handle_peer_not_modifier inner name_opt base_class props =
  let inner_str =
    match inner with
    | Style.Not_bracket content -> "[" ^ content ^ "]"
    | m -> Modifiers.pp_modifier m
  in
  let name_suffix = match name_opt with None -> "" | Some n -> "/" ^ n in
  let modified_class =
    "peer-not-" ^ inner_str ^ name_suffix ^ ":" ^ base_class
  in
  let nvo = Modifiers.not_variant_order (Style.Peer_not (inner, name_opt)) in
  let is_media_inner =
    match inner with
    | Style.Hover | Style.Device_hocus -> true
    | _ -> Option.is_some (media_condition_of_modifier inner)
  in
  if is_media_inner then []
  else
    let peer_class =
      match name_opt with
      | None -> Css.Selector.Class "peer"
      | Some name -> Css.Selector.Class ("peer/" ^ name)
    in
    let not_conditions =
      match inner with
      | Style.Not_bracket content when content <> "" && content.[0] = ':' ->
          [ parse_bracket_pseudo content ]
      | _ -> extract_not_conditions inner base_class
    in
    let open Css.Selector in
    let rel =
      combine
        (compound [ where [ peer_class ]; Not not_conditions ])
        Subsequent_sibling universal
    in
    [
      regular ~not_order:nvo
        ~selector:(compound [ Class modified_class; is_ [ rel ] ])
        ~props ~base_class:modified_class ();
    ]

(** Convert a base modifier to its CSS pseudo-class selector. *)
let pseudo_selector_of_modifier = function
  | Style.Hover -> Css.Selector.Hover
  | Style.Focus -> Css.Selector.Focus
  | Style.Active -> Css.Selector.Active
  | Style.Disabled -> Css.Selector.Disabled
  | Style.Checked -> Css.Selector.Checked
  | Style.First -> Css.Selector.First_child
  | Style.Last -> Css.Selector.Last_child
  | Style.Odd -> Css.Selector.(Nth_child (Odd, None))
  | Style.Even -> Css.Selector.(Nth_child (Even, None))
  | Style.Only -> Css.Selector.Only_child
  | Style.First_of_type -> Css.Selector.First_of_type
  | Style.Last_of_type -> Css.Selector.Last_of_type
  | Style.Only_of_type -> Css.Selector.Only_of_type
  | Style.Visited -> Css.Selector.Visited
  | Style.Target -> Css.Selector.Target
  | Style.Default -> Css.Selector.Default
  | Style.Indeterminate -> Css.Selector.Indeterminate
  | Style.Placeholder_shown -> Css.Selector.Placeholder_shown
  | Style.Autofill -> Css.Selector.Autofill
  | Style.Optional -> Css.Selector.Optional
  | Style.Required -> Css.Selector.Required
  | Style.Valid -> Css.Selector.Valid
  | Style.Invalid -> Css.Selector.Invalid
  | Style.In_range -> Css.Selector.In_range
  | Style.Out_of_range -> Css.Selector.Out_of_range
  | Style.Read_only -> Css.Selector.Read_only
  | Style.Read_write -> Css.Selector.Read_write
  | Style.User_valid -> Css.Selector.User_valid
  | Style.User_invalid -> Css.Selector.User_invalid
  | Style.Enabled -> Css.Selector.Enabled
  | Style.Empty -> Css.Selector.Empty
  | Style.Focus_within -> Css.Selector.Focus_within
  | Style.Focus_visible -> Css.Selector.Focus_visible
  | Style.Open ->
      Css.Selector.(is_ [ attribute "open" Presence; Popover_open; Open ])
  | _ -> Css.Selector.Focus (* fallback *)

(** Build the group-STATE selector rel: :where(.group/name):STATE descendant *)
let named_group_rel name pseudo =
  let open Css.Selector in
  combine
    (compound [ where [ Class ("group/" ^ name) ]; pseudo ])
    Descendant universal

(** Handle not-group-STATE/name compound variant *)
let handle_not_named_group inner name base_class props =
  let inner_str = Modifiers.pp_modifier inner in
  let modified_class =
    "not-group-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let rel = named_group_rel name pseudo in
  let open Css.Selector in
  let sel = compound [ Class modified_class; Not [ is_ [ rel ] ] ] in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

(** Handle has-group-STATE/name compound variant *)
let handle_has_named_group inner name base_class props =
  let inner_str = Modifiers.pp_modifier inner in
  let modified_class =
    "has-group-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let rel = named_group_rel name pseudo in
  let open Css.Selector in
  let sel = compound [ Class modified_class; Has [ is_ [ rel ] ] ] in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

(** Handle in-group-STATE/name compound variant — ancestor pattern *)
let handle_in_named_group inner name base_class props =
  let inner_str = Modifiers.pp_modifier inner in
  let modified_class =
    "in-group-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let rel = named_group_rel name pseudo in
  let open Css.Selector in
  let sel = combine (Where [ is_ [ rel ] ]) Descendant (Class modified_class) in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

(** Handle group-peer-STATE/name compound variant *)
let handle_group_peer_named inner name base_class props =
  let inner_str = Modifiers.pp_modifier inner in
  let modified_class =
    "group-peer-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let open Css.Selector in
  let peer_rel =
    combine
      (compound [ where [ Class "peer" ]; pseudo ])
      Subsequent_sibling universal
  in
  let group_rel =
    combine
      (compound [ where [ Class ("group/" ^ name) ]; is_ [ peer_rel ] ])
      Descendant universal
  in
  let sel = compound [ Class modified_class; is_ [ group_rel ] ] in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

(* Arbitrary selector: [&_p] → .class p *)
let arbitrary_selector_rule content base_class props =
  let open Css.Selector in
  let s = String.map (fun c -> if c = '_' then ' ' else c) content in
  let modified_class = "[" ^ content ^ "]:" ^ base_class in
  let parts = String.split_on_char '&' s in
  let sel =
    match parts with
    | [ ""; rest ] ->
        let rest = String.trim rest in
        if rest = "" then Class modified_class
        else
          let reader = Css.Reader.of_string rest in
          let descendant_sel = Css.Selector.read reader in
          combine (Class modified_class) Descendant descendant_sel
    | _ -> Class modified_class
  in
  regular ~selector:sel ~props ~base_class:modified_class ()

(** Convert a modifier and its context to a CSS rule. [inner_has_hover]
    indicates if the inner rule has a hover modifier that needs to be wrapped in
    CSS nesting with {i \@media (hover:hover)}. *)
let modifier_to_rule ?(inner_has_hover = false) modifier base_class selector
    props =
  match modifier with
  (* Data modifiers *)
  | Style.Data_state _ | Style.Data_variant _ ->
      route_data_modifier modifier base_class selector props
  | Style.Data_custom _ ->
      let modified_base_selector = Modifiers.to_selector modifier base_class in
      let modified_class =
        Rules_selector.extract_modified_class_name modified_base_selector
          base_class
      in
      let new_selector =
        Rules_selector.transform_selector_with_modifier modified_base_selector
          base_class modified_class selector
      in
      regular ~selector:new_selector ~props ~base_class:modified_class ()
  (* Media-like modifiers: dark, motion, contrast, print, orientation,
     forced-colors, inverted-colors, pointer, any-pointer, noscript *)
  | _ when Option.is_some (media_condition_of_modifier modifier) ->
      let condition = Option.get (media_condition_of_modifier modifier) in
      handle_media_like_modifier modifier ~condition ~inner_has_hover base_class
        selector props
  (* Supports feature query *)
  | Style.Supports condition_str ->
      handle_supports_modifier condition_str base_class selector props
  (* Responsive and container *)
  | Style.Responsive breakpoint ->
      responsive_rule breakpoint base_class selector props
  | Style.Min_responsive breakpoint ->
      min_responsive_rule breakpoint base_class selector props
  | Style.Max_responsive breakpoint ->
      max_responsive_rule breakpoint base_class selector props
  | Style.Min_arbitrary px -> min_arbitrary_rule px base_class selector props
  | Style.Max_arbitrary px -> max_arbitrary_rule px base_class selector props
  | Style.Min_arbitrary_length l ->
      min_arbitrary_length_rule l base_class selector props
  | Style.Max_arbitrary_length l ->
      max_arbitrary_length_rule l base_class selector props
  | Style.Custom_responsive name ->
      custom_responsive_rule name base_class selector props
  | Style.Min_custom name -> min_custom_rule name base_class selector props
  | Style.Max_custom name -> max_custom_rule name base_class selector props
  | Style.Container query -> container_rule query base_class selector props
  (* :not(), :not-bracket, group-not, peer-not — handled in
     apply_modifier_to_rule for multi-rule support *)
  | Style.Not _ | Style.Not_bracket _ | Style.Group_not _ | Style.Peer_not _ ->
      (* Should not be reached — these are handled in apply_modifier_to_rule *)
      regular ~selector ~props ~base_class ()
  (* :has() variants *)
  | Style.Has _ | Style.Group_has _ | Style.Peer_has _ ->
      route_has_modifier modifier base_class props
  (* Aria bracket and group/peer aria variants *)
  | Style.Aria_bracket _ | Style.Group_aria _ | Style.Peer_aria _
  | Style.Aria_checked | Style.Aria_expanded | Style.Aria_selected
  | Style.Aria_disabled ->
      let modifier =
        match modifier with
        | Style.Aria_checked -> Style.Aria_bracket "checked"
        | Style.Aria_expanded -> Style.Aria_bracket "expanded"
        | Style.Aria_selected -> Style.Aria_bracket "selected"
        | Style.Aria_disabled -> Style.Aria_bracket "disabled"
        | m -> m
      in
      route_aria_modifier modifier base_class props
  (* Data bracket and group/peer data variants *)
  | Style.Data_bracket _ | Style.Group_data _ | Style.Peer_data _ ->
      route_data_bracket_modifier modifier base_class props
  (* Starting style - selector includes starting: prefix *)
  | Style.Starting ->
      let modified_class = "starting:" ^ base_class in
      starting_style ~selector:(Css.Selector.Class modified_class) ~props
        ~base_class:modified_class ()
  (* Interactive pseudo-classes *)
  | Style.Hover | Style.Focus | Style.Active | Style.Focus_within
  | Style.Focus_visible | Style.Disabled ->
      handle_pseudo_class_modifier ~inner_has_hover modifier base_class selector
        props
  (* Pseudo-elements ::before and ::after - always prepend content property *)
  | Style.Pseudo_before | Style.Pseudo_after ->
      let sel = Modifiers.to_selector modifier base_class in
      let content_ref = Var.reference Typography.content_var in
      let content_decl = Css.content (Var content_ref) in
      let final_props = content_decl :: props in
      regular ~selector:sel ~props:final_props ~base_class ()
  | Style.Arbitrary_selector content ->
      arbitrary_selector_rule content base_class props
  (* Prose element variants — descendant selector with element filter *)
  | Style.Prose_element name ->
      let modified_class = "prose-" ^ name ^ ":" ^ base_class in
      let outer_sel = Css.Selector.Class modified_class in
      let inner_sel = Modifiers.prose_element_inner_selector name in
      let combined_sel =
        Css.Selector.combine outer_sel Css.Selector.Descendant inner_sel
      in
      regular ~selector:combined_sel ~props ~base_class:modified_class ()
  (* Fallback for other modifiers *)
  | _ ->
      handle_fallback_modifier ~inner_has_hover modifier base_class selector
        props

(** Generate pseudo-element rules with separate selectors for browser
    compatibility. An invalid pseudo-element in a comma list causes the entire
    rule to be dropped, so each variant gets its own rule. *)
let pseudo_element_rules ~pseudo_selectors bc props prefix =
  let c = Css.Selector.Class (prefix ^ ":" ^ bc) in
  let mc = prefix ^ ":" ^ bc in
  let open Css.Selector in
  List.map
    (fun sel -> regular ~selector:sel ~props ~base_class:mc ())
    (List.map
       (fun ps -> [ combine c Descendant ps; compound [ c; ps ] ])
       pseudo_selectors
    |> List.concat)

(** Apply a modifier to a Media_query rule by wrapping it in an outer media
    query. Handles media-like modifiers, responsive modifiers, and falls back to
    returning the rule unchanged. *)
let apply_modifier_to_media_query modifier ~inner_condition ~selector ~props
    ~base_class ~nested =
  let bc = Option.value base_class ~default:"" in
  let modified_base_selector = Modifiers.to_selector modifier bc in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector bc
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector bc
      modified_class selector
  in
  let inner_rule = Css.rule ~selector:new_selector props in
  let inner_media =
    Css.media ~condition:inner_condition (inner_rule :: nested)
  in
  let wrap_in_media condition =
    [
      media_query ~condition ~selector:new_selector ~props:[] ?base_class
        ~nested:[ inner_media ] ();
    ]
  in
  match media_condition_of_modifier modifier with
  | Some condition -> wrap_in_media condition
  | None -> (
      match modifier with
      | Style.Responsive _ | Style.Min_responsive _ | Style.Max_responsive _
      | Style.Min_arbitrary _ | Style.Max_arbitrary _
      | Style.Min_arbitrary_length _ | Style.Max_arbitrary_length _
      | Style.Custom_responsive _ | Style.Min_custom _ | Style.Max_custom _ ->
          let outer_condition, _ = responsive_modifier_condition modifier in
          wrap_in_media outer_condition
      | _ ->
          [
            Media_query
              {
                condition = inner_condition;
                selector;
                props;
                base_class;
                nested;
                not_order = 0;
              };
          ])

(* Extract selector and properties from a single Utility *)
(* Apply modifier to extracted rule *)
let apply_modifier_to_rule modifier = function
  | Regular { selector; props; base_class; has_hover; _ } -> (
      let bc = Option.value base_class ~default:"" in
      match modifier with
      | Style.Pseudo_marker ->
          let open Css.Selector in
          pseudo_element_rules
            ~pseudo_selectors:[ Marker; Webkit_details_marker ]
            bc props "marker"
      | Style.Pseudo_selection ->
          let open Css.Selector in
          pseudo_element_rules ~pseudo_selectors:[ Selection ] bc props
            "selection"
      | Style.Not inner_modifier -> (
          match inner_modifier with
          | Style.In_bracket content -> handle_not_in_bracket content bc props
          | _ -> handle_not_modifier inner_modifier bc selector props)
      | Style.Not_bracket content -> handle_not_bracket content bc props
      | Style.In_bracket content -> handle_in_bracket content bc props
      | Style.In_data attr -> handle_in_data attr bc props
      | Style.Group_not (inner, name_opt) ->
          handle_group_not_modifier inner name_opt bc props
      | Style.Peer_not (inner, name_opt) ->
          handle_peer_not_modifier inner name_opt bc props
      | Style.Not_named_group (inner, name) ->
          handle_not_named_group inner name bc props
      | Style.Has_named_group (inner, name) ->
          handle_has_named_group inner name bc props
      | Style.In_named_group (inner, name) ->
          handle_in_named_group inner name bc props
      | Style.Group_peer_named (inner, name) ->
          handle_group_peer_named inner name bc props
      | _ -> (
          try
            [
              modifier_to_rule ~inner_has_hover:has_hover modifier bc selector
                props;
            ]
          with Invalid_argument _ -> []))
  | Media_query
      { condition = inner_condition; selector; props; base_class; nested; _ } ->
      apply_modifier_to_media_query modifier ~inner_condition ~selector ~props
        ~base_class ~nested
  | other -> [ other ]

(* Handle Modified style by recursively extracting and applying modifier *)
let handle_modified util_inner modifier base_style extract_fn =
  (* Strip the outermost modifier if it matches the one being applied, to avoid
     doubled prefixes like .focus\:focus\:ring. But preserve inner modifiers for
     nested cases like dark [ aria_selected [...] ] ->
     .dark\:aria-selected\:... *)
  let inner_util, style =
    match util_inner with
    | Utility.Modified (inner_mod, u) when inner_mod = modifier ->
        (* Same modifier - strip it to avoid doubling *)
        (u, base_style)
    | _ ->
        (* Different or no modifier - preserve it *)
        (util_inner, base_style)
  in
  let base_class_name = Utility.to_class inner_util in
  let base_rules = extract_fn base_class_name inner_util style in
  List.concat_map (apply_modifier_to_rule modifier) base_rules

(* Handle Group style by extracting each item *)
let handle_group class_name util_inner styles extract_fn =
  match util_inner with
  | Utility.Group util_items ->
      let extract_item style_item util_item =
        let class_name_item = Utility.to_class util_item in
        extract_fn class_name_item util_item style_item
      in
      List.map2 extract_item styles util_items |> List.concat
  | _ -> List.concat_map (extract_fn class_name util_inner) styles

(** Replace placeholder selector "_" with the actual utility class selector *)
let resolve_placeholder_selector sel selector =
  if Css.Selector.to_string selector = "._" then sel else selector

(** Extract outputs from a [@media] statement's inner rules *)
let extract_media_outputs ~class_name ~sel condition statements =
  statements
  |> List.filter_map (fun inner ->
      match Css.as_rule inner with
      | Some (selector, declarations, _) ->
          let actual_selector = resolve_placeholder_selector sel selector in
          Some
            (media_query ~condition ~selector:actual_selector
               ~props:declarations ~base_class:class_name ())
      | None -> None)

(** Extract outputs from a [@container] statement's inner rules *)
let extract_container_outputs ~class_name condition statements =
  statements
  |> List.filter_map (fun inner ->
      match Css.as_rule inner with
      | Some (selector, declarations, _) ->
          Some
            (container_query ~condition ~selector ~props:declarations
               ~base_class:class_name ())
      | None -> None)

(** Extract outputs from a [@supports] statement's inner rules *)
let extract_supports_outputs ~class_name ~sel ?merge_key condition statements =
  statements
  |> List.filter_map (fun inner ->
      match Css.as_rule inner with
      | Some (selector, declarations, _) ->
          let actual_selector = resolve_placeholder_selector sel selector in
          Some
            (supports_query ~condition ~selector:actual_selector
               ~props:declarations ~base_class:class_name ?merge_key ())
      | None -> None)

(** Process a non-nested statement from a rule_list, returning outputs. Sets
    [has_regular_rules] to true when a regular rule is found. *)
let process_rule_list_stmt ~sel ~class_name ?merge_key ~has_regular_rules stmt =
  match Css.as_rule stmt with
  | Some (selector, declarations, nested) ->
      has_regular_rules := true;
      let actual_selector = resolve_placeholder_selector sel selector in
      Some
        [
          regular ~selector:actual_selector ~props:declarations
            ~base_class:class_name ~nested ?merge_key ();
        ]
  | None -> (
      match Css.as_media stmt with
      | Some (condition, statements) ->
          Some (extract_media_outputs ~class_name ~sel condition statements)
      | None -> (
          match Css.as_container stmt with
          | Some (_, condition, statements) ->
              Some (extract_container_outputs ~class_name condition statements)
          | None -> (
              match Css.as_supports stmt with
              | Some (condition, statements) ->
                  Some
                    (extract_supports_outputs ~class_name ~sel ?merge_key
                       condition statements)
              | None -> None)))

(* Process a Style with a rule_list into output rules. Processes rule_list items
   in order, preserving the original interleaving of Regular rules, @supports
   blocks, and @media queries.

   @supports blocks become separate [Supports_query] entries so they sort
   independently and don't prevent the base rule from being combined by the
   optimizer. @media blocks that appear at the top level of rule_list are
   collected and nested on the base rule (they represent modifier-based media
   that must stay grouped with the utility). *)
let extract_style_with_rules ~sel ~class_name ?merge_key ~props rule_list =
  (* Collect top-level @media for nesting on the base rule *)
  let nested_media = rule_list |> List.filter Css.is_nested_media in
  (* Process rule_list items in order, preserving interleaving *)
  let has_regular_rules = ref false in
  let ordered_entries =
    rule_list
    |> List.concat_map (fun stmt ->
        if Css.is_nested_media stmt then
          (* @media stays nested on base rule, skip here *)
          []
        else if Css.is_nested_supports stmt then
          (* @supports → hoist to Supports_query entries *)
          match Css.as_supports stmt with
          | Some (condition, statements) ->
              extract_supports_outputs ~class_name ~sel ?merge_key condition
                statements
          | None -> []
        else
          match
            process_rule_list_stmt ~sel ~class_name ?merge_key
              ~has_regular_rules stmt
          with
          | Some entries -> entries
          | None -> [])
  in
  (* Base rule with props and nested @media *)
  let base_rule =
    if props = [] && nested_media = [] then []
    else
      [
        regular ~selector:sel ~props ~base_class:class_name ~nested:nested_media
          ?merge_key ();
      ]
  in
  (* If the rule_list contains regular rules (forms plugin, gradient utilities),
     they come first so custom selectors precede the base props rule. Otherwise,
     base comes first to maintain fallback → @supports cascade order. *)
  if !has_regular_rules then ordered_entries @ base_rule
  else base_rule @ ordered_entries

let outputs util =
  let rec extract_with_class class_name util_inner = function
    | Style.Style { props; rules; merge_key; pseudo_suffix; _ } -> (
        let sel =
          match pseudo_suffix with
          | None -> Css.Selector.Class class_name
          | Some pseudo ->
              Css.Selector.compound [ Css.Selector.Class class_name; pseudo ]
        in
        match rules with
        | None ->
            if props = [] then []
            else
              [
                regular ~selector:sel ~props ~base_class:class_name ?merge_key
                  ();
              ]
        | Some rule_list ->
            extract_style_with_rules ~sel ~class_name ?merge_key ~props
              rule_list)
    | Style.Modified (modifier, base_style) ->
        handle_modified util_inner modifier base_style extract_with_class
    | Style.Group styles ->
        handle_group class_name util_inner styles extract_with_class
  in
  let class_name = Utility.to_class util in
  let style = Utility.to_style util in
  extract_with_class class_name util style
