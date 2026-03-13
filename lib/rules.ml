(** CSS rule generation and management

    This module converts Tailwind utility classes into optimized CSS rules. The
    complexity comes from several requirements:

    - Rule Extraction: Transform modifier structures into CSS rules
    - Conflict Resolution: Order utilities by specificity
    - CSS Layers: Generate proper [@layer] directives
    - Variable Resolution: Track CSS custom property dependencies
    - Media/Container Queries: Handle responsive modifiers *)

(* ======================================================================== *)
(* Types *)
(* ======================================================================== *)

type output =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
      nested : Css.statement list; (* Nested statements (e.g., @media) *)
      merge_key : string option;
      not_order : int; (* Variant order for not-* rules, 0 for normal rules *)
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list;
          (* Nested statements for compound modifiers *)
      not_order : int;
    }
  | Container_query of {
      condition : Css.Container.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Supports_query of {
      condition : Css.Supports.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      merge_key : string option;
      not_order : int;
    }

type by_type = {
  regular : output list;
  media : output list;
  container : output list;
  starting : output list;
  supports : output list;
}

(* Indexed rule for sorting with typed fields *)
type indexed_rule = {
  index : int;
  rule_type :
    [ `Regular
    | `Media of Css.Media.t
    | `Container of Css.Container.t
    | `Starting
    | `Supports of Css.Supports.t ];
  selector : Css.Selector.t;
  props : Css.declaration list;
  order : int * int;
  nested : Css.statement list;
  base_class : string option;
  merge_key : string option;
  not_order : int;
  variant_order : int;
}

(* Result of building individual layers *)
type layers_result = {
  theme_layer : Css.t;
  base_layer : Css.t;
  properties_layer : Css.t option;
  utilities_layer : Css.t;
  property_rules : Css.statement list;
}

(* ======================================================================== *)
(* Selector Classification - Types for analyzing CSS selectors *)
(* ======================================================================== *)

(** Classification of CSS selectors for ordering purposes *)
type selector_kind =
  | Simple  (** Plain class selector like .foo *)
  | Pseudo_element
      (** Selector with pseudo-element like .before:absolute::before *)
  | Complex of {
      has_focus : bool;
      has_focus_within : bool;
      has_focus_visible : bool;
      has_group : bool;  (** group-* without :has() like group-focus *)
      has_peer : bool;  (** peer-* without :has() like peer-checked *)
      has_group_has : bool;
          (** group-* with :has() like group-has-[:checked] *)
      has_peer_has : bool;  (** peer-* with :has() like peer-has-[:checked] *)
      has_standalone_has : bool;
      has_aria : bool;
    }  (** Selector with combinators, pseudo-classes, etc. *)

(** Relationship between two rules being compared *)
type rule_relationship =
  | Same_utility of string  (** Both rules from same base utility *)
  | Different_utilities  (** Rules from different utilities *)

(* ======================================================================== *)
(* Smart constructors for output *)
(* ======================================================================== *)

let regular ~selector ~props ?base_class ?(has_hover = false) ?(nested = [])
    ?merge_key ?(not_order = 0) () =
  Regular
    { selector; props; base_class; has_hover; nested; merge_key; not_order }

let media_query ~condition ~selector ~props ?base_class ?(nested = [])
    ?(not_order = 0) () =
  Media_query { condition; selector; props; base_class; nested; not_order }

let container_query ~condition ~selector ~props ?base_class () =
  Container_query { condition; selector; props; base_class }

let starting_style ~selector ~props ?base_class () =
  Starting_style { selector; props; base_class }

let supports_query ~condition ~selector ~props ?base_class ?merge_key
    ?(not_order = 0) () =
  Supports_query
    { condition; selector; props; base_class; merge_key; not_order }

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

(* Known aria shorthand names *)
let is_aria_shorthand_name = function
  | "busy" | "checked" | "disabled" | "expanded" | "hidden" | "pressed"
  | "readonly" | "required" | "selected" ->
      true
  | _ -> false

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
  let is_shorthand = is_aria_shorthand_name raw_str in
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
let not_variant_order = function
  (* Block 1: structural pseudo-classes *)
  | Style.First -> 100
  | Style.Last -> 200
  | Style.Only -> 300
  | Style.Odd -> 400
  | Style.Even -> 500
  | Style.First_of_type -> 600
  | Style.Last_of_type -> 700
  | Style.Only_of_type -> 800
  (* Block 1: state pseudo-classes *)
  | Style.Visited -> 900
  | Style.Target -> 1000
  | Style.Open -> 1100
  | Style.Default -> 1200
  | Style.Checked -> 1300
  | Style.Indeterminate -> 1400
  | Style.Placeholder_shown -> 1500
  | Style.Autofill -> 1600
  | Style.Optional -> 1700
  | Style.Required -> 1800
  | Style.Valid -> 1900
  | Style.Invalid -> 2000
  | Style.In_range -> 2100
  | Style.Out_of_range -> 2200
  | Style.Read_only -> 2300
  (* Block 1: misc before hover *)
  | Style.Empty -> 2400
  | Style.Focus_within -> 2500
  | Style.Hover -> 2600
  (* Block 2: interactive pseudo-classes (after hover media) *)
  | Style.Focus -> 2700
  | Style.Focus_visible -> 2800
  | Style.Active -> 2900
  | Style.Enabled -> 3000
  | Style.Disabled -> 3100
  | Style.Inert -> 3200
  (* Block 2: complex selectors *)
  | Style.Has _ -> 3300
  | Style.Aria_selected -> 3340
  | Style.Aria_checked -> 3350
  | Style.Aria_expanded -> 3360
  | Style.Aria_disabled -> 3370
  | Style.Aria_bracket _ -> 3380
  | Style.Group_aria _ -> 3385
  | Style.Peer_aria _ -> 3390
  | Style.Data_custom _ -> 3400
  | Style.Data_bracket _ -> 3401
  | Style.Group_data _ -> 3402
  | Style.Peer_data _ -> 3403
  | Style.Data_state _ -> 3410
  | Style.Data_variant _ -> 3420
  | Style.Data_active -> 3430
  | Style.Data_inactive -> 3440
  | Style.Nth _ -> 3500
  | Style.Nth_last _ -> 3550
  | Style.Nth_of_type _ -> 3600
  | Style.Nth_last_of_type _ -> 3650
  (* @supports *)
  | Style.Supports _ -> 4000
  (* Media: accessibility preferences *)
  | Style.Motion_safe -> 5000
  | Style.Motion_reduce -> 5100
  | Style.Contrast_more -> 5200
  | Style.Contrast_less -> 5300
  (* Media: responsive — max first (descending), then min (ascending) *)
  | Style.Max_responsive _ -> 6000
  | Style.Max_arbitrary _ -> 6100
  | Style.Max_arbitrary_length _ -> 6150
  | Style.Min_arbitrary _ -> 6200
  | Style.Min_arbitrary_length _ -> 6250
  | Style.Min_responsive _ -> 6300
  | Style.Responsive _ -> 6400
  (* Media: other *)
  | Style.Portrait -> 7000
  | Style.Landscape -> 7100
  (* Selector: directionality *)
  | Style.Ltr -> 8000
  | Style.Rtl -> 8100
  (* Media: appearance *)
  | Style.Dark -> 9000
  | Style.Print -> 9100
  | Style.Forced_colors -> 9200
  | Style.Noscript -> 9300
  (* Custom: hocus/device-hocus *)
  | Style.Hocus -> 10000
  | Style.Device_hocus -> 10100
  (* Bracket patterns *)
  | Style.Not_bracket _ -> 11000
  (* Group/peer-not *)
  | Style.Group_not _ -> 12000
  | Style.Peer_not _ -> 12100
  (* Fallback *)
  | _ -> 5500

(** Variant order for a media condition. Maps the condition directly to the same
    ordering as variant_order_of_prefix for the corresponding prefix string. Used
    to compute inner_vo from nested media statements (e.g., dark:group-hover has
    nested @media(hover:hover), giving inner_vo=20000 rather than 500 from the
    "group-hover" prefix). This ensures hover-nested rules sort after non-nested
    peer-/group- rules but before has-* and aria-* rules. *)
let variant_order_of_media_cond cond =
  let open Css.Media in
  match cond with
  | Hover -> 20000
  | Prefers_reduced_motion `No_preference -> 50000
  | Prefers_reduced_motion `Reduce -> 50100
  | Prefers_contrast `More -> 50200
  | Prefers_contrast `Less -> 50300
  | Orientation `Portrait -> 70000
  | Orientation `Landscape -> 70100
  | Prefers_color_scheme `Dark -> 90000
  | Prefers_color_scheme `Light -> 90000
  | Print -> 91000
  | Forced_colors `Active -> 92000
  | Inverted_colors `Inverted -> 93100
  | _ -> 0

(** Variant order for modifier prefixes. Maps a modifier prefix string (the part
    before the last ":" in base_class) to a position number in the Tailwind v4
    variant cascade. Used to sort variant rules across rule types. *)
let variant_order_of_prefix prefix =
  match prefix with
  (* Pseudo-elements *)
  | "group-hover" | "peer-hover" -> 500
  | "first-letter" | "first-line" -> 1000
  | "marker" -> 1100
  | "selection" -> 1200
  | "file" -> 1300
  | "placeholder" -> 1400
  | "backdrop" -> 1401
  | "details-content" -> 1500
  | "before" -> 1600
  | "after" -> 1601
  (* Block 1: structural pseudo-classes *)
  | "first" -> 10100
  | "last" -> 10200
  | "only" -> 10300
  | "odd" -> 10400
  | "even" -> 10500
  | "first-of-type" -> 10600
  | "last-of-type" -> 10700
  | "only-of-type" -> 10800
  | "visited" -> 10900
  | "target" -> 11000
  | "open" -> 11100
  | "default" -> 11200
  | "checked" -> 11300
  | "indeterminate" -> 11400
  | "placeholder-shown" -> 11500
  | "autofill" -> 11600
  | "optional" -> 11700
  | "required" -> 11800
  | "valid" -> 11900
  | "invalid" -> 12000
  | "in-range" -> 12100
  | "out-of-range" -> 12200
  | "read-only" -> 12300
  | "empty" -> 12400
  | "focus-within" -> 12500
  (* Hover — in @media(hover:hover) but between block 1 and block 2 *)
  | "hover" -> 20000
  (* Block 2: interactive pseudo-classes *)
  | "focus" -> 30100
  | "focus-visible" -> 30200
  | "active" -> 30300
  | "enabled" -> 30400
  | "disabled" -> 30500
  | "inert" -> 30550
  | "data-custom" | "data-active" | "data-inactive" -> 30800
  (* Hocus *)
  | "hocus" | "device-hocus" -> 35000
  | "portrait" -> 70000
  | "landscape" -> 70100
  (* Directionality *)
  | "ltr" -> 80000
  | "rtl" -> 80100
  (* Appearance media *)
  | "dark" -> 90000
  | "print" -> 91000
  | "forced-colors" -> 92000
  | "noscript" -> 93000
  | "inverted-colors" -> 93100
  (* @starting-style: comes after all media queries including dark:hover *)
  | "starting" -> 95000
  | _ ->
      let starts_with s p =
        String.length s >= String.length p
        && String.sub s 0 (String.length p) = p
      in
      if starts_with prefix "group-" then 500
        (* All group-* variants: same order as group-hover *)
      else if starts_with prefix "peer-" then 600
        (* All peer-* variants: after group-*, before pseudo-elements *)
      else if starts_with prefix "has-" then 30600
      else if starts_with prefix "aria-" then
        (* Named aria: aria-busy=30700, aria-checked=30701, etc.
           (alphabetical) *)
        if String.length prefix > 5 && prefix.[5] <> '[' then 30700
        else (* aria-[...] bracket *) 30790
      else if starts_with prefix "data-" then
        if String.length prefix > 5 && prefix.[5] = '[' then 30810 else 30800
      else if starts_with prefix "supports-" || starts_with prefix "supports"
      then 40000
      else if prefix = "motion-safe" then 50000
      else if prefix = "motion-reduce" then 50100
      else if prefix = "contrast-more" then 50200
      else if prefix = "contrast-less" then 50300
      else if starts_with prefix "pointer-" then 50400
      else if starts_with prefix "any-pointer-" then 50500
      else if
        prefix = "sm" || prefix = "md" || prefix = "lg" || prefix = "xl"
        || prefix = "2xl" || starts_with prefix "min-"
        || starts_with prefix "max-"
      then
        (* All responsive variants share the same variant_order. Within this
           group, Css.Media.compare handles the correct ordering: max-* before
           min-*, descending breakpoint for max-*, ascending for min-*. *)
        60000
      else if String.length prefix > 0 && prefix.[0] = '[' then 100000
      else if String.length prefix > 0 && prefix.[0] = '@' then 110000
      else 0

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
        let vo = variant_order_of_prefix prefix in
        if vo > 0 then vo
        else
          (* For compound prefixes like "dark:group-focus", try the outermost
             modifier (before the first colon in the prefix). *)
          match String.index_opt prefix ':' with
          | Some j -> variant_order_of_prefix (String.sub prefix 0 j)
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
  let nvo = not_variant_order inner_modifier in
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
  let nvo = not_variant_order (Style.Not_bracket content) in
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
  let nvo = not_variant_order (Style.Group_not (inner, name_opt)) in
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
  let nvo = not_variant_order (Style.Peer_not (inner, name_opt)) in
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

(** Process a Style with a rule_list into output rules. Processes rule_list
    items in order, preserving the original interleaving of Regular rules,
    @supports blocks, and @media queries.

    @supports blocks become separate [Supports_query] entries so they sort
    independently and don't prevent the base rule from being combined by the
    optimizer. @media blocks that appear at the top level of rule_list are
    collected and nested on the base rule (they represent modifier-based
    media that must stay grouped with the utility). *)
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

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(** Strip modifier prefixes (sm:, md:, hover:, etc.) to extract base utility
    name. Modifier prefixes come before the utility name. Colons inside bracket
    values (e.g., [family-name:var(...)]) are not modifier separators. *)
let extract_base_utility class_name_no_pseudo =
  let len = String.length class_name_no_pseudo in
  (* Find the last colon that is NOT inside brackets or parens *)
  let rec find_last_colon i bracket_depth paren_depth last_colon =
    if i >= len then last_colon
    else
      match class_name_no_pseudo.[i] with
      | '[' ->
          find_last_colon (i + 1) (bracket_depth + 1) paren_depth last_colon
      | ']' ->
          find_last_colon (i + 1)
            (max 0 (bracket_depth - 1))
            paren_depth last_colon
      | '(' ->
          find_last_colon (i + 1) bracket_depth (paren_depth + 1) last_colon
      | ')' ->
          find_last_colon (i + 1) bracket_depth
            (max 0 (paren_depth - 1))
            last_colon
      | ':' when bracket_depth = 0 && paren_depth = 0 ->
          find_last_colon (i + 1) bracket_depth paren_depth (Some i)
      | _ -> find_last_colon (i + 1) bracket_depth paren_depth last_colon
  in
  match find_last_colon 0 0 0 None with
  | Some colon_pos ->
      String.sub class_name_no_pseudo (colon_pos + 1) (len - colon_pos - 1)
  | None -> class_name_no_pseudo

(** Parse utility and get ordering, with fallback for non-utility classes *)
let parse_utility_order base_utility =
  let parts = String.split_on_char '-' base_utility in
  match Utility.base_of_strings parts with
  | Ok u -> Utility.order u
  | Error _ ->
      (* Some selectors (like .group, .peer, .container) are marker classes that
         don't parse as utilities. Give them a default low priority. *)
      (9999, 0)

(** Compute conflict resolution order from selector string using the AST. Parses
    the selector, finds the first class token (ignoring pseudo-tokens), strips
    modifier prefixes (e.g., "hover:"), and maps to Utility.order. Falls back to
    a default low priority when no class is found. *)
let conflict_order selector =
  let reader = Css.Reader.of_string selector in
  let sel = Css.Selector.read reader in
  match Css.Selector.first_class sel with
  | Some class_name -> class_name |> extract_base_utility |> parse_utility_order
  | None -> (9999, 0)

(* Extract selector and props pairs from Regular rules. *)
let selector_props_pairs rules =
  List.filter_map
    (fun rule ->
      match rule with
      | Regular { selector; props; base_class; _ } ->
          (* Compute ordering from base_class if available, otherwise parse
             selector *)
          let order =
            match base_class with
            | Some class_name -> (
                match Utility.base_of_class class_name with
                | Ok u -> Utility.order u
                | Error _ ->
                    (* base_class doesn't parse as a utility (e.g. "group"
                       marker class). Fall back to parsing the selector
                       string. *)
                    let sel_str = Css.Selector.to_string selector in
                    conflict_order sel_str)
            | None ->
                (* Fallback: parse selector if base_class is missing *)
                let sel_str = Css.Selector.to_string selector in
                conflict_order sel_str
          in
          Some (selector, props, order)
      | _ -> None)
    rules

(* ======================================================================== *)
(* Rule Processing - Group and organize rules *)
(* ======================================================================== *)

let classify_by_type all_rules =
  let ( regular_rules,
        media_rules,
        container_rules,
        starting_rules,
        supports_rules ) =
    List.fold_left
      (fun (reg, media, cont, start, sup) rule ->
        match rule with
        | Regular _ -> (rule :: reg, media, cont, start, sup)
        | Media_query _ -> (reg, rule :: media, cont, start, sup)
        | Container_query _ -> (reg, media, rule :: cont, start, sup)
        | Starting_style _ -> (reg, media, cont, rule :: start, sup)
        | Supports_query _ -> (reg, media, cont, start, rule :: sup))
      ([], [], [], [], []) all_rules
  in
  (* Reverse to maintain original order since we prepended *)
  {
    regular = List.rev regular_rules;
    media = List.rev media_rules;
    container = List.rev container_rules;
    starting = List.rev starting_rules;
    supports = List.rev supports_rules;
  }

let is_hover_rule = function
  | Regular { has_hover; _ } -> has_hover
  | _ -> false

let is_simple_class_selector sel =
  (* Check if selector is a simple class without combinators or
     pseudo-elements *)
  match sel with
  | Css.Selector.Class _ -> true
  | _ -> false

(** Compare complex selector kinds. Returns ordering value for sorting. At equal
    priority levels, the order is: simple/complex < pseudo-element < group <
    group-has < peer < peer-has < focus-within < focus-visible < has < aria *)
let complex_selector_order = function
  | Complex { has_aria = true; _ } -> 60
  | Complex { has_standalone_has = true; _ } -> 50
  | Complex { has_focus_visible = true; _ } -> 40
  | Complex { has_focus_within = true; _ } -> 30
  | Complex { has_peer_has = true; _ } -> 21
  | Complex { has_peer = true; _ } -> 20
  | Complex { has_group_has = true; _ } -> 11
  | Complex { has_group = true; _ } -> 10
  | Pseudo_element -> 5 (* After simple/complex but before late modifiers *)
  | Simple -> 0
  | Complex _ -> 0

(** Determine the relationship between two rules *)
let rule_relationship r1 r2 =
  match (r1.base_class, r2.base_class) with
  | Some bc1, Some bc2 when bc1 = bc2 -> Same_utility bc1
  | _ -> Different_utilities

(* ======================================================================== *)

(** Traverse a selector tree like [Css.Selector.any] but skip [Not] children.
    This prevents :not(:focus) from being classified as a focus modifier, which
    would break ordering for not-* variant rules. *)
let rec any_outside_not p = function
  | Css.Selector.Not _ -> false
  | Css.Selector.Compound xs -> List.exists (any_outside_not p) xs
  | Css.Selector.Combined (a, _, b) ->
      any_outside_not p a || any_outside_not p b
  | Css.Selector.Relative (_, b) -> any_outside_not p b
  | Css.Selector.List xs -> List.exists (any_outside_not p) xs
  | Css.Selector.Is xs | Css.Selector.Where xs | Css.Selector.Has xs ->
      List.exists (any_outside_not p) xs
  | s -> p s

(** Classify a selector into Simple or Complex with focus/has analysis. For List
    selectors (merged selectors like `.a, .b`), classify based on the first
    element to preserve sort order. *)
let classify_selector sel =
  (* For List selectors (created by optimizer when merging), use first element's
     classification *)
  let sel_to_classify =
    match Css.Selector.as_list sel with
    | Some (first :: _) -> first
    | Some [] -> sel (* Empty list shouldn't happen, but handle it *)
    | None -> sel
  in
  if is_simple_class_selector sel_to_classify then Simple
  else if Css.Selector.has_pseudo_element sel_to_classify then Pseudo_element
  else
    (* Helper to detect :has() pseudo-class — skips :not() to avoid false
       positives for not-has-* selectors *)
    let has_has_pseudo s =
      any_outside_not (function Css.Selector.Has _ -> true | _ -> false) s
    in
    (* Helper to detect aria-* attributes — skips :not() to avoid false
       positives for not-aria-* selectors *)
    let has_aria_attr s =
      any_outside_not
        (function
          | Css.Selector.Attribute (_, Css.Selector.Aria _, _, _) -> true
          | _ -> false)
        s
    in
    let has_group = Css.Selector.has_group_marker sel_to_classify in
    let has_peer = Css.Selector.has_peer_marker sel_to_classify in
    let has_has = has_has_pseudo sel_to_classify in
    Complex
      {
        (* Use any_outside_not for focus detection so :not(:focus) does not
           count as a focus modifier — this prevents not-focus rules from being
           incorrectly sorted after all media queries. *)
        has_focus =
          any_outside_not
            (function Css.Selector.Focus -> true | _ -> false)
            sel_to_classify;
        has_focus_within =
          any_outside_not
            (function Css.Selector.Focus_within -> true | _ -> false)
            sel_to_classify;
        has_focus_visible =
          any_outside_not
            (function Css.Selector.Focus_visible -> true | _ -> false)
            sel_to_classify;
        has_group = has_group && not has_has;
        has_peer = has_peer && not has_has;
        has_group_has = has_group && has_has;
        has_peer_has = has_peer && has_has;
        has_standalone_has = has_has && (not has_group) && not has_peer;
        has_aria = has_aria_attr sel_to_classify;
      }

(** Get sort key for preference media conditions. Tailwind order: reduced-motion
    (no-preference, reduce) < contrast (more, less) *)
let preference_condition_order cond = Css.Media.preference_order cond

let compare_indexed ~filter_custom_props (i1, sel1, _, (prio1, sub1))
    (i2, sel2, _, (prio2, sub2)) =
  let prio_cmp = Int.compare prio1 prio2 in
  if prio_cmp <> 0 then prio_cmp
  else
    (* Then by suborder *)
    let sub_cmp = Int.compare sub1 sub2 in
    if sub_cmp <> 0 then sub_cmp
    else if
      filter_custom_props
      && is_simple_class_selector sel1
      && is_simple_class_selector sel2
    then
      (* Same priority/suborder: sort alphabetically for simple class selectors,
         then by original index for stability. *)
      let sel_cmp =
        String.compare
          (Css.Selector.to_string sel1)
          (Css.Selector.to_string sel2)
      in
      if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2
    else Int.compare i1 i2

(* Convert selector/props/order triples to CSS rules with conflict ordering *)
(* Helper to filter custom properties for utilities layer *)
let should_keep_in_utilities decl =
  match Css.custom_declaration_layer decl with
  | Some layer when layer = "utilities" -> true
  | Some _ -> false
  | None -> (
      (* No fallback to name prefixes: keep only non-custom declarations when
         metadata is missing. *)
      match Css.custom_declaration_name decl with
      | None -> true
      | Some _ -> false)

let of_grouped ?(filter_custom_props = false) grouped_list =
  (* Sort by (priority, suborder, selector_name, original_index) to match
     Tailwind v4 ordering. *)
  let indexed =
    List.mapi (fun i (sel, props, order) -> (i, sel, props, order)) grouped_list
  in
  let sorted_indexed =
    List.sort (compare_indexed ~filter_custom_props) indexed
  in
  List.map
    (fun (_idx, selector, props, _order) ->
      let filtered_props =
        if filter_custom_props then List.filter should_keep_in_utilities props
        else props
      in
      Css.rule ~selector filtered_props)
    sorted_indexed

(* Type-directed helpers for rule sorting and construction *)

(** Count modifier colons in a selector's first class name. Used to determine
    modifier stacking depth for hover media interleaving. *)
let selector_modifier_depth sel =
  match Css.Selector.first_class sel with
  | Some cls ->
      String.fold_left (fun acc c -> if c = ':' then acc + 1 else acc) 0 cls
  | None -> 0

(** Check if a selector contains :hover pseudo-class at any depth (used to
    detect compound variants like group-hocus that combine hover+focus). *)
let rec selector_has_hover = function
  | Css.Selector.Hover -> true
  | Css.Selector.Compound sels -> List.exists selector_has_hover sels
  | Css.Selector.Combined (l, _, r) ->
      selector_has_hover l || selector_has_hover r
  | Css.Selector.Is sels | Css.Selector.Where sels ->
      List.exists selector_has_hover sels
  | Css.Selector.List sels -> List.exists selector_has_hover sels
  | _ -> false

(* Determine sort group for rule types. Regular and Media are grouped together
   to preserve utility grouping - media queries appear immediately after their
   base utility rule. *)
let rule_type_order = function
  | `Regular -> 0
  | `Media _ -> 0 (* Same as Regular to keep grouped *)
  | `Supports _ -> 0 (* Same as Regular to keep grouped with base rule *)
  | `Container _ -> 1
  | `Starting -> 2

(* Extract media sort key using Css.Media.kind and group_order. Returns (group,
   subkey) where subkey is rem value for responsive conditions. *)
let extract_media_sort_key = function
  | `Media cond -> Css.Media.group_order (Css.Media.kind cond)
  | _ -> (0, 0.)

(* ======================================================================== *)
(* Priority Comparison - Type-directed comparison helpers *)
(* ======================================================================== *)

(** Compare two simple selectors by suborder, then alphabetically, then index.
    Index fallback is critical for utilities like prose that emit multiple rules
    with the same selector - preserves original order. *)
let compare_simple_selectors sel1 sel2 s1 s2 i1 i2 =
  let sub_cmp = Int.compare s1 s2 in
  if sub_cmp <> 0 then sub_cmp
  else
    let sel_cmp =
      String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
    in
    if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare two complex selectors by kind, then selector string (for aria), then
    suborder, then index. Order: focus < group-has < peer-has < has < aria. For
    aria selectors, sort by attribute name before property to match Tailwind. *)
let compare_complex_selectors sel1 sel2 kind1 kind2 s1 s2 i1 i2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2
  else if k1 = 60 then
    (* Both are aria selectors - compare by selector string (aria attribute)
       before suborder (property shade) to match Tailwind v4 behavior *)
    let sel_cmp =
      String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
    in
    if sel_cmp <> 0 then sel_cmp
    else
      let sub_cmp = Int.compare s1 s2 in
      if sub_cmp <> 0 then sub_cmp else Int.compare i1 i2
  else
    (* Other complex selectors - use suborder first *)
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let sel_cmp =
        String.compare
          (Css.Selector.to_string sel1)
          (Css.Selector.to_string sel2)
      in
      if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare rules by priority, then suborder, then by selector kind. Uses
    type-directed dispatch based on selector classification. At the same
    priority/suborder, cross-kind comparisons preserve source order (index) to
    match tailwindcss output exactly. *)
let compare_by_priority_suborder_alpha sel1 sel2 (p1, s1) (p2, s2) i1 i2 =
  (* Priority comes first *)
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    (* Same priority: compare by suborder *)
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      (* Same priority and suborder: dispatch based on selector kinds *)
      let kind1 = classify_selector sel1 in
      let kind2 = classify_selector sel2 in
      match (kind1, kind2) with
      | Simple, Simple -> compare_simple_selectors sel1 sel2 s1 s2 i1 i2
      | Pseudo_element, Pseudo_element ->
          compare_simple_selectors sel1 sel2 s1 s2 i1 i2
      | Pseudo_element, Simple ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Pseudo_element, Complex _ ->
          (* At same priority/suborder, preserve source order via index. Prose
             rules need pseudo-elements after complex selectors to match
             tailwindcss output exactly. *)
          Int.compare i1 i2
      | Simple, Pseudo_element ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Simple, Complex _ ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Complex _, Pseudo_element ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Complex _, Simple ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Complex _, Complex _ ->
          compare_complex_selectors sel1 sel2 kind1 kind2 s1 s2 i1 i2

(* Note: compare_regular_rules was removed - now using compare_same_utility and
   compare_cross_utility in compare_indexed_rules. The :has() handling is done
   at the selector level if needed. *)

(* Compare two media query rules - sort by media type first (responsive last),
   then by media condition (to group related queries), then by whether they have
   nested media (rules with nested come after), then by priority/suborder. The
   nested sorting ensures that dark:hover:X rules come after dark:X rules,
   allowing the optimizer to merge consecutive @media (hover:hover) blocks. *)

(** Compare two media conditions within the same group *)
let compare_media_conditions group1 sub1 sub2 cond1 cond2 =
  if group1 = 2000 then
    (* Responsive: sort by rem value ascending *)
    Float.compare sub1 sub2
  else if group1 = 1000 then
    (* Preference_accessibility: sort by preference_condition_order *)
    match (cond1, cond2) with
    | Some c1, Some c2 ->
        Int.compare
          (preference_condition_order c1)
          (preference_condition_order c2)
    | _ -> 0
  else
    match (cond1, cond2) with
    | Some c1, Some c2 -> Css.Media.compare c1 c2
    | _ -> 0

(* compare_by_selector_complexity was removed - we now use
   compare_by_priority_suborder_alpha for all media groups to preserve source
   order and avoid artificial media block merging *)

(* For hover media, separate rules by modifier depth so that single-modifier
   hover rules (group-hover:flex) form a separate block from stacked hover rules
   (group-focus:group-hover:flex) *)
let compare_hover_depth cond1 cond2 sel1 sel2 =
  match (cond1, cond2) with
  | Some Css.Media.Hover, Some Css.Media.Hover ->
      Int.compare (selector_modifier_depth sel1) (selector_modifier_depth sel2)
  | _ -> 0

(* Compare by nested media condition when both have nested media. This sorts
   stacked min/max variants like min-sm:max-xl vs min-sm:max-lg by the inner
   media condition. *)
let compare_nested_media_cond nested1 nested2 =
  match (nested1, nested2) with
  | [ n1 ], [ n2 ] -> (
      match (Css.as_media n1, Css.as_media n2) with
      | Some (c1, _), Some (c2, _) -> Css.Media.compare c1 c2
      | _ -> 0)
  | _ -> 0

let compare_same_media_group sel1 sel2 order1 order2 i1 i2 nested1 nested2 bc1
    bc2 cond1 cond2 =
  let depth_cmp = compare_hover_depth cond1 cond2 sel1 sel2 in
  if depth_cmp <> 0 then depth_cmp
  else
    let nested_media_cmp = compare_nested_media_cond nested1 nested2 in
    if nested_media_cmp <> 0 then nested_media_cmp
    else
      let same_utility =
        match (bc1, bc2) with
        | Some b1, Some b2 -> String.equal b1 b2
        | _ -> false
      in
      if same_utility then
        let order_cmp = compare order1 order2 in
        if order_cmp <> 0 then order_cmp else Int.compare i1 i2
      else compare_by_priority_suborder_alpha sel1 sel2 order1 order2 i1 i2

let compare_media_rules typ1 typ2 sel1 sel2 order1 order2 i1 i2 nested1 nested2
    bc1 bc2 =
  (* Nested/compound media queries come after simple ones *)
  let nested_cmp = Bool.compare (nested1 <> []) (nested2 <> []) in
  if nested_cmp <> 0 then nested_cmp
  else
    let group1, sub1 = extract_media_sort_key typ1 in
    let group2, sub2 = extract_media_sort_key typ2 in
    let key_cmp = Int.compare group1 group2 in
    if key_cmp <> 0 then key_cmp
    else
      let cond1 = match typ1 with `Media c -> Some c | _ -> None in
      let cond2 = match typ2 with `Media c -> Some c | _ -> None in
      let cond_cmp = compare_media_conditions group1 sub1 sub2 cond1 cond2 in
      if cond_cmp <> 0 then cond_cmp
      else
        compare_same_media_group sel1 sel2 order1 order2 i1 i2 nested1 nested2
          bc1 bc2 cond1 cond2

(* ======================================================================== *)
(* Regular vs Media Comparison - Type-directed comparison for mixed rules *)
(* ======================================================================== *)

(* For the same base utility, preserve original order (index) to keep media
   rules adjacent to their related state rules. This matches Tailwind's behavior
   where @media (forced-colors:active) appears right after :checked state. *)
let compare_same_utility_regular_media r1 r2 = Int.compare r1.index r2.index

(** Check if a selector has special modifiers that should come at the end. This
    includes :has() variants (group-has, peer-has, has) and focus-within/
    focus-visible variants that have modifier prefixes.

    Note: Regular focus: with modifier prefix is NOT a late modifier - it comes
    before motion-safe/motion-reduce media rules. Only focus-within: and
    focus-visible: are late modifiers.

    IMPORTANT: Only selectors with modifier colons (like `.focus-within\:ring`)
    are late modifiers. Native pseudo-classes like `.form-radio:focus` (no
    modifier prefix) should NOT be treated as late modifiers - they're just
    regular rules from the base utility. *)
let is_late_modifier sel_kind selector =
  let has_modifier_colon = Css.Selector.contains_modifier_colon selector in
  match sel_kind with
  | Complex { has_group_has = true; _ } -> true
  | Complex { has_peer_has = true; _ } -> true
  | Complex { has_focus_within = true; _ } -> has_modifier_colon
  | Complex { has_focus_visible = true; _ } -> has_modifier_colon
  | Complex { has_standalone_has = true; _ } -> true
  | _ -> false

(** Check if a selector is a state modifier rule. These include :active,
    :disabled, [aria-*], and :has() selectors with modifier colons. These come
    very late in the utilities layer, after focus-visible but before media
    queries. *)
let is_state_modifier_rule sel_kind selector =
  if not (Css.Selector.contains_modifier_colon selector) then false
  else
    match sel_kind with
    | Complex { has_aria = true; _ } -> true
    | Complex { has_standalone_has = true; _ } -> true
    | Complex _ ->
        (* Check for :active or :disabled pseudo-classes in the selector *)
        let sel_str = Css.Selector.to_string selector in
        String.ends_with ~suffix:":active" sel_str
        || String.ends_with ~suffix:":disabled" sel_str
    | _ -> false

(** Check if a selector is a focus: modifier rule (has :focus pseudo-class and
    modifier colon). These rules come AFTER hover:hover media but BEFORE other
    modifier-prefixed media like motion-safe:/motion-reduce:/contrast-more:. *)
let is_focus_modifier_rule sel_kind selector =
  Css.Selector.contains_modifier_colon selector
  && match sel_kind with Complex { has_focus = true; _ } -> true | _ -> false

(** Compare by (priority, suborder), defaulting to [regular_first] (-1). *)
let compare_by_order_regular_first (p1, s1) (p2, s2) =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp else -1

(* For hover media, interleave by modifier depth: non-hover regular rules at
   depth N come between hover media at depth N and depth N+1. *)

(** Compare Regular vs Media rules from different utilities. Uses selector
    classification to determine ordering. *)
let try_hover_media_interleave kind1 sel1 sel2 media_type =
  match media_type with
  | Some Css.Media.Hover when is_focus_modifier_rule kind1 sel1 ->
      let d1 = selector_modifier_depth sel1 in
      let d2 = selector_modifier_depth sel2 in
      let has_hov = selector_has_hover sel1 in
      if d2 > d1 && not has_hov then Some (-1) else Some 1
  | _ -> None

let compare_different_utility_regular_media sel1 sel2 order1 order2 media_type =
  let kind1 = classify_selector sel1 in
  match try_hover_media_interleave kind1 sel1 sel2 media_type with
  | Some c -> c
  | None -> (
      if
        (* Focus modifier rules come after ALL media queries *)
        is_focus_modifier_rule kind1 sel1
      then 1
      else
        (* Check if this is truly modifier-prefixed media (like dark:hover:,
           motion-safe:hover:) vs plain media with modifiers in the class
           name *)
        let is_modifier_prefixed_media =
          Css.Selector.contains_modifier_colon sel2
          &&
          match media_type with
          | Some
              ( Css.Media.Prefers_color_scheme _
              | Css.Media.Prefers_reduced_motion _
              | Css.Media.Prefers_contrast _ ) ->
              true
          | _ -> false
        in
        if is_modifier_prefixed_media then
          if is_late_modifier kind1 sel1 then 1 else -1
        else
          (* Plain/built-in media or modifier-based responsive/hover media *)
          let has_modifier_colon = Css.Selector.contains_modifier_colon sel2 in
          if not has_modifier_colon then
            (* Built-in media (e.g., container breakpoints) - respect
               priority *)
            let prio_cmp = Int.compare (fst order1) (fst order2) in
            if prio_cmp <> 0 then prio_cmp
            else
              match media_type with
              | Some
                  ( Css.Media.Min_width _ | Css.Media.Min_width_rem _
                  | Css.Media.Hover ) ->
                  -1
              | _ -> compare_by_order_regular_first order1 order2
          else
            (* Modifier-based responsive/hover media (md:, lg:, hover:) *)
            match media_type with
            | Some
                ( Css.Media.Hover | Css.Media.Min_width _
                | Css.Media.Min_width_rem _ ) ->
                -1
            | _ -> compare_by_order_regular_first order1 order2)

(** Compare Regular vs Media rules using rule relationship dispatch. *)
let compare_regular_vs_media r1 r2 =
  match rule_relationship r1 r2 with
  | Same_utility _ -> compare_same_utility_regular_media r1 r2
  | Different_utilities ->
      let media_type =
        match r2.rule_type with `Media m -> Some m | _ -> None
      in
      compare_different_utility_regular_media r1.selector r2.selector r1.order
        r2.order media_type

(* ======================================================================== *)
(* Regular Rule Comparison - Type-directed comparison for Regular rules *)
(* ======================================================================== *)

(** Compare pseudo-element vs non-pseudo-element selectors. In Tailwind v4,
    Simple selectors ALWAYS come before Pseudo_element selectors, regardless of
    priority or suborder. This ensures that `.mx-auto` comes before
    `.before\:absolute::before` even though they have different priorities. *)
let compare_pseudo_elements kind1 kind2 _sel1 _sel2 =
  match (kind1, kind2) with
  | Simple, Pseudo_element -> Some (-1) (* Simple before Pseudo_element *)
  | Pseudo_element, Simple -> Some 1 (* Pseudo_element after Simple *)
  | Pseudo_element, Pseudo_element -> None (* Compare by priority *)
  | _, _ -> None (* Let priority comparison handle other cases *)

(** Compare regular rules from the same base utility (e.g., all prose-sm rules).
    Preserves original index order to maintain source ordering within a utility.
    This ensures that pseudo-elements like ::marker stay in their natural
    position relative to their parent elements, matching Tailwind's behavior. *)
let compare_same_utility_regular r1 r2 =
  let prio_cmp = compare r1.order r2.order in
  if prio_cmp <> 0 then prio_cmp
  else
    (* Within the same utility and priority, preserve source order *)
    Int.compare r1.index r2.index

(* Debug flag for tracing comparisons *)
let debug_compare = ref false
let set_debug_compare b = debug_compare := b

(** Compare regular rules from different base utilities. Groups all rules from
    one utility together by sorting on base_class after priority.

    "Late modifier" utilities (group-has, peer-has, focus-within, focus-visible,
    has) always come after all other utilities, then are sorted by modifier
    type. Other complex selectors (with :focus, :checked, etc.) are sorted
    normally with their priority group.

    Order: late_modifier -> priority -> suborder -> modifier_kind -> base_class
    -> index *)
let compare_base_class_option bc1 bc2 =
  match (bc1, bc2) with
  | Some bc1, Some bc2 -> String.compare bc1 bc2
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let compare_by_priority_index r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let bc_cmp = compare_base_class_option r1.base_class r2.base_class in
      if bc_cmp <> 0 then bc_cmp
      else
        (* Compare by index first to preserve source order within same
           utility *)
        let idx_cmp = Int.compare r1.index r2.index in
        if idx_cmp <> 0 then idx_cmp
        else
          (* For rules from different utilities with same everything else, sort
             by selector name for deterministic ordering *)
          String.compare
            (Css.Selector.to_string r1.selector)
            (Css.Selector.to_string r2.selector)

let is_outline_utility bc =
  match bc with
  | Some s ->
      String.contains s ':' && String.contains s 'o'
      &&
      let idx = String.index s ':' in
      idx + 8 <= String.length s && String.sub s idx 8 = ":outline"
  | None -> false

(* Natural sort comparison: treats consecutive digit sequences as integers.
   E.g., "2.5" < "2.25" because 5 < 25 when compared as numbers. This matches
   Tailwind v4's selector ordering for opacity modifiers like /2.5 vs /2.25. *)
let natural_is_digit c = c >= '0' && c <= '9'

let natural_extract_number s i =
  let rec go j acc =
    if j >= String.length s || not (natural_is_digit s.[j]) then (acc, j)
    else go (j + 1) ((acc * 10) + Char.code s.[j] - Char.code '0')
  in
  go i 0

(* Skip CSS escape backslash before '#': compare \# as #. Tailwind v4 sorts by
   unescaped class names, but Css.Selector.to_string returns escaped form. '#'
   (0x23) sorts below digits but '\' (0x5C) sorts above them, so \[\#0088cc\]
   wrongly sorts after \[10px...\]. Only unescape \# — other escapes like \/
   need the backslash for correct opacity modifier ordering. *)
let natural_skip_hash_escape s i len =
  if i < len && s.[i] = '\\' && i + 1 < len && s.[i + 1] = '#' then i + 1 else i

let boundary_compare i1 len1 i2 len2 =
  if i1 >= len1 && i2 >= len2 then `Equal
  else if i1 >= len1 then `Less
  else if i2 >= len2 then `Greater
  else `Continue

let natural_compare s1 s2 =
  let len1 = String.length s1 and len2 = String.length s2 in
  let rec compare_at i1 i2 =
    match boundary_compare i1 len1 i2 len2 with
    | `Equal -> 0
    | `Less -> -1
    | `Greater -> 1
    | `Continue ->
        let i1 = natural_skip_hash_escape s1 i1 len1 in
        let i2 = natural_skip_hash_escape s2 i2 len2 in
        compare_at_chars i1 i2
  and compare_at_chars i1 i2 =
    match boundary_compare i1 len1 i2 len2 with
    | `Equal -> 0
    | `Less -> -1
    | `Greater -> 1
    | `Continue ->
        let c1 = s1.[i1] and c2 = s2.[i2] in
        if natural_is_digit c1 && natural_is_digit c2 then
          let n1, end1 = natural_extract_number s1 i1 in
          let n2, end2 = natural_extract_number s2 i2 in
          let num_cmp = Int.compare n1 n2 in
          if num_cmp <> 0 then num_cmp else compare_at end1 end2
        else
          let char_cmp = Char.compare c1 c2 in
          if char_cmp <> 0 then char_cmp else compare_at (i1 + 1) (i2 + 1)
  in
  compare_at 0 0

let compare_late_modifiers r1 r2 kind1 kind2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2 else compare_by_priority_index r1 r2

let compare_focus_modifiers r1 r2 =
  let outline1 = is_outline_utility r1.base_class in
  let outline2 = is_outline_utility r2.base_class in
  if outline1 && not outline2 then 1
  else if outline2 && not outline1 then -1
  else compare_by_priority_index r1 r2

(** Check if a selector kind is a focus-visible late modifier *)
let is_focus_visible_late_modifier kind selector =
  is_late_modifier kind selector
  &&
  match kind with
  | Complex { has_focus_visible = true; _ } -> true
  | _ -> false

(** Compare focus-visible and state modifier ordering. Returns [Some cmp] if at
    least one rule is a focus-visible or state modifier, [None] otherwise. *)
let compare_focus_visible_state r1 r2 kind1 kind2 =
  let fv1 = is_focus_visible_late_modifier kind1 r1.selector in
  let fv2 = is_focus_visible_late_modifier kind2 r2.selector in
  let s1 = is_state_modifier_rule kind1 r1.selector in
  let s2 = is_state_modifier_rule kind2 r2.selector in
  (* Order: focus-visible < state modifiers *)
  if fv1 && s2 then Some (-1)
  else if s1 && fv2 then Some 1
  else if fv1 && (not fv2) && not s2 then Some 1
  else if fv2 && (not fv1) && not s1 then Some (-1)
  else if fv1 && fv2 then Some (compare_by_priority_index r1 r2)
  else if s1 && not s2 then Some 1
  else if s2 && not s1 then Some (-1)
  else if s1 && s2 then Some (compare_by_priority_index r1 r2)
  else None

(** Compare focus modifier ordering. Returns [Some cmp] if at least one rule is
    a focus modifier, [None] otherwise. *)
let compare_focus_modifier_ordering r1 r2 kind1 kind2 =
  let f1 = is_focus_modifier_rule kind1 r1.selector in
  let f2 = is_focus_modifier_rule kind2 r2.selector in
  if f1 && not f2 then Some 1
  else if f2 && not f1 then Some (-1)
  else if f1 && f2 then Some (compare_focus_modifiers r1 r2)
  else None

(** Compare by priority, suborder, late modifiers, then natural selector sort.
    Used as the final comparison when focus-visible/state/focus modifiers don't
    apply. *)
let compare_by_prio_sub_late r1 r2 kind1 kind2 =
  let p1, _ = r1.order and p2, _ = r2.order in
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let _, s1 = r1.order and _, s2 = r2.order in
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      (* When priority and suborder are equal, check late modifiers first *)
      let late1 = is_late_modifier kind1 r1.selector in
      let late2 = is_late_modifier kind2 r2.selector in
      if late1 && not late2 then 1
      else if late2 && not late1 then -1
      else if late1 && late2 then compare_late_modifiers r1 r2 kind1 kind2
      else
        (* Not late modifiers - sort by selector name using natural sort
           (numeric-aware). Tailwind v4 uses natural sort so that e.g. /2.5
           comes before /2.25 (5 < 25 as integers). *)
        natural_compare
          (Css.Selector.to_string r1.selector)
          (Css.Selector.to_string r2.selector)

let compare_cross_utility_regular r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let kind1 = classify_selector r1.selector in
  let kind2 = classify_selector r2.selector in
  if !debug_compare then (
    let sel1 = Css.Selector.to_string r1.selector in
    let sel2 = Css.Selector.to_string r2.selector in
    let kind_str = function
      | Simple -> "Simple"
      | Pseudo_element -> "Pseudo_element"
      | Complex _ -> "Complex"
    in
    prerr_string
      (String.concat ""
         [
           "compare_cross_prio: ";
           sel1;
           " (";
           string_of_int p1;
           ",";
           string_of_int s1;
           ") vs ";
           sel2;
           " (";
           string_of_int p2;
           ",";
           string_of_int s2;
           ")\n";
         ]);
    prerr_string
      (String.concat ""
         [
           "compare_cross_kind: ";
           sel1;
           " (";
           kind_str kind1;
           ") vs ";
           sel2;
           " (";
           kind_str kind2;
           ")\n";
         ]));
  (* Check pseudo-elements, but only within the same priority group. Across
     different priorities, priority wins (e.g., form-input::placeholder at
     priority 3 comes before form-textarea at priority 8). *)
  let same_priority = p1 = p2 in
  match
    if same_priority then
      compare_pseudo_elements kind1 kind2 r1.selector r2.selector
    else None
  with
  | Some cmp -> cmp
  | None -> (
      match compare_focus_visible_state r1 r2 kind1 kind2 with
      | Some cmp -> cmp
      | None -> (
          match compare_focus_modifier_ordering r1 r2 kind1 kind2 with
          | Some cmp -> cmp
          | None -> compare_by_prio_sub_late r1 r2 kind1 kind2))

(** Compare two Regular rules using rule relationship dispatch. *)
let compare_regular_rules r1 r2 =
  let rel = rule_relationship r1 r2 in
  if !debug_compare then
    prerr_string
      (String.concat ""
         [
           "compare_regular: ";
           Css.Selector.to_string r1.selector;
           " vs ";
           Css.Selector.to_string r2.selector;
           " -> ";
           (match rel with
           | Same_utility bc -> "Same:" ^ bc
           | Different_utilities -> "Different");
           "\n";
         ]);
  match rel with
  | Same_utility _ -> compare_same_utility_regular r1 r2
  | Different_utilities -> compare_cross_utility_regular r1 r2

(** Compare two Starting style rules by priority then index. *)
let compare_starting_rules r1 r2 =
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index

(* ======================================================================== *)
(* Main Rule Comparison - Type-directed dispatch for all rule types *)
(* ======================================================================== *)

(* Normalize base_class for lexicographic comparison *)
let normalize_for_sort s =
  String.map
    (function
      | '_' -> ' ' | '[' | ']' -> '~' | '/' -> '|' | ':' -> '!' | c -> c)
    s

(* Compare by normalized base_class, then index *)
let compare_by_base_class r1 r2 =
  let bc1 =
    match r1.base_class with Some s -> normalize_for_sort s | None -> ""
  in
  let bc2 =
    match r2.base_class with Some s -> normalize_for_sort s | None -> ""
  in
  let class_cmp = String.compare bc1 bc2 in
  if class_cmp <> 0 then class_cmp else Int.compare r1.index r2.index

(* Sort key for supports modifier variants: named before bracket *)
let supports_sort_key bc =
  match bc with
  | Some s when String.length s > 9 && String.sub s 0 9 = "supports-" ->
      let after = String.sub s 9 (String.length s - 9) in
      if String.length after > 0 && after.[0] = '[' then (1, after)
      else (0, after)
  | Some s -> (0, s)
  | None -> (0, "")

(* Compare supports modifier rules by sort key *)
let compare_supports_by_key r1 r2 =
  let g1, k1 = supports_sort_key r1.base_class in
  let g2, k2 = supports_sort_key r2.base_class in
  let grp_cmp = Int.compare g1 g2 in
  if grp_cmp <> 0 then grp_cmp
  else
    let key_cmp = natural_compare k1 k2 in
    if key_cmp <> 0 then key_cmp else Int.compare r1.index r2.index

(* Compare by order tuple, then selector, then index *)
let compare_by_order_then_selector r1 r2 =
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp
  else
    let sel_cmp =
      natural_compare
        (Css.Selector.to_string r1.selector)
        (Css.Selector.to_string r2.selector)
    in
    if sel_cmp <> 0 then sel_cmp else Int.compare r1.index r2.index

(* Compare by order tuple, then index *)
let compare_by_order_then_index r1 r2 =
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index

(* Compare nested media conditions *)
let compare_nested_media r1 r2 =
  match (r1.nested, r2.nested) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | [ n1 ], [ n2 ] -> (
      match (Css.as_media n1, Css.as_media n2) with
      | Some (c1, _), Some (c2, _) -> Css.Media.compare c1 c2
      | _ -> 0)
  | _ -> 0

(* Compare rules when both have variant_order > 0 *)
let compare_variant_ordered r1 r2 =
  match (r1.rule_type, r2.rule_type) with
  | `Supports _, `Supports _ when r1.variant_order = r2.variant_order ->
      compare_supports_by_key r1 r2
  | _ ->
      let vo_cmp = Int.compare r1.variant_order r2.variant_order in
      if vo_cmp <> 0 then vo_cmp
      else
        (* Same variant_order: first sort by inner variant order for compound
           modifiers (e.g., dark:group-focus vs dark:hover). This must come
           before media_cmp because compare_nested_media would otherwise put
           rules without nested media before rules with nested media, overriding
           the intended inner modifier ordering. *)
        let variant_prefix bc =
          match bc with
          | Some s -> (
              match String.rindex_opt s ':' with
              | Some i -> String.sub s 0 i
              | None -> "")
          | None -> ""
        in
        let inner_vo prefix =
          let starts_with s p =
            String.length s >= String.length p
            && String.sub s 0 (String.length p) = p
          in
          let strip_group_peer_vo p =
            if starts_with p "group-" then
              variant_order_of_prefix (String.sub p 6 (String.length p - 6))
            else if starts_with p "peer-" then
              variant_order_of_prefix (String.sub p 5 (String.length p - 5))
            else variant_order_of_prefix p
          in
          match String.index_opt prefix ':' with
          | Some j ->
              let outer = String.sub prefix 0 j in
              if starts_with outer "group-" || starts_with outer "peer-" then
                (* For group-*/peer-* compound modifiers (e.g.,
                   group-focus:group-hover), use max stripped vo + 1 to place
                   compounds just after their highest-ordered constituent *)
                let parts = String.split_on_char ':' prefix in
                let max_vo =
                  List.fold_left
                    (fun acc p -> max acc (strip_group_peer_vo p))
                    0 parts
                in
                max_vo + 1
              else
                let inner =
                  String.sub prefix (j + 1) (String.length prefix - j - 1)
                in
                variant_order_of_prefix inner
          | None ->
              (* Only strip group-*/peer-* to get inner variant order. For all
                 other prefixes (e.g., "dark"), return 0 to preserve existing
                 ordering within consolidated media blocks. *)
              if starts_with prefix "group-" then
                variant_order_of_prefix
                  (String.sub prefix 6 (String.length prefix - 6))
              else if starts_with prefix "peer-" then
                variant_order_of_prefix
                  (String.sub prefix 5 (String.length prefix - 5))
              else 0
        in
        let p1_prefix = variant_prefix r1.base_class in
        let p2_prefix = variant_prefix r2.base_class in
        (* When inner_vo is 0 (prefix doesn't explicitly encode an inner order,
           e.g., plain "dark" prefix), use the nested media condition's order to
           distinguish compound rules. For example, contrast-more:dark with
           base_class "dark:text-white" gets inner_vo=0, but its nested
           @media(prefers-color-scheme:dark) gives effective_ivo=90000,
           correctly placing it after plain dark:text-white (effective_ivo=0).
           When inner_vo > 0 (prefix explicitly encodes order, e.g.,
           "dark:group-hover"→500 or "dark:hover"→20000), keep that value so
           that group-hover (500) sorts before hover (20000) and group-focus
           (also 500 but non-nested) sorts after group-hover (nested-first). *)
        let effective_ivo r prefix =
          let ivo = inner_vo prefix in
          if ivo > 0 then ivo
          else
            match r.nested with
            | [ n ] -> (
                match Css.as_media n with
                | Some (cond, _) -> variant_order_of_media_cond cond
                | None -> 0)
            | _ -> 0
        in
        let ivo_cmp =
          Int.compare (effective_ivo r1 p1_prefix) (effective_ivo r2 p2_prefix)
        in
        if ivo_cmp <> 0 then ivo_cmp
        else
          (* Same effective inner variant order: nested media rules (like
             group-hover with @media(hover:hover)) come before non-nested rules
             with the same ivo (like group-focus). This matches Tailwind's
             ordering where group-hover precedes group-focus within dark. *)
          let nested_cmp =
            let has_nested = function [] -> 0 | _ -> -1 in
            Int.compare (has_nested r1.nested) (has_nested r2.nested)
          in
          if nested_cmp <> 0 then nested_cmp
          else
            (* Same nesting: compare media conditions so that responsive
               breakpoints sort by ascending breakpoint value (e.g., sm before
               md before lg) regardless of utility priority. *)
            let media_cmp =
              match (r1.rule_type, r2.rule_type) with
              | `Media c1, `Media c2 ->
                  let cmp = Css.Media.compare c1 c2 in
                  if cmp <> 0 then cmp else compare_nested_media r1 r2
              | _ -> 0
            in
            if media_cmp <> 0 then media_cmp
            else
              (* Within same inner variant order and same media condition, sort
                 by variant prefix string to keep rules with the same modifier
                 prefix together (e.g., all group-focus rules before all
                 group-has rules). *)
              let prefix_cmp = String.compare p1_prefix p2_prefix in
              if prefix_cmp <> 0 then prefix_cmp
              else
                let p1, s1 = r1.order and p2, s2 = r2.order in
                let prio_cmp = Int.compare p1 p2 in
                if prio_cmp <> 0 then prio_cmp
                else
                  let sub_cmp = Int.compare s1 s2 in
                  if sub_cmp <> 0 then sub_cmp else compare_by_base_class r1 r2

(* Compare two Supports rules *)
let compare_supports_rules r1 r2 =
  let is_modifier_supports bc =
    match bc with
    | Some s -> String.length s > 9 && String.sub s 0 9 = "supports-"
    | None -> false
  in
  let m1 = is_modifier_supports r1.base_class in
  let m2 = is_modifier_supports r2.base_class in
  if m1 && m2 then compare_supports_by_key r1 r2
  else compare_by_order_then_selector r1 r2

(** Compare indexed rules for sorting. Uses type-directed dispatch based on
    rule_type. *)
let compare_indexed_rules r1 r2 =
  (if !debug_compare then
     let rule_type_str = function
       | `Regular -> "R"
       | `Media _ -> "M"
       | `Container _ -> "C"
       | `Starting -> "S"
       | `Supports _ -> "U"
     in
     prerr_string
       (String.concat ""
          [
            "compare_indexed: ";
            Css.Selector.to_string r1.selector;
            " vs ";
            Css.Selector.to_string r2.selector;
            " (types: ";
            rule_type_str r1.rule_type;
            "/";
            rule_type_str r2.rule_type;
            ")\n";
          ]));
  if r1.variant_order > 0 && r2.variant_order > 0 then
    compare_variant_ordered r1 r2
  else if r1.variant_order > 0 then 1
    (* variant-ordered rules always come after non-variant rules *)
  else if r2.variant_order > 0 then -1
    (* non-variant rules always come before variant-ordered rules *)
  else if r1.not_order > 0 || r2.not_order > 0 then
    let order_cmp = compare r1.order r2.order in
    if order_cmp <> 0 then order_cmp else compare_by_base_class r1 r2
  else
    let type_cmp =
      Int.compare (rule_type_order r1.rule_type) (rule_type_order r2.rule_type)
    in
    if type_cmp <> 0 then type_cmp
    else
      match (r1.rule_type, r2.rule_type) with
      | `Regular, `Regular -> compare_regular_rules r1 r2
      | `Media _, `Media _ ->
          compare_media_rules r1.rule_type r2.rule_type r1.selector r2.selector
            r1.order r2.order r1.index r2.index r1.nested r2.nested
            r1.base_class r2.base_class
      | `Regular, `Media _ -> compare_regular_vs_media r1 r2
      | `Media _, `Regular -> -compare_regular_vs_media r2 r1
      | `Starting, `Starting -> compare_starting_rules r1 r2
      | `Container _, `Container _ -> Int.compare r1.index r2.index
      | `Supports _, `Supports _ -> compare_supports_rules r1 r2
      | `Regular, `Supports _ | `Supports _, `Regular ->
          compare_by_order_then_selector r1 r2
      | `Supports _, `Media _ | `Media _, `Supports _ ->
          compare_by_order_then_index r1 r2
      | _, _ -> Int.compare r1.index r2.index

(* Filter properties to only include utilities layer declarations *)
let rec filter_utility_properties props =
  List.filter_map
    (fun decl ->
      match Css.as_theme_guarded decl with
      | Some (var_name, inner) -> (
          let filtered = filter_utility_properties [ inner ] in
          match filtered with
          | [ d ] -> Some (Css.theme_guarded ~var_name d)
          | _ -> None)
      | None -> (
          match Css.custom_declaration_layer decl with
          | Some layer when layer = "utilities" -> Some decl
          | Some _ -> None
          | None -> (
              match Css.custom_declaration_name decl with
              | None -> Some decl
              | Some _ -> None)))
    props

(* Recursively filter theme declarations from nested statements *)
let rec filter_theme_from_statements statements =
  List.map
    (fun stmt ->
      match Css.as_declarations stmt with
      | Some decls ->
          (* Bare declarations block - filter theme properties *)
          let filtered_decls = filter_utility_properties decls in
          Css.declarations filtered_decls
      | None -> (
          match Css.as_rule stmt with
          | Some (selector, decls, nested) ->
              let filtered_decls = filter_utility_properties decls in
              let filtered_nested = filter_theme_from_statements nested in
              Css.rule ~selector ~nested:filtered_nested filtered_decls
          | None -> (
              match Css.as_media stmt with
              | Some (condition, content) ->
                  Css.media ~condition (filter_theme_from_statements content)
              | None -> (
                  match Css.as_layer stmt with
                  | Some (name, content) ->
                      Css.layer ?name (filter_theme_from_statements content)
                  | None -> (
                      match Css.as_container stmt with
                      | Some (name, condition, content) ->
                          Css.container ?name ~condition
                            (filter_theme_from_statements content)
                      | None -> stmt)))))
    statements

(* Compute merge key from a base class name as a fallback when the utility
   handler does not provide a typed merge_key via Style.t. For bracket
   utilities, strips both bracket content and opacity so that e.g.
   accent-[#0088cc]/50 and accent-[#0088cc]/[0.5] share key "accent-". For
   non-bracket utilities, strips opacity suffix so that e.g. outline-red-500/50
   and outline-red-500/[0.5] share key "outline-red-500". Handlers that need
   finer control (e.g. preventing merging for named bracket colors) should set
   merge_key via Style.t instead. *)
let merge_key_of_base_class base_class =
  match base_class with
  | None -> None
  | Some class_name ->
      let base = extract_base_utility class_name in
      let key =
        match String.index_opt base '[' with
        | Some bracket_pos ->
            let k = String.sub base 0 bracket_pos in
            (* Strip trailing / before [ so "bg-red-500/[50%]" and
               "bg-red-500/50" share the same key "bg-red-500" *)
            if String.ends_with ~suffix:"/" k then
              String.sub k 0 (String.length k - 1)
            else k
        | None -> (
            match String.index_opt base '/' with
            | Some slash_pos -> String.sub base 0 slash_pos
            | None -> base)
      in
      Some key

(* Convert indexed rule to CSS statement *)
let indexed_rule_to_statement r =
  let filtered_props = filter_utility_properties r.props in
  let filtered_nested = filter_theme_from_statements r.nested in
  let merge_key =
    match r.merge_key with
    | Some _ as mk -> mk
    | None -> merge_key_of_base_class r.base_class
  in
  match r.rule_type with
  | `Regular ->
      Css.rule ~selector:r.selector ?merge_key ~nested:filtered_nested
        filtered_props
  | `Starting ->
      (* Wrap selector+declarations in @starting-style block
         (Tailwind-compatible format) *)
      Css.starting_style [ Css.rule ~selector:r.selector filtered_props ]
  | `Media condition ->
      (* For compound modifiers (e.g., dark:hover:), nested contains the inner
         media query. Otherwise, just emit a simple rule inside the media. *)
      if filtered_nested <> [] then
        (* Has nested statements (e.g., @media (hover:hover) { ... }) *)
        Css.media ~condition filtered_nested
      else
        Css.media ~condition
          [ Css.rule ~selector:r.selector ?merge_key filtered_props ]
  | `Container condition ->
      Css.container ~condition
        [ Css.rule ~selector:r.selector ?merge_key filtered_props ]
  | `Supports condition ->
      Css.supports ~condition
        [ Css.rule ~selector:r.selector ?merge_key filtered_props ]

(* Deduplicate typed triples while preserving first occurrence order *)
let deduplicate_typed_triples triples =
  let seen = Hashtbl.create (List.length triples) in
  List.filter
    (fun (typ, sel, props, _order, nested, _base_class, _merge_key, _not_order)
       ->
      let key = (typ, Css.Selector.to_string sel, props, nested) in
      if Hashtbl.mem seen key then false
      else (
        Hashtbl.add seen key ();
        true))
    triples

(* Get utility order from base class, with fallback to conflict order. Note:
   base_class may contain modifier prefixes (e.g., "md:grid-cols-2"), so we need
   to strip those before looking up the utility. Pseudo-element modifiers
   (before:, after:) use a fixed high suborder to preserve source order. *)
let order_of_base base_class selector =
  match base_class with
  | Some class_name -> (
      (* Check if this has a pseudo-element modifier prefix *)
      let has_pseudo_element_modifier =
        String.starts_with ~prefix:"before:" class_name
        || String.starts_with ~prefix:"after:" class_name
      in
      (* Strip modifier prefix to get base utility name *)
      let base_utility = extract_base_utility class_name in
      let parts = String.split_on_char '-' base_utility in
      match Utility.base_of_strings parts with
      | Ok u ->
          let prio, suborder = Utility.order u in
          if has_pseudo_element_modifier then
            (* Pseudo-element modifiers add 5000 to the base utility's suborder.
               This keeps them near their base utility but after all regular
               utilities, matching Tailwind v4 behavior where pseudo-elements
               appear late. *)
            (prio, suborder + 5000)
          else Utility.order u
      | Error _ -> conflict_order (Css.Selector.to_string selector))
  | None -> conflict_order (Css.Selector.to_string selector)

(* Adjust order with not-variant offset *)
let apply_not_order (prio, sub) not_order =
  if not_order = 0 then (prio, sub) else (prio, sub + not_order)

(* Convert each rule type to typed triple *)
let triple typ ~selector ~props ~order ~nested ~base_class ~merge_key ~not_order
    =
  Some (typ, selector, props, order, nested, base_class, merge_key, not_order)

let rule_to_triple = function
  | Regular
      { selector; props; base_class; nested; has_hover; merge_key; not_order }
    ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      let typ = if has_hover then `Media Css.Media.Hover else `Regular in
      triple typ ~selector ~props ~order ~nested ~base_class ~merge_key
        ~not_order
  | Media_query { condition; selector; props; base_class; nested; not_order } ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      triple (`Media condition) ~selector ~props ~order ~nested ~base_class
        ~merge_key:None ~not_order
  | Container_query { condition; selector; props; base_class } ->
      triple (`Container condition) ~selector ~props
        ~order:(order_of_base base_class selector)
        ~nested:[] ~base_class ~merge_key:None ~not_order:0
  | Starting_style { selector; props; base_class } ->
      triple `Starting ~selector ~props
        ~order:(order_of_base base_class selector)
        ~nested:[] ~base_class ~merge_key:None ~not_order:0
  | Supports_query
      { condition; selector; props; base_class; merge_key; not_order } ->
      let order =
        apply_not_order (order_of_base base_class selector) not_order
      in
      triple (`Supports condition) ~selector ~props ~order ~nested:[]
        ~base_class ~merge_key ~not_order

(* Add index to each triple for stable sorting *)
let add_index triples =
  List.mapi
    (fun i (typ, sel, props, order, nested, base_class, merge_key, not_order) ->
      {
        index = i;
        rule_type = typ;
        selector = sel;
        props;
        order;
        nested;
        base_class;
        merge_key;
        not_order;
        variant_order = compute_variant_order base_class sel;
      })
    triples

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  (* All rules (including hover) are now sorted together. Hover rules are
     converted to Media "(hover:hover)" rules in rule_to_triple, so they
     participate in the normal media query sorting. *)
  let indexed =
    all_rules
    |> List.filter_map rule_to_triple
    |> deduplicate_typed_triples |> add_index
  in
  let sorted = List.sort compare_indexed_rules indexed in
  if !debug_compare then
    List.iter
      (fun r ->
        Printf.eprintf "SORTED: vo=%d base=%s type=%s nested=%d\n"
          r.variant_order
          (match r.base_class with Some s -> s | None -> "<none>")
          (match r.rule_type with
          | `Regular -> "R"
          | `Media m -> "M:" ^ Css.Media.to_string m
          | `Container _ -> "C"
          | `Starting -> "S"
          | `Supports _ -> "U")
          (List.length r.nested))
      sorted;
  sorted |> List.map indexed_rule_to_statement

let build_utilities_layer ~layers ~statements =
  (* Statements are already in the correct order with media queries interleaved.
     Consecutive media queries with the same condition will be merged by the
     optimizer (css/optimize.ml) while preserving cascade order. *)
  if layers then Css.v [ Css.layer ~name:"utilities" statements ]
  else Css.v statements

(* Get sorted indexed rules - used for extracting first-usage order of
   variables *)
let sorted_indexed_rules all_rules =
  all_rules
  |> List.filter_map rule_to_triple
  |> deduplicate_typed_triples |> add_index
  |> List.sort compare_indexed_rules

(* Sort var names by property_order. Names include -- prefix. *)
let sort_vars_by_property_order vars =
  let get_order name =
    (* Strip -- prefix for lookup *)
    let name_without_prefix =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    match Var.property_order name_without_prefix with
    | Some o -> o
    | None -> 1000 (* Default for vars without property_order *)
  in
  List.sort (fun n1 n2 -> compare (get_order n1) (get_order n2)) vars

(* Extract all var names from sorted indexed rules in utility order. For each
   utility, collects: 1. Vars that are SET (custom declarations) 2. Vars that
   are REFERENCED and need @property (e.g., transform refs rotate/skew) Within
   each utility, vars are sorted by property_order to ensure consistent family
   ordering (e.g., ring before inset-ring regardless of CSS value order). *)
let var_names_of_sorted_rules sorted_rules =
  sorted_rules
  |> List.concat_map (fun r ->
      (* Vars that this utility SETS *)
      let filtered = filter_utility_properties r.props in
      let set_vars = Css.custom_prop_names filtered in
      (* Vars that this utility REFERENCES and need @property *)
      let all_vars = Css.vars_of_declarations r.props in
      let ref_vars =
        all_vars
        |> List.filter (fun (Css.V v) ->
            let name = Css.var_name v in
            Var.needs_property name)
        |> List.map (fun (Css.V v) -> "--" ^ Css.var_name v)
      in
      (* Sort all vars from this utility by property_order *)
      sort_vars_by_property_order (set_vars @ ref_vars))

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map outputs in
  rule_sets_from_selector_props all_rules

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes = List.concat_map outputs tw_classes

(* Helper to extract theme declarations from nested CSS statements
   recursively *)
let rec extract_theme_from_statements theme_vars insertion_order statements =
  List.iter
    (fun stmt ->
      (* Check if this is a rule with declarations *)
      (match Css.statement_declarations stmt with
      | Some props ->
          Css.custom_declarations ~layer:"theme" props
          |> List.iter (fun decl ->
              match Css.custom_declaration_name decl with
              | Some name when not (Hashtbl.mem theme_vars name) ->
                  Hashtbl.add theme_vars name decl;
                  insertion_order := decl :: !insertion_order
              | _ -> ())
      | None -> ());
      (* Recurse into nested statements *)
      (match Css.as_rule stmt with
      | Some (_, _, nested) ->
          extract_theme_from_statements theme_vars insertion_order nested
      | None -> ());
      (match Css.as_media stmt with
      | Some (_, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ());
      (match Css.as_layer stmt with
      | Some (_, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ());
      match Css.as_container stmt with
      | Some (_, _, content) ->
          extract_theme_from_statements theme_vars insertion_order content
      | None -> ())
    statements

let extract_non_tw_custom_declarations selector_props =
  (* Use Hashtbl to collect unique theme variables efficiently *)
  let theme_vars = Hashtbl.create 32 in
  let insertion_order = ref [] in

  selector_props
  |> List.iter (function
    | Regular { props; nested; _ } ->
        (* Extract from top-level props *)
        Css.custom_declarations ~layer:"theme" props
        |> List.iter (fun decl ->
            match Css.custom_declaration_name decl with
            | Some name when not (Hashtbl.mem theme_vars name) ->
                Hashtbl.add theme_vars name decl;
                insertion_order := decl :: !insertion_order
            | _ -> ());
        (* Extract from nested statements *)
        extract_theme_from_statements theme_vars insertion_order nested
    | Media_query { props; _ }
    | Container_query { props; _ }
    | Starting_style { props; _ }
    | Supports_query { props; _ } ->
        Css.custom_declarations ~layer:"theme" props
        |> List.iter (fun decl ->
            match Css.custom_declaration_name decl with
            | Some name when not (Hashtbl.mem theme_vars name) ->
                Hashtbl.add theme_vars name decl;
                insertion_order := decl :: !insertion_order
            | _ -> ()));
  (* Return in original insertion order *)
  List.rev !insertion_order

(* Get Var.any from declaration metadata *)
(* var_of_declaration_meta no longer used after refactor *)

(* assemble_theme_decls_metadata no longer used; ordering handled in
   compute_theme_layer *)

(* Check if declaration name is a default font family indirection *)
let is_default_family_name = function
  | "default-font-family" | "default-mono-font-family" -> true
  | _ -> false

(* Build set of declaration names for fast lookup *)
let names_set_of decls =
  List.fold_left
    (fun acc d ->
      match Css.custom_declaration_name d with
      | Some n -> Strings.add n acc
      | None -> acc)
    Strings.empty decls

(* Filter declarations whose names are not in the excluded set *)
let filter_non_duplicates excluded_names decls =
  List.filter
    (fun d ->
      match Css.custom_declaration_name d with
      | Some n -> not (Strings.mem n excluded_names)
      | None -> false)
    decls

(* Split defaults into pre (font families) and post (default-* indirections) *)
let split_defaults defaults =
  List.partition
    (fun decl ->
      match Css.custom_declaration_name decl with
      | Some n -> not (is_default_family_name n)
      | None -> false)
    defaults

(* Compare two order pairs *)
let compare_orders order_a order_b =
  match (order_a, order_b) with
  | Some (prio_a, sub_a), Some (prio_b, sub_b) ->
      let prio_cmp = Int.compare prio_a prio_b in
      if prio_cmp = 0 then Int.compare sub_a sub_b else prio_cmp
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

(* Sort declarations by their Var order metadata, alphabetical fallback *)
let sort_by_var_order decls =
  decls
  |> List.map (fun d ->
      (d, Var.order_of_declaration d, Css.custom_declaration_name d))
  |> List.sort (fun (_, a, na) (_, b, nb) ->
      let c = compare_orders a b in
      if c <> 0 then c else compare na nb)
  |> List.map (fun (d, _, _) -> d)

(* Build theme layer rule from declarations *)
let theme_layer_rule ~layers = function
  | [] -> if layers then Css.v [ Css.layer ~name:"theme" [] ] else Css.empty
  | decls ->
      let selector = Css.Selector.(list [ Root; host () ]) in
      let rule = Css.rule ~selector decls in
      if layers then Css.v [ Css.layer ~name:"theme" [ rule ] ]
      else Css.v [ rule ]

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let theme_layer_of_props ?(layers = true) ?(default_decls = []) selector_props =
  let extracted = extract_non_tw_custom_declarations selector_props in
  let pre_defaults, post_defaults = split_defaults default_decls in

  (* Filter defaults to remove duplicates of extracted vars *)
  let extracted_names = names_set_of extracted in
  let pre = filter_non_duplicates extracted_names pre_defaults in
  let post =
    filter_non_duplicates
      (Strings.union extracted_names (names_set_of pre))
      post_defaults
  in

  pre @ extracted @ post |> sort_by_var_order |> theme_layer_rule ~layers

let theme_layer_of ?(default_decls = []) tw_classes =
  let selector_props = collect_selector_props tw_classes in
  theme_layer_of_props ~default_decls selector_props

let placeholder_supports =
  let placeholder = Css.Selector.Placeholder in

  (* Create the inner @supports for modern browsers *)
  let modern_rule =
    Css.rule ~selector:placeholder
      [
        Css.color
          (Css.color_mix ~in_space:Oklab ~percent1:50. Current Transparent);
      ]
  in
  let modern_support_stmt =
    Css.supports
      ~condition:
        (Css.Supports.Property ("color", "color-mix(in lab, red, red)"))
      [ modern_rule ]
  in

  (* Create the outer @supports with the fallback rule and nested modern
     support *)
  let fallback_rule = Css.rule ~selector:placeholder [ Css.color Current ] in
  let outer_support_content = [ fallback_rule; modern_support_stmt ] in

  Css.v
    [
      Css.supports
        ~condition:
          (Css.Supports.Or
             ( Css.Supports.Not
                 (Css.Supports.Property
                    ("-webkit-appearance", "-apple-pay-button")),
               Css.Supports.Property ("contain-intrinsic-size", "1px") ))
        outer_support_content;
    ]

let build_base_layer ?supports ?(forms_base = false) () =
  let preflight =
    Preflight.stylesheet ?placeholder_supports:supports ~forms:forms_base ()
  in
  let base =
    if forms_base then Css.concat [ preflight; Forms.base_stylesheet () ]
    else preflight
  in
  Css.layer_of ~name:"base" base

(* Use the centralized conversion function from Var module *)

(* Property helpers are centralized in Property module *)
let partition_properties = Property.split
let dedup_properties = Property.dedup
let initial_values_of = Property.initial_values

(* Browser detection condition for properties layer. Detects browsers that need
   @property fallbacks: Safari <15.4 or Firefox <128. *)
let browser_detection =
  let open Css.Supports in
  Or
    ( And
        ( Property ("-webkit-hyphens", "none"),
          Not (Property ("margin-trim", "inline")) ),
      And
        ( Property ("-moz-orient", "inline"),
          Not (Property ("color", "rgb(from red r g b)")) ) )

(* Build a mapping from property names to their first-usage index. Tailwind
   orders properties in @supports and @property by first usage order in the
   sorted utilities output. Names already include -- prefix. *)
let build_first_usage_order set_var_names =
  let seen = Hashtbl.create 16 in
  let idx = ref 0 in
  List.iter
    (fun name ->
      (* Names from custom_prop_names already include -- prefix *)
      if not (Hashtbl.mem seen name) then (
        Hashtbl.add seen name !idx;
        incr idx))
    set_var_names;
  seen

(* Get property order from static registry. *)
let property_order_from name =
  match Var.property_order name with
  | Some o -> o
  | None ->
      failwith
        ("Missing property_order for variable '" ^ name
       ^ "'. Register ~property_order when defining the variable \
          (Var.channel/property_default).")

(* Build family first-usage order from the first_usage_order hashtbl. Returns a
   hashtbl mapping family to its first occurrence index. *)
let build_family_order first_usage_order =
  let family_order = Hashtbl.create 16 in
  Hashtbl.iter
    (fun name idx ->
      match Var.family name with
      | Some fam -> (
          match Hashtbl.find_opt family_order fam with
          | None -> Hashtbl.add family_order fam idx
          | Some existing ->
              if idx < existing then Hashtbl.replace family_order fam idx)
      | None -> ())
    first_usage_order;
  family_order

let gradient_family_index n =
  if not (String.starts_with ~prefix:"--tw-gradient-" n) then 100
  else
    match n with
    | "--tw-gradient-position" -> 0
    | "--tw-gradient-from" -> 1
    | "--tw-gradient-via" -> 2
    | "--tw-gradient-to" -> 3
    | "--tw-gradient-stops" -> 4
    | "--tw-gradient-via-stops" -> 5
    | "--tw-gradient-from-position" -> 6
    | "--tw-gradient-via-position" -> 7
    | "--tw-gradient-to-position" -> 8
    | _ -> 100

let uses_direct_property_order = function
  | Some
      ( `Gradient | `Translate | `Rotate | `Skew | `Scale | `Duration
      | `Font_weight | `Leading ) ->
      false
      (* Transforms, gradient, duration, and typography use first-usage order *)
  | Some _ -> true (* All other named families use property_order directly *)
  | None -> false
(* Variables without families (e.g. --tw-ease) are NOT direct; get_family_order
   returns 1000 for None, placing them last *)

let compare_property_vars ~get_family_order n1 n2 po1 po2 fam1 fam2 =
  (* Variables with negative property_order and no family come FIRST *)
  match (fam1, po1 < 0, fam2, po2 < 0) with
  | None, true, None, true -> compare po1 po2
  | None, true, _, _ -> -1
  | _, _, None, true -> 1
  | Some `Gradient, _, Some `Gradient, _ ->
      compare (gradient_family_index n1) (gradient_family_index n2)
  | _ when uses_direct_property_order fam1 && uses_direct_property_order fam2 ->
      compare po1 po2
  | _ ->
      let fo1 = get_family_order n1 in
      let fo2 = get_family_order n2 in
      if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2

let sort_properties_by_order first_usage_order initial_values =
  let family_order = build_family_order first_usage_order in
  let get_family_order name =
    match Var.family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  let cmp (n1, _) (n2, _) =
    let fam1 = Var.family n1 in
    let fam2 = Var.family n2 in
    let po1 = property_order_from n1 in
    let po2 = property_order_from n2 in
    compare_property_vars ~get_family_order n1 n2 po1 po2 fam1 fam2
  in
  List.sort cmp initial_values

(* Build property layer content with browser detection *)
let property_layer_content first_usage_order initial_values other_statements =
  let selector = Css.Selector.(list [ universal; Before; After; Backdrop ]) in
  let sorted_values =
    sort_properties_by_order first_usage_order initial_values
  in
  let initial_declarations =
    List.map (fun (name, value) -> Css.custom_property name value) sorted_values
  in
  let rule = Css.rule ~selector initial_declarations in
  let supports_stmt = Css.supports ~condition:browser_detection [ rule ] in
  let layer_content = [ supports_stmt ] @ other_statements in
  Css.v [ Css.layer ~name:"properties" layer_content ]

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let build_properties_layer first_usage_order explicit_property_rules_statements
    =
  let property_rules, other_statements =
    partition_properties explicit_property_rules_statements
  in
  let deduplicated = dedup_properties property_rules in
  let initial_values = initial_values_of deduplicated in

  if deduplicated = [] && initial_values = [] then (Css.empty, [])
  else
    let layer =
      property_layer_content first_usage_order initial_values other_statements
    in
    (layer, deduplicated)

(** Extract SET variable names from Custom_declarations *)
let set_var_names_from_props props = Css.custom_prop_names props

(** Extract variables and property rules from utility styles recursively.
    Returns (all_vars, set_var_names, property_rules) where:
    - all_vars: all referenced variables (for theme layer)
    - set_var_names: names of variables that are SET via Custom_declaration
    - property_rules: explicit property rules from utilities *)
let rec extract_style_vars_and_rules = function
  | Style.Style { props; rules; property_rules; _ } ->
      let vars_from_props = Css.vars_of_declarations props in
      let vars_from_rules =
        match rules with Some r -> Css.vars_of_rules r | None -> []
      in
      let set_names = set_var_names_from_props props in
      (vars_from_props @ vars_from_rules, set_names, [ property_rules ])
  | Style.Modified (_, t) -> extract_style_vars_and_rules t
  | Style.Group ts ->
      let results = List.map extract_style_vars_and_rules ts in
      let vars_list, set_names_list, prop_rules_list =
        List.fold_right
          (fun (v, s, p) (vs, ss, ps) -> (v :: vs, s :: ss, p :: ps))
          results ([], [], [])
      in
      ( List.concat vars_list,
        List.concat set_names_list,
        List.concat prop_rules_list )

(* Filter variables that need @property rules *)
let vars_needing_property vars =
  List.filter (fun (Css.V v) -> Var.needs_property_rule v) vars

(* Extract names from explicit @property rules into a set *)
let property_names_of statements =
  statements
  |> List.filter_map (fun stmt ->
      match Css.as_property stmt with
      | Some (Css.Property_info info) -> Some info.name
      | None -> None)
  |> List.fold_left (fun acc n -> Strings.add n acc) Strings.empty

(* Generate @property rules for variables not in explicit set *)
let property_rules_for vars excluded_names =
  vars
  |> List.filter (fun (Css.V v) ->
      let var_name = "--" ^ Css.var_name v in
      not (Strings.mem var_name excluded_names))
  |> List.map (fun (Css.V v) ->
      let var_name = "--" ^ Css.var_name v in
      Css.property ~name:var_name Css.Universal ~inherits:false ())

(** Collect all property rules: explicit ones and auto-generated ones. Only
    auto-generates [\@property] for variables that are: 1. Actually SET (via
    Custom_declaration) in the utilities 2. Have needs_property=true in their
    metadata *)
let collect_all_property_rules vars_from_utilities set_var_names
    explicit_property_rules_statements =
  let set_names_set =
    List.fold_left (fun acc n -> Strings.add n acc) Strings.empty set_var_names
  in
  (* Filter to only vars that are SET, not just referenced *)
  let needing_property =
    vars_needing_property vars_from_utilities
    |> List.filter (fun (Css.V v) ->
        let var_name = "--" ^ Css.var_name v in
        Strings.mem var_name set_names_set)
  in
  let explicit_names = property_names_of explicit_property_rules_statements in
  let generated_rules = property_rules_for needing_property explicit_names in
  let generated_statements =
    generated_rules |> List.concat_map Css.statements
  in
  explicit_property_rules_statements @ generated_statements

(** Build layer declaration list based on which layers are present *)
let build_layer_declaration ~has_properties ~include_base =
  let names =
    (if has_properties then [ "properties" ] else [])
    @
    if include_base then [ "theme"; "base"; "components"; "utilities" ]
    else [ "theme"; "components"; "utilities" ]
  in
  Css.v [ Css.layer_decl names ]

(** Sort [@property] rules using family-based first-usage order *)
let sort_property_rules_by_usage first_usage_order property_rules_for_end =
  let family_order = build_family_order first_usage_order in
  let get_family_order name =
    match Var.family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  property_rules_for_end
  |> List.sort (fun s1 s2 ->
      match (Css.as_property s1, Css.as_property s2) with
      | ( Some (Css.Property_info { name = n1; _ }),
          Some (Css.Property_info { name = n2; _ }) ) ->
          let fam1 = Var.family n1 in
          let fam2 = Var.family n2 in
          let po1 = property_order_from n1 in
          let po2 = property_order_from n2 in
          (* Variables with no family and negative property_order (e.g.
             --tw-space-x-reverse) always come before family variables *)
          let no_family_negative_first =
            match (fam1, fam2) with
            | None, Some _ when po1 < 0 -> -1
            | Some _, None when po2 < 0 -> 1
            | _ -> 0
          in
          if no_family_negative_first <> 0 then no_family_negative_first
          else if
            (* Named families that use property_order directly (Ring,
               Inset_ring, Shadow, Border, etc.) sort by property_order across
               families. This groups Ring+Inset_ring correctly (po 13-20).
               No-family vars (--tw-ease etc.) are NOT direct and get fo=1000,
               so they appear after all named-family vars. *)
            uses_direct_property_order fam1 && uses_direct_property_order fam2
          then compare po1 po2
          else
            let fo1 = get_family_order n1 in
            let fo2 = get_family_order n2 in
            if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2
      | _ -> 0)

(** Deduplicate keyframes by name, keeping first occurrence, then convert to CSS
    statements *)
let dedup_keyframes_to_css keyframes =
  let seen = Hashtbl.create 8 in
  let deduped =
    List.filter
      (fun (name, _) ->
        if Hashtbl.mem seen name then false
        else (
          Hashtbl.add seen name ();
          true))
      keyframes
  in
  let stmts =
    List.map (fun (name, frames) -> Css.keyframes name frames) deduped
  in
  if stmts = [] then [] else [ Css.v stmts ]

(** Assemble all CSS layers in the correct order *)
let assemble_all_layers ~layers ~include_base ~properties_layer ~theme_layer
    ~base_layer ~utilities_layer ~property_rules_for_end ~keyframes
    ~first_usage_order =
  let base_layers =
    if include_base then [ theme_layer; base_layer ] else [ theme_layer ]
  in
  let initial_layers =
    match properties_layer with None -> [] | Some l -> [ l ]
  in
  let layers_without_property =
    if layers then
      let components_declaration = Css.v [ Css.layer_decl [ "components" ] ] in
      let layer_names =
        build_layer_declaration
          ~has_properties:(Option.is_some properties_layer)
          ~include_base
      in
      [ layer_names ] @ initial_layers @ base_layers
      @ [ components_declaration; utilities_layer ]
    else initial_layers @ base_layers @ [ utilities_layer ]
  in
  let sorted_property_rules =
    sort_property_rules_by_usage first_usage_order property_rules_for_end
  in
  let property_rules_css =
    if sorted_property_rules = [] then [] else [ Css.v sorted_property_rules ]
  in
  let keyframes_css = dedup_keyframes_to_css keyframes in
  layers_without_property @ property_rules_css @ keyframes_css

(* Extract variables, set var names, and property rules from all utilities *)
let extract_vars_and_rules utilities =
  let styles = List.map Utility.to_style utilities in
  let results = List.map extract_style_vars_and_rules styles in
  let vars_list, set_names_list, prop_rules_list =
    List.fold_right
      (fun (v, s, p) (vs, ss, ps) -> (v :: vs, s :: ss, p :: ps))
      results ([], [], [])
  in
  ( List.concat vars_list,
    List.concat set_names_list,
    List.concat prop_rules_list )

(* Flatten property rules into CSS statements *)
let flatten_property_rules property_rules_lists =
  property_rules_lists |> List.concat_map Css.statements

(* Build individual CSS layers *)
(* Detect if forms utilities are used - triggers including forms base layer *)
let has_forms_utilities tw_classes =
  let rec check_utility = function
    | Utility.Base u ->
        let name = Utility.name_of_base u in
        name = "forms" || name = "forms_select"
    | Utility.Modified (_, u) -> check_utility u
    | Utility.Group us -> List.exists check_utility us
  in
  List.exists check_utility tw_classes

(* Detect if before/after pseudo-elements are used - triggers content var
   property rule *)
let has_pseudo_elements tw_classes =
  let rec has_pseudo = function
    | Style.Pseudo_before | Style.Pseudo_after -> true
    | _ -> false
  and check_utility = function
    | Utility.Base _ -> false
    | Utility.Modified (modifier, u) -> has_pseudo modifier || check_utility u
    | Utility.Group us -> List.exists check_utility us
  in
  List.exists check_utility tw_classes

let has_transition_utility selector_props =
  List.exists
    (fun r ->
      let bc =
        match r with
        | Regular { base_class; _ }
        | Media_query { base_class; _ }
        | Container_query { base_class; _ }
        | Starting_style { base_class; _ }
        | Supports_query { base_class; _ } ->
            base_class
      in
      match bc with
      | Some c ->
          String.length c >= 10
          && String.sub c 0 10 = "transition"
          && c <> "transition-none"
      | None -> false)
    selector_props

let build_individual_layers ~layers ~include_base ~forms_base first_usage_order
    selector_props all_property_statements statements =
  let theme_defaults =
    let font_defaults =
      if include_base then Typography.default_font_family_declarations else []
    in
    let transition_defaults =
      if include_base && has_transition_utility selector_props then
        Transitions.default_transition_declarations
      else []
    in
    font_defaults @ transition_defaults
  in
  let theme_layer =
    theme_layer_of_props ~layers ~default_decls:theme_defaults selector_props
  in
  let base_layer =
    build_base_layer ~supports:placeholder_supports ~forms_base ()
  in
  let properties_layer, property_rules =
    if all_property_statements = [] then (None, [])
    else
      let layer, prop_rules =
        build_properties_layer first_usage_order all_property_statements
      in
      if layer = Css.empty then (None, prop_rules) else (Some layer, prop_rules)
  in
  let utilities_layer = build_utilities_layer ~layers ~statements in
  { theme_layer; base_layer; properties_layer; utilities_layer; property_rules }

(* Extract @keyframes from Style.rules *)
let rec collect_keyframes acc = function
  | Style.Style { rules = Some rs; _ } ->
      List.fold_left
        (fun acc stmt ->
          match Css.as_keyframes stmt with
          | Some (name, frames) -> (name, frames) :: acc
          | None -> acc)
        acc rs
  | Style.Style { rules = None; _ } -> acc
  | Style.Modified (_, t) -> collect_keyframes acc t
  | Style.Group ts -> List.fold_left collect_keyframes acc ts

(** Sort keyframes by their associated theme variable order. Keyframes like
    "spin"/"pulse"/"bounce" are associated with theme variables
    "animate-spin"/"animate-pulse"/"animate-bounce" that have explicit
    (priority, suborder) tuples registered. *)
let sort_keyframes_by_var_order keyframes =
  keyframes
  |> List.sort (fun (name1, _) (name2, _) ->
      let keyframe_var_order name =
        match Var.order ("animate-" ^ name) with
        | Some (p, s) -> (p * 1000) + s
        | None -> 1000000 (* Unknown keyframes sort last *)
      in
      let order_cmp =
        Int.compare (keyframe_var_order name1) (keyframe_var_order name2)
      in
      if order_cmp <> 0 then order_cmp
      else String.compare name1 name2 (* Stable sort for same order *))

(** Build all CSS layers from utilities and rules *)
let build_layers ~layers ~include_base ?forms ~selector_props tw_classes
    statements =
  let styles = List.map Utility.to_style tw_classes in
  let vars_from_utilities, set_var_names, property_rules_lists =
    extract_vars_and_rules tw_classes
  in
  (* Get sorted indexed_rules to extract first-usage order from sorted output *)
  let sorted_rules = sorted_indexed_rules selector_props in
  (* Build first-usage order from ALL vars per utility in utility order. For
     each utility, collects SET vars then REFERENCED vars needing @property.
     Within each utility, vars are sorted by property_order (done in
     var_names_of_sorted_rules). Across utilities, we preserve first-usage order
     to match Tailwind's behavior. *)
  let all_vars = var_names_of_sorted_rules sorted_rules in
  let first_usage_order = build_first_usage_order all_vars in
  let base_property_rules = flatten_property_rules property_rules_lists in
  (* Add content_var's property_rule if before/after pseudo-elements are used *)
  let explicit_property_rules =
    if has_pseudo_elements tw_classes then
      let content_property_rule =
        Var.property_rules Typography.content_var |> Css.statements
      in
      base_property_rules @ content_property_rule
    else base_property_rules
  in
  let all_property_statements =
    collect_all_property_rules vars_from_utilities set_var_names
      explicit_property_rules
  in
  (* Use explicit forms flag if provided, otherwise auto-detect from
     utilities *)
  let forms_base =
    match forms with Some f -> f | None -> has_forms_utilities tw_classes
  in
  let individual =
    build_individual_layers ~layers ~include_base ~forms_base first_usage_order
      selector_props all_property_statements statements
  in
  let keyframes =
    List.fold_left collect_keyframes [] styles
    |> List.rev |> sort_keyframes_by_var_order
  in
  assemble_all_layers ~layers ~include_base
    ~properties_layer:individual.properties_layer
    ~theme_layer:individual.theme_layer ~base_layer:individual.base_layer
    ~utilities_layer:individual.utilities_layer
    ~property_rules_for_end:individual.property_rules ~keyframes
    ~first_usage_order

let wrap_css_items statements =
  (* For inline mode, just wrap the statements in a stylesheet *)
  Css.v statements

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)

(* ======================================================================== *)

type config = {
  base : bool;
  forms : bool option;
  mode : Css.mode;
  layers : bool;
  optimize : bool;
}
(** Configuration for CSS generation. [forms] can be [None] for auto-detection,
    [Some true] to force forms base styles, or [Some false] to disable them.
    [layers] controls whether CSS output is wrapped in [@layer] directives; when
    [false], the content is the same but without [@layer theme] and
    [@layer utilities] wrappers ([@layer properties] is always kept). *)

let default_config =
  {
    base = true;
    forms = None;
    mode = Css.Variables;
    layers = true;
    optimize = false;
  }

let to_css ?(config = default_config) tw_classes =
  (* Extract once and share for rule sets and theme layer *)
  let selector_props = List.concat_map outputs tw_classes in

  let statements = rule_sets_from_selector_props selector_props in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  let stylesheet =
    match config.mode with
    | Css.Variables ->
        let layer_results =
          build_layers ~layers:config.layers ~include_base:config.base
            ?forms:config.forms ~selector_props tw_classes statements
        in
        Css.concat layer_results
    | Css.Inline ->
        (* No layers - just raw utility rules with var() resolved to fallback
           values *)
        wrap_css_items statements
  in
  (* Apply optimization if requested *)
  if config.optimize then Css.optimize stylesheet else stylesheet

(* Recursively collect all declarations from a style *)
let rec collect_declarations acc = function
  | Style.Style { props; rules; _ } ->
      let from_rules =
        match rules with
        | None -> []
        | Some rs ->
            List.concat
              (List.filter_map
                 (fun rule ->
                   match Css.as_rule rule with
                   | Some (_selector, declarations, _important) ->
                       Some declarations
                   | None -> None)
                 rs)
      in
      let acc = List.rev_append from_rules acc in
      List.rev_append props acc
  | Style.Modified (_, t) -> collect_declarations acc t
  | Style.Group ts -> List.fold_left collect_declarations acc ts

(* Filter out CSS custom properties (variables) *)
let filter_non_variables decls =
  List.filter (fun decl -> Css.custom_declaration_name decl = None) decls

let to_inline_style utilities =
  let styles = List.map Utility.to_style utilities in
  let all_props = List.rev (List.fold_left collect_declarations [] styles) in
  let non_variable_props = filter_non_variables all_props in
  Css.inline_style_of_declarations non_variable_props
