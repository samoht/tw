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
    }
  | Media_query of {
      condition : Css.Media.t;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
      nested : Css.statement list; (* Nested statements for compound modifiers *)
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

let regular ~selector ~props ?base_class ?(has_hover = false) ?(nested = []) ()
    =
  Regular { selector; props; base_class; has_hover; nested }

let media_query ~condition ~selector ~props ?base_class ?(nested = []) () =
  Media_query { condition; selector; props; base_class; nested }

let container_query ~condition ~selector ~props ?base_class () =
  Container_query { condition; selector; props; base_class }

let starting_style ~selector ~props ?base_class () =
  Starting_style { selector; props; base_class }

let supports_query ~condition ~selector ~props ?base_class () =
  Supports_query { condition; selector; props; base_class }

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

let selector_with_data_key selector key value =
  let attr_selector = Css.Selector.attribute key (Exact value) in
  Css.Selector.combine selector Descendant attr_selector

let responsive_rule breakpoint base_class selector props =
  let prefix = string_of_breakpoint breakpoint in
  let px_value =
    match prefix with
    | "sm" -> 640.
    | "md" -> 768.
    | "lg" -> 1024.
    | "xl" -> 1280.
    | "2xl" -> 1536.
    | _ -> 0.
  in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Min_width px_value) ~selector:new_selector
    ~props ~base_class:modified_class ()

let max_responsive_rule breakpoint base_class selector props =
  let prefix =
    match breakpoint with
    | `Sm -> "max-sm"
    | `Md -> "max-md"
    | `Lg -> "max-lg"
    | `Xl -> "max-xl"
    | `Xl_2 -> "max-2xl"
  in
  let px_value =
    match breakpoint with
    | `Sm -> 640.
    | `Md -> 768.
    | `Lg -> 1024.
    | `Xl -> 1280.
    | `Xl_2 -> 1536.
  in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  media_query ~condition:(Css.Media.Not_min_width px_value)
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

let container_rule query base_class selector props =
  let prefix = Containers.container_query_to_class_prefix query in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition = Containers.container_query_to_condition query in
  container_query ~condition ~selector:new_selector ~props ~base_class ()

let has_like_selector kind selector_str base_class props =
  let open Css.Selector in
  let reader = Css.Reader.of_string selector_str in
  let parsed_selector = Css.Selector.read reader in
  match kind with
  | `Has ->
      let class_name = "has-[" ^ selector_str ^ "]:" ^ base_class in
      let sel = compound [ class_ class_name; has [ parsed_selector ] ] in
      regular ~selector:sel ~props ~base_class:class_name ()
  | `Group_has ->
      let class_name = "group-has-[" ^ selector_str ^ "]:" ^ base_class in
      let rel =
        combine
          (compound [ where [ Class "group" ]; has [ parsed_selector ] ])
          Descendant universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ()
  | `Peer_has ->
      let class_name = "peer-has-[" ^ selector_str ^ "]:" ^ base_class in
      let rel =
        combine
          (compound [ where [ Class "peer" ]; has [ parsed_selector ] ])
          Subsequent_sibling universal
      in
      let sel = compound [ Class class_name; is_ [ rel ] ] in
      regular ~selector:sel ~props ~base_class:class_name ()

(* Pseudo-class modifiers: transform the base selector and mark hover when
   needed. *)
let handle_pseudo_class_modifier modifier base_class selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  let has_hover = Modifiers.is_hover modifier in
  regular ~selector:new_selector ~props ~base_class:modified_class ~has_hover ()

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

(* Route :has() variants to appropriate handler *)
let route_has_modifier modifier base_class props =
  let kind, selector_str =
    match modifier with
    | Style.Has s -> (`Has, s)
    | Style.Group_has s -> (`Group_has, s)
    | Style.Peer_has s -> (`Peer_has, s)
    | _ -> failwith "Invalid has modifier"
  in
  has_like_selector kind selector_str base_class props

(* Handle fallback for unmatched modifiers. Must extract modified_class so that
   outer modifiers like dark: can properly transform the selector. *)
let handle_fallback_modifier modifier base_class props =
  let sel = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name sel base_class
  in
  let has_hover = Modifiers.is_hover modifier in
  regular ~selector:sel ~props ~base_class:modified_class ~has_hover ()

(** Convert a modifier and its context to a CSS rule. [inner_has_hover]
    indicates if the inner rule has a hover modifier that needs to be wrapped in
    CSS nesting with {i \@media (hover:hover)}. *)
let modifier_to_rule ?(inner_has_hover = false) modifier base_class selector
    props =
  match modifier with
  (* Data modifiers *)
  | Style.Data_state _ | Style.Data_variant _ | Style.Data_custom _ ->
      route_data_modifier modifier base_class selector props
  (* Color scheme *)
  | Style.Dark ->
      handle_media_like_modifier Style.Dark
        ~condition:(Css.Media.Prefers_color_scheme `Dark) ~inner_has_hover
        base_class selector props
  (* Preference media modifiers *)
  | Style.Motion_safe ->
      handle_media_like_modifier Style.Motion_safe
        ~condition:(Css.Media.Prefers_reduced_motion `No_preference)
        ~inner_has_hover base_class selector props
  | Style.Motion_reduce ->
      handle_media_like_modifier Style.Motion_reduce
        ~condition:(Css.Media.Prefers_reduced_motion `Reduce) ~inner_has_hover
        base_class selector props
  | Style.Contrast_more ->
      handle_media_like_modifier Style.Contrast_more
        ~condition:(Css.Media.Prefers_contrast `More) ~inner_has_hover
        base_class selector props
  | Style.Contrast_less ->
      handle_media_like_modifier Style.Contrast_less
        ~condition:(Css.Media.Prefers_contrast `Less) ~inner_has_hover
        base_class selector props
  (* Print media *)
  | Style.Print ->
      handle_media_like_modifier Style.Print ~condition:Css.Media.Print
        ~inner_has_hover base_class selector props
  (* Orientation media *)
  | Style.Portrait ->
      handle_media_like_modifier Style.Portrait
        ~condition:(Css.Media.Orientation `Portrait) ~inner_has_hover base_class
        selector props
  | Style.Landscape ->
      handle_media_like_modifier Style.Landscape
        ~condition:(Css.Media.Orientation `Landscape) ~inner_has_hover
        base_class selector props
  (* Forced colors *)
  | Style.Forced_colors ->
      handle_media_like_modifier Style.Forced_colors
        ~condition:(Css.Media.Forced_colors `Active) ~inner_has_hover base_class
        selector props
  (* Supports feature query *)
  | Style.Supports condition_str ->
      let modified_class = "supports-[" ^ condition_str ^ "]:" ^ base_class in
      let new_selector =
        Rules_selector.replace_class_in_selector ~old_class:base_class
          ~new_class:modified_class selector
      in
      (* Parse as Property(prop, value) for simple "property:value" patterns,
         otherwise use Raw for complex conditions like "not (foo)" *)
      let condition =
        match String.index_opt condition_str ':' with
        | Some colon_pos when not (String.contains condition_str '(') ->
            (* Simple property:value - use structured type *)
            let prop = String.sub condition_str 0 colon_pos |> String.trim in
            let value =
              String.sub condition_str (colon_pos + 1)
                (String.length condition_str - colon_pos - 1)
              |> String.trim
            in
            Css.Supports.Property (prop, value)
        | _ ->
            (* Complex condition - pass through as Raw *)
            Css.Supports.Raw condition_str
      in
      supports_query ~condition ~selector:new_selector ~props
        ~base_class:modified_class ()
  (* Responsive and container *)
  | Style.Responsive breakpoint ->
      responsive_rule breakpoint base_class selector props
  | Style.Max_responsive breakpoint ->
      max_responsive_rule breakpoint base_class selector props
  | Style.Min_arbitrary px -> min_arbitrary_rule px base_class selector props
  | Style.Max_arbitrary px -> max_arbitrary_rule px base_class selector props
  | Style.Container query -> container_rule query base_class selector props
  (* :not() pseudo-class *)
  | Style.Not _modifier ->
      regular
        ~selector:
          (Css.Selector.Class
             ("not-" ^ base_class ^ ":not("
             ^ Css.Selector.to_string selector
             ^ ")"))
        ~props ~base_class ()
  (* :has() variants *)
  | Style.Has _ | Style.Group_has _ | Style.Peer_has _ ->
      route_has_modifier modifier base_class props
  (* Starting style - selector includes starting: prefix *)
  | Style.Starting ->
      let modified_class = "starting:" ^ base_class in
      starting_style ~selector:(Css.Selector.Class modified_class) ~props
        ~base_class:modified_class ()
  (* Interactive pseudo-classes *)
  | Style.Hover | Style.Focus | Style.Active | Style.Focus_within
  | Style.Focus_visible | Style.Disabled ->
      handle_pseudo_class_modifier modifier base_class selector props
  (* Pseudo-elements ::before and ::after - add content property if not already
     set by the inner utility *)
  | Style.Pseudo_before | Style.Pseudo_after ->
      let sel = Modifiers.to_selector modifier base_class in
      (* Check if any prop already sets the content property *)
      let has_content_prop =
        List.exists (fun decl -> Css.declaration_name decl = "content") props
      in
      let final_props =
        if has_content_prop then props
        else
          let content_ref = Var.reference Typography.content_var in
          let content_decl = Css.content (Var content_ref) in
          content_decl :: props
      in
      regular ~selector:sel ~props:final_props ~base_class ()
  (* Fallback for other modifiers *)
  | _ -> handle_fallback_modifier modifier base_class props

(* Extract selector and properties from a single Utility *)
(* Apply modifier to extracted rule *)
let apply_modifier_to_rule modifier = function
  | Regular { selector; props; base_class; has_hover; _ } ->
      let bc = Option.value base_class ~default:"" in
      [ modifier_to_rule ~inner_has_hover:has_hover modifier bc selector props ]
  | Media_query
      { condition = inner_condition; selector; props; base_class; nested; _ }
    -> (
      (* Wrap media query in another media query for stacked modifiers like
         contrast-more:dark:text-white. The structure is: @media
         (outer-condition) { @media (inner-condition) {
         .contrast-more\:dark\:text-white { props } nested... } } The selector
         needs to be updated to include the outer modifier prefix. *)
      let bc = Option.value base_class ~default:"" in
      let modified_base_selector = Modifiers.to_selector modifier bc in
      let modified_class =
        Rules_selector.extract_modified_class_name modified_base_selector bc
      in
      let new_selector =
        Rules_selector.transform_selector_with_modifier modified_base_selector
          bc modified_class selector
      in
      let inner_rule = Css.rule ~selector:new_selector props in
      let inner_media =
        Css.media ~condition:inner_condition (inner_rule :: nested)
      in
      match modifier with
      | Style.Dark ->
          [
            media_query ~condition:(Css.Media.Prefers_color_scheme `Dark)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Contrast_more ->
          [
            media_query ~condition:(Css.Media.Prefers_contrast `More)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Contrast_less ->
          [
            media_query ~condition:(Css.Media.Prefers_contrast `Less)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Motion_safe ->
          [
            media_query
              ~condition:(Css.Media.Prefers_reduced_motion `No_preference)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Motion_reduce ->
          [
            media_query ~condition:(Css.Media.Prefers_reduced_motion `Reduce)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Print ->
          [
            media_query ~condition:Css.Media.Print ~selector:new_selector
              ~props:[] ?base_class ~nested:[ inner_media ] ();
          ]
      | Style.Portrait ->
          [
            media_query ~condition:(Css.Media.Orientation `Portrait)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Landscape ->
          [
            media_query ~condition:(Css.Media.Orientation `Landscape)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | Style.Forced_colors ->
          [
            media_query ~condition:(Css.Media.Forced_colors `Active)
              ~selector:new_selector ~props:[] ?base_class
              ~nested:[ inner_media ] ();
          ]
      | _ ->
          (* For other modifiers, return unchanged *)
          [
            Media_query
              {
                condition = inner_condition;
                selector;
                props;
                base_class;
                nested;
              };
          ])
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

let outputs util =
  let rec extract_with_class class_name util_inner = function
    | Style.Style { props; rules; _ } -> (
        let sel = Css.Selector.Class class_name in
        match rules with
        | None ->
            (* Do not emit empty marker classes like .group or .peer *)
            if props = [] then []
            else [ regular ~selector:sel ~props ~base_class:class_name () ]
        | Some rule_list ->
            (* Extract nested at-rules (CSS nesting) for the base rule. Both
               @media and @supports blocks are kept nested inside the rule. *)
            let nested_atrules =
              rule_list
              |> List.filter (fun s ->
                  Css.is_nested_media s || Css.is_nested_supports s)
            in
            (* Process rules in order, preserving original sequence. This keeps
               media rules adjacent to their related state rules (e.g., @media
               (forced-colors:active) right after :checked state). *)
            let has_regular_rules = ref false in
            let ordered_rules =
              rule_list
              |> List.filter_map (fun stmt ->
                  if Css.is_nested_media stmt || Css.is_nested_supports stmt
                  then None
                  else
                    match Css.as_rule stmt with
                    | Some (selector, declarations, nested) ->
                        has_regular_rules := true;
                        (* Replace placeholder selector "_" with the actual
                           utility class selector *)
                        let actual_selector =
                          if Css.Selector.to_string selector = "._" then sel
                          else selector
                        in
                        Some
                          [
                            regular ~selector:actual_selector
                              ~props:declarations ~base_class:class_name ~nested
                              ();
                          ]
                    | None -> (
                        match Css.as_media stmt with
                        | Some (condition, statements) ->
                            statements
                            |> List.filter_map (fun inner ->
                                match Css.as_rule inner with
                                | Some (selector, declarations, _) ->
                                    (* Replace placeholder selector "_" with the
                                       actual utility class selector *)
                                    let actual_selector =
                                      if Css.Selector.to_string selector = "._"
                                      then sel
                                      else selector
                                    in
                                    Some
                                      (media_query ~condition
                                         ~selector:actual_selector
                                         ~props:declarations
                                         ~base_class:class_name ())
                                | None -> None)
                            |> fun l -> Some l
                        | None -> (
                            match Css.as_container stmt with
                            | Some (_, condition, statements) ->
                                statements
                                |> List.filter_map (fun inner ->
                                    match Css.as_rule inner with
                                    | Some (selector, declarations, _) ->
                                        Some
                                          (container_query ~condition ~selector
                                             ~props:declarations
                                             ~base_class:class_name ())
                                    | None -> None)
                                |> fun l -> Some l
                            | None -> (
                                (* Handle @supports with rules inside (top-level
                                   style). Note: is_nested_supports only matches
                                   @supports with bare declarations, so these
                                   won't be filtered above. *)
                                match Css.as_supports stmt with
                                | Some (condition, statements) ->
                                    statements
                                    |> List.filter_map (fun inner ->
                                        match Css.as_rule inner with
                                        | Some (selector, declarations, _) ->
                                            (* Replace placeholder selector "_"
                                               with the actual utility class
                                               selector *)
                                            let actual_selector =
                                              if
                                                Css.Selector.to_string selector
                                                = "._"
                                              then sel
                                              else selector
                                            in
                                            Some
                                              (supports_query ~condition
                                                 ~selector:actual_selector
                                                 ~props:declarations
                                                 ~base_class:class_name ())
                                        | None -> None)
                                    |> fun l -> Some l
                                | None -> None))))
              |> List.concat
            in
            (* Base rule with nested atrules (only if it has props or nested) *)
            let base_rule =
              if props = [] && nested_atrules = [] then []
              else
                [
                  regular ~selector:sel ~props ~base_class:class_name
                    ~nested:nested_atrules ();
                ]
            in
            (* Combine rules with base. If there are regular rules, they should
               come first (forms plugin has interleaved regular/media rules). If
               there are only media rules, base comes first (container). *)
            if !has_regular_rules then ordered_rules @ base_rule
            else base_rule @ ordered_rules)
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
    name. Modifier prefixes come before the utility name. *)
let extract_base_utility class_name_no_pseudo =
  match String.rindex_opt class_name_no_pseudo ':' with
  | Some colon_pos ->
      String.sub class_name_no_pseudo (colon_pos + 1)
        (String.length class_name_no_pseudo - colon_pos - 1)
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
    (* Helper to detect :has() pseudo-class *)
    let rec has_has_pseudo = function
      | Css.Selector.Has _ -> true
      | Css.Selector.Compound sels -> List.exists has_has_pseudo sels
      | Css.Selector.Combined (left, _, right) ->
          has_has_pseudo left || has_has_pseudo right
      | Css.Selector.List sels -> List.exists has_has_pseudo sels
      | Css.Selector.Not sels -> List.exists has_has_pseudo sels
      | Css.Selector.Is sels -> List.exists has_has_pseudo sels
      | Css.Selector.Where sels -> List.exists has_has_pseudo sels
      | _ -> false
    in
    (* Helper to detect aria-* attributes *)
    let rec has_aria_attr = function
      | Css.Selector.Attribute (_, Css.Selector.Aria _, _, _) -> true
      | Css.Selector.Compound sels -> List.exists has_aria_attr sels
      | Css.Selector.Combined (left, _, right) ->
          has_aria_attr left || has_aria_attr right
      | Css.Selector.List sels -> List.exists has_aria_attr sels
      | Css.Selector.Not sels -> List.exists has_aria_attr sels
      | Css.Selector.Is sels -> List.exists has_aria_attr sels
      | Css.Selector.Where sels -> List.exists has_aria_attr sels
      | Css.Selector.Has sels -> List.exists has_aria_attr sels
      | _ -> false
    in
    let has_group = Css.Selector.has_group_marker sel_to_classify in
    let has_peer = Css.Selector.has_peer_marker sel_to_classify in
    let has_has = has_has_pseudo sel_to_classify in
    Complex
      {
        has_focus = Css.Selector.has_focus sel_to_classify;
        has_focus_within = Css.Selector.has_focus_within sel_to_classify;
        has_focus_visible = Css.Selector.has_focus_visible sel_to_classify;
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
    type-directed dispatch based on selector classification. Pseudo-elements,
    simple, and complex selectors are all sorted by priority/suborder/index,
    with pseudo-elements coming before complex at same priority/suborder. *)
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
          (* Pseudo-elements before complex selectors *)
          -1
      | Simple, Pseudo_element ->
          (* At same priority/suborder, preserve source order via index *)
          Int.compare i1 i2
      | Simple, Complex _ -> -1 (* Simple before complex *)
      | Complex _, Pseudo_element ->
          (* Complex after pseudo-elements *)
          1
      | Complex _, Simple -> 1
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

let compare_media_rules typ1 typ2 sel1 sel2 order1 order2 i1 i2 nested1 nested2
    =
  (* First, check nesting status - nested/compound media queries (like
     contrast-more:dark:text-white) should come after all simple media queries,
     regardless of their group order. This matches Tailwind v4 behavior where
     simple media queries (hover, motion-*, contrast-more, md, dark) all come
     before compound queries (contrast-more > dark nested). *)
  let has_nested1 = nested1 <> [] in
  let has_nested2 = nested2 <> [] in
  let nested_cmp = Bool.compare has_nested1 has_nested2 in
  if nested_cmp <> 0 then nested_cmp
  else
    (* Both are nested or both are simple - compare by group *)
    let group1, sub1 = extract_media_sort_key typ1 in
    let group2, sub2 = extract_media_sort_key typ2 in
    let key_cmp = Int.compare group1 group2 in
    if key_cmp <> 0 then key_cmp
    else
      (* Same media group. For Responsive (2000), sort by rem value (ascending).
         For Preference_accessibility (1000), use preference_condition_order.
         For Preference_appearance (3000) and others, use Css.Media.compare. *)
      let cond1 = match typ1 with `Media c -> Some c | _ -> None in
      let cond2 = match typ2 with `Media c -> Some c | _ -> None in
      let cond_cmp = compare_media_conditions group1 sub1 sub2 cond1 cond2 in
      if cond_cmp <> 0 then cond_cmp
      else
        (* Same media condition - compare by priority/suborder first to match
           Tailwind's ordering. Within the same media query (e.g., @media
           (min-width:48rem)), utilities are sorted by priority/suborder, not by
           selector complexity. For example, md:space-y-6 (priority 17) comes
           before md:p-6 (priority 21) even though space-y has a complex
           :where() selector. *)
        compare_by_priority_suborder_alpha sel1 sel2 order1 order2 i1 i2

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
        String.contains sel_str ':'
        && (Str.string_match (Str.regexp ".*:active$") sel_str 0
           || Str.string_match (Str.regexp ".*:disabled$") sel_str 0)
    | _ -> false

(** Check if a selector is a focus: modifier rule (has :focus pseudo-class and
    modifier colon). These rules come AFTER hover:hover media but BEFORE other
    modifier-prefixed media like motion-safe:/motion-reduce:/contrast-more:. *)
let is_focus_modifier_rule sel_kind selector =
  Css.Selector.contains_modifier_colon selector
  && match sel_kind with Complex { has_focus = true; _ } -> true | _ -> false

(** Compare Regular vs Media rules from different utilities. Uses selector
    classification to determine ordering. *)
let compare_different_utility_regular_media sel1 sel2 order1 order2 media_type =
  let kind1 = classify_selector sel1 in
  (* Check if regular rule is a focus: modifier rule *)
  let is_focus = is_focus_modifier_rule kind1 sel1 in
  (* Check if this is truly modifier-prefixed media (like dark:hover:,
     motion-safe:hover:) vs plain media with modifiers in the class name (like
     hover:bg in @media (hover:hover)) *)
  let is_modifier_prefixed_media =
    Css.Selector.contains_modifier_colon sel2
    &&
    match media_type with
    | Some
        ( Css.Media.Prefers_color_scheme _ | Css.Media.Prefers_reduced_motion _
        | Css.Media.Prefers_contrast _ ) ->
        true
    | _ -> false
  in
  (* Check focus modifier rules FIRST - they should come after ALL media
     queries *)
  if is_focus then
    (* ALL focus: utilities come AFTER all media queries (hover, responsive,
       preference) *)
    1
  else if is_modifier_prefixed_media then
    (* Modifier-prefixed media (like motion-safe:, dark:, contrast-more:) *)
    if is_late_modifier kind1 sel1 then
      (* Late modifiers (group-has, peer-has, focus-within, etc.) come after *)
      1
    else
      (* Other regular rules come before modifier-prefixed media *)
      -1
  else
    (* Plain/built-in media (e.g., container breakpoints, hover:hover without
       modifier prefix). Check if this is truly built-in media (no modifier
       colon in selector) vs modifier-based responsive media (like md:). *)
    let has_modifier_colon = Css.Selector.contains_modifier_colon sel2 in
    let p1, s1 = order1 in
    let p2, s2 = order2 in

    if not has_modifier_colon then
      (* Built-in media (e.g., container breakpoints) - respect priority *)
      let prio_cmp = Int.compare p1 p2 in
      if prio_cmp <> 0 then prio_cmp
      else
        (* Same priority - Regular before its media variants *)
        match media_type with
        | Some (Css.Media.Min_width _ | Css.Media.Hover) -> -1
        | _ ->
            let sub_cmp = Int.compare s1 s2 in
            if sub_cmp <> 0 then sub_cmp else -1
    else if
      (* Modifier-based responsive/hover media (md:, lg:, hover:, group-hover:) *)
      (* Check focus FIRST before media type to ensure all focus utilities group together *)
      is_focus
    then
      (* ALL focus: utilities come AFTER hover:hover and responsive media,
         regardless of their underlying utility's priority *)
      match media_type with
      | Some (Css.Media.Hover | Css.Media.Min_width _) -> 1
      | _ ->
          (* For other media types, compare by priority *)
          let prio_cmp = Int.compare p1 p2 in
          if prio_cmp <> 0 then prio_cmp
          else
            let sub_cmp = Int.compare s1 s2 in
            if sub_cmp <> 0 then sub_cmp else -1
    else
      match media_type with
      | Some Css.Media.Hover ->
          (* For modifier-based hover media (hover:, group-hover:), Regular
             utilities always come first, regardless of priority. *)
          -1
      | Some (Css.Media.Min_width _) ->
          (* For responsive media (md:, lg:), Regular always comes before Media
             when comparing different utilities *)
          -1
      | _ ->
          (* For other media types, compare by priority *)
          let prio_cmp = Int.compare p1 p2 in
          if prio_cmp <> 0 then prio_cmp
          else
            let sub_cmp = Int.compare s1 s2 in
            if sub_cmp <> 0 then sub_cmp else -1

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

let compare_by_priority_suborder_baseclass_index r1 r2 =
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

let compare_late_modifiers r1 r2 kind1 kind2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2
  else compare_by_priority_suborder_baseclass_index r1 r2

let compare_focus_modifiers r1 r2 =
  let outline1 = is_outline_utility r1.base_class in
  let outline2 = is_outline_utility r2.base_class in
  if outline1 && not outline2 then 1
  else if outline2 && not outline1 then -1
  else compare_by_priority_suborder_baseclass_index r1 r2

let compare_cross_utility_regular r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let kind1 = classify_selector r1.selector in
  let kind2 = classify_selector r2.selector in
  if !debug_compare then (
    Format.eprintf "compare_cross_prio: %s (%d,%d) vs %s (%d,%d)@."
      (Css.Selector.to_string r1.selector)
      p1 s1
      (Css.Selector.to_string r2.selector)
      p2 s2;
    Format.eprintf "compare_cross_kind: %s (%s) vs %s (%s)@."
      (Css.Selector.to_string r1.selector)
      (match kind1 with
      | Simple -> "Simple"
      | Pseudo_element -> "Pseudo_element"
      | Complex _ -> "Complex")
      (Css.Selector.to_string r2.selector)
      (match kind2 with
      | Simple -> "Simple"
      | Pseudo_element -> "Pseudo_element"
      | Complex _ -> "Complex"));
  (* Check pseudo-elements BEFORE priority *)
  match compare_pseudo_elements kind1 kind2 r1.selector r2.selector with
  | Some cmp -> cmp
  | None ->
      (* Check for focus-visible modifiers BEFORE priority *)
      let focus_visible1 =
        is_late_modifier kind1 r1.selector
        &&
        match kind1 with
        | Complex { has_focus_visible = true; _ } -> true
        | _ -> false
      in
      let focus_visible2 =
        is_late_modifier kind2 r2.selector
        &&
        match kind2 with
        | Complex { has_focus_visible = true; _ } -> true
        | _ -> false
      in
      (* Check for state-modified rules *)
      let state1 = is_state_modifier_rule kind1 r1.selector in
      let state2 = is_state_modifier_rule kind2 r2.selector in

      (* Order: focus-visible < state modifiers *)
      if focus_visible1 && state2 then -1 (* focus-visible before state *)
      else if state1 && focus_visible2 then 1 (* state after focus-visible *)
      else if focus_visible1 && (not focus_visible2) && not state2 then 1
      else if focus_visible2 && (not focus_visible1) && not state1 then -1
      else if focus_visible1 && focus_visible2 then
        compare_by_priority_suborder_baseclass_index r1 r2
      else if state1 && not state2 then 1
      else if state2 && not state1 then -1
      else if state1 && state2 then
        compare_by_priority_suborder_baseclass_index r1 r2
      else
        (* Check for other focus-modified rules BEFORE priority - they always
           come late *)
        let focus1 = is_focus_modifier_rule kind1 r1.selector in
        let focus2 = is_focus_modifier_rule kind2 r2.selector in
        if focus1 && not focus2 then 1
        else if focus2 && not focus1 then -1
        else if focus1 && focus2 then compare_focus_modifiers r1 r2
        else
          (* Compare by priority *)
          let prio_cmp = Int.compare p1 p2 in
          if prio_cmp <> 0 then prio_cmp
          else
            (* Same priority - compare by suborder *)
            let sub_cmp = Int.compare s1 s2 in
            if sub_cmp <> 0 then sub_cmp
            else
              (* When priority and suborder are equal, check late modifiers
                 first *)
              let late1 = is_late_modifier kind1 r1.selector in
              let late2 = is_late_modifier kind2 r2.selector in
              if late1 && not late2 then 1
              else if late2 && not late1 then -1
              else if late1 && late2 then
                compare_late_modifiers r1 r2 kind1 kind2
              else
                (* Not late modifiers - sort alphabetically by selector name. In
                   Tailwind v4, utilities with the same priority AND suborder
                   are sorted alphabetically by selector name. *)
                String.compare
                  (Css.Selector.to_string r1.selector)
                  (Css.Selector.to_string r2.selector)

(** Compare two Regular rules using rule relationship dispatch. *)
let compare_regular_rules r1 r2 =
  let rel = rule_relationship r1 r2 in
  if !debug_compare then
    Format.eprintf "compare_regular: %s vs %s -> %s@."
      (Css.Selector.to_string r1.selector)
      (Css.Selector.to_string r2.selector)
      (match rel with
      | Same_utility bc -> "Same:" ^ bc
      | Different_utilities -> "Different");
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

(** Compare indexed rules for sorting. Uses type-directed dispatch based on
    rule_type. *)
let compare_indexed_rules r1 r2 =
  if !debug_compare then
    Format.eprintf "compare_indexed: %s vs %s (types: %s/%s)@."
      (Css.Selector.to_string r1.selector)
      (Css.Selector.to_string r2.selector)
      (match r1.rule_type with
      | `Regular -> "R"
      | `Media _ -> "M"
      | `Container _ -> "C"
      | `Starting -> "S"
      | `Supports _ -> "U")
      (match r2.rule_type with
      | `Regular -> "R"
      | `Media _ -> "M"
      | `Container _ -> "C"
      | `Starting -> "S"
      | `Supports _ -> "U");
  (* First compare by rule type group *)
  let type_cmp =
    Int.compare (rule_type_order r1.rule_type) (rule_type_order r2.rule_type)
  in
  if type_cmp <> 0 then type_cmp
  else
    (* Same rule type group - dispatch to specialized comparators *)
    match (r1.rule_type, r2.rule_type) with
    | `Regular, `Regular -> compare_regular_rules r1 r2
    | `Media _, `Media _ ->
        compare_media_rules r1.rule_type r2.rule_type r1.selector r2.selector
          r1.order r2.order r1.index r2.index r1.nested r2.nested
    | `Regular, `Media _ -> compare_regular_vs_media r1 r2
    | `Media _, `Regular -> -compare_regular_vs_media r2 r1
    | `Starting, `Starting -> compare_starting_rules r1 r2
    | `Container _, `Container _ -> Int.compare r1.index r2.index
    (* Supports rules should come after their base rule *)
    | `Regular, `Supports _ -> -1
    | `Supports _, `Regular -> 1
    | `Supports _, `Supports _ -> Int.compare r1.index r2.index
    | _, _ -> Int.compare r1.index r2.index

(* Filter properties to only include utilities layer declarations *)
let filter_utility_properties props =
  List.filter
    (fun decl ->
      match Css.custom_declaration_layer decl with
      | Some layer when layer = "utilities" -> true
      | Some _ -> false
      | None -> (
          match Css.custom_declaration_name decl with
          | None -> true
          | Some _ -> false))
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

(* Convert indexed rule to CSS statement *)
let indexed_rule_to_statement r =
  let filtered_props = filter_utility_properties r.props in
  let filtered_nested = filter_theme_from_statements r.nested in
  match r.rule_type with
  | `Regular ->
      Css.rule ~selector:r.selector ~nested:filtered_nested filtered_props
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
      else Css.media ~condition [ Css.rule ~selector:r.selector filtered_props ]
  | `Container condition ->
      Css.container ~condition [ Css.rule ~selector:r.selector filtered_props ]
  | `Supports condition ->
      Css.supports ~condition [ Css.rule ~selector:r.selector filtered_props ]

(* Deduplicate typed triples while preserving first occurrence order *)
let deduplicate_typed_triples triples =
  let seen = Hashtbl.create (List.length triples) in
  List.filter
    (fun (typ, sel, props, _order, nested, _base_class) ->
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

(* Convert each rule type to typed triple *)
let rule_to_triple = function
  | Regular { selector; props; base_class; nested; has_hover } ->
      if has_hover then
        (* Hover rules become Media rules with (hover:hover) condition *)
        Some
          ( `Media Css.Media.Hover,
            selector,
            props,
            order_of_base base_class selector,
            nested,
            base_class )
      else
        Some
          ( `Regular,
            selector,
            props,
            order_of_base base_class selector,
            nested,
            base_class )
  | Media_query { condition; selector; props; base_class; nested } ->
      Some
        ( `Media condition,
          selector,
          props,
          order_of_base base_class selector,
          nested,
          base_class )
  | Container_query { condition; selector; props; base_class } ->
      Some
        ( `Container condition,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )
  | Starting_style { selector; props; base_class } ->
      Some
        ( `Starting,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )
  | Supports_query { condition; selector; props; base_class } ->
      Some
        ( `Supports condition,
          selector,
          props,
          order_of_base base_class selector,
          [],
          base_class )

(* Add index to each triple for stable sorting *)
let add_index triples =
  List.mapi
    (fun i (typ, sel, props, order, nested, base_class) ->
      {
        index = i;
        rule_type = typ;
        selector = sel;
        props;
        order;
        nested;
        base_class;
      })
    triples

(* Convert selector/props pairs to CSS rules. *)
(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  (* All rules (including hover) are now sorted together. Hover rules are
     converted to Media "(hover:hover)" rules in rule_to_triple, so they
     participate in the normal media query sorting. *)
  all_rules
  |> List.filter_map rule_to_triple
  |> deduplicate_typed_triples |> add_index
  |> List.sort compare_indexed_rules
  |> List.map indexed_rule_to_statement

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
    match Var.get_property_order name_without_prefix with
    | Some o -> o
    | None -> 1000 (* Default for vars without property_order *)
  in
  List.sort (fun n1 n2 -> compare (get_order n1) (get_order n2)) vars

(* Extract all var names from sorted indexed rules in utility order. For each
   utility, collects: 1. Vars that are SET (custom declarations) 2. Vars that
   are REFERENCED and need @property (e.g., transform refs rotate/skew) Within
   each utility, vars are sorted by property_order to ensure consistent family
   ordering (e.g., ring before inset-ring regardless of CSS value order). *)
let all_var_names_from_sorted_rules sorted_rules =
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
            Var.get_needs_property name)
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

(* Sort declarations by their Var order metadata *)
let sort_by_var_order decls =
  decls
  |> List.map (fun d -> (d, Var.order_of_declaration d))
  |> List.sort (fun (_, a) (_, b) -> compare_orders a b)
  |> List.map fst

(* Build theme layer rule from declarations *)
let theme_layer_rule ~layers = function
  | [] -> if layers then Css.v [ Css.layer ~name:"theme" [] ] else Css.empty
  | decls ->
      let selector = Css.Selector.(list [ Root; host () ]) in
      let rule = Css.rule ~selector decls in
      if layers then Css.v [ Css.layer ~name:"theme" [ rule ] ]
      else Css.v [ rule ]

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let compute_theme_layer_from_selector_props ?(layers = true)
    ?(default_decls = []) selector_props =
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
  compute_theme_layer_from_selector_props ~default_decls selector_props

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
      ~condition:(Css.Supports.Raw "(color:color-mix(in lab, red, red))")
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
          (Css.Supports.Raw
             "(not ((-webkit-appearance:-apple-pay-button))) or \
              (contain-intrinsic-size:1px)")
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

(* Browser detection condition for properties layer. Uses Raw with spaces - the
   pp function strips spaces when minifying. *)
let browser_detection =
  Css.Supports.Raw
    "(((-webkit-hyphens: none)) and (not (margin-trim: inline))) or \
     ((-moz-orient: inline) and (not (color: rgb(from red r g b))))"

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
  match Var.get_property_order name with
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
      match Var.get_family name with
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
      ( `Gradient | `Translate | `Rotate | `Skew | `Scale | `Font_weight
      | `Leading ) ->
      false (* Transforms, gradient, and typography use first-usage order *)
  | Some _ -> true (* All other families use property_order *)
  | None -> true (* Variables without families also use property_order *)

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
    match Var.get_family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  let cmp (n1, _) (n2, _) =
    let fam1 = Var.get_family n1 in
    let fam2 = Var.get_family n2 in
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
let rec extract_vars_and_property_rules_from_style = function
  | Style.Style { props; rules; property_rules; _ } ->
      let vars_from_props = Css.vars_of_declarations props in
      let vars_from_rules =
        match rules with Some r -> Css.vars_of_rules r | None -> []
      in
      let set_names = set_var_names_from_props props in
      (vars_from_props @ vars_from_rules, set_names, [ property_rules ])
  | Style.Modified (_, t) -> extract_vars_and_property_rules_from_style t
  | Style.Group ts ->
      let results = List.map extract_vars_and_property_rules_from_style ts in
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
  List.filter (fun (Css.V v) -> Var.var_needs_property v) vars

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
  (* Sort @property rules using family-based first-usage order *)
  let family_order = build_family_order first_usage_order in
  let get_family_order name =
    match Var.get_family name with
    | Some fam -> (
        match Hashtbl.find_opt family_order fam with
        | Some o -> o
        | None -> 1000)
    | None -> 1000
  in
  (* Check if a family uses property_order directly *)
  let uses_direct_property_order = function
    | Some (`Gradient | `Translate | `Rotate | `Skew | `Font_weight | `Leading)
      ->
        false (* Transforms and typography use first-usage order *)
    | Some _ ->
        true (* All other families (including Scale) use property_order *)
    | None -> true (* Variables without families also use property_order *)
  in
  let sorted_property_rules =
    property_rules_for_end
    |> List.sort (fun s1 s2 ->
        match (Css.as_property s1, Css.as_property s2) with
        | ( Some (Css.Property_info { name = n1; _ }),
            Some (Css.Property_info { name = n2; _ }) ) ->
            let fam1 = Var.get_family n1 in
            let fam2 = Var.get_family n2 in
            let po1 = property_order_from n1 in
            let po2 = property_order_from n2 in
            (* Variables with no family and negative property_order come
               first *)
            let no_family_negative_first =
              match (fam1, fam2) with
              | None, Some _ when po1 < 0 -> -1
              | Some _, None when po2 < 0 -> 1
              | _ -> 0
            in
            if no_family_negative_first <> 0 then no_family_negative_first
            else if
              uses_direct_property_order fam1 && uses_direct_property_order fam2
            then
              (* These families use property_order directly *)
              compare po1 po2
            else
              let fo1 = get_family_order n1 in
              let fo2 = get_family_order n2 in
              if fo1 <> fo2 then compare fo1 fo2 else compare po1 po2
        | _ -> 0)
  in
  let property_rules_css =
    if sorted_property_rules = [] then [] else [ Css.v sorted_property_rules ]
  in
  (* Convert keyframes to statements - preserve first-usage order (matching
     Tailwind) and deduplicate by name (keeping first occurrence) *)
  let dedup_keyframes kfs =
    let seen = Hashtbl.create 8 in
    List.filter
      (fun (name, _) ->
        if Hashtbl.mem seen name then false
        else (
          Hashtbl.add seen name ();
          true))
      kfs
  in
  let keyframes_stmts =
    keyframes |> dedup_keyframes
    |> List.map (fun (name, frames) -> Css.keyframes name frames)
  in
  let keyframes_css =
    if keyframes_stmts = [] then [] else [ Css.v keyframes_stmts ]
  in
  layers_without_property @ property_rules_css @ keyframes_css

(* Extract variables, set var names, and property rules from all utilities *)
let extract_vars_and_rules utilities =
  let styles = List.map Utility.to_style utilities in
  let results = List.map extract_vars_and_property_rules_from_style styles in
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

(* Detect if focus:ring + contrast-more combination is used *)
let has_focus_ring_and_contrast_more tw_classes =
  let has_focus_ring =
    let rec check_utility = function
      | Utility.Modified (Style.Focus, Utility.Base u) ->
          let class_name = Utility.class_of_base u in
          class_name = "ring" || check_utility (Utility.Base u)
      | Utility.Modified (_, u) -> check_utility u
      | Utility.Group us -> List.exists check_utility us
      | Utility.Base u ->
          let class_name = Utility.class_of_base u in
          class_name = "ring"
    in
    List.exists check_utility tw_classes
  in
  let has_contrast_more =
    let rec check_modifier = function
      | Utility.Modified (Style.Contrast_more, _) -> true
      | Utility.Modified (_, u) -> check_modifier u
      | Utility.Group us -> List.exists check_modifier us
      | Utility.Base _ -> false
    in
    List.exists check_modifier tw_classes
  in
  has_focus_ring && has_contrast_more

let build_individual_layers ~layers ~include_base ~forms_base first_usage_order
    selector_props all_property_statements statements =
  (* Only include font family defaults when base is enabled - these are defaults
     that should not appear in bare utility-only output *)
  let theme_defaults =
    if include_base then Typography.default_font_family_declarations else []
  in
  let theme_layer =
    compute_theme_layer_from_selector_props ~layers
      ~default_decls:theme_defaults selector_props
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
     all_var_names_from_sorted_rules). Across utilities, we preserve first-usage
     order to match Tailwind's behavior. *)
  let all_vars = all_var_names_from_sorted_rules sorted_rules in
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
  (* Extract keyframes from all utilities and sort by theme variable order.
     Keyframes like "spin"/"pulse"/"bounce" are associated with theme variables
     "animate-spin"/"animate-pulse"/"animate-bounce" that have explicit
     (priority, suborder) tuples registered. Convert to integer for
     comparison. *)
  let keyframes =
    List.fold_left collect_keyframes [] styles
    |> List.rev
    |> List.sort (fun (name1, _) (name2, _) ->
        let var_name1 = "animate-" ^ name1 in
        let var_name2 = "animate-" ^ name2 in
        let order1 =
          match Var.get_order var_name1 with
          | Some (p, s) -> (p * 1000) + s
          | None -> 1000000 (* Unknown keyframes sort last *)
        in
        let order2 =
          match Var.get_order var_name2 with
          | Some (p, s) -> (p * 1000) + s
          | None -> 1000000
        in
        let order_cmp = Int.compare order1 order2 in
        if order_cmp <> 0 then order_cmp
        else String.compare name1 name2 (* Stable sort for same order *))
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
  (* When focus:ring + contrast-more are used, include base .ring utility to
     match Tailwind *)
  let tw_classes_with_ring =
    if has_focus_ring_and_contrast_more tw_classes then
      tw_classes @ [ Effects.ring ]
    else tw_classes
  in
  (* Extract once and share for rule sets and theme layer *)
  let selector_props = List.concat_map outputs tw_classes_with_ring in

  let statements = rule_sets_from_selector_props selector_props in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  let stylesheet =
    match config.mode with
    | Css.Variables ->
        let layer_results =
          build_layers ~layers:config.layers ~include_base:config.base
            ?forms:config.forms ~selector_props tw_classes_with_ring statements
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
