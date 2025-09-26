(** CSS rule generation and management

    This module converts Tailwind utility classes into optimized CSS rules. The
    complexity comes from several requirements:

    - Rule Extraction: Transform modifier structures into CSS rules
    - Conflict Resolution: Order utilities by specificity
    - CSS Layers: Generate proper [@layer] directives
    - Variable Resolution: Track CSS custom property dependencies
    - Media/Container Queries: Handle responsive modifiers *)

open Core

(* ======================================================================== *)
(* Types *)
(* ======================================================================== *)

type output =
  | Regular of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option; (* Base class name without the dot *)
      has_hover : bool; (* Track if this rule has hover modifier *)
    }
  | Media_query of {
      condition : string;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Container_query of {
      condition : string;
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }
  | Starting_style of {
      selector : Css.Selector.t;
      props : Css.declaration list;
      base_class : string option;
    }

type by_type = {
  regular : output list;
  media : output list;
  container : output list;
  starting : output list;
}

(* ======================================================================== *)
(* Smart constructors for output *)
(* ======================================================================== *)

let regular ~selector ~props ?base_class ?(has_hover = false) () =
  Regular { selector; props; base_class; has_hover }

let media_query ~condition ~selector ~props ?base_class () =
  Media_query { condition; selector; props; base_class }

let container_query ~condition ~selector ~props ?base_class () =
  Container_query { condition; selector; props; base_class }

let starting_style ~selector ~props ?base_class () =
  Starting_style { selector; props; base_class }

(* ======================================================================== *)
(* Basic Utilities *)
(* ======================================================================== *)

(* String manipulation helpers *)
let drop_prefix prefix s =
  if String.starts_with ~prefix s then
    let lp = String.length prefix in
    let ls = String.length s in
    String.sub s lp (ls - lp)
  else s

let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

let responsive_breakpoint = function
  | "sm" -> "40rem" (* 640px / 16 = 40rem *)
  | "md" -> "48rem" (* 768px / 16 = 48rem *)
  | "lg" -> "64rem" (* 1024px / 16 = 64rem *)
  | "xl" -> "80rem" (* 1280px / 16 = 80rem *)
  | "2xl" -> "96rem" (* 1536px / 16 = 96rem *)
  | _ -> "0rem"

(* Small memoization for escaping, as many utilities reuse the same base class
   names across modifiers. *)
let escape_cache : (string, string) Hashtbl.t = Hashtbl.create 256

let escape_class_name name =
  match Hashtbl.find_opt escape_cache name with
  | Some v -> v
  | None ->
      (* Escape special CSS selector characters for Tailwind class names. This
         covers the common characters used in Tailwind utilities like arbitrary
         values (p-[10px]), responsive prefixes (sm:p-4), fractions (w-1/2), and
         other special cases. Note: This is not a complete CSS.escape
         implementation but handles all characters typically found in Tailwind
         class names. *)
      let buf = Buffer.create (String.length name * 2) in
      String.iter
        (function
          | '[' -> Buffer.add_string buf "\\["
          | ']' -> Buffer.add_string buf "\\]"
          | '(' -> Buffer.add_string buf "\\("
          | ')' -> Buffer.add_string buf "\\)"
          | ',' -> Buffer.add_string buf "\\,"
          | '/' -> Buffer.add_string buf "\\/"
          | ':' -> Buffer.add_string buf "\\:"
          | '%' -> Buffer.add_string buf "\\%"
          | '.' -> Buffer.add_string buf "\\."
          | '#' -> Buffer.add_string buf "\\#"
          | ' ' -> Buffer.add_string buf "\\ "
          | '"' -> Buffer.add_string buf "\\\""
          | '\'' -> Buffer.add_string buf "\\'"
          | '@' -> Buffer.add_string buf "\\@"
          | c -> Buffer.add_char buf c)
        name;
      let escaped = Buffer.contents buf in
      Hashtbl.add escape_cache name escaped;
      escaped

(* ======================================================================== *)
(* Rule Extraction - Convert Core.t to CSS rules *)
(* ======================================================================== *)

let selector_with_data_key selector key value =
  let attr_selector = Css.Selector.attribute key (Exact value) in
  Css.Selector.combine selector Descendant attr_selector

let media_modifier ~condition ~prefix base_class props =
  let selector_str = prefix ^ escape_class_name base_class in
  (* For now, create a simple class selector - this needs proper parsing *)
  let selector =
    Css.Selector.class_
      (String.sub selector_str 1 (String.length selector_str - 1))
  in
  media_query ~condition ~selector ~props ~base_class ()

let responsive_rule breakpoint base_class props =
  let prefix = string_of_breakpoint breakpoint in
  let condition = "(min-width:" ^ responsive_breakpoint prefix ^ ")" in
  (* Prefixes like sm/md/lg/xl/2xl are safe identifiers; skip escaping. *)
  let sel = prefix ^ "\\:" ^ escape_class_name base_class in
  let selector = Css.Selector.class_ sel in
  media_query ~condition ~selector ~props ~base_class ()

let container_rule query base_class props =
  let prefix = Containers.container_query_to_class_prefix query in
  let escaped_prefix = escape_class_name prefix in
  let escaped_class = escaped_prefix ^ "\\:" ^ escape_class_name base_class in
  let selector = Css.Selector.class_ escaped_class in
  let condition = Containers.container_query_to_css_prefix query in
  let cond =
    if String.starts_with ~prefix:"@container " condition then
      drop_prefix "@container " condition
    else "(min-width: 0)"
  in
  container_query ~condition:cond ~selector ~props ~base_class ()

let has_like_selector kind selector_str base_class props =
  let open Css.Selector in
  let escaped_selector = escape_class_name selector_str in
  (* Parse the selector string to get a proper selector *)
  let parsed_selector =
    let reader = Css.Reader.of_string selector_str in
    try Css.Selector.read reader with
    | Css.Reader.Parse_error _ ->
        (* Fallback to a simple class selector if parsing fails *)
        class_ selector_str
    | Invalid_argument _ ->
        (* Invalid identifier; degrade gracefully to raw class name *)
        class_ selector_str
  in
  match kind with
  | `Has ->
      let sel =
        compound
          [
            class_
              ("has-\\[" ^ escaped_selector ^ "\\]\\:"
              ^ escape_class_name base_class);
            has [ parsed_selector ];
          ]
      in
      regular ~selector:sel ~props ~base_class ()
  | `Group_has ->
      let left = compound [ class_ "group"; has [ parsed_selector ] ] in
      let right =
        class_
          ("group-has-\\[" ^ escaped_selector ^ "\\]\\:"
          ^ escape_class_name base_class)
      in
      let sel = combine left Descendant right in
      regular ~selector:sel ~props ~base_class ()
  | `Peer_has ->
      let left = compound [ class_ "peer"; has [ parsed_selector ] ] in
      let right =
        class_
          ("peer-has-\\[" ^ escaped_selector ^ "\\]\\:"
          ^ escape_class_name base_class)
      in
      let sel = combine left Subsequent_sibling right in
      regular ~selector:sel ~props ~base_class ()

let modifier_to_rule modifier base_class selector props =
  match modifier with
  | Data_state value ->
      regular
        ~selector:(selector_with_data_key selector "data-state" value)
        ~props ~base_class ()
  | Data_variant value ->
      regular
        ~selector:(selector_with_data_key selector "data-variant" value)
        ~props ~base_class ()
  | Data_custom (key, value) ->
      regular
        ~selector:(selector_with_data_key selector ("data-" ^ key) value)
        ~props ~base_class ()
  | Dark ->
      media_query ~condition:"(prefers-color-scheme: dark)" ~selector ~props
        ~base_class ()
  | Responsive breakpoint -> responsive_rule breakpoint base_class props
  | Container query -> container_rule query base_class props
  | Not _modifier ->
      regular
        ~selector:
          (Css.Selector.class_
             ("not-"
             ^ escape_class_name base_class
             ^ ":not("
             ^ Css.Selector.to_string selector
             ^ ")"))
        ~props ~base_class ()
  | Has selector_str -> has_like_selector `Has selector_str base_class props
  | Group_has selector_str ->
      has_like_selector `Group_has selector_str base_class props
  | Peer_has selector_str ->
      has_like_selector `Peer_has selector_str base_class props
  | Starting ->
      starting_style
        ~selector:(Css.Selector.class_ (escape_class_name base_class))
        ~props ~base_class ()
  | Motion_safe ->
      media_modifier ~condition:"(prefers-reduced-motion: no-preference)"
        ~prefix:".motion-safe\\:" base_class props
  | Motion_reduce ->
      media_modifier ~condition:"(prefers-reduced-motion: reduce)"
        ~prefix:".motion-reduce\\:" base_class props
  | Contrast_more ->
      media_modifier ~condition:"(prefers-contrast: more)"
        ~prefix:".contrast-more\\:" base_class props
  | Contrast_less ->
      media_modifier ~condition:"(prefers-contrast: less)"
        ~prefix:".contrast-less\\:" base_class props
  | Hover | Focus | Active | Focus_within | Focus_visible | Disabled ->
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = Modifiers.is_hover modifier in
      regular ~selector:sel ~props ~base_class ~has_hover ()
  | _ ->
      let sel = Modifiers.to_selector modifier base_class in
      let has_hover = Modifiers.is_hover modifier in
      regular ~selector:sel ~props ~base_class ~has_hover ()

(* Extract selector and properties from a single Tw style *)
let extract_selector_props tw =
  let rec extract = function
    | Style { name; props; rules; _ } -> (
        let escaped_name = escape_class_name name in
        let sel = Css.Selector.class_ escaped_name in
        match rules with
        | None -> [ regular ~selector:sel ~props ~base_class:name () ]
        | Some rule_list ->
            (* Convert custom rules to selector/props pairs *)
            let custom_rules =
              rule_list
              |> List.map (fun rule ->
                     match Css.as_rule rule with
                     | Some (selector, declarations, _) ->
                         regular ~selector ~props:declarations ~base_class:name
                           ()
                     | None ->
                         regular ~selector:Css.Selector.universal ~props:[]
                           ~base_class:name ())
            in

            (* If there are base props, add them after the custom rules to match
               Tailwind's order *)
            if props = [] then custom_rules
            else
              custom_rules
              @ [ regular ~selector:sel ~props ~base_class:name () ])
    | Modified (modifier, t) ->
        let base = extract t in
        List.concat_map
          (fun rule_out ->
            match rule_out with
            | Regular { selector; props; base_class; _ } ->
                (* Use the base_class from the rule, not extract from
                   selector *)
                let bc = Option.value base_class ~default:"" in
                [ modifier_to_rule modifier bc selector props ]
            | _ -> [ rule_out ])
          base
    | Group styles -> List.concat_map extract styles
  in
  extract tw

(* Extract selector and props pairs from Regular rules. *)
let extract_selector_props_pairs rules =
  List.filter_map
    (fun rule ->
      match rule with
      | Regular { selector; props; _ } -> Some (selector, props)
      | _ -> None)
    rules

(* ======================================================================== *)
(* Rule Processing - Group and organize rules *)
(* ======================================================================== *)

let classify all_rules =
  let regular_rules, media_rules, container_rules, starting_rules =
    List.fold_left
      (fun (reg, media, cont, start) rule ->
        match rule with
        | Regular _ -> (rule :: reg, media, cont, start)
        | Media_query _ -> (reg, rule :: media, cont, start)
        | Container_query _ -> (reg, media, rule :: cont, start)
        | Starting_style _ -> (reg, media, cont, rule :: start))
      ([], [], [], []) all_rules
  in
  (* Reverse to maintain original order since we prepended *)
  {
    regular = List.rev regular_rules;
    media = List.rev media_rules;
    container = List.rev container_rules;
    starting = List.rev starting_rules;
  }

let is_hover_rule = function
  | Regular { has_hover; _ } -> has_hover
  | _ -> false

let group_media_queries media_rules =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      match rule with
      | Media_query { condition; selector; props; _ } ->
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props) :: rules)
      | _ -> ())
    media_rules;
  (* Reverse once to restore original insertion order per condition *)
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

let group_container_queries container_rules =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun rule ->
      match rule with
      | Container_query { condition; selector; props; _ } ->
          let rules = try Hashtbl.find tbl condition with Not_found -> [] in
          Hashtbl.replace tbl condition ((selector, props) :: rules)
      | _ -> ())
    container_rules;
  (* Reverse once to restore original insertion order per condition *)
  Hashtbl.fold (fun k v acc -> (k, List.rev v) :: acc) tbl []

(* ======================================================================== *)
(* Conflict Resolution - Order utilities by specificity *)
(* ======================================================================== *)

(* Centralized color ordering - matches Tailwind's color palette order *)
(* Colors are ordered alphabetically for consistency and predictability *)
let color_order_map =
  [
    ("amber", 0);
    ("blue", 1);
    ("cyan", 2);
    ("emerald", 3);
    ("fuchsia", 4);
    ("gray", 5);
    ("green", 6);
    ("indigo", 7);
    ("lime", 8);
    ("neutral", 9);
    ("orange", 10);
    ("pink", 11);
    ("purple", 12);
    ("red", 13);
    ("rose", 14);
    ("sky", 15);
    ("slate", 16);
    ("stone", 17);
    ("teal", 18);
    ("violet", 19);
    ("yellow", 20);
    ("zinc", 21);
  ]

let color_order color_name =
  match List.assoc_opt color_name color_order_map with
  | Some order -> order
  | None -> 100 (* Unknown colors go last *)

(* Utility prefix constants for classification *)
let display_prefixes =
  [
    "block";
    "inline";
    "inline-";
    "flex";
    "grid";
    "table";
    "contents";
    "flow-root";
  ]

let position_prefixes = [ "static"; "fixed"; "absolute"; "relative"; "sticky" ]

let margin_prefixes =
  [
    "m-";
    "-m-";
    "mx-";
    "-mx-";
    "my-";
    "-my-";
    "mt-";
    "-mt-";
    "mr-";
    "-mr-";
    "mb-";
    "-mb-";
    "ml-";
    "-ml-";
  ]

let padding_prefixes = [ "p-"; "px-"; "py-"; "pt-"; "pr-"; "pb-"; "pl-" ]

let typography_prefixes =
  [
    "font-";
    "text-";
    "tracking-";
    "leading-";
    "whitespace-";
    "break-";
    "list-";
    "content-";
  ]

let sizing_prefixes = [ "w-"; "h-"; "min-w-"; "min-h-"; "max-w-"; "max-h-" ]

let effects_prefixes =
  [
    "shadow-";
    "shadow";
    "opacity-";
    "mix-blend-";
    "background-blend-";
    "transform";
    "translate-";
    "scale-";
    "rotate-";
    "skew-";
    "transition";
    "duration-";
    "ease-";
    "delay-";
    "animate-";
  ]

let interactivity_prefixes =
  [ "cursor-"; "select-"; "resize-"; "scroll-"; "overflow-"; "overscroll-" ]

let flexbox_grid_prefixes =
  [
    "flex-";
    "grow";
    "shrink";
    "basis-";
    "order-";
    "grid-cols-";
    "col-";
    "grid-rows-";
    "row-";
    "grid-flow-";
    "auto-cols-";
    "auto-rows-";
  ]

let gap_prefixes = [ "gap-"; "space-" ]

(* Utility classification functions *)
let has_any_prefix prefixes core =
  List.exists (fun p -> String.starts_with ~prefix:p core) prefixes

let is_display_util = has_any_prefix display_prefixes
let is_position_util = has_any_prefix position_prefixes
let is_margin_util = has_any_prefix margin_prefixes
let is_padding_util = has_any_prefix padding_prefixes
let is_typography_util = has_any_prefix typography_prefixes
let is_sizing_util = has_any_prefix sizing_prefixes
let is_effects_util = has_any_prefix effects_prefixes
let is_interactivity_util = has_any_prefix interactivity_prefixes
let is_flexbox_grid_util = has_any_prefix flexbox_grid_prefixes
let is_gap_util = has_any_prefix gap_prefixes

let is_border_util core =
  String.starts_with ~prefix:"rounded" core
  || String.starts_with ~prefix:"border" core
  || String.starts_with ~prefix:"outline-" core

let is_container_or_prose core =
  core = "container" || String.starts_with ~prefix:"prose" core

(* Suborder functions for fine-grained sorting within groups *)
let margin_suborder core =
  if
    String.starts_with ~prefix:"m-" core
    || String.starts_with ~prefix:"-m-" core
  then 0 (* All margins *)
  else if
    String.starts_with ~prefix:"mx-" core
    || String.starts_with ~prefix:"-mx-" core
    || String.starts_with ~prefix:"my-" core
    || String.starts_with ~prefix:"-my-" core
  then 1 (* Axis margins *)
  else 2 (* Individual margins *)

let padding_suborder core =
  if String.starts_with ~prefix:"p-" core then 0 (* All padding *)
  else if
    String.starts_with ~prefix:"px-" core
    || String.starts_with ~prefix:"py-" core
  then 1 (* Axis padding *)
  else 2 (* Individual padding *)

let alignment_suborder core =
  if String.starts_with ~prefix:"items-" core then 0
  else if String.starts_with ~prefix:"justify-" core then 1
  else if has_any_prefix [ "content-"; "self-"; "place-" ] core then 2
  else -1

(* Conflict group classification table Groups are ordered by priority (lower
   number = higher priority) This ordering ensures proper cascade behavior in
   CSS *)
type utility_group = {
  priority : int;
  name : string;
  classifier : string -> bool;
  suborder : string -> int;
}

let utility_groups =
  [
    {
      priority = 10;
      name = "display";
      classifier = (fun c -> c = "hidden" || is_display_util c);
      suborder = (fun c -> if c = "hidden" then 3 else 1);
    };
    {
      priority = 11;
      name = "position";
      classifier = is_position_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 100;
      name = "margin";
      classifier = is_margin_util;
      suborder = margin_suborder;
    };
    {
      priority = 200;
      name = "background";
      classifier =
        (fun c ->
          String.starts_with ~prefix:"bg-" c
          || String.starts_with ~prefix:"from-" c
          || String.starts_with ~prefix:"via-" c
          || String.starts_with ~prefix:"to-" c);
      suborder =
        (fun c ->
          if String.starts_with ~prefix:"bg-" c then
            let color_part = drop_prefix "bg-" c in
            let color_name =
              try
                let last_dash = String.rindex color_part '-' in
                String.sub color_part 0 last_dash
              with Not_found -> color_part
            in
            color_order color_name
          else 50 (* Gradient utilities come after solid colors *));
    };
    {
      priority = 300;
      name = "padding";
      classifier = is_padding_util;
      suborder = padding_suborder;
    };
    {
      priority = 400;
      name = "typography";
      classifier = is_typography_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 500;
      name = "border";
      classifier = is_border_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 600;
      name = "sizing";
      classifier = is_sizing_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 700;
      name = "effects";
      classifier = is_effects_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 800;
      name = "interactivity";
      classifier = is_interactivity_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 900;
      name = "flexbox_grid";
      classifier = is_flexbox_grid_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 901;
      name = "alignment";
      classifier = (fun c -> alignment_suborder c >= 0);
      suborder = alignment_suborder;
    };
    {
      priority = 902;
      name = "gap";
      classifier = is_gap_util;
      suborder = (fun _ -> 0);
    };
    {
      priority = 1000;
      name = "container_prose";
      classifier = is_container_or_prose;
      suborder =
        (fun core ->
          (* Ensure .prose comes before .prose :where(...) *)
          if core = "prose" then 0
          else if String.starts_with ~prefix:"prose " core then 1
          else 2);
    };
  ]

(* Main conflict resolution function *)
let conflict_group selector =
  let core =
    if String.starts_with ~prefix:"." selector then
      String.sub selector 1 (String.length selector - 1)
    else selector
  in

  (* Find the first matching group *)
  match List.find_opt (fun g -> g.classifier core) utility_groups with
  | Some group -> (group.priority, group.suborder core)
  | None -> (9999, 0)
(* Unknown utilities go last *)

let build_utilities_layer ~rules ~media_queries ~container_queries =
  (* Preserve source order to maintain CSS cascade semantics *)
  let statements =
    rules
    @ List.map
        (fun (condition, rules) -> Css.media ~condition rules)
        media_queries
    @ List.map
        (fun (name, condition, rules) -> Css.container ?name ~condition rules)
        container_queries
  in
  Css.of_statements [ Css.layer ~name:"utilities" statements ]

let add_hover_to_media_map hover_rules media_map =
  (* Gate hover rules behind (hover:hover) media query to prevent them from
     applying on touch devices where :hover can stick after tapping. This
     follows modern CSS best practices for hover states. *)
  if hover_rules = [] then media_map
  else
    let hover_condition = "(hover:hover)" in
    (* Update association list in-place to avoid hashtable churn. *)
    let rec update acc = function
      | [] -> List.rev ((hover_condition, hover_rules) :: acc)
      | (cond, rules) :: tl when String.equal cond hover_condition ->
          (* Prepend hover rules to existing for this condition *)
          List.rev_append acc ((hover_condition, hover_rules @ rules) :: tl)
      | hd :: tl -> update (hd :: acc) tl
    in
    update [] media_map

(* Convert selector/props pairs to CSS rules. *)
let of_grouped ?(filter_custom_props = false) grouped_list =
  List.map
    (fun (selector, props) ->
      let filtered_props =
        if filter_custom_props then
          (* In utilities, keep only declarations explicitly tagged for the
             utilities layer via Var metadata. Non-custom declarations are
             always kept. Custom declarations without metadata are dropped. *)
          List.filter
            (fun decl ->
              match Css.custom_declaration_layer decl with
              | Some layer when layer = "utilities" -> true
              | Some _ -> false
              | None -> (
                  (* No fallback to name prefixes: keep only non-custom
                     declarations when metadata is missing. *)
                  match Css.custom_declaration_name decl with
                  | None -> true
                  | Some _ -> false))
            props
        else props
      in
      Css.rule ~selector filtered_props)
    grouped_list

(* Internal: build rule sets from pre-extracted outputs. *)
let rule_sets_from_selector_props all_rules =
  let separated = classify all_rules in
  (* First separate hover from non-hover rules *)
  let hover_regular, non_hover_regular =
    List.partition is_hover_rule separated.regular
  in
  let non_hover_pairs = extract_selector_props_pairs non_hover_regular in
  let hover_pairs = extract_selector_props_pairs hover_regular in
  let rules = of_grouped ~filter_custom_props:true non_hover_pairs in
  let media_queries_map =
    group_media_queries separated.media |> add_hover_to_media_map hover_pairs
  in
  let media_queries =
    List.map
      (fun (condition, rule_list) ->
        (condition, of_grouped ~filter_custom_props:true rule_list))
      media_queries_map
  in
  let container_queries_map = group_container_queries separated.container in
  let container_queries =
    List.map
      (fun (condition, rule_list) ->
        (None, condition, of_grouped ~filter_custom_props:true rule_list))
      container_queries_map
  in
  (rules, media_queries, container_queries)

let rule_sets tw_classes =
  let all_rules = tw_classes |> List.concat_map extract_selector_props in
  rule_sets_from_selector_props all_rules

(* ======================================================================== *)
(* Layer Generation - CSS @layer directives and theme variable resolution *)
(* ======================================================================== *)

module Strings = Set.Make (String)

(* Helpers for theme layer extraction and ordering *)
let collect_selector_props tw_classes =
  List.concat_map extract_selector_props tw_classes

let extract_non_tw_custom_declarations selector_props =
  (* Use Hashtbl to collect unique theme variables efficiently *)
  let theme_vars = Hashtbl.create 32 in
  let insertion_order = ref [] in

  selector_props
  |> List.iter (function
         | Regular { props; _ }
         | Media_query { props; _ }
         | Container_query { props; _ }
         | Starting_style { props; _ }
         ->
         Css.extract_custom_declarations props
         |> List.iter (fun decl ->
                match Css.custom_declaration_layer decl with
                | Some layer when layer = "theme" -> (
                    (* Add to hashtable if not already present *)
                    match Css.custom_declaration_name decl with
                    | Some name when not (Hashtbl.mem theme_vars name) ->
                        Hashtbl.add theme_vars name decl;
                        insertion_order := decl :: !insertion_order
                    | _ -> ())
                | _ -> ()));
  (* Return in original insertion order *)
  List.rev !insertion_order

(* Get Var.any from declaration metadata *)
(* var_of_declaration_meta no longer used after refactor *)

(* assemble_theme_decls_metadata no longer used; ordering handled in
   compute_theme_layer *)

(* Internal helper to compute theme layer from pre-extracted outputs. *)
let compute_theme_layer_from_selector_props ?(default_decls = []) selector_props
    =
  let extracted = extract_non_tw_custom_declarations selector_props in
  (* Split defaults so we can place base font family vars before extracted
     tokens, and the indirection defaults (default-font-family,
     default-mono-font-family) after extracted tokens to match Tailwind's
     order. *)
  let is_default_family name =
    name = "default-font-family" || name = "default-mono-font-family"
  in
  let pre_defaults, post_defaults =
    List.partition
      (fun decl ->
        match Css.custom_declaration_name decl with
        | Some n -> not (is_default_family n)
        | None -> false)
      default_decls
  in
  (* Filter out defaults already present by name (use sets for faster
     lookups) *)
  let names_set_of lst =
    List.fold_left
      (fun acc d ->
        match Css.custom_declaration_name d with
        | Some n -> Strings.add n acc
        | None -> acc)
      Strings.empty lst
  in
  let extracted_names = names_set_of extracted in
  let pre =
    pre_defaults
    |> List.filter (fun d ->
           match Css.custom_declaration_name d with
           | Some n -> not (Strings.mem n extracted_names)
           | None -> false)
  in
  let pre_names = names_set_of pre in
  let post =
    post_defaults
    |> List.filter (fun d ->
           match Css.custom_declaration_name d with
           | Some n ->
               (not (Strings.mem n extracted_names))
               && not (Strings.mem n pre_names)
           | None -> false)
  in
  let theme_generated_vars = pre @ extracted @ post in

  (* Sort variables by their order metadata *)
  (* Pre-extract metadata once to avoid repeated extraction during sorting *)
  let vars_with_order =
    List.map
      (fun decl ->
        let order = Var.order_of_declaration decl in
        (decl, order))
      theme_generated_vars
  in
  let sorted_vars_with_order =
    List.sort
      (fun (_, order_a) (_, order_b) ->
        match (order_a, order_b) with
        | Some a, Some b -> Int.compare a b
        | Some _, None -> -1
        | None, Some _ -> 1
        | None, None -> 0)
      vars_with_order
  in
  let sorted_vars = List.map fst sorted_vars_with_order in

  if sorted_vars = [] then Css.of_statements [ Css.layer ~name:"theme" [] ]
  else
    let selector = Css.Selector.(list [ Root; host () ]) in
    Css.of_statements
      [ Css.layer ~name:"theme" [ Css.rule ~selector sorted_vars ] ]

let compute_theme_layer ?(default_decls = []) tw_classes =
  let selector_props = collect_selector_props tw_classes in
  compute_theme_layer_from_selector_props ~default_decls selector_props

let placeholder_supports =
  let placeholder = Css.Selector.Placeholder in

  (* Create the inner @supports for modern browsers *)
  let modern_rule =
    Css.rule ~selector:placeholder
      [
        Css.color
          (Css.color_mix ~in_space:Oklab ~percent1:50 Current Transparent);
      ]
  in
  let modern_support_stmt =
    Css.supports ~condition:"(color:color-mix(in lab, red, red))"
      [ modern_rule ]
  in

  (* Create the outer @supports with the fallback rule and nested modern
     support *)
  let fallback_rule = Css.rule ~selector:placeholder [ Css.color Current ] in
  let outer_support_content = [ fallback_rule; modern_support_stmt ] in

  Css.of_statements
    [
      Css.supports
        ~condition:
          "(not ((-webkit-appearance:-apple-pay-button))) or \
           (contain-intrinsic-size:1px)"
        outer_support_content;
    ]

let build_base_layer ?supports () =
  let base = Preflight.stylesheet ?placeholder_supports:supports () in
  Css.layer_of ~name:"base" base

(* Use the centralized conversion function from Var module *)

(* Build the properties layer with browser detection for initial values *)
(* Returns (properties_layer, property_rules) - @property rules are separate *)
let build_properties_layer explicit_property_rules_statements =
  (* Split statements into @property rules and other statements *)
  let property_rules, other_statements =
    List.partition
      (fun stmt ->
        match Css.as_property stmt with Some _ -> true | None -> false)
      explicit_property_rules_statements
  in

  (* Extract variable initial values from @property declarations *)
  let variable_initial_values =
    List.fold_left
      (fun acc stmt ->
        match Css.as_property stmt with
        | Some (Css.Property_info info as prop_info) ->
            let value = Var.property_initial_string prop_info in
            (info.name, value) :: acc
        | None -> acc)
      [] property_rules
    |> List.rev
  in

  if property_rules = [] && variable_initial_values = [] then (Css.empty, [])
  else
    (* Build the properties layer with browser detection but WITHOUT @property
       rules *)
    let browser_detection_condition =
      "(((-webkit-hyphens:none)) and (not (margin-trim:inline))) or \
       ((-moz-orient:inline) and (not (color:rgb(from red r g b))))"
    in
    (* Create the selector for universal + pseudo-elements *)
    let selector = Css.Selector.(list [ universal; Before; After; Backdrop ]) in
    (* Create initial declarations for each property with their actual initial
       values *)
    let initial_declarations =
      List.map
        (fun (name, value) -> Css.custom_property name value)
        variable_initial_values
    in
    let rule = Css.rule ~selector initial_declarations in
    let supports_content = [ rule ] in
    let supports_stmt =
      Css.supports ~condition:browser_detection_condition supports_content
    in
    (* Properties layer only has the supports statement and other statements,
       NOT @property rules *)
    let layer_content = [ supports_stmt ] @ other_statements in
    let layer =
      Css.of_statements [ Css.layer ~name:"properties" layer_content ]
    in
    (layer, property_rules)

let build_layers ~include_base ~selector_props tw_classes rules media_queries
    container_queries =
  (* Combined extraction of variables and property rules in a single
     traversal *)
  let rec extract_vars_and_property_rules = function
    | Core.Style { props; rules; property_rules; _ } ->
        let vars_from_props = Css.vars_of_declarations props in
        let vars_from_rules =
          match rules with Some r -> Css.vars_of_rules r | None -> []
        in
        (vars_from_props @ vars_from_rules, [ property_rules ])
    | Core.Modified (_, t) -> extract_vars_and_property_rules t
    | Core.Group ts ->
        let vars_list, prop_rules_list =
          List.split (List.map extract_vars_and_property_rules ts)
        in
        (List.concat vars_list, List.concat prop_rules_list)
  in
  (* Collect CSS variables and property rules in a single pass *)
  let vars_from_utilities, property_rules_lists =
    let results = List.map extract_vars_and_property_rules tw_classes in
    let vars_list, prop_rules_list = List.split results in
    (List.concat vars_list, List.concat prop_rules_list)
  in
  (* Collect explicit property_rules from Style objects first *)
  (* We need this before processing other variables *)
  let explicit_property_rules_statements =
    property_rules_lists |> List.concat_map Css.statements
  in

  (* Get variables that need @property rules (from ~property flag) *)
  let vars_needing_property =
    vars_from_utilities
    |> List.filter (fun (Css.V v) -> Var.var_needs_property v)
  in

  (* Generate @property rules for variables that need them but don't have
     explicit rules *)
  (* Compute names of variables that already have explicit @property rules *)
  let explicit_property_var_names_set =
    explicit_property_rules_statements
    |> List.filter_map (fun stmt ->
           match Css.as_property stmt with
           | Some (Css.Property_info info) -> Some info.name
           | None -> None)
    |> List.fold_left (fun acc n -> Strings.add n acc) Strings.empty
  in
  let property_rules_from_utilities =
    vars_needing_property
    |> List.filter (fun (Css.V v) ->
           (* Don't generate automatic @property if there's an explicit one *)
           let var_name = "--" ^ Css.var_name v in
           not (Strings.mem var_name explicit_property_var_names_set))
    |> List.map (fun (Css.V v) ->
           let var_name = "--" ^ Css.var_name v in
           (* For now, use Universal syntax ("*") which accepts any value *)
           Css.property ~name:var_name Css.Universal ~inherits:false ())
  in

  (* Existing layers in exact order *)
  let theme_defaults =
    Typography.default_font_declarations
    @ Typography.default_font_family_declarations
  in
  let theme_layer =
    compute_theme_layer_from_selector_props ~default_decls:theme_defaults
      selector_props
  in
  let base_layer = build_base_layer ~supports:placeholder_supports () in

  (* Build layer list with properties layer first if we have property rules *)
  let layers =
    (* Properties layer is built from ALL property rules (explicit +
       auto-generated) *)
    let property_rules_from_utilities_as_statements =
      property_rules_from_utilities |> List.concat_map Css.statements
    in
    let all_property_statements =
      explicit_property_rules_statements
      @ property_rules_from_utilities_as_statements
    in
    let properties_layer, property_rules_for_end =
      if all_property_statements = [] then (None, [])
      else
        let layer, prop_rules =
          build_properties_layer all_property_statements
        in
        (* Check if the properties layer is actually empty (Css.empty) *)
        if layer = Css.empty then (None, prop_rules)
        else (Some layer, prop_rules)
    in
    let base_layers =
      if include_base then [ theme_layer; base_layer ] else [ theme_layer ]
    in
    (* Always generate separate components declaration and utilities layer to
       match Tailwind v4 behavior *)
    let components_declaration =
      Css.of_statements [ Css.layer_decl [ "components" ] ]
    in
    let utilities_layer =
      build_utilities_layer ~rules ~media_queries ~container_queries
    in

    (* Add layer declaration list at beginning *)
    let layer_names =
      let names =
        (match properties_layer with Some _ -> [ "properties" ] | None -> [])
        @
        if include_base then [ "theme"; "base"; "components"; "utilities" ]
        else [ "theme"; "components"; "utilities" ]
      in
      Css.of_statements [ Css.layer_decl names ]
    in

    let initial_layers =
      match properties_layer with None -> [] | Some l -> [ l ]
    in
    (* Layers without @property rules *)
    let layers_without_property =
      [ layer_names ] @ initial_layers @ base_layers
      @ [ components_declaration; utilities_layer ]
    in
    (* Append @property rules at the END (after all layers) to match Tailwind
       v4 *)
    let property_rules_css =
      if property_rules_for_end = [] then []
      else [ Css.of_statements property_rules_for_end ]
    in
    layers_without_property @ property_rules_css
  in

  (* Return layers with @property rules at the end *)
  layers

let wrap_css_items ~rules ~media_queries ~container_queries =
  let rules_stylesheet = Css.v rules in
  let media_stylesheets =
    List.map
      (fun (condition, rules) ->
        Css.of_statements [ Css.media ~condition rules ])
      media_queries
  in
  let container_stylesheets =
    List.map
      (fun (name, condition, rules) ->
        Css.of_statements [ Css.container ?name ~condition rules ])
      container_queries
  in
  Css.concat (rules_stylesheet :: (media_stylesheets @ container_stylesheets))

(* ======================================================================== *)
(* Main API - Convert Tw styles to CSS *)

(* ======================================================================== *)

type config = { base : bool; mode : Css.mode; optimize : bool }
(** Configuration for CSS generation *)

let default_config = { base = true; mode = Css.Variables; optimize = false }

let to_css ?(config = default_config) tw_classes =
  (* Extract once and share for rule sets and theme layer *)
  let selector_props = List.concat_map extract_selector_props tw_classes in
  let rules, media_queries, container_queries =
    rule_sets_from_selector_props selector_props
  in

  (* Generate layers whenever mode = Variables. Include the base layer only when
     [reset=true]. In Inline mode, emit raw rules without layers. *)
  match config.mode with
  | Css.Variables ->
      let layers =
        build_layers ~include_base:config.base ~selector_props tw_classes rules
          media_queries container_queries
      in
      Css.concat layers
  | Css.Inline ->
      (* No layers - just raw utility rules with var() resolved to fallback
         values *)
      wrap_css_items ~rules ~media_queries ~container_queries

let to_inline_style styles =
  (* Collect all declarations from props and embedded rules. Build in reverse
     using [rev_append] to avoid quadratic concatenations, then reverse once. *)
  let rec collect acc = function
    | Style { props; rules; _ } ->
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
    | Modified (_, t) -> collect acc t
    | Group ts -> List.fold_left collect acc ts
  in
  let all_props = List.rev (List.fold_left collect [] styles) in
  (* Filter out CSS custom properties (variables) - they shouldn't be in inline
     styles *)
  let non_variable_props =
    List.filter (fun decl -> Css.custom_declaration_name decl = None) all_props
  in
  Css.inline_style_of_declarations non_variable_props
