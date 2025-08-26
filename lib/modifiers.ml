(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

open Core

(** Generate CSS selector for a modifier and base class *)
let to_selector modifier base_class =
  match modifier with
  | Hover -> ".hover\\:" ^ base_class ^ ":hover"
  | Focus -> ".focus\\:" ^ base_class ^ ":focus"
  | Active -> ".active\\:" ^ base_class ^ ":active"
  | Disabled -> ".disabled\\:" ^ base_class ^ ":disabled"
  | Group_hover -> ".group:hover .group-hover\\:" ^ base_class
  | Group_focus -> ".group:focus .group-focus\\:" ^ base_class
  | Peer_hover -> ".peer-hover\\:" ^ base_class ^ ":is(:where(.peer):hover~*)"
  | Peer_focus -> ".peer-focus\\:" ^ base_class ^ ":is(:where(.peer):focus~*)"
  | Peer_checked ->
      ".peer-checked\\:" ^ base_class ^ ":is(:where(.peer):checked~*)"
  | Aria_checked -> ".aria-checked\\:" ^ base_class ^ "[aria-checked=true]"
  | Aria_expanded -> ".aria-expanded\\:" ^ base_class ^ "[aria-expanded=true]"
  | Aria_selected ->
      ".aria-selected\\:" ^ base_class ^ "[aria-selected=\"true\"]"
  | Aria_disabled -> ".aria-disabled\\:" ^ base_class ^ "[aria-disabled=true]"
  | Data_active -> ".data-\\[active\\]\\:" ^ base_class ^ "[data-active]"
  | Data_inactive -> ".data-\\[inactive\\]\\:" ^ base_class ^ "[data-inactive]"
  | Focus_within -> ".focus-within\\:" ^ base_class ^ ":focus-within"
  | Focus_visible -> ".focus-visible\\:" ^ base_class ^ ":focus-visible"
  | Pseudo_before -> ".before\\:" ^ base_class ^ "::before"
  | Pseudo_after -> ".after\\:" ^ base_class ^ "::after"
  | _ -> base_class (* fallback for complex modifiers *)

(** Check if a modifier generates a hover rule *)
let is_hover = function Hover | Group_hover -> true | _ -> false

let wrap m styles =
  match styles with
  | [] -> Group []
  | _ -> Group (List.map (fun t -> Modified (m, t)) styles)

(* State variants *)
let hover = wrap Hover
let focus = wrap Focus
let active = wrap Active
let disabled = wrap Disabled
let focus_within = wrap Focus_within
let focus_visible = wrap Focus_visible

(* Group/peer markers *)
let group = style "group" []
let peer = style "peer" []

(* Group/peer variants *)
let group_hover = wrap Group_hover
let group_focus = wrap Group_focus
let peer_hover = wrap Peer_hover
let peer_focus = wrap Peer_focus

(* :has() helpers *)
let has selector styles = wrap (Has selector) styles
let group_has selector styles = wrap (Group_has selector) styles
let peer_has selector styles = wrap (Peer_has selector) styles

(* Theme/motion/contrast *)
let dark = wrap Dark
let motion_safe = wrap Motion_safe
let motion_reduce = wrap Motion_reduce
let contrast_more = wrap Contrast_more
let contrast_less = wrap Contrast_less
let starting = wrap Starting

(* Pseudo-elements *)
let before = wrap Pseudo_before
let after = wrap Pseudo_after

(* Check if a style already has a responsive modifier *)
let rec has_responsive_modifier = function
  | Style _ -> false
  | Modified (Responsive _, _) -> true
  | Modified (_, t) -> has_responsive_modifier t
  | Group styles -> List.exists has_responsive_modifier styles

(* Validate no nested responsive modifiers *)
let validate_no_nested_responsive styles =
  List.iter
    (fun style ->
      if has_responsive_modifier style then
        failwith
          "Cannot apply responsive modifiers to styles that already have \
           responsive modifiers")
    styles

(* Responsive - with validation to prevent nesting *)
let sm styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Sm) styles

let md styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Md) styles

let lg styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Lg) styles

let xl styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Xl) styles

let xxl styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Xl_2) styles

(* ARIA/Peer/Data variants *)
let peer_checked styles = wrap Peer_checked styles
let aria_checked styles = wrap Aria_checked styles
let aria_expanded styles = wrap Aria_expanded styles
let aria_selected styles = wrap Aria_selected styles
let aria_disabled styles = wrap Aria_disabled styles
let data_state value style = Modified (Data_state value, style)
let data_variant value style = Modified (Data_variant value, style)
let data_custom key value style = Modified (Data_custom (key, value), style)
let data_active styles = wrap Data_active styles
let data_inactive styles = wrap Data_inactive styles

(* Parse modifiers (responsive, states) from class string *)
let of_string class_str =
  let parts = String.split_on_char ':' class_str in
  match List.rev parts with
  | [] -> ([], class_str)
  | base_class :: modifiers -> (List.rev modifiers, base_class)

(* Convert modifier to its string prefix *)
let pp_modifier = function
  | Hover -> "hover"
  | Focus -> "focus"
  | Active -> "active"
  | Disabled -> "disabled"
  | Group_hover -> "group-hover"
  | Group_focus -> "group-focus"
  | Peer_hover -> "peer-hover"
  | Peer_focus -> "peer-focus"
  | Peer_checked -> "peer-checked"
  | Aria_checked -> "aria-checked"
  | Aria_expanded -> "aria-expanded"
  | Aria_selected -> "aria-selected"
  | Aria_disabled -> "aria-disabled"
  | Data_state value -> "data-[state=" ^ value ^ "]"
  | Data_variant value -> "data-[variant=" ^ value ^ "]"
  | Data_active -> "data-[active]"
  | Data_inactive -> "data-[inactive]"
  | Data_custom (key, value) -> "data-[" ^ key ^ "=" ^ value ^ "]"
  | Dark -> "dark"
  | Responsive breakpoint -> (
      match breakpoint with
      | `Sm -> "sm"
      | `Md -> "md"
      | `Lg -> "lg"
      | `Xl -> "xl"
      | `Xl_2 -> "2xl")
  | Container query -> Containers.container_query_to_class_prefix query
  | Not _modifier -> "not" (* Simplified for class names *)
  | Has selector -> "has-[" ^ selector ^ "]"
  | Group_has selector -> "group-has-[" ^ selector ^ "]"
  | Peer_has selector -> "peer-has-[" ^ selector ^ "]"
  | Starting -> "starting"
  | Focus_within -> "focus-within"
  | Focus_visible -> "focus-visible"
  | Motion_safe -> "motion-safe"
  | Motion_reduce -> "motion-reduce"
  | Contrast_more -> "contrast-more"
  | Contrast_less -> "contrast-less"
  | Pseudo_before -> "before"
  | Pseudo_after -> "after"

(* Apply a list of modifier strings to a base style *)
let apply modifiers base_style =
  List.fold_left
    (fun acc modifier ->
      match modifier with
      | "sm" -> (
          match acc with Group styles -> sm styles | single -> sm [ single ])
      | "md" -> (
          match acc with Group styles -> md styles | single -> md [ single ])
      | "lg" -> (
          match acc with Group styles -> lg styles | single -> lg [ single ])
      | "xl" -> (
          match acc with Group styles -> xl styles | single -> xl [ single ])
      | "2xl" -> (
          match acc with Group styles -> xxl styles | single -> xxl [ single ])
      | "hover" -> (
          match acc with
          | Group styles -> hover styles
          | single -> hover [ single ])
      | "focus" -> (
          match acc with
          | Group styles -> focus styles
          | single -> focus [ single ])
      | "active" -> (
          match acc with
          | Group styles -> active styles
          | single -> active [ single ])
      | "disabled" -> (
          match acc with
          | Group styles -> disabled styles
          | single -> disabled [ single ])
      | "dark" -> (
          match acc with
          | Group styles -> dark styles
          | single -> dark [ single ])
      | _ -> acc (* ignore unknown modifiers for now *))
    base_style modifiers
