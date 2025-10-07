(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

(* We work with Utility.t instead of Style.t *)
open Style
(** We still need Style.modifier type *)

(** Helper to build class selectors *)
let build_class prefix cls = Css.Selector.Class (prefix ^ cls)

(** Helper functions for building class names *)
let hover cls = build_class "hover:" cls

let focus cls = build_class "focus:" cls
let active cls = build_class "active:" cls
let disabled cls = build_class "disabled:" cls
let group_hover cls = build_class "group-hover:" cls
let group_focus cls = build_class "group-focus:" cls
let peer_hover cls = build_class "peer-hover:" cls
let peer_focus cls = build_class "peer-focus:" cls
let peer_checked cls = build_class "peer-checked:" cls
let aria_checked cls = build_class "aria-checked:" cls
let aria_expanded cls = build_class "aria-expanded:" cls
let aria_selected cls = build_class "aria-selected:" cls
let aria_disabled cls = build_class "aria-disabled:" cls
let data_active cls = build_class "data-active:" cls
let data_inactive cls = build_class "data-inactive:" cls
let focus_within cls = build_class "focus-within:" cls
let focus_visible cls = build_class "focus-visible:" cls
let before cls = build_class "before:" cls
let after cls = build_class "after:" cls

(** Base marker classes *)
let group = Css.Selector.Class "group"

let peer = Css.Selector.Class "peer"

(** Generate CSS selector for a modifier and base class *)
let to_selector (modifier : modifier) cls =
  let open Css.Selector in
  match modifier with
  | Hover -> compound [ hover cls; Hover ]
  | Focus -> compound [ focus cls; Focus ]
  | Active -> compound [ active cls; Active ]
  | Disabled -> compound [ disabled cls; Disabled ]
  | Group_hover ->
      combine (compound [ group; Hover ]) Descendant (group_hover cls)
  | Group_focus ->
      combine (compound [ group; Focus ]) Descendant (group_focus cls)
  | Peer_hover ->
      let rel =
        combine
          (compound [ where [ peer ]; Hover ])
          Subsequent_sibling universal
      in
      compound [ peer_hover cls; is_ [ rel ] ]
  | Peer_focus ->
      let rel =
        combine
          (compound [ where [ peer ]; Focus ])
          Subsequent_sibling universal
      in
      compound [ peer_focus cls; is_ [ rel ] ]
  | Peer_checked ->
      let rel =
        combine
          (compound [ where [ peer ]; Checked ])
          Subsequent_sibling universal
      in
      compound [ peer_checked cls; is_ [ rel ] ]
  | Aria_checked ->
      compound [ aria_checked cls; attribute "aria-checked" (Exact "true") ]
  | Aria_expanded ->
      compound [ aria_expanded cls; attribute "aria-expanded" (Exact "true") ]
  | Aria_selected ->
      compound [ aria_selected cls; attribute "aria-selected" (Exact "true") ]
  | Aria_disabled ->
      compound [ aria_disabled cls; attribute "aria-disabled" (Exact "true") ]
  | Data_active ->
      compound [ data_active cls; attribute "data-active" Presence ]
  | Data_inactive ->
      compound [ data_inactive cls; attribute "data-inactive" Presence ]
  | Focus_within -> compound [ focus_within cls; Focus_within ]
  | Focus_visible -> compound [ focus_visible cls; Focus_visible ]
  | Pseudo_before -> compound [ before cls; Before ]
  | Pseudo_after -> compound [ after cls; After ]
  | _ -> Css.Selector.Class cls (* fallback for complex modifiers *)

(** Check if a modifier generates a hover rule *)
let is_hover = function Hover | Group_hover | Peer_hover -> true | _ -> false

let wrap m styles =
  match styles with
  | [] -> Utility.Group []
  | _ -> Utility.Group (List.map (fun t -> Utility.Modified (m, t)) styles)

(* State variants *)
let hover = wrap Hover
let focus = wrap Focus
let active = wrap Active
let disabled = wrap Disabled
let focus_within = wrap Focus_within
let focus_visible = wrap Focus_visible

(* Group/peer markers *)
let group = Interactivity.group
let peer = Interactivity.peer

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

(* Check if a utility already has a responsive modifier *)
let rec has_responsive_modifier = function
  | Utility.Base _ -> false
  | Utility.Modified (Responsive _, _) -> true
  | Utility.Modified (_, t) -> has_responsive_modifier t
  | Utility.Group styles -> List.exists has_responsive_modifier styles

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

let xl2 styles =
  validate_no_nested_responsive styles;
  wrap (Responsive `Xl_2) styles

(* ARIA/Peer/Data variants *)
let peer_checked styles = wrap Peer_checked styles
let aria_checked styles = wrap Aria_checked styles
let aria_expanded styles = wrap Aria_expanded styles
let aria_selected styles = wrap Aria_selected styles
let aria_disabled styles = wrap Aria_disabled styles
let data_state value style = Utility.Modified (Data_state value, style)
let data_variant value style = Utility.Modified (Data_variant value, style)

let data_custom key value style =
  Utility.Modified (Data_custom (key, value), style)

let data_active styles = wrap Data_active styles
let data_inactive styles = wrap Data_inactive styles

(* Parse modifiers (responsive, states) from class string *)
let of_string class_str =
  let parts = String.split_on_char ':' class_str in
  match List.rev parts with
  | [] -> ([], class_str)
  | cls :: modifiers -> (List.rev modifiers, cls)

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
  | Data_active -> "data-active"
  | Data_inactive -> "data-inactive"
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

(* Apply a list of modifier strings to a base utility *)
let apply modifiers base_utility =
  List.fold_left
    (fun acc modifier ->
      match modifier with
      | "sm" -> (
          match acc with
          | Utility.Group styles -> sm styles
          | single -> sm [ single ])
      | "md" -> (
          match acc with
          | Utility.Group styles -> md styles
          | single -> md [ single ])
      | "lg" -> (
          match acc with
          | Utility.Group styles -> lg styles
          | single -> lg [ single ])
      | "xl" -> (
          match acc with
          | Utility.Group styles -> xl styles
          | single -> xl [ single ])
      | "2xl" -> (
          match acc with
          | Utility.Group styles -> xl2 styles
          | single -> xl2 [ single ])
      | "hover" -> (
          match acc with
          | Utility.Group styles -> hover styles
          | single -> hover [ single ])
      | "focus" -> (
          match acc with
          | Utility.Group styles -> focus styles
          | single -> focus [ single ])
      | "active" -> (
          match acc with
          | Utility.Group styles -> active styles
          | single -> active [ single ])
      | "disabled" -> (
          match acc with
          | Utility.Group styles -> disabled styles
          | single -> disabled [ single ])
      | "dark" -> (
          match acc with
          | Utility.Group styles -> dark styles
          | single -> dark [ single ])
      | _ -> acc (* ignore unknown modifiers for now *))
    base_utility modifiers
