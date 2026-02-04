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
      (* Tailwind uses: .group-hover\:cls:is(:where(.group):hover x) *)
      let rel =
        combine (compound [ where [ group ]; Hover ]) Descendant universal
      in
      compound [ group_hover cls; is_ [ rel ] ]
  | Group_focus ->
      let rel =
        combine (compound [ where [ group ]; Focus ]) Descendant universal
      in
      compound [ group_focus cls; is_ [ rel ] ]
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
  (* Media-like modifiers that only prefix the class; actual media gating is
     handled by rules. *)
  | Dark -> Css.Selector.Class ("dark:" ^ cls)
  | Motion_safe -> Css.Selector.Class ("motion-safe:" ^ cls)
  | Motion_reduce -> Css.Selector.Class ("motion-reduce:" ^ cls)
  | Contrast_more -> Css.Selector.Class ("contrast-more:" ^ cls)
  | Contrast_less -> Css.Selector.Class ("contrast-less:" ^ cls)
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

(* Parse modifiers (responsive, states) from class string. Handles brackets
   properly so has-[:checked]:bg-red-500 parses as modifiers=["has-[:checked]"]
   and base_class="bg-red-500" *)
let of_string class_str =
  let len = String.length class_str in
  let rec split_parts acc current_start i bracket_depth =
    if i >= len then
      (* End of string - add final part *)
      let part = String.sub class_str current_start (len - current_start) in
      List.rev (part :: acc)
    else
      match class_str.[i] with
      | '[' -> split_parts acc current_start (i + 1) (bracket_depth + 1)
      | ']' -> split_parts acc current_start (i + 1) (max 0 (bracket_depth - 1))
      | ':' when bracket_depth = 0 ->
          (* Split here - colon outside brackets *)
          let part = String.sub class_str current_start (i - current_start) in
          split_parts (part :: acc) (i + 1) (i + 1) 0
      | _ -> split_parts acc current_start (i + 1) bracket_depth
  in
  let parts = split_parts [] 0 0 0 in
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
  (* Apply a single modifier to an accumulated utility *)
  let apply_one acc modifier =
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
    | "starting" -> (
        match acc with
        | Utility.Group styles -> starting styles
        | single -> starting [ single ])
    | "focus-within" -> (
        match acc with
        | Utility.Group styles -> focus_within styles
        | single -> focus_within [ single ])
    | "focus-visible" -> (
        match acc with
        | Utility.Group styles -> focus_visible styles
        | single -> focus_visible [ single ])
    | "dark" -> (
        match acc with
        | Utility.Group styles -> dark styles
        | single -> dark [ single ])
    | "motion-safe" -> (
        match acc with
        | Utility.Group styles -> motion_safe styles
        | single -> motion_safe [ single ])
    | "motion-reduce" -> (
        match acc with
        | Utility.Group styles -> motion_reduce styles
        | single -> motion_reduce [ single ])
    | "contrast-more" -> (
        match acc with
        | Utility.Group styles -> contrast_more styles
        | single -> contrast_more [ single ])
    | "contrast-less" -> (
        match acc with
        | Utility.Group styles -> contrast_less styles
        | single -> contrast_less [ single ])
    | "before" -> (
        match acc with
        | Utility.Group styles -> before styles
        | single -> before [ single ])
    | "after" -> (
        match acc with
        | Utility.Group styles -> after styles
        | single -> after [ single ])
    (* Group/peer variants *)
    | "group-hover" -> (
        match acc with
        | Utility.Group styles -> group_hover styles
        | single -> group_hover [ single ])
    | "group-focus" -> (
        match acc with
        | Utility.Group styles -> group_focus styles
        | single -> group_focus [ single ])
    | "peer-hover" -> (
        match acc with
        | Utility.Group styles -> peer_hover styles
        | single -> peer_hover [ single ])
    | "peer-focus" -> (
        match acc with
        | Utility.Group styles -> peer_focus styles
        | single -> peer_focus [ single ])
    | "peer-checked" -> (
        match acc with
        | Utility.Group styles -> peer_checked styles
        | single -> peer_checked [ single ])
    (* ARIA variants *)
    | "aria-checked" -> (
        match acc with
        | Utility.Group styles -> aria_checked styles
        | single -> aria_checked [ single ])
    | "aria-expanded" -> (
        match acc with
        | Utility.Group styles -> aria_expanded styles
        | single -> aria_expanded [ single ])
    | "aria-selected" -> (
        match acc with
        | Utility.Group styles -> aria_selected styles
        | single -> aria_selected [ single ])
    | "aria-disabled" -> (
        match acc with
        | Utility.Group styles -> aria_disabled styles
        | single -> aria_disabled [ single ])
    (* Bracketed :has() variants *)
    | _ when String.starts_with ~prefix:"group-has-[" modifier -> (
        let rest =
          String.sub modifier
            (String.length "group-has-[")
            (String.length modifier - String.length "group-has-[")
        in
        (* rest is like "<selector>]..."; take up to ']' *)
        match String.index_opt rest ']' with
        | Some i -> (
            let sel = String.sub rest 0 i in
            match acc with
            | Utility.Group styles -> group_has sel styles
            | single -> group_has sel [ single ])
        | None -> acc)
    | _ when String.starts_with ~prefix:"peer-has-[" modifier -> (
        let rest =
          String.sub modifier
            (String.length "peer-has-[")
            (String.length modifier - String.length "peer-has-[")
        in
        match String.index_opt rest ']' with
        | Some i -> (
            let sel = String.sub rest 0 i in
            match acc with
            | Utility.Group styles -> peer_has sel styles
            | single -> peer_has sel [ single ])
        | None -> acc)
    | _ when String.starts_with ~prefix:"has-[" modifier -> (
        let rest =
          String.sub modifier (String.length "has-[")
            (String.length modifier - String.length "has-[")
        in
        match String.index_opt rest ']' with
        | Some i -> (
            let sel = String.sub rest 0 i in
            match acc with
            | Utility.Group styles -> has sel styles
            | single -> has sel [ single ])
        | None -> acc)
    (* Container query modifiers (@sm, @md, @lg, @xl, @2xl) *)
    | "@sm" -> (
        match acc with
        | Utility.Group styles -> Containers.container_sm styles
        | single -> Containers.container_sm [ single ])
    | "@md" -> (
        match acc with
        | Utility.Group styles -> Containers.container_md styles
        | single -> Containers.container_md [ single ])
    | "@lg" -> (
        match acc with
        | Utility.Group styles -> Containers.container_lg styles
        | single -> Containers.container_lg [ single ])
    | "@xl" -> (
        match acc with
        | Utility.Group styles -> Containers.container_xl styles
        | single -> Containers.container_xl [ single ])
    | "@2xl" -> (
        match acc with
        | Utility.Group styles -> Containers.container_2xl styles
        | single -> Containers.container_2xl [ single ])
    | _ -> acc (* ignore unknown modifiers for now *)
  in
  (* Apply modifiers in reverse order so that the first modifier in the string
     (e.g., "dark" in "dark:hover:...") ends up as the outermost wrapper
     (Modified(Dark, Modified(Hover, base))). This matches how the programmatic
     API works: dark [ hover [ ... ] ]

     We apply ALL modifiers in reverse order to preserve their relative
     positions. This handles both "dark:has-[:checked]:X" -> Modified(Dark,
     Modified(Has, X)) and "group-has-[.y]:hover:m-2" -> Modified(Group_has,
     Modified(Hover, X)). *)
  List.fold_left apply_one base_utility (List.rev modifiers)
