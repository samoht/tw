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

(** Helper: compound selector with class prefix and pseudo-class *)
let class_pseudo prefix cls pseudo =
  Css.Selector.compound [ Css.Selector.Class (prefix ^ ":" ^ cls); pseudo ]

(** Helper: parse an nth expression string *)
let parse_nth expr =
  let reader = Css.Reader.of_string expr in
  Css.Selector.read_nth reader

(** Helper: breakpoint name for responsive modifiers *)
let breakpoint_name qual bp =
  let base =
    match bp with
    | `Sm -> "sm"
    | `Md -> "md"
    | `Lg -> "lg"
    | `Xl -> "xl"
    | `Xl_2 -> "2xl"
  in
  match qual with "" -> base | q -> q ^ "-" ^ base

(** Helper: arbitrary breakpoint class selector *)
let arbitrary_breakpoint_class prefix px cls =
  let px_str =
    if Float.is_integer px then Int.to_string (Float.to_int px)
    else Float.to_string px
  in
  Css.Selector.Class (prefix ^ "[" ^ px_str ^ "px]:" ^ cls)

(** Helper: direction selector (ltr/rtl) *)
let dir_selector dir cls =
  let open Css.Selector in
  let dir_sel = Dir dir in
  let attr_sel = attribute "dir" (Exact dir) in
  let desc_sel = combine attr_sel Descendant universal in
  compound [ Class (dir ^ ":" ^ cls); where [ dir_sel; attr_sel; desc_sel ] ]

(** Helper: inert pseudo-selector *)
let inert_pseudo () =
  let open Css.Selector in
  let inert_attr = attribute "inert" Presence in
  let inert_desc = combine inert_attr Descendant universal in
  is_ [ inert_attr; inert_desc ]

(** Helper: open pseudo-selector *)
let open_pseudo () =
  let open Css.Selector in
  is_ [ attribute "open" Presence; Popover_open; Open ]

(** Group variant selector — :where(.group):pseudo descendant *)
let group_selector cls modifier =
  let open Css.Selector in
  let gp prefix pseudo =
    let rel =
      combine (compound [ where [ group ]; pseudo ]) Descendant universal
    in
    compound [ Class (prefix ^ ":" ^ cls); is_ [ rel ] ]
  in
  match modifier with
  | Group_hover -> gp "group-hover" Hover
  | Group_focus -> gp "group-focus" Focus
  | Group_first -> gp "group-first" First_child
  | Group_last -> gp "group-last" Last_child
  | Group_odd -> gp "group-odd" (Nth_child (Odd, None))
  | Group_even -> gp "group-even" (Nth_child (Even, None))
  | Group_only -> gp "group-only" Only_child
  | Group_first_of_type -> gp "group-first-of-type" First_of_type
  | Group_last_of_type -> gp "group-last-of-type" Last_of_type
  | Group_only_of_type -> gp "group-only-of-type" Only_of_type
  | Group_active -> gp "group-active" Active
  | Group_visited -> gp "group-visited" Visited
  | Group_disabled -> gp "group-disabled" Disabled
  | Group_checked -> gp "group-checked" Checked
  | Group_empty -> gp "group-empty" Empty
  | Group_required -> gp "group-required" Required
  | Group_valid -> gp "group-valid" Valid
  | Group_invalid -> gp "group-invalid" Invalid
  | Group_indeterminate -> gp "group-indeterminate" Indeterminate
  | Group_default -> gp "group-default" Default
  | Group_open -> gp "group-open" (open_pseudo ())
  | Group_target -> gp "group-target" Target
  | Group_optional -> gp "group-optional" Optional
  | Group_read_only -> gp "group-read-only" Read_only
  | Group_read_write -> gp "group-read-write" Read_write
  | Group_inert -> gp "group-inert" (inert_pseudo ())
  | Group_user_valid -> gp "group-user-valid" User_valid
  | Group_user_invalid -> gp "group-user-invalid" User_invalid
  | Group_placeholder_shown -> gp "group-placeholder-shown" Placeholder_shown
  | Group_autofill -> gp "group-autofill" Autofill
  | Group_in_range -> gp "group-in-range" In_range
  | Group_out_of_range -> gp "group-out-of-range" Out_of_range
  | Group_focus_within -> gp "group-focus-within" Focus_within
  | Group_focus_visible -> gp "group-focus-visible" Focus_visible
  | Group_enabled -> gp "group-enabled" Enabled
  | _ -> Class cls

(** Peer variant selector — :where(.peer):pseudo ~ *)
let peer_selector cls modifier =
  let open Css.Selector in
  let pp prefix pseudo =
    let rel =
      combine (compound [ where [ peer ]; pseudo ]) Subsequent_sibling universal
    in
    compound [ Class (prefix ^ ":" ^ cls); is_ [ rel ] ]
  in
  match modifier with
  | Peer_hover -> pp "peer-hover" Hover
  | Peer_focus -> pp "peer-focus" Focus
  | Peer_checked -> pp "peer-checked" Checked
  | Peer_first -> pp "peer-first" First_child
  | Peer_last -> pp "peer-last" Last_child
  | Peer_odd -> pp "peer-odd" (Nth_child (Odd, None))
  | Peer_even -> pp "peer-even" (Nth_child (Even, None))
  | Peer_only -> pp "peer-only" Only_child
  | Peer_first_of_type -> pp "peer-first-of-type" First_of_type
  | Peer_last_of_type -> pp "peer-last-of-type" Last_of_type
  | Peer_only_of_type -> pp "peer-only-of-type" Only_of_type
  | Peer_active -> pp "peer-active" Active
  | Peer_visited -> pp "peer-visited" Visited
  | Peer_disabled -> pp "peer-disabled" Disabled
  | Peer_empty -> pp "peer-empty" Empty
  | Peer_required -> pp "peer-required" Required
  | Peer_valid -> pp "peer-valid" Valid
  | Peer_invalid -> pp "peer-invalid" Invalid
  | Peer_indeterminate -> pp "peer-indeterminate" Indeterminate
  | Peer_default -> pp "peer-default" Default
  | Peer_open -> pp "peer-open" (open_pseudo ())
  | Peer_target -> pp "peer-target" Target
  | Peer_optional -> pp "peer-optional" Optional
  | Peer_read_only -> pp "peer-read-only" Read_only
  | Peer_read_write -> pp "peer-read-write" Read_write
  | Peer_inert -> pp "peer-inert" (inert_pseudo ())
  | Peer_user_valid -> pp "peer-user-valid" User_valid
  | Peer_user_invalid -> pp "peer-user-invalid" User_invalid
  | Peer_placeholder_shown -> pp "peer-placeholder-shown" Placeholder_shown
  | Peer_autofill -> pp "peer-autofill" Autofill
  | Peer_in_range -> pp "peer-in-range" In_range
  | Peer_out_of_range -> pp "peer-out-of-range" Out_of_range
  | Peer_focus_within -> pp "peer-focus-within" Focus_within
  | Peer_focus_visible -> pp "peer-focus-visible" Focus_visible
  | Peer_enabled -> pp "peer-enabled" Enabled
  | _ -> Class cls

(** Form state modifier selector dispatch *)
let form_state_selector cls modifier =
  let cp = class_pseudo in
  match modifier with
  | Checked -> cp "checked" cls Css.Selector.Checked
  | Indeterminate -> cp "indeterminate" cls Css.Selector.Indeterminate
  | Default -> cp "default" cls Css.Selector.Default
  | Required -> cp "required" cls Css.Selector.Required
  | Valid -> cp "valid" cls Css.Selector.Valid
  | Invalid -> cp "invalid" cls Css.Selector.Invalid
  | In_range -> cp "in-range" cls Css.Selector.In_range
  | Out_of_range -> cp "out-of-range" cls Css.Selector.Out_of_range
  | Placeholder_shown ->
      cp "placeholder-shown" cls Css.Selector.Placeholder_shown
  | Autofill -> cp "autofill" cls Css.Selector.Autofill
  | Read_only -> cp "read-only" cls Css.Selector.Read_only
  | Read_write -> cp "read-write" cls Css.Selector.Read_write
  | Optional -> cp "optional" cls Css.Selector.Optional
  | Open ->
      Css.Selector.compound
        [
          Css.Selector.Class ("open:" ^ cls);
          Css.Selector.is_
            [ Css.Selector.attribute "open" Presence; Popover_open; Open ];
        ]
  | Enabled -> cp "enabled" cls Css.Selector.Enabled
  | Target -> cp "target" cls Css.Selector.Target
  | Visited -> cp "visited" cls Css.Selector.Visited
  | Inert ->
      Css.Selector.compound
        [ Css.Selector.Class ("inert:" ^ cls); inert_pseudo () ]
  | User_valid -> cp "user-valid" cls Css.Selector.User_valid
  | User_invalid -> cp "user-invalid" cls Css.Selector.User_invalid
  | _ -> group_selector cls modifier

(** Media and responsive modifiers that prefix the class name *)
let media_prefix_selector cls modifier =
  match modifier with
  | Dark -> Css.Selector.Class ("dark:" ^ cls)
  | Motion_safe -> Css.Selector.Class ("motion-safe:" ^ cls)
  | Motion_reduce -> Css.Selector.Class ("motion-reduce:" ^ cls)
  | Contrast_more -> Css.Selector.Class ("contrast-more:" ^ cls)
  | Contrast_less -> Css.Selector.Class ("contrast-less:" ^ cls)
  | Print -> Css.Selector.Class ("print:" ^ cls)
  | Portrait -> Css.Selector.Class ("portrait:" ^ cls)
  | Landscape -> Css.Selector.Class ("landscape:" ^ cls)
  | Forced_colors -> Css.Selector.Class ("forced-colors:" ^ cls)
  | Inverted_colors -> Css.Selector.Class ("inverted-colors:" ^ cls)
  | Pointer_none -> Css.Selector.Class ("pointer-none:" ^ cls)
  | Pointer_coarse -> Css.Selector.Class ("pointer-coarse:" ^ cls)
  | Pointer_fine -> Css.Selector.Class ("pointer-fine:" ^ cls)
  | Any_pointer_none -> Css.Selector.Class ("any-pointer-none:" ^ cls)
  | Any_pointer_coarse -> Css.Selector.Class ("any-pointer-coarse:" ^ cls)
  | Any_pointer_fine -> Css.Selector.Class ("any-pointer-fine:" ^ cls)
  | Noscript -> Css.Selector.Class ("noscript:" ^ cls)
  | Responsive bp -> Css.Selector.Class (breakpoint_name "" bp ^ ":" ^ cls)
  | Min_responsive bp ->
      Css.Selector.Class (breakpoint_name "min" bp ^ ":" ^ cls)
  | Max_responsive bp ->
      Css.Selector.Class (breakpoint_name "max" bp ^ ":" ^ cls)
  | Min_arbitrary px -> arbitrary_breakpoint_class "min-" px cls
  | Max_arbitrary px -> arbitrary_breakpoint_class "max-" px cls
  | Peer_hover | Peer_focus | Peer_checked | Peer_first | Peer_last | Peer_odd
  | Peer_even | Peer_only | Peer_first_of_type | Peer_last_of_type
  | Peer_only_of_type | Peer_active | Peer_visited | Peer_disabled | Peer_empty
  | Peer_required | Peer_valid | Peer_invalid | Peer_indeterminate
  | Peer_default | Peer_open | Peer_target | Peer_optional | Peer_read_only
  | Peer_read_write | Peer_inert | Peer_user_valid | Peer_user_invalid
  | Peer_placeholder_shown | Peer_autofill | Peer_in_range | Peer_out_of_range
  | Peer_focus_within | Peer_focus_visible | Peer_enabled ->
      peer_selector cls modifier
  | _ -> form_state_selector cls modifier

(** Structural pseudo-class modifiers *)
let structural_selector cls modifier =
  let cp = class_pseudo in
  match modifier with
  | First -> cp "first" cls Css.Selector.First_child
  | Last -> cp "last" cls Css.Selector.Last_child
  | Only -> cp "only" cls Css.Selector.Only_child
  | Odd -> cp "odd" cls Css.Selector.(Nth_child (Odd, None))
  | Even -> cp "even" cls Css.Selector.(Nth_child (Even, None))
  | First_of_type -> cp "first-of-type" cls Css.Selector.First_of_type
  | Last_of_type -> cp "last-of-type" cls Css.Selector.Last_of_type
  | Only_of_type -> cp "only-of-type" cls Css.Selector.Only_of_type
  | Nth expr ->
      let nth = parse_nth expr in
      Css.Selector.compound
        [
          Css.Selector.Class ("nth-[" ^ expr ^ "]:" ^ cls);
          Css.Selector.Nth_child (nth, None);
        ]
  | Nth_last expr ->
      let nth = parse_nth expr in
      Css.Selector.compound
        [
          Css.Selector.Class ("nth-last-[" ^ expr ^ "]:" ^ cls);
          Css.Selector.Nth_last_child (nth, None);
        ]
  | Empty -> cp "empty" cls Css.Selector.Empty
  | _ -> media_prefix_selector cls modifier

(** Generate CSS selector for a modifier and base class *)
let to_selector (modifier : modifier) cls =
  let open Css.Selector in
  let cp = class_pseudo in
  match modifier with
  | Hover -> compound [ hover cls; Hover ]
  | Focus -> compound [ focus cls; Focus ]
  | Active -> compound [ active cls; Active ]
  | Disabled -> compound [ disabled cls; Disabled ]
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
  (* Pseudo-element variants *)
  | Pseudo_marker -> cp "marker" cls Marker
  | Pseudo_selection -> cp "selection" cls Selection
  | Pseudo_placeholder -> cp "placeholder" cls Placeholder
  | Pseudo_backdrop -> cp "backdrop" cls Backdrop
  | Pseudo_file -> cp "file" cls File_selector_button
  | Pseudo_first_letter -> cp "first-letter" cls First_letter
  | Pseudo_first_line -> cp "first-line" cls First_line
  | Pseudo_details_content -> cp "details-content" cls Details_content
  (* Child/descendant selectors *)
  | Children ->
      let child_sel = combine (Class ("*:" ^ cls)) Child universal in
      is_ [ child_sel ]
  | Descendants ->
      let desc_sel = combine (Class ("**:" ^ cls)) Descendant universal in
      is_ [ desc_sel ]
  | Ltr -> dir_selector "ltr" cls
  | Rtl -> dir_selector "rtl" cls
  (* Structural, media, peer, form state — dispatched to sub-functions *)
  | _ -> structural_selector cls modifier

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

(* Max responsive variants *)
let max_sm styles =
  validate_no_nested_responsive styles;
  wrap (Max_responsive `Sm) styles

let max_md styles =
  validate_no_nested_responsive styles;
  wrap (Max_responsive `Md) styles

let max_lg styles =
  validate_no_nested_responsive styles;
  wrap (Max_responsive `Lg) styles

let max_xl styles =
  validate_no_nested_responsive styles;
  wrap (Max_responsive `Xl) styles

let max_xl2 styles =
  validate_no_nested_responsive styles;
  wrap (Max_responsive `Xl_2) styles

(* Arbitrary breakpoint variants *)
let min_arbitrary px styles =
  validate_no_nested_responsive styles;
  wrap (Min_arbitrary px) styles

let max_arbitrary px styles =
  validate_no_nested_responsive styles;
  wrap (Max_arbitrary px) styles

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

(* Structural pseudo-class variants *)
let first styles = wrap First styles
let last styles = wrap Last styles
let only styles = wrap Only styles
let odd styles = wrap Odd styles
let even styles = wrap Even styles
let first_of_type styles = wrap First_of_type styles
let last_of_type styles = wrap Last_of_type styles
let only_of_type styles = wrap Only_of_type styles
let nth expr styles = wrap (Nth expr) styles
let nth_last expr styles = wrap (Nth_last expr) styles
let empty styles = wrap Empty styles

(* Form state variants *)
let checked styles = wrap Checked styles
let indeterminate styles = wrap Indeterminate styles
let default styles = wrap Default styles
let required styles = wrap Required styles
let valid styles = wrap Valid styles
let invalid styles = wrap Invalid styles
let in_range styles = wrap In_range styles
let out_of_range styles = wrap Out_of_range styles
let placeholder_shown styles = wrap Placeholder_shown styles
let autofill styles = wrap Autofill styles
let read_only styles = wrap Read_only styles
let read_write styles = wrap Read_write styles
let optional styles = wrap Optional styles
let open_ styles = wrap Open styles
let enabled styles = wrap Enabled styles
let target styles = wrap Target styles
let visited styles = wrap Visited styles
let inert styles = wrap Inert styles
let user_valid styles = wrap User_valid styles
let user_invalid styles = wrap User_invalid styles

(* Group/peer structural variants *)
let group_first styles = wrap Group_first styles
let group_last styles = wrap Group_last styles
let group_only styles = wrap Group_only styles
let group_odd styles = wrap Group_odd styles
let group_even styles = wrap Group_even styles
let group_first_of_type styles = wrap Group_first_of_type styles
let group_last_of_type styles = wrap Group_last_of_type styles
let group_only_of_type styles = wrap Group_only_of_type styles
let peer_first styles = wrap Peer_first styles
let peer_last styles = wrap Peer_last styles
let peer_only styles = wrap Peer_only styles
let peer_odd styles = wrap Peer_odd styles
let peer_even styles = wrap Peer_even styles
let peer_first_of_type styles = wrap Peer_first_of_type styles
let peer_last_of_type styles = wrap Peer_last_of_type styles
let peer_only_of_type styles = wrap Peer_only_of_type styles

(* More group/peer state variants *)
let group_active styles = wrap Group_active styles
let group_visited styles = wrap Group_visited styles
let group_disabled styles = wrap Group_disabled styles
let group_checked styles = wrap Group_checked styles
let group_empty styles = wrap Group_empty styles
let group_required styles = wrap Group_required styles
let group_valid styles = wrap Group_valid styles
let group_invalid styles = wrap Group_invalid styles
let group_indeterminate styles = wrap Group_indeterminate styles
let group_default styles = wrap Group_default styles
let group_open styles = wrap Group_open styles
let group_target styles = wrap Group_target styles
let peer_active styles = wrap Peer_active styles
let peer_visited styles = wrap Peer_visited styles
let peer_disabled styles = wrap Peer_disabled styles
let peer_empty styles = wrap Peer_empty styles
let peer_required styles = wrap Peer_required styles
let peer_valid styles = wrap Peer_valid styles
let peer_invalid styles = wrap Peer_invalid styles
let peer_indeterminate styles = wrap Peer_indeterminate styles
let peer_default styles = wrap Peer_default styles
let peer_open styles = wrap Peer_open styles
let peer_target styles = wrap Peer_target styles
let group_optional styles = wrap Group_optional styles
let peer_optional styles = wrap Peer_optional styles
let group_read_only styles = wrap Group_read_only styles
let peer_read_only styles = wrap Peer_read_only styles
let group_read_write styles = wrap Group_read_write styles
let peer_read_write styles = wrap Peer_read_write styles
let group_inert styles = wrap Group_inert styles
let peer_inert styles = wrap Peer_inert styles
let group_user_valid styles = wrap Group_user_valid styles
let peer_user_valid styles = wrap Peer_user_valid styles
let group_user_invalid styles = wrap Group_user_invalid styles
let peer_user_invalid styles = wrap Peer_user_invalid styles
let group_placeholder_shown styles = wrap Group_placeholder_shown styles
let peer_placeholder_shown styles = wrap Peer_placeholder_shown styles
let group_autofill styles = wrap Group_autofill styles
let peer_autofill styles = wrap Peer_autofill styles
let group_in_range styles = wrap Group_in_range styles
let peer_in_range styles = wrap Peer_in_range styles
let group_out_of_range styles = wrap Group_out_of_range styles
let peer_out_of_range styles = wrap Peer_out_of_range styles
let group_focus_within styles = wrap Group_focus_within styles
let peer_focus_within styles = wrap Peer_focus_within styles
let group_focus_visible styles = wrap Group_focus_visible styles
let peer_focus_visible styles = wrap Peer_focus_visible styles
let group_enabled styles = wrap Group_enabled styles
let peer_enabled styles = wrap Peer_enabled styles

(* Pseudo-element variants *)
let marker styles = wrap Pseudo_marker styles
let selection styles = wrap Pseudo_selection styles
let placeholder styles = wrap Pseudo_placeholder styles
let backdrop styles = wrap Pseudo_backdrop styles
let file styles = wrap Pseudo_file styles
let first_letter styles = wrap Pseudo_first_letter styles
let first_line styles = wrap Pseudo_first_line styles
let details_content styles = wrap Pseudo_details_content styles
let children styles = wrap Children styles
let descendants styles = wrap Descendants styles

(* Directionality variants *)
let ltr styles = wrap Ltr styles
let rtl styles = wrap Rtl styles

(* Media type variants *)
let print styles = wrap Print styles
let portrait styles = wrap Portrait styles
let landscape styles = wrap Landscape styles
let forced_colors styles = wrap Forced_colors styles
let supports cond styles = wrap (Supports cond) styles

(* Parse modifiers (responsive, states) from class string. Handles brackets
   properly so has-[:checked]:bg-red-500 parses as modifiers=["has-[:checked]"]
   and base_class="bg-red-500" *)
let of_string class_str =
  let len = String.length class_str in
  let rec split_parts acc current_start i bracket_depth paren_depth =
    if i >= len then
      (* End of string - add final part *)
      let part = String.sub class_str current_start (len - current_start) in
      List.rev (part :: acc)
    else
      match class_str.[i] with
      | '[' ->
          split_parts acc current_start (i + 1) (bracket_depth + 1) paren_depth
      | ']' ->
          split_parts acc current_start (i + 1)
            (max 0 (bracket_depth - 1))
            paren_depth
      | '(' ->
          split_parts acc current_start (i + 1) bracket_depth (paren_depth + 1)
      | ')' ->
          split_parts acc current_start (i + 1) bracket_depth
            (max 0 (paren_depth - 1))
      | ':' when bracket_depth = 0 && paren_depth = 0 ->
          (* Split here - colon outside brackets and parens *)
          let part = String.sub class_str current_start (i - current_start) in
          split_parts (part :: acc) (i + 1) (i + 1) 0 0
      | _ -> split_parts acc current_start (i + 1) bracket_depth paren_depth
  in
  let parts = split_parts [] 0 0 0 0 in
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
  | Min_responsive breakpoint -> (
      match breakpoint with
      | `Sm -> "min-sm"
      | `Md -> "min-md"
      | `Lg -> "min-lg"
      | `Xl -> "min-xl"
      | `Xl_2 -> "min-2xl")
  | Max_responsive breakpoint -> (
      match breakpoint with
      | `Sm -> "max-sm"
      | `Md -> "max-md"
      | `Lg -> "max-lg"
      | `Xl -> "max-xl"
      | `Xl_2 -> "max-2xl")
  | Min_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      "min-[" ^ px_str ^ "px]"
  | Max_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      "max-[" ^ px_str ^ "px]"
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
  | First -> "first"
  | Last -> "last"
  | Only -> "only"
  | Odd -> "odd"
  | Even -> "even"
  | First_of_type -> "first-of-type"
  | Last_of_type -> "last-of-type"
  | Only_of_type -> "only-of-type"
  | Nth expr -> "nth-[" ^ expr ^ "]"
  | Nth_last expr -> "nth-last-[" ^ expr ^ "]"
  | Empty -> "empty"
  | Checked -> "checked"
  | Indeterminate -> "indeterminate"
  | Default -> "default"
  | Required -> "required"
  | Valid -> "valid"
  | Invalid -> "invalid"
  | In_range -> "in-range"
  | Out_of_range -> "out-of-range"
  | Placeholder_shown -> "placeholder-shown"
  | Autofill -> "autofill"
  | Read_only -> "read-only"
  | Read_write -> "read-write"
  | Optional -> "optional"
  | Open -> "open"
  | Enabled -> "enabled"
  | Target -> "target"
  | Visited -> "visited"
  | Inert -> "inert"
  | User_valid -> "user-valid"
  | User_invalid -> "user-invalid"
  | Group_first -> "group-first"
  | Group_last -> "group-last"
  | Group_only -> "group-only"
  | Group_odd -> "group-odd"
  | Group_even -> "group-even"
  | Group_first_of_type -> "group-first-of-type"
  | Group_last_of_type -> "group-last-of-type"
  | Group_only_of_type -> "group-only-of-type"
  | Peer_first -> "peer-first"
  | Peer_last -> "peer-last"
  | Peer_only -> "peer-only"
  | Peer_odd -> "peer-odd"
  | Peer_even -> "peer-even"
  | Peer_first_of_type -> "peer-first-of-type"
  | Peer_last_of_type -> "peer-last-of-type"
  | Peer_only_of_type -> "peer-only-of-type"
  | Group_active -> "group-active"
  | Group_visited -> "group-visited"
  | Group_disabled -> "group-disabled"
  | Group_checked -> "group-checked"
  | Group_empty -> "group-empty"
  | Group_required -> "group-required"
  | Group_valid -> "group-valid"
  | Group_invalid -> "group-invalid"
  | Group_indeterminate -> "group-indeterminate"
  | Group_default -> "group-default"
  | Group_open -> "group-open"
  | Group_target -> "group-target"
  | Peer_active -> "peer-active"
  | Peer_visited -> "peer-visited"
  | Peer_disabled -> "peer-disabled"
  | Peer_empty -> "peer-empty"
  | Peer_required -> "peer-required"
  | Peer_valid -> "peer-valid"
  | Peer_invalid -> "peer-invalid"
  | Peer_indeterminate -> "peer-indeterminate"
  | Peer_default -> "peer-default"
  | Peer_open -> "peer-open"
  | Peer_target -> "peer-target"
  | Group_optional -> "group-optional"
  | Peer_optional -> "peer-optional"
  | Group_read_only -> "group-read-only"
  | Peer_read_only -> "peer-read-only"
  | Group_read_write -> "group-read-write"
  | Peer_read_write -> "peer-read-write"
  | Group_inert -> "group-inert"
  | Peer_inert -> "peer-inert"
  | Group_user_valid -> "group-user-valid"
  | Peer_user_valid -> "peer-user-valid"
  | Group_user_invalid -> "group-user-invalid"
  | Peer_user_invalid -> "peer-user-invalid"
  | Group_placeholder_shown -> "group-placeholder-shown"
  | Peer_placeholder_shown -> "peer-placeholder-shown"
  | Group_autofill -> "group-autofill"
  | Peer_autofill -> "peer-autofill"
  | Group_in_range -> "group-in-range"
  | Peer_in_range -> "peer-in-range"
  | Group_out_of_range -> "group-out-of-range"
  | Peer_out_of_range -> "peer-out-of-range"
  | Group_focus_within -> "group-focus-within"
  | Peer_focus_within -> "peer-focus-within"
  | Group_focus_visible -> "group-focus-visible"
  | Peer_focus_visible -> "peer-focus-visible"
  | Group_enabled -> "group-enabled"
  | Peer_enabled -> "peer-enabled"
  | Pseudo_marker -> "marker"
  | Pseudo_selection -> "selection"
  | Pseudo_placeholder -> "placeholder"
  | Pseudo_backdrop -> "backdrop"
  | Pseudo_file -> "file"
  | Pseudo_first_letter -> "first-letter"
  | Pseudo_first_line -> "first-line"
  | Pseudo_details_content -> "details-content"
  | Children -> "*"
  | Descendants -> "**"
  | Ltr -> "ltr"
  | Rtl -> "rtl"
  | Print -> "print"
  | Portrait -> "portrait"
  | Landscape -> "landscape"
  | Forced_colors -> "forced-colors"
  | Inverted_colors -> "inverted-colors"
  | Pointer_none -> "pointer-none"
  | Pointer_coarse -> "pointer-coarse"
  | Pointer_fine -> "pointer-fine"
  | Any_pointer_none -> "any-pointer-none"
  | Any_pointer_coarse -> "any-pointer-coarse"
  | Any_pointer_fine -> "any-pointer-fine"
  | Noscript -> "noscript"
  | Supports cond -> "supports-[" ^ cond ^ "]"

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
    Option.map (fun i -> String.sub rest 0 i) (matching_bracket rest)
  else None

(* Parse a pixel value from a string like "600px" or "600" *)
let parse_px_value s =
  let s =
    if String.ends_with ~suffix:"px" s then String.sub s 0 (String.length s - 2)
    else s
  in
  try Some (float_of_string s) with Failure _ -> None

(* Try parsing a bracketed modifier, returning Some if matched *)
let try_bracketed_modifier s =
  let ( let* ) = Option.bind in
  let try_pattern prefix make =
    let* content = extract_bracket_content ~prefix s in
    Some (make content)
  in
  let try_pattern_with prefix parse make =
    let* content = extract_bracket_content ~prefix s in
    let* value = parse content in
    Some (make value)
  in
  (* Order matters - more specific prefixes first *)
  let patterns =
    [
      (fun () -> try_pattern "group-has-[" (fun sel -> Group_has sel));
      (fun () -> try_pattern "peer-has-[" (fun sel -> Peer_has sel));
      (fun () -> try_pattern "has-[" (fun sel -> Has sel));
      (fun () ->
        try_pattern_with "min-[" parse_px_value (fun px -> Min_arbitrary px));
      (fun () ->
        try_pattern_with "max-[" parse_px_value (fun px -> Max_arbitrary px));
      (fun () -> try_pattern "nth-last-[" (fun e -> Nth_last e));
      (fun () -> try_pattern "nth-[" (fun e -> Nth e));
      (fun () -> try_pattern "supports-[" (fun c -> Supports c));
    ]
  in
  match List.find_map (fun f -> f ()) patterns with
  | Some _ as r -> r
  | None ->
      (* Handle supports-<property> shorthand *)
      if
        String.length s > 9
        && String.sub s 0 9 = "supports-"
        && not (String.contains s '[')
      then
        let prop = String.sub s 9 (String.length s - 9) in
        Some (Supports (prop ^ ": var(--tw)"))
      else
        let* content = extract_bracket_content ~prefix:"data-[" s in
        let key, value =
          match String.index_opt content '=' with
          | Some i ->
              ( String.sub content 0 i,
                String.sub content (i + 1) (String.length content - i - 1) )
          | None -> (content, "")
        in
        Some (Data_custom (key, value))

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
    (* Container queries *)
    ("@sm", Container Container_sm);
    ("@md", Container Container_md);
    ("@lg", Container Container_lg);
    ("@xl", Container Container_xl);
    ("@2xl", Container Container_2xl);
  ]

(* Parse a modifier string into a typed Style.modifier *)
let parse_modifier s : modifier option =
  match List.assoc_opt s simple_modifiers with
  | Some m -> Some m
  | None -> (
      match try_bracketed_modifier s with
      | Some _ as r -> r
      | None ->
          (* Try not-* prefix: strip "not-" and wrap inner modifier *)
          if String.length s > 4 && String.sub s 0 4 = "not-" then
            let inner = String.sub s 4 (String.length s - 4) in
            match List.assoc_opt inner simple_modifiers with
            | Some m -> Some (Not m)
            | None -> None
          else None)

(* Apply a list of modifier strings to a base utility *)
let apply modifiers base_utility =
  (* Convert utility to a list for wrapping *)
  let to_list = function
    | Utility.Group styles -> styles
    | single -> [ single ]
  in
  (* Apply a single parsed modifier to an accumulated utility *)
  let apply_one acc modifier_str =
    match acc with
    | None -> None
    | Some u -> (
        match parse_modifier modifier_str with
        | Some m -> Some (wrap m (to_list u))
        | None -> None)
  in
  (* Apply modifiers in reverse order so that the first modifier in the string
     (e.g., "dark" in "dark:hover:...") ends up as the outermost wrapper
     (Modified(Dark, Modified(Hover, base))). This matches how the programmatic
     API works: dark [ hover [ ... ] ] *)
  List.fold_left apply_one (Some base_utility) (List.rev modifiers)
