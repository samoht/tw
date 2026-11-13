(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

module Css = Cascade.Css

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
let arbitrary_breakpoint_class prefix (w : Style.arbitrary_px) cls =
  Css.Selector.Class (prefix ^ "[" ^ w.text ^ "]:" ^ cls)

(* One spelling for a class name, shared with [Style], which names the same
   utilities through [Style.to_class]. *)
let compact_length = Style.compact_length

(** Build class selector for an arbitrary length breakpoint *)
let arbitrary_length_class prefix (l : Style.arbitrary_length) cls =
  Css.Selector.Class (prefix ^ "[" ^ l.text ^ "]:" ^ cls)

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

let nest_selector ~parent template =
  let open Css.Selector in
  let sel = Css.Selector.read (Cascade.Cursor.of_string template) in
  if Cascade.Nest.contains sel then Cascade.Nest.substitute ~parent sel
  else
    (* No anchor: the template compounds onto the parent. A type selector cannot
       follow a class in a compound, so it goes in an [:is()]; anything else
       ([.line], [[data-x]], [:hover]) attaches directly. *)
    let inner =
      match sel with
      | Element _ | Compound (Element _ :: _) -> is_ [ sel ]
      | _ -> sel
    in
    compound [ parent; inner ]

(** Parse arbitrary bracket content into a selector tree. In bracket content:
    [_] = space, [&] = anchor (group/peer). E.g. "&_p" with group anchor ->
    ":where(.group) p" E.g. "&:hover" with group anchor ->
    ":where(.group):hover" *)
let parse_arbitrary_selector_content content anchor =
  let open Css.Selector in
  let s = Parse.decode_underscores content in
  combine (nest_selector ~parent:(where [ anchor ]) s) Descendant universal

(** Build an anchor-based variant selector: :where(.anchor):pseudo combinator *)
let anchor_pseudo_selector ~anchor ~combinator cls prefix pseudo =
  let open Css.Selector in
  let rel =
    combine (compound [ where [ anchor ]; pseudo ]) combinator universal
  in
  compound [ Class (prefix ^ ":" ^ cls); is_ [ rel ] ]

(** Build a hocus (hover + focus) variant selector for an anchor *)
let anchor_hocus_selector ~anchor ~combinator cls label =
  let open Css.Selector in
  let rel =
    combine
      (is_
         [
           compound [ where [ anchor ]; Hover ];
           compound [ where [ anchor ]; Focus ];
         ])
      combinator universal
  in
  compound [ Class (label ^ ":" ^ cls); is_ [ rel ] ]

(** Build an arbitrary bracket variant selector for an anchor *)
let anchor_arbitrary_selector ~anchor ~combinator cls sel label =
  let open Css.Selector in
  let prefix = label ^ "-[" ^ sel ^ "]" in
  let rel = parse_arbitrary_selector_content sel anchor in
  let rel =
    match combinator with
    | Css.Selector.Subsequent_sibling -> (
        (* For peer, replace outermost Descendant with Subsequent_sibling (~) *)
        match rel with
        | Combined (left, Descendant, (Universal _ as right)) ->
            Combined (left, Subsequent_sibling, right)
        | other -> other)
    | _ -> rel
  in
  compound [ Class (prefix ^ ":" ^ cls); is_ [ rel ] ]

(** Group variant selector — :where(.group):pseudo descendant *)
let group_selector cls modifier =
  let gp = anchor_pseudo_selector ~anchor:group ~combinator:Descendant cls in
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
  | Group_hocus ->
      anchor_hocus_selector ~anchor:group ~combinator:Descendant cls
        "group-hocus"
  | Group_arbitrary sel ->
      anchor_arbitrary_selector ~anchor:group ~combinator:Descendant cls sel
        "group"
  | _ -> Css.Selector.Class cls

(** Peer variant selector — :where(.peer):pseudo ~ *)
let peer_selector cls modifier =
  let pp =
    anchor_pseudo_selector ~anchor:peer ~combinator:Subsequent_sibling cls
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
  | Peer_hocus ->
      anchor_hocus_selector ~anchor:peer ~combinator:Subsequent_sibling cls
        "peer-hocus"
  | Peer_arbitrary sel ->
      anchor_arbitrary_selector ~anchor:peer ~combinator:Subsequent_sibling cls
        sel "peer"
  | _ -> Css.Selector.Class cls

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
  | Min_arbitrary_length l -> arbitrary_length_class "min-" l cls
  | Max_arbitrary_length l -> arbitrary_length_class "max-" l cls
  | Custom_responsive name -> Css.Selector.Class (name ^ ":" ^ cls)
  | Min_custom name -> Css.Selector.Class ("min-" ^ name ^ ":" ^ cls)
  | Max_custom name -> Css.Selector.Class ("max-" ^ name ^ ":" ^ cls)
  | Peer_hover | Peer_focus | Peer_checked | Peer_first | Peer_last | Peer_odd
  | Peer_even | Peer_only | Peer_first_of_type | Peer_last_of_type
  | Peer_only_of_type | Peer_active | Peer_visited | Peer_disabled | Peer_empty
  | Peer_required | Peer_valid | Peer_invalid | Peer_indeterminate
  | Peer_default | Peer_open | Peer_target | Peer_optional | Peer_read_only
  | Peer_read_write | Peer_inert | Peer_user_valid | Peer_user_invalid
  | Peer_placeholder_shown | Peer_autofill | Peer_in_range | Peer_out_of_range
  | Peer_focus_within | Peer_focus_visible | Peer_enabled | Peer_hocus
  | Peer_arbitrary _ ->
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
  | Nth n ->
      let nth, of_ = Variant_parse.nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_child (nth, of_) ]
  | Nth_last n ->
      let nth, of_ = Variant_parse.nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth-last" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_last_child (nth, of_) ]
  | Nth_of_type n ->
      let nth, of_ = Variant_parse.nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth-of-type" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_of_type (nth, of_) ]
  | Nth_last_of_type n ->
      let nth, of_ = Variant_parse.nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth-last-of-type" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_last_of_type (nth, of_) ]
  | Empty -> cp "empty" cls Css.Selector.Empty
  | _ -> media_prefix_selector cls modifier

(** Aria and data attribute modifier selectors *)
let aria_data_selector cls modifier =
  let open Css.Selector in
  match modifier with
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
  | Data_custom (attr, "") ->
      compound
        [
          Class ("data-" ^ attr ^ ":" ^ cls);
          attribute ("data-" ^ attr) Presence;
        ]
  | Data_custom (attr, value) ->
      compound
        [
          Class ("data-[" ^ attr ^ "=" ^ value ^ "]:" ^ cls);
          attribute ("data-" ^ attr) (Exact value);
        ]
  | _ -> structural_selector cls modifier

(** Pseudo-element modifier selectors *)
let pseudo_element_selector cls modifier =
  let cp = class_pseudo in
  match modifier with
  | Pseudo_before ->
      Css.Selector.compound
        [ before cls; Css.Selector.Before Css.Selector.Double ]
  | Pseudo_after ->
      Css.Selector.compound
        [ after cls; Css.Selector.After Css.Selector.Double ]
  | Pseudo_marker -> cp "marker" cls Css.Selector.Marker
  | Pseudo_selection -> cp "selection" cls Css.Selector.Selection
  | Pseudo_placeholder -> cp "placeholder" cls Css.Selector.Placeholder
  | Pseudo_backdrop -> cp "backdrop" cls Css.Selector.Backdrop
  | Pseudo_file -> cp "file" cls Css.Selector.File_selector_button
  | Pseudo_first_letter ->
      cp "first-letter" cls (Css.Selector.First_letter Css.Selector.Double)
  | Pseudo_first_line ->
      cp "first-line" cls (Css.Selector.First_line Css.Selector.Double)
  | Pseudo_details_content ->
      cp "details-content" cls Css.Selector.Details_content
  | _ -> aria_data_selector cls modifier

(** Map prose element variant names to their CSS element selectors *)
let prose_element_selectors name =
  let open Css.Selector in
  match name with
  | "headings" ->
      [
        Element (None, "h1");
        Element (None, "h2");
        Element (None, "h3");
        Element (None, "h4");
        Element (None, "h5");
        Element (None, "h6");
        Element (None, "th");
      ]
  | "lead" ->
      [ Attribute (None, Regular "class", Whitespace_list "lead", None) ]
  | s -> [ Element (None, s) ]

(** Build the inner prose element selector for a prose element variant. Creates
    ":where(ELTS):not(:where(not-prose, not-prose descendant))" which is
    combined as a descendant of the outer class selector. *)
let not_prose_exclusion_selector () =
  let open Css.Selector in
  let not_prose_class = attribute "class" (Whitespace_list "not-prose") in
  let not_prose_descendant = combine not_prose_class Descendant universal in
  let not_prose_where = where [ not_prose_class; not_prose_descendant ] in
  not [ not_prose_where ]

let prose_element_inner_selector name =
  let open Css.Selector in
  compound
    [ where (prose_element_selectors name); not_prose_exclusion_selector () ]

(** Generate CSS selector for a modifier and base class *)
let to_selector (modifier : modifier) cls =
  let open Css.Selector in
  match modifier with
  | Hover -> compound [ hover cls; Hover ]
  | Focus -> compound [ focus cls; Focus ]
  | Active -> compound [ active cls; Active ]
  | Disabled -> compound [ disabled cls; Disabled ]
  | Focus_within -> compound [ focus_within cls; Focus_within ]
  | Focus_visible -> compound [ focus_visible cls; Focus_visible ]
  (* Child/descendant selectors *)
  | Children ->
      let child_sel = combine (Class ("*:" ^ cls)) Child universal in
      is_ [ child_sel ]
  | Descendants ->
      let desc_sel = combine (Class ("**:" ^ cls)) Descendant universal in
      is_ [ desc_sel ]
  | Ltr -> dir_selector "ltr" cls
  | Rtl -> dir_selector "rtl" cls
  (* Hocus/Device_hocus — compound :hover, :focus *)
  | Hocus -> compound [ Class ("hocus:" ^ cls); is_ [ Hover; Focus ] ]
  | Device_hocus ->
      compound [ Class ("device-hocus:" ^ cls); is_ [ Hover; Focus ] ]
  (* Prose element variants — the class, then the elements it targets inside it.
     Returning the class alone loses the descendant part wherever the selector
     is built from here rather than in the routing, which is every rule that is
     already a query: [prose-p:md:text-lg] styled the container instead of its
     paragraphs. *)
  | Prose_element name ->
      combine
        (Class ("prose-" ^ name ^ ":" ^ cls))
        Descendant
        (prose_element_inner_selector name)
  (* Pseudo-elements, aria/data, structural, media, peer, form state *)
  | _ -> pseudo_element_selector cls modifier

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
let group_hocus = wrap Group_hocus
let peer_hocus = wrap Peer_hocus

(* :has() helpers *)
let has selector styles = wrap (Has selector) styles
let group_has ?name selector styles = wrap (Group_has (selector, name)) styles
let peer_has ?name selector styles = wrap (Peer_has (selector, name)) styles

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
  | Utility.Important (_, t) -> has_responsive_modifier t
  | Utility.Aliased (_, t) | Utility.Theme_bound (_, t) ->
      has_responsive_modifier t

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
(* A caller with a bare number has written no bracket, so spell one the way
   [min-[600px]] reads. *)
let arbitrary_px px : Style.arbitrary_px = { px; text = Pp.float px ^ "px" }

let min_arbitrary px styles =
  validate_no_nested_responsive styles;
  wrap (Min_arbitrary (arbitrary_px px)) styles

let max_arbitrary px styles =
  validate_no_nested_responsive styles;
  wrap (Max_arbitrary (arbitrary_px px)) styles

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
let nth expr styles = wrap (Nth (Style.nth_expr expr)) styles
let nth_last expr styles = wrap (Nth_last (Style.nth_expr expr)) styles
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
let supports cond styles = wrap (Supports_condition cond) styles

(* Prose element variants *)
let prose_headings styles = wrap (Prose_element "headings") styles
let prose_p styles = wrap (Prose_element "p") styles
let prose_a styles = wrap (Prose_element "a") styles
let prose_strong styles = wrap (Prose_element "strong") styles
let prose_em styles = wrap (Prose_element "em") styles
let prose_code styles = wrap (Prose_element "code") styles
let prose_pre styles = wrap (Prose_element "pre") styles
let prose_ol styles = wrap (Prose_element "ol") styles
let prose_ul styles = wrap (Prose_element "ul") styles
let prose_li styles = wrap (Prose_element "li") styles
let prose_blockquote styles = wrap (Prose_element "blockquote") styles
let prose_h1 styles = wrap (Prose_element "h1") styles
let prose_h2 styles = wrap (Prose_element "h2") styles
let prose_h3 styles = wrap (Prose_element "h3") styles
let prose_h4 styles = wrap (Prose_element "h4") styles
let prose_h5 styles = wrap (Prose_element "h5") styles
let prose_h6 styles = wrap (Prose_element "h6") styles
let prose_dl styles = wrap (Prose_element "dl") styles
let prose_dt styles = wrap (Prose_element "dt") styles
let prose_dd styles = wrap (Prose_element "dd") styles
let prose_table styles = wrap (Prose_element "table") styles
let prose_tr styles = wrap (Prose_element "tr") styles
let prose_picture styles = wrap (Prose_element "picture") styles
let prose_img styles = wrap (Prose_element "img") styles
let prose_video styles = wrap (Prose_element "video") styles
let prose_figure styles = wrap (Prose_element "figure") styles
let prose_figcaption styles = wrap (Prose_element "figcaption") styles
let prose_hr styles = wrap (Prose_element "hr") styles
let prose_th styles = wrap (Prose_element "th") styles
let prose_td styles = wrap (Prose_element "td") styles
let prose_thead styles = wrap (Prose_element "thead") styles
let prose_kbd styles = wrap (Prose_element "kbd") styles
let prose_lead styles = wrap (Prose_element "lead") styles

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

(* One table, in [Style]: this renders the class name that [Utility.to_class]
   puts in the class attribute, and the selector built here has to be the
   selector that class matches. Two tables drift, and the drift is invisible
   until a class fails to select its own rule. *)
let pp_modifier = Style.pp_modifier

(* Apply a list of modifier strings to a base utility *)
let apply ?(theme = Scheme.default) modifiers base_utility =
  (* Convert utility to a list for wrapping *)
  let to_list = function
    | Utility.Group styles -> styles
    | single -> [ single ]
  in
  (* Apply a single parsed modifier to an accumulated utility *)
  let apply_one acc modifier_str =
    Option.bind acc (fun u ->
        Option.map
          (fun m -> wrap m (to_list u))
          (Variant_parse.modifier ~theme modifier_str))
  in
  (* Apply modifiers in reverse order so that the first modifier in the string
     (e.g., "dark" in "dark:hover:...") ends up as the outermost wrapper
     (Modified(Dark, Modified(Hover, base))). This matches how the programmatic
     API works: dark [ hover [ ... ] ] *)
  List.fold_left apply_one (Some base_utility) (List.rev modifiers)
