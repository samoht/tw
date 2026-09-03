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

(** Parse an nth expression that may contain an "of <selector>" clause. Bracket
    content uses underscores for spaces, e.g. "2n+1_of_.foo" → nth = 2n+1, of_ =
    Some [Class "foo"]. *)
let parse_nth_selector expr =
  let spaced = Parse.decode_underscores expr in
  Css.Selector.read_nth_selector (Cascade.Cursor.of_string spaced)

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
    if s = name then
      Option.map
        (fun v -> Custom_variant (s, custom_variant_apply cv.template v))
        (List.assoc_opt "" cv.values)
    else
      let prefix = name ^ "-" in
      if
        String.length s > String.length prefix
        && String.sub s 0 (String.length prefix) = prefix
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
        Option.map
          (fun v -> Custom_variant (s, custom_variant_apply cv.template v))
          value
      else None
  in
  List.find_map (fun (name, cv) -> resolve name cv) theme.custom_variants

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
      if String.length s > 4 && String.sub s 0 4 = "not-" then
        let inner = String.sub s 4 (String.length s - 4) in
        Option.map
          (fun cond -> Container_style (s, negate_container cond))
          (lookup inner)
      else None

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
      let nth, of_ = parse_nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_child (nth, of_) ]
  | Nth_last n ->
      let nth, of_ = parse_nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth-last" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_last_child (nth, of_) ]
  | Nth_of_type n ->
      let nth, of_ = parse_nth_selector n.Style.expr in
      let prefix = Style.pp_nth "nth-of-type" n in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_of_type (nth, of_) ]
  | Nth_last_of_type n ->
      let nth, of_ = parse_nth_selector n.Style.expr in
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
  | Utility.Aliased (_, t) -> has_responsive_modifier t

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

(* Convert modifier to its string prefix *)
let group_state_modifiers = Style.group_state_modifiers
let is_has_shorthand = Style.is_has_shorthand

(* One table, in [Style]: this renders the class name that [Utility.to_class]
   puts in the class attribute, and the selector built here has to be the
   selector that class matches. Two tables drift, and the drift is invisible
   until a class fails to select its own rule. *)
let pp_modifier = Style.pp_modifier

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
  if String.length s > 11 && String.sub s 0 11 = "group-aria-" then
    let rest = String.sub s 11 (String.length s - 11) in
    let base, name = split_name rest in
    if is_aria_shorthand base then Some (Group_aria (base, name)) else None
  else if String.length s > 10 && String.sub s 0 10 = "peer-aria-" then
    let rest = String.sub s 10 (String.length s - 10) in
    let base, name = split_name rest in
    if is_aria_shorthand base then Some (Peer_aria (base, name)) else None
  else None

let try_has_shorthand s =
  if String.length s > 4 && String.sub s 0 4 = "has-" then
    let rest = String.sub s 4 (String.length s - 4) in
    if is_has_shorthand rest then Some (Has rest) else None
  else if String.length s > 10 && String.sub s 0 10 = "group-has-" then
    let rest = String.sub s 10 (String.length s - 10) in
    let base, name = split_name rest in
    if is_has_shorthand base then Some (Group_has (base, name)) else None
  else if String.length s > 9 && String.sub s 0 9 = "peer-has-" then
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
   stands for a space here, the decoding {!nest_selector} applies. *)
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
  if String.length s <= plen || String.sub s 0 plen <> prefix then None
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
   spelling: it is what the variant's own class is named after. *)
let parse_px_value s =
  let digits =
    if String.ends_with ~suffix:"px" s then String.sub s 0 (String.length s - 2)
    else s
  in
  match float_of_string_opt digits with
  | Some px -> Some ({ px; text = s } : Style.arbitrary_px)
  | None -> None

(* Parse a has-selector string as a relative CSS selector ([:has()] accepts a
   bare leading combinator, e.g. [>div] or [~img]), with [&] resolved to the
   universal selector via {!Cascade.Nest.substitute}, the same substitution
   {!nest_selector} performs for anchored templates. *)
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

(* An at-rule in brackets is a variant too: [[@supports(display:grid)]] and
   [[@starting-style]] wrap the utility rather than select it. *)
let try_bracket_at_rule s =
  if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then
    let inner = String.sub s 1 (String.length s - 2) in
    if
      inner = "@starting-style"
      || (String.length inner > 9 && String.sub inner 0 9 = "@supports")
    then Some (At_rule inner)
    else None
  else None

let normalize_supports_condition condition_str =
  let cond = Parse.decode_underscores condition_str in
  if
    String.length cond > 2
    && cond.[0] = '-'
    && cond.[1] = '-'
    && not (String.contains cond ':')
  then
    (* Bare custom property: --test tests against var(--tw), built directly
       rather than assembled as "(--test: var(--tw))" and re-parsed. *)
    Css.Supports.property cond "var(--tw)"
  else if cond <> "" && cond.[0] = '(' && cond.[String.length cond - 1] = ')'
  then
    (* Already parenthesised, so it is the condition the author wrote; only the
       real grammar reader can make sense of arbitrary authored text. *)
    Css.Supports.of_string cond
  else if String.contains cond ':' then
    (* [prop: value], the property test Tailwind emits, built directly. *)
    let i = String.index cond ':' in
    let prop = String.trim (String.sub cond 0 i) in
    let value =
      String.trim (String.sub cond (i + 1) (String.length cond - i - 1))
    in
    Css.Supports.property prop value
  else if String.contains cond '(' then
    (* Function call like font-format(opentype) or var(--test); again arbitrary
       authored text, so the real grammar reader parses it. *)
    Css.Supports.of_string cond
  else
    (* Bare property name: backdrop-filter tests against var(--tw), the same
       expansion as the bare custom property above. A lone identifier is not a
       condition the CSS grammar has a production for, so leaving it raised a
       parse error out of the scanner. *)
    Css.Supports.property cond "var(--tw)"

(* Validate that a supports-[...] bracket normalizes to a condition the
   [@supports] grammar has a production for. An empty or half-written one used
   to reach the condition reader and raise from there. *)
let is_valid_supports_condition cond =
  cond <> ""
  &&
    try
      ignore (normalize_supports_condition cond);
      true
    with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> false

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
    ignore (parse_nth_selector expr);
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
    String.length s > 9
    && String.sub s 0 9 = "supports-"
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
    if String.length s > plen && String.sub s 0 plen = prefix then
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
  match List.mem s breakpoints with
  | true -> Some (Custom_responsive s)
  | false ->
      (* min-<name>: e.g., "min-10xl" → Min_custom *)
      if String.length s > 4 && String.sub s 0 4 = "min-" then
        let name = String.sub s 4 (String.length s - 4) in
        if List.mem name breakpoints then Some (Min_custom name) else None
      else if String.length s > 4 && String.sub s 0 4 = "max-" then
        let name = String.sub s 4 (String.length s - 4) in
        if List.mem name breakpoints then Some (Max_custom name) else None
      else None

(** Try not-* shorthand patterns that aren't in simple_modifiers or bracket
    patterns. These are modifiers like data-foo, has-checked, nth-2 that work as
    shorthands in the not-* context. *)
let try_not_shorthand inner =
  (* supports-X shorthand *)
  if
    String.length inner > 9
    && String.sub inner 0 9 = "supports-"
    && (not (String.contains inner '['))
    && not (String.contains inner '/')
  then
    let prop = String.sub inner 9 (String.length inner - 9) in
    Some (Not (Supports_property prop))
    (* data-X shorthand — attribute presence check *)
  else if String.length inner > 5 && String.sub inner 0 5 = "data-" then
    let attr = String.sub inner 5 (String.length inner - 5) in
    Some (Not (Data_custom (attr, "")))
    (* has-X shorthand — :has(:X) pseudo-class *)
  else if String.length inner > 4 && String.sub inner 0 4 = "has-" then
    (* The shorthand names a pseudo-class, so it has to read as one; the bracket
       form validates its selector the same way. *)
    let sel = ":" ^ String.sub inner 4 (String.length inner - 4) in
    if is_valid_has_selector sel then Some (Not (Has sel)) else None
    (* nth-X shorthand — :nth-child(X) *)
  else if String.length inner > 4 && String.sub inner 0 4 = "nth-" then
    let expr = String.sub inner 4 (String.length inner - 4) in
    Some (Not (Nth (Style.nth_expr expr)))
  else None

(** Check if a modifier is compatible with not-* negation. Pseudo-elements,
    starting style, children/descendants, and container queries cannot be
    negated: a container query has no negated selector form, and tw used to
    build [.not-\@md\:flex:not(.flex)] for one, negating the utility's own class
    so the rule matched nothing. *)
let is_not_compatible = function
  | Pseudo_before | Pseudo_after | Pseudo_marker | Pseudo_selection
  | Pseudo_placeholder | Pseudo_backdrop | Pseudo_file | Pseudo_first_letter
  | Pseudo_first_line | Pseudo_details_content | Starting | Children
  | Descendants | Prose_element _ | Container _ ->
      false
  | _ -> true

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
      (String.length content > 6 && String.sub content 0 6 = "@media")
      || (String.length content > 7 && String.sub content 0 7 = "@media_")
    then not (String.contains content ',')
    else if first = ':' then true
    else reads_as_selector content

(** Try parsing a not-[...] bracket pattern. Returns the Not_bracket modifier
    for pseudo-class or media bracket content. *)
let try_not_bracket inner =
  if inner <> "" && inner.[0] = '[' then
    let rest = String.sub inner 1 (String.length inner - 1) in
    match matching_bracket rest with
    | Some i when i = String.length rest - 1 ->
        let content = String.sub rest 0 i in
        if is_valid_not_bracket_content content then Some (Not_bracket content)
        else None
    | Some i ->
        (* There's content after the bracket — check for /name suffix *)
        let remainder = String.sub rest (i + 1) (String.length rest - i - 1) in
        if String.length remainder > 0 && remainder.[0] = '/' then
          (* not-[:checked]/foo — named not-bracket variants are invalid *)
          None
        else None
    | _ -> None
  else None

(** Parse group-not-* or peer-not-* pattern. Splits the rest into inner modifier
    and optional /name suffix. *)
let parse_group_peer_not_inner rest =
  (* Check for bracket content first: [...]/name or [...] *)
  if rest <> "" && rest.[0] = '[' then
    let after_bracket = String.sub rest 1 (String.length rest - 1) in
    match matching_bracket after_bracket with
    | Some i ->
        let content = String.sub after_bracket 0 i in
        let remainder =
          String.sub after_bracket (i + 1) (String.length after_bracket - i - 1)
        in
        let name =
          if String.length remainder > 1 && remainder.[0] = '/' then
            Some (String.sub remainder 1 (String.length remainder - 1))
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
          | Some m when not (renders_as_media m) -> Some m
          | Some _ | None -> None
        in
        Option.map (fun m -> (m, Some name)) inner_mod
    | None -> (
        match List.assoc_opt rest simple_modifiers with
        | Some m when not (renders_as_media m) -> Some (m, None)
        | Some _ | None -> None)

(* Try to parse compound named group variants: not-group-STATE/name,
   has-group-STATE/name, in-group-STATE/name, group-peer-STATE/name *)
let try_compound_named_group s =
  let try_match prefix make =
    let plen = String.length prefix in
    if String.length s > plen && String.sub s 0 plen = prefix then
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
  if not (String.length s > 3 && String.sub s 0 3 = "in-") then None
  else
    let rest = String.sub s 3 (String.length s - 3) in
    if rest <> "" && rest.[0] = '[' then
      let after = String.sub rest 1 (String.length rest - 1) in
      match matching_bracket after with
      | Some i when i = String.length after - 1 ->
          Some (In_bracket (String.sub after 0 i))
      | _ -> None
    else if String.length rest > 5 && String.sub rest 0 5 = "data-" then
      Some (In_data (String.sub rest 5 (String.length rest - 5)))
    else
      (* [in-focus]: an ancestor in that state, the same state names the other
         variants take. *)
      match List.assoc_opt rest group_state_modifiers with
      | Some m -> Some (In_state (m, rest))
      | None -> None

(* Try not-in-[...] pattern *)
let try_not_in_modifier s =
  if not (String.length s > 8 && String.sub s 0 7 = "not-in-" && s.[7] = '[')
  then None
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
    if String.length s > plen && String.sub s 0 plen = prefix then
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
  if not (String.length s > 4 && String.sub s 0 4 = "not-") then None
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
              | Some m -> Some (Not m)
              | None -> None);
            (fun () -> try_not_shorthand inner);
          ]
        in
        List.find_map (fun f -> f ()) fns

(* Try bare data-X or aria-X shorthand *)
let try_bare_data_aria s =
  if
    String.length s > 5
    && String.sub s 0 5 = "data-"
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then Some (Data_custom (String.sub s 5 (String.length s - 5), ""))
  else if
    String.length s > 5
    && String.sub s 0 5 = "aria-"
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then
    let expr = String.sub s 5 (String.length s - 5) in
    if names_attribute expr then Some (Aria_bracket expr) else None
  else None

(* The order @tailwindcss/typography registers its element variants in, which is
   the order it emits their rules in. A variant missing from here sorts before
   every other, so its utility loses to one it should win against. *)
let prose_element_variant_order = function
  | "headings" -> 96001
  | "h1" -> 96002
  | "h2" -> 96003
  | "h3" -> 96004
  | "h4" -> 96005
  | "h5" -> 96006
  | "h6" -> 96007
  | "p" -> 96008
  | "a" -> 96009
  | "blockquote" -> 96010
  | "figure" -> 96011
  | "figcaption" -> 96012
  | "strong" -> 96013
  | "em" -> 96014
  | "kbd" -> 96015
  | "code" -> 96016
  | "pre" -> 96017
  | "ol" -> 96018
  | "ul" -> 96019
  | "li" -> 96020
  | "dl" -> 96021
  | "dt" -> 96022
  | "dd" -> 96023
  | "table" -> 96024
  | "thead" -> 96025
  | "tr" -> 96026
  | "th" -> 96027
  | "td" -> 96028
  | "img" -> 96029
  | "picture" -> 96030
  | "video" -> 96031
  | "hr" -> 96032
  | "lead" -> 96033
  | _ -> 96000

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
  if String.length s > 6 && String.sub s 0 6 = "prose-" then
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

(* A container-query arbitrary value may reference a theme token, e.g.
   [@min-[theme(--breakpoint-lg)]]. [theme(--breakpoint-lg)] resolves to the
   [--breakpoint-lg] default (64rem) via the same token table the [lg:] variant
   publishes, so a plain length parse falls back to token resolution. *)
let parse_container_length content =
  match Css.parse_length content with
  | Some _ as len -> len
  | None ->
      let s = String.trim content in
      (* Tailwind spells the theme lookup [theme(--x)] and [--theme(--x)]. *)
      let s =
        if String.length s > 2 && String.sub s 0 2 = "--" then
          String.sub s 2 (String.length s - 2)
        else s
      in
      let n = String.length s in
      if n > 7 && String.sub s 0 6 = "theme(" && s.[n - 1] = ')' then
        let inner = String.trim (String.sub s 6 (n - 7)) in
        let name =
          if String.length inner >= 2 && String.sub inner 0 2 = "--" then
            String.sub inner 2 (String.length inner - 2)
          else inner
        in
        Option.bind (Scheme.token_default name) Css.parse_length
      else None

(* [@sm/main] aims a size query at the container named [main]. The name is the
   tail after the last [/]; the head is any other container-query spelling. *)
let rec try_container_query s =
  (* Match ["<prefix>[<len>]"] and build a modifier from the parsed length. *)
  let bracketed prefix mk =
    let plen = String.length prefix and slen = String.length s in
    if
      slen > plen + 2
      && String.sub s 0 plen = prefix
      && s.[plen] = '['
      && s.[slen - 1] = ']'
    then
      let raw = String.sub s (plen + 1) (slen - plen - 2) in
      match parse_container_length raw with
      | Some len -> Some (mk raw len)
      | None -> None
    else None
  in
  (* Match ["<prefix><size>"] against the named size scale. *)
  let sized prefix cmp =
    let plen = String.length prefix in
    if String.length s > plen && String.sub s 0 plen = prefix then
      match
        container_size_of_string (String.sub s plen (String.length s - plen))
      with
      | Some q -> Some (Container (Container_size (cmp, q)))
      | None -> None
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
        (fun () -> try_scoped_container_query s);
      ]

and try_scoped_container_query s =
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
          | None -> try_container_query head
        with
        | Some (Container q) -> Some (Container (Container_scoped (name, q)))
        | _ -> None)

(* Parse a modifier string into a typed Style.modifier *)
let rec parse_modifier ~(theme : Scheme.t) s : modifier option =
  let fns =
    [
      (fun () -> lookup_simple theme s);
      (fun () -> try_container_query s);
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
    ]
  in
  List.find_map (fun f -> f ()) fns

(* [group-not-has-[...]] and [peer-not-...]: the inner is any variant, read on
   its own. The reading above only knows the simple state names and a bare
   bracket. *)
and try_group_peer_not_variant ~theme s =
  let try_prefix prefix make =
    let plen = String.length prefix in
    if String.length s <= plen || String.sub s 0 plen <> prefix then None
    else
      let base, name =
        split_name (String.sub s plen (String.length s - plen))
      in
      match parse_modifier ~theme base with
      (* A plain [not-hover] is fine - it negates the selector and keeps the
         hover media gate. A group or peer negation has only the selector, so a
         media-rendered inner leaves it with nothing to emit. *)
      | Some m when is_not_compatible m && not (renders_as_media m) ->
          Some (make m name)
      | Some _ | None -> None
  in
  match try_prefix "group-not-" (fun i n -> Group_not (i, n)) with
  | Some _ as r -> r
  | None -> try_prefix "peer-not-" (fun i n -> Peer_not (i, n))

(* [has-<variant>]: the argument is any variant, and that variant's own selector
   goes inside [:has()]. The shorthand reading only knows the state names and a
   bracket, so [has-peer-checked] fell through. *)
and try_has_variant ~theme s =
  if String.length s > 4 && String.sub s 0 4 = "has-" then
    match parse_modifier ~theme (String.sub s 4 (String.length s - 4)) with
    | Some m when is_not_compatible m -> Some (Has_variant m)
    | Some _ | None -> None
  else None

and try_not_of_modifier ~theme s =
  if not (String.length s > 4 && String.sub s 0 4 = "not-") then None
  else
    match parse_modifier ~theme (String.sub s 4 (String.length s - 4)) with
    | Some m when is_not_compatible m -> Some (Not m)
    | Some _ | None -> None

(* Apply a list of modifier strings to a base utility *)
let apply ?(theme = Scheme.default) modifiers base_utility =
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
        match parse_modifier ~theme modifier_str with
        | Some m -> Some (wrap m (to_list u))
        | None -> None)
  in
  (* Apply modifiers in reverse order so that the first modifier in the string
     (e.g., "dark" in "dark:hover:...") ends up as the outermost wrapper
     (Modified(Dark, Modified(Hover, base))). This matches how the programmatic
     API works: dark [ hover [ ... ] ] *)
  List.fold_left apply_one (Some base_utility) (List.rev modifiers)

(** {1 Variant Ordering}

    The Tailwind v4 cascade order of variants, written once. A {!Slot.t} names
    one position in that order and {!Slot.rank} is the only place a number
    appears; the three readings below - a modifier constructor, a class-name
    token, a media condition a rule is nested in - each land on a slot rather
    than on a scale of their own. Ordering lives here, in the module that owns
    modifier semantics, rather than in the assembly pipeline. *)

module Slot = struct
  (* One position in the variant cascade. Named separately from [Style.modifier]
     because several modifiers share a position: every [group-*] spelling sorts
     where [group-] sorts, and a [not-X] sorts where X does. *)
  type t =
    | Child  (** [*:]: every direct child. *)
    | Descendant  (** [**:]: every descendant. *)
    | Negation  (** [not-X:], ordered inside the slot by the X it negates. *)
    | Group
    | Peer
    | Pseudo_first_letter
    | Pseudo_first_line
    | Pseudo_marker
    | Pseudo_selection
    | Pseudo_file
    | Pseudo_placeholder
    | Pseudo_backdrop
    | Pseudo_details_content
    | Pseudo_before
    | Pseudo_after
    | First
    | Last
    | Only
    | Odd
    | Even
    | First_of_type
    | Last_of_type
    | Only_of_type
    | Visited
    | Target
    | Open
    | Default
    | Checked
    | Indeterminate
    | Placeholder_shown
    | Autofill
    | Optional
    | Required
    | Valid
    | Invalid
    | User_valid
    | User_invalid
    | In_range
    | Out_of_range
    | Read_only
    | Read_write
    | Empty
    | Focus_within
    | Hover
    | Focus
    | Focus_visible
    | Active
    | Enabled
    | Disabled
    | Inert
    | Ancestor  (** [in-*]: a state on an ancestor. *)
    | Has
    | Aria_named
    | Aria_arbitrary
    | Data_named
    | Data_arbitrary
    | Nth
    | Nth_last
    | Nth_of_type
    | Nth_last_of_type
    | Hocus
    | Supports
    | Motion_safe
    | Motion_reduce
    | Contrast_more
    | Contrast_less
    | Pointer
    | Any_pointer
    | Breakpoint  (** Every [sm:]/[max-lg:]/[min-[32rem]:] spelling. *)
    | Portrait
    | Landscape
    | Ltr
    | Rtl
    | Dark
    | Print
    | Forced_colors
    | Noscript
    | Inverted_colors
    | Starting
    | Custom  (** A [\@custom-variant] the theme registers. *)
    | Prose of string  (** [prose-X:], ordered by the element X names. *)
    | Arbitrary  (** [[&>*]:], [not-[...]:] and the other bracket variants. *)
    | Container_query

  (* The cascade order itself. Every other function in this section maps onto a
     slot; this is the only place a position is a number. The gaps leave room to
     insert a slot without renumbering its neighbours. *)
  let rank = function
    | Child -> 100
    | Descendant -> 200
    | Negation -> 300
    | Group -> 500
    | Peer -> 600
    | Pseudo_first_letter -> 1000
    | Pseudo_first_line -> 1000
    | Pseudo_marker -> 1100
    | Pseudo_selection -> 1200
    | Pseudo_file -> 1300
    | Pseudo_placeholder -> 1400
    | Pseudo_backdrop -> 1401
    | Pseudo_details_content -> 1500
    | Pseudo_before -> 1600
    | Pseudo_after -> 1601
    | First -> 10100
    | Last -> 10200
    | Only -> 10300
    | Odd -> 10400
    | Even -> 10500
    | First_of_type -> 10600
    | Last_of_type -> 10700
    | Only_of_type -> 10800
    | Visited -> 10900
    | Target -> 11000
    | Open -> 11100
    | Default -> 11200
    | Checked -> 11300
    | Indeterminate -> 11400
    | Placeholder_shown -> 11500
    | Autofill -> 11600
    | Optional -> 11700
    | Required -> 11800
    | Valid -> 11900
    | Invalid -> 12000
    | User_valid -> 12010
    | User_invalid -> 12020
    | In_range -> 12100
    | Out_of_range -> 12200
    | Read_only -> 12300
    | Read_write -> 12310
    | Empty -> 12400
    | Focus_within -> 12500
    | Hover -> 20000
    | Focus -> 30100
    | Focus_visible -> 30200
    | Active -> 30300
    | Enabled -> 30400
    | Disabled -> 30500
    | Inert -> 30550
    | Ancestor -> 30560
    | Has -> 30600
    | Aria_named -> 30700
    | Aria_arbitrary -> 30790
    | Data_named -> 30800
    | Data_arbitrary -> 30810
    | Nth -> 30900
    | Nth_last -> 30910
    | Nth_of_type -> 30920
    | Nth_last_of_type -> 30930
    | Hocus -> 35000
    | Supports -> 40000
    | Motion_safe -> 50000
    | Motion_reduce -> 50100
    | Contrast_more -> 50200
    | Contrast_less -> 50300
    | Breakpoint -> 60000
    | Container_query -> 65000
    | Portrait -> 70000
    | Landscape -> 70100
    | Ltr -> 80000
    | Rtl -> 80100
    | Dark -> 90000
    | Starting -> 90500
    | Print -> 91000
    | Forced_colors -> 92000
    | Inverted_colors -> 93100
    | Pointer -> 93200
    | Any_pointer -> 93300
    | Noscript -> 93400
    | Custom -> 95500
    | Prose element -> prose_element_variant_order element
    | Arbitrary -> 100000
end

(* The slot a modifier constructor sorts in. Exhaustive on purpose: a new
   variant has to be given a position here rather than falling into a catch-all
   that would drop it in the middle of the table. *)
let rec slot_of_modifier : modifier -> Slot.t = function
  (* Every group-/peer- spelling sorts where its wrapper does, whatever state it
     carries. *)
  | Group_hover | Group_focus | Group_active | Group_visited | Group_disabled
  | Group_checked | Group_empty | Group_required | Group_valid | Group_invalid
  | Group_indeterminate | Group_default | Group_open | Group_target
  | Group_optional | Group_read_only | Group_read_write | Group_inert
  | Group_user_valid | Group_user_invalid | Group_placeholder_shown
  | Group_autofill | Group_in_range | Group_out_of_range | Group_focus_within
  | Group_focus_visible | Group_enabled | Group_first | Group_last | Group_only
  | Group_odd | Group_even | Group_first_of_type | Group_last_of_type
  | Group_only_of_type | Group_hocus | Group_has _ | Group_arbitrary _
  | Group_not _ | Group_data _ | Group_aria _ | Named_group _
  | Not_named_group _ | Group_peer_named _ ->
      Slot.Group
  | Peer_hover | Peer_focus | Peer_checked | Peer_active | Peer_visited
  | Peer_disabled | Peer_empty | Peer_required | Peer_valid | Peer_invalid
  | Peer_indeterminate | Peer_default | Peer_open | Peer_target | Peer_optional
  | Peer_read_only | Peer_read_write | Peer_inert | Peer_user_valid
  | Peer_user_invalid | Peer_placeholder_shown | Peer_autofill | Peer_in_range
  | Peer_out_of_range | Peer_focus_within | Peer_focus_visible | Peer_enabled
  | Peer_first | Peer_last | Peer_only | Peer_odd | Peer_even
  | Peer_first_of_type | Peer_last_of_type | Peer_only_of_type | Peer_hocus
  | Peer_has _ | Peer_arbitrary _ | Peer_not _ | Peer_data _ | Peer_aria _
  | Named_peer _ ->
      Slot.Peer
  | Children -> Slot.Child
  | Descendants -> Slot.Descendant
  | Pseudo_first_letter -> Slot.Pseudo_first_letter
  | Pseudo_first_line -> Slot.Pseudo_first_line
  | Pseudo_marker -> Slot.Pseudo_marker
  | Pseudo_selection -> Slot.Pseudo_selection
  | Pseudo_file -> Slot.Pseudo_file
  | Pseudo_placeholder -> Slot.Pseudo_placeholder
  | Pseudo_backdrop -> Slot.Pseudo_backdrop
  | Pseudo_details_content -> Slot.Pseudo_details_content
  | Pseudo_before -> Slot.Pseudo_before
  | Pseudo_after -> Slot.Pseudo_after
  | First -> Slot.First
  | Last -> Slot.Last
  | Only -> Slot.Only
  | Odd -> Slot.Odd
  | Even -> Slot.Even
  | First_of_type -> Slot.First_of_type
  | Last_of_type -> Slot.Last_of_type
  | Only_of_type -> Slot.Only_of_type
  | Visited -> Slot.Visited
  | Target -> Slot.Target
  | Open -> Slot.Open
  | Default -> Slot.Default
  | Checked -> Slot.Checked
  | Indeterminate -> Slot.Indeterminate
  | Placeholder_shown -> Slot.Placeholder_shown
  | Autofill -> Slot.Autofill
  | Optional -> Slot.Optional
  | Required -> Slot.Required
  | Valid -> Slot.Valid
  | Invalid -> Slot.Invalid
  | User_valid -> Slot.User_valid
  | User_invalid -> Slot.User_invalid
  | In_range -> Slot.In_range
  | Out_of_range -> Slot.Out_of_range
  | Read_only -> Slot.Read_only
  | Read_write -> Slot.Read_write
  | Empty -> Slot.Empty
  | Focus_within -> Slot.Focus_within
  | Hover -> Slot.Hover
  | Focus -> Slot.Focus
  | Focus_visible -> Slot.Focus_visible
  | Active -> Slot.Active
  | Enabled -> Slot.Enabled
  | Disabled -> Slot.Disabled
  | Inert -> Slot.Inert
  | In_bracket _ | In_data _ | In_state _ | In_named_group _ -> Slot.Ancestor
  | Has _ | Has_variant _ | Has_named_group _ -> Slot.Has
  | Aria_checked | Aria_expanded | Aria_selected | Aria_disabled ->
      Slot.Aria_named
  | Aria_bracket _ -> Slot.Aria_arbitrary
  | Data_state _ | Data_variant _ | Data_active | Data_inactive | Data_custom _
    ->
      Slot.Data_named
  | Data_bracket _ -> Slot.Data_arbitrary
  | Nth _ -> Slot.Nth
  | Nth_last _ -> Slot.Nth_last
  | Nth_of_type _ -> Slot.Nth_of_type
  | Nth_last_of_type _ -> Slot.Nth_last_of_type
  | Hocus | Device_hocus -> Slot.Hocus
  | Supports_property _ | Supports_condition _ -> Slot.Supports
  | Motion_safe -> Slot.Motion_safe
  | Motion_reduce -> Slot.Motion_reduce
  | Contrast_more -> Slot.Contrast_more
  | Contrast_less -> Slot.Contrast_less
  | Pointer_none | Pointer_coarse | Pointer_fine -> Slot.Pointer
  | Any_pointer_none | Any_pointer_coarse | Any_pointer_fine -> Slot.Any_pointer
  | Responsive _ | Min_responsive _ | Max_responsive _ | Min_arbitrary _
  | Max_arbitrary _ | Min_arbitrary_length _ | Max_arbitrary_length _
  | Custom_responsive _ | Min_custom _ | Max_custom _ ->
      Slot.Breakpoint
  | Portrait -> Slot.Portrait
  | Landscape -> Slot.Landscape
  | Ltr -> Slot.Ltr
  | Rtl -> Slot.Rtl
  | Dark -> Slot.Dark
  | Print -> Slot.Print
  | Forced_colors -> Slot.Forced_colors
  | Noscript -> Slot.Noscript
  | Inverted_colors -> Slot.Inverted_colors
  | Starting -> Slot.Starting
  | Custom_variant _ -> Slot.Custom
  | Prose_element element -> Slot.Prose element
  | Not_bracket _ | Arbitrary_selector _ | At_rule _ -> Slot.Arbitrary
  | Container _ | Container_style _ -> Slot.Container_query
  (* A negation sorts where the variant it negates sorts. *)
  | Not inner -> slot_of_modifier inner

(* The slot a class-name token sorts in, or [None] when the token names no
   variant this table knows. The token is one modifier of a class name, the part
   between two ":" (["hover"], ["group-has-checked"], ["@min-[64rem]"]). *)
let slot_of_prefix prefix : Slot.t option =
  let starts_with prefix' = String.starts_with ~prefix:prefix' prefix in
  match prefix with
  (* [peer-hover] shares [group-hover]'s position rather than the one every
     other [peer-] spelling takes. *)
  | "*" -> Some Slot.Child
  | "**" -> Some Slot.Descendant
  | "group-hover" | "peer-hover" -> Some Slot.Group
  | "first-letter" -> Some Slot.Pseudo_first_letter
  | "first-line" -> Some Slot.Pseudo_first_line
  | "marker" -> Some Slot.Pseudo_marker
  | "selection" -> Some Slot.Pseudo_selection
  | "file" -> Some Slot.Pseudo_file
  | "placeholder" -> Some Slot.Pseudo_placeholder
  | "backdrop" -> Some Slot.Pseudo_backdrop
  | "details-content" -> Some Slot.Pseudo_details_content
  | "before" -> Some Slot.Pseudo_before
  | "after" -> Some Slot.Pseudo_after
  | "first" -> Some Slot.First
  | "last" -> Some Slot.Last
  | "only" -> Some Slot.Only
  | "odd" -> Some Slot.Odd
  | "even" -> Some Slot.Even
  | "first-of-type" -> Some Slot.First_of_type
  | "last-of-type" -> Some Slot.Last_of_type
  | "only-of-type" -> Some Slot.Only_of_type
  | "visited" -> Some Slot.Visited
  | "target" -> Some Slot.Target
  | "open" -> Some Slot.Open
  | "default" -> Some Slot.Default
  | "checked" -> Some Slot.Checked
  | "indeterminate" -> Some Slot.Indeterminate
  | "placeholder-shown" -> Some Slot.Placeholder_shown
  | "autofill" -> Some Slot.Autofill
  | "optional" -> Some Slot.Optional
  | "required" -> Some Slot.Required
  | "valid" -> Some Slot.Valid
  | "invalid" -> Some Slot.Invalid
  | "user-valid" -> Some Slot.User_valid
  | "user-invalid" -> Some Slot.User_invalid
  | "in-range" -> Some Slot.In_range
  | "out-of-range" -> Some Slot.Out_of_range
  | "read-only" -> Some Slot.Read_only
  | "read-write" -> Some Slot.Read_write
  | "empty" -> Some Slot.Empty
  | "focus-within" -> Some Slot.Focus_within
  | "hover" -> Some Slot.Hover
  | "focus" -> Some Slot.Focus
  | "focus-visible" -> Some Slot.Focus_visible
  | "active" -> Some Slot.Active
  | "enabled" -> Some Slot.Enabled
  | "disabled" -> Some Slot.Disabled
  | "inert" -> Some Slot.Inert
  | "data-custom" | "data-active" | "data-inactive" -> Some Slot.Data_named
  | "hocus" | "device-hocus" -> Some Slot.Hocus
  | "motion-safe" -> Some Slot.Motion_safe
  | "motion-reduce" -> Some Slot.Motion_reduce
  | "contrast-more" -> Some Slot.Contrast_more
  | "contrast-less" -> Some Slot.Contrast_less
  | "portrait" -> Some Slot.Portrait
  | "landscape" -> Some Slot.Landscape
  | "ltr" -> Some Slot.Ltr
  | "rtl" -> Some Slot.Rtl
  | "dark" -> Some Slot.Dark
  | "print" -> Some Slot.Print
  | "forced-colors" -> Some Slot.Forced_colors
  | "noscript" -> Some Slot.Noscript
  | "inverted-colors" -> Some Slot.Inverted_colors
  | "starting" -> Some Slot.Starting
  | "sm" | "md" | "lg" | "xl" | "2xl" -> Some Slot.Breakpoint
  | _ when starts_with "group-" -> Some Slot.Group
  | _ when starts_with "peer-" -> Some Slot.Peer
  | _ when starts_with "has-" -> Some Slot.Has
  | _ when starts_with "aria-" ->
      (* [aria-] with nothing after it, and [aria-[expr]], take the arbitrary
         position; the shorthand names take the named one. *)
      if String.length prefix > 5 && prefix.[5] <> '[' then Some Slot.Aria_named
      else Some Slot.Aria_arbitrary
  | _ when starts_with "data-" ->
      if String.length prefix > 5 && prefix.[5] = '[' then
        Some Slot.Data_arbitrary
      else Some Slot.Data_named
  | _ when starts_with "supports" -> Some Slot.Supports
  | _ when starts_with "pointer-" -> Some Slot.Pointer
  | _ when starts_with "any-pointer-" -> Some Slot.Any_pointer
  | _ when starts_with "min-" || starts_with "max-" -> Some Slot.Breakpoint
  (* [nth-3], [nth-last-of-type-2]: the count is the argument, not part of the
     variant name. Longest spelling first, so [nth-last-of-type-] is not read as
     an [nth-last-] with a strange argument. *)
  | _ when starts_with "nth-last-of-type-" -> Some Slot.Nth_last_of_type
  | _ when starts_with "nth-of-type-" -> Some Slot.Nth_of_type
  | _ when starts_with "nth-last-" -> Some Slot.Nth_last
  | _ when starts_with "nth-" -> Some Slot.Nth
  (* [in-focus], [in-data-open], [in-[.foo]]: a state on an ancestor. The
     pseudo-classes that merely start with the same letters ([in-range],
     [inert], [invalid]) are matched by name above. *)
  | _ when starts_with "in-" -> Some Slot.Ancestor
  (* Every [not-X] sorts together, ahead of the variants it can negate;
     [variant_inner_order] puts the X back inside the slot. *)
  | _ when starts_with "not-" -> Some Slot.Negation
  | _ when starts_with "prose-" ->
      Some (Slot.Prose (String.sub prefix 6 (String.length prefix - 6)))
  | _ when prefix <> "" && prefix.[0] = '[' -> Some Slot.Arbitrary
  | _ when prefix <> "" && prefix.[0] = '@' -> Some Slot.Container_query
  | _ -> None

(* The slot a media condition sorts in. A rule can take its position from the
   query it is nested in rather than from its class name: [dark:group-hover]
   writes a [(hover: hover)] query, and sorts where a bare [hover] sorts. *)
let slot_of_media_cond (cond : Css.Media.t) : Slot.t option =
  let open Css.Media in
  match cond with
  | Cond (Feature (Plain (Hover, Ident Hover))) -> Some Slot.Hover
  | Cond (Feature (Plain (Prefers_reduced_motion, Ident No_preference))) ->
      Some Slot.Motion_safe
  | Cond (Feature (Plain (Prefers_reduced_motion, Ident Reduce))) ->
      Some Slot.Motion_reduce
  | Cond (Feature (Plain (Prefers_contrast, Ident More))) ->
      Some Slot.Contrast_more
  | Cond (Feature (Plain (Prefers_contrast, Ident Less))) ->
      Some Slot.Contrast_less
  | Cond (Feature (Plain (Orientation, Ident Portrait))) -> Some Slot.Portrait
  | Cond (Feature (Plain (Orientation, Ident Landscape))) -> Some Slot.Landscape
  | Cond (Feature (Plain (Prefers_color_scheme, (Ident Dark | Ident Light)))) ->
      Some Slot.Dark
  | Type { prefix = None; type_ = Print; trailing = None } -> Some Slot.Print
  | Cond (Feature (Plain (Forced_colors, Ident Active))) ->
      Some Slot.Forced_colors
  | Cond (Feature (Plain (Inverted_colors, Ident Inverted))) ->
      Some Slot.Inverted_colors
  | _ -> None

(* The variant wrapped by one compound token, if any. Keeping this extraction in
   one place lets ordering follow group-not-has-peer-not-data-active all the way
   to its data predicate instead of stopping at the first [not] or [has]
   slot. *)
let variant_inner_token token =
  let after n = String.sub token n (String.length token - n) in
  let anchor_inner n =
    let inner = after n in
    (* A slash inside an arbitrary selector belongs to the selector. Named
       simple states have no brackets, so their suffix can be removed here. *)
    if String.contains inner '[' then inner else fst (split_name inner)
  in
  if String.starts_with ~prefix:"group-" token then Some (anchor_inner 6)
  else if String.starts_with ~prefix:"peer-" token then Some (anchor_inner 5)
  else if String.starts_with ~prefix:"not-" token then Some (after 4)
  else if String.starts_with ~prefix:"has-" token then Some (after 4)
  else
    (* [in-focus] names a state on an ancestor; [in-range] names one on the
       element itself, and the table matches that by name. *)
    match slot_of_prefix token with
    | Some Slot.Ancestor -> Some (after 3)
    | Some _ | None -> None

let variant_order_of_prefix ?theme prefix =
  match theme with
  | Some theme when Option.is_some (try_custom_variant theme prefix) ->
      (* Tailwind registers [dark] before it reads [@custom-variant]. Replacing
         that exact registration changes its body but retains its slot. *)
      if String.equal prefix "dark" then Slot.rank Slot.Dark
      else Slot.rank Slot.Custom
  | Some _ | None -> (
      match slot_of_prefix prefix with Some slot -> Slot.rank slot | None -> 0)

let rec variant_inner_order_path ?theme token =
  match variant_inner_token token with
  | None -> []
  | Some inner ->
      let order = variant_order_of_prefix ?theme inner in
      if order = 0 then [] else order :: variant_inner_order_path ?theme inner

(* The first wrapped slot retained for callers that only need the immediate
   compound variant. *)
let variant_inner_order token =
  match variant_inner_order_path token with order :: _ -> order | [] -> 0

let not_variant_order m = Slot.rank (slot_of_modifier m)

let variant_order_of_media_cond cond =
  match slot_of_media_cond cond with Some slot -> Slot.rank slot | None -> 0
