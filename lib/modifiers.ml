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

(** Parse an nth expression that may contain an "of <selector>" clause. Bracket
    content uses underscores for spaces, e.g. "2n+1_of_.foo" → nth = 2n+1, of_ =
    Some [Class "foo"]. *)
let parse_nth_selector expr =
  let spaced = String.map (fun c -> if c = '_' then ' ' else c) expr in
  let reader = Css.Reader.of_string spaced in
  Css.Selector.read_nth_selector reader

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

(** Render a CSS length in compact form (no spaces in calc operators) for class
    names. *)
let format_float f =
  if Float.is_integer f then Int.to_string (Float.to_int f)
  else Float.to_string f

let rec compact_length (l : Css.length) =
  match l with
  | Px f -> format_float f ^ "px"
  | Em f -> format_float f ^ "em"
  | Rem f -> format_float f ^ "rem"
  | Vh f -> format_float f ^ "vh"
  | Vw f -> format_float f ^ "vw"
  | Cm f -> format_float f ^ "cm"
  | Mm f -> format_float f ^ "mm"
  | In f -> format_float f ^ "in"
  | Pt f -> format_float f ^ "pt"
  | Calc c -> "calc(" ^ compact_calc c ^ ")"
  | _ -> Css.Pp.to_string (Css.pp_length ~always:true) l

and compact_calc : Css.length Css.calc -> string = function
  | Val l -> compact_length l
  | Num n -> format_float n
  | Expr (left, Add, right) -> compact_calc left ^ "+" ^ compact_calc right
  | Expr (left, Sub, right) -> compact_calc left ^ "-" ^ compact_calc right
  | Expr (left, Mul, right) -> compact_calc left ^ "*" ^ compact_calc right
  | Expr (left, Div, right) -> compact_calc left ^ "/" ^ compact_calc right
  | Var v -> "var(--" ^ Css.var_name v ^ ")"
  | Nested inner -> "calc(" ^ compact_calc inner ^ ")"
  | Parens inner -> "(" ^ compact_calc inner ^ ")"

(** Build class selector for an arbitrary length breakpoint *)
let arbitrary_length_class prefix (l : Css.length) cls =
  let len_str = compact_length l in
  Css.Selector.Class (prefix ^ "[" ^ len_str ^ "]:" ^ cls)

(** Registry for custom breakpoint names (set via scheme) *)
let custom_breakpoints : (string * float) list ref = ref []

let register_custom_breakpoints bps = custom_breakpoints := bps
let clear_custom_breakpoints () = custom_breakpoints := []

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

(** Parse arbitrary bracket content into a selector tree. In bracket content:
    [_] = space, [&] = anchor (group/peer). E.g. "&_p" with group anchor ->
    ":where(.group) p" E.g. "&:hover" with group anchor ->
    ":where(.group):hover" *)
let parse_arbitrary_selector_content content anchor =
  let open Css.Selector in
  (* Replace _ with space for Tailwind convention *)
  let s = String.map (fun c -> if c = '_' then ' ' else c) content in
  (* Split on & to find the anchor positions *)
  let parts = String.split_on_char '&' s in
  match parts with
  | [ ""; rest ] ->
      (* Content starts with & — most common case *)
      let rest = String.trim rest in
      if rest = "" then
        (* Just "&" → :where(.group/peer) descendant *)
        combine (where [ anchor ]) Descendant universal
      else if rest.[0] = ':' then
        (* "&:hover" → :where(.group):hover descendant *)
        let pseudo_str = String.sub rest 1 (String.length rest - 1) in
        let pseudo_sel =
          match pseudo_str with
          | "hover" -> Hover
          | "focus" -> Focus
          | "active" -> Active
          | "focus-within" -> Focus_within
          | "focus-visible" -> Focus_visible
          | "checked" -> Checked
          | "disabled" -> Disabled
          | "first-child" -> First_child
          | "last-child" -> Last_child
          | _ -> Class (":" ^ pseudo_str)
        in
        combine (compound [ where [ anchor ]; pseudo_sel ]) Descendant universal
      else
        (* "& p" → :where(.group) p * — content after & is a descendant
           element *)
        let trimmed = String.trim rest in
        let element_sel = Element (None, trimmed) in
        combine
          (combine (where [ anchor ]) Descendant element_sel)
          Descendant universal
  | _ ->
      (* Fallback — just use the anchor as descendant *)
      combine (where [ anchor ]) Descendant universal

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
  | Nth expr ->
      let nth, of_ = parse_nth_selector expr in
      let prefix = Style.pp_nth "nth" expr in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_child (nth, of_) ]
  | Nth_last expr ->
      let nth, of_ = parse_nth_selector expr in
      let prefix = Style.pp_nth "nth-last" expr in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_last_child (nth, of_) ]
  | Nth_of_type expr ->
      let nth, of_ = parse_nth_selector expr in
      let prefix = Style.pp_nth "nth-of-type" expr in
      Css.Selector.compound
        [ Css.Selector.Class (prefix ^ ":" ^ cls); Nth_of_type (nth, of_) ]
  | Nth_last_of_type expr ->
      let nth, of_ = parse_nth_selector expr in
      let prefix = Style.pp_nth "nth-last-of-type" expr in
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
  | Pseudo_before -> Css.Selector.compound [ before cls; Css.Selector.Before ]
  | Pseudo_after -> Css.Selector.compound [ after cls; Css.Selector.After ]
  | Pseudo_marker -> cp "marker" cls Css.Selector.Marker
  | Pseudo_selection -> cp "selection" cls Css.Selector.Selection
  | Pseudo_placeholder -> cp "placeholder" cls Css.Selector.Placeholder
  | Pseudo_backdrop -> cp "backdrop" cls Css.Selector.Backdrop
  | Pseudo_file -> cp "file" cls Css.Selector.File_selector_button
  | Pseudo_first_letter -> cp "first-letter" cls Css.Selector.First_letter
  | Pseudo_first_line -> cp "first-line" cls Css.Selector.First_line
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
let prose_element_inner_selector name =
  let open Css.Selector in
  let elements = prose_element_selectors name in
  let not_prose_class = attribute "class" (Whitespace_list "not-prose") in
  let not_prose_descendant = combine not_prose_class Descendant universal in
  let not_prose_where = where [ not_prose_class; not_prose_descendant ] in
  let not_sel = not [ not_prose_where ] in
  compound [ where elements; not_sel ]

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
  (* Prose element variants — outer selector is just the class *)
  | Prose_element name -> Class ("prose-" ^ name ^ ":" ^ cls)
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
let rec pp_modifier = function
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
  | Data_custom (key, "") -> "data-" ^ key
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
  | Not m -> "not-" ^ pp_modifier m
  | Has selector -> "has-[" ^ selector ^ "]"
  | Group_has (selector, None) -> "group-has-[" ^ selector ^ "]"
  | Group_has (selector, Some name) -> "group-has-[" ^ selector ^ "]/" ^ name
  | Peer_has (selector, None) -> "peer-has-[" ^ selector ^ "]"
  | Peer_has (selector, Some name) -> "peer-has-[" ^ selector ^ "]/" ^ name
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
  | Nth expr -> Style.pp_nth "nth" expr
  | Nth_last expr -> Style.pp_nth "nth-last" expr
  | Nth_of_type expr -> Style.pp_nth "nth-of-type" expr
  | Nth_last_of_type expr -> Style.pp_nth "nth-last-of-type" expr
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
  | Custom_responsive name -> name
  | Min_custom name -> "min-" ^ name
  | Max_custom name -> "max-" ^ name
  | Group_hocus -> "group-hocus"
  | Peer_hocus -> "peer-hocus"
  | Group_arbitrary sel -> "group-[" ^ sel ^ "]"
  | Peer_arbitrary sel -> "peer-[" ^ sel ^ "]"
  | Min_arbitrary_length l -> "min-[" ^ compact_length l ^ "]"
  | Max_arbitrary_length l -> "max-[" ^ compact_length l ^ "]"
  | Hocus -> "hocus"
  | Device_hocus -> "device-hocus"
  | Not_bracket content -> "[" ^ content ^ "]"
  | Group_not (inner, None) -> "group-not-" ^ pp_modifier inner
  | Group_not (inner, Some name) ->
      "group-not-" ^ pp_modifier inner ^ "/" ^ name
  | Peer_not (inner, None) -> "peer-not-" ^ pp_modifier inner
  | Peer_not (inner, Some name) -> "peer-not-" ^ pp_modifier inner ^ "/" ^ name
  | In_bracket content -> "in-[" ^ content ^ "]"
  | In_data attr -> "in-data-" ^ attr
  | Data_bracket expr -> "data-[" ^ expr ^ "]"
  | Group_data (expr, None) -> "group-data-[" ^ expr ^ "]"
  | Group_data (expr, Some name) -> "group-data-[" ^ expr ^ "]/" ^ name
  | Peer_data (expr, None) -> "peer-data-[" ^ expr ^ "]"
  | Peer_data (expr, Some name) -> "peer-data-[" ^ expr ^ "]/" ^ name
  | Aria_bracket expr -> "aria-[" ^ expr ^ "]"
  | Group_aria (expr, None) -> "group-aria-" ^ expr
  | Group_aria (expr, Some name) -> "group-aria-" ^ expr ^ "/" ^ name
  | Peer_aria (expr, None) -> "peer-aria-" ^ expr
  | Peer_aria (expr, Some name) -> "peer-aria-" ^ expr ^ "/" ^ name
  | Not_named_group (inner, name) ->
      "not-group-" ^ pp_modifier inner ^ "/" ^ name
  | Has_named_group (inner, name) ->
      "has-group-" ^ pp_modifier inner ^ "/" ^ name
  | In_named_group (inner, name) -> "in-group-" ^ pp_modifier inner ^ "/" ^ name
  | Group_peer_named (inner, name) ->
      "group-peer-" ^ pp_modifier inner ^ "/" ^ name
  | Arbitrary_selector content -> "[" ^ content ^ "]"
  | Prose_element name -> "prose-" ^ name

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

(* Parse a CSS length from a string like "600px", "40rem", "100vh" *)
let parse_css_length s : Css.length option = Css.parse_length s

(* Parse a pixel value from a string like "600px" or "600" *)
let parse_px_value s =
  let s =
    if String.ends_with ~suffix:"px" s then String.sub s 0 (String.length s - 2)
    else s
  in
  try Some (float_of_string s) with Failure _ -> None

(* Preprocess a has-selector string: & → * and ensure combinator spacing. *)
let preprocess_has_selector s =
  let buf = Buffer.create (String.length s + 4) in
  let len = String.length s in
  for i = 0 to len - 1 do
    match s.[i] with
    | '&' -> Buffer.add_char buf '*'
    | ('+' | '>' | '~') as c ->
        if
          Buffer.length buf > 0
          &&
          let b = Buffer.contents buf in
          b.[String.length b - 1] <> ' '
        then Buffer.add_char buf ' ';
        Buffer.add_char buf c;
        if i + 1 < len && s.[i + 1] <> ' ' then Buffer.add_char buf ' '
    | c -> Buffer.add_char buf c
  done;
  Buffer.contents buf

(* Validate that a has-selector string can be parsed as a CSS selector. Rejects
   invalid selectors like "@media_print" at parse time. *)
let is_valid_has_selector sel =
  try
    let processed = preprocess_has_selector sel in
    let reader = Css.Reader.of_string processed in
    ignore (Css.Selector.read_relative reader);
    true
  with Css.Reader.Parse_error _ | Invalid_argument _ -> false

(* Try parsing a bracketed modifier, returning Some if matched *)
(* Build the list of bracket pattern matchers for a given input string *)
let bracket_named_patterns s =
  let ( let* ) = Option.bind in
  let try_pattern prefix make =
    let* content = extract_bracket_content ~prefix s in
    Some (make content)
  in
  let try_named prefix make =
    let* expr, name = extract_bracket_content_with_name ~prefix s in
    Some (make expr name)
  in
  let try_named_has prefix make =
    let* sel, name = extract_bracket_content_with_name ~prefix s in
    if is_valid_has_selector sel then Some (make sel name) else None
  in
  [
    (fun () -> try_named "group-aria-[" (fun e n -> Group_aria (e, n)));
    (fun () -> try_named "peer-aria-[" (fun e n -> Peer_aria (e, n)));
    (fun () -> try_pattern "aria-[" (fun expr -> Aria_bracket expr));
    (fun () -> try_named "group-data-[" (fun e n -> Group_data (e, n)));
    (fun () -> try_named "peer-data-[" (fun e n -> Peer_data (e, n)));
    (fun () ->
      let* expr = extract_bracket_content ~prefix:"data-[" s in
      if expr = "" then None else Some (Data_bracket expr));
    (fun () -> try_named_has "group-has-[" (fun s n -> Group_has (s, n)));
    (fun () -> try_named_has "peer-has-[" (fun s n -> Peer_has (s, n)));
    (fun () ->
      let* sel = extract_bracket_content ~prefix:"has-[" s in
      if is_valid_has_selector sel then Some (Has sel) else None);
  ]

let bracket_value_patterns s =
  let ( let* ) = Option.bind in
  let try_pattern prefix make =
    let* content = extract_bracket_content ~prefix s in
    Some (make content)
  in
  let try_with prefix parse make =
    let* content = extract_bracket_content ~prefix s in
    let* value = parse content in
    Some (make value)
  in
  [
    (fun () -> try_with "min-[" parse_px_value (fun px -> Min_arbitrary px));
    (fun () -> try_with "max-[" parse_px_value (fun px -> Max_arbitrary px));
    (fun () ->
      try_with "min-[" parse_css_length (fun l -> Min_arbitrary_length l));
    (fun () ->
      try_with "max-[" parse_css_length (fun l -> Max_arbitrary_length l));
    (fun () -> try_pattern "nth-last-of-type-[" (fun e -> Nth_last_of_type e));
    (fun () -> try_pattern "nth-of-type-[" (fun e -> Nth_of_type e));
    (fun () -> try_pattern "nth-last-[" (fun e -> Nth_last e));
    (fun () -> try_pattern "nth-[" (fun e -> Nth e));
    (fun () -> try_pattern "supports-[" (fun c -> Supports c));
    (fun () ->
      try_with "group-["
        (fun sel ->
          if sel <> "" && String.contains sel '&' then Some sel else None)
        (fun sel -> Group_arbitrary sel));
    (fun () ->
      try_with "peer-["
        (fun sel ->
          if sel <> "" && String.contains sel '&' then Some sel else None)
        (fun sel -> Peer_arbitrary sel));
    (fun () ->
      if
        String.length s >= 3
        && s.[0] = '['
        && s.[String.length s - 1] = ']'
        && String.contains s '&'
      then Some (Arbitrary_selector (String.sub s 1 (String.length s - 2)))
      else None);
  ]

let bracket_patterns s = bracket_named_patterns s @ bracket_value_patterns s

(* Try supports-<property> shorthand: supports-grid → Supports "grid:
   var(--tw)" *)
let try_supports_shorthand s =
  if
    String.length s > 9
    && String.sub s 0 9 = "supports-"
    && (not (String.contains s '['))
    && not (String.contains s '/')
  then
    let prop = String.sub s 9 (String.length s - 9) in
    Some (Supports (prop ^ ": var(--tw)"))
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
  match try_prefix "nth-last-of-type-" (fun n -> Nth_last_of_type n) with
  | Some _ as r -> r
  | None -> (
      match try_prefix "nth-of-type-" (fun n -> Nth_of_type n) with
      | Some _ as r -> r
      | None -> (
          match try_prefix "nth-last-" (fun n -> Nth_last n) with
          | Some _ as r -> r
          | None -> try_prefix "nth-" (fun n -> Nth n)))

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
    ("@sm", Container Container_sm);
    ("@md", Container Container_md);
    ("@lg", Container Container_lg);
    ("@xl", Container Container_xl);
    ("@2xl", Container Container_2xl);
  ]

(* Try looking up a custom breakpoint (e.g., "10xl", "min-10xl", "max-10xl") *)
let try_custom_breakpoint s =
  (* Direct name: e.g., "10xl" → Custom_responsive *)
  match List.assoc_opt s !custom_breakpoints with
  | Some _px -> Some (Custom_responsive s)
  | None ->
      (* min-<name>: e.g., "min-10xl" → Min_custom *)
      if String.length s > 4 && String.sub s 0 4 = "min-" then
        let name = String.sub s 4 (String.length s - 4) in
        match List.assoc_opt name !custom_breakpoints with
        | Some _px -> Some (Min_custom name)
        | None -> None
      else if String.length s > 4 && String.sub s 0 4 = "max-" then
        let name = String.sub s 4 (String.length s - 4) in
        match List.assoc_opt name !custom_breakpoints with
        | Some _px -> Some (Max_custom name)
        | None -> None
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
    Some (Not (Supports (prop ^ ": var(--tw)")))
    (* data-X shorthand — attribute presence check *)
  else if String.length inner > 5 && String.sub inner 0 5 = "data-" then
    let attr = String.sub inner 5 (String.length inner - 5) in
    Some (Not (Data_custom (attr, "")))
    (* has-X shorthand — :has(:X) pseudo-class *)
  else if String.length inner > 4 && String.sub inner 0 4 = "has-" then
    let pseudo = String.sub inner 4 (String.length inner - 4) in
    Some (Not (Has (":" ^ pseudo))) (* nth-X shorthand — :nth-child(X) *)
  else if String.length inner > 4 && String.sub inner 0 4 = "nth-" then
    let expr = String.sub inner 4 (String.length inner - 4) in
    Some (Not (Nth expr))
  else None

(** Check if a modifier is compatible with not-* negation. Pseudo-elements,
    starting style, children/descendants, and container queries cannot be
    negated. *)
let is_not_compatible = function
  | Pseudo_before | Pseudo_after | Pseudo_marker | Pseudo_selection
  | Pseudo_placeholder | Pseudo_backdrop | Pseudo_file | Pseudo_first_letter
  | Pseudo_first_line | Pseudo_details_content | Starting | Children
  | Descendants | Prose_element _ ->
      false
  | _ -> true

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
    else true

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
          | Some m -> Some m
          | None -> None
        in
        Option.map (fun m -> (m, Some name)) inner_mod
    | None -> (
        match List.assoc_opt rest simple_modifiers with
        | Some m -> Some (m, None)
        | None -> None)

(* Valid has-shorthand names. These are stored as-is (without : prefix) so they
   remain distinct from bracket forms like has-[:checked]. *)
let is_has_shorthand = function "checked" | "hocus" -> true | _ -> false

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

(* Map of simple state names to base modifiers for compound variant parsing *)
let group_state_modifiers =
  [
    ("hover", Hover);
    ("focus", Focus);
    ("active", Active);
    ("visited", Visited);
    ("disabled", Disabled);
    ("checked", Checked);
    ("empty", Empty);
    ("required", Required);
    ("valid", Valid);
    ("invalid", Invalid);
    ("indeterminate", Indeterminate);
    ("default", Default);
    ("open", Open);
    ("target", Target);
    ("optional", Optional);
    ("read-only", Read_only);
    ("read-write", Read_write);
    ("inert", Inert);
    ("user-valid", User_valid);
    ("user-invalid", User_invalid);
    ("placeholder-shown", Placeholder_shown);
    ("autofill", Autofill);
    ("in-range", In_range);
    ("out-of-range", Out_of_range);
    ("focus-within", Focus_within);
    ("focus-visible", Focus_visible);
    ("enabled", Enabled);
    ("first", First);
    ("last", Last);
    ("only", Only);
    ("odd", Odd);
    ("even", Even);
    ("first-of-type", First_of_type);
    ("last-of-type", Last_of_type);
    ("only-of-type", Only_of_type);
  ]

(* Try to parse compound named group variants: not-group-STATE/name,
   has-group-STATE/name, in-group-STATE/name, group-peer-STATE/name *)
let try_compound_named_group s =
  let try_match prefix make =
    let plen = String.length prefix in
    if String.length s > plen && String.sub s 0 plen = prefix then
      let rest = String.sub s plen (String.length s - plen) in
      let base, name_opt = split_name rest in
      match name_opt with
      | Some name -> (
          match List.assoc_opt base group_state_modifiers with
          | Some m -> Some (make m name)
          | None -> None)
      | None -> None
    else None
  in
  match try_match "not-group-" (fun m n -> Not_named_group (m, n)) with
  | Some _ as r -> r
  | None -> (
      match try_match "has-group-" (fun m n -> Has_named_group (m, n)) with
      | Some _ as r -> r
      | None -> (
          match try_match "in-group-" (fun m n -> In_named_group (m, n)) with
          | Some _ as r -> r
          | None -> try_match "group-peer-" (fun m n -> Group_peer_named (m, n))
          ))

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
    else None

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
let try_not_modifier s =
  if not (String.length s > 4 && String.sub s 0 4 = "not-") then None
  else
    let inner = String.sub s 4 (String.length s - 4) in
    match List.assoc_opt inner simple_modifiers with
    | Some m when is_not_compatible m -> Some (Not m)
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
    && not (String.contains s '[')
  then Some (Aria_bracket (String.sub s 5 (String.length s - 5)))
  else None

(* Ordering of prose element variants (matches Tailwind v4 typography plugin) *)
let prose_element_variant_order = function
  | "headings" -> 96001
  | "h1" -> 96002
  | "h2" -> 96003
  | "h3" -> 96004
  | "h4" -> 96005
  | "p" -> 96006
  | "a" -> 96007
  | "blockquote" -> 96008
  | "figure" -> 96009
  | "figcaption" -> 96010
  | "strong" -> 96011
  | "em" -> 96012
  | "kbd" -> 96013
  | "code" -> 96014
  | "pre" -> 96015
  | "ol" -> 96016
  | "ul" -> 96017
  | "li" -> 96018
  | "thead" -> 96019
  | "th" -> 96020
  | "td" -> 96021
  | "img" -> 96022
  | "video" -> 96023
  | "hr" -> 96024
  | "lead" -> 96025
  | _ -> 96000

(* Known prose element variant names (matches Tailwind v4 typography plugin) *)
let is_prose_element_name = function
  | "headings" | "p" | "a" | "strong" | "em" | "code" | "pre" | "ol" | "ul"
  | "li" | "blockquote" | "h1" | "h2" | "h3" | "h4" | "img" | "video" | "figure"
  | "figcaption" | "hr" | "th" | "td" | "thead" | "kbd" | "lead" ->
      true
  | _ -> false

(* Try parsing prose-* element variant modifier *)
let try_prose_element s =
  if String.length s > 6 && String.sub s 0 6 = "prose-" then
    let name = String.sub s 6 (String.length s - 6) in
    if is_prose_element_name name then Some (Prose_element name) else None
  else None

(* Parse a modifier string into a typed Style.modifier *)
let parse_modifier s : modifier option =
  let fns =
    [
      (fun () -> List.assoc_opt s simple_modifiers);
      (fun () -> try_bracketed_modifier s);
      (fun () -> try_aria_shorthand s);
      (fun () -> try_has_shorthand s);
      (fun () -> try_numeric_nth s);
      (fun () -> try_compound_named_group s);
      (fun () -> try_in_modifier s);
      (fun () -> try_not_in_modifier s);
      (fun () -> try_group_peer_not s);
      (fun () -> try_not_modifier s);
      (fun () -> try_bare_data_aria s);
      (fun () -> try_prose_element s);
      (fun () -> try_custom_breakpoint s);
    ]
  in
  List.find_map (fun f -> f ()) fns

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

(** {1 Variant Ordering}

    These functions define the canonical Tailwind v4 cascade order for
    modifiers. Ordering lives here — in the module that owns modifier semantics
    — rather than in the assembly pipeline. *)

(** [not_variant_order m] returns the sort key for a [not-*] inner modifier.
    Used when building [:not()] selectors to preserve cascade ordering. *)
let not_variant_order = function
  (* Block 1: structural pseudo-classes *)
  | First -> 100
  | Last -> 200
  | Only -> 300
  | Odd -> 400
  | Even -> 500
  | First_of_type -> 600
  | Last_of_type -> 700
  | Only_of_type -> 800
  (* Block 1: state pseudo-classes *)
  | Visited -> 900
  | Target -> 1000
  | Open -> 1100
  | Default -> 1200
  | Checked -> 1300
  | Indeterminate -> 1400
  | Placeholder_shown -> 1500
  | Autofill -> 1600
  | Optional -> 1700
  | Required -> 1800
  | Valid -> 1900
  | Invalid -> 2000
  | In_range -> 2100
  | Out_of_range -> 2200
  | Read_only -> 2300
  (* Block 1: misc before hover *)
  | Empty -> 2400
  | Focus_within -> 2500
  | Hover -> 2600
  (* Block 2: interactive pseudo-classes (after hover media) *)
  | Focus -> 2700
  | Focus_visible -> 2800
  | Active -> 2900
  | Enabled -> 3000
  | Disabled -> 3100
  | Inert -> 3200
  (* Block 2: complex selectors *)
  | Has _ -> 3300
  | Aria_selected -> 3340
  | Aria_checked -> 3350
  | Aria_expanded -> 3360
  | Aria_disabled -> 3370
  | Aria_bracket _ -> 3380
  | Group_aria _ -> 3385
  | Peer_aria _ -> 3390
  | Data_custom _ -> 3400
  | Data_bracket _ -> 3401
  | Group_data _ -> 3402
  | Peer_data _ -> 3403
  | Data_state _ -> 3410
  | Data_variant _ -> 3420
  | Data_active -> 3430
  | Data_inactive -> 3440
  | Nth _ -> 3500
  | Nth_last _ -> 3550
  | Nth_of_type _ -> 3600
  | Nth_last_of_type _ -> 3650
  (* @supports *)
  | Supports _ -> 4000
  (* Media: accessibility preferences *)
  | Motion_safe -> 5000
  | Motion_reduce -> 5100
  | Contrast_more -> 5200
  | Contrast_less -> 5300
  (* Media: responsive — max first (descending), then min (ascending) *)
  | Max_responsive _ -> 6000
  | Max_arbitrary _ -> 6100
  | Max_arbitrary_length _ -> 6150
  | Min_arbitrary _ -> 6200
  | Min_arbitrary_length _ -> 6250
  | Min_responsive _ -> 6300
  | Responsive _ -> 6400
  (* Media: other *)
  | Portrait -> 7000
  | Landscape -> 7100
  (* Selector: directionality *)
  | Ltr -> 8000
  | Rtl -> 8100
  (* Media: appearance *)
  | Dark -> 9000
  | Print -> 9100
  | Forced_colors -> 9200
  | Noscript -> 9300
  (* Custom: hocus/device-hocus *)
  | Hocus -> 10000
  | Device_hocus -> 10100
  (* Bracket patterns *)
  | Not_bracket _ -> 11000
  (* Group/peer-not *)
  | Group_not _ -> 12000
  | Peer_not _ -> 12100
  (* Fallback *)
  | _ -> 5500

(** [variant_order_of_prefix prefix] returns the position of a modifier prefix
    string in the Tailwind v4 variant cascade. The prefix is the part before the
    last ":" in a class name (e.g., "hover" in "hover:bg-blue-500", or
    "dark:group-hover" in "dark:group-hover:text-white"). Returns 0 for unknown
    or non-variant prefixes. *)
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
  | "user-valid" -> 12010
  | "user-invalid" -> 12020
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
      if String.starts_with ~prefix:"group-" prefix then 500
      else if String.starts_with ~prefix:"peer-" prefix then 600
      else if String.starts_with ~prefix:"has-" prefix then 30600
      else if String.starts_with ~prefix:"aria-" prefix then
        if String.length prefix > 5 && prefix.[5] <> '[' then 30700 else 30790
      else if String.starts_with ~prefix:"data-" prefix then
        if String.length prefix > 5 && prefix.[5] = '[' then 30810 else 30800
      else if
        String.starts_with ~prefix:"supports-" prefix
        || String.starts_with ~prefix:"supports" prefix
      then 40000
      else if prefix = "motion-safe" then 50000
      else if prefix = "motion-reduce" then 50100
      else if prefix = "contrast-more" then 50200
      else if prefix = "contrast-less" then 50300
      else if String.starts_with ~prefix:"pointer-" prefix then 50400
      else if String.starts_with ~prefix:"any-pointer-" prefix then 50500
      else if
        prefix = "sm" || prefix = "md" || prefix = "lg" || prefix = "xl"
        || prefix = "2xl"
        || String.starts_with ~prefix:"min-" prefix
        || String.starts_with ~prefix:"max-" prefix
      then 60000
      else if String.starts_with ~prefix:"prose-" prefix then
        let name = String.sub prefix 6 (String.length prefix - 6) in
        prose_element_variant_order name
      else if String.length prefix > 0 && prefix.[0] = '[' then 100000
      else if String.length prefix > 0 && prefix.[0] = '@' then 110000
      else 0

(* [variant_order_of_media_cond cond] returns the same sort key as
   [variant_order_of_prefix] for the corresponding CSS media condition. Used
   to determine the cascade position of rules with nested media queries (e.g.,
   dark:group-hover has a nested @media(hover:hover), so its effective inner
   order is 20000, matching standalone hover). *)
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
