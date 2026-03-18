(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

open Style
open Utility

(** {1 Internal Helpers} *)

val compact_length : Css.length -> string
(** [compact_length l] renders a CSS length without spaces (for use in class
    names). *)

val to_selector : modifier -> string -> Css.Selector.t
(** [to_selector modifier base_class] generates the CSS selector for a modifier
    applied to a base class. *)

val prose_element_inner_selector : string -> Css.Selector.t
(** [prose_element_inner_selector name] builds the inner nested selector for a
    prose element variant rule. Creates the "& :is(:where(ELTS):not(not-prose))"
    pattern. *)

val is_hover : modifier -> bool
(** [is_hover m] returns true if the modifier generates a :hover rule. *)

val register_custom_breakpoints : (string * float) list -> unit
(** [register_custom_breakpoints bps] sets the custom breakpoint names and their
    px values for modifier parsing. *)

val clear_custom_breakpoints : unit -> unit
(** [clear_custom_breakpoints ()] clears the custom breakpoint registry. *)

(** {1 State Variants} *)

val hover : t list -> t
(** [hover styles] applies [styles] on :hover. *)

val focus : t list -> t
(** [focus styles] applies [styles] on :focus. *)

val active : t list -> t
(** [active styles] applies [styles] on :active. *)

val disabled : t list -> t
(** [disabled styles] applies [styles] on :disabled. *)

val focus_within : t list -> t
(** [focus_within styles] applies [styles] on :focus-within. *)

val focus_visible : t list -> t
(** [focus_visible styles] applies [styles] on :focus-visible. *)

(** {1 Group/Peer Variants} *)

val group : t
(** [group] marks an element as a group parent. *)

val peer : t
(** [peer] marks an element as a peer for sibling selectors. *)

val group_hover : t list -> t
(** [group_hover styles] applies [styles] when .group:hover matches. *)

val group_focus : t list -> t
(** [group_focus styles] applies [styles] when .group:focus matches. *)

val peer_hover : t list -> t
(** [peer_hover styles] applies [styles] when a preceding .peer:hover matches.
*)

val peer_focus : t list -> t
(** [peer_focus styles] applies [styles] when a preceding .peer:focus matches.
*)

val group_hocus : t list -> t
(** [group_hocus styles] applies [styles] when .group:hover or .group:focus. *)

val peer_hocus : t list -> t
(** [peer_hocus styles] applies [styles] when .peer:hover or .peer:focus. *)

val has : string -> t list -> t
(** [has selector styles] applies [styles] with :has([selector]). *)

val group_has : ?name:string -> string -> t list -> t
(** [group_has ?name selector styles] applies [styles] with
    .group:has([selector]). Optional [name] for named groups. *)

val peer_has : ?name:string -> string -> t list -> t
(** [peer_has ?name selector styles] applies [styles] with
    .peer:has([selector]). Optional [name] for named peers. *)

(** {1 Theme/Motion/Contrast Variants} *)

val dark : t list -> t
(** [dark styles] applies [styles] when dark mode is enabled. *)

val motion_safe : t list -> t
(** [motion_safe styles] applies [styles] when reduced motion is not requested.
*)

val motion_reduce : t list -> t
(** [motion_reduce styles] applies [styles] when reduced motion is requested. *)

val contrast_more : t list -> t
(** [contrast_more styles] applies [styles] when higher contrast is preferred.
*)

val contrast_less : t list -> t
(** [contrast_less styles] applies [styles] when lower contrast is preferred. *)

val starting : t list -> t
(** [starting styles] applies [styles] using [@starting-style]. *)

(** {1 Pseudo-element Variants} *)

val before : t list -> t
(** [before styles] applies [styles] to ::before. *)

val after : t list -> t
(** [after styles] applies [styles] to ::after. *)

(** {1 Responsive Variants} *)

val sm : t list -> t
(** [sm styles] applies [styles] at the small breakpoint. *)

val md : t list -> t
(** [md styles] applies [styles] at the medium breakpoint. *)

val lg : t list -> t
(** [lg styles] applies [styles] at the large breakpoint. *)

val xl : t list -> t
(** [xl styles] applies [styles] at the extra-large breakpoint. *)

val xl2 : t list -> t
(** [xl2 styles] applies [styles] at the 2xl breakpoint. *)

val max_sm : t list -> t
(** [max_sm styles] applies [styles] below the small breakpoint. *)

val max_md : t list -> t
(** [max_md styles] applies [styles] below the medium breakpoint. *)

val max_lg : t list -> t
(** [max_lg styles] applies [styles] below the large breakpoint. *)

val max_xl : t list -> t
(** [max_xl styles] applies [styles] below the extra-large breakpoint. *)

val max_xl2 : t list -> t
(** [max_xl2 styles] applies [styles] below the 2xl breakpoint. *)

val min_arbitrary : float -> t list -> t
(** [min_arbitrary px styles] applies [styles] at min-width:[px]px. *)

val max_arbitrary : float -> t list -> t
(** [max_arbitrary px styles] applies [styles] below [px]px. *)

(** {1 Structural Pseudo-class Variants} *)

val first : t list -> t
(** [first styles] applies [styles] on :first-child. *)

val last : t list -> t
(** [last styles] applies [styles] on :last-child. *)

val only : t list -> t
(** [only styles] applies [styles] on :only-child. *)

val odd : t list -> t
(** [odd styles] applies [styles] on :nth-child(odd). *)

val even : t list -> t
(** [even styles] applies [styles] on :nth-child(even). *)

val first_of_type : t list -> t
(** [first_of_type styles] applies [styles] on :first-of-type. *)

val last_of_type : t list -> t
(** [last_of_type styles] applies [styles] on :last-of-type. *)

val only_of_type : t list -> t
(** [only_of_type styles] applies [styles] on :only-of-type. *)

val nth : string -> t list -> t
(** [nth expr styles] applies [styles] on :nth-child([expr]). *)

val nth_last : string -> t list -> t
(** [nth_last expr styles] applies [styles] on :nth-last-child([expr]). *)

val empty : t list -> t
(** [empty styles] applies [styles] on :empty. *)

(** {1 Form State Variants} *)

val checked : t list -> t
(** [checked styles] applies [styles] on :checked. *)

val indeterminate : t list -> t
(** [indeterminate styles] applies [styles] on :indeterminate. *)

val default : t list -> t
(** [default styles] applies [styles] on :default. *)

val required : t list -> t
(** [required styles] applies [styles] on :required. *)

val valid : t list -> t
(** [valid styles] applies [styles] on :valid. *)

val invalid : t list -> t
(** [invalid styles] applies [styles] on :invalid. *)

val in_range : t list -> t
(** [in_range styles] applies [styles] on :in-range. *)

val out_of_range : t list -> t
(** [out_of_range styles] applies [styles] on :out-of-range. *)

val placeholder_shown : t list -> t
(** [placeholder_shown styles] applies [styles] on :placeholder-shown. *)

val autofill : t list -> t
(** [autofill styles] applies [styles] on :autofill. *)

val read_only : t list -> t
(** [read_only styles] applies [styles] on :read-only. *)

val read_write : t list -> t
(** [read_write styles] applies [styles] on :read-write. *)

val optional : t list -> t
(** [optional styles] applies [styles] on :optional. *)

val open_ : t list -> t
(** [open_ styles] applies [styles] on :is(:popover-open, [open]). *)

val enabled : t list -> t
(** [enabled styles] applies [styles] on :enabled. *)

val target : t list -> t
(** [target styles] applies [styles] on :target. *)

val visited : t list -> t
(** [visited styles] applies [styles] on :visited. *)

val inert : t list -> t
(** [inert styles] applies [styles] on inert elements. *)

val user_valid : t list -> t
(** [user_valid styles] applies [styles] on :user-valid. *)

val user_invalid : t list -> t
(** [user_invalid styles] applies [styles] on :user-invalid. *)

(** {1 ARIA/Peer/Data Variants} *)

val peer_checked : t list -> t
(** [peer_checked styles] applies [styles] when a preceding .peer:checked
    matches. *)

val aria_checked : t list -> t
(** [aria_checked styles] applies [styles] when aria-checked=true. *)

val aria_expanded : t list -> t
(** [aria_expanded styles] applies [styles] when aria-expanded=true. *)

val aria_selected : t list -> t
(** [aria_selected styles] applies [styles] when aria-selected=true. *)

val aria_disabled : t list -> t
(** [aria_disabled styles] applies [styles] when aria-disabled=true. *)

val data_state : string -> t -> t
(** [data_state value style] applies [style] when data-state=[value]. *)

val data_variant : string -> t -> t
(** [data_variant value style] applies [style] when data-variant=[value]. *)

val data_custom : string -> string -> t -> t
(** [data_custom key value style] applies [style] when data-[key]=[value]. *)

val data_active : t list -> t
(** [data_active styles] applies [styles] when data-active is present. *)

val data_inactive : t list -> t
(** [data_inactive styles] applies [styles] when data-inactive is present. *)

(** {1 Group Structural Variants} *)

val group_first : t list -> t
(** [group_first styles] applies [styles] when in a group-first context. *)

val group_last : t list -> t
(** [group_last styles] applies [styles] when in a group-last context. *)

val group_only : t list -> t
(** [group_only styles] applies [styles] when in a group-only context. *)

val group_odd : t list -> t
(** [group_odd styles] applies [styles] when in a group-odd context. *)

val group_even : t list -> t
(** [group_even styles] applies [styles] when in a group-even context. *)

val group_first_of_type : t list -> t
(** [group_first_of_type styles] applies [styles] when in a group-first-of-type
    context. *)

val group_last_of_type : t list -> t
(** [group_last_of_type styles] applies [styles] when in a group-last-of-type
    context. *)

val group_only_of_type : t list -> t
(** [group_only_of_type styles] applies [styles] when in a group-only-of-type
    context. *)

(** {1 Peer Structural Variants} *)

val peer_first : t list -> t
(** [peer_first styles] applies [styles] when a peer is first. *)

val peer_last : t list -> t
(** [peer_last styles] applies [styles] when a peer is last. *)

val peer_only : t list -> t
(** [peer_only styles] applies [styles] when a peer is only. *)

val peer_odd : t list -> t
(** [peer_odd styles] applies [styles] when a peer is odd. *)

val peer_even : t list -> t
(** [peer_even styles] applies [styles] when a peer is even. *)

val peer_first_of_type : t list -> t
(** [peer_first_of_type styles] applies [styles] when a peer is first-of-type.
*)

val peer_last_of_type : t list -> t
(** [peer_last_of_type styles] applies [styles] when a peer is last-of-type. *)

val peer_only_of_type : t list -> t
(** [peer_only_of_type styles] applies [styles] when a peer is only-of-type. *)

(** {1 Group State Variants} *)

val group_active : t list -> t
(** [group_active styles] applies [styles] when the group is active. *)

val group_visited : t list -> t
(** [group_visited styles] applies [styles] when the group is visited. *)

val group_disabled : t list -> t
(** [group_disabled styles] applies [styles] when the group is disabled. *)

val group_checked : t list -> t
(** [group_checked styles] applies [styles] when the group is checked. *)

val group_empty : t list -> t
(** [group_empty styles] applies [styles] when the group is empty. *)

val group_required : t list -> t
(** [group_required styles] applies [styles] when the group is required. *)

val group_valid : t list -> t
(** [group_valid styles] applies [styles] when the group is valid. *)

val group_invalid : t list -> t
(** [group_invalid styles] applies [styles] when the group is invalid. *)

val group_indeterminate : t list -> t
(** [group_indeterminate styles] applies [styles] when the group is
    indeterminate. *)

val group_default : t list -> t
(** [group_default styles] applies [styles] when the group is default. *)

val group_open : t list -> t
(** [group_open styles] applies [styles] when the group is open. *)

val group_target : t list -> t
(** [group_target styles] applies [styles] when the group is the target. *)

val group_optional : t list -> t
(** [group_optional styles] applies [styles] when the group is optional. *)

val group_read_only : t list -> t
(** [group_read_only styles] applies [styles] when the group is read-only. *)

val group_read_write : t list -> t
(** [group_read_write styles] applies [styles] when the group is read-write. *)

val group_inert : t list -> t
(** [group_inert styles] applies [styles] when the group is inert. *)

val group_user_valid : t list -> t
(** [group_user_valid styles] applies [styles] when the group is user-valid. *)

val group_user_invalid : t list -> t
(** [group_user_invalid styles] applies [styles] when the group is user-invalid.
*)

val group_placeholder_shown : t list -> t
(** [group_placeholder_shown styles] applies [styles] when the group placeholder
    is shown. *)

val group_autofill : t list -> t
(** [group_autofill styles] applies [styles] when the group is autofilled. *)

val group_in_range : t list -> t
(** [group_in_range styles] applies [styles] when the group is in range. *)

val group_out_of_range : t list -> t
(** [group_out_of_range styles] applies [styles] when the group is out of range.
*)

val group_focus_within : t list -> t
(** [group_focus_within styles] applies [styles] when focus is within the group.
*)

val group_focus_visible : t list -> t
(** [group_focus_visible styles] applies [styles] when group focus is visible.
*)

val group_enabled : t list -> t
(** [group_enabled styles] applies [styles] when the group is enabled. *)

(** {1 Peer State Variants} *)

val peer_active : t list -> t
(** [peer_active styles] applies [styles] when a peer is active. *)

val peer_visited : t list -> t
(** [peer_visited styles] applies [styles] when a peer is visited. *)

val peer_disabled : t list -> t
(** [peer_disabled styles] applies [styles] when a peer is disabled. *)

val peer_empty : t list -> t
(** [peer_empty styles] applies [styles] when a peer is empty. *)

val peer_required : t list -> t
(** [peer_required styles] applies [styles] when a peer is required. *)

val peer_valid : t list -> t
(** [peer_valid styles] applies [styles] when a peer is valid. *)

val peer_invalid : t list -> t
(** [peer_invalid styles] applies [styles] when a peer is invalid. *)

val peer_indeterminate : t list -> t
(** [peer_indeterminate styles] applies [styles] when a peer is indeterminate.
*)

val peer_default : t list -> t
(** [peer_default styles] applies [styles] when a peer is default. *)

val peer_open : t list -> t
(** [peer_open styles] applies [styles] when a peer is open. *)

val peer_target : t list -> t
(** [peer_target styles] applies [styles] when a peer is the target. *)

val peer_optional : t list -> t
(** [peer_optional styles] applies [styles] when a peer is optional. *)

val peer_read_only : t list -> t
(** [peer_read_only styles] applies [styles] when a peer is read-only. *)

val peer_read_write : t list -> t
(** [peer_read_write styles] applies [styles] when a peer is read-write. *)

val peer_inert : t list -> t
(** [peer_inert styles] applies [styles] when a peer is inert. *)

val peer_user_valid : t list -> t
(** [peer_user_valid styles] applies [styles] when a peer is user-valid. *)

val peer_user_invalid : t list -> t
(** [peer_user_invalid styles] applies [styles] when a peer is user-invalid. *)

val peer_placeholder_shown : t list -> t
(** [peer_placeholder_shown styles] applies [styles] when a peer placeholder is
    shown. *)

val peer_autofill : t list -> t
(** [peer_autofill styles] applies [styles] when a peer is autofilled. *)

val peer_in_range : t list -> t
(** [peer_in_range styles] applies [styles] when a peer is in range. *)

val peer_out_of_range : t list -> t
(** [peer_out_of_range styles] applies [styles] when a peer is out of range. *)

val peer_focus_within : t list -> t
(** [peer_focus_within styles] applies [styles] when focus is within a peer. *)

val peer_focus_visible : t list -> t
(** [peer_focus_visible styles] applies [styles] when peer focus is visible. *)

val peer_enabled : t list -> t
(** [peer_enabled styles] applies [styles] when a peer is enabled. *)

(** {1 Pseudo-element Variants} *)

val marker : t list -> t
(** [marker styles] applies [styles] to the [::marker] pseudo-element. *)

val selection : t list -> t
(** [selection styles] applies [styles] to the [::selection] pseudo-element. *)

val placeholder : t list -> t
(** [placeholder styles] applies [styles] to the [::placeholder] pseudo-element.
*)

val backdrop : t list -> t
(** [backdrop styles] applies [styles] to the [::backdrop] pseudo-element. *)

val file : t list -> t
(** [file styles] applies [styles] to the [::file-selector-button]
    pseudo-element. *)

val first_letter : t list -> t
(** [first_letter styles] applies [styles] to the [::first-letter]
    pseudo-element. *)

val first_line : t list -> t
(** [first_line styles] applies [styles] to the [::first-line] pseudo-element.
*)

val details_content : t list -> t
(** [details_content styles] applies [styles] to the [::details-content]
    pseudo-element. *)

val children : t list -> t
(** [children styles] applies [styles] to direct children. *)

val descendants : t list -> t
(** [descendants styles] applies [styles] to all descendants. *)

(** {1 Directionality Variants} *)

val ltr : t list -> t
(** [ltr styles] applies [styles] in left-to-right direction. *)

val rtl : t list -> t
(** [rtl styles] applies [styles] in right-to-left direction. *)

(** {1 Media Variants} *)

val print : t list -> t
(** [print styles] applies [styles] in print media. *)

val portrait : t list -> t
(** [portrait styles] applies [styles] in portrait orientation. *)

val landscape : t list -> t
(** [landscape styles] applies [styles] in landscape orientation. *)

val forced_colors : t list -> t
(** [forced_colors styles] applies [styles] when forced colors are active. *)

val supports : string -> t list -> t
(** [supports condition styles] applies [styles] when the CSS condition is
    supported. *)

(** {1 Prose Element Variants} *)

val prose_headings : t list -> t
(** [prose_headings styles] applies [styles] to h1-h6, th within prose. *)

val prose_p : t list -> t
(** [prose_p styles] applies [styles] to p elements within prose. *)

val prose_a : t list -> t
(** [prose_a styles] applies [styles] to a elements within prose. *)

val prose_strong : t list -> t
(** [prose_strong styles] applies [styles] to strong elements within prose. *)

val prose_em : t list -> t
(** [prose_em styles] applies [styles] to em elements within prose. *)

val prose_code : t list -> t
(** [prose_code styles] applies [styles] to code elements within prose. *)

val prose_pre : t list -> t
(** [prose_pre styles] applies [styles] to pre elements within prose. *)

val prose_ol : t list -> t
(** [prose_ol styles] applies [styles] to ol elements within prose. *)

val prose_ul : t list -> t
(** [prose_ul styles] applies [styles] to ul elements within prose. *)

val prose_li : t list -> t
(** [prose_li styles] applies [styles] to li elements within prose. *)

val prose_blockquote : t list -> t
(** [prose_blockquote styles] applies [styles] to blockquote elements within
    prose. *)

val prose_h1 : t list -> t
(** [prose_h1 styles] applies [styles] to h1 elements within prose. *)

val prose_h2 : t list -> t
(** [prose_h2 styles] applies [styles] to h2 elements within prose. *)

val prose_h3 : t list -> t
(** [prose_h3 styles] applies [styles] to h3 elements within prose. *)

val prose_h4 : t list -> t
(** [prose_h4 styles] applies [styles] to h4 elements within prose. *)

val prose_img : t list -> t
(** [prose_img styles] applies [styles] to img elements within prose. *)

val prose_video : t list -> t
(** [prose_video styles] applies [styles] to video elements within prose. *)

val prose_figure : t list -> t
(** [prose_figure styles] applies [styles] to figure elements within prose. *)

val prose_figcaption : t list -> t
(** [prose_figcaption styles] applies [styles] to figcaption elements within
    prose. *)

val prose_hr : t list -> t
(** [prose_hr styles] applies [styles] to hr elements within prose. *)

val prose_th : t list -> t
(** [prose_th styles] applies [styles] to th elements within prose. *)

val prose_td : t list -> t
(** [prose_td styles] applies [styles] to td elements within prose. *)

val prose_thead : t list -> t
(** [prose_thead styles] applies [styles] to thead elements within prose. *)

val prose_kbd : t list -> t
(** [prose_kbd styles] applies [styles] to kbd elements within prose. *)

val prose_lead : t list -> t
(** [prose_lead styles] applies [styles] to lead elements within prose. *)

(** {1 Validation} *)

val has_responsive_modifier : t -> bool
(** [has_responsive_modifier style] checks if a style already has a responsive
    modifier. *)

val validate_no_nested_responsive : t list -> unit
(** [validate_no_nested_responsive styles] ensures no responsive modifiers are
    nested. Raises Failure if validation fails. *)

(** {1 Parsing and Pretty-printing} *)

val of_string : string -> string list * string
(** [of_string class_str] parses a class string with modifiers like
    "hover:bg-blue-500" and returns [(modifiers, base_class)]. *)

val pp_modifier : modifier -> string
(** [pp_modifier m] returns the string prefix for a modifier (e.g., "hover" for
    Hover). *)

val apply : string list -> t -> t option
(** [apply modifiers style] applies a list of modifier strings to a base style.
    Returns [None] if any modifier is unrecognized. Example:
    [apply ["hover"; "sm"] (bg blue 500)] creates a hover:sm:bg-blue-500 style.
*)

(** {1 ARIA Helpers} *)

val is_aria_shorthand : string -> bool
(** [is_aria_shorthand name] returns [true] if [name] is a known ARIA shorthand
    attribute (e.g., "checked", "expanded", "hidden") that maps to
    [aria-X=true]. *)

(** {1 Variant Ordering} *)

val not_variant_order : modifier -> int
(** [not_variant_order m] returns the cascade sort key for a [not-*] inner
    modifier. Matches the ordering of [variant_order_of_prefix] for the
    corresponding pseudo-class/media prefix. *)

val variant_order_of_prefix : string -> int
(** [variant_order_of_prefix prefix] returns the position of a modifier prefix
    string in the Tailwind v4 variant cascade. The prefix is the part before the
    last ":" in a base class name (e.g. ["hover"], ["dark:group-hover"]).
    Returns 0 for unknown prefixes. *)

val variant_order_of_media_cond : Css.Media.t -> int
(** [variant_order_of_media_cond cond] returns the same sort key as
    [variant_order_of_prefix] for the corresponding CSS media condition. Used to
    derive the cascade position of rules whose ordering comes from a nested
    media query rather than from the class-name prefix alone. *)
