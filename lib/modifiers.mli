(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

open Style
open Utility

(** {1 Internal Helpers} *)

val to_selector : modifier -> string -> Css.Selector.t
(** [to_selector modifier base_class] generates the CSS selector for a modifier
    applied to a base class. *)

val is_hover : modifier -> bool
(** [is_hover m] returns true if the modifier generates a :hover rule. *)

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

val has : string -> t list -> t
(** [has selector styles] applies [styles] with :has([selector]). *)

val group_has : string -> t list -> t
(** [group_has selector styles] applies [styles] with .group:has([selector]). *)

val peer_has : string -> t list -> t
(** [peer_has selector styles] applies [styles] with .peer:has([selector]). *)

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
val group_last : t list -> t
val group_only : t list -> t
val group_odd : t list -> t
val group_even : t list -> t
val group_first_of_type : t list -> t
val group_last_of_type : t list -> t
val group_only_of_type : t list -> t

(** {1 Peer Structural Variants} *)

val peer_first : t list -> t
val peer_last : t list -> t
val peer_only : t list -> t
val peer_odd : t list -> t
val peer_even : t list -> t
val peer_first_of_type : t list -> t
val peer_last_of_type : t list -> t
val peer_only_of_type : t list -> t

(** {1 Group State Variants} *)

val group_active : t list -> t
val group_visited : t list -> t
val group_disabled : t list -> t
val group_checked : t list -> t
val group_empty : t list -> t
val group_required : t list -> t
val group_valid : t list -> t
val group_invalid : t list -> t
val group_indeterminate : t list -> t
val group_default : t list -> t
val group_open : t list -> t
val group_target : t list -> t
val group_optional : t list -> t
val group_read_only : t list -> t
val group_read_write : t list -> t
val group_inert : t list -> t
val group_user_valid : t list -> t
val group_user_invalid : t list -> t
val group_placeholder_shown : t list -> t
val group_autofill : t list -> t
val group_in_range : t list -> t
val group_out_of_range : t list -> t
val group_focus_within : t list -> t
val group_focus_visible : t list -> t
val group_enabled : t list -> t

(** {1 Peer State Variants} *)

val peer_active : t list -> t
val peer_visited : t list -> t
val peer_disabled : t list -> t
val peer_empty : t list -> t
val peer_required : t list -> t
val peer_valid : t list -> t
val peer_invalid : t list -> t
val peer_indeterminate : t list -> t
val peer_default : t list -> t
val peer_open : t list -> t
val peer_target : t list -> t
val peer_optional : t list -> t
val peer_read_only : t list -> t
val peer_read_write : t list -> t
val peer_inert : t list -> t
val peer_user_valid : t list -> t
val peer_user_invalid : t list -> t
val peer_placeholder_shown : t list -> t
val peer_autofill : t list -> t
val peer_in_range : t list -> t
val peer_out_of_range : t list -> t
val peer_focus_within : t list -> t
val peer_focus_visible : t list -> t
val peer_enabled : t list -> t

(** {1 Pseudo-element Variants} *)

val marker : t list -> t
val selection : t list -> t
val placeholder : t list -> t
val backdrop : t list -> t
val file : t list -> t
val first_letter : t list -> t
val first_line : t list -> t
val details_content : t list -> t
val children : t list -> t
val descendants : t list -> t

(** {1 Directionality Variants} *)

val ltr : t list -> t
val rtl : t list -> t

(** {1 Media Variants} *)

val print : t list -> t
val portrait : t list -> t
val landscape : t list -> t
val forced_colors : t list -> t
val supports : string -> t list -> t

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

val apply : string list -> t -> t
(** [apply modifiers style] applies a list of modifier strings to a base style.
    Example: [apply ["hover"; "sm"] (bg blue 500)] creates a
    hover:sm:bg-blue-500 style. *)
