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
