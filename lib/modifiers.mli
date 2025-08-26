(** Tailwind variant helpers (hover, focus, dark, responsive, group/peer, etc.)
*)

open Core

(** {1 State Variants} *)

val hover : t list -> t
val focus : t list -> t
val active : t list -> t
val disabled : t list -> t
val focus_within : t list -> t
val focus_visible : t list -> t

(** {1 Group/Peer Variants} *)

val group : t
val peer : t
val group_hover : t list -> t
val group_focus : t list -> t
val peer_hover : t list -> t
val peer_focus : t list -> t
val has : string -> t list -> t
val group_has : string -> t list -> t
val peer_has : string -> t list -> t

(** {1 Theme/Motion/Contrast Variants} *)

val dark : t list -> t
val motion_safe : t list -> t
val motion_reduce : t list -> t
val contrast_more : t list -> t
val contrast_less : t list -> t
val starting : t list -> t

(** {1 Pseudo-element Variants} *)

val before : t list -> t
val after : t list -> t

(** {1 Responsive Variants} *)

val sm : t list -> t
val md : t list -> t
val lg : t list -> t
val xl : t list -> t
val xxl : t list -> t

(** {1 ARIA/Peer/Data Variants} *)

val peer_checked : t list -> t
val aria_checked : t list -> t
val aria_expanded : t list -> t
val aria_selected : t list -> t
val aria_disabled : t list -> t
val data_state : string -> t -> t
val data_variant : string -> t -> t
val data_custom : string -> string -> t -> t
val data_active : t list -> t
val data_inactive : t list -> t

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
