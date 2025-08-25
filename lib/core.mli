(** Core types and helpers for Tailwind CSS DSL *)

type breakpoint = [ `Sm | `Md | `Lg | `Xl | `Xl_2 ]

type container_query =
  | Container_sm
  | Container_md
  | Container_lg
  | Container_xl
  | Container_2xl
  | Container_named of string * int

type modifier =
  | Hover
  | Focus
  | Active
  | Disabled
  | Group_hover
  | Group_focus
  | Dark
  | Responsive of breakpoint
  | Peer_hover
  | Peer_focus
  | Peer_checked
  | Aria_checked
  | Aria_expanded
  | Aria_selected
  | Aria_disabled
  | Data_state of string
  | Data_variant of string
  | Data_active
  | Data_inactive
  | Data_custom of string * string
  | Container of container_query
  | Not of modifier
  | Has of string
  | Group_has of string
  | Peer_has of string
  | Starting
  | Focus_within
  | Focus_visible
  | Motion_safe
  | Motion_reduce
  | Contrast_more
  | Contrast_less

type var =
  | Color of { name : string; shade : int option; value : string }
    (* e.g., name="blue", shade=Some 500, value="#3b82f6" *)
  | Spacing of { multiplier : int; value : string }
    (* e.g., multiplier=4 for 1rem, value="1rem" *)
  | Radius of { name : string; value : string }
    (* e.g., name="sm", value=".125rem" *)
  | Font of { name : string; value : string }
(* e.g., name="sans", value="ui-sans-serif, ..." *)

val color_var : ?shade:int -> string -> var

val spacing_var : int -> var
(** [spacing_var n] creates a spacing variable for multiplier n (n * 0.25rem) *)

val var_to_css_properties : var -> (string * string) list
(** [var_to_css_properties var] returns the CSS custom property declarations
    needed for this variable. Returns a list of (property-name, value) pairs. *)

type t =
  | Style of { name : string; props : Css.declaration list; vars : var list }
  | Prose of Prose.t
  | Modified of modifier * t
  | Group of t list

type size = [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full ]
type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

val style : string -> Css.declaration list -> t
val style_with_vars : string -> Css.declaration list -> var list -> t

val class_name : t -> string
(** [class_name t] returns the base class name(s) for [t]. Note: modifiers
    (e.g., hover:, md:) are ignored; for composed class strings with modifiers,
    use Tw.pp/Tw.to_classes. *)

val pp : t -> string
(** [pp t] returns the full class name(s) for [t] including modifiers. *)
