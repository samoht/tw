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
  | Pseudo_before
  | Pseudo_after

type t =
  | Style of {
      props : Css.declaration list;
      rules : Css.statement list option;
      property_rules : Css.t;
    }
  | Modified of modifier * t
  | Group of t list

val pp : Format.formatter -> t -> unit
(** [pp ppf t] pretty-prints a style. *)

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

val pp_modifier : modifier -> string
(** [pp_modifier m] converts a modifier to its string representation. *)

val style :
  ?rules:Css.statement list option ->
  ?property_rules:Css.t ->
  Css.declaration list ->
  t
(** [style ?rules ?property_rules props] defines a utility with CSS [props].
    - [rules]: Optional custom CSS rules (for utilities like prose that generate
      multiple rules with descendant selectors).
    - [property_rules]: Optional CSS property rules needed by this utility.
    - [props]: CSS properties to apply. *)
