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
      name : string;
      props : Css.declaration list;
      rules : Css.rule list option;
    }
  | Modified of modifier * t
  | Group of t list

type size = [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full ]
type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

val style :
  ?rules:Css.rule list option ->
  string ->
  Css.declaration list ->
  t
(** [style ?rules name props] defines a utility [name] with CSS [props].
    - [rules]: Optional custom CSS rules (for utilities like prose that generate
      multiple rules with descendant selectors)
    - [name]: The CSS class name
    - [props]: CSS properties to apply *)

val class_name : t -> string
(** [class_name t] returns the base class name(s) for [t]. Note: modifiers
    (e.g., hover:, md:) are ignored; for composed class strings with modifiers,
    use Tw.pp/Tw.to_classes. *)

val pp : t -> string
(** [pp t] returns the full class name(s) for [t] including modifiers. *)
