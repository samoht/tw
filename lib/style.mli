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
  | First
  | Last
  | Only
  | Odd
  | Even
  | First_of_type
  | Last_of_type
  | Only_of_type
  | Empty
  | Checked
  | Indeterminate
  | Default
  | Required
  | Valid
  | Invalid
  | In_range
  | Out_of_range
  | Placeholder_shown
  | Autofill
  | Read_only
  | Read_write
  | Optional
  | Open
  | Enabled
  | Target
  | Visited
  | Group_first
  | Group_last
  | Group_odd
  | Group_even
  | Peer_first
  | Peer_last
  | Peer_odd
  | Peer_even
  | Group_active
  | Group_visited
  | Group_disabled
  | Group_checked
  | Group_empty
  | Group_required
  | Group_valid
  | Group_invalid
  | Group_indeterminate
  | Group_default
  | Group_open
  | Group_target
  | Peer_active
  | Peer_visited
  | Peer_disabled
  | Peer_empty
  | Peer_required
  | Peer_valid
  | Peer_invalid
  | Peer_indeterminate
  | Peer_default
  | Peer_open
  | Peer_target
  | Group_optional
  | Peer_optional
  | Pseudo_marker
  | Pseudo_selection
  | Pseudo_placeholder
  | Pseudo_backdrop
  | Pseudo_file
  | Pseudo_first_letter
  | Pseudo_first_line
  | Pseudo_details_content
  | Children
  | Descendants

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
