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

type var = Color of { name : string; shade : int option } | Spacing of int

let color_var ?shade name = Color { name; shade }

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

(* Helper to create a style with no variable requirements *)
let style name props = Style { name; props; vars = [] }

(* Helper to create a style with variable requirements *)
let style_with_vars name props vars = Style { name; props; vars }
