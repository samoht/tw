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

(* Helper to create a style *)
let style ?(rules = None) name props =
  Style { name; props; rules }

(* Extract base class name(s) from Core.t. Modifiers are ignored. *)
let rec class_name = function
  | Style { name; _ } -> name
  | Modified (_, t) -> class_name t
  | Group ts -> String.concat " " (List.map class_name ts)

(* Convert modifier to string prefix *)
let rec pp_modifier = function
  | Hover -> "hover:"
  | Focus -> "focus:"
  | Active -> "active:"
  | Disabled -> "disabled:"
  | Dark -> "dark:"
  | Responsive `Sm -> "sm:"
  | Responsive `Md -> "md:"
  | Responsive `Lg -> "lg:"
  | Responsive `Xl -> "xl:"
  | Responsive `Xl_2 -> "2xl:"
  | Container Container_sm -> "@sm:"
  | Container Container_md -> "@md:"
  | Container Container_lg -> "@lg:"
  | Container Container_xl -> "@xl:"
  | Container Container_2xl -> "@2xl:"
  | Container (Container_named (n, size)) ->
      Pp.str [ "@"; n; "/"; string_of_int size; ":" ]
  | Group_hover -> "group-hover:"
  | Group_focus -> "group-focus:"
  | Peer_hover -> "peer-hover:"
  | Peer_focus -> "peer-focus:"
  | Peer_checked -> "peer-checked:"
  | Aria_checked -> "aria-checked:"
  | Aria_expanded -> "aria-expanded:"
  | Aria_selected -> "aria-selected:"
  | Aria_disabled -> "aria-disabled:"
  | Data_state s -> Pp.str [ "data-state="; s; ":" ]
  | Data_variant s -> Pp.str [ "data-variant="; s; ":" ]
  | Data_active -> "data-active:"
  | Data_inactive -> "data-inactive:"
  | Data_custom (k, v) -> Pp.str [ "data-"; k; "="; v; ":" ]
  | Not m -> Pp.str [ "not("; pp_modifier m; ")" ]
  | Has s -> Pp.str [ "has("; s; "):" ]
  | Group_has s -> Pp.str [ "group-has("; s; "):" ]
  | Peer_has s -> Pp.str [ "peer-has("; s; "):" ]
  | Starting -> "starting:"
  | Focus_within -> "focus-within:"
  | Focus_visible -> "focus-visible:"
  | Motion_safe -> "motion-safe:"
  | Motion_reduce -> "motion-reduce:"
  | Contrast_more -> "contrast-more:"
  | Contrast_less -> "contrast-less:"
  | Pseudo_before -> "before:"
  | Pseudo_after -> "after:"

(* Extract full class name including modifiers *)
let rec pp = function
  | Style { name; _ } -> name
  | Modified (m, t) -> Pp.str [ pp_modifier m; pp t ]
  | Group ts -> String.concat " " (List.map pp ts)
