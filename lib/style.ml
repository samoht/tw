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

type t =
  | Style of {
      props : Css.declaration list;
      rules : Css.statement list option;
      property_rules : Css.t;
    }
  | Modified of modifier * t
  | Group of t list

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

type spacing = [ `Px | `Full | `Rem of float ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

(* Helper to create a style *)
let style ?(rules = None) ?(property_rules = Css.empty) props =
  Style { props; rules; property_rules }

(* Convert modifier to string prefix *)
let rec pp_modifier = function
  | Hover -> "hover"
  | Focus -> "focus"
  | Active -> "active"
  | Disabled -> "disabled"
  | Dark -> "dark"
  | Responsive `Sm -> "sm"
  | Responsive `Md -> "md"
  | Responsive `Lg -> "lg"
  | Responsive `Xl -> "xl"
  | Responsive `Xl_2 -> "2xl"
  | Container Container_sm -> "@sm"
  | Container Container_md -> "@md"
  | Container Container_lg -> "@lg"
  | Container Container_xl -> "@xl"
  | Container Container_2xl -> "@2xl"
  | Container (Container_named (n, size)) ->
      String.concat "" [ "@"; n; "/"; string_of_int size ]
  | Group_hover -> "group-hover"
  | Group_focus -> "group-focus"
  | Peer_hover -> "peer-hover"
  | Peer_focus -> "peer-focus"
  | Peer_checked -> "peer-checked"
  | Aria_checked -> "aria-checked"
  | Aria_expanded -> "aria-expanded"
  | Aria_selected -> "aria-selected"
  | Aria_disabled -> "aria-disabled"
  | Data_state s -> String.concat "" [ "data-state="; s ]
  | Data_variant s -> String.concat "" [ "data-variant="; s ]
  | Data_active -> "data-active"
  | Data_inactive -> "data-inactive"
  | Data_custom (k, v) -> String.concat "" [ "data-"; k; "="; v ]
  | Not m -> String.concat "" [ "not("; pp_modifier m; ")" ]
  | Has s -> String.concat "" [ "has-["; s; "]" ]
  | Group_has s -> String.concat "" [ "group-has-["; s; "]" ]
  | Peer_has s -> String.concat "" [ "peer-has-["; s; "]" ]
  | Starting -> "starting"
  | Focus_within -> "focus-within"
  | Focus_visible -> "focus-visible"
  | Motion_safe -> "motion-safe"
  | Motion_reduce -> "motion-reduce"
  | Contrast_more -> "contrast-more"
  | Contrast_less -> "contrast-less"
  | Pseudo_before -> "before"
  | Pseudo_after -> "after"
  | First -> "first"
  | Last -> "last"
  | Only -> "only"
  | Odd -> "odd"
  | Even -> "even"
  | First_of_type -> "first-of-type"
  | Last_of_type -> "last-of-type"
  | Only_of_type -> "only-of-type"
  | Empty -> "empty"
  | Checked -> "checked"
  | Indeterminate -> "indeterminate"
  | Default -> "default"
  | Required -> "required"
  | Valid -> "valid"
  | Invalid -> "invalid"
  | In_range -> "in-range"
  | Out_of_range -> "out-of-range"
  | Placeholder_shown -> "placeholder-shown"
  | Autofill -> "autofill"
  | Read_only -> "read-only"
  | Open -> "open"
  | Enabled -> "enabled"
  | Target -> "target"
  | Visited -> "visited"
  | Group_first -> "group-first"
  | Group_last -> "group-last"
  | Group_odd -> "group-odd"
  | Group_even -> "group-even"
  | Peer_first -> "peer-first"
  | Peer_last -> "peer-last"
  | Peer_odd -> "peer-odd"
  | Peer_even -> "peer-even"

let rec pp ppf = function
  | Style { props; _ } ->
      Format.fprintf ppf "Style(%d props)" (List.length props)
  | Modified (m, s) -> Format.fprintf ppf "%s:%a" (pp_modifier m) pp s
  | Group styles -> Format.fprintf ppf "Group(%d)" (List.length styles)
