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
  | Max_responsive of breakpoint
  | Min_arbitrary of float
  | Max_arbitrary of float
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
  | Nth of string
  | Nth_last of string
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
  | Inert
  | User_valid
  | User_invalid
  | Group_first
  | Group_last
  | Group_only
  | Group_odd
  | Group_even
  | Group_first_of_type
  | Group_last_of_type
  | Group_only_of_type
  | Peer_first
  | Peer_last
  | Peer_only
  | Peer_odd
  | Peer_even
  | Peer_first_of_type
  | Peer_last_of_type
  | Peer_only_of_type
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
  | Group_read_only
  | Peer_read_only
  | Group_read_write
  | Peer_read_write
  | Group_inert
  | Peer_inert
  | Group_user_valid
  | Peer_user_valid
  | Group_user_invalid
  | Peer_user_invalid
  | Group_placeholder_shown
  | Peer_placeholder_shown
  | Group_autofill
  | Peer_autofill
  | Group_in_range
  | Peer_in_range
  | Group_out_of_range
  | Peer_out_of_range
  | Group_focus_within
  | Peer_focus_within
  | Group_focus_visible
  | Peer_focus_visible
  | Group_enabled
  | Peer_enabled
  | Pseudo_marker
  | Pseudo_selection
  | Pseudo_placeholder
  | Pseudo_backdrop
  | Pseudo_file
  | Pseudo_first_letter
  | Pseudo_first_line
  | Pseudo_details_content
  | Children (* * variant - all direct children *)
  | Descendants (* ** variant - all descendants *)
  | Ltr
  | Rtl
  | Print
  | Portrait
  | Landscape
  | Forced_colors
  | Inverted_colors
  | Pointer_none
  | Pointer_coarse
  | Pointer_fine
  | Any_pointer_none
  | Any_pointer_coarse
  | Any_pointer_fine
  | Scripting_none
  | Supports of string

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
  | Max_responsive `Sm -> "max-sm"
  | Max_responsive `Md -> "max-md"
  | Max_responsive `Lg -> "max-lg"
  | Max_responsive `Xl -> "max-xl"
  | Max_responsive `Xl_2 -> "max-2xl"
  | Min_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      String.concat "" [ "min-["; px_str; "px]" ]
  | Max_arbitrary px ->
      let px_str =
        if Float.is_integer px then Int.to_string (Float.to_int px)
        else Float.to_string px
      in
      String.concat "" [ "max-["; px_str; "px]" ]
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
  | Nth expr -> "nth-[" ^ expr ^ "]"
  | Nth_last expr -> "nth-last-[" ^ expr ^ "]"
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
  | Read_write -> "read-write"
  | Optional -> "optional"
  | Open -> "open"
  | Enabled -> "enabled"
  | Target -> "target"
  | Visited -> "visited"
  | Inert -> "inert"
  | User_valid -> "user-valid"
  | User_invalid -> "user-invalid"
  | Group_first -> "group-first"
  | Group_last -> "group-last"
  | Group_only -> "group-only"
  | Group_odd -> "group-odd"
  | Group_even -> "group-even"
  | Group_first_of_type -> "group-first-of-type"
  | Group_last_of_type -> "group-last-of-type"
  | Group_only_of_type -> "group-only-of-type"
  | Peer_first -> "peer-first"
  | Peer_last -> "peer-last"
  | Peer_only -> "peer-only"
  | Peer_odd -> "peer-odd"
  | Peer_even -> "peer-even"
  | Peer_first_of_type -> "peer-first-of-type"
  | Peer_last_of_type -> "peer-last-of-type"
  | Peer_only_of_type -> "peer-only-of-type"
  | Group_active -> "group-active"
  | Group_visited -> "group-visited"
  | Group_disabled -> "group-disabled"
  | Group_checked -> "group-checked"
  | Group_empty -> "group-empty"
  | Group_required -> "group-required"
  | Group_valid -> "group-valid"
  | Group_invalid -> "group-invalid"
  | Group_indeterminate -> "group-indeterminate"
  | Group_default -> "group-default"
  | Group_open -> "group-open"
  | Group_target -> "group-target"
  | Peer_active -> "peer-active"
  | Peer_visited -> "peer-visited"
  | Peer_disabled -> "peer-disabled"
  | Peer_empty -> "peer-empty"
  | Peer_required -> "peer-required"
  | Peer_valid -> "peer-valid"
  | Peer_invalid -> "peer-invalid"
  | Peer_indeterminate -> "peer-indeterminate"
  | Peer_default -> "peer-default"
  | Peer_open -> "peer-open"
  | Peer_target -> "peer-target"
  | Group_optional -> "group-optional"
  | Peer_optional -> "peer-optional"
  | Group_read_only -> "group-read-only"
  | Peer_read_only -> "peer-read-only"
  | Group_read_write -> "group-read-write"
  | Peer_read_write -> "peer-read-write"
  | Group_inert -> "group-inert"
  | Peer_inert -> "peer-inert"
  | Group_user_valid -> "group-user-valid"
  | Peer_user_valid -> "peer-user-valid"
  | Group_user_invalid -> "group-user-invalid"
  | Peer_user_invalid -> "peer-user-invalid"
  | Group_placeholder_shown -> "group-placeholder-shown"
  | Peer_placeholder_shown -> "peer-placeholder-shown"
  | Group_autofill -> "group-autofill"
  | Peer_autofill -> "peer-autofill"
  | Group_in_range -> "group-in-range"
  | Peer_in_range -> "peer-in-range"
  | Group_out_of_range -> "group-out-of-range"
  | Peer_out_of_range -> "peer-out-of-range"
  | Group_focus_within -> "group-focus-within"
  | Peer_focus_within -> "peer-focus-within"
  | Group_focus_visible -> "group-focus-visible"
  | Peer_focus_visible -> "peer-focus-visible"
  | Group_enabled -> "group-enabled"
  | Peer_enabled -> "peer-enabled"
  | Pseudo_marker -> "marker"
  | Pseudo_selection -> "selection"
  | Pseudo_placeholder -> "placeholder"
  | Pseudo_backdrop -> "backdrop"
  | Pseudo_file -> "file"
  | Pseudo_first_letter -> "first-letter"
  | Pseudo_first_line -> "first-line"
  | Pseudo_details_content -> "details-content"
  | Children -> "*"
  | Descendants -> "**"
  | Ltr -> "ltr"
  | Rtl -> "rtl"
  | Print -> "print"
  | Portrait -> "portrait"
  | Landscape -> "landscape"
  | Forced_colors -> "forced-colors"
  | Inverted_colors -> "inverted-colors"
  | Pointer_none -> "pointer-none"
  | Pointer_coarse -> "pointer-coarse"
  | Pointer_fine -> "pointer-fine"
  | Any_pointer_none -> "any-pointer-none"
  | Any_pointer_coarse -> "any-pointer-coarse"
  | Any_pointer_fine -> "any-pointer-fine"
  | Scripting_none -> "scripting-none"
  | Supports cond -> "supports-[" ^ cond ^ "]"

let rec pp ppf = function
  | Style { props; _ } ->
      Format.fprintf ppf "Style(%d props)" (List.length props)
  | Modified (m, s) -> Format.fprintf ppf "%s:%a" (pp_modifier m) pp s
  | Group styles -> Format.fprintf ppf "Group(%d)" (List.length styles)
