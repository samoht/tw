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
  | Min_responsive of breakpoint
  | Max_responsive of breakpoint
  | Min_arbitrary of float
  | Max_arbitrary of float
  | Min_arbitrary_length of Css.length
  | Max_arbitrary_length of Css.length
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
  | Group_has of string * string option
  | Peer_has of string * string option
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
  | Children
  | Descendants
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
  | Noscript
  | Supports of string
  | Group_hocus
  | Peer_hocus
  | Custom_responsive of string
  | Min_custom of string
  | Max_custom of string
  | Group_arbitrary of string
  | Peer_arbitrary of string
  | Hocus  (** [:hover, :focus] compound selector (no media query) *)
  | Device_hocus  (** [:hover, :focus] compound + [@media (hover: hover)] *)
  | Not_bracket of string
      (** [not-[...]] bracket patterns — stores raw bracket content *)
  | In_bracket of string
      (** [in-[...]] ancestor patterns — element must be descendant of selector
      *)
  | In_data of string
      (** [in-data-X] — element must be descendant of [data-X] *)
  | Group_not of modifier * string option
      (** [group-not-X/name] — inner modifier + optional group name *)
  | Peer_not of modifier * string option
      (** [peer-not-X/name] — inner modifier + optional peer name *)
  | Data_bracket of string
      (** [data-[expr]] — full CSS attribute expression, e.g. "foo$=bar_baz_i"
      *)
  | Group_data of string * string option
      (** [group-data-[expr]/name] — group data variant with optional name *)
  | Peer_data of string * string option
      (** [peer-data-[expr]/name] — peer data variant with optional name *)
  | Aria_bracket of string
      (** [aria-[expr]] — arbitrary aria attribute, e.g. "modal", "valuenow=1"
      *)
  | Group_aria of string * string option
      (** [group-aria-X/name] — group aria variant with optional name *)
  | Peer_aria of string * string option
      (** [peer-aria-X/name] — peer aria variant with optional name *)
  | Not_named_group of modifier * string
      (** [not-group-X/name] — negate named group variant *)
  | Has_named_group of modifier * string
      (** [has-group-X/name] — has named group variant *)
  | In_named_group of modifier * string
      (** [in-group-X/name] — descendant of named group variant *)
  | Group_peer_named of modifier * string
      (** [group-peer-X/name] — peer-X within named group *)

type t =
  | Style of {
      props : Css.declaration list;
      rules : Css.statement list option;
      property_rules : Css.t;
      merge_key : string option;
      pseudo_suffix : Css.Selector.t option;
    }
  | Modified of modifier * t
  | Group of t list

val pp : t -> string
(** [pp t] returns a string representation of a style. *)

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

type spacing = [ `Px | `Full | `Rem of float | `Named of string ]
type margin = [ spacing | `Auto ]
type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
type shadow = [ size | `Inner ]

val pp_modifier : modifier -> string
(** [pp_modifier m] converts a modifier to its string representation. *)

val style :
  ?rules:Css.statement list option ->
  ?property_rules:Css.t ->
  ?merge_key:string ->
  ?pseudo_suffix:Css.Selector.t ->
  Css.declaration list ->
  t
(** [style ?rules ?property_rules props] defines a utility with CSS [props].
    - [rules]: Optional custom CSS rules (for utilities like prose that generate
      multiple rules with descendant selectors).
    - [property_rules]: Optional CSS property rules needed by this utility.
    - [props]: CSS properties to apply. *)
