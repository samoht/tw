(** Utility module for common utility types and functions *)

(** Base utility type without modifiers *)
type base =
  | Positioning of Positioning.utility
  | Grid of Grid.utility
  | Margin of Margin.utility
  | Containers of Containers.utility
  | Prose of Prose.utility
  | Display of Display.utility
  | Layout of Layout.utility
  | Tables of Tables.utility
  | Sizing of Sizing.utility
  | Cursor of Cursor.utility
  | Grid_template of Grid_template.utility
  | Flex of Flex.utility
  | Alignment of Alignment.utility
  | Gap of Gap.utility
  | Borders of Borders.utility
  | Backgrounds of Backgrounds.utility
  | Padding of Padding.utility
  | Typography of Typography.utility
  | Color of Color.utility
  | Effects of Effects.utility
  | Filters of Filters.utility
  | Transforms of Transforms.utility
  | Animations of Animations.utility
  | Interactivity of Interactivity.utility
  | Forms of Forms.utility
  | Svg of Svg.utility
  | Accessibility of Accessibility.utility

(** Unified utility type with modifiers support *)
type t = Utility of base | Modified of Style.modifier * t | Group of t list

(** Parse CSS string into AST *)
val css_of_string :
  ?filename:string -> string -> (Css.t, Css.parse_error) result
(** [css_of_string ?filename css_str] parses a CSS string into an AST. Returns
    [Ok ast] on success or [Error err] on parse failure. *)

(** Parse a class string into a base utility (without modifiers) *)
val base_of_string : string list -> (base, [ `Msg of string ]) result
(** [base_of_string parts] parses a list of string parts into a base utility.
    For internal use by the Tw module. *)

val base_to_style : base -> Style.t
(** Convert a base utility (without modifiers) to Style.t *)

val to_style : t -> Style.t
(** Convert Utility.t (with modifiers) to Style.t *)

val priority : base -> int
(** Get the priority for a base utility *)

val order : base -> int * int
(** Get the ordering information (priority, suborder) for a base utility *)

val deduplicate : t list -> t list
(** Deduplicate utilities while preserving order (last occurrence wins) *)
