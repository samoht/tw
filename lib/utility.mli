(** Utility module for common utility types and functions

    {1 Styling Guide for Utility Modules}

    All utility modules should follow this standardized pattern:

    {2 Module Structure}

    {[
      (** Module documentation with links to Tailwind CSS docs *)

      open Style
      open Css

      module Handler = struct
        type t =
          | Variant1
          | Variant2
          | ...

        type Utility.base += Self of t

        let priority = N  (* See priority table below *)

        let variant1 = style "class-name" [ css_prop value ]
        let variant2 = style "class-name" [ css_prop value ]

        (* Note: Use Css.prop qualified names if helper name conflicts with CSS property *)
        let example = style "example" [ Css.flex Auto ]

        let to_style = function
          | Variant1 -> variant1
          | Variant2 -> variant2
          | ...

        let suborder = function
          | Variant1 -> 0
          | Variant2 -> 1
          | ...

        let of_string =
          let err_not_utility = Error (`Msg "Not a <category> utility") in
          function
          | [ "class"; "name" ] -> Ok Variant1
          | [ "other"; "class" ] -> Ok Variant2
          | _ -> err_not_utility
      end

      open Handler

      let () = Utility.register (module Handler)

      let utility x = Utility.base (Self x)

      (* Public API *)
      let variant1 = utility Variant1
      let variant2 = utility Variant2
    ]}

    {2 Key Rules}

    + All helper functions go inside Handler module
    + Use [Css.property] qualification when helper name conflicts with CSS
      property
    + Public API functions shadow Handler helpers after [open Handler]
    + Use descriptive error messages in [of_string]

    {2 Priority Assignment}

    Utilities are ordered by (priority, suborder). Common priorities:
    - 1-10: Layout fundamentals (position, display)
    - 11-20: Flexbox, Grid, Spacing
    - 21-30: Sizing, Typography
    - 31-50: Colors, Borders, Effects
    - 51-100: Transforms, Transitions, Animations
    - 100+: Modifiers and special utilities
    - 800+: Component-level utilities (forms, etc.) *)

type base = ..
(** Base utility type without modifiers - extensible variant *)

(** Unified utility type with modifiers support *)
type t = Base of base | Modified of Style.modifier * t | Group of t list

val base : base -> t
(** [base u] wraps a base utility into a Utility.t *)

(** Handler module type for utility registration *)
module type Handler = sig
  type t
  (** The utility type *)

  type base += Self of t  (** Extension of the base utility type *)

  val to_style : t -> Style.t
  (** Convert utility to style *)

  val priority : int
  (** Priority for ordering utilities *)

  val suborder : t -> int
  (** Suborder within the same priority *)

  val of_string : string list -> (t, [ `Msg of string ]) result
  (** Parse string parts into utility *)
end

val register : (module Handler with type t = 'a) -> unit
(** [register (module H)] registers a utility handler module *)

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

val order : base -> int * int
(** Get the ordering information (priority, suborder) for a base utility *)

val deduplicate : t list -> t list
(** Deduplicate utilities while preserving order (last occurrence wins) *)
