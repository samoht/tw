(** Utility module for common utility types and functions

    {1 Styling Guide for Utility Modules}

    All utility modules should follow this standardized pattern:

    {2 Module Structure}

    {[
    module Example = struct
      module Css = Cascade.Css
      module Style = Tw.Style
      module Utility = Tw.Utility

      module Handler = struct
        type t = Block
        type Utility.base += Self of t

        let name = "example"
        let priority = 4
        let to_style = function Block -> Style.style [ Css.display Css.Block ]
        let suborder = function Block -> 0

        let of_class = function
          | "example-block" -> Ok Block
          | _ -> Error (`Msg "Not an example utility")

        let to_class = function Block -> "example-block"
      end

      let () = Utility.register (module Handler)
      let utility x = Utility.base (Handler.Self x)
      let example_block = utility Handler.Block
    end
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
type t =
  | Base of base
  | Modified of Style.modifier * t
  | Group of t list
  | Important of t

val base : base -> t
(** [base u] wraps a base utility into a Utility.t. *)

val important : t -> t
(** [important u] marks every declaration [u] emits as [!important] (the [!]
    utility prefix). *)

val pp : t -> string
(** [pp u] is a human-readable representation of [u] for debugging. *)

(** Handler module type for utility registration *)
module type Handler = sig
  type t
  (** The utility type *)

  type base += Self of t  (** Extension of the base utility type *)

  val name : string
  (** Name of this utility handler. *)

  val to_style : t -> Style.t
  (** [to_style u] converts utility [u] to a style. *)

  val priority : int
  (** Priority for ordering utilities. *)

  val suborder : t -> int
  (** [suborder u] is the suborder within the same priority. *)

  val of_class : string -> (t, [ `Msg of string ]) result
  (** [of_class name] parses class [name] into a utility. *)

  val to_class : t -> string
  (** [to_class u] is the CSS class name for utility [u]. *)
end

val register : (module Handler with type t = 'a) -> unit
(** [register h] registers a utility handler module. *)

val base_of_class : string -> (base, [ `Msg of string ]) result
(** [base_of_class class_name] parses a class name into a base utility (without
    modifiers). For internal use by the Tw module. *)

val base_of_strings : string list -> (base, [ `Msg of string ]) result
(** [base_of_strings parts] parses a list of string parts into a base utility.
    Deprecated: use base_of_class. For backward compatibility with tests. *)

val base_to_style : base -> Style.t
(** [base_to_style u] converts a base utility (without modifiers) to Style.t. *)

val name_of_base : base -> string
(** [name_of_base u] returns the utility name. *)

val class_of_base : base -> string
(** [class_of_base u] returns the CSS class name for a base utility. *)

val to_style : t -> Style.t
(** [to_style u] converts Utility.t (with modifiers) to Style.t. *)

val to_class : t -> string
(** [to_class u] converts Utility.t (with modifiers) to class name string. *)

val order : base -> int * int
(** [order u] gets the ordering information (priority, suborder) for a base
    utility. *)

val deduplicate : t list -> t list
(** [deduplicate utils] deduplicates utilities while preserving order (last
    occurrence wins). *)
