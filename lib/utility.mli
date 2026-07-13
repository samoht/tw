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
        let priority _ = 4

        let to_style _theme = function
          | Block -> Style.style [ Css.display Css.Block ]

        let suborder = function Block -> 0

        let of_class _theme = function
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
  | Important of bool * t  (** [bool] is [true] for the v4 trailing [!] form. *)
  | Aliased of string * t
      (** [Aliased (class_name, u)] renders as [u] but reports [class_name] from
          {!to_class}, so the emitted selector matches the source spelling. Used
          for the [prop-(--x)] shorthand, which is [prop-[var(--x)]] in value
          but keeps its own class name. *)

val base : base -> t
(** [base u] wraps a base utility into a Utility.t. *)

val alias : string -> t -> t
(** [alias class_name u] is [u] with its class name overridden to [class_name]
    (see {!constructor-Aliased}). *)

val important : ?suffix:bool -> t -> t
(** [important ?suffix u] marks every declaration [u] emits as [!important]: the
    [!] utility prefix, or the v4 trailing [!] form when [suffix] is [true]. *)

val pp : t -> string
(** [pp u] is a human-readable representation of [u] for debugging. *)

(** Handler module type for utility registration *)
module type Handler = sig
  type t
  (** The utility type *)

  type base += Self of t  (** Extension of the base utility type *)

  val name : string
  (** Name of this utility handler. *)

  val to_style : Scheme.t -> t -> Style.t
  (** [to_style theme u] converts utility [u] to a style, reading any theme
      values it needs from [theme]. *)

  val priority : t -> int
  (** [priority u] is the primary ordering key for utility [u]. Usually a module
      constant ([let priority _ = n]); modules whose variants span several
      canonical families (e.g. layout) return per-variant values. *)

  val suborder : t -> int
  (** [suborder u] is the suborder within the same priority. *)

  val of_class : Scheme.t -> string -> (t, [ `Msg of string ]) result
  (** [of_class theme name] parses class [name] into a utility, consulting
      [theme] for custom token validation (e.g. named colors and opacities). *)

  val to_class : t -> string
  (** [to_class u] is the CSS class name for utility [u]. *)
end

val register : (module Handler with type t = 'a) -> unit
(** [register h] registers a utility handler module. *)

val base_of_class : Scheme.t -> string -> (base, [ `Msg of string ]) result
(** [base_of_class theme class_name] parses a class name into a base utility
    (without modifiers). For internal use by the Tw module. *)

val base_of_strings :
  Scheme.t -> string list -> (base, [ `Msg of string ]) result
(** [base_of_strings theme parts] parses a list of string parts into a base
    utility. Deprecated: use base_of_class. For backward compatibility with
    tests. *)

val base_to_style : Scheme.t -> base -> Style.t
(** [base_to_style theme u] converts a base utility (without modifiers) to
    Style.t, reading theme values from [theme]. *)

val name_of_base : base -> string
(** [name_of_base u] returns the utility name. *)

val class_of_base : base -> string
(** [class_of_base u] returns the CSS class name for a base utility. *)

val to_style : Scheme.t -> t -> Style.t
(** [to_style theme u] converts Utility.t (with modifiers) to Style.t, reading
    theme values from [theme]. *)

val to_class : t -> string
(** [to_class u] converts Utility.t (with modifiers) to class name string. *)

val order : base -> int * int
(** [order u] gets the ordering information (priority, suborder) for a base
    utility. *)

val deduplicate : t list -> t list
(** [deduplicate utils] deduplicates utilities while preserving order (last
    occurrence wins). *)
