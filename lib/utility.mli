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

        let name = "example"
        let priority _ = 4

        let to_style _theme = function
          | Block -> Style.style [ Css.display Css.Block ]

        let suborder = function Block -> 0

        let of_class _theme = function
          | "example-block" -> Ok Block
          | _ -> Error (`Msg "Not an example utility")

        let to_class = function Block -> "example-block"

        (* One utility per property this handler sets, so a project's own
           [@utility] declaring [display] can find its slot. *)
        let examples = [ Block ]
      end

      module Utility_factory = Utility.Make (Handler)

      let utility = Utility_factory.v
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

type base
(** Base utility type without modifiers. Values are created only by a
    {!Make}-registered handler, so dispatch over one is total. *)

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

(** Tailwind's property table, read as a suborder scale.

    Tailwind sorts a layer by the ranks its property table gives the properties
    each rule writes: two rules are separated by the first rank they do not
    share, and one whose ranks run out there sorts after one that keeps going. A
    {!Handler.suborder} is a single integer, so each rank takes a slot a
    thousand values wide. *)
module Property_order : sig
  val slot : int -> int
  (** [slot rank] is the suborder of a utility whose sort key reaches [rank] and
      writes a further property. Add what separates it from the others sharing
      [rank], keeping the total below [slot (rank + 1)]. *)

  val last : int -> int
  (** [last rank] is the suborder of a utility whose sort key ends at [rank]. It
      sorts after every utility in [rank]'s slot. *)
end

(** Handler module type for utility registration *)
module type Handler = sig
  type t
  (** The utility type *)

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

  val examples : t list
  (** A few of this handler's utilities, one per property it can set.
      {!order_of_property} runs them through {!to_style} to learn which
      properties belong to which slot, so nothing has to restate the order or
      pair a property with a variant by hand. Covering a property here is what
      lets a project's [@utility] declaring it sort in the right place. *)
end

module Make (H : Handler) : sig
  val v : H.t -> t
  (** Applying [Make] registers [H] at module initialization. [v value] packages
      [value] as a utility handled by [H]. *)
end

val order_of_property : Cascade.Css.Declaration.prop_key -> (int * int) option
(** [order_of_property k] is the [(priority, suborder)] of the utility family
    that sets property [k], or [None] when no registered handler claims it. This
    is where a declared [@utility] gets its place: Tailwind sorts one by the
    property it declares, not by its name. A family that writes [k] on the
    element itself outranks one that writes it only on a pseudo-element, so
    [color] resolves to the text colours rather than to
    [placeholder-transparent]. *)

val ordering_property :
  Cascade.Css.declaration list -> Cascade.Css.Declaration.prop_key option
(** [ordering_property declarations] is the first property that determines a
    utility's Tailwind family. It skips theme-token declarations, the carrier
    declarations a width utility emits to reference the style channel
    ([border-style], [outline-style]), and a vendor-prefixed spelling a later
    declaration repeats unprefixed. A prefixed property with no unprefixed twin
    ([-webkit-line-clamp]) is the utility's own and keeps its slot. *)

val base_of_class : Scheme.t -> string -> (base, [ `Msg of string ]) result
(** [base_of_class theme class_name] parses a class name into a base utility
    (without modifiers). For internal use by the Tw module. *)

val examples_classes : unit -> string list
(** [examples_classes ()] is the class name of every utility each registered
    handler offers as one of its {!Handler.examples}. *)

val claiming_handlers : Scheme.t -> string -> string list
(** [claiming_handlers theme class_name] is the name of every registered handler
    whose [of_class] accepts [class_name]. {!base_of_class} answers with the
    first, and handlers are tried in dune link order, so a class more than one
    handler accepts resolves on a build detail rather than on anything declared.
    Exposed so a test can assert that no class is claimed twice. *)

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
