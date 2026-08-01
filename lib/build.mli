(** CSS layer assembly — converts {!Output.t} lists into Tailwind's layered
    stylesheet.

    This module is the final stage of the pipeline. It takes the flat list of
    {!Output.t} values produced by {!Rule.outputs}, sorts them with {!Sort},
    deduplicates theme variables, and assembles the result into Tailwind v4's
    layer architecture:

    {v
      @layer properties   (* CSS custom property registrations *)

      @layer theme        (* design-token variables *)
      @layer base         (* Preflight reset + forms base styles *)
      @layer components   (* (empty placeholder) *)
      @layer utilities    (* sorted utility rules *)
    v}

    The main entry point for normal use is {!to_css}. The lower-level functions
    ([theme_layer_of], [rule_sets], [utilities_layer]) are exposed for testing
    and for the [tw] CLI. *)

(* [open Cascade] below shadows this library's own [Sort] with cascade's; bind
   it first so the signature can still name the rule type. *)
module Rule_sort := Sort

open Cascade
(** {1 CSS generation} *)

type config = {
  base : bool;
      (** Emit [@layer base] with Preflight reset and forms styles. Default:
          [true]. *)
  forms : bool option;
      (** Include forms plugin base styles. [None] = auto-detect from utility
          classes; [Some true] = always include; [Some false] = never include.
      *)
  layers : bool;
      (** Wrap output in [@layer] directives. When [false] the same content is
          emitted without the [@layer theme] / [@layer utilities] wrappers
          ([@layer properties] is always kept). Default: [true]. *)
}

val default_config : config
(** The default configuration. Base layer enabled. *)

(** A utility a project declared with Tailwind's [@utility], as the CSS its body
    expanded to plus the position it takes among the built-in utilities. *)
module Declared : sig
  (** Where a rule sorts in the utilities layer.

      Tailwind orders that layer by the property each utility writes, and a
      project's own [@utility] takes its place in it like any other. A body
      reaches a slot two ways: by applying a utility that sorts there, or by
      declaring one of the properties that sort there. *)
  module Slot : sig
    type t

    val of_class : ?theme:Scheme.t -> string -> t option
    (** [of_class cls] is the slot the utility named [cls] sorts at, and [None]
        when [cls] names no utility. A body that [@apply]s [cls] reaches it. *)

    val of_property : Css.Declaration.prop_key -> t option
    (** [of_property k] is the slot of the utility family that writes [k], and
        [None] when no family writes it. A body that declares [k] reaches it. *)

    val earliest : t list -> t option
    (** [earliest slots] is the first of [slots] in the layer, and [None] for
        the empty list. A utility sorts at the earliest slot it reaches:
        Tailwind sorts [@utility x { @apply bg-red-500 relative }] with
        [relative], not with [bg-red-500]. *)
  end

  type t

  val v : cls:string -> slot:Slot.t -> Css.statement list -> t
  (** [v ~cls ~slot stmts] is the utility [cls], whose body expanded to [stmts],
      sorting into the utilities layer at [slot]. *)
end

val to_css :
  ?theme:Scheme.t ->
  ?config:config ->
  ?declared:Declared.t list ->
  Utility.t list ->
  Css.t
(** [to_css ?theme ?config ?declared utilities] generates a full CSS stylesheet
    for [utilities]. This is the main entry point for the library. [theme]
    (default {!Scheme.default}) supplies the theme values utilities read while
    generating CSS. Rendering concerns such as inlining and optimization are
    handled by {!Css.to_string}.

    [declared] carries the project's own [@utility] rules. The handlers know
    nothing about a declared utility, so its slot comes in with it, and its
    statements sort into the utilities layer at that slot instead of landing
    after it. *)

val to_inline_style : ?theme:Scheme.t -> Utility.t list -> string
(** [to_inline_style ?theme utilities] returns a CSS [style] attribute string
    (e.g. ["color: red; font-size: 1rem"]) suitable for embedding in HTML.
    Custom properties are stripped; only concrete declarations are included. *)

(** {1 Layer building} *)

val theme_layer_of :
  ?default_decls:Css.declaration list -> Utility.t list -> Css.t
(** [theme_layer_of ?default_decls utilities] builds the [@layer theme] block
    containing all CSS custom property variables referenced by [utilities], plus
    any [default_decls] (e.g. baseline font-family declarations). *)

val rule_sets : Utility.t list -> Css.statement list
(** [rule_sets utilities] extracts and sorts CSS statements for [utilities] with
    media queries interleaved in Tailwind order. Used by the [tw] CLI and by
    {!utilities_layer}. *)

val utilities_layer : layers:bool -> statements:Css.statement list -> Css.t
(** [utilities_layer ~layers ~statements] wraps [statements] in
    [@layer utilities] when [layers = true], merging consecutive [@media] blocks
    with the same condition. *)

(** {1 Testing helpers}

    Exposed for test coverage of the internal pipeline; not part of the stable
    API. *)

val conflict_order : string -> int * int
(** [conflict_order selector] returns the [(priority, suborder)] pair that
    determines cascade position for the utility named by [selector]. Lower
    priority numbers win over higher ones when classes conflict. *)

val indexed_rules : Utility.t list -> Rule_sort.indexed_rule list
(** [indexed_rules utilities] is the unsorted rule list that {!compare_rules} is
    applied to, exposed so the comparator can be checked on the values it
    actually sees rather than on hand-built records. *)

val compare_rules : Rule_sort.indexed_rule -> Rule_sort.indexed_rule -> int
(** [compare_rules] is the comparator {!val-rule_sets} sorts with. Sorting is
    only defined for a total order, so this is the function whose antisymmetry
    and transitivity the sort suite checks. *)

val rule_selector : Rule_sort.indexed_rule -> string
(** [rule_selector r] is [r]'s selector, for naming a rule in a failure. *)

val selector_props_pairs :
  Output.t list -> (Css.Selector.t * Css.declaration list * (int * int)) list
(** [selector_props_pairs outputs] flattens [Regular] outputs into
    [(selector, props, order)] triples. Other variants are dropped. *)

val of_grouped :
  ?filter_custom_props:bool ->
  (Css.Selector.t * Css.declaration list * (int * int)) list ->
  Css.statement list
(** [of_grouped triples] converts sorted [(selector, props, order)] triples into
    CSS rule statements, merging adjacent rules with the same selector. When
    [filter_custom_props = true] (default [false]), custom property declarations
    are stripped from the output. *)
