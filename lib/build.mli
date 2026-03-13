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

(** {1 CSS generation} *)

type config = {
  base : bool;
      (** Emit [@layer base] with Preflight reset and forms styles. Default:
          [true]. *)
  forms : bool option;
      (** Include forms plugin base styles. [None] = auto-detect from utility
          classes; [Some true] = always include; [Some false] = never include.
      *)
  mode : Css.mode;
      (** [Variables]: full layered output with CSS custom properties. [Inline]:
          raw rules, variables resolved to their fallback values. Default:
          [Variables]. *)
  layers : bool;
      (** Wrap output in [@layer] directives. When [false] the same content is
          emitted without the [@layer theme] / [@layer utilities] wrappers
          ([@layer properties] is always kept). Default: [true]. *)
  optimize : bool;
      (** Apply CSS optimizations (merge adjacent rules, deduplicate
          declarations). Default: [false]. *)
}

val default_config : config
(** The default configuration. Base layer enabled, variables mode, no
    optimization. *)

val to_css : ?config:config -> Utility.t list -> Css.t
(** [to_css ?config utilities] generates a full CSS stylesheet for [utilities].
    This is the main entry point for the library.

    In [Variables] mode the stylesheet contains all five layers plus trailing
    [@property] registrations. In [Inline] mode it contains only raw utility
    rules with variable references resolved to their fallback values. *)

val to_inline_style : Utility.t list -> string
(** [to_inline_style utilities] returns a CSS [style] attribute string (e.g.
    ["color: red; font-size: 1rem"]) suitable for embedding in HTML. Custom
    properties are stripped; only concrete declarations are included. *)

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
