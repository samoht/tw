(** Theme scheme configuration for customizing CSS output.

    A scheme defines theme overrides that affect how utilities generate CSS.
    This allows matching Tailwind's test expectations which use custom [@theme]
    definitions with hex colors and explicit spacing variables. *)

open Cascade

(** Color value - either hex string or oklch components *)
type color_value =
  | Hex of string  (** e.g., "#ef4444" *)
  | Oklch of { l : float; c : float; h : float }
      (** e.g., oklch(63.7% 0.237 25.331) *)

type custom_variant = { values : (string * string) list; template : string }
(** A [\@custom-variant] registered with a value map (the [DEFAULT] value under
    key [""]) and a selector template with [{}] where the value is substituted.
    [&] in the template denotes the element's own class. *)

type t = {
  colors : (string * color_value) list;
      (** Color overrides. Key is color name like "red-500". When a color is
          Hex, opacity modifiers use hex+alpha fallback. When Oklch, opacity
          modifiers use color-mix fallback. *)
  spacing : (int * Css.length) list;
      (** Explicit spacing variables. Key is the multiplier (e.g., 4 for
          --spacing-4). When defined, utilities use var(--spacing-N) instead of
          calc(var(--spacing) * N). *)
  radius : (string * Css.length) list;
      (** Explicit radius variables. Key is the radius name (e.g., "none",
          "full", "sm"). When defined, utilities use var(--radius-NAME) instead
          of raw values. *)
  default_ring_width : int;
      (** Default ring width in pixels for bare [ring] utility. Corresponds to
          Tailwind's [@theme \{ --default-ring-width: Npx \}]. Default: 1. *)
  default_border_width : int;
      (** Default border width in pixels for bare [border] utility. Corresponds
          to Tailwind's [@theme \{ --default-border-width: Npx \}]. Default: 1.
      *)
  default_outline_width : int;
      (** Default outline width in pixels for bare [outline] utility.
          Corresponds to Tailwind's [@theme \{ --default-outline-width: Npx \}].
          Default: 1. *)
  breakpoints : (string * float) list;
      (** Explicit breakpoint values in px. Key is breakpoint name (e.g., "sm").
          When defined, responsive media queries use [@media (min-width: Xpx)]
          instead of rem-based values. *)
  token_overrides : (string * string) list;
      (** Per-render theme token overrides (from a [@theme] block). Key is the
          variable name without the leading [--]; value is the CSS string.
          Threaded replacement for the global [Var.theme_value_overrides]. *)
  inline_tokens : string list;
      (** Names of the tokens a project declared in an [\@theme inline] block.
          Such a token has no declaration of its own: a utility reading it
          inlines the value instead of referencing [var(--name)]. *)
  static_theme : bool;
      (** Whether the package was imported with [theme(static)], which emits
          every theme variable rather than only the ones a utility used. *)
  custom_variants : (string * custom_variant) list;
      (** The [\@custom-variant]s this [\@theme] declared, each a value map and
          a selector template. A variant belongs to the theme that declared it,
          so two stylesheets built in one process cannot see each other's. *)
  container_variants : (string * Css.Container.t) list;
      (** The [\@custom-variant]s this [\@theme] declared with a container-query
          body (e.g. [has-a] -> [\@container style(--a)]). Kept apart from
          {!custom_variants} because the condition is structural, so the [not-]
          prefix negates it soundly. *)
}
(** Theme scheme configuration *)

val pp : t -> string
(** [pp t] returns a string representation of a scheme configuration. *)

val default : t
(** [default] is the default scheme using oklch colors and calc-based spacing
    (matches Tailwind v4 default). *)

val register_default_token : string -> string -> unit
(** [register_default_token name css] registers the v4.3.1 baseline default CSS
    for theme token [name] (without [--]) in the process-global registry. Called
    once at module-init by the utility that owns the token. *)

val all_default_tokens : unit -> (string * string) list
(** [all_default_tokens ()] is every token a family has published through
    {!register_default_token}. [theme(static)] emits them all. *)

val token_default : string -> string option
(** [token_default name] returns the registered baseline default for [name]. *)

val is_removed : t -> string -> bool
(** [is_removed t name] is whether the [\@theme] block removed [name]. Tailwind
    reads [--name: initial] as "remove this token", [--namespace-*: initial] as
    "remove the whole namespace" and the bare [--*: initial] as "remove every
    token", and a candidate that needed the token stops resolving. A token the
    block declares in its own right survives a reset, and so does a nested scale
    that merely shares the prefix: [--font-*: initial] leaves [--font-weight-*]
    alone. *)

val token_override : t -> string -> string option
(** [token_override t name] returns the per-render override for [name], if any.
    A removed token has none. *)

val theme_value : t option -> string -> string option
(** [theme_value theme name] looks up a per-render token override from the
    optionally-threaded [theme] ([None] when no theme is threaded). Threaded
    replacement for the global [Var.theme_value]. *)

val token : t -> string -> string option
(** [token t name] resolves a theme token: override (if any) else default, or
    nothing at all when the [\@theme] block removed it. *)

val with_overrides : ?inline:string list -> t -> (string * string) list -> t
(** [with_overrides ?inline t overrides] applies [overrides] on top of [t]'s
    existing token overrides (new entries win). [inline] names the tokens that
    came from an [\@theme inline] block. *)

val is_inline_token : t -> string -> bool
(** [is_inline_token t name] is whether [name] was declared in an
    [\@theme inline] block, so a utility reading it inlines the value rather
    than referencing [var(--name)]. *)

val color : t -> string -> color_value option
(** [color t name] looks up a color in the scheme. *)

val spacing : t -> int -> Css.length option
(** [spacing t n] looks up a spacing value in the scheme. *)

val is_hex_color : t -> string -> bool
(** [is_hex_color t name] checks if a color is defined as hex in the scheme. *)

val hex_color : t -> string -> string option
(** [hex_color t name] returns the hex value for a color if defined as hex. *)

val has_explicit_spacing : t -> int -> bool
(** [has_explicit_spacing t n] checks if spacing has an explicit variable. *)

val radius : t -> string -> Css.length option
(** [radius t name] looks up a radius value in the scheme. *)

val has_explicit_radius : t -> string -> bool
(** [has_explicit_radius t name] checks if radius has an explicit variable. *)

val breakpoint : t -> string -> float option
(** [breakpoint t name] looks up a breakpoint px value in the scheme. *)

val breakpoint_length : t -> string -> Css.length option
(** [breakpoint_length t name] looks up a breakpoint as its exact CSS length. A
    [breakpoint-NAME] token override takes precedence over the legacy px-only
    {!breakpoint} field. *)

val all_breakpoints : t -> (string * Css.length) list
(** [all_breakpoints t] is every breakpoint [t] defines, keyed by name and
    sorted by name: the registered defaults, the [--breakpoint-*] tokens a
    [\@theme] block set, and the legacy px-only {!breakpoints} field. A
    breakpoint the block removed is left out. *)

val has_breakpoint : t -> string -> bool
(** [has_breakpoint t name] is whether [t] still defines the breakpoint [name],
    reading the same set as {!all_breakpoints}. A breakpoint the [\@theme] block
    removed is gone for the variants that name it. *)

val breakpoint_names : t -> string list
(** [breakpoint_names t] returns the custom breakpoint names available while
    parsing variants. *)
