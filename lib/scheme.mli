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

val token_default : string -> string option
(** [token_default name] returns the registered baseline default for [name]. *)

val token_override : t -> string -> string option
(** [token_override t name] returns the per-render override for [name], if any.
*)

val theme_value : t option -> string -> string option
(** [theme_value theme name] looks up a per-render token override from the
    optionally-threaded [theme] ([None] when no theme is threaded). Threaded
    replacement for the global [Var.theme_value]. *)

val token : t -> string -> string option
(** [token t name] resolves a theme token: override (if any) else default. *)

val with_overrides : t -> (string * string) list -> t
(** [with_overrides t overrides] applies [overrides] on top of [t]'s existing
    token overrides (new entries win). *)

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
