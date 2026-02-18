(** Theme scheme configuration for customizing CSS output.

    A scheme defines theme overrides that affect how utilities generate CSS.
    This allows matching Tailwind's test expectations which use custom @theme
    definitions with hex colors and explicit spacing variables. *)

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
}
(** Theme scheme configuration *)

val default : t
(** Default scheme - uses oklch colors and calc-based spacing (matches Tailwind
    v4 default) *)

val find_color : t -> string -> color_value option
(** Lookup a color in the scheme *)

val find_spacing : t -> int -> Css.length option
(** Lookup a spacing value in the scheme *)

val is_hex_color : t -> string -> bool
(** Check if a color is defined as hex in the scheme *)

val get_hex_color : t -> string -> string option
(** Get hex value for a color if defined as hex *)

val has_explicit_spacing : t -> int -> bool
(** Check if spacing has an explicit variable *)

val find_radius : t -> string -> Css.length option
(** Lookup a radius value in the scheme *)

val has_explicit_radius : t -> string -> bool
(** Check if radius has an explicit variable *)
