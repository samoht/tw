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
}
(** Theme scheme configuration *)

(** Default scheme - uses oklch colors and calc-based spacing (matches Tailwind
    v4 default) *)
let default : t = { colors = []; spacing = []; radius = [] }

(** Lookup a color in the scheme *)
let find_color scheme name = List.assoc_opt name scheme.colors

(** Lookup a spacing value in the scheme *)
let find_spacing scheme n = List.assoc_opt n scheme.spacing

(** Check if a color is defined as hex in the scheme *)
let is_hex_color scheme name =
  match find_color scheme name with Some (Hex _) -> true | _ -> false

(** Get hex value for a color if defined as hex *)
let get_hex_color scheme name =
  match find_color scheme name with Some (Hex h) -> Some h | _ -> None

(** Check if spacing has an explicit variable *)
let has_explicit_spacing scheme n = Option.is_some (find_spacing scheme n)

(** Lookup a radius value in the scheme *)
let find_radius scheme name = List.assoc_opt name scheme.radius

(** Check if radius has an explicit variable *)
let has_explicit_radius scheme name = Option.is_some (find_radius scheme name)
