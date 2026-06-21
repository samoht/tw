(** Theme scheme configuration for customizing CSS output.

    A scheme defines theme overrides that affect how utilities generate CSS.
    This allows matching Tailwind's test expectations which use custom [@theme]
    definitions with hex colors and explicit spacing variables. *)

module Css = Cascade.Css

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
          variable name without the leading [--] (e.g. "text-shadow-2xs"), value
          is the CSS string. Threaded replacement for the global
          [Var.theme_value_overrides]. *)
}
(** Theme scheme configuration *)

(** Process-global registry of theme token DEFAULTS (the v4.3.1 baseline).
    Populated once at module-init by the utilities that own theme tokens
    (replaces the [Var.theme_ref_registry] defaults). Defaults are static, so
    they live here rather than in the per-render {!t}; overrides are threaded via
    {!t.token_overrides}. *)
let default_tokens : (string, string) Hashtbl.t = Hashtbl.create 64

let register_default_token name css = Hashtbl.replace default_tokens name css
let token_default name = Hashtbl.find_opt default_tokens name

(** Default scheme - uses oklch colors and calc-based spacing (matches Tailwind
    v4 default) *)
let default : t =
  {
    colors = [];
    spacing = [];
    radius = [];
    default_ring_width = 1;
    default_border_width = 1;
    default_outline_width = 1;
    breakpoints = [];
    token_overrides = [];
  }

let pp t =
  Pp.str
    [
      "{colors=";
      Pp.int (List.length t.colors);
      "; spacing=";
      Pp.int (List.length t.spacing);
      "; radius=";
      Pp.int (List.length t.radius);
      "; ring=";
      Pp.int t.default_ring_width;
      "; border=";
      Pp.int t.default_border_width;
      "; outline=";
      Pp.int t.default_outline_width;
      "; breakpoints=";
      Pp.int (List.length t.breakpoints);
      "}";
    ]

(** Lookup a color in the scheme *)
let color scheme name = List.assoc_opt name scheme.colors

(** Lookup a spacing value in the scheme *)
let spacing scheme n = List.assoc_opt n scheme.spacing

(** Check if a color is defined as hex in the scheme *)
let is_hex_color scheme name =
  match color scheme name with Some (Hex _) -> true | _ -> false

(** Get hex value for a color if defined as hex *)
let hex_color scheme name =
  match color scheme name with Some (Hex h) -> Some h | _ -> None

(** Check if spacing has an explicit variable *)
let has_explicit_spacing scheme n = Option.is_some (spacing scheme n)

(** Lookup a radius value in the scheme *)
let radius scheme name = List.assoc_opt name scheme.radius

(** Check if radius has an explicit variable *)
let has_explicit_radius scheme name = Option.is_some (radius scheme name)

(** Lookup a breakpoint px value in the scheme *)
let breakpoint scheme name = List.assoc_opt name scheme.breakpoints

(** Lookup a per-render theme token override (from a [@theme] block). *)
let token_override scheme name = List.assoc_opt name scheme.token_overrides

(** Resolve a theme token: override (if any) else the registered default. *)
let token scheme name =
  match token_override scheme name with
  | Some _ as v -> v
  | None -> token_default name

(** [with_overrides scheme overrides] returns [scheme] with [overrides] applied
    on top of any existing token overrides (new entries win). *)
let with_overrides scheme overrides =
  { scheme with token_overrides = overrides @ scheme.token_overrides }
