(** The theme a stylesheet is generated against. See [theme.mli]. *)

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
  inline_tokens : string list;
  static_theme : bool;
      (** Per-render theme token overrides (from a [@theme] block). Key is the
          variable name without the leading [--] (e.g. "text-shadow-2xs"), value
          is the CSS string. Threaded replacement for the global
          [Var.theme_value_overrides]. *)
}
(** Theme scheme configuration *)

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
    inline_tokens = [];
    static_theme = false;
  }

let to_string t =
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

(** [override theme name] looks up a per-render token override from the
    optionally-threaded [theme] ([None] when no theme is threaded). Threaded
    replacement for the global [Var.override]. *)
let override theme name =
  match theme with
  | Some t -> List.assoc_opt name t.token_overrides
  | None -> None

(** Resolve a theme token: override (if any) else the registered default. *)
let token scheme name =
  match token_override scheme name with
  | Some _ as v -> v
  | None -> Token_defaults.find name

(** [with_overrides scheme overrides] returns [scheme] with [overrides] applied
    on top of any existing token overrides (new entries win). *)
let with_overrides ?(inline = []) scheme overrides =
  {
    scheme with
    token_overrides = overrides @ scheme.token_overrides;
    inline_tokens = inline @ scheme.inline_tokens;
  }

let is_inline_token scheme name = List.mem name scheme.inline_tokens
let with_colors colors t = { t with colors = colors @ t.colors }
let with_spacing spacing t = { t with spacing = spacing @ t.spacing }
let with_radius radius t = { t with radius = radius @ t.radius }

let with_breakpoints breakpoints t =
  { t with breakpoints = t.breakpoints @ breakpoints }

let with_widths ?border ?ring ?outline t =
  {
    t with
    default_border_width = Option.value border ~default:t.default_border_width;
    default_ring_width = Option.value ring ~default:t.default_ring_width;
    default_outline_width =
      Option.value outline ~default:t.default_outline_width;
  }

let with_static t = { t with static_theme = true }
let breakpoints t = t.breakpoints
let border_width t = t.default_border_width
let ring_width t = t.default_ring_width
let outline_width t = t.default_outline_width
let inline_tokens t = t.inline_tokens
let is_static t = t.static_theme
