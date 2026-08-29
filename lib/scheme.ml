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

type custom_variant = { values : (string * string) list; template : string }
(** A [matchVariant]-registered variant: a value map (including a [DEFAULT]
    entry under the key [""]) and a selector template containing [{}] where the
    resolved value is substituted. *)

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
  custom_variants : (string * custom_variant) list;
      (** The [@custom-variant]s this [@theme] declared as a value map plus a
          selector template. *)
  container_variants : (string * Css.Container.t) list;
      (** The [@custom-variant]s this [@theme] declared with a container-query
          body. Kept apart from {!custom_variants} because the condition is
          structural, so the [not-] prefix can negate it soundly. *)
}
(** Theme scheme configuration *)

(** Process-global registry of theme token DEFAULTS (the v4.3.1 baseline).
    Populated once at module-init by the utilities that own theme tokens
    (replaces the [Var.theme_ref_registry] defaults). Defaults are static, so
    they live here rather than in the per-render {!t}; overrides are threaded
    via {!t.token_overrides}. *)
let default_tokens : (string, string) Hashtbl.t = Hashtbl.create 64

let register_default_token name css = Hashtbl.replace default_tokens name css
let token_default name = Hashtbl.find_opt default_tokens name

let all_default_tokens () =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) default_tokens []

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
    inline_tokens = [];
    static_theme = false;
    custom_variants = [];
    container_variants = [];
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

(** Lookup the exact CSS length of a breakpoint. Entrypoint [@theme] tokens take
    precedence over the legacy px-only record field. *)
let breakpoint_length scheme name =
  match List.assoc_opt ("breakpoint-" ^ name) scheme.token_overrides with
  | Some value -> Css.parse_length value
  | None ->
      Option.map (fun px -> (Css.Px px : Css.length)) (breakpoint scheme name)

let breakpoint_names scheme =
  let from_tokens =
    List.filter_map
      (fun (name, value) ->
        let prefix = "breakpoint-" in
        if
          String.starts_with ~prefix name
          && Option.is_some (Css.parse_length value)
        then
          Some
            (String.sub name (String.length prefix)
               (String.length name - String.length prefix))
        else None)
      scheme.token_overrides
  in
  List.sort_uniq String.compare (List.map fst scheme.breakpoints @ from_tokens)
  |> List.filter (fun name -> Option.is_some (breakpoint_length scheme name))

(* Tailwind reads [--name: initial] in a [@theme] block as "remove this token",
   and [--namespace-*: initial] as "remove the whole namespace", so a candidate
   that needed the token stops resolving. *)
let removed_value = "initial"

(* Scales whose names begin with another scale's name and are not part of it:
   [--font-*: initial] resets the font families, not the weights or the sizes.
   Mirrors Tailwind's own [ignoredThemeKeyMap]. *)
let nested_scales =
  [
    ("font", [ "font-weight"; "font-size" ]);
    ("inset", [ "inset-shadow"; "inset-ring" ]);
    ( "text",
      [
        "text-color";
        "text-decoration-color";
        "text-decoration-thickness";
        "text-indent";
        "text-shadow";
        "text-underline-offset";
      ] );
    ("grid-column", [ "grid-column-start"; "grid-column-end" ]);
    ("grid-row", [ "grid-row-start"; "grid-row-end" ]);
  ]

let in_nested_scale namespace name =
  List.exists
    (fun nested ->
      String.equal name nested || String.starts_with ~prefix:(nested ^ "-") name)
    (Option.value ~default:[] (List.assoc_opt namespace nested_scales))

let clears_one_namespace name key =
  String.length key > 2
  && String.equal (String.sub key (String.length key - 2) 2) "-*"
  &&
  let namespace = String.sub key 0 (String.length key - 2) in
  String.starts_with ~prefix:namespace name
  && not (in_nested_scale namespace name)

(* [--ns-*: initial] resets one namespace; the bare [--*: initial] names no
   namespace at all and resets the whole theme. *)
let clears_namespace name (key, value) =
  String.equal value removed_value
  && (String.equal key "*" || clears_one_namespace name key)

(** Whether a [@theme] block removed [name], either outright or by resetting the
    namespace it belongs to. A token the block goes on to declare survives its
    own namespace reset. *)
let is_removed scheme name =
  match List.assoc_opt name scheme.token_overrides with
  | Some value -> String.equal value removed_value
  | None -> List.exists (clears_namespace name) scheme.token_overrides

(** Lookup a per-render theme token override (from a [@theme] block). *)
let token_override scheme name =
  if is_removed scheme name then None
  else List.assoc_opt name scheme.token_overrides

(** [theme_value theme name] looks up a per-render token override from the
    optionally-threaded [theme] ([None] when no theme is threaded). Threaded
    replacement for the global [Var.theme_value]. *)
let theme_value theme name =
  match theme with Some s -> token_override s name | None -> None

(** Resolve a theme token: override (if any) else the registered default. A
    token the [@theme] block removed resolves to nothing, default or not. *)
let token scheme name =
  match token_override scheme name with
  | Some _ as v -> v
  | None -> if is_removed scheme name then None else token_default name

(** Every breakpoint the theme defines, keyed by name: the registered defaults,
    the [--breakpoint-*] tokens a [@theme] block set, and the legacy px-only
    field. *)
let all_breakpoints scheme =
  let prefix = "breakpoint-" in
  let suffix (key, _) =
    if String.starts_with ~prefix key then
      Some
        (String.sub key (String.length prefix)
           (String.length key - String.length prefix))
    else None
  in
  let names =
    List.filter_map suffix (all_default_tokens ())
    @ List.filter_map suffix scheme.token_overrides
    @ List.map fst scheme.breakpoints
  in
  List.sort_uniq String.compare names
  |> List.filter_map (fun name ->
      if is_removed scheme (prefix ^ name) then None
      else
        let length =
          match breakpoint_length scheme name with
          | Some _ as length -> length
          | None -> Option.bind (token_default (prefix ^ name)) Css.parse_length
        in
        Option.map (fun length -> (name, length)) length)

(** [has_breakpoint scheme name] is whether [scheme] still defines the
    breakpoint [name], reading the same set as {!all_breakpoints} so a variant
    and a container agree on what the theme has. *)
let has_breakpoint scheme name = List.mem_assoc name (all_breakpoints scheme)

(** [with_overrides scheme overrides] returns [scheme] with [overrides] applied
    on top of any existing token overrides (new entries win). *)
let with_overrides ?(inline = []) scheme overrides =
  let breakpoints =
    List.fold_left
      (fun breakpoints (name, value) ->
        let prefix = "breakpoint-" in
        if String.starts_with ~prefix name then
          let name =
            String.sub name (String.length prefix)
              (String.length name - String.length prefix)
          in
          match Css.parse_length value with
          | Some (Css.Px px) -> (name, px) :: List.remove_assoc name breakpoints
          | _ -> breakpoints
        else breakpoints)
      scheme.breakpoints overrides
  in
  {
    scheme with
    breakpoints;
    token_overrides = overrides @ scheme.token_overrides;
    inline_tokens = inline @ scheme.inline_tokens;
  }

let is_inline_token scheme name = List.mem name scheme.inline_tokens
