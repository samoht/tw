module Css = Cascade.Css

type color_value =
  | Hex of string
  | Oklch of { l : float; c : float; h : float }

type custom_variant = { values : (string * string) list; template : string }
type variant_branch = { media : Css.Media.t option; selector : Css.Selector.t }

type t = {
  colors : (string * color_value) list;
  spacing : (int * Css.length) list;
  radius : (string * Css.length) list;
  default_ring_width : int;
  default_border_width : int;
  default_outline_width : int;
  breakpoints : (string * float) list;
  token_overrides : (string * string) list;
  inline_tokens : string list;
  reference_tokens : string list;
  reference_theme : bool;
  static_tokens : string list;
  static_theme : bool;
  important : bool;
  prefix : string option;
  custom_variants : (string * custom_variant) list;
  container_variants : (string * Css.Container.t) list;
  dark : variant_branch list option;
}

(* The default token catalog. Utilities publish their entries once at module
   initialisation, through an [Atomic] so no lock is needed; per-render values
   stay in [token_overrides]. *)
module Tokens = Map.Make (String)

let default_tokens = Atomic.make Tokens.empty

let rec register_default_token name css =
  let current = Atomic.get default_tokens in
  let next = Tokens.add name css current in
  if not (Atomic.compare_and_set default_tokens current next) then
    register_default_token name css

let token_default name = Tokens.find_opt name (Atomic.get default_tokens)
let all_default_tokens () = Tokens.bindings (Atomic.get default_tokens)

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
    reference_tokens = [];
    reference_theme = false;
    static_tokens = [];
    static_theme = false;
    important = false;
    prefix = None;
    custom_variants = [];
    container_variants = [];
    dark = None;
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

let color scheme name = List.assoc_opt name scheme.colors
let spacing scheme n = List.assoc_opt n scheme.spacing

let hex_color scheme name =
  match color scheme name with Some (Hex h) -> Some h | _ -> None

let has_explicit_spacing scheme n = Option.is_some (spacing scheme n)
let radius scheme name = List.assoc_opt name scheme.radius
let has_explicit_radius scheme name = Option.is_some (radius scheme name)
let breakpoint scheme name = List.assoc_opt name scheme.breakpoints

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
  String.ends_with ~suffix:"-*" key
  && String.length key > 2
  &&
  let namespace = String.sub key 0 (String.length key - 2) in
  String.starts_with ~prefix:namespace name
  && not (in_nested_scale namespace name)

(* [--ns-*: initial] resets one namespace; the bare [--*: initial] names no
   namespace at all and resets the whole theme. *)
let clears_namespace name (key, value) =
  String.equal value removed_value
  && (String.equal key "*" || clears_one_namespace name key)

(* A token the block goes on to declare survives its own namespace reset. *)
let is_removed scheme name =
  match List.assoc_opt name scheme.token_overrides with
  | Some value -> String.equal value removed_value
  | None -> List.exists (clears_namespace name) scheme.token_overrides

let token_override scheme name =
  if is_removed scheme name then None
  else List.assoc_opt name scheme.token_overrides

let or_default theme = Option.value ~default theme
let theme_value theme name = Option.bind theme (fun s -> token_override s name)

let declares_color theme name =
  match theme with
  | None -> false
  | Some t -> color t name <> None || token_override t ("color-" ^ name) <> None

let token scheme name =
  match token_override scheme name with
  | Some _ as v -> v
  | None -> if is_removed scheme name then None else token_default name

let removes_tokens scheme =
  List.exists
    (fun (_, value) -> String.equal value removed_value)
    scheme.token_overrides

(* A name that was a token before the block took it away: [--tw-shadow] is
   nobody's token, so the bare [--*: initial] does not reach it. *)
let is_removed_token scheme name =
  is_removed scheme name
  && (Option.is_some (token_default name)
     || List.mem_assoc name scheme.token_overrides)

(* A removed breakpoint has no length: [initial] would read as one, and a query
   built from it is one no browser honours. *)
let breakpoint_length scheme name =
  let key = "breakpoint-" ^ name in
  if is_removed scheme key then None
  else
    match List.assoc_opt key scheme.token_overrides with
    | Some value -> Css.parse_length value
    | None ->
        Option.map (fun px -> (Css.Px px : Css.length)) (breakpoint scheme name)

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

let has_breakpoint scheme name = List.mem_assoc name (all_breakpoints scheme)

(* The names a variant may spell, read off the same set: a breakpoint the
   [@theme] block removed is not among them, whatever value the removal
   wrote. *)
let breakpoint_names scheme = List.map fst (all_breakpoints scheme)

let with_overrides ?(inline = []) ?(reference = []) ?(static = []) scheme
    overrides =
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
    reference_tokens = reference @ scheme.reference_tokens;
    static_tokens = static @ scheme.static_tokens;
  }

let is_inline_token scheme name = List.mem name scheme.inline_tokens

let is_reference_token scheme name =
  List.mem name scheme.reference_tokens
  || (scheme.reference_theme && not (List.mem_assoc name scheme.token_overrides))
