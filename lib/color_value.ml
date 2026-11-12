(** The colours a class names, and the values and theme tokens they resolve to.
*)

module Css = Cascade.Css
open Srgb

type color =
  | Black
  | White
  | Gray
  | Slate
  | Zinc
  | Neutral
  | Stone
  | Mauve
  | Olive
  | Mist
  | Taupe
  | Red
  | Orange
  | Amber
  | Yellow
  | Lime
  | Green
  | Emerald
  | Teal
  | Cyan
  | Sky
  | Blue
  | Indigo
  | Violet
  | Purple
  | Fuchsia
  | Pink
  | Rose
  | Hex of string
  | Rgb of { red : int; green : int; blue : int }
  | Oklch of oklch
  | Css of Css.color
  | Theme_named of string

(* Color constructors *)
let black = Black
let white = White
let gray = Gray
let slate = Slate
let mauve = Mauve
let olive = Olive
let mist = Mist
let taupe = Taupe
let zinc = Zinc
let neutral = Neutral
let stone = Stone
let red = Red
let orange = Orange
let amber = Amber
let yellow = Yellow
let lime = Lime
let green = Green
let emerald = Emerald
let teal = Teal
let cyan = Cyan
let sky = Sky
let blue = Blue
let indigo = Indigo
let violet = Violet
let purple = Purple
let fuchsia = Fuchsia
let pink = Pink
let rose = Rose

let hex s =
  if is_rgb_call s then
    match parse_rgb_string s with
    | Some rgb -> Hex (shorten_hex_str (hex_string_of_rgb rgb))
    | None -> Hex s
  else Hex (shorten_hex_str s)

(* Convert string name to color type *)
let of_string_exn = function
  | "black" -> Black
  | "white" -> White
  | "gray" -> Gray
  | "slate" -> Slate
  | "mauve" -> Mauve
  | "olive" -> Olive
  | "mist" -> Mist
  | "taupe" -> Taupe
  | "zinc" -> Zinc
  | "neutral" -> Neutral
  | "stone" -> Stone
  | "red" -> Red
  | "orange" -> Orange
  | "amber" -> Amber
  | "yellow" -> Yellow
  | "lime" -> Lime
  | "green" -> Green
  | "emerald" -> Emerald
  | "teal" -> Teal
  | "cyan" -> Cyan
  | "sky" -> Sky
  | "blue" -> Blue
  | "indigo" -> Indigo
  | "violet" -> Violet
  | "purple" -> Purple
  | "fuchsia" -> Fuchsia
  | "pink" -> Pink
  | "rose" -> Rose
  | s -> failwith ("Unknown color: " ^ s)

let of_string = function
  | "black" -> Ok Black
  | "white" -> Ok White
  | "gray" -> Ok Gray
  | "slate" -> Ok Slate
  | "mauve" -> Ok Mauve
  | "olive" -> Ok Olive
  | "mist" -> Ok Mist
  | "taupe" -> Ok Taupe
  | "zinc" -> Ok Zinc
  | "neutral" -> Ok Neutral
  | "stone" -> Ok Stone
  | "red" -> Ok Red
  | "orange" -> Ok Orange
  | "amber" -> Ok Amber
  | "yellow" -> Ok Yellow
  | "lime" -> Ok Lime
  | "green" -> Ok Green
  | "emerald" -> Ok Emerald
  | "teal" -> Ok Teal
  | "cyan" -> Ok Cyan
  | "sky" -> Ok Sky
  | "blue" -> Ok Blue
  | "indigo" -> Ok Indigo
  | "violet" -> Ok Violet
  | "purple" -> Ok Purple
  | "fuchsia" -> Ok Fuchsia
  | "pink" -> Ok Pink
  | "rose" -> Ok Rose
  | s ->
      let len = String.length s in
      if len >= 4 && s.[0] = '[' && s.[1] = '#' && s.[len - 1] = ']' then
        (* Arbitrary bracket hex value like [#0088cc]. Store original hex
           (unshortened) so class names preserve it. Shortening happens later in
           to_css for CSS output, through the raising [Css.hex], so only a
           spelling that constructor reads is a colour: a digit count it does
           not know ([#12345]) is as much a miss as [#0088cc]/[0.5], where the ]
           belongs to a different bracket. *)
        let hex = String.sub s 2 (len - 3) in
        if Option.is_some (Css.hex_opt hex) then Ok (Hex hex)
        else Error (`Msg ("Unknown color: " ^ s))
      else if len >= 3 && s.[0] = '[' && s.[len - 1] = ']' then
        let inner = String.sub s 1 (len - 2) in
        let normalized = Parse.decode_underscores inner in
        if Parse.is_css_color_fn normalized then
          match Css.parse_color normalized with
          | Some c -> Ok (Css c)
          | None -> Error (`Msg ("Unknown color: " ^ s))
        else Error (`Msg ("Unknown color: " ^ s))
      else Error (`Msg ("Unknown color: " ^ s))

let rgb r g b =
  if r < 0 || r > 255 then
    invalid_arg ("RGB red value " ^ string_of_int r ^ " out of range [0-255]");
  if g < 0 || g > 255 then
    invalid_arg ("RGB green value " ^ string_of_int g ^ " out of range [0-255]");
  if b < 0 || b > 255 then
    invalid_arg ("RGB blue value " ^ string_of_int b ^ " out of range [0-255]");
  Rgb { red = r; green = g; blue = b }

(* Convert color to OKLCH data for a given shade *)
let to_oklch_opt color shade =
  match color with
  | Black -> Some { l = 0.0; c = 0.0; h = 0.0 }
  | White -> Some { l = 100.0; c = 0.0; h = 0.0 }
  | Oklch oklch -> Some oklch
  | Hex h -> (
      match hex_to_rgb h with
      | Some rgb -> Some (rgb_to_oklch rgb)
      | None -> Some { l = 0.0; c = 0.0; h = 0.0 })
  | Rgb { red; green; blue } ->
      Some (rgb_to_oklch { r = red; g = green; b = blue })
  | Css c -> (
      (* Extract RGB channels from CSS color for oklch conversion *)
      match c with
      | Css.Hex { r; g; b; _ } | Css.Authored_hex { r; g; b; _ } -> (
          match hex_to_rgb (hex_string_of_rgb (r, g, b)) with
          | Some rgb -> Some (rgb_to_oklch rgb)
          | None -> Some { l = 0.0; c = 0.0; h = 0.0 })
      | Css.Rgb (Channels { r; g; b })
      | Css.Rgba { rgb = Channels { r; g; b }; _ } ->
          let chan_to_int : Css.channel -> int = function
            | Int i -> i
            | Num f -> Float.to_int (Float.round f)
            | Pct f -> Float.to_int (Float.round (f *. 2.55))
            | Var _ -> 0
            | None -> 0
          in
          Some
            (rgb_to_oklch
               { r = chan_to_int r; g = chan_to_int g; b = chan_to_int b })
      | _ -> Some { l = 0.0; c = 0.0; h = 0.0 })
  | _ ->
      (* For named colors, get OKLCH data directly from Tailwind *)
      let color_name =
        match color with
        | Gray -> "gray"
        | Slate -> "slate"
        | Zinc -> "zinc"
        | Neutral -> "neutral"
        | Stone -> "stone"
        | Mauve -> "mauve"
        | Olive -> "olive"
        | Mist -> "mist"
        | Taupe -> "taupe"
        | Red -> "red"
        | Orange -> "orange"
        | Amber -> "amber"
        | Yellow -> "yellow"
        | Lime -> "lime"
        | Green -> "green"
        | Emerald -> "emerald"
        | Teal -> "teal"
        | Cyan -> "cyan"
        | Sky -> "sky"
        | Blue -> "blue"
        | Indigo -> "indigo"
        | Violet -> "violet"
        | Purple -> "purple"
        | Fuchsia -> "fuchsia"
        | Pink -> "pink"
        | Rose -> "rose"
        | _ -> ""
      in
      Palette.oklch color_name shade

(* A colour outside the palette has no OKLCH of its own; reading it as black
   keeps the conversion total. Callers that can do better - the ones with a
   theme to consult - go through [to_oklch_opt] instead. *)
let to_oklch color shade =
  match to_oklch_opt color shade with
  | Some oklch -> oklch
  | None -> { l = 0.0; c = 0.0; h = 0.0 }

(* Convert color to OKLCH CSS string for a given shade *)
let to_oklch_css color shade =
  match color with
  | Black -> "#000"
  | White -> "#fff"
  | Oklch oklch -> oklch_to_css oklch
  | Hex h -> hex_to_oklch_css h
  | Rgb { red; green; blue } ->
      rgb_to_oklch { r = red; g = green; b = blue } |> oklch_to_css
  | _ -> (
      (* For named colors, get from Tailwind *)
      let color_name =
        match color with
        | Gray -> "gray"
        | Slate -> "slate"
        | Zinc -> "zinc"
        | Neutral -> "neutral"
        | Stone -> "stone"
        | Mauve -> "mauve"
        | Olive -> "olive"
        | Mist -> "mist"
        | Taupe -> "taupe"
        | Red -> "red"
        | Orange -> "orange"
        | Amber -> "amber"
        | Yellow -> "yellow"
        | Lime -> "lime"
        | Green -> "green"
        | Emerald -> "emerald"
        | Teal -> "teal"
        | Cyan -> "cyan"
        | Sky -> "sky"
        | Blue -> "blue"
        | Indigo -> "indigo"
        | Violet -> "violet"
        | Purple -> "purple"
        | Fuchsia -> "fuchsia"
        | Pink -> "pink"
        | Rose -> "rose"
        | _ -> ""
      in
      match Palette.oklch_css color_name shade with
      | Some value -> value
      | None -> "oklch(0% 0 0)" (* Fallback *))

(* Tailwind writes the powerless hue of an achromatic colour as [none] rather
   than a number. Keeping the component missing matters: a numeric hue lets the
   value fold to a plain hex, and interpolation would take hue 0 instead of the
   other colour's hue. *)
let css_color_of_oklch (o : oklch) : Css.color =
  if o.c = 0.0 then Css.oklch_none_hue o.l o.c else Css.oklch o.l o.c o.h

let oklch_node_of color shade =
  let oklch = to_oklch color shade in
  css_color_of_oklch oklch

(* A project token declared in an [\@theme] block names a colour the palette
   knows nothing about: [--color-brand] has no shades to convert, and its value
   is whatever the block bound it to. *)
let theme_named_color ?theme name =
  match Scheme.theme_value theme ("color-" ^ name) with
  | Some value -> Css.parse_color value
  | None -> None

(* The palette ([Red], [Blue], ...) is a fixed set of (colour, shade) pairs and
   its oklch nodes are immutable, so materialise each node once and share it
   across every utility and variant that uses the colour, instead of
   reconstructing an identical node per use. Built during module initialisation
   and then read-only; only the constant palette constructors are keyed here,
   [Rgb]/[Theme_named] carry open-ended payloads and build fresh. *)
let palette_nodes =
  let table = Hashtbl.create 256 in
  List.iter
    (fun (color, palette) ->
      List.iter
        (fun (shade, o) ->
          Hashtbl.replace table (color, shade) (css_color_of_oklch o))
        palette)
    [
      (Gray, Palette.gray);
      (Slate, Palette.slate);
      (Zinc, Palette.zinc);
      (Neutral, Palette.neutral);
      (Stone, Palette.stone);
      (Mauve, Palette.mauve);
      (Olive, Palette.olive);
      (Mist, Palette.mist);
      (Taupe, Palette.taupe);
      (Red, Palette.red);
      (Orange, Palette.orange);
      (Amber, Palette.amber);
      (Yellow, Palette.yellow);
      (Lime, Palette.lime);
      (Green, Palette.green);
      (Emerald, Palette.emerald);
      (Teal, Palette.teal);
      (Cyan, Palette.cyan);
      (Sky, Palette.sky);
      (Blue, Palette.blue);
      (Indigo, Palette.indigo);
      (Violet, Palette.violet);
      (Purple, Palette.purple);
      (Fuchsia, Palette.fuchsia);
      (Pink, Palette.pink);
      (Rose, Palette.rose);
    ];
  table

(* The value a colour has with no scheme colour standing in for it. *)
let palette_css ?theme color shade =
  match color with
  (* Tailwind writes these two palette entries in the three digits its own theme
     block spells, where every other entry is an [oklch()]. [Css.hex] keeps the
     decoded bytes alone and the printer then spells them in full. *)
  | Black -> authored_hex "#000"
  | White -> authored_hex "#fff"
  (* The class named the spelling, so that is the one CSS gets, [#] and all. *)
  | Hex hex -> authored_hex hex
  | Oklch oklch -> css_color_of_oklch oklch
  | Css c -> c
  | Theme_named name -> (
      match theme_named_color ?theme name with
      | Some c -> c
      (* Only a token the theme declares parses into [Theme_named], so a class
         never lands here; a colour built by hand out of thin air does. *)
      | None -> Css.Transparent)
  | Rgb _ -> oklch_node_of color shade
  | _ -> (
      match Hashtbl.find_opt palette_nodes (color, shade) with
      | Some node -> node
      | None -> oklch_node_of color shade)

let named_color_name = function
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
  | Mauve -> "mauve"
  | Olive -> "olive"
  | Mist -> "mist"
  | Taupe -> "taupe"
  | Red -> "red"
  | Orange -> "orange"
  | Amber -> "amber"
  | Yellow -> "yellow"
  | Lime -> "lime"
  | Green -> "green"
  | Emerald -> "emerald"
  | Teal -> "teal"
  | Cyan -> "cyan"
  | Sky -> "sky"
  | Blue -> "blue"
  | Indigo -> "indigo"
  | Violet -> "violet"
  | Purple -> "purple"
  | Fuchsia -> "fuchsia"
  | Pink -> "pink"
  | Rose -> "rose"
  | _ -> ""

let is_named_color color = named_color_name color <> ""

(* Get the name of a color as a string *)
let to_name color =
  if is_named_color color then named_color_name color
  else
    match color with
    | Hex h ->
        let h_stripped =
          if String.starts_with ~prefix:"#" h then
            String.sub h 1 (String.length h - 1)
          else h
        in
        let pp_hex ctx h =
          Css.Pp.string ctx "[";
          Css.Pp.string ctx h;
          Css.Pp.string ctx "]"
        in
        Css.Pp.to_string ~minify:false pp_hex h_stripped
    | Rgb { red; green; blue } ->
        let pp_rgb ctx (r, g, b) =
          Css.Pp.string ctx "[rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")]"
        in
        Css.Pp.to_string ~minify:false pp_rgb (red, green, blue)
    | Oklch oklch ->
        let pp_oklch ctx oklch =
          Css.Pp.string ctx "[oklch(";
          Css.Pp.float ctx oklch.l;
          Css.Pp.string ctx "%,";
          Css.Pp.float ctx oklch.c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx oklch.h;
          Css.Pp.string ctx ")]"
        in
        Css.Pp.to_string ~minify:false pp_oklch oklch
    | Css Css.Transparent -> "transparent"
    | Css Css.Inherit -> "inherit"
    | Css c ->
        (* Serialize CSS color to bracket string for class names. Only spaces
           map to [_]: Tailwind reads an [_] inside a [var()] name as itself,
           which {!Parse.encode_underscores} would escape. *)
        let s = Css.Pp.to_string ~minify:true Css.pp_color c in
        let s = String.map (fun c -> if c = ' ' then '_' else c) s in
        "[" ^ s ^ "]"
    | Theme_named name -> name
    | _ -> ""

(* Pretty printer for colors *)
let pp color =
  if is_named_color color then named_color_name color
  else
    match color with
    | Hex s ->
        (* Use Tailwind's arbitrary value syntax [#hex] for hex colors *)
        let hex_value =
          if String.starts_with ~prefix:"#" s then s else "#" ^ s
        in
        let pp_hex_val ctx v =
          Css.Pp.string ctx "[";
          Css.Pp.string ctx v;
          Css.Pp.string ctx "]"
        in
        Css.Pp.to_string ~minify:false pp_hex_val hex_value
    | Rgb { red; green; blue } ->
        let pp_rgb_val ctx (r, g, b) =
          Css.Pp.string ctx "Rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")"
        in
        Css.Pp.to_string ~minify:false pp_rgb_val (red, green, blue)
    | Oklch { l; c; h } ->
        let pp_oklch_val ctx (l, c, h) =
          Css.Pp.string ctx "Oklch(";
          Css.Pp.float ctx l;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx h;
          Css.Pp.string ctx ")"
        in
        Css.Pp.to_string ~minify:false pp_oklch_val (l, c, h)
    | Css Css.Transparent -> "transparent"
    | Css Css.Inherit -> "inherit"
    | Css c ->
        let s = Css.Pp.to_string ~minify:true Css.pp_color c in
        "Css(" ^ s ^ ")"
    | Theme_named name -> name
    | _ -> ""

(* Check if a color is black or white *)
let is_base_color = function Black | White -> true | _ -> false

(* Check if a color is a custom color (hex, rgb, or oklch) *)
let is_custom_color = function
  | Hex _ | Rgb _ | Oklch _ | Css _ -> true
  | _ -> false

(* CSS-wide colour keywords need no palette or theme lookup. Keep them out of
   [of_string]: a few utility families intentionally support only one of
   them. *)
let opacity_keyword = function
  | Css Css.Transparent -> Some Css.Transparent
  | Css Css.Inherit -> Some Css.Inherit
  | _ -> None

(* Check if a color is a theme-named color (no shade suffix) *)
let is_theme_named = function Theme_named _ -> true | _ -> false

(* The name a scheme keys a colour's token by: [pink-500], [black], or a project
   colour's own name. *)
let scheme_color_name c shade =
  match c with
  | Black | White | Theme_named _ -> pp c
  | _ -> pp c ^ "-" ^ string_of_int shade

(* The value a scheme binds a colour's token to. *)
let scheme_color ?theme c shade : Css.color option =
  match theme with
  | None -> None
  | Some scheme -> (
      match Scheme.color scheme (scheme_color_name c shade) with
      | Some (Scheme.Hex hex) -> Some (Css.hex hex)
      | Some (Scheme.Oklch { l; c; h }) -> Some (css_color_of_oklch { l; c; h })
      | None -> None)

(* A colour's value: what the scheme binds its token to, or else the palette's.
   Every utility reads the token, so every utility has to read it here. *)
let to_css ?theme color shade =
  match scheme_color ?theme color shade with
  | Some value -> value
  | None -> palette_css ?theme color shade

(* Check if a color should NOT have a shade suffix in class names *)
let is_shadeless c = is_base_color c || is_custom_color c || is_theme_named c

let is_valid_shade color shade =
  is_shadeless color || List.mem shade Palette.shades

let check_shade ~utility color shade =
  if not (is_valid_shade color shade) then
    invalid_arg
      (utility ^ ": " ^ pp color ^ " has no shade " ^ string_of_int shade
     ^ " (valid shades: 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950)")

(* Theme layer color variable ordering map. See build.mli for detailed layer
   ordering documentation. Tailwind declares no [--color-transparent] token, but
   the keyword is a colour a utility can name, so it leads the ranking. *)
let theme_color_order_map =
  List.mapi (fun index name -> (name, index)) ("transparent" :: Palette.names)

(* Utilities layer color ordering map for conflict resolution. The utilities
   layer ranks the same names the theme layer does, but by its own rule:
   [transparent] and [black] lead and the rest are alphabetical. Reading the
   names off the theme map is what keeps a colour added there from silently
   taking the unknown-colour slot here. *)
let utilities_color_order_map =
  let leading = [ "transparent"; "black" ] in
  let alphabetical =
    List.sort String.compare
      (List.filter_map
         (fun (name, _) -> if List.mem name leading then None else Some name)
         theme_color_order_map)
  in
  List.mapi (fun index name -> (name, index)) (leading @ alphabetical)

(* Get theme layer order for a color variable. Returns (priority=2, suborder)
   where 2 indicates these are theme layer variables. *)
let theme_order color_name =
  match List.assoc_opt color_name theme_color_order_map with
  | Some suborder ->
      (2, suborder * 1000)
      (* Priority 2 for theme layer variables, multiply by 1000 for spacing *)
  | None -> (2, 100000)
(* Unknown colors go last within theme layer *)

(* Get utilities layer order for conflict resolution. Returns (priority,
   suborder) for utilities layer ordering. *)
let utilities_order color_name =
  match List.assoc_opt color_name utilities_color_order_map with
  | Some suborder -> (2, suborder) (* Priority 2 for color utilities *)
  | None -> (2, 100)
(* Unknown colors go last *)

(* Helper function to extract color order with shade for utilities like
   bg-blue-500, text-red-400, etc. *)
let suborder_with_shade color_part =
  (* A shadeless name like [black] or [white] has no trailing [-<digits>], and
     [utilities_order] answers for any name, so there is nothing here that can
     fail. *)
  let shadeless () =
    let _, color_order = utilities_order color_part in
    color_order * 1000
  in
  match String.rindex_opt color_part '-' with
  | None -> shadeless ()
  | Some last_dash -> (
      let color_name = String.sub color_part 0 last_dash in
      let shade_str =
        String.sub color_part (last_dash + 1)
          (String.length color_part - last_dash - 1)
      in
      match int_of_string_opt shade_str with
      | None -> shadeless ()
      | Some shade ->
          let _, color_order = utilities_order color_name in
          (color_order * 1000) + shade)

(* Get theme layer order for a color variable with shade. Formula: (priority=2,
   base_order * 1000 + shade) This ensures color variables are grouped by color
   with shades in ascending order. *)
let theme_order_with_shade color_name shade =
  let var_priority, base_order = theme_order color_name in
  (var_priority, base_order + shade)

(* Memoization table for color variables *)
(* Keyed by [(color, shade)] (shade normalised to 0 for shadeless colours, which
   ignore it) rather than the materialized name, so a cache hit skips building
   the name string entirely - and within one generation bg/text/border share the
   same colour, so hits happen even on a single cold run. *)
let color_var_cache = Domain_cache.v 128

(* Property-scoped colour variables (text-color-*, accent-color-*, ...) keyed by
   their full name, kept separate from the [(color, shade)] cache above. *)
let property_color_var_cache = Domain_cache.v 64

(* Helper to create a color variable with memoization. Creates theme layer
   variables with deterministic ordering based on color and shade. *)
let color_var color shade =
  let shadeless = is_shadeless color in
  let key = (color, if shadeless then 0 else shade) in
  Domain_cache.or_add color_var_cache key (fun () ->
      let base = pp color in
      (* The name goes after the [--] this module does not write itself, so it
         is a CSS name rather than a whole ident: [escape_name] is the exact
         serialisation for that position. A palette colour is already all name
         code points and comes back unchanged; an arbitrary one carries
         brackets, a [#] or parens, and every one of those has to be escaped for
         the result to lex as a single dashed ident. *)
      let escaped_base = Cascade.Parser.escape_name base in
      let name =
        if shadeless then "color-" ^ escaped_base
        else Pp.str [ "color-"; escaped_base; "-"; string_of_int shade ]
      in
      (* Create theme variable with deterministic theme layer order: - Base
         colors use theme_order(color_name) - Shaded colors use
         theme_order_with_shade(color_name, shade)

         Note: Tailwind v4 appears to order variables by first usage in the
         input, not by a fixed ordering. Our implementation uses a fixed
         ordering for determinism and consistency. *)
      let var_order =
        if shadeless then theme_order_with_shade base 0
        else theme_order_with_shade base shade
      in
      Var.theme Css.Color name ~order:var_order)

(* The theme declaration a colour token needs and the value a utility writes for
   it: the reference, with the token declared beside it, or for a token an
   [@theme inline] block declared, the value itself and no declaration. That is
   what inline is for - a value a [var()] cannot reach, inside [@keyframes] or
   composed into a [color-mix()] - so a reference with the token declared is the
   opposite of what was asked for, even where the two paint the same. *)
let bound ?theme cvar (value : Css.color) : Css.declaration list * Css.color =
  match theme with
  | Some t when Scheme.is_inline_token t (Var.name cvar) -> ([], value)
  | Some _ | None ->
      let decl, reference = Var.binding cvar value in
      ([ decl ], Var reference)

let color_to_string (c : color) : string =
  match c with
  | Black -> "black"
  | White -> "white"
  | Gray -> "gray"
  | Slate -> "slate"
  | Zinc -> "zinc"
  | Neutral -> "neutral"
  | Stone -> "stone"
  | Mauve -> "mauve"
  | Olive -> "olive"
  | Mist -> "mist"
  | Taupe -> "taupe"
  | Red -> "red"
  | Orange -> "orange"
  | Amber -> "amber"
  | Yellow -> "yellow"
  | Lime -> "lime"
  | Green -> "green"
  | Emerald -> "emerald"
  | Teal -> "teal"
  | Cyan -> "cyan"
  | Sky -> "sky"
  | Blue -> "blue"
  | Indigo -> "indigo"
  | Violet -> "violet"
  | Purple -> "purple"
  | Fuchsia -> "fuchsia"
  | Pink -> "pink"
  | Rose -> "rose"
  | Hex h ->
      (* Hex values stored without # by shorten_hex_str, add it back for class
         name *)
      let h_with_hash =
        if String.starts_with ~prefix:"#" h then h else "#" ^ h
      in
      Css.Pp.to_string ~minify:false
        (fun ctx s ->
          Css.Pp.string ctx "[";
          Css.Pp.string ctx s;
          Css.Pp.string ctx "]")
        h_with_hash
  | Rgb { red; green; blue } ->
      Css.Pp.to_string ~minify:false
        (fun ctx (r, g, b) ->
          Css.Pp.string ctx "[rgb(";
          Css.Pp.int ctx r;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx g;
          Css.Pp.string ctx ",";
          Css.Pp.int ctx b;
          Css.Pp.string ctx ")]")
        (red, green, blue)
  | Oklch oklch ->
      Css.Pp.to_string ~minify:false
        (fun ctx o ->
          Css.Pp.string ctx "[oklch(";
          Css.Pp.float ctx o.l;
          Css.Pp.string ctx "%,";
          Css.Pp.float ctx o.c;
          Css.Pp.string ctx ",";
          Css.Pp.float ctx o.h;
          Css.Pp.string ctx ")]")
        oklch
  | Css Css.Transparent -> "transparent"
  | Css Css.Inherit -> "inherit"
  | Css c ->
      (* Only spaces map to [_], as in [to_name]. *)
      let s = Css.Pp.to_string ~minify:true Css.pp_color c in
      let s = String.map (fun ch -> if ch = ' ' then '_' else ch) s in
      "[" ^ s ^ "]"
  | Theme_named name -> name

(* The named palette colours read a [--color-<name>] token, shaded or not; the
   arbitrary spellings carry their own value and read nothing, and a
   [Theme_named] one is looked up against the theme already. *)
let palette_token color shade =
  match color with
  | Hex _ | Rgb _ | Oklch _ | Css _ | Theme_named _ -> None
  | _ ->
      let base = pp color in
      if is_shadeless color then Some ("color-" ^ base)
      else Some (Pp.str [ "color-"; base; "-"; string_of_int shade ])

(* A [@theme] block that removed the token a palette colour reads leaves the
   utility painting a variable nothing declares, so the colour stops resolving
   the way a project colour the block never declared does. *)
let palette_is_declared theme color shade =
  match (theme, palette_token color shade) with
  | Some scheme, Some token -> not (Scheme.is_removed scheme token)
  | (Some _ | None), _ -> true

(* Parse color and shade from string list. A name the palette does not know is
   still a colour when the [\@theme] block declared [--color-<name>]; such a
   token carries no shade, and its name may span several segments. A palette
   family on its own is no colour either: [red] reads [--color-red], which only
   a project's [\@theme] declares, where [black] and [white] read a token of
   their own. *)
let shade_of_strings ?theme parts =
  let theme_named () =
    let name = String.concat "-" parts in
    if Parse.is_valid_theme_name name && Scheme.declares_color theme name then
      Ok (Theme_named name, 500)
    else Error (`Msg ("Invalid color: " ^ name))
  in
  match parts with
  | [ color_str; shade_str ] -> (
      match of_string color_str with
      | Ok color -> (
          match Parse.decimal_int shade_str with
          | Some shade
            when shade >= 0
                 && (not (is_shadeless color))
                 && is_valid_shade color shade
                 && palette_is_declared theme color shade ->
              Ok (color, shade)
          | _ -> theme_named ())
      | Error _ -> theme_named ())
  | [ color_str ] -> (
      match of_string color_str with
      | Ok color when is_shadeless color && palette_is_declared theme color 500
        ->
          Ok (color, 500)
      | Ok _ | Error _ -> theme_named ())
  | [] -> Error (`Msg "No color specified")
  | _ -> theme_named ()

let token_value ?theme (c : color) shade =
  to_css ?theme c (if is_base_color c then 500 else shade)

(* The theme-layer declaration for a palette colour together with the typed
   reference to it, so a utility outside this module can both emit the token and
   point at it. *)
let color_binding ?theme c shade =
  Var.binding (color_var c shade) (token_value ?theme c shade)

let bound_color ?theme ~shade color =
  let value = to_css ?theme color shade in
  if is_custom_color color then ([], value)
  else bound ?theme (color_var color shade) value

(* [\@import "tailwindcss" theme(static)] emits the whole theme rather than only
   the tokens a utility used, so the sheet needs every palette colour.
   [transparent] is not one: Tailwind declares no token for it. *)
let all_palette_declarations ?theme () =
  List.concat_map
    (fun name ->
      let c = of_string_exn name in
      let shades = if is_base_color c then [ 500 ] else Palette.shades in
      List.map (fun sh -> fst (color_binding ?theme c sh)) shades)
    Palette.names

(* The palette colour a [--color-*] token names, so a value that references the
   token can be rendered from the palette. *)
let theme_color_of_name name =
  if not (String.starts_with ~prefix:"color-" name && String.length name > 6)
  then None
  else
    let rest = String.sub name 6 (String.length name - 6) in
    match shade_of_strings (String.split_on_char '-' rest) with
    | Ok (c, shade) when not (is_custom_color c) -> Some (c, shade)
    | _ -> None

(* The theme-layer declaration for a colour token named like "color-red-500", or
   None when the name is not a catalogued colour token: a palette colour, or one
   the scheme's [colors] names. [color_var] registers the token's canonical
   order and [token_value] supplies the typed value, so the result matches what
   a colour utility would emit. Used to emit tokens that arbitrary values
   reference via var() but that no colour utility set. *)
let theme_color_decl ?theme name =
  let decl c shade =
    Some (Var.set (color_var c shade) (token_value ?theme c shade))
  in
  match theme_color_of_name name with
  | Some (c, shade) -> decl c shade
  | None when String.starts_with ~prefix:"color-" name -> (
      let rest = String.sub name 6 (String.length name - 6) in
      match Option.bind theme (fun t -> Scheme.color t rest) with
      | Some _ -> decl (Theme_named rest) 500
      | None -> None)
  | None -> None

(** Get a color variable for a property. Checks if a property-scoped theme value
    exists (e.g., [--accent-color-blue-500]) and if so creates a property-scoped
    variable. Otherwise falls back to the generic [--color-{name}] variable. *)
let property_color_var ?theme ~property_prefix (c : color) shade =
  let color_name = scheme_color_name c shade in
  let prop_name = property_prefix ^ "-" ^ color_name in
  match Scheme.theme_value theme prop_name with
  | Some _ ->
      (* Property-scoped theme value exists, create scoped variable *)
      let name = prop_name in
      Domain_cache.or_add property_color_var_cache name (fun () ->
          let base = pp c in
          let var_order =
            if is_shadeless c then theme_order_with_shade base 0
            else theme_order_with_shade base shade
          in
          Var.theme Css.Color name ~order:var_order)
  | None ->
      (* Fall back to generic --color-{name} *)
      color_var c shade

(** Get the color value for use with color variables. Checks for property-scoped
    theme value first, then scheme, then generic theme value, then the palette.
*)
let property_color_value ?theme ~property_prefix (c : color) shade =
  let color_name = scheme_color_name c shade in
  let prop_name = property_prefix ^ "-" ^ color_name in
  let parse_theme_color value =
    match Css.parse_color value with
    | Some color -> color
    | None -> invalid_arg ("Invalid theme colour: " ^ value)
  in
  match Scheme.theme_value theme prop_name with
  | Some value -> parse_theme_color value
  | None -> (
      let shade = if is_base_color c then 500 else shade in
      match scheme_color ?theme c shade with
      | Some value -> value
      | None -> (
          (* Check theme value overrides for standard color name *)
          let std_name = "color-" ^ color_name in
          match Scheme.theme_value theme std_name with
          | Some value -> parse_theme_color value
          | None -> palette_css ?theme c shade))

(* Tailwind's [--alpha(<color>/<pct>)] inside an arbitrary value is
   [color-mix(in oklab, <color> <pct>, transparent)]. *)

(** Parse a bracket inner string into a typed [Css.color], if it represents a
    valid color. Handles hex strings (with [#] prefix), CSS color functions like
    [rgb(...)], [hsl(...)], etc., and named Tailwind colors (which are converted
    to their CSS representation via [to_css]). Returns [None] for non-color
    values. *)
let parse_bracket_color (inner : string) : Css.color option =
  if Parse.is_var inner then
    (* A bare var() is a valid arbitrary color: border-[var(--x)] and its paren
       shorthand border-(--x). *)
    Some (Css.Var (Var.bracket (Parse.extract_var_name inner)))
  else
    match Parse.alpha_call inner with
    | Some _ ->
        (* The call is the whole value: the [color-mix()] it expands to, read as
           any colour is, with the colour itself at a whole alpha. The inner
           colour is a raw CSS colour: [red] is the keyword, not the red-500
           palette entry. *)
        Css.parse_color (Parse.decode_arbitrary_value inner)
    | None ->
        (* A [#] prefix only names a colour when what follows is a hex spelling,
           so this reads the digits rather than raising on them inside
           [of_class]. Reading them through the parser is what keeps the
           spelling the bracket wrote, which is the one Tailwind emits. *)
        let starts_with_hash = String.length inner > 0 && inner.[0] = '#' in
        if starts_with_hash then
          match Css.hex_opt inner with
          | Some _ -> Some (authored_hex inner)
          | None -> None
        else
          (* A bracket holds CSS, so only a colour CSS knows is one: keywords
             and system colours included, [[Field]] and [[light-dark(a,b)]] are
             values. A palette name is not; Tailwind writes [[emerald]] through
             as the identifier it is, which browsers drop, where reading it as
             the 500 shade painted a colour the page does not have. *)
          Css.parse_color (Parse.decode_underscores inner)

(* What a bracket value names, once the [color:]/[var(] spellings are told apart
   from a plain colour. Every colour-bearing utility (text, outline, ring,
   shadow, fill, stroke, ...) reads the same bracket this way; only the variant
   it stores the result in differs. *)
type bracket_hint =
  | Typed_var of string (* [color:var(--x)], the var() text *)
  | Bare_var of string (* [var(--x)], the full "var(...)" text *)
  | Plain_color of Css.color (* any other colour spelling *)

let parse_bracket_hint inner =
  if String.starts_with ~prefix:"color:" inner then
    let value = String.sub inner 6 (String.length inner - 6) in
    (* [color:] says how to read what follows it and nothing more. Only a var()
       reference names a custom property; every other spelling is the colour
       itself, so [color:red] is the colour red. *)
    if Parse.is_var value then Some (Typed_var value)
    else
      match parse_bracket_color value with
      | Some c -> Some (Plain_color c)
      | None -> None
  else if String.starts_with ~prefix:"var(" inner then Some (Bare_var inner)
  else
    match parse_bracket_color inner with
    | Some c -> Some (Plain_color c)
    | None -> None

let is_bracket_color value =
  Parse.is_var value || parse_bracket_color value <> None

let bracket_color_hint inner =
  match Parse.data_type_hint inner with
  | Some ("color", v) when is_bracket_color v -> Some v
  | Some _ | None -> None

(* The registry holds the tokens a theme declares; the palette is a table of its
   own, so a colour token is spelled out from it. A removed token binds nothing,
   whichever table would have answered. *)
let theme_token theme name =
  match Scheme.token theme name with
  | Some _ as value -> value
  | None when Scheme.is_removed theme name -> None
  | None ->
      Option.map
        (fun (c, shade) ->
          Css.Pp.to_string Css.pp_color (token_value ~theme c shade))
        (theme_color_of_name name)
