(** Colours under an opacity modifier. *)

module Css = Cascade.Css
open Srgb
open Color_value

(* A custom colour (a hex or an rgb() the author wrote) with an alpha folded in.
   Tailwind writes the oklab form, with [none] for a channel that is zero. *)
let custom_color_with_alpha (c : color) alpha =
  let ok_l, ok_a, ok_b =
    match c with
    | Hex h -> (
        match hex_to_rgb h with
        | Some rgb -> rgb_to_oklab rgb
        | None -> (0.0, 0.0, 0.0))
    | Rgb { red; green; blue } -> rgb_to_oklab { r = red; g = green; b = blue }
    | _ -> (0.0, 0.0, 0.0)
  in
  Css.oklaba_none_zeros ok_l ok_a ok_b alpha

(* A number in an opacity modifier, with the digits the author wrote beside the
   value they denote. The class name is a selector, so it has to repeat the
   spelling: [/[25]] and [/[25.0]] are one alpha and two different classes, and
   re-printing the float picks the wrong one. *)
type opacity_number = { value : float; text : string }

(** Opacity modifier type *)
type opacity_modifier =
  | No_opacity
  | Opacity_percent of opacity_number (* e.g., /50 means 50% *)
  | Opacity_arbitrary of opacity_number (* e.g., /[0.5] means 0.5 *)
  | Opacity_bracket_percent of
      opacity_number (* e.g., /[50%] means 50% but preserves bracket form *)
  | Opacity_named of string (* e.g., /half, /custom - theme-defined names *)
  | Opacity_var of string
(* the modifier as written: /[var(--x)] or the /(--x) shorthand *)

let opacity_of_int pct =
  Opacity_percent { value = Float.of_int pct; text = string_of_int pct }

(* The class-name spelling of a modifier, without the leading [/]. *)
let pp_opacity = function
  | No_opacity -> ""
  | Opacity_percent n -> n.text
  | Opacity_bracket_percent n -> "[" ^ n.text ^ "%]"
  | Opacity_arbitrary n -> "[" ^ n.text ^ "]"
  | Opacity_named name -> name
  | Opacity_var v -> v

let opacity_suffix = function No_opacity -> "" | o -> "/" ^ pp_opacity o

(** Parse the modifier that follows the [/] in a colour class. *)
let opacity_of_string ?theme opacity_str =
  if
    String.length opacity_str > 2
    && opacity_str.[0] = '['
    && opacity_str.[String.length opacity_str - 1] = ']'
  then
    (* Arbitrary value like [0.5] or [50%] or [var(--x)] *)
    let inner = String.sub opacity_str 1 (String.length opacity_str - 2) in
    if String.ends_with ~suffix:"%" inner then
      let num_str = String.sub inner 0 (String.length inner - 1) in
      match float_of_string_opt num_str with
      | Some f -> Some (Opacity_bracket_percent { value = f; text = num_str })
      | None -> None
    else
      match float_of_string_opt inner with
      | Some f -> Some (Opacity_arbitrary { value = f; text = inner })
      | None ->
          if Parse.is_var inner then Some (Opacity_var opacity_str) else None
  else if
    String.length opacity_str > 4
    && opacity_str.[0] = '('
    && opacity_str.[String.length opacity_str - 1] = ')'
  then
    (* The [(--x)] shorthand for [[var(--x)]]. *)
    let inner = String.sub opacity_str 1 (String.length opacity_str - 2) in
    if String.starts_with ~prefix:"--" inner && String.length inner > 2 then
      Some (Opacity_var opacity_str)
    else None
  else
    (* Numeric value like 50 or 2.5, or named opacity like half/custom *)
    match Parse.decimal_float opacity_str with
    | Some f when f >= 0. ->
        Some (Opacity_percent { value = f; text = opacity_str })
    | _ ->
        (* Not a number — check if it's a named opacity defined in the theme
           (e.g., /half when --opacity-half exists) *)
        if
          Parse.is_valid_theme_name opacity_str
          && Scheme.theme_value theme ("opacity-" ^ opacity_str) <> None
        then Some (Opacity_named opacity_str)
        else None

(* The [/] a modifier follows: the last one outside a bracket or a paren, so the
   [/] a bracket value spells, as [rgb(0 0 0 / 0.1)] does, stays inside. *)

(** Parse opacity modifier from a string that may contain /NN or /[N.N] *)
let modifier_slash s =
  let last = ref None in
  let depth = ref 0 in
  String.iteri
    (fun i c ->
      match c with
      | '[' | '(' -> incr depth
      | ']' | ')' -> if !depth > 0 then decr depth
      | '/' when !depth = 0 -> last := Some i
      | _ -> ())
    s;
  !last

let has_opacity s = String.contains s '/'

let parse_opacity_modifier ?theme s =
  match modifier_slash s with
  | None -> (s, No_opacity)
  | Some idx -> (
      let base = String.sub s 0 idx in
      let opacity_str = String.sub s (idx + 1) (String.length s - idx - 1) in
      match opacity_of_string ?theme opacity_str with
      | Some opacity -> (base, opacity)
      | None -> (s, No_opacity))

(* Parse color, shade, and optional opacity modifier from string list. Handles
   formats like ["red"; "500/50"] or ["red"; "500/[0.5]"] *)
let shade_and_opacity_of_strings ?theme parts =
  (* The modifier rides on the last segment however long the name: a project
     token spelled [brand-primary/50] splits into ["brand"; "primary/50"]. *)
  let theme_named () =
    match List.rev parts with
    | [] -> Error (`Msg "No color specified")
    | last :: front ->
        let base, opacity = parse_opacity_modifier ?theme last in
        let name = String.concat "-" (List.rev (base :: front)) in
        if Parse.is_valid_theme_name name && Scheme.declares_color theme name
        then Ok (Theme_named name, 500, opacity)
        else Error (`Msg ("Invalid color: " ^ name))
  in
  match parts with
  | [ color_str; shade_opacity_str ] -> (
      let shade_str, opacity =
        parse_opacity_modifier ?theme shade_opacity_str
      in
      match of_string color_str with
      | Ok color -> (
          match Parse.decimal_int shade_str with
          | Some shade
            when shade >= 0
                 && (not (is_shadeless color))
                 && is_valid_shade color shade
                 && palette_is_declared theme color shade ->
              Ok (color, shade, opacity)
          | _ -> theme_named ())
      | Error _ -> theme_named ())
  | [ color_str ] -> (
      (* Could be "current/50" or just "black" *)
      let base_str, opacity = parse_opacity_modifier ?theme color_str in
      let keyword =
        match (base_str, opacity) with
        | ( "transparent",
            ( Opacity_percent _ | Opacity_arbitrary _
            | Opacity_bracket_percent _ | Opacity_named _ | Opacity_var _ ) ) ->
            Some Css.Transparent
        | ( "inherit",
            ( Opacity_percent _ | Opacity_arbitrary _
            | Opacity_bracket_percent _ | Opacity_named _ | Opacity_var _ ) ) ->
            Some Css.Inherit
        | _ -> None
      in
      match keyword with
      | Some keyword -> Ok (Css keyword, 500, opacity)
      | None -> (
          match of_string base_str with
          | Ok color
            when is_shadeless color && palette_is_declared theme color 500 ->
              Ok (color, 500, opacity)
          | Ok _ | Error _ -> theme_named ()))
  | [] -> Error (`Msg "No color specified")
  | _ -> theme_named ()

(** Convert opacity modifier to a percentage value (0-100) *)
let opacity_to_percent = function
  | No_opacity -> 100.0
  | Opacity_percent p -> p.value (* Already a percentage like 50 *)
  | Opacity_bracket_percent p -> p.value (* [50%] is also a percentage *)
  | Opacity_arbitrary f -> f.value *. 100.0 (* e.g., 0.5 -> 50 *)
  | Opacity_named _ | Opacity_var _ ->
      (* Named/var opacity requires variable lookup, default to 100% *)
      100.0

(* [opacity_to_percent] answers 100 for an opacity it cannot resolve (a var or
   a theme name), so a caller cannot use that to mean "fully opaque". *)
(* The bare custom-property name inside an opacity modifier, written either
   as [[var(--x)]] or as the [(--x)] shorthand. *)
let opacity_var_bare v =
  let inner =
    if Parse.is_bracket_value v then Parse.bracket_inner v
    else if String.length v > 2 && v.[0] = '(' then
      String.sub v 1 (String.length v - 2)
    else v
  in
  let name = Parse.extract_var_name inner in
  Option.value ~default:name (Parse.bare_name name)

(* The custom property an opacity modifier reads its percentage from, when it
   names one rather than giving a number. *)
let opacity_var_bare_of = function
  | Opacity_var v -> Some (opacity_var_bare v)
  | Opacity_named name -> Some ("opacity-" ^ Parse.extract_var_name name)
  | _ -> None

let is_fully_opaque = function
  | Opacity_named _ | Opacity_var _ -> false
  | opacity -> opacity_to_percent opacity >= 100.

(* The colour with the modifier's alpha applied: a percentage folds in, an alpha
   read from a var is referenced by name. *)
let mix_alpha ?(in_space : Css.color_space = Css.Oklab) opacity color =
  match opacity_var_bare_of opacity with
  | Some var_name ->
      Css.color_mix_var_percent ~in_space ~var_name color Css.Transparent
  | None ->
      Css.color_mix ~in_space color Css.Transparent
        ~percent1:(opacity_to_percent opacity)

(* A colour keyword takes the modifier's alpha through the same mix as any other
   colour. At full opacity the mix is a no-op and Tailwind writes the keyword
   itself. *)
let apply_alpha ?(in_space : Css.color_space = Css.Oklab) opacity color =
  if is_fully_opaque opacity then color else mix_alpha ~in_space opacity color

(** Condition for progressive enhancement with color-mix in oklab *)
let color_mix_supports_condition =
  Css.Supports.property "color" "color-mix(in lab, red, red)"

(* [decls] on the utility's own class, behind the [color-mix()] guard. *)
let color_mix_supports decls =
  Css.supports ~condition:color_mix_supports_condition
    [ Css.rule ~selector:(Css.Selector.class_ "_") decls ]

(* What a browser without [color-mix()] reads. A palette colour converts to a
   plain hex carrying the alpha. A project token has no such conversion - its
   value is whatever the [\@theme] block bound it to, and that may be a colour
   space no hex can spell - so it takes the sRGB mix Tailwind writes instead.
   [value] is the colour already resolved against the theme. *)
let opacity_fallback ?theme ~percent c shade value =
  let overridden =
    match palette_token c shade with
    | Some token -> Scheme.theme_value theme token <> None
    | None -> false
  in
  match (overridden, to_oklch_opt c shade) with
  | false, Some oklch ->
      Css.hex (hex_with_alpha (rgb_to_hex (oklch_to_rgb oklch)) percent)
  | true, _ | false, None ->
      Css.color_mix ~in_space:Css.Srgb value Css.Transparent ~percent1:percent

(* Resolve the colour tokens inside an arbitrary [color-mix()] through the
   render's scheme. Tailwind emits that resolved mix in sRGB as the fallback,
   then keeps the authored mix behind a [color-mix()] support query. When a
   token is unavailable at compile time (including a token removed by the
   project), the first operand is the legacy fallback instead. *)
let theme_var_color theme name =
  match Option.bind (Scheme.token theme name) Css.parse_color with
  | Some _ as color -> color
  | None when Scheme.is_removed theme name -> None
  | None ->
      Option.map
        (fun (c, shade) -> token_value ~theme c shade)
        (theme_color_of_name name)

type mix_resolution = {
  color : Css.color;
  has_dynamic_color : bool;
  unresolved : bool;
}

let static color = { color; has_dynamic_color = false; unresolved = false }

let fallback_space (space : Css.color_space option) =
  match space with
  | Some (Css.Lab | Css.Oklab | Css.Lch | Css.Oklch) -> Some Css.Srgb
  | space -> space

(* A percentage read from a custom property makes the mix dynamic the way a
   colour read from one does: a theme token resolves to the percentage it binds,
   anything else leaves the mix unresolved. The colour carried back is only the
   resolution's marker; the percentage is the first half. *)
let resolve_percentage_theme_var theme (percent : Css.percentage option) =
  let marker ~unresolved =
    { color = Css.Transparent; has_dynamic_color = true; unresolved }
  in
  match percent with
  | Some (Css.Var v) -> (
      let name = Option.value ~default:v.name (Parse.bare_name v.name) in
      let value = Option.map String.trim (Scheme.token theme name) in
      match value with
      | Some p
        when String.ends_with ~suffix:"%" p
             && Option.is_some
                  (float_of_string_opt (String.sub p 0 (String.length p - 1)))
        ->
          let p = float_of_string (String.sub p 0 (String.length p - 1)) in
          (Some (Css.Pct p : Css.percentage), marker ~unresolved:false)
      | Some _ | None -> (percent, marker ~unresolved:true))
  | percent -> (percent, static Css.Transparent)

let rec resolve_color_theme_vars theme seen (color : Css.color) =
  match color with
  | Css.Var v -> (
      if List.mem v.name seen then
        { color; has_dynamic_color = true; unresolved = true }
      else
        match theme_var_color theme v.name with
        | None -> { color; has_dynamic_color = true; unresolved = true }
        | Some value ->
            let resolved =
              resolve_color_theme_vars theme (v.name :: seen) value
            in
            { resolved with has_dynamic_color = true })
  | Css.Current -> { color; has_dynamic_color = true; unresolved = true }
  | Css.Mix { in_space; hue; color1; percent1; color2; percent2 } ->
      let first = resolve_color_theme_vars theme seen color1 in
      let second = resolve_color_theme_vars theme seen color2 in
      let percent1, p1 = resolve_percentage_theme_var theme percent1 in
      let percent2, p2 = resolve_percentage_theme_var theme percent2 in
      let has_dynamic_color =
        first.has_dynamic_color || second.has_dynamic_color
        || p1.has_dynamic_color || p2.has_dynamic_color
      in
      if not has_dynamic_color then static color
      else if
        first.unresolved || second.unresolved || p1.unresolved || p2.unresolved
      then
        (* A browser without [color-mix()] can still use the first operand. Keep
           [unresolved] set so an enclosing mix follows the same rule. *)
        { first with has_dynamic_color = true; unresolved = true }
      else
        {
          color =
            Css.Mix
              {
                in_space = fallback_space in_space;
                hue;
                color1 = first.color;
                percent1;
                color2 = second.color;
                percent2;
              };
          has_dynamic_color = true;
          unresolved = false;
        }
  | color -> static color

let pre_color_mix_fallback theme (color : Css.color) =
  match color with
  | Css.Mix _ ->
      let resolved = resolve_color_theme_vars theme [] color in
      if resolved.has_dynamic_color then Some resolved.color else None
  | _ -> None

let bracket_colors_style ?merge_key ~theme ~properties css_color =
  let declarations color =
    List.map (fun property -> property color) properties
  in
  let color = resolve_bracket_css_color css_color in
  match pre_color_mix_fallback theme color with
  | None -> Style.style ?merge_key (declarations color)
  | Some fallback ->
      let supports_block = color_mix_supports (declarations color) in
      Style.style ?merge_key ~rules:(Some [ supports_block ])
        (declarations fallback)

let bracket_color_style ?merge_key ~theme ~property css_color =
  bracket_colors_style ?merge_key ~theme ~properties:[ property ] css_color

(* A colour the scheme spells as a hex: that hex carrying the alpha in the open,
   and the token mixed behind the [color-mix()] guard. *)
let scheme_hex_opacity_style ?theme ?merge_key ~property_decls c shade opacity
    hex_value =
  let percent = opacity_to_percent opacity in
  (* Scheme has hex color: use hex+alpha fallback with top-level @supports *)
  let hex_with_alpha = hex_with_alpha hex_value percent in
  let fallback_decls = property_decls (Css.hex hex_with_alpha) in
  (* Theme declaration for the variable *)
  let decls, color = bound ?theme (color_var c shade) (Css.hex hex_value) in
  (* Progressive enhancement: color-mix(in oklab, var(--color-X) NN%,
     transparent) *)
  let oklab_color = mix_alpha opacity color in
  (* Create @supports block with oklab version as top-level rule. Use
     placeholder selector that rule.ml replaces with actual class. *)
  let supports_block = color_mix_supports (property_decls oklab_color) in
  Style.style ?merge_key ~rules:(Some [ supports_block ])
    (decls @ fallback_decls)

(* A colour read through its token, a property-scoped one when [property_prefix]
   names it: the [opacity_fallback] in the open, and the token mixed behind the
   [color-mix()] guard. *)
let token_opacity_style ?theme ?property_prefix ?merge_key ~property_decls c
    shade opacity =
  let percent = opacity_to_percent opacity in
  (* Non-scheme color: use property-scoped variable if prefix given *)
  let color_var =
    match property_prefix with
    | Some prefix -> property_color_var ?theme ~property_prefix:prefix c shade
    | None -> color_var c shade
  in
  let color_value =
    match property_prefix with
    | Some prefix -> property_color_value ?theme ~property_prefix:prefix c shade
    | None -> to_css ?theme c (if is_base_color c then 500 else shade)
  in
  let decls, color = bound ?theme color_var color_value in
  (* An opacity read from a var has no percentage to fold into a hex fallback,
     so the fallback is the colour at full opacity. *)
  let fallback_decls =
    match opacity_var_bare_of opacity with
    | Some _ -> property_decls color_value
    | None ->
        property_decls (opacity_fallback ?theme ~percent c shade color_value)
  in
  let oklab_color =
    match opacity_var_bare_of opacity with
    | Some var_name ->
        Css.color_mix_var_percent ~in_space:Css.Oklab ~var_name color
          Css.Transparent
    | None ->
        Css.color_mix ~in_space:Css.Oklab color Css.Transparent
          ~percent1:percent
  in
  let supports_block = color_mix_supports (property_decls oklab_color) in
  Style.style ?merge_key ~rules:(Some [ supports_block ])
    (decls @ fallback_decls)

(* The same colour set on several properties at once: a per-side border colour
   on the [x]/[y] axes needs two. *)

(** Generate color with opacity using progressive enhancement. Output depends on
    scheme:
    - With hex scheme: fallback is hex+alpha, [\@supports] has color-mix
    - With oklch scheme (default): fallback is color-mix(srgb), [\@supports] has
      color-mix(oklab) *)
let colors_with_opacity_style ?theme ~properties ?property_prefix ?merge_key c
    shade opacity =
  let property_decls value = List.map (fun set -> set value) properties in
  let percent = opacity_to_percent opacity in
  match opacity_keyword c with
  | Some keyword ->
      Style.style ?merge_key (property_decls (apply_alpha opacity keyword))
  | None when is_fully_opaque opacity && not (is_custom_color c) ->
      (* At 100% the mix is a no-op, so Tailwind writes the colour itself and
         needs neither the fallback nor the [@supports] pair. *)
      let decls, value = bound ?theme (color_var c shade) (to_css c shade) in
      Style.style ?merge_key (decls @ property_decls value)
  | None when is_custom_color c && opacity_var_bare_of opacity <> None ->
      Style.style ?merge_key
        (property_decls (mix_alpha opacity (to_css c shade)))
  | None when is_custom_color c ->
      (* Custom/arbitrary colors (hex, rgb): output oklab() directly. No theme
         variables, no @supports, no hex+alpha fallback. *)
      Style.style ?merge_key
        (property_decls (custom_color_with_alpha c (percent /. 100.0)))
  | None -> (
      let scheme = Scheme.or_default theme in
      let color_name = scheme_color_name c shade in
      (* Check if color is defined as hex in the scheme *)
      match Scheme.hex_color scheme color_name with
      | Some hex_value ->
          scheme_hex_opacity_style ?theme ?merge_key ~property_decls c shade
            opacity hex_value
      | None ->
          token_opacity_style ?theme ?property_prefix ?merge_key ~property_decls
            c shade opacity)

let color_with_opacity_style ?theme ~property ?property_prefix ?merge_key c
    shade opacity =
  colors_with_opacity_style ?theme ~properties:[ property ] ?property_prefix
    ?merge_key c shade opacity

(* What an opacity modifier makes of a bracket colour. [Folded] is the single
   value a modifier a browser can read on its own resolves to. [Guarded] is the
   pair the rest need: the colour itself, for a browser with no [color-mix()],
   and the mix behind an [\@supports] guard. The properties the two land on are
   the caller's, which is why this answers values rather than declarations - a
   decoration colour writes a vendor prefix alongside, and a divide colour hangs
   both on its child selector. *)
type bracket_opacity =
  | Folded of Css.color
  | Guarded of { fallback : Css.color; mixed : Css.color }

(* A bracket colour arrives already parsed into a typed [Css.color], and an
   opacity modifier applies to that value. Reading the bracket text back through
   the palette parser answered black for every colour the palette does not name.

   The modifier stays a [color-mix()] wherever Tailwind writes one: the
   bracket's contents are emitted as authored, so evaluating the mix would
   replace the colour the class named with a computed [oklab()] no class spells.
   Only a fully opaque modifier resolves, to the colour itself. *)
let bracket_color_opacity ?(theme = Scheme.default) css_color opacity =
  let base = resolve_bracket_css_color css_color in
  if is_fully_opaque opacity then Folded base
  else
    let mixed = mix_alpha opacity base in
    match pre_color_mix_fallback theme mixed with
    | Some fallback -> Guarded { fallback; mixed }
    | None when opacity_var_bare_of opacity <> None ->
        (* The mix reads its percentage from a custom property, which
           [pre_color_mix_fallback] does not look at: it answers on the colour
           operands alone. Without the pair a browser with no [color-mix()]
           drops the declaration and paints nothing, where Tailwind paints the
           bracket colour. *)
        Guarded { fallback = base; mixed }
    | None -> Folded mixed

(* The same modifier on a bracket colour set on several properties at once,
   which a per-side border colour needs. *)
let bracket_colors_opacity_style ?(theme = Scheme.default) ?merge_key
    ~properties css_color opacity =
  let declarations c = List.map (fun property -> property c) properties in
  match bracket_color_opacity ~theme css_color opacity with
  | Folded value -> Style.style ?merge_key (declarations value)
  | Guarded { fallback; mixed } ->
      let supports_block = color_mix_supports (declarations mixed) in
      Style.style ?merge_key ~rules:(Some [ supports_block ])
        (declarations fallback)

let bracket_color_opacity_style ?theme ?merge_key ~property css_color opacity =
  bracket_colors_opacity_style ?theme ?merge_key ~properties:[ property ]
    css_color opacity

let hex_alpha_color ?theme c shade opacity =
  let percent = opacity_to_percent opacity in
  let color_name = scheme_color_name c shade in
  match Scheme.hex_color (Scheme.or_default theme) color_name with
  | Some hex_value -> Some (hex_with_alpha hex_value percent)
  | None ->
      (* Shadeless base colours (black/white) have no scheme entry but a known
         hex, so an /opacity modifier still resolves to a colour. *)
      if is_base_color c then
        Some (hex_with_alpha (to_oklch_css c shade) percent)
      else if is_theme_named c then
        (* A project token is whatever CSS the block wrote: a hex takes the
           alpha; anything else has no compile-time fallback to fold it into. *)
        match Cascade.Values.nonkeyword_color (to_css ?theme c shade) with
        | Css.Hex { r; g; b; _ } | Css.Authored_hex { r; g; b; _ } ->
            Some (hex_with_alpha (rgb_to_hex { r; g; b }) percent)
        | _ -> None
      else
        (* A theme that binds palette colours to var references has no scheme
           hex; convert through oklch so the /opacity modifier still resolves,
           as [color_with_opacity_style] does for bg/text/border. *)
        let hex_value = rgb_to_hex (oklch_to_rgb (to_oklch c shade)) in
        Some (hex_with_alpha hex_value percent)

(** {1 Color with Opacity Helpers}

    Generic helpers for scheme-aware color generation with progressive
    enhancement. These can be used by other modules (svg, divide) to avoid code
    duplication. *)

let custom_color_to_oklab c =
  match c with
  | Hex h -> (
      match hex_to_rgb h with
      | Some rgb -> rgb_to_oklab rgb
      | None -> (0.0, 0.0, 0.0))
  | Rgb { red; green; blue } -> rgb_to_oklab { r = red; g = green; b = blue }
  | _ -> (0.0, 0.0, 0.0)

let color_mix_supports_stmts ~stmts =
  Css.supports ~condition:color_mix_supports_condition stmts

let opacity_percentage opacity : Css.percentage =
  match opacity_var_bare_of opacity with
  | Some name -> Var (Var.bracket name)
  | None -> Pct (opacity_to_percent opacity)

(* A bracket alpha with no [%] records the author's own number, unscaled and
   without a unit, which is what Tailwind writes: [shadow-lg/[25]] gives
   [--tw-shadow-alpha: 25]. Every other spelling is a percentage. *)
let opacity_alpha_value opacity : Css.percentage =
  match opacity with
  | Opacity_arbitrary n -> Num n.value
  | _ -> opacity_percentage opacity

let relative_color_supports =
  Css.Supports.property "color" "lab(from red l a b)"

(* Tailwind's [oklab(from <colour> l a b / <alpha>)]. A relative colour carries
   its channel expression verbatim, in the spelling the reader normalises to -
   no space around the slash - so the alpha is printed into it. *)
let relative_alpha opacity (c : Css.color) : Css.color =
  let alpha =
    Css.Pp.to_string ~minify:true
      (Css.Values.pp_percentage ~always:true)
      (opacity_percentage opacity)
  in
  Css.Relative_color ("oklab", c, "l a b/" ^ alpha)

(* A shadow list under an opacity modifier, as Tailwind's
   [alphaReplacedShadowProperties] writes it. Each layer's colour takes the
   alpha through [oklab(from <colour> l a b / <alpha>)], folded here when the
   channels are named and the alpha is a number, as Tailwind's minifier folds
   it. A [currentcolor] layer takes it through a [color-mix()] instead, behind
   the guard for that. A [var()] colour, or an alpha read from a custom
   property, is a value only the browser can resolve: every layer then keeps its
   authored colour in the open and takes the alpha behind the relative colour
   guard, with the [color-mix()] guard nested inside it for a [currentcolor]
   layer. *)
let shadow_alpha_decls opacity ~colours ~rebuild =
  let dynamic = opacity_var_bare_of opacity <> None in
  let is_current (c : Css.color) = c = Css.Current in
  let is_var (c : Css.color) = match c with Css.Var _ -> true | _ -> false in
  (* A [currentcolor] layer never asks for the relative guard: it takes a
     dynamic alpha through its [color-mix()] as well. *)
  let requires_fallback =
    List.exists is_var colours
    || (dynamic && List.exists (fun c -> not (is_current c)) colours)
  in
  let has_current = List.exists is_current colours in
  let alpha_of c =
    if dynamic then relative_alpha opacity c
    else
      match oklab_alpha c (opacity_to_percent opacity /. 100.) with
      | Some folded -> folded
      | None -> relative_alpha opacity c
  in
  let mixed_current = mix_alpha opacity Css.Current in
  let layers ~current ~other =
    rebuild
      (List.map (fun c -> if is_current c then current c else other c) colours)
  in
  let guard condition stmts =
    Css.supports ~condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") stmts ]
  in
  let base =
    layers ~current:Fun.id ~other:(fun c ->
        if requires_fallback then c else alpha_of c)
  in
  let mixed = layers ~current:(fun _ -> mixed_current) ~other:alpha_of in
  match (requires_fallback, has_current) with
  | false, false -> (base, None)
  | false, true -> (base, Some (guard color_mix_supports_condition [ mixed ]))
  | true, false ->
      ( base,
        Some
          (guard relative_color_supports
             [ layers ~current:Fun.id ~other:alpha_of ]) )
  | true, true ->
      ( base,
        Some
          (Css.supports ~condition:relative_color_supports
             [
               Css.rule ~selector:(Css.Selector.class_ "_")
                 [ layers ~current:Fun.id ~other:alpha_of ];
               guard color_mix_supports_condition [ mixed ];
             ]) )

let oklab_with_supports ?theme ~property ~fallback_decl c shade opacity =
  let cvar = color_var c shade in
  let color_value = to_css ?theme c (if is_base_color c then 500 else shade) in
  let decls, color = bound ?theme cvar color_value in
  let oklab_color = mix_alpha opacity color in
  let oklab_decl = property oklab_color in
  let supports_block = color_mix_supports (decls @ [ oklab_decl ]) in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

let generic_color_with_opacity ?theme ~property c shade opacity =
  let percent = opacity_to_percent opacity in
  let alpha_var = opacity_var_bare_of opacity <> None in
  match opacity_keyword c with
  | Some keyword -> Style.style [ property (apply_alpha opacity keyword) ]
  | None when is_fully_opaque opacity && not (is_custom_color c) ->
      (* At 100% the mix is a no-op, so Tailwind writes the colour itself. *)
      let decls, value =
        bound ?theme (color_var c shade) (to_css ?theme c shade)
      in
      Style.style (decls @ [ property value ])
  | None when is_custom_color c ->
      if alpha_var then
        Style.style [ property (mix_alpha opacity (to_css c shade)) ]
      else
        let ok_l, ok_a, ok_b = custom_color_to_oklab c in
        let oklab_value = Css.oklaba ok_l ok_a ok_b (percent /. 100.0) in
        Style.style [ property oklab_value ]
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (Scheme.or_default theme) color_name with
      | Some hex_value ->
          let fallback_decl =
            if alpha_var then property (Css.hex hex_value)
            else property (Css.hex (hex_with_alpha hex_value percent))
          in
          oklab_with_supports ?theme ~property ~fallback_decl c shade opacity
      | None ->
          (* A palette colour renders as its oklch node; a project token as
             whatever the theme bound it to. *)
          let base = to_css ?theme c shade in
          let fallback_color =
            if alpha_var then base
            else opacity_fallback ?theme ~percent c shade base
          in
          oklab_with_supports ?theme ~property
            ~fallback_decl:(property fallback_color) c shade opacity)

let generic_current_with_opacity ?merge_key ~fallback_decl ~property opacity =
  let oklab_color = mix_alpha opacity Css.Current in
  let oklab_decl = property oklab_color in
  let supports_block = color_mix_supports [ oklab_decl ] in
  Style.style ?merge_key ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* Fill/stroke helpers for SVG utilities *)
let fill_with_opacity ?theme c shade opacity =
  generic_color_with_opacity ?theme
    ~property:(fun color -> Css.fill (Css.Color color))
    c shade opacity

let stroke_with_opacity ?theme c shade opacity =
  generic_color_with_opacity ?theme
    ~property:(fun color -> Css.stroke (Css.Color color))
    c shade opacity

let fill_current_with_opacity opacity =
  generic_current_with_opacity ~merge_key:"fill-current"
    ~fallback_decl:(Css.fill Current_color)
    ~property:(fun color -> Css.fill (Css.Color color))
    opacity

let stroke_current_with_opacity opacity =
  generic_current_with_opacity ~merge_key:"stroke-current"
    ~fallback_decl:(Css.stroke Current_color)
    ~property:(fun color -> Css.stroke (Css.Color color))
    opacity

let divide_opacity_via_property ?theme ~selector c shade opacity =
  let cvar =
    property_color_var ?theme ~property_prefix:"border-color" c shade
  in
  let color_value =
    property_color_value ?theme ~property_prefix:"border-color" c shade
  in
  let fallback_color =
    if opacity_var_bare_of opacity <> None then color_value
    else
      opacity_fallback ?theme
        ~percent:(opacity_to_percent opacity)
        c shade color_value
  in
  let fallback_rule = Css.rule ~selector [ Css.border_color fallback_color ] in
  let decls, color = bound ?theme cvar color_value in
  let oklab_color = mix_alpha opacity color in
  let supports_rule =
    Css.rule ~selector (decls @ [ Css.border_color oklab_color ])
  in
  let supports_block = color_mix_supports_stmts ~stmts:[ supports_rule ] in
  Style.style ~rules:(Some [ fallback_rule; supports_block ]) []

let bg_opacity_via_property ?theme c shade opacity =
  let cvar = color_var c shade in
  let color_value = to_css ?theme c (if is_base_color c then 500 else shade) in
  let decls, color = bound ?theme cvar color_value in
  (* An opacity read from a var has no percentage to fold into the fallback, so
     that is the colour at full opacity, as Tailwind emits. *)
  let fallback_decl =
    match opacity_var_bare_of opacity with
    | Some _ -> Css.background_color color_value
    | None ->
        Css.background_color
          (opacity_fallback ?theme
             ~percent:(opacity_to_percent opacity)
             c shade color_value)
  in
  let oklab_decl = Css.background_color (mix_alpha opacity color) in
  let supports_block = color_mix_supports (decls @ [ oklab_decl ]) in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* Divide helpers with custom selector *)
let divide_with_opacity_selector ?theme ~selector c shade opacity =
  let percent = opacity_to_percent opacity in
  let alpha_var = opacity_var_bare_of opacity <> None in
  match opacity_keyword c with
  | Some keyword ->
      let rule =
        Css.rule ~selector [ Css.border_color (apply_alpha opacity keyword) ]
      in
      Style.style ~rules:(Some [ rule ]) []
  | None when is_fully_opaque opacity && not (is_custom_color c) ->
      let decls, value =
        bound ?theme (color_var c shade) (to_css ?theme c shade)
      in
      let rule = Css.rule ~selector [ Css.border_color value ] in
      Style.style ~rules:(Some [ rule ]) decls
  | None when is_custom_color c ->
      let value =
        if alpha_var then mix_alpha opacity (to_css c shade)
        else
          let ok_l, ok_a, ok_b = custom_color_to_oklab c in
          Css.oklaba ok_l ok_a ok_b (percent /. 100.0)
      in
      let rule = Css.rule ~selector [ Css.border_color value ] in
      Style.style ~rules:(Some [ rule ]) []
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (Scheme.or_default theme) color_name with
      | Some hex_value ->
          let hex_alpha =
            if alpha_var then hex_value else hex_with_alpha hex_value percent
          in
          let fallback_rule =
            Css.rule ~selector [ Css.border_color (Css.hex hex_alpha) ]
          in
          let cvar = color_var c shade in
          let _theme_decl, color_ref = Var.binding cvar (Css.hex hex_value) in
          let oklab_color = mix_alpha opacity (Css.Var color_ref) in
          let supports_rule =
            Css.rule ~selector [ Css.border_color oklab_color ]
          in
          let supports_block =
            color_mix_supports_stmts ~stmts:[ supports_rule ]
          in
          Style.style ~rules:(Some [ fallback_rule; supports_block ]) []
      | None -> divide_opacity_via_property ?theme ~selector c shade opacity)

let divide_with_opacity ?theme c shade opacity selector =
  divide_with_opacity_selector ?theme ~selector c shade opacity

let divide_current_with_opacity_selector ~selector opacity =
  (* Fallback: just currentColor (browsers that don't support color-mix) *)
  let fallback_rule = Css.rule ~selector [ Css.border_color Css.Current ] in
  let oklab_color = mix_alpha opacity Css.Current in
  let supports_rule = Css.rule ~selector [ Css.border_color oklab_color ] in
  let supports_block =
    Css.supports ~condition:color_mix_supports_condition [ supports_rule ]
  in
  Style.style ~rules:(Some [ fallback_rule; supports_block ]) []

let divide_current_with_opacity opacity selector =
  divide_current_with_opacity_selector ~selector opacity

(** Background color with opacity - scheme-aware. Uses hex+alpha fallback with
    theme variable in [@supports] block. *)
let bg_with_opacity ?theme c shade opacity =
  let percent = opacity_to_percent opacity in
  match opacity_keyword c with
  | Some keyword ->
      Style.style [ Css.background_color (apply_alpha opacity keyword) ]
  | None when is_fully_opaque opacity ->
      (* 100% opacity = no transparency. Tailwind outputs the plain color var
         reference, identical to the no-opacity case. *)
      let cvar = color_var c shade in
      let color_value =
        to_css ?theme c (if is_base_color c then 500 else shade)
      in
      let _, color = bound ?theme cvar color_value in
      Style.style [ Css.background_color color ]
  | None when is_custom_color c ->
      let value =
        if opacity_var_bare_of opacity <> None then
          mix_alpha opacity (to_css c shade)
        else
          let ok_l, ok_a, ok_b = custom_color_to_oklab c in
          Css.oklaba_none_zeros ok_l ok_a ok_b (percent /. 100.0)
      in
      Style.style [ Css.background_color value ]
  | None -> (
      let color_name = scheme_color_name c shade in
      match Scheme.hex_color (Scheme.or_default theme) color_name with
      | Some hex_value ->
          let cvar = color_var c shade in
          let decls, color = bound ?theme cvar (Css.hex hex_value) in
          let fallback_decl =
            if opacity_var_bare_of opacity <> None then
              Css.background_color color
            else
              Css.background_color (Css.hex (hex_with_alpha hex_value percent))
          in
          let oklab_decl = Css.background_color (mix_alpha opacity color) in
          let supports_block = color_mix_supports [ oklab_decl ] in
          Style.style ~rules:(Some [ supports_block ])
            (decls @ [ fallback_decl ])
      | None -> bg_opacity_via_property ?theme c shade opacity)

(** Background currentColor with opacity *)
let bg_current_with_opacity opacity =
  let fallback_decl = Css.background_color Css.Current in
  let oklab_decl = Css.background_color (mix_alpha opacity Css.Current) in
  let supports_block = color_mix_supports [ oklab_decl ] in
  Style.style ~rules:(Some [ supports_block ]) [ fallback_decl ]

(* ============ Colour channels ============ *)

type channel = {
  color : Css.color Var.channel;
  alpha : Css.percentage Var.property_default;
  property_prefix : string;
  metadata : Var.metadata list;
  property_rules : Css.t;
}

let channel_style ?(decls = []) ch ~fallback mixed =
  let enhanced =
    Css.color_mix_var_percent ~in_space:Css.Oklab ~var_name:(Var.name ch.alpha)
      mixed Css.Transparent
  in
  let supports_block =
    color_mix_supports (decls @ [ Var.set ch.color enhanced ])
  in
  Style.style ~rules:(Some [ supports_block ]) ~metadata:ch.metadata
    ~property_rules:ch.property_rules
    [ Var.set ch.color fallback ]

let channel_inherit ch =
  Style.style ~metadata:ch.metadata ~property_rules:ch.property_rules
    [ Var.set ch.color Css.Inherit ]

let palette_hex ?theme ?property_prefix c shade =
  let color_name = scheme_color_name c shade in
  let scoped =
    match property_prefix with
    | Some prefix -> Scheme.theme_value theme (prefix ^ "-" ^ color_name)
    | None -> None
  in
  match Scheme.hex_color (Scheme.or_default theme) color_name with
  | Some h -> h
  | None -> (
      match scoped with
      | Some h -> h
      | None -> (
          match Scheme.theme_value theme ("color-" ^ color_name) with
          | Some h -> h
          | None -> rgb_to_hex (oklch_to_rgb (to_oklch c shade))))

let channel_color ?theme ch c shade =
  let property_prefix = ch.property_prefix in
  let value = property_color_value ?theme ~property_prefix c shade in
  let decls, color =
    bound ?theme (property_color_var ?theme ~property_prefix c shade) value
  in
  channel_style ~decls ch ~fallback:value color

(* A modifier reading a custom property has no percentage for the plain fallback
   to carry, so the hex stays whole and the property mixes into the guarded
   value instead. *)
let channel_color_opacity ?theme ch c shade opacity =
  let property_prefix = ch.property_prefix in
  let hex = palette_hex ?theme ~property_prefix c shade in
  let decls, color =
    bound ?theme
      (property_color_var ?theme ~property_prefix c shade)
      (Css.hex hex)
  in
  let fallback =
    if Option.is_some (opacity_var_bare_of opacity) then Css.hex hex
    else Css.hex (hex_with_alpha hex (opacity_to_percent opacity))
  in
  channel_style ~decls ch ~fallback
    (mix_alpha ~in_space:Css.Oklab opacity color)

let channel_current ch = channel_style ch ~fallback:Css.Current Css.Current

let channel_current_opacity ch opacity =
  channel_style ch ~fallback:Css.Current
    (mix_alpha ~in_space:Css.Oklab opacity Css.Current)

let channel_transparent ch =
  channel_style ch ~fallback:Css.Transparent Css.Transparent

let channel_transparent_opacity ch opacity =
  channel_style ch ~fallback:Css.Transparent
    (apply_alpha opacity Css.Transparent)

let channel_bracket_color ~theme ch c =
  let enhanced = resolve_bracket_css_color c in
  let fallback =
    Option.value ~default:enhanced (pre_color_mix_fallback theme enhanced)
  in
  channel_style ch ~fallback enhanced

(* Tailwind writes the plain fallback as a hex carrying the alpha byte and keeps
   the oklab spelling for the [color-mix] the [\@supports] block guards, so the
   two are built separately. Folding the fallback through oklab as well made it
   depend on a colour-space round trip. *)
let channel_bracket_color_opacity ~theme ch c opacity =
  let c = Option.value ~default:c (css_color_to_hex c) in
  let percent = opacity_to_percent opacity in
  let alpha_var = opacity_var_bare_of opacity <> None in
  let guarded =
    mix_alpha ~in_space:Css.Oklab opacity (resolve_bracket_css_color c)
  in
  let fallback, mixed =
    match pre_color_mix_fallback theme guarded with
    | Some fallback -> (fallback, guarded)
    | None -> (
        match c with
        (* A modifier reading a custom property has no percentage for an alpha
           byte to carry, so the hex stays whole and the property mixes into the
           guarded value instead. *)
        | (Css.Hex _ | Css.Authored_hex _) when alpha_var ->
            (c, mix_alpha ~in_space:Css.Oklab opacity c)
        | Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ } ->
            let hex = rgba_to_hex r g b a in
            ( Css.hex (hex_with_alpha hex percent),
              hex_to_oklab_alpha hex (percent /. 100.0) )
        (* A colour with no sRGB hex has nothing to carry the alpha byte, so the
           modifier stays a mix: sRGB for the plain fallback, oklab for the
           guarded value. *)
        | _ ->
            let guarded = mix_alpha ~in_space:Css.Oklab opacity c in
            if alpha_var then (c, guarded)
            else (mix_alpha ~in_space:Css.Srgb opacity c, guarded))
  in
  channel_style ch ~fallback mixed

let bracket_var_ref v : Css.color =
  Css.Var (Var.bracket (Parse.extract_var_name v))

let bracket_var_color v =
  match Css.parse_color v with Some c -> c | None -> bracket_var_ref v

let shadow_token_colour (c : Parse.shadow_colour) : Css.color =
  match c with
  | Parse.Hex_token h -> authored_hex h
  | Parse.Var_token v -> bracket_var_color v
  | Parse.Colour c -> Option.value ~default:c (css_color_to_hex c)
  | Parse.No_colour -> Css.Current

let channel_bracket_var ch v =
  let color = bracket_var_ref v in
  channel_style ch ~fallback:color color

let channel_bracket_var_opacity ch v opacity =
  let color = bracket_var_ref v in
  channel_style ch ~fallback:color (mix_alpha ~in_space:Css.Oklab opacity color)
