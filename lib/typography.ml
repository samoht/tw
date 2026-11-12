(** Typography utilities for text and font styling. *)

module Css = Cascade.Css

(* Text size variables with line heights *)
let text_xs_var = Var.theme Css.Length "text-xs" ~order:(6, 0)

let text_xs_lh_var =
  Var.theme Css.Line_height "text-xs--line-height" ~order:(6, 1)

let text_sm_var = Var.theme Css.Length "text-sm" ~order:(6, 2)

let text_sm_lh_var =
  Var.theme Css.Line_height "text-sm--line-height" ~order:(6, 3)

let text_base_var = Var.theme Css.Length "text-base" ~order:(6, 4)

let text_base_lh_var =
  Var.theme Css.Line_height "text-base--line-height" ~order:(6, 5)

let text_lg_var = Var.theme Css.Length "text-lg" ~order:(6, 6)

let text_lg_lh_var =
  Var.theme Css.Line_height "text-lg--line-height" ~order:(6, 7)

let text_xl_var = Var.theme Css.Length "text-xl" ~order:(6, 8)

let text_xl_lh_var =
  Var.theme Css.Line_height "text-xl--line-height" ~order:(6, 9)

let text_2xl_var = Var.theme Css.Length "text-2xl" ~order:(6, 10)

let text_2xl_lh_var =
  Var.theme Css.Line_height "text-2xl--line-height" ~order:(6, 11)

let text_3xl_var = Var.theme Css.Length "text-3xl" ~order:(6, 12)

let text_3xl_lh_var =
  Var.theme Css.Line_height "text-3xl--line-height" ~order:(6, 13)

let text_4xl_var = Var.theme Css.Length "text-4xl" ~order:(6, 14)

let text_4xl_lh_var =
  Var.theme Css.Line_height "text-4xl--line-height" ~order:(6, 15)

let text_5xl_var = Var.theme Css.Length "text-5xl" ~order:(6, 16)

let text_5xl_lh_var =
  Var.theme Css.Line_height "text-5xl--line-height" ~order:(6, 17)

let text_6xl_var = Var.theme Css.Length "text-6xl" ~order:(6, 18)

let text_6xl_lh_var =
  Var.theme Css.Line_height "text-6xl--line-height" ~order:(6, 19)

let text_7xl_var = Var.theme Css.Length "text-7xl" ~order:(6, 20)

let text_7xl_lh_var =
  Var.theme Css.Line_height "text-7xl--line-height" ~order:(6, 21)

let text_8xl_var = Var.theme Css.Length "text-8xl" ~order:(6, 22)

let text_8xl_lh_var =
  Var.theme Css.Line_height "text-8xl--line-height" ~order:(6, 23)

let text_9xl_var = Var.theme Css.Length "text-9xl" ~order:(6, 24)

let text_9xl_lh_var =
  Var.theme Css.Line_height "text-9xl--line-height" ~order:(6, 25)

let font_weight_thin_var =
  Var.theme Css.Font_weight "font-weight-thin" ~order:(6, 30)

let font_weight_extralight_var =
  Var.theme Css.Font_weight "font-weight-extralight" ~order:(6, 31)

let font_weight_light_var =
  Var.theme Css.Font_weight "font-weight-light" ~order:(6, 32)

let font_weight_normal_var =
  Var.theme Css.Font_weight "font-weight-normal" ~order:(6, 33)

let font_weight_medium_var =
  Var.theme Css.Font_weight "font-weight-medium" ~order:(6, 34)

let font_weight_semibold_var =
  Var.theme Css.Font_weight "font-weight-semibold" ~order:(6, 35)

let font_weight_bold_var =
  Var.theme Css.Font_weight "font-weight-bold" ~order:(6, 36)

let font_weight_extrabold_var =
  Var.theme Css.Font_weight "font-weight-extrabold" ~order:(6, 37)

let font_weight_black_var =
  Var.theme Css.Font_weight "font-weight-black" ~order:(6, 38)

(* A [--font-weight-*] token the project declared names a weight the built-in
   scale has no slot for; they share the slot after the scale. *)
let font_weight_named_cache = Domain_cache.v 8

let font_weight_named_var name =
  Domain_cache.or_add font_weight_named_cache name (fun () ->
      Var.theme Css.Font_weight ("font-weight-" ^ name) ~order:(6, 38))

(* Publish the weight scale through the theme-token registry, the way rule.ml
   publishes the breakpoints, so [theme(--font-weight-bold)] resolves and
   [theme(static)] emits it. *)
let () =
  List.iter
    (fun (var, weight) -> Theme.register_default var weight)
    [
      (font_weight_thin_var, (Weight 100. : Css.font_weight));
      (font_weight_extralight_var, Weight 200.);
      (font_weight_light_var, Weight 300.);
      (font_weight_normal_var, Weight 400.);
      (font_weight_medium_var, Weight 500.);
      (font_weight_semibold_var, Weight 600.);
      (font_weight_bold_var, Weight 700.);
      (font_weight_extrabold_var, Weight 800.);
      (font_weight_black_var, Weight 900.);
    ]

(* Theme variables for named leading values *)
let leading_none_var = Var.theme Css.Line_height "leading-none" ~order:(6, 47)
let leading_tight_var = Var.theme Css.Line_height "leading-tight" ~order:(6, 48)
let leading_snug_var = Var.theme Css.Line_height "leading-snug" ~order:(6, 49)

let leading_normal_var =
  Var.theme Css.Line_height "leading-normal" ~order:(6, 50)

let leading_relaxed_var =
  Var.theme Css.Line_height "leading-relaxed" ~order:(6, 51)

let leading_loose_var = Var.theme Css.Line_height "leading-loose" ~order:(6, 52)

let leading_var =
  (* Leading appears in @layer properties after transforms (position 11) *)
  Var.channel ~needs_property:true ~property_order:11 ~family:`Leading
    Css.Line_height "tw-leading"

let font_weight_var =
  (* Font-weight appears in @layer properties after leading (position 12) *)
  Var.channel ~needs_property:true ~property_order:12 ~family:`Font_weight
    Css.Font_weight "tw-font-weight"

(* Theme record for line height variables *)
type line_height_theme = { leading : Css.declaration * Css.line_height Css.var }

(* Default line height theme with empty fallback for Tailwind's var(--name,)
   pattern *)
let default_line_height_theme : line_height_theme =
  let leading_decl, leading_ref =
    Var.binding leading_var ~fallback:Css.Empty (Num 1.5)
  in
  { leading = (leading_decl, leading_ref) }

(* Content variable *)
let content_var =
  Var.property_default Content ~initial:(String "") ~universal:true
    ~property_order:100 ~family:`Content "tw-content"

(* Font family theme variables. A project [@theme] can name families of its own
   ([--font-source]); order (1, 100) puts those after the three built-in
   stacks. *)
let font_named_cache = Domain_cache.v 8

let font_named_var name =
  Domain_cache.or_add font_named_cache name (fun () ->
      Var.theme Css.Font_family ("font-" ^ name) ~order:(1, 100))

(* The [--font-<name>--font-feature-settings] and [--font-variation-settings]
   tokens a project declares beside a family, each after the family. *)
let font_named_features_cache = Domain_cache.v 8
let font_named_variations_cache = Domain_cache.v 8

let font_named_features_var name =
  Domain_cache.or_add font_named_features_cache name (fun () ->
      Var.theme Css.Font_feature_settings
        ("font-" ^ name ^ "--font-feature-settings")
        ~order:(1, 101))

let font_named_variations_var name =
  Domain_cache.or_add font_named_variations_cache name (fun () ->
      Var.theme Css.Font_variation_settings
        ("font-" ^ name ^ "--font-variation-settings")
        ~order:(1, 101))

(* A [--text-*] token the project declared names a font size the built-in scale
   has no slot for; they share the slot after the scale, the way the project's
   font families share theirs. *)
let text_named_cache = Domain_cache.v 8

let text_named_var name =
  Domain_cache.or_add text_named_cache name (fun () ->
      Var.theme Css.Length ("text-" ^ name) ~order:(6, 26))

(* The [--text-<name>--line-height], [--letter-spacing] and [--font-weight]
   tokens a project declares beside a size of its own, each after the size. *)
let text_named_lh_cache = Domain_cache.v 8
let text_named_tracking_cache = Domain_cache.v 8
let text_named_weight_cache = Domain_cache.v 8

let text_named_lh_var name =
  Domain_cache.or_add text_named_lh_cache name (fun () ->
      Var.theme Css.Line_height ("text-" ^ name ^ "--line-height") ~order:(6, 27))

let text_named_tracking_var name =
  Domain_cache.or_add text_named_tracking_cache name (fun () ->
      Var.theme Css.Length ("text-" ^ name ^ "--letter-spacing") ~order:(6, 27))

let text_named_weight_var name =
  Domain_cache.or_add text_named_weight_cache name (fun () ->
      Var.theme Css.Font_weight ("text-" ^ name ^ "--font-weight") ~order:(6, 27))

(* The same for a [--leading-*] token the project declared. *)
let leading_named_cache = Domain_cache.v 8

let leading_named_var name =
  Domain_cache.or_add leading_named_cache name (fun () ->
      Var.theme Css.Line_height ("leading-" ^ name) ~order:(6, 53))

let font_sans_var = Var.theme Css.Font_family "font-sans" ~order:(1, 0)
let font_serif_var = Var.theme Css.Font_family "font-serif" ~order:(1, 1)
let font_mono_var = Var.theme Css.Font_family "font-mono" ~order:(1, 2)

(* Default font family variables that reference the base font variables *)
let default_font_family_var =
  Var.theme Css.Font_family "default-font-family" ~order:(8, 39)

let default_mono_font_family_var =
  Var.theme Css.Font_family "default-mono-font-family" ~order:(8, 42)

(* Tailwind v4.3.2 replaced the [ui-sans-serif, system-ui, ...] default with the
   platform system-font stack. *)
let default_sans_stack : Css.font_family =
  List
    [
      Name "-apple-system";
      Name "BlinkMacSystemFont";
      Segoe_ui;
      Roboto;
      Helvetica_neue;
      Noto_sans;
      Arial;
      Sans_serif;
      Apple_color_emoji;
      Segoe_ui_emoji;
      Segoe_ui_symbol;
      Noto_color_emoji;
    ]

let default_mono_stack : Css.font_family =
  List
    [
      Ui_monospace;
      SFMono_regular;
      Menlo;
      Monaco;
      Consolas;
      Liberation_mono;
      Courier_new;
      Monospace;
    ]

let default_serif_stack : Css.font_family =
  List [ Ui_serif; Georgia; Cambria; Times_new_roman; Times; Serif ]

(* Published the same way, so [font-[theme(--font-mono)]] resolves. *)
let () =
  List.iter
    (fun (var, stack) -> Theme.register_default var stack)
    [
      (font_sans_var, default_sans_stack);
      (font_serif_var, default_serif_stack);
      (font_mono_var, default_mono_stack);
    ]

(* Base font family variables for theme layer *)
let default_font_declarations =
  let sans_decl = Var.set font_sans_var default_sans_stack in
  let mono_decl = Var.set font_mono_var default_mono_stack in
  [ sans_decl; mono_decl ]

(* Default font family variables that reference the base font variables. *)
let default_font_family_declarations =
  let sans_decl, sans_ref = Var.binding font_sans_var default_sans_stack in
  let mono_decl, mono_ref = Var.binding font_mono_var default_mono_stack in
  let default_font_decl = Var.set default_font_family_var (Css.Var sans_ref) in
  let default_mono_decl =
    Var.set default_mono_font_family_var (Css.Var mono_ref)
  in
  [ sans_decl; mono_decl; default_font_decl; default_mono_decl ]

(* CSS Fonts 4 sec. 6.4: [font-feature-settings] is [normal] or a comma list of
   [<opentype-tag> [<integer> | on | off]?], the tag being a quoted
   four-character string. The docs' [<value>] placeholder is neither. *)

(** Early typography handler - comes before color utilities (priority 22) *)
let is_feature_tag_list s =
  let s = Parse.decode_underscores s in
  let entry e =
    match
      String.split_on_char ' ' (String.trim e) |> List.filter (( <> ) "")
    with
    | [] -> false
    | tag :: rest -> (
        let n = String.length tag in
        n = 6
        && (tag.[0] = '"' || tag.[0] = '\'')
        && tag.[n - 1] = tag.[0]
        &&
        match rest with
        | [] -> true
        | [ x ] -> x = "on" || x = "off" || int_of_string_opt x <> None
        | _ -> false)
  in
  String.trim s = "normal"
  || (s <> "" && List.for_all entry (String.split_on_char ',' s))

(* CSS Fonts 4 sec. 2.2: a family name is an identifier sequence or a quoted
   string, and the list is comma-separated. *)
let is_font_family_value s =
  let entry e =
    let e = String.trim e in
    let n = String.length e in
    if n = 0 then false
    else if (e.[0] = '"' || e.[0] = '\'') && n > 1 then e.[n - 1] = e.[0]
    else
      String.split_on_char ' ' e |> List.filter (fun w -> w <> "") |> fun ws ->
      ws <> [] && List.for_all Parse.is_ident ws
  in
  s <> "" && List.for_all entry (String.split_on_char ',' s)

module Typography_early = struct
  open Style
  open Css

  type t =
    | (* Text sizes *)
      Text_xs
    | Text_sm
    | Text_base
    | Text_lg
    | Text_xl
    | Text_2xl
    | Text_3xl
    | Text_4xl
    | Text_5xl
    | Text_6xl
    | Text_7xl
    | Text_8xl
    | Text_9xl
    | (* Font weights *)
      Font_thin
    | Font_extralight
    | Font_light
    | Font_normal
    | Font_medium
    | Font_semibold
    | Font_bold
    | Font_extrabold
    | Font_black
    | (* Arbitrary font values - store (bracket_content, parsed_value) *)
      Font_bracket_weight of string * int (* font-[100] *)
    | Font_bracket_weight_var of string * string
      (* font-[number:var(--x)] or font-[var(--x)] *)
    | Font_bracket_family_quoted of string * string (* font-["arial_rounded"] *)
    | Font_bracket_family_name of string * string (* font-[ui-sans-serif] *)
    | Font_bracket_family_var of string * string
      (* font-[family-name:var(--x)] or font-[generic-name:var(--x)] *)
    | (* Font feature settings *)
      Font_features_quoted of string (* font-features-["smcp"] *)
    | Font_features_raw of string * string
      (* Each of these families writes one longhand, so a bracket no reader took
         still names it and the value is forwarded verbatim - Tailwind's
         token-stream contract. *)
    | Font_features_var of string (* font-features-[var(--x)] *)
    | Font_features_bare_var of string (* font-features-(--my-var) *)
    | (* Font families. The flag records whether the theme adds feature
         settings, which gives the candidate a wider property set. *)
      Font_sans of bool
    | Font_serif of bool
    | Font_mono of bool
    | Font_named of string * bool (* font-source, from a --font-* theme token *)
    | Font_weight_theme of string (* weight from a --font-weight-* token *)
    | Leading_theme of string (* line height from a --leading-* token *)
    | (* Font styles *)
      Italic
    | Not_italic
    | (* Text alignment *)
      Text_left
    | Text_center
    | Text_right
    | Text_justify
    | Text_start
    | Text_end
    | (* Leading *)
      Leading_none
    | Leading_tight
    | Leading_snug
    | Leading_normal
    | Leading_relaxed
    | Leading_loose
    | Leading of int
    | Leading_step of float (* leading-0.5, a half-step scale value *)
    | Leading_var of string (* leading-[var(--value)] *)
    (* The author's bracket text travels with the line-height it denotes, so the
       class name is spelled exactly as it was written. *)
    | Leading_bracket of
        string * Css.line_height (* leading-[1.8], leading-[24px], etc. *)
    | Leading_raw of string * string
    | (* Font-size with explicit line-height modifier (text-sm/6) *)
      Text_named_lh of string * lh_modifier
    | (* Font size named by a project [--text-*] token (text-huge) *)
      Text_theme of string
    | Text_theme_lh of string * lh_modifier
    | (* Arbitrary font-size (text-[12px]) *)
      Text_bracket_fs of string
    | (* Arbitrary font-size with line-height (text-[12px]/6) *)
      Text_bracket_fs_lh of string * lh_modifier
    | (* A font-size hint over a value no font-size grammar reads
         (text-[length:red]). The hint chooses the longhand and the value is
         forwarded verbatim, which is Tailwind's token-stream contract; the
         browser discards the declaration. *)
      Text_bracket_fs_raw of string * string

  and lh_modifier =
    | Spacing of int (* /6 → calc(var(--spacing) * 6) *)
    | No_leading (* /none → line-height: 1 *)
    | Named of string (* /snug → var(--leading-snug) *)
    | Bracket of string * Css.line_height (* /[4px] → 4px *)
    | Var_shorthand of string (* /(--lh) → var(--lh) *)

  let name = "typography_early"

  (* Most early-typography families (text-align, font, leading) sort at priority
     24. font-style (italic) is canonically later (rank 87, after tracking/
     whitespace/text-transform), so it joins the late text families' band at
     priority 26 via a suborder just above text-transform (~8360, in
     lib/text_transform.ml). *)
  let priority = function Italic | Not_italic -> 26 | _ -> 24
  let err_not_utility = Error (`Msg "Not an early typography utility")

  (* Lookup table for named text sizes: (name, size_var, default_rem) *)
  let text_size_data =
    [
      ("xs", text_xs_var, 0.75);
      ("sm", text_sm_var, 0.875);
      ("base", text_base_var, 1.0);
      ("lg", text_lg_var, 1.125);
      ("xl", text_xl_var, 1.25);
      ("2xl", text_2xl_var, 1.5);
      ("3xl", text_3xl_var, 1.875);
      ("4xl", text_4xl_var, 2.25);
      ("5xl", text_5xl_var, 3.0);
      ("6xl", text_6xl_var, 3.75);
      ("7xl", text_7xl_var, 4.5);
      ("8xl", text_8xl_var, 6.0);
      ("9xl", text_9xl_var, 8.0);
    ]

  (* The acceptor for [text-<size>] reads the converter's own table: the two
     have to hold the same names, and a name in one but not the other is an
     accepted class that emits no declarations at all. *)
  (* The line-height each text size carries, which its [text-*] utility and its
     [--text-*--line-height] token both spell. *)
  let text_line_heights : (string * Css.line_height) list =
    [
      ("xs", Rem 1.0);
      ("sm", Rem 1.25);
      ("base", Rem 1.5);
      ("lg", Rem 1.75);
      ("xl", Rem 1.75);
      ("2xl", Rem 2.0);
      ("3xl", Rem 2.25);
      ("4xl", Rem 2.5);
      ("5xl", Num 1.0);
      ("6xl", Num 1.0);
      ("7xl", Num 1.0);
      ("8xl", Num 1.0);
      ("9xl", Num 1.0);
    ]

  let line_height_of size = List.assoc size text_line_heights
  let named_sizes = List.map (fun (name, _, _) -> name) text_size_data
  let is_named_size s = List.mem s named_sizes

  (** Font-size keyword lookup. *)
  let font_size_keyword = function
    | "larger" -> Stdlib.Option.Some Css.Larger
    | "smaller" -> Stdlib.Option.Some Css.Smaller
    | "xx-large" -> Stdlib.Option.Some Css.Xx_large
    | "x-large" -> Stdlib.Option.Some Css.X_large
    | "large" -> Stdlib.Option.Some Css.Large
    | "medium" -> Stdlib.Option.Some Css.Medium
    | "small" -> Stdlib.Option.Some Css.Small
    | "x-small" -> Stdlib.Option.Some Css.X_small
    | "xx-small" -> Stdlib.Option.Some Css.Xx_small
    | "xxx-large" -> Stdlib.Option.Some Css.Xxx_large
    | _ -> Stdlib.Option.None

  (** A bracket font-size is any CSS length, but it has to carry a unit:
      Tailwind reads the unitless zero of [text-[0]] as a colour, not a size. *)
  let try_parse_length_value s =
    match Parse.arbitrary_length s with
    | Stdlib.Option.Some Css.Zero -> Stdlib.Option.None
    | len -> len

  (** Split a type prefix like "absolute-size:var(--my-size)" into (prefix,
      value). Finds first ':' outside parens/brackets. *)
  let split_type_prefix inner =
    let len = String.length inner in
    let rec find i depth =
      if i >= len then Stdlib.Option.None
      else
        match inner.[i] with
        | '(' | '[' -> find (i + 1) (depth + 1)
        | ')' | ']' -> find (i + 1) (depth - 1)
        | ':' when depth = 0 ->
            Stdlib.Option.Some
              (String.sub inner 0 i, String.sub inner (i + 1) (len - i - 1))
        | _ -> find (i + 1) depth
    in
    find 0 0

  (* Tailwind's [--spacing(N)] inside an arbitrary value reads the spacing
     scale: [text-[--spacing(2)]] is [font-size: calc(var(--spacing) * 2)]. *)

  (** Does [inner] look like something we can emit as a font-size? Accepts typed
      prefix (length/percentage/absolute-size/relative-size), font-size keyword
      (medium, xxx-large, larger, ...), any CSS length carrying a unit (16px,
      1rem, 1.5em, 3ch, 100%, clamp(...), calc(...)), or var(...). Rejects bare
      identifiers that aren't recognised keywords -- e.g. a bare "1A202C" is not
      a valid font-size: a length must carry a unit and a hex color must start
      with "#" (CSS Color §5.4.6). *)
  let parse_spacing_call s =
    Option.bind (Parse.call_body "--spacing" s) float_of_string_opt

  (* A data-type hint in front of an arbitrary font size says how to read the
     value written after it, so [text-[length:1rem]] is the size [1rem] and not
     a reference to a custom property called [--1rem]. *)
  let is_font_size_hint = function
    | "absolute-size" | "relative-size" | "length" | "percentage" -> true
    | _ -> false

  (* The value a bracket font size carries, told apart from the hint that
     introduces it. *)
  let bracket_font_size_value inner =
    match split_type_prefix inner with
    | Stdlib.Option.Some (prefix, value) when is_font_size_hint prefix ->
        (value, true)
    | _ -> (inner, false)

  (* A hint settles the ambiguity [try_parse_length_value] guards against, so a
     hinted [0] is the size zero where a bare one is a colour. A bare number
     after the hint goes through Tailwind's generator as written and its
     minifier reads it as pixels, which is the spelling [--diff] compares and
     the one [stroke-[1.5]] already follows; see
     docs/token-stream-contract.md. *)
  let bracket_font_size_length value ~hinted =
    if hinted then
      match Parse.arbitrary_length value with
      | Stdlib.Option.Some _ as len -> len
      | Stdlib.Option.None -> (
          match float_of_string_opt value with
          | Some f -> Stdlib.Option.Some (Css.Px f)
          | None -> Stdlib.Option.None)
    else try_parse_length_value value

  (* A [length:] hint says the bracket is a font size whatever the value turns
     out to be, so a value no font-size grammar reads is still a font size -
     forwarded verbatim under the token-stream contract. Without the hint the
     same text names no utility, because nothing says which longhand it is
     for. *)
  let hinted_font_size_token_stream inner =
    match split_type_prefix inner with
    | Stdlib.Option.Some (prefix, _) when is_font_size_hint prefix ->
        Parse.arbitrary_declaration_value inner
    | _ -> Stdlib.Option.None

  let is_valid_bracket_font_size (inner : string) : bool =
    let value, hinted = bracket_font_size_value inner in
    match font_size_keyword value with
    | Some _ -> true
    | Stdlib.Option.None ->
        bracket_font_size_length value ~hinted <> Stdlib.Option.None
        || Parse.is_var value
        || parse_spacing_call value <> Stdlib.Option.None

  (** Check if bracket content looks like a color (should go to color handler).
  *)
  let is_color_bracket inner =
    (* Hex color *)
    (String.length inner > 0 && inner.[0] = '#')
    (* color: typed prefix *)
    || String.starts_with ~prefix:"color:" inner
    (* bare var() without type prefix defaults to color *)
    || Parse.is_var inner
    (* CSS color functions like rgba(...), hsl(...), oklch(...) *)
    || Parse.is_css_color_fn inner

  (** Split on first '/' outside brackets/parens. *)
  let split_on_slash s =
    let len = String.length s in
    let rec find i depth =
      if i >= len then Stdlib.Option.None
      else
        match s.[i] with
        | '[' | '(' -> find (i + 1) (depth + 1)
        | ']' | ')' -> find (i + 1) (depth - 1)
        | '/' when depth = 0 ->
            Stdlib.Option.Some
              (String.sub s 0 i, String.sub s (i + 1) (len - i - 1))
        | _ -> find (i + 1) depth
    in
    find 0 0

  (* The one reader for an arbitrary line-height, shared by [leading-[...]] and
     by the [/leading] modifier on a text size. The string parsing is cascade's,
     so every unit, [calc()] and [var()] spelling it reads is accepted; a
     bracket it cannot read is not a line-height and the utility is refused. *)
  let parse_bracket_leading raw : Css.line_height option =
    if Parse.is_var raw then Some (Var (Var.bracket raw))
    else
      let cursor =
        Cascade.Cursor.of_string (Parse.decode_arbitrary_value raw)
      in
      match
        Cascade.Cursor.try_parse_full_err Css.Properties.read_line_height cursor
      with
      (* The reader keeps a dimension whose unit it does not know as verbatim
         text, so a stray source word reading as one would become a utility. The
         length grammar is what says whether the unit is a length; asking it
         keeps the answer out of a unit table here. *)
      | Ok (Number { repr; unit = Some unit; _ } as lh) ->
          if Css.parse_length (repr ^ unit) = None then None else Some lh
      | Ok lh -> Some lh
      | Error _ -> None

  (* [none] is not here: [parse_lh_modifier] answers it with [No_leading] before
     this list is consulted, because Tailwind writes [line-height: 1] for it
     rather than reading a [--leading-none] token it never defines. A row here
     would contradict that the moment anyone removed the short-circuit. *)
  let known_leading_names = [ "tight"; "snug"; "normal"; "relaxed"; "loose" ]

  (* The modifier names a [--leading-*] token, and one the project declared
     counts the same as a built-in one. *)
  let parse_lh_modifier theme s =
    if s = "none" then Stdlib.Option.Some No_leading
    else if Parse.is_bare_var s then
      (* The [(--name)] shorthand reads the variable as the line height, the way
         [[var(--name)]] does, and keeps its own spelling in the class. *)
      Stdlib.Option.Some (Var_shorthand (Parse.bare_var_inner s))
    else if Parse.is_bracket_value s then
      let inner = Parse.bracket_inner s in
      match parse_bracket_leading inner with
      | Stdlib.Option.Some lh -> Stdlib.Option.Some (Bracket (inner, lh))
      | Stdlib.Option.None -> Stdlib.Option.None
    else
      match Parse.decimal_int s with
      | Stdlib.Option.Some n when n >= 0 -> Stdlib.Option.Some (Spacing n)
      | _ ->
          if
            List.mem s known_leading_names
            || Scheme.theme_value (Some theme) ("leading-" ^ s) <> None
          then Stdlib.Option.Some (Named s)
          else Stdlib.Option.None

  (* A font family the project named in its [@theme]. Gated on the token being
     there so a stray source word (font-awesome) is not a utility. *)
  (* A weight or line height the project named in its [@theme]. The built-in
     scales are matched before these are consulted. *)
  let is_theme_font_weight theme n =
    Scheme.theme_value (Some theme) ("font-weight-" ^ n) <> None

  (* [leading-6] is the spacing-derived numeric form, whatever a project happens
     to have declared, so a plain number is never a token name. *)
  let is_theme_leading theme n =
    Parse.decimal_int n = None
    && Scheme.theme_value (Some theme) ("leading-" ^ n) <> None

  let is_named_font theme n =
    Scheme.theme_value (Some theme) ("font-" ^ n) <> None

  let has_font_features theme n =
    Scheme.theme_value (Some theme) ("font-" ^ n ^ "--font-feature-settings")
    <> None

  (* A font size the project named in its [@theme]. The built-in scale is
     published through the static token catalog, so exclude it: those names have
     their own constructors. [text-<name>] also spells a colour, and a project
     that declares both under one name gets the colour, as Tailwind does.
     [--text-shadow-*] is a namespace of its own, and a doubled hyphen marks a
     modifier on another token ([--text-lg--line-height]), so neither names a
     size; nor does a token whose value is not a length. *)
  let is_theme_text_size theme n =
    (not (is_named_size n))
    && (not (String.starts_with ~prefix:"shadow-" n))
    && List.for_all (fun seg -> seg <> "") (String.split_on_char '-' n)
    && Scheme.theme_value (Some theme) ("color-" ^ n) = None
    &&
    match Scheme.theme_value (Some theme) ("text-" ^ n) with
    | None -> false
    | Some raw -> Css.parse_length raw <> None

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "xs" ] -> Ok Text_xs
    | [ "text"; "sm" ] -> Ok Text_sm
    | [ "text"; "base" ] -> Ok Text_base
    | [ "text"; "lg" ] -> Ok Text_lg
    | [ "text"; "xl" ] -> Ok Text_xl
    | [ "text"; "2xl" ] -> Ok Text_2xl
    | [ "text"; "3xl" ] -> Ok Text_3xl
    | [ "text"; "4xl" ] -> Ok Text_4xl
    | [ "text"; "5xl" ] -> Ok Text_5xl
    | [ "text"; "6xl" ] -> Ok Text_6xl
    | [ "text"; "7xl" ] -> Ok Text_7xl
    | [ "text"; "8xl" ] -> Ok Text_8xl
    | [ "text"; "9xl" ] -> Ok Text_9xl
    | [ "font"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        (* A family name is a list of idents or quoted strings, so the docs'
           [<value>] placeholder is not one. [_] separates the words of a name
           and [\_] is a literal underscore inside one, so the stack is read
           decoded while the class keeps the spelling it was written with. *)
        let family raw value =
          if Parse.is_var value then Ok (Font_bracket_family_var (raw, value))
          else
            let decoded = Parse.decode_underscores value in
            if is_font_family_value decoded then
              Ok (Font_bracket_family_name (raw, decoded))
            else err_not_utility
        in
        let weight raw value =
          if Parse.is_var value then Ok (Font_bracket_weight_var (raw, value))
          else
            match Parse.decimal_int value with
            | Some n -> Ok (Font_bracket_weight (raw, n))
            | None -> err_not_utility
        in
        if String.length inner > 0 && inner.[0] = '"' then
          (* font-["arial_rounded"] → the decoded bracket text becomes the
             literal font-family value, quotes and all; Tailwind never wraps it
             in another layer of quoting. *)
          let decoded = Parse.decode_underscores inner in
          Ok (Font_bracket_family_quoted (inner, decoded))
        else
          (* A data-type hint picks which of the two readings applies; the value
             written after it is read the way the un-hinted spelling is, so only
             a var() reference names a custom property. *)
          match split_type_prefix inner with
          | Stdlib.Option.Some (("family-name" | "generic-name"), value) ->
              family inner value
          | Stdlib.Option.Some ("number", value) -> weight inner value
          | _ ->
              if Parse.decimal_int inner <> None || Parse.is_var inner then
                weight inner inner
              else family inner inner)
    | [ "font"; "features"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        if Parse.is_var inner then Ok (Font_features_var inner)
        else if is_feature_tag_list inner then Ok (Font_features_quoted inner)
        else
          match Parse.arbitrary_declaration_value inner with
          | Some raw -> Ok (Font_features_raw (inner, raw))
          | None -> err_not_utility)
    | [ "font"; "features"; v ] when Parse.is_bare_var v ->
        Ok (Font_features_bare_var v)
    | [ "font"; "thin" ] -> Ok Font_thin
    | [ "font"; "extralight" ] -> Ok Font_extralight
    | [ "font"; "light" ] -> Ok Font_light
    | [ "font"; "normal" ] -> Ok Font_normal
    | [ "font"; "medium" ] -> Ok Font_medium
    | [ "font"; "semibold" ] -> Ok Font_semibold
    | [ "font"; "bold" ] -> Ok Font_bold
    | [ "font"; "extrabold" ] -> Ok Font_extrabold
    | [ "font"; "black" ] -> Ok Font_black
    | [ "font"; "sans" ] -> Ok (Font_sans (has_font_features theme "sans"))
    | [ "font"; "serif" ] -> Ok (Font_serif (has_font_features theme "serif"))
    | [ "font"; "mono" ] -> Ok (Font_mono (has_font_features theme "mono"))
    | "font" :: (_ :: _ as rest)
      when is_named_font theme (String.concat "-" rest) ->
        let name = String.concat "-" rest in
        Ok (Font_named (name, has_font_features theme name))
    | "font" :: (_ :: _ as rest)
      when is_theme_font_weight theme (String.concat "-" rest) ->
        Ok (Font_weight_theme (String.concat "-" rest))
    | [ "italic" ] -> Ok Italic
    | [ "not"; "italic" ] -> Ok Not_italic
    | [ "text"; "left" ] -> Ok Text_left
    | [ "text"; "center" ] -> Ok Text_center
    | [ "text"; "right" ] -> Ok Text_right
    | [ "text"; "justify" ] -> Ok Text_justify
    | [ "text"; "start" ] -> Ok Text_start
    | [ "text"; "end" ] -> Ok Text_end
    | [ "leading"; "none" ] -> Ok Leading_none
    | [ "leading"; "tight" ] -> Ok Leading_tight
    | [ "leading"; "snug" ] -> Ok Leading_snug
    | [ "leading"; "normal" ] -> Ok Leading_normal
    | [ "leading"; "relaxed" ] -> Ok Leading_relaxed
    | [ "leading"; "loose" ] -> Ok Leading_loose
    | [ "leading"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        if Parse.is_var inner then Ok (Leading_var inner)
        else
          match parse_bracket_leading inner with
          | Stdlib.Option.Some lh -> Ok (Leading_bracket (inner, lh))
          | Stdlib.Option.None -> (
              match Parse.arbitrary_declaration_value inner with
              | Some value -> Ok (Leading_raw (inner, value))
              | None -> err_not_utility))
    | "leading" :: (_ :: _ as rest)
      when is_theme_leading theme (String.concat "-" rest) ->
        Ok (Leading_theme (String.concat "-" rest))
    | [ "leading"; n ] -> (
        (* Tailwind accepts any non-negative leading-N (or a quarter-step
           leading-N.M), derived from spacing. *)
        match Parse.spacing_value ~name:"leading" n with
        | Ok f when Float.is_integer f -> Ok (Leading (int_of_float f))
        | Ok f -> Ok (Leading_step f)
        | Error _ as e -> e)
    | [ "text"; part ] -> (
        match split_on_slash part with
        | Stdlib.Option.Some (base, lh_str) -> (
            match parse_lh_modifier theme lh_str with
            | Stdlib.Option.Some lh_mod ->
                if Parse.is_bracket_value base then
                  let inner = Parse.bracket_inner base in
                  if is_color_bracket inner then err_not_utility
                  else if is_valid_bracket_font_size inner then
                    Ok (Text_bracket_fs_lh (inner, lh_mod))
                  else err_not_utility
                else if is_named_size base then
                  Ok (Text_named_lh (base, lh_mod))
                else if is_theme_text_size theme base then
                  Ok (Text_theme_lh (base, lh_mod))
                else err_not_utility
            | Stdlib.Option.None -> err_not_utility)
        | Stdlib.Option.None ->
            if Parse.is_bracket_value part then
              let inner = Parse.bracket_inner part in
              if is_color_bracket inner then err_not_utility
              else if is_valid_bracket_font_size inner then
                Ok (Text_bracket_fs inner)
              else
                match hinted_font_size_token_stream inner with
                | Stdlib.Option.Some value ->
                    Ok (Text_bracket_fs_raw (inner, value))
                | Stdlib.Option.None -> err_not_utility
            else if is_theme_text_size theme part then Ok (Text_theme part)
            else err_not_utility)
    (* A project token's name may span several segments, and the line-height
       modifier still rides on the last one. *)
    | "text" :: (_ :: _ :: _ as rest) -> (
        let joined = String.concat "-" rest in
        match split_on_slash joined with
        | Stdlib.Option.Some (base, lh_str) when is_theme_text_size theme base
          -> (
            match parse_lh_modifier theme lh_str with
            | Stdlib.Option.Some lh_mod -> Ok (Text_theme_lh (base, lh_mod))
            | Stdlib.Option.None -> err_not_utility)
        | _ when is_theme_text_size theme joined -> Ok (Text_theme joined)
        | _ -> err_not_utility)
    | _ -> err_not_utility

  let lh_to_string = function
    | Spacing n -> string_of_int n
    | No_leading -> "none"
    | Named name -> name
    | Bracket (v, _) -> "[" ^ v ^ "]"
    | Var_shorthand name -> "(" ^ name ^ ")"

  let to_class = function
    | Text_xs -> "text-xs"
    | Text_sm -> "text-sm"
    | Text_base -> "text-base"
    | Text_lg -> "text-lg"
    | Text_xl -> "text-xl"
    | Text_2xl -> "text-2xl"
    | Text_3xl -> "text-3xl"
    | Text_4xl -> "text-4xl"
    | Text_5xl -> "text-5xl"
    | Text_6xl -> "text-6xl"
    | Text_7xl -> "text-7xl"
    | Text_8xl -> "text-8xl"
    | Text_9xl -> "text-9xl"
    | Font_thin -> "font-thin"
    | Font_extralight -> "font-extralight"
    | Font_light -> "font-light"
    | Font_normal -> "font-normal"
    | Font_medium -> "font-medium"
    | Font_semibold -> "font-semibold"
    | Font_bold -> "font-bold"
    | Font_extrabold -> "font-extrabold"
    | Font_black -> "font-black"
    | Font_bracket_weight (raw, _) -> "font-[" ^ raw ^ "]"
    | Font_bracket_weight_var (raw, _) -> "font-[" ^ raw ^ "]"
    | Font_bracket_family_quoted (raw, _) -> "font-[" ^ raw ^ "]"
    | Font_bracket_family_name (raw, _) -> "font-[" ^ raw ^ "]"
    | Font_bracket_family_var (raw, _) -> "font-[" ^ raw ^ "]"
    | Font_features_quoted raw | Font_features_raw (raw, _) ->
        "font-features-[" ^ raw ^ "]"
    | Font_features_var raw -> "font-features-[" ^ raw ^ "]"
    | Font_features_bare_var raw -> "font-features-" ^ raw
    | Font_named (name, _) -> "font-" ^ name
    | Font_weight_theme name -> "font-" ^ name
    | Leading_theme name -> "leading-" ^ name
    | Font_sans _ -> "font-sans"
    | Font_serif _ -> "font-serif"
    | Font_mono _ -> "font-mono"
    | Italic -> "italic"
    | Not_italic -> "not-italic"
    | Text_left -> "text-left"
    | Text_center -> "text-center"
    | Text_right -> "text-right"
    | Text_justify -> "text-justify"
    | Text_start -> "text-start"
    | Text_end -> "text-end"
    | Leading_none -> "leading-none"
    | Leading_tight -> "leading-tight"
    | Leading_snug -> "leading-snug"
    | Leading_normal -> "leading-normal"
    | Leading_relaxed -> "leading-relaxed"
    | Leading_loose -> "leading-loose"
    | Leading n -> "leading-" ^ string_of_int n
    | Leading_step f ->
        "leading-" ^ Spacing.pp_spacing_suffix (`Rem (f *. 0.25))
    | Leading_var v -> "leading-[" ^ v ^ "]"
    | Leading_bracket (v, _) -> "leading-[" ^ v ^ "]"
    | Leading_raw (v, _) -> "leading-[" ^ v ^ "]"
    | Text_named_lh (name, lh) -> "text-" ^ name ^ "/" ^ lh_to_string lh
    | Text_theme name -> "text-" ^ name
    | Text_theme_lh (name, lh) -> "text-" ^ name ^ "/" ^ lh_to_string lh
    | Text_bracket_fs raw -> "text-[" ^ raw ^ "]"
    | Text_bracket_fs_raw (raw, _) -> "text-[" ^ raw ^ "]"
    | Text_bracket_fs_lh (raw, lh) -> "text-[" ^ raw ^ "]/" ^ lh_to_string lh

  (** {1 Ordering Support} *)

  let suborder = function
    (* Text align comes first - alphabetical order *)
    | Text_center -> 1001
    | Text_end -> 1002
    | Text_justify -> 1003
    | Text_left -> 1004
    | Text_right -> 1005
    | Text_start -> 1006
    (* Font-family candidates share a slot and sort by class name. A theme
       family that also writes feature settings has a wider property set, so
       Tailwind places it in the preceding slot. *)
    | Font_bracket_family_quoted _ -> 1503
    | Font_bracket_family_var _ -> 1503
    | Font_bracket_family_name _ -> 1503
    (* Bracket font weights come before named font weights *)
    (* Font family - comes between text-align and text-size. *)
    | Font_mono true | Font_sans true | Font_serif true | Font_named (_, true)
      ->
        1502
    | Font_mono false
    | Font_sans false
    | Font_serif false
    | Font_named (_, false) ->
        1503
    | Font_weight_theme _ -> 4201
    | Leading_theme _ -> 3206
    (* Bracket font-size/line-height candidates follow 9xl in the shared slot;
       the candidate-name tiebreak keeps 9xl first. *)
    | Text_bracket_fs_lh _ -> 2008
    (* Font sizes come second - alphabetical order *)
    | Text_2xl -> 2001
    | Text_3xl -> 2002
    | Text_4xl -> 2003
    | Text_5xl -> 2004
    | Text_6xl -> 2005
    | Text_7xl -> 2006
    | Text_8xl -> 2007
    | Text_9xl -> 2008
    | Text_base -> 2009
    | Text_lg -> 2010
    | Text_sm -> 2011
    | Text_xl -> 2012
    | Text_xs -> 2013
    (* Named size with line-height modifier — same suborder as base size *)
    | Text_named_lh (name, _) -> (
        match name with
        | "2xl" -> 2001
        | "3xl" -> 2002
        | "4xl" -> 2003
        | "5xl" -> 2004
        | "6xl" -> 2005
        | "7xl" -> 2006
        | "8xl" -> 2007
        | "9xl" -> 2008
        | "base" -> 2009
        | "lg" -> 2010
        | "sm" -> 2011
        | "xl" -> 2012
        | "xs" -> 2013
        | _ -> 2000)
    (* A project's own size sorts after the whole built-in scale, the way
       Tailwind orders it. *)
    | Text_theme _ -> 2014
    | Text_theme_lh _ -> 2014
    (* Bracket font-size without modifier — after named sizes *)
    | Text_bracket_fs _ | Text_bracket_fs_raw _ -> 2100
    (* Leading comes third — numeric first, then arbitrary, then named
       (alphabetical: loose, none, normal, relaxed, snug, tight) *)
    | Leading n -> 3000 + n
    | Leading_step f -> 3000 + int_of_float (f *. 10.)
    | Leading_var _ -> 3100
    | Leading_bracket _ -> 3100
    | Leading_raw _ -> 3100
    | Leading_loose -> 3201
    | Leading_none -> 3202
    | Leading_normal -> 3203
    | Leading_relaxed -> 3204
    | Leading_snug -> 3205
    | Leading_tight -> 3206
    (* Bracket font weights come before named font weights *)
    | Font_bracket_weight _ -> 4000
    | Font_bracket_weight_var _ -> 4000
    (* Font weight comes fourth - alphabetical order *)
    | Font_black -> 4100
    | Font_bold -> 4200
    | Font_extrabold -> 4300
    | Font_extralight -> 4400
    | Font_light -> 4500
    | Font_medium -> 4600
    | Font_normal -> 4700
    | Font_semibold -> 4800
    | Font_thin -> 4900
    (* Font feature settings *)
    | Font_features_quoted _ | Font_features_raw _ -> 1600
    | Font_features_var _ -> 1600
    | Font_features_bare_var _ -> 1600
    (* Italic *)
    | Italic -> 8380
    | Not_italic -> 8381

  (* Text utilities use theme record for line height variable reference *)
  (* Tailwind v4 expresses a rem line-height as the ratio [calc(line-height /
     font-size)]; unitless line-heights stay verbatim. *)
  let text_line_height ~size_rem : Css.line_height -> Css.line_height = function
    | Rem lh_rem ->
        let num f : Css.line_height Css.calc = Num f in
        Calc (Css.Calc.div (num lh_rem) (num size_rem))
    | other -> other

  let text_size_utility (size_var : Css.length Var.theme)
      (lh_var : Css.line_height Var.theme) size_rem lh_value =
    let size_decl, size_ref = Var.binding size_var (Rem size_rem) in
    let lh_value = text_line_height ~size_rem lh_value in
    let lh_decl, lh_ref = Var.binding lh_var lh_value in
    (* Use shared theme record - no declaration, just reference *)
    let theme = default_line_height_theme in
    let leading_ref = snd theme.leading in
    let leading_with_fallback =
      Css.with_fallback leading_ref (Css.Var lh_ref)
    in
    style
      [
        size_decl;
        lh_decl;
        font_size (Css.Var size_ref);
        line_height (Var leading_with_fallback);
      ]

  let text_xs () =
    text_size_utility text_xs_var text_xs_lh_var 0.75 (line_height_of "xs")

  let text_sm () =
    text_size_utility text_sm_var text_sm_lh_var 0.875 (line_height_of "sm")

  let text_base () =
    text_size_utility text_base_var text_base_lh_var 1.0 (line_height_of "base")

  let text_lg () =
    text_size_utility text_lg_var text_lg_lh_var 1.125 (line_height_of "lg")

  let text_xl () =
    text_size_utility text_xl_var text_xl_lh_var 1.25 (line_height_of "xl")

  let text_2xl () =
    text_size_utility text_2xl_var text_2xl_lh_var 1.5 (line_height_of "2xl")

  let text_3xl () =
    text_size_utility text_3xl_var text_3xl_lh_var 1.875 (line_height_of "3xl")

  let text_4xl () =
    text_size_utility text_4xl_var text_4xl_lh_var 2.25 (line_height_of "4xl")

  let text_5xl () =
    text_size_utility text_5xl_var text_5xl_lh_var 3.0 (line_height_of "5xl")

  let text_6xl () =
    text_size_utility text_6xl_var text_6xl_lh_var 3.75 (line_height_of "6xl")

  let text_7xl () =
    text_size_utility text_7xl_var text_7xl_lh_var 4.5 (line_height_of "7xl")

  let text_8xl () =
    text_size_utility text_8xl_var text_8xl_lh_var 6.0 (line_height_of "8xl")

  let text_9xl () =
    text_size_utility text_9xl_var text_9xl_lh_var 8.0 (line_height_of "9xl")

  (* Font weight utilities set --tw-font-weight for animation but use theme var
     directly *)
  let font_weight_utility weight_var weight_value =
    let weight_theme_decl, weight_theme_ref =
      Var.binding weight_var weight_value
    in
    let weight_util_decl = Var.set font_weight_var (Css.Var weight_theme_ref) in
    (* Get @property rule for font-weight channel (needed for animations) *)
    let property_rules =
      match Var.property_rule font_weight_var with
      | None -> Css.empty
      | Some rule -> rule
    in
    style ~property_rules
      [
        weight_theme_decl;
        weight_util_decl;
        font_weight (Css.Var weight_theme_ref);
      ]

  let font_thin = font_weight_utility font_weight_thin_var (Weight 100.)

  let font_extralight =
    font_weight_utility font_weight_extralight_var (Weight 200.)

  let font_light = font_weight_utility font_weight_light_var (Weight 300.)
  let font_normal = font_weight_utility font_weight_normal_var (Weight 400.)
  let font_medium = font_weight_utility font_weight_medium_var (Weight 500.)
  let font_semibold = font_weight_utility font_weight_semibold_var (Weight 600.)
  let font_bold () = font_weight_utility font_weight_bold_var (Weight 700.)

  let font_extrabold =
    font_weight_utility font_weight_extrabold_var (Weight 800.)

  let font_black = font_weight_utility font_weight_black_var (Weight 900.)

  let font_serif =
    let serif_decl, serif_ref =
      Var.binding font_serif_var default_serif_stack
    in
    style [ serif_decl; font_family (Css.Var serif_ref) ]

  let font_mono =
    let mono_decl, mono_ref = Var.binding font_mono_var default_mono_stack in
    style [ mono_decl; font_family (Css.Var mono_ref) ]

  (* Font family utilities use the font variables directly *)
  let font_sans =
    let sans_decl, sans_ref = Var.binding font_sans_var default_sans_stack in
    style [ sans_decl; font_family (Css.Var sans_ref) ]

  (* [font-source] reads --font-source from the theme; the token has to be
     there, or a stray source word would parse as a utility.

     An [@theme inline] token has no declaration of its own, so the utility
     carries the value instead of a reference to it. A self-referential one
     ([--font-a: var(--font-a)]) is the exception: inlining it would leave the
     reference dangling, so the token keeps its declaration. *)
  (* A family the project declared also carries the [--font-<name>--font-
     feature-settings] beside it, the way Tailwind emits it. *)
  (* The feature and variation settings a project declares beside a family, as
     [--font-<name>--font-feature-settings]: each is a token of its own that
     the utility reads, or the value itself when the block is inline. *)
  let font_feature_decls theme name =
    let sub : type a.
        string ->
        (Cascade.Cursor.t -> a) ->
        a Var.theme ->
        (a -> Css.declaration) ->
        (a Css.var -> Css.declaration) ->
        Css.declaration list =
     fun suffix read var property property_of_var ->
      let token = String.concat "" [ "font-"; name; "--"; suffix ] in
      match Scheme.theme_value (Some theme) token with
      | None -> []
      | Some raw -> (
          match
            Cascade.Cursor.try_parse_full_err read
              (Cascade.Cursor.of_string raw)
          with
          | Error _ -> []
          | Ok value ->
              if Scheme.is_inline_token theme token then [ property value ]
              else
                let decl, ref_ = Var.binding var value in
                [ decl; property_of_var ref_ ])
    in
    sub "font-feature-settings" Css.Properties.read_font_feature_settings
      (font_named_features_var name) font_feature_settings (fun v ->
        font_feature_settings (Var v))
    @ sub "font-variation-settings" Css.Properties.read_font_variation_settings
        (font_named_variations_var name) font_variation_settings (fun v ->
          font_variation_settings (Var v))

  let font_weight_theme theme name =
    match Scheme.theme_value (Some theme) ("font-weight-" ^ name) with
    | None -> style []
    | Some raw -> (
        match Parse.decimal_int raw with
        | None -> style []
        | Some n ->
            font_weight_utility
              (font_weight_named_var name)
              (Css.Weight (float_of_int n)))

  let font_named ?fallback theme name =
    let token = "font-" ^ name in
    match Scheme.theme_value (Some theme) token with
    | None -> ( match fallback with Some s -> s | None -> style [])
    | Some raw -> (
        match Css.parse_font_family raw with
        | None -> ( match fallback with Some s -> s | None -> style [])
        | Some family ->
            let self_referential =
              match family with
              | Css.Var v -> Css.var_name v = token
              | _ -> false
            in
            let features = font_feature_decls theme name in
            if Scheme.is_inline_token theme token && not self_referential then
              style (font_family family :: features)
            else
              let decl, ref = Var.binding (font_named_var name) family in
              style (decl :: font_family (Css.Var ref) :: features))

  let italic = style [ font_style Italic ]
  let not_italic = style [ font_style Normal ]
  let text_left = style [ text_align Left ]
  let text_center = style [ text_align Center ]
  let text_right = style [ text_align Right ]
  let text_justify = style [ text_align Justify ]
  let text_start = style [ text_align Start ]
  let text_end = style [ text_align End ]

  let leading_with_theme_var theme_var default_value =
    let theme_decl, theme_ref = Var.binding theme_var default_value in
    let channel_decl = Var.set leading_var (Css.Var theme_ref) in
    let property_rules =
      Var.property_rule leading_var |> Option.to_list |> Css.concat
    in
    style ~property_rules
      [ theme_decl; channel_decl; line_height (Css.Var theme_ref) ]

  let leading_theme theme name =
    match Scheme.theme_value (Some theme) ("leading-" ^ name) with
    | None -> style []
    | Some raw -> (
        match parse_bracket_leading raw with
        | Stdlib.Option.None -> style []
        | Stdlib.Option.Some lh ->
            leading_with_theme_var (leading_named_var name) lh)

  let leading_none ?theme () =
    match Scheme.theme_value theme "leading-none" with
    | Some _ -> leading_with_theme_var leading_none_var (Num 1.0)
    | None ->
        (* Tailwind v4.3 ships no --leading-none token, so inline the literal
           rather than minting a var. *)
        let value : line_height = Num 1.0 in
        let channel_decl = Var.set leading_var value in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; line_height value ]

  let leading_tight = leading_with_theme_var leading_tight_var (Num 1.25)
  let leading_snug = leading_with_theme_var leading_snug_var (Num 1.375)
  let leading_normal = leading_with_theme_var leading_normal_var (Num 1.5)
  let leading_relaxed = leading_with_theme_var leading_relaxed_var (Num 1.625)
  let leading_loose = leading_with_theme_var leading_loose_var (Num 2.0)

  (* Shared by the int base ([leading]) and its half-step sibling ([leading']):
     [n] is the class number (spacing-scale multiplier), an integer or a
     fractional quarter-step alike. *)
  let leading_calc ?theme (n : float) =
    let name = "leading-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25)) in
    match Scheme.theme_value theme name with
    | Some _ ->
        (* A theme overrides --leading-N: reference it like a named leading. *)
        let theme_var = Var.theme Css.Line_height name ~order:(6, 53) in
        leading_with_theme_var theme_var (Rem (n *. 0.25))
    | None when n = 0.0 ->
        (* leading-0 is a literal 0, not calc(var(--spacing) * 0). *)
        let value : line_height = Num 0.0 in
        let channel_decl = Var.set leading_var value in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; line_height value ]
    | None ->
        (* Tailwind v4.3 default: derive numeric leading from the spacing scale,
           leading-1 -> var(--spacing), leading-N -> calc(var(--spacing) *
           N). *)
        let spacing_decl = Var.set Theme.spacing_var Theme.spacing_base in
        let spacing = Var.name Theme.spacing_var in
        let value : line_height =
          if n = 1.0 then Css.Var (Var.theme_ref spacing)
          else Css.Calc (Css.Calc.mul (Css.Calc.var spacing) (Css.Calc.float n))
        in
        let channel_decl = Var.set leading_var value in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ spacing_decl; channel_decl; line_height value ]

  let leading ?theme n = leading_calc ?theme (float_of_int n)
  let leading' ?theme n = leading_calc ?theme n

  (* The leading scale, published the same way. *)
  let () =
    List.iter
      (fun (name, value) ->
        Scheme.register_default_token ("leading-" ^ name)
          (Css.Pp.to_string ~minify:true Css.Properties.pp_line_height value))
      [
        ("tight", (Num 1.25 : Css.line_height));
        ("snug", Num 1.375);
        ("normal", Num 1.5);
        ("relaxed", Num 1.625);
        ("loose", Num 2.0);
      ]

  (* Publish the text scale through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [theme()] in a project's CSS resolves against
     the same values the utilities use. *)
  let () =
    List.iter
      (fun (name, _var, rem) ->
        Scheme.register_default_token ("text-" ^ name)
          (Css.Pp.to_string Css.pp_length (Css.Rem rem)))
      text_size_data

  (* And each size's line-height, which [theme(static)] declares with the rest
     of the theme. *)
  let () =
    List.iter
      (fun (name, _var, rem) ->
        Scheme.register_default_token
          ("text-" ^ name ^ "--line-height")
          (Css.Pp.to_string ~minify:true Css.Properties.pp_line_height
             (text_line_height ~size_rem:rem (line_height_of name))))
      text_size_data

  (* A theme token's value is text; read it back at the type the property
     wants. *)
  let parse_theme_line_height theme token =
    match Scheme.theme_value (Some theme) token with
    | None -> Stdlib.Option.None
    | Some raw -> (
        let cursor = Cascade.Cursor.of_string raw in
        match
          Cascade.Cursor.try_parse_full_err Css.Properties.read_line_height
            cursor
        with
        | Ok lh -> Stdlib.Option.Some lh
        | Error _ -> Stdlib.Option.None)

  (* Both tables are compile-time constants: building them per call allocated
     the list and a second remapped copy once per utility rendered. *)
  let named_leading_table :
      (string * (Css.line_height Var.theme * Css.line_height)) list =
    [
      (* [none] is absent for the reason [known_leading_names] gives. *)
      ("tight", (leading_tight_var, (Num 1.25 : Css.line_height)));
      ("snug", (leading_snug_var, Num 1.375));
      ("normal", (leading_normal_var, Num 1.5));
      ("relaxed", (leading_relaxed_var, Num 1.625));
      ("loose", (leading_loose_var, Num 2.0));
    ]

  let text_size_table = List.map (fun (n, v, d) -> (n, (v, d))) text_size_data

  (** Convert a line-height modifier to (extra_declarations, line_height_value).
  *)
  let lh_modifier_to_css theme = function
    | Spacing n ->
        let spacing_decl, _spacing_ref =
          Var.binding Theme.spacing_var Theme.spacing_base
        in
        (* [--spacing] holds a length; here it is read as a line height, so the
           reference is built at that kind off the same handle. *)
        let lh_spacing_ref : Css.line_height Css.var =
          Var.bracket (Var.name Theme.spacing_var)
        in
        let lh : Css.line_height =
          Calc (Css.Calc.mul (Var lh_spacing_ref) (Num (float_of_int n)))
        in
        ([ spacing_decl ], lh)
    | No_leading -> ([], Num 1.0)
    | Named name -> (
        match List.assoc_opt name named_leading_table with
        | Stdlib.Option.Some (theme_var, default_val) ->
            let decl, ref_ = Var.binding theme_var default_val in
            ([ decl ], Var ref_)
        | Stdlib.Option.None -> (
            (* A [--leading-*] the project declared: bind it, so the theme layer
               carries the token the utility reads. *)
            match parse_theme_line_height theme ("leading-" ^ name) with
            | Stdlib.Option.Some lh ->
                let decl, ref_ = Var.binding (leading_named_var name) lh in
                ([ decl ], Css.Var ref_)
            | Stdlib.Option.None ->
                let ref_ : Css.line_height Css.var =
                  Var.bracket ("leading-" ^ name)
                in
                ([], Css.Var ref_)))
    | Bracket (_, lh) -> ([], lh)
    | Var_shorthand name -> ([], Css.Var (Var.bracket ("var(" ^ name ^ ")")))

  (* A size the project declared, with the line height, letter spacing and font
     weight the block may declare beside it as [--text-<name>--line-height] and
     its siblings. Each goes through its channel with the token as the fallback,
     as a built-in size's line height does, so [leading-*] on the same element
     still wins. A [/modifier] names the line height itself, and Tailwind then
     writes the size and that line height alone. *)
  let text_theme_decls ?(modified = false) theme name =
    let token = "text-" ^ name in
    (* The channel keeps the token as its fallback either way; an inline token
       puts its value there, as Tailwind writes [var(--tw-leading, 1.2)]. *)
    let sub : type a.
        string ->
        (Cascade.Cursor.t -> a) ->
        a Var.theme ->
        a Var.channel ->
        (a Css.var -> Css.declaration) ->
        Css.declaration list =
     fun suffix read var channel property ->
      let token = String.concat "" [ token; "--"; suffix ] in
      match Scheme.theme_value (Some theme) token with
      | None -> []
      | Some raw -> (
          match
            Cascade.Cursor.try_parse_full_err read
              (Cascade.Cursor.of_string raw)
          with
          | Error _ -> []
          | Ok value ->
              if Scheme.is_inline_token theme token then
                [ property (Var.reference_with_fallback channel value) ]
              else
                let decl = Var.set var value in
                [
                  decl;
                  property (Var.reference_with_var_fallback channel var value);
                ])
    in
    match Scheme.theme_value (Some theme) token with
    | None -> []
    | Some raw -> (
        match Css.parse_length raw with
        | None -> []
        | Some len ->
            let size =
              if Scheme.is_inline_token theme token then [ font_size len ]
              else
                let decl, ref = Var.binding (text_named_var name) len in
                [ decl; font_size (Css.Var ref) ]
            in
            if modified then size
            else
              size
              @ sub "line-height" Css.Properties.read_line_height
                  (text_named_lh_var name) leading_var (fun v ->
                    line_height (Var v))
              @ sub "letter-spacing"
                  (Css.Values.read_length ~allow_negative:true ~length_only:true)
                  (text_named_tracking_var name) Letter_spacing.tracking_var
                  (fun v -> letter_spacing (Var v))
              @ sub "font-weight" Css.Properties.read_font_weight
                  (text_named_weight_var name) font_weight_var (fun v ->
                    font_weight (Var v)))

  (** Generate font-size + line-height style for a named text size with
      modifier. *)
  let text_named_with_lh theme name lh_mod =
    match List.assoc_opt name text_size_table with
    | Stdlib.Option.Some (size_var, default_rem) ->
        let size_decl, size_ref = Var.binding size_var (Rem default_rem) in
        let lh_extra, lh_value = lh_modifier_to_css theme lh_mod in
        ( (size_decl :: lh_extra) @ [ font_size (Css.Var size_ref) ],
          [ line_height lh_value ] )
    | Stdlib.Option.None ->
        (* Fallback — should not happen for valid named sizes *)
        ([], [])

  (* font_size_keyword / try_parse_length_value / split_type_prefix are defined
     earlier in the module so of_class can validate arbitrary bracket values up
     front. *)

  (* is_valid_bracket_font_size is defined earlier (before of_class). *)

  (** Parse bracket content as font-size declarations (without line-height).
      Caller must have already passed [inner] through
      [is_valid_bracket_font_size]. *)
  let bracket_font_size_decls inner =
    let value, hinted = bracket_font_size_value inner in
    match parse_spacing_call value with
    | Stdlib.Option.Some n ->
        let decl, len = Theme.spacing_calc_float n in
        [ decl; font_size len ]
    | Stdlib.Option.None -> (
        match font_size_keyword value with
        | Stdlib.Option.Some kw -> [ Css.font_size_kw kw ]
        | Stdlib.Option.None -> (
            match bracket_font_size_length value ~hinted with
            | Stdlib.Option.Some fs_len -> [ font_size fs_len ]
            | Stdlib.Option.None ->
                if Parse.is_var value then
                  let bare = Parse.extract_var_name value in
                  [ font_size (Css.Var (Var.bracket bare)) ]
                else
                  invalid_arg
                    ("bracket_font_size_decls: not a valid font-size value: "
                   ^ inner)))

  (** Generate font-size-only style for bracket value. *)
  let bracket_font_size_style raw = style (bracket_font_size_decls raw)

  let to_style theme =
    let leading_none () = leading_none ~theme () in
    let leading n = leading ~theme n in
    let leading_step f = leading' ~theme f in
    function
    | Text_xs -> text_xs ()
    | Text_sm -> text_sm ()
    | Text_base -> text_base ()
    | Text_lg -> text_lg ()
    | Text_xl -> text_xl ()
    | Text_2xl -> text_2xl ()
    | Text_3xl -> text_3xl ()
    | Text_4xl -> text_4xl ()
    | Text_5xl -> text_5xl ()
    | Text_6xl -> text_6xl ()
    | Text_7xl -> text_7xl ()
    | Text_8xl -> text_8xl ()
    | Text_9xl -> text_9xl ()
    | Font_thin -> font_thin
    | Font_extralight -> font_extralight
    | Font_light -> font_light
    | Font_normal -> font_normal
    | Font_medium -> font_medium
    | Font_semibold -> font_semibold
    | Font_bold -> font_bold ()
    | Font_extrabold -> font_extrabold
    | Font_black -> font_black
    | Font_bracket_weight (_, n) ->
        let weight_util_decl =
          Var.set font_weight_var (Weight (float_of_int n))
        in
        let property_rules =
          match Var.property_rule font_weight_var with
          | None -> Css.empty
          | Some rule -> rule
        in
        style ~property_rules
          [ weight_util_decl; font_weight (Weight (float_of_int n)) ]
    | Font_bracket_weight_var (_, var_str) ->
        let bare_name = Parse.extract_var_name var_str in
        let var_ref : Css.font_weight Css.var = Var.bracket bare_name in
        let weight_util_decl = Var.set font_weight_var (Css.Var var_ref) in
        let property_rules =
          match Var.property_rule font_weight_var with
          | None -> Css.empty
          | Some rule -> rule
        in
        style ~property_rules [ weight_util_decl; font_weight (Var var_ref) ]
    | Font_bracket_family_quoted (_, decoded) ->
        let n = String.length decoded in
        if n >= 2 && decoded.[0] = '"' && decoded.[n - 1] = '"' then
          (* font-["arial_rounded"] → the decoded text is one quoted family
             name; strip the quotes cascade's own printer adds back. *)
          style [ font_family (Name (String.sub decoded 1 (n - 2))) ]
        else
          (* font-["liga"_0x10] → not a single quoted name. The decoded text
             already carries its own quoting (a quoted string mixed with other
             tokens); tokenize it and pass the tokens through rather than
             wrapping the whole string in one more layer of quotes. *)
          let tokens =
            Css.Values.read_invalid_value (Cascade.Cursor.of_string decoded)
          in
          style [ font_family (Invalid tokens) ]
    | Font_bracket_family_name (_, s) ->
        (* Each comma segment is its own [<family-name>] (CSS Fonts 4 sec. 2.1),
           so a generic keyword only matches a segment on its own, not the
           bracket as a whole; [font-[Papyrus,fantasy]] is the two-entry stack
           [Papyrus, fantasy], never the single literal name
           ["Papyrus,fantasy"]. A quoted segment is always a custom-ident
           literal, so it never resolves to a generic keyword either. *)
        let family_of_entry entry : Css.font_family =
          let entry = String.trim entry in
          let n = String.length entry in
          if
            n >= 2
            && (entry.[0] = '"' || entry.[0] = '\'')
            && entry.[n - 1] = entry.[0]
          then Css.Name (String.sub entry 1 (n - 2))
          else
            match entry with
            | "ui-sans-serif" -> Css.Ui_sans_serif
            | "ui-serif" -> Css.Ui_serif
            | "ui-monospace" -> Css.Ui_monospace
            | "ui-rounded" -> Css.Ui_rounded
            | "sans-serif" -> Css.Sans_serif
            | "serif" -> Css.Serif
            | "monospace" -> Css.Monospace
            | "cursive" -> Css.Cursive
            | "fantasy" -> Css.Fantasy
            | "system-ui" -> Css.System_ui
            | "emoji" -> Css.Emoji
            | "math" -> Css.Math
            | name -> Css.Name name
        in
        let family =
          match String.split_on_char ',' s with
          | [ single ] -> family_of_entry single
          | entries -> Css.List (List.map family_of_entry entries)
        in
        style [ font_family family ]
    | Font_bracket_family_var (_, var_str) ->
        let bare_name = Parse.extract_var_name var_str in
        let var_ref : Css.font_family Css.var = Var.bracket bare_name in
        style [ font_family (Var var_ref) ]
    | Font_features_raw (_, v) ->
        style
          (Option.to_list (Parse.opaque_declaration "font-feature-settings" v))
    | Font_features_quoted s -> (
        match
          Css.parse_declaration "font-feature-settings"
            (Parse.decode_underscores s)
        with
        | Some decl -> style [ decl ]
        | None -> style [])
    | Font_features_var var_str ->
        let bare_name = Parse.extract_var_name var_str in
        let var_ref : Css.font_feature_settings Css.var =
          Var.bracket bare_name
        in
        style [ font_feature_settings (Var var_ref) ]
    | Font_features_bare_var v ->
        (* bare_var_inner "(--my-features)" → "--my-features"; strip the --
           prefix since var_ref adds it *)
        let inner = Parse.bare_var_inner v in
        let bare_name = Option.value ~default:inner (Parse.bare_name inner) in
        let var_ref : Css.font_feature_settings Css.var =
          Var.bracket bare_name
        in
        style [ font_feature_settings (Var var_ref) ]
    | Font_named (name, _) -> font_named theme name
    | Font_weight_theme name -> font_weight_theme theme name
    | Leading_theme name -> leading_theme theme name
    | Font_sans _ -> font_named ~fallback:font_sans theme "sans"
    | Font_serif _ -> font_named ~fallback:font_serif theme "serif"
    | Font_mono _ -> font_named ~fallback:font_mono theme "mono"
    | Italic -> italic
    | Not_italic -> not_italic
    | Text_left -> text_left
    | Text_center -> text_center
    | Text_right -> text_right
    | Text_justify -> text_justify
    | Text_start -> text_start
    | Text_end -> text_end
    | Leading_none -> leading_none ()
    | Leading_tight -> leading_tight
    | Leading_snug -> leading_snug
    | Leading_normal -> leading_normal
    | Leading_relaxed -> leading_relaxed
    | Leading_loose -> leading_loose
    | Leading n -> leading n
    | Leading_step f -> leading_step f
    | Leading_var v ->
        let bare_name = Parse.extract_var_name v in
        let var_ref : Css.line_height Css.var = Var.bracket bare_name in
        let channel_decl = Var.set leading_var (Css.Var var_ref) in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; line_height (Css.Var var_ref) ]
    | Leading_bracket (_, lh) ->
        let channel_decl = Var.set leading_var lh in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        style ~property_rules [ channel_decl; line_height lh ]
    | Leading_raw (_, value) ->
        let channel_decl =
          Css.custom_property ~layer:"utilities" (Var.css_name leading_var)
            value
        in
        let property_rules =
          Var.property_rule leading_var |> Option.to_list |> Css.concat
        in
        let declarations =
          channel_decl
          :: Option.to_list (Parse.opaque_declaration "line-height" value)
        in
        style
          ~metadata:[ Var.metadata leading_var ]
          ~property_rules declarations
    | Text_named_lh (name, lh_mod) ->
        let fs_decl, lh_decls = text_named_with_lh theme name lh_mod in
        style (fs_decl @ lh_decls)
    | Text_theme name -> style (text_theme_decls theme name)
    | Text_theme_lh (name, lh_mod) ->
        let lh_extra, lh_value = lh_modifier_to_css theme lh_mod in
        style
          (text_theme_decls ~modified:true theme name
          @ lh_extra
          @ [ line_height lh_value ])
    | Text_bracket_fs raw -> bracket_font_size_style raw
    | Text_bracket_fs_raw (_, value) ->
        style (Option.to_list (Parse.opaque_declaration "font-size" value))
    | Text_bracket_fs_lh (raw, lh_mod) ->
        let fs_decls = bracket_font_size_decls raw in
        let lh_extra, lh_value = lh_modifier_to_css theme lh_mod in
        style (fs_decls @ lh_extra @ [ line_height lh_value ])

  let examples =
    [
      Text_base; Font_normal; Font_sans false; Italic; Text_center; Leading_none;
    ]
end

module Early_utility = Utility.Make (Typography_early)

(* Public API *)

let utility_early = Early_utility.v
let text_xs = utility_early Typography_early.Text_xs
let text_sm = utility_early Typography_early.Text_sm
let text_base = utility_early Typography_early.Text_base
let text_lg = utility_early Typography_early.Text_lg
let text_xl = utility_early Typography_early.Text_xl
let text_2xl = utility_early Typography_early.Text_2xl
let text_3xl = utility_early Typography_early.Text_3xl
let text_4xl = utility_early Typography_early.Text_4xl
let text_5xl = utility_early Typography_early.Text_5xl
let text_6xl = utility_early Typography_early.Text_6xl
let text_7xl = utility_early Typography_early.Text_7xl
let text_8xl = utility_early Typography_early.Text_8xl
let text_9xl = utility_early Typography_early.Text_9xl
let font_medium = utility_early Typography_early.Font_medium
let font_thin = utility_early Typography_early.Font_thin
let font_extralight = utility_early Typography_early.Font_extralight
let font_light = utility_early Typography_early.Font_light
let font_normal = utility_early Typography_early.Font_normal
let font_semibold = utility_early Typography_early.Font_semibold
let font_bold = utility_early Typography_early.Font_bold
let font_extrabold = utility_early Typography_early.Font_extrabold
let font_black = utility_early Typography_early.Font_black
let font_mono = utility_early (Typography_early.Font_mono false)
let font_sans = utility_early (Typography_early.Font_sans false)
let font_serif = utility_early (Typography_early.Font_serif false)
let italic = utility_early Typography_early.Italic
let not_italic = utility_early Typography_early.Not_italic
let text_left = utility_early Typography_early.Text_left
let text_center = utility_early Typography_early.Text_center
let text_right = utility_early Typography_early.Text_right
let text_justify = utility_early Typography_early.Text_justify
let text_start = utility_early Typography_early.Text_start
let text_end = utility_early Typography_early.Text_end
let leading_none = utility_early Typography_early.Leading_none
let leading_tight = utility_early Typography_early.Leading_tight
let leading_snug = utility_early Typography_early.Leading_snug
let leading_normal = utility_early Typography_early.Leading_normal
let leading_relaxed = utility_early Typography_early.Leading_relaxed
let leading_loose = utility_early Typography_early.Leading_loose
let leading n = utility_early (Typography_early.Leading n)
let leading' n = utility_early (Typography_early.Leading_step n)
