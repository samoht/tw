(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | X of int (* divide-x = 1, divide-x-4 = 4 *)
    | Y of int
    (* The author's bracket text travels with the parsed width so that the class
       name is spelled exactly as it was written. *)
    | X_arb of string * Css.border_width (* divide-x-[4px] *)
    | Y_arb of string * Css.border_width
    | X_reverse
    | Y_reverse
    | Named_color of Color.color * int
    | Named_color_opacity of Color.color * int * Color.opacity_modifier
    | Transparent
    | Current
    | Current_opacity of Color.opacity_modifier
    | Inherit
    | Bracket_color of string * Css.color
    | Bracket_color_opacity of string * Css.color * Color.opacity_modifier
    | Line_style of Css.border_style

  type Utility.base += Self of t

  let name = "divide"

  (* The divide properties follow gap and precede self-alignment. The unranked
     [divide-x-reverse] custom property sits between backface and
     perspective. *)
  let priority = function X_reverse -> 38 | _ -> 17

  (* CSS Variables for divide reverse. Property order 4/5 places these BEFORE
     --tw-border-style (order 6) in within-utility sorting, which determines
     first-usage order. Must use ~family:`Border so they sort together with
     --tw-border-style by property_order rather than by family-order. *)
  let divide_x_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:4 ~family:`Border "tw-divide-x-reverse"

  let divide_y_reverse_var =
    Var.property_default Css.Number_percentage ~initial:(Num 0.0)
      ~universal:true ~property_order:5 ~family:`Border "tw-divide-y-reverse"

  (* [--tw-border-style] is one custom property with one slot in the properties
     layer. Borders declares it; reading its handle here is what keeps the two
     families pointing at the same declaration. *)
  let border_style_var = Borders.border_style_var

  (* {2 Divide Width Utilities} *)

  let divide_x_width_style ~class_name ~(width : Css.border_width) =
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let reverse_decl, reverse_ref =
      Var.binding divide_x_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let border_style_ref = Var.reference border_style_var in
    let start_width : Css.border_width =
      Calc Css.Calc.(mul (Val width) (var reverse_var_name))
    in
    let end_width : Css.border_width =
      Calc
        Css.Calc.(
          mul (Val width) (nested (sub (Num 1.0) (var reverse_var_name))))
    in
    let property_rules =
      [
        Var.property_rule divide_x_reverse_var;
        Var.property_rule border_style_var;
      ]
      |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          reverse_decl;
          border_inline_style (Var border_style_ref);
          border_inline_start_width start_width;
          border_inline_end_width end_width;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  let divide_y_width_style ~class_name ~(width : Css.border_width) =
    let selector =
      Css.Selector.(where [ class_ class_name >> not [ Last_child ] ])
    in
    let reverse_decl, reverse_ref =
      Var.binding divide_y_reverse_var (Css.Num 0.0)
    in
    let reverse_var_name = Css.var_name reverse_ref in
    let border_style_ref = Var.reference border_style_var in
    let start_width : Css.border_width =
      Calc Css.Calc.(mul (Val width) (var reverse_var_name))
    in
    let end_width : Css.border_width =
      Calc
        Css.Calc.(
          mul (Val width) (nested (sub (Num 1.0) (var reverse_var_name))))
    in
    let property_rules =
      [
        Var.property_rule divide_y_reverse_var;
        Var.property_rule border_style_var;
      ]
      |> List.filter_map Fun.id
    in
    let rule =
      Css.rule ~selector
        [
          reverse_decl;
          border_bottom_style (Var border_style_ref);
          border_top_style (Var border_style_ref);
          border_top_width start_width;
          border_bottom_width end_width;
        ]
    in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* divide-x-reverse utility sets --tw-divide-x-reverse: 1 on children *)
  let divide_x_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-x-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_x_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_x_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* divide-y-reverse utility sets --tw-divide-y-reverse: 1 on children *)
  let divide_y_reverse_style () =
    let selector =
      Css.Selector.(where [ class_ "divide-y-reverse" >> not [ Last_child ] ])
    in
    let decl, _ = Var.binding divide_y_reverse_var (Css.Num 1.0) in
    let property_rules =
      [ Var.property_rule divide_y_reverse_var ] |> List.filter_map Fun.id
    in
    let rule = Css.rule ~selector [ decl ] in
    style ~rules:(Some [ rule ]) ~property_rules:(Css.concat property_rules) []

  (* [:where(.CLASS > :not(:last-child))], the selector every divide utility
     hangs its declaration on. *)
  let divide_children_selector class_name =
    Css.Selector.(
      where [ Combined (Class class_name, Child, Not [ Last_child ]) ])

  (* Divide color utilities use nested rules with :where(.divide-X >
     :not(:last-child)) We construct the full class name in the selector like
     space-x-reverse does. *)
  let divide_color_style ?theme color shade =
    let class_name =
      if Color.is_shadeless color then "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let selector = divide_children_selector class_name in
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      let rule = Css.rule ~selector [ Css.border_color css_color ] in
      style ~rules:(Some [ rule ]) []
    else
      let color_var =
        Color.property_color_var ?theme ~property_prefix:"border-color" color
          shade
      in
      let color_value =
        Color.property_color_value ?theme ~property_prefix:"border-color" color
          shade
      in
      let decl, color_ref = Var.binding color_var color_value in
      let rule =
        Css.rule ~selector [ decl; Css.border_color (Css.Var color_ref) ]
      in
      style ~rules:(Some [ rule ]) []

  let divide_transparent_style () =
    let selector = divide_children_selector "divide-transparent" in
    let rule = Css.rule ~selector [ Css.border_color (Css.hex "#0000") ] in
    style ~rules:(Some [ rule ]) []

  let divide_current_style () =
    let selector = divide_children_selector "divide-current" in
    let rule = Css.rule ~selector [ Css.border_color Css.Current ] in
    style ~rules:(Some [ rule ]) []

  let divide_inherit_style () =
    let selector = divide_children_selector "divide-inherit" in
    let rule = Css.rule ~selector [ Css.border_color Css.Inherit ] in
    style ~rules:(Some [ rule ]) []

  let divide_bracket_color_style class_name inner c =
    let color =
      if String.length inner > 0 && inner.[0] = '#' then
        let shortened = Color.shorten_hex_str inner in
        Css.hex ("#" ^ shortened)
      else match Color.css_color_to_hex c with Some h -> h | None -> c
    in
    let selector = divide_children_selector class_name in
    let rule = Css.rule ~selector [ Css.border_color color ] in
    style ~rules:(Some [ rule ]) []

  (* An opacity modifier applies to the colour the bracket was parsed into. The
     modifier read the bracket text back through the CSS colour parser and
     answered black whenever that failed, and it hung the [@supports] rule on
     the bare class rather than on the children the utility borders. *)
  let divide_bracket_color_opacity_style class_name c opacity =
    let selector = divide_children_selector class_name in
    match Color.bracket_color_opacity c opacity with
    | Color.Folded value ->
        let rule = Css.rule ~selector [ Css.border_color value ] in
        style ~rules:(Some [ rule ]) []
    | Color.Guarded { fallback; mixed } ->
        let rule = Css.rule ~selector [ Css.border_color fallback ] in
        let supports_block =
          Css.supports ~condition:Color.color_mix_supports_condition
            [ Css.rule ~selector [ Css.border_color mixed ] ]
        in
        style ~rules:(Some [ rule; supports_block ]) []

  let divide_style_of_string (s : string) =
    let open Css in
    let r : border_style option =
      match s with
      | "dashed" -> Some Dashed
      | "dotted" -> Some Dotted
      | "double" -> Some Double
      | "none" -> Some None
      | "solid" -> Some Solid
      | _ -> Stdlib.Option.none
    in
    r

  let border_style_to_string (bs : Css.border_style) =
    match bs with
    | Dashed -> "dashed"
    | Dotted -> "dotted"
    | Double -> "double"
    | None -> "none"
    | Solid -> "solid"
    | _ -> "solid"

  let divide_style_style (bs : Css.border_style) =
    let name = border_style_to_string bs in
    let class_name = "divide-" ^ name in
    let selector = divide_children_selector class_name in
    let decl, _ = Var.binding border_style_var bs in
    let rule = Css.rule ~selector [ decl; Css.border_style bs ] in
    style ~rules:(Some [ rule ]) []

  (* Divide color with opacity using Color helpers *)
  let divide_color_opacity_style ?theme color shade opacity =
    let base_class_name =
      if Color.is_shadeless color then "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let class_name = base_class_name ^ Color.opacity_suffix opacity in
    let selector = divide_children_selector class_name in
    Color.divide_with_opacity ?theme color shade opacity selector

  let divide_current_opacity_style opacity =
    let class_name = "divide-current" ^ Color.opacity_suffix opacity in
    let selector = divide_children_selector class_name in
    Color.divide_current_with_opacity opacity selector

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  let to_class = function
    | X n -> if n = 1 then "divide-x" else "divide-x-" ^ string_of_int n
    | Y n -> if n = 1 then "divide-y" else "divide-y-" ^ string_of_int n
    | X_arb (spelling, _) -> "divide-x-[" ^ spelling ^ "]"
    | Y_arb (spelling, _) -> "divide-y-[" ^ spelling ^ "]"
    | X_reverse -> "divide-x-reverse"
    | Y_reverse -> "divide-y-reverse"
    | Named_color (c, shade) ->
        if Color.is_shadeless c then "divide-" ^ Color.color_to_string c
        else "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Named_color_opacity (c, shade, opacity) ->
        if Color.is_shadeless c then
          "divide-" ^ Color.color_to_string c ^ Color.opacity_suffix opacity
        else
          "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
          ^ Color.opacity_suffix opacity
    | Transparent -> "divide-transparent"
    | Current -> "divide-current"
    | Current_opacity opacity -> "divide-current" ^ Color.opacity_suffix opacity
    | Inherit -> "divide-inherit"
    | Bracket_color (v, _) -> "divide-[" ^ v ^ "]"
    | Bracket_color_opacity (v, _, opacity) ->
        "divide-[" ^ v ^ "]" ^ Color.opacity_suffix opacity
    | Line_style bs -> "divide-" ^ border_style_to_string bs

  let to_style theme =
    let divide_color_style color shade =
      divide_color_style ~theme color shade
    in
    let divide_color_opacity_style color shade opacity =
      divide_color_opacity_style ~theme color shade opacity
    in
    function
    | X n ->
        let class_name =
          if n = 1 then "divide-x" else "divide-x-" ^ string_of_int n
        in
        let w = if n = 1 then theme.Scheme.default_border_width else n in
        divide_x_width_style ~class_name ~width:(Px (float_of_int w))
    | Y n ->
        let class_name =
          if n = 1 then "divide-y" else "divide-y-" ^ string_of_int n
        in
        let w = if n = 1 then theme.Scheme.default_border_width else n in
        divide_y_width_style ~class_name ~width:(Px (float_of_int w))
    | X_arb (spelling, width) ->
        let class_name = to_class (X_arb (spelling, width)) in
        divide_x_width_style ~class_name ~width
    | Y_arb (spelling, width) ->
        let class_name = to_class (Y_arb (spelling, width)) in
        divide_y_width_style ~class_name ~width
    | X_reverse -> divide_x_reverse_style ()
    | Y_reverse -> divide_y_reverse_style ()
    | Named_color (color, shade) -> divide_color_style color shade
    | Named_color_opacity (color, shade, opacity) ->
        divide_color_opacity_style color shade opacity
    | Transparent -> divide_transparent_style ()
    | Current -> divide_current_style ()
    | Current_opacity opacity -> divide_current_opacity_style opacity
    | Inherit -> divide_inherit_style ()
    | Bracket_color (inner, c) ->
        let class_name = to_class (Bracket_color (inner, c)) in
        divide_bracket_color_style class_name inner c
    | Bracket_color_opacity (inner, c, opacity) ->
        let class_name = to_class (Bracket_color_opacity (inner, c, opacity)) in
        divide_bracket_color_opacity_style class_name c opacity
    | Line_style bs -> divide_style_style bs

  (* Tailwind's order across the family, read off its own output: divide-x,
     divide-x-2, divide-y, divide-y-4, divide-y-reverse, the styles, the
     colours, and divide-x-reverse last. The 60k-66k range fits after gap and
     before self-alignment in the shared priority-17 band. *)
  let suborder = function
    (* The bare divide-x / divide-y (DEFAULT, n=1) sorts before the numbered
       variants: divide-x, divide-x-0, divide-x-4, ... The "-1" offset keeps the
       default ahead of divide-x-0 (n=0). *)
    | X 1 -> 59_999
    | X n -> 60_000 + min n 1_997
    | X_arb _ -> 61_998
    | Y 1 -> 61_999
    | Y n -> 62_000 + min n 1_997
    | Y_arb _ -> 63_998
    | Y_reverse -> 64_000
    | Line_style _ -> 65_000
    (* All divide color utilities use flat suborder for natural sort *)
    | Named_color _ | Named_color_opacity _ -> 66_000
    | Bracket_color _ | Bracket_color_opacity _ -> 66_000
    | Current | Current_opacity _ -> 66_000
    | Inherit -> 66_000
    | Transparent -> 66_000
    | X_reverse -> 1_700

  (* The bracket spelling of an arbitrary width, for the typed constructors,
     which are handed a width rather than the text an author wrote. cascade
     prints the width, so a unit it learns needs no table here; the keywords and
     the CSS functions have no bracket spelling at all. *)
  let bracket_spelling (width : Css.border_width) : string option =
    match width with
    (* [None] is exactly what the bracket reader refuses, so the typed
       constructor can build every class the parser accepts and no other. A
       sizing keyword is not a width, and a [var()] has no spelling of its
       own. *)
    | Auto | Max_content | Min_content | Fit_content | From_font | Var _ -> None
    (* The bracket is re-read as a width, and a bare [0] is not one, so the
       unitless zero takes the unit back. *)
    | Zero -> Some "0px"
    | width -> Some (Css.Pp.to_string Css.Properties.pp_border_width width)

  (* The bracket text and the width it denotes. The text is what [to_class]
     spells, so a class parsed here is reproduced verbatim. A divide width sets
     the same property a border width does, so it is read by the same reader. *)
  let parse_bracket_width s : (string * Css.border_width) option =
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      match Borders.parse_border_width inner with
      | Some width -> Some (inner, width)
      | None -> None
    else None

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "divide"; "x" ] -> Ok (X 1)
    | [ "divide"; "y" ] -> Ok (Y 1)
    | [ "divide"; "x"; "reverse" ] -> Ok X_reverse
    | [ "divide"; "y"; "reverse" ] -> Ok Y_reverse
    | [ "divide"; "x"; value ] -> (
        match parse_bracket_width value with
        | Some (spelling, w) -> Ok (X_arb (spelling, w))
        | None -> (
            match Parse.decimal_int value with
            | Some n when n >= 0 -> Ok (X n)
            | _ -> Error (`Msg "Not a divide utility")))
    | [ "divide"; "y"; value ] -> (
        match parse_bracket_width value with
        | Some (spelling, w) -> Ok (Y_arb (spelling, w))
        | None -> (
            match Parse.decimal_int value with
            | Some n when n >= 0 -> Ok (Y n)
            | _ -> Error (`Msg "Not a divide utility")))
    | [ "divide"; "transparent" ] -> Ok Transparent
    | [ "divide"; "inherit" ] -> Ok Inherit
    | [ "divide"; style_str ]
      when Stdlib.Option.is_some (divide_style_of_string style_str) ->
        Ok (Line_style (Stdlib.Option.get (divide_style_of_string style_str)))
    | [ "divide"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier ~theme current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Current
        | Color.No_opacity -> Error (`Msg ("Invalid divide: " ^ current_str))
        | _ -> Ok (Current_opacity opacity))
    | [ "divide"; v ]
      when Parse.is_bracket_value (fst (Color.parse_opacity_modifier ~theme v))
      -> (
        let base_str, opacity = Color.parse_opacity_modifier ~theme v in
        let inner = Parse.bracket_inner base_str in
        (* Every colour spelling CSS knows, not only a [#] hex and a colour
           function: a named colour and a keyword name a divide colour too. *)
        match Color.parse_bracket_color inner with
        | Some c -> (
            match opacity with
            | Color.No_opacity -> Ok (Bracket_color (inner, c))
            | _ -> Ok (Bracket_color_opacity (inner, c, opacity)))
        | None -> Error (`Msg ("Invalid divide bracket color: " ^ inner)))
    | "divide" :: color_parts when List.exists has_opacity color_parts -> (
        match Color.shade_and_opacity_of_strings ~theme color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Named_color_opacity (color, shade, opacity))
        | Error _ ->
            (* Try as theme-named color *)
            let name = String.concat "-" color_parts in
            let base, opacity = Color.parse_opacity_modifier ~theme name in
            if
              Scheme.theme_value (Some theme) ("color-" ^ base) <> None
              || Scheme.theme_value (Some theme) ("border-color-" ^ base)
                 <> None
            then Ok (Named_color_opacity (Theme_named base, 500, opacity))
            else Error (`Msg ("Invalid divide color: " ^ name)))
    | "divide" :: color_parts -> (
        match Color.shade_of_strings ~theme color_parts with
        | Ok (color, shade) -> Ok (Named_color (color, shade))
        | Error _ ->
            (* Try as theme-named color - check both generic and property-scoped
               theme values *)
            let name = String.concat "-" color_parts in
            if
              Scheme.theme_value (Some theme) ("color-" ^ name) <> None
              || Scheme.theme_value (Some theme) ("border-color-" ^ name)
                 <> None
            then Ok (Named_color (Theme_named name, 500))
            else Error (`Msg ("Invalid divide color: " ^ name)))
    | _ -> Error (`Msg "Not a divide utility")

  let examples = []
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let divide_x_reverse = utility X_reverse
let divide_y_reverse = utility Y_reverse

(** {1 Divide Width Utilities} *)

let divide_x n = utility (X n)
let divide_y n = utility (Y n)

let bracket_spelling ~name w =
  match Handler.bracket_spelling w with
  | Some spelling -> spelling
  | None -> invalid_arg (name ^ ": width has no arbitrary-value spelling")

let divide_x_length w =
  utility (X_arb (bracket_spelling ~name:"divide_x_length" w, w))

let divide_y_length w =
  utility (Y_arb (bracket_spelling ~name:"divide_y_length" w, w))

(** {1 Divide Colour Utilities} *)

let divide_color ?opacity ?(shade = 500) color =
  Color.check_shade ~utility:"divide_color" color shade;
  match opacity with
  | None -> utility (Named_color (color, shade))
  | Some pct ->
      utility (Named_color_opacity (color, shade, Color.opacity_of_int pct))

let divide_transparent = utility Transparent
let divide_current = utility Current
let divide_inherit = utility Inherit

(** {1 Divide Style Utilities} *)

let divide_style s = utility (Line_style s)
