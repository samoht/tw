(** Divide utilities for creating gaps between child elements

    @see <https://tailwindcss.com/docs/divide-width>
      Tailwind CSS Divide Width documentation *)

(* Current scheme for default border width *)
let current_scheme : Scheme.t ref = ref Scheme.default

(* Set the current scheme for divide generation *)
let set_scheme scheme = current_scheme := scheme

module Handler = struct
  open Style

  let pp_int = Pp.int
  let pp_float = Pp.float

  open Css

  type t =
    | Divide_x of int (* divide-x = 1, divide-x-4 = 4 *)
    | Divide_y of int
    | Divide_x_arb of Css.border_width (* divide-x-[4px] *)
    | Divide_y_arb of Css.border_width
    | Divide_x_reverse
    | Divide_y_reverse
    | Divide_color of Color.color * int
    | Divide_color_opacity of Color.color * int * Color.opacity_modifier
    | Divide_transparent
    | Divide_current
    | Divide_current_opacity of Color.opacity_modifier
    | Divide_inherit
    | Divide_bracket_color of string * Css.color
    | Divide_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Divide_style of Css.border_style

  type Utility.base += Self of t

  let name = "divide"
  let priority = 5

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

  (* Reference to --tw-border-style (defined in borders.ml). Must use the same
     property_order:6 as borders.ml to avoid overwriting the registry. *)
  let border_style_var =
    Var.property_default Css.Border_style
      ~initial:(Solid : Css.border_style)
      ~property_order:6 ~family:`Border "tw-border-style"

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

  (* Divide color utilities use nested rules with :where(.divide-X >
     :not(:last-child)) We construct the full class name in the selector like
     space-x-reverse does. *)
  let divide_color_style color shade =
    let class_name =
      if Color.is_shadeless color then "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    if Color.is_custom_color color then
      let css_color = Color.to_css color shade in
      let rule = Css.rule ~selector [ Css.border_color css_color ] in
      style ~rules:(Some [ rule ]) []
    else
      let color_var =
        Color.property_color_var ~property_prefix:"border-color" color shade
      in
      let color_value =
        Color.property_color_value ~property_prefix:"border-color" color shade
      in
      let decl, color_ref = Var.binding color_var color_value in
      let rule =
        Css.rule ~selector [ decl; Css.border_color (Css.Var color_ref) ]
      in
      style ~rules:(Some [ rule ]) []

  let divide_transparent_style () =
    let selector =
      Css.Selector.(
        where
          [ Combined (Class "divide-transparent", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color (Css.hex "#0000") ] in
    style ~rules:(Some [ rule ]) []

  let divide_current_style () =
    let selector =
      Css.Selector.(
        where [ Combined (Class "divide-current", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color Css.Current ] in
    style ~rules:(Some [ rule ]) []

  let divide_inherit_style () =
    let selector =
      Css.Selector.(
        where [ Combined (Class "divide-inherit", Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color Css.Inherit ] in
    style ~rules:(Some [ rule ]) []

  let divide_bracket_color_style class_name inner c =
    let color =
      if String.length inner > 0 && inner.[0] = '#' then
        let shortened = Color.shorten_hex_str inner in
        Css.hex ("#" ^ shortened)
      else match Color.css_color_to_hex c with Some h -> h | None -> c
    in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    let rule = Css.rule ~selector [ Css.border_color color ] in
    style ~rules:(Some [ rule ]) []

  let divide_bracket_color_opacity_style class_name inner _c opacity =
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    let percent = Color.opacity_to_percent opacity in
    let alpha = percent /. 100.0 in
    if String.length inner > 0 && inner.[0] = '#' then
      match Color.hex_to_rgb (String.sub inner 1 (String.length inner - 1)) with
      | Some rgb ->
          let ok_l, ok_a, ok_b = Color.rgb_to_oklab rgb in
          let oklab_value = Css.oklaba_none_zeros ok_l ok_a ok_b alpha in
          let rule = Css.rule ~selector [ Css.border_color oklab_value ] in
          style ~rules:(Some [ rule ]) []
      | None ->
          let rule = Css.rule ~selector [ Css.border_color (Css.hex "#000") ] in
          style ~rules:(Some [ rule ]) []
    else
      let normalized = String.map (fun c -> if c = '_' then ' ' else c) inner in
      let css_color =
        match Css.parse_color normalized with
        | Some c -> c
        | None -> Css.hex "#000"
      in
      let hex_color =
        match Color.css_color_to_hex css_color with
        | Some h -> h
        | None -> css_color
      in
      let _ = alpha in
      let fallback_decl = Css.border_color hex_color in
      let oklab_color =
        Css.color_mix ~in_space:Oklab hex_color Css.Transparent
          ~percent1:percent
      in
      let oklab_decl = Css.border_color oklab_color in
      let supports_block =
        Css.supports ~condition:Color.color_mix_supports_condition
          [ Css.rule ~selector:(Css.Selector.class_ "_") [ oklab_decl ] ]
      in
      let rule = Css.rule ~selector [ fallback_decl ] in
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
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    let decl, _ = Var.binding border_style_var bs in
    let rule = Css.rule ~selector [ decl; Css.border_style bs ] in
    style ~rules:(Some [ rule ]) []

  (* Format opacity modifier for class names *)
  let opacity_suffix = function
    | Color.No_opacity -> ""
    | Color.Opacity_percent p ->
        if Float.is_integer p then "/" ^ pp_int (int_of_float p)
        else "/" ^ pp_float p
    | Color.Opacity_bracket_percent p ->
        if Float.is_integer p then "/[" ^ pp_int (int_of_float p) ^ "%]"
        else "/[" ^ pp_float p ^ "%]"
    | Color.Opacity_arbitrary f -> "/[" ^ pp_float f ^ "]"
    | Color.Opacity_named name -> "/" ^ name
    | Color.Opacity_var v -> "/[" ^ v ^ "]"

  (* Divide color with opacity using Color helpers *)
  let divide_color_opacity_style color shade opacity =
    let base_class_name =
      if Color.is_shadeless color then "divide-" ^ Color.color_to_string color
      else "divide-" ^ Color.color_to_string color ^ "-" ^ string_of_int shade
    in
    let class_name = base_class_name ^ opacity_suffix opacity in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    Color.divide_with_opacity color shade opacity selector

  let divide_current_opacity_style opacity =
    let class_name = "divide-current" ^ opacity_suffix opacity in
    let selector =
      Css.Selector.(
        where [ Combined (Class class_name, Child, Not [ Last_child ]) ])
    in
    Color.divide_current_with_opacity opacity selector

  (* Helper to check if a string contains an opacity modifier *)
  let has_opacity s = String.contains s '/'

  let to_class = function
    | Divide_x n -> if n = 1 then "divide-x" else "divide-x-" ^ string_of_int n
    | Divide_y n -> if n = 1 then "divide-y" else "divide-y-" ^ string_of_int n
    | Divide_x_arb len -> (
        match len with
        | Px f ->
            let s = string_of_float f in
            let s =
              if String.ends_with ~suffix:"." s then
                String.sub s 0 (String.length s - 1)
              else s
            in
            "divide-x-[" ^ s ^ "px]"
        | _ -> "divide-x-[<length>]")
    | Divide_y_arb len -> (
        match len with
        | Px f ->
            let s = string_of_float f in
            let s =
              if String.ends_with ~suffix:"." s then
                String.sub s 0 (String.length s - 1)
              else s
            in
            "divide-y-[" ^ s ^ "px]"
        | _ -> "divide-y-[<length>]")
    | Divide_x_reverse -> "divide-x-reverse"
    | Divide_y_reverse -> "divide-y-reverse"
    | Divide_color (c, shade) ->
        if Color.is_shadeless c then "divide-" ^ Color.color_to_string c
        else "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
    | Divide_color_opacity (c, shade, opacity) ->
        if Color.is_shadeless c then
          "divide-" ^ Color.color_to_string c ^ opacity_suffix opacity
        else
          "divide-" ^ Color.color_to_string c ^ "-" ^ string_of_int shade
          ^ opacity_suffix opacity
    | Divide_transparent -> "divide-transparent"
    | Divide_current -> "divide-current"
    | Divide_current_opacity opacity ->
        "divide-current" ^ opacity_suffix opacity
    | Divide_inherit -> "divide-inherit"
    | Divide_bracket_color (v, _) -> "divide-[" ^ v ^ "]"
    | Divide_bracket_color_opacity (v, _, opacity) ->
        "divide-[" ^ v ^ "]/" ^ opacity_suffix opacity
    | Divide_style bs -> "divide-" ^ border_style_to_string bs

  let to_style = function
    | Divide_x n ->
        let class_name =
          if n = 1 then "divide-x" else "divide-x-" ^ string_of_int n
        in
        let w = if n = 1 then !current_scheme.default_border_width else n in
        divide_x_width_style ~class_name ~width:(Px (float_of_int w))
    | Divide_y n ->
        let class_name =
          if n = 1 then "divide-y" else "divide-y-" ^ string_of_int n
        in
        let w = if n = 1 then !current_scheme.default_border_width else n in
        divide_y_width_style ~class_name ~width:(Px (float_of_int w))
    | Divide_x_arb len ->
        let class_name = to_class (Divide_x_arb len) in
        divide_x_width_style ~class_name ~width:len
    | Divide_y_arb len ->
        let class_name = to_class (Divide_y_arb len) in
        divide_y_width_style ~class_name ~width:len
    | Divide_x_reverse -> divide_x_reverse_style ()
    | Divide_y_reverse -> divide_y_reverse_style ()
    | Divide_color (color, shade) -> divide_color_style color shade
    | Divide_color_opacity (color, shade, opacity) ->
        divide_color_opacity_style color shade opacity
    | Divide_transparent -> divide_transparent_style ()
    | Divide_current -> divide_current_style ()
    | Divide_current_opacity opacity -> divide_current_opacity_style opacity
    | Divide_inherit -> divide_inherit_style ()
    | Divide_bracket_color (inner, c) ->
        let class_name = to_class (Divide_bracket_color (inner, c)) in
        divide_bracket_color_style class_name inner c
    | Divide_bracket_color_opacity (inner, c, opacity) ->
        let class_name =
          to_class (Divide_bracket_color_opacity (inner, c, opacity))
        in
        divide_bracket_color_opacity_style class_name inner c opacity
    | Divide_style bs -> divide_style_style bs

  let suborder = function
    | Divide_x n -> -20000 + n
    | Divide_y n -> -10000 + n
    | Divide_x_arb _ -> -20000 + 50000
    | Divide_y_arb _ -> -10000 + 50000
    | Divide_x_reverse -> 0
    | Divide_y_reverse -> 1
    (* All divide color utilities use flat suborder for natural sort *)
    | Divide_color _ | Divide_color_opacity _ -> 70000
    | Divide_bracket_color _ | Divide_bracket_color_opacity _ -> 70000
    | Divide_current | Divide_current_opacity _ -> 70000
    | Divide_inherit -> 70000
    | Divide_transparent -> 70000
    | Divide_style _ -> -30000

  let parse_bracket_width s : Css.border_width option =
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      if String.ends_with ~suffix:"px" inner then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with Some f -> Some (Px f) | None -> None
      else if String.ends_with ~suffix:"rem" inner then
        let n = String.sub inner 0 (String.length inner - 3) in
        match float_of_string_opt n with Some f -> Some (Rem f) | None -> None
      else None
    else None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "divide"; "x" ] -> Ok (Divide_x 1)
    | [ "divide"; "y" ] -> Ok (Divide_y 1)
    | [ "divide"; "x"; "reverse" ] -> Ok Divide_x_reverse
    | [ "divide"; "y"; "reverse" ] -> Ok Divide_y_reverse
    | [ "divide"; "x"; value ] -> (
        match parse_bracket_width value with
        | Some w -> Ok (Divide_x_arb w)
        | None -> (
            match int_of_string_opt value with
            | Some n when n >= 0 -> Ok (Divide_x n)
            | _ -> Error (`Msg "Not a divide utility")))
    | [ "divide"; "y"; value ] -> (
        match parse_bracket_width value with
        | Some w -> Ok (Divide_y_arb w)
        | None -> (
            match int_of_string_opt value with
            | Some n when n >= 0 -> Ok (Divide_y n)
            | _ -> Error (`Msg "Not a divide utility")))
    | [ "divide"; "transparent" ] -> Ok Divide_transparent
    | [ "divide"; "inherit" ] -> Ok Divide_inherit
    | [ "divide"; style_str ]
      when Stdlib.Option.is_some (divide_style_of_string style_str) ->
        Ok (Divide_style (Stdlib.Option.get (divide_style_of_string style_str)))
    | [ "divide"; current_str ]
      when String.starts_with ~prefix:"current" current_str -> (
        let base, opacity = Color.parse_opacity_modifier current_str in
        match opacity with
        | Color.No_opacity when base = "current" -> Ok Divide_current
        | Color.No_opacity -> Error (`Msg ("Invalid divide: " ^ current_str))
        | _ -> Ok (Divide_current_opacity opacity))
    | [ "divide"; v ]
      when Parse.is_bracket_value (fst (Color.parse_opacity_modifier v)) ->
        let base_str, opacity = Color.parse_opacity_modifier v in
        let inner = Parse.bracket_inner base_str in
        let normalized =
          String.map (fun c -> if c = '_' then ' ' else c) inner
        in
        if
          (String.length inner > 0 && inner.[0] = '#')
          || Parse.is_css_color_fn normalized
        then
          let css_color =
            if String.length inner > 0 && inner.[0] = '#' then
              Some (Css.hex inner)
            else Css.parse_color normalized
          in
          match css_color with
          | Some c -> (
              match opacity with
              | Color.No_opacity -> Ok (Divide_bracket_color (inner, c))
              | _ -> Ok (Divide_bracket_color_opacity (inner, c, opacity)))
          | None -> Error (`Msg ("Invalid divide bracket color: " ^ inner))
        else Error (`Msg ("Invalid divide bracket value: " ^ inner))
    | "divide" :: color_parts when List.exists has_opacity color_parts -> (
        match Color.shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Divide_color_opacity (color, shade, opacity))
        | Error _ ->
            (* Try as theme-named color *)
            let name = String.concat "-" color_parts in
            let base, opacity = Color.parse_opacity_modifier name in
            if
              Var.theme_value ("color-" ^ base) <> None
              || Var.theme_value ("border-color-" ^ base) <> None
            then Ok (Divide_color_opacity (Theme_named base, 500, opacity))
            else Error (`Msg ("Invalid divide color: " ^ name)))
    | "divide" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Divide_color (color, shade))
        | Error _ ->
            (* Try as theme-named color - check both generic and property-scoped
               theme values *)
            let name = String.concat "-" color_parts in
            if
              Var.theme_value ("color-" ^ name) <> None
              || Var.theme_value ("border-color-" ^ name) <> None
            then Ok (Divide_color (Theme_named name, 500))
            else Error (`Msg ("Invalid divide color: " ^ name)))
    | _ -> Error (`Msg "Not a divide utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let divide_x_reverse = utility Divide_x_reverse
let divide_y_reverse = utility Divide_y_reverse
