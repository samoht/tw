(** Text shadow utilities for CSS text-shadow property. *)

(* Capture Pp.float before open Css shadows it *)
let pp_float = Pp.float

module Handler = struct
  open Style
  open Css

  type shape = S_2xs | S_xs | S_sm | S_md | S_lg

  (* Color in an arbitrary shadow value *)
  type arb_color = Arb_hex of string | Arb_var of string | Arb_none

  type t =
    | Text_shadow_none
    | Text_shadow_shape of shape
    | Text_shadow_shape_opacity of shape * Color.opacity_modifier
    | Text_shadow_color of Color.color * int
    | Text_shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Text_shadow_current
    | Text_shadow_current_opacity of Color.opacity_modifier
    | Text_shadow_inherit
    | Text_shadow_transparent
    | Text_shadow_bracket_hex of string
    | Text_shadow_bracket_hex_opacity of string * Color.opacity_modifier
    | Text_shadow_bracket_color_var of string
    | Text_shadow_bracket_cvar_opacity of string * Color.opacity_modifier
    | Text_shadow_bracket_shadow of string
    | Text_shadow_bracket_var of string
    | Text_shadow_arbitrary of string
    | Text_shadow_arbitrary_opacity of string * Color.opacity_modifier

  type Utility.base += Self of t

  let name = "text_shadow"
  let priority = 35

  let text_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:35 ~family:`Text_shadow
      Css.Color "tw-text-shadow-color"

  let text_shadow_alpha_var =
    Var.property_default Css.Float ~initial:100.0 ~property_order:36
      ~family:`Text_shadow "tw-text-shadow-alpha"

  let text_shadow_property_rules =
    [
      Var.property_rule text_shadow_color_var;
      Var.property_rule text_shadow_alpha_var;
    ]
    |> List.filter_map Fun.id |> Css.concat

  let shorten_hex = Color.shorten_hex_str

  let alpha_decl percent =
    Css.custom_property ~layer:"utilities" "--tw-text-shadow-alpha"
      (pp_float percent ^ "%")

  let color_mix_supports decls =
    Css.supports ~condition:Color.color_mix_supports_condition
      [ Css.rule ~selector:(Css.Selector.class_ "_") decls ]

  let relative_color_supports =
    Css.Supports.Property ("color", "lab(from red l a b)")

  let make_color_var vn : Css.color = Css.Var (Css.var_ref vn)

  (* ============ Parse arbitrary shadow ============ *)

  let parse_arbitrary_shadow (s : string) :
      (length * length * length option * arb_color) option =
    let normalized = String.map (fun c -> if c = '_' then ' ' else c) s in
    let parts = String.split_on_char ' ' normalized in
    let parse_length str : length option =
      let len = String.length str in
      if len >= 1 then (
        let num_end = ref 0 in
        while
          !num_end < len
          && (str.[!num_end] = '-'
             || str.[!num_end] = '.'
             || (str.[!num_end] >= '0' && str.[!num_end] <= '9'))
        do
          incr num_end
        done;
        let num_str = String.sub str 0 !num_end in
        let unit_str = String.sub str !num_end (len - !num_end) in
        match float_of_string_opt num_str with
        | Some n -> (
            match unit_str with
            | "px" -> Some (Px n)
            | "rem" -> Some (Rem n)
            | "em" -> Some (Em n)
            | "" when n = 0.0 -> Some Zero
            | _ -> Stdlib.Option.None)
        | Stdlib.Option.None -> Stdlib.Option.None)
      else Stdlib.Option.None
    in
    let rec find_color_and_lengths acc (parts : string list) :
        string list * arb_color =
      match parts with
      | [] -> (List.rev acc, Arb_none)
      | x :: _rest when String.length x > 0 && x.[0] = '#' ->
          (List.rev acc, Arb_hex x)
      | x :: _rest when String.length x > 4 && String.sub x 0 4 = "var(" ->
          (List.rev acc, Arb_var x)
      | x :: rest -> find_color_and_lengths (x :: acc) rest
    in
    let length_strs, color = find_color_and_lengths [] parts in
    let lengths = List.filter_map parse_length length_strs in
    match lengths with
    | [ h; v ] -> Some (h, v, Stdlib.Option.None, color)
    | [ h; v; blur ] -> Some (h, v, Some blur, color)
    | _ -> Stdlib.Option.None

  (* ============ Shape definitions ============ *)

  let shape_shadows (shape : shape) :
      (length * length * length option * string) list =
    match shape with
    | S_2xs -> [ ((Px 0. : length), Px 1., Some (Px 0.), "#0000001a") ]
    | S_xs -> [ ((Px 0. : length), Px 1., Some (Px 1.), "#0000001a") ]
    | S_sm ->
        [
          ((Px 0. : length), Px 1., Some (Px 2.), "#0000000f");
          (Px 0., Px 2., Some (Px 2.), "#0000000f");
        ]
    | S_md -> [ ((Px 0. : length), Px 2., Some (Px 4.), "#0000001a") ]
    | S_lg -> [ ((Px 0. : length), Px 4., Some (Px 8.), "#0000001a") ]

  let shape_text_shadow shape =
    let shadows = shape_shadows shape in
    let text_shadows =
      List.map
        (fun (h, v, blur, fallback_hex) ->
          let color_ref =
            Var.reference_with_fallback text_shadow_color_var
              (Css.hex fallback_hex)
          in
          Css.Text_shadow
            { h_offset = h; v_offset = v; blur; color = Some (Var color_ref) })
        shadows
    in
    match text_shadows with
    | [ single ] -> Css.text_shadow single
    | multiple -> Css.text_shadows multiple

  let shape_text_shadow_opacity shape opacity =
    let shadows = shape_shadows shape in
    let percent = Color.opacity_to_percent opacity in
    let alpha = percent /. 100.0 in
    let text_shadows =
      List.map
        (fun (h, v, blur, fallback_hex) ->
          let base_hex =
            if String.length fallback_hex = 9 then String.sub fallback_hex 0 7
            else fallback_hex
          in
          let oklab_fallback = Color.hex_to_oklab_alpha base_hex alpha in
          let color_ref =
            Var.reference_with_fallback text_shadow_color_var oklab_fallback
          in
          Css.Text_shadow
            { h_offset = h; v_offset = v; blur; color = Some (Var color_ref) })
        shadows
    in
    match text_shadows with
    | [ single ] -> Css.text_shadow single
    | multiple -> Css.text_shadows multiple

  (* ============ Color-setting styles ============ *)

  let color_hex c shade =
    let color_name = Color.scheme_color_name c shade in
    match Scheme.hex_color (Color.scheme ()) color_name with
    | Some h -> h
    | Stdlib.Option.None -> (
        match Var.theme_value ("color-" ^ color_name) with
        | Some h -> h
        | Stdlib.Option.None ->
            let oklch = Color.to_oklch c shade in
            let rgb = Color.oklch_to_rgb oklch in
            Color.rgb_to_hex rgb)

  let set_color c shade =
    let hex_value = color_hex c shade in
    let base_decl, _ = Var.binding text_shadow_color_var (Css.hex hex_value) in
    let theme_color_var = Color.color_var c shade in
    let theme_decl, color_ref =
      Var.binding theme_color_var (Css.hex hex_value)
    in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        (Css.Var color_ref) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_color_opacity c shade opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_value = color_hex c shade in
    let hex_with_alpha = Color.hex_with_alpha hex_value percent in
    let base_decl, _ =
      Var.binding text_shadow_color_var (Css.hex hex_with_alpha)
    in
    let theme_color_var = Color.color_var c shade in
    let theme_decl, color_ref =
      Var.binding theme_color_var (Css.hex hex_value)
    in
    let inner_mix =
      Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ theme_decl; enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_current () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Current in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        Css.Current Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_current_opacity opacity =
    let percent = Color.opacity_to_percent opacity in
    let base_decl, _ = Var.binding text_shadow_color_var Css.Current in
    let inner_mix =
      Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
        ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_transparent () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Transparent in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        Css.Transparent Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_inherit () =
    let base_decl, _ = Var.binding text_shadow_color_var Css.Inherit in
    style ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_hex hex =
    let short = shorten_hex ("#" ^ hex) in
    let base_decl, _ = Var.binding text_shadow_color_var (Css.hex short) in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        (Css.hex short) Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_hex_opacity hex opacity =
    let percent = Color.opacity_to_percent opacity in
    let hex_with_alpha = Color.hex_with_alpha hex percent in
    let base_decl, _ =
      Var.binding text_shadow_color_var (Css.hex hex_with_alpha)
    in
    let alpha = percent /. 100.0 in
    let oklab_color = Color.hex_to_oklab_alpha hex alpha in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        oklab_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_color_var var_expr =
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding text_shadow_color_var var_color in
    let enhanced_color =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        var_color Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var enhanced_color in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  let set_bracket_color_var_opacity var_expr opacity =
    let percent = Color.opacity_to_percent opacity in
    let var_name = Parse.extract_var_name var_expr in
    let var_color = make_color_var var_name in
    let base_decl, _ = Var.binding text_shadow_color_var var_color in
    let inner_mix =
      Css.color_mix ~in_space:Oklab var_color Css.Transparent ~percent1:percent
    in
    let outer_mix =
      Css.color_mix_var_percent ~in_space:Oklab ~var_name:"tw-text-shadow-alpha"
        inner_mix Css.Transparent
    in
    let enhanced_decl, _ = Var.binding text_shadow_color_var outer_mix in
    let supports_block = color_mix_supports [ enhanced_decl ] in
    style ~rules:(Some [ supports_block ])
      ~property_rules:text_shadow_property_rules [ base_decl ]

  (* ============ Arbitrary shadow styles ============ *)

  let make_text_shadow_var var_expr : Css.text_shadow =
    Css.Var (Css.var_ref (Parse.extract_var_name var_expr))

  let arbitrary_shadow_style arb =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) ->
        let fallback_color : Css.color =
          match color with
          | Arb_hex c -> Css.hex (shorten_hex c)
          | Arb_var v -> make_color_var (Parse.extract_var_name v)
          | Arb_none -> Css.Current
        in
        let color_ref =
          Var.reference_with_fallback text_shadow_color_var fallback_color
        in
        style ~property_rules:text_shadow_property_rules
          [
            Css.text_shadow
              (Css.Text_shadow
                 { h_offset; v_offset; blur; color = Some (Var color_ref) });
          ]
    | Stdlib.Option.None -> style [ Css.text_shadow Css.None ]

  let arbitrary_shadow_opacity_style arb opacity =
    match parse_arbitrary_shadow arb with
    | Some (h_offset, v_offset, blur, color) ->
        let percent = Color.opacity_to_percent opacity in
        let alpha = percent /. 100.0 in
        let alpha_d = alpha_decl percent in
        let base_fallback : Css.color =
          match color with
          | Arb_hex c -> Color.hex_to_oklab_alpha c alpha
          | Arb_var v -> make_color_var (Parse.extract_var_name v)
          | Arb_none -> Css.Current
        in
        let base_color_ref =
          Var.reference_with_fallback text_shadow_color_var base_fallback
        in
        let base_shadow =
          Css.text_shadow
            (Css.Text_shadow
               { h_offset; v_offset; blur; color = Some (Var base_color_ref) })
        in
        let rules =
          match color with
          | Arb_hex _ -> Stdlib.Option.None
          | Arb_var v ->
              let vn = Parse.extract_var_name v in
              let raw_fb =
                Printf.sprintf "oklab(from var(--%s) l a b / %s%%)" vn
                  (pp_float percent)
              in
              let enhanced_ref =
                Css.var_ref ~fallback:(Raw_fallback raw_fb)
                  "tw-text-shadow-color"
              in
              let enhanced_shadow =
                Css.text_shadow
                  (Css.Text_shadow
                     {
                       h_offset;
                       v_offset;
                       blur;
                       color = Some (Var enhanced_ref);
                     })
              in
              let supports_block =
                Css.supports ~condition:relative_color_supports
                  [
                    Css.rule ~selector:(Css.Selector.class_ "_")
                      [ enhanced_shadow ];
                  ]
              in
              Some [ supports_block ]
          | Arb_none ->
              let color_mix_fallback =
                Css.color_mix ~in_space:Oklab Css.Current Css.Transparent
                  ~percent1:percent
              in
              let enhanced_ref =
                Var.reference_with_fallback text_shadow_color_var
                  color_mix_fallback
              in
              let enhanced_shadow =
                Css.text_shadow
                  (Css.Text_shadow
                     {
                       h_offset;
                       v_offset;
                       blur;
                       color = Some (Var enhanced_ref);
                     })
              in
              let supports_block = color_mix_supports [ enhanced_shadow ] in
              Some [ supports_block ]
        in
        style ~rules ~property_rules:text_shadow_property_rules
          [ alpha_d; base_shadow ]
    | Stdlib.Option.None -> style [ Css.text_shadow Css.None ]

  (* ============ Style dispatch ============ *)

  let to_style = function
    | Text_shadow_none ->
        style ~property_rules:text_shadow_property_rules
          [ Css.text_shadow Css.None ]
    | Text_shadow_shape shape ->
        style ~property_rules:text_shadow_property_rules
          [ shape_text_shadow shape ]
    | Text_shadow_shape_opacity (shape, opacity) ->
        let percent = Color.opacity_to_percent opacity in
        style ~property_rules:text_shadow_property_rules
          [ alpha_decl percent; shape_text_shadow_opacity shape opacity ]
    | Text_shadow_color (c, shade) -> set_color c shade
    | Text_shadow_color_opacity (c, shade, opacity) ->
        set_color_opacity c shade opacity
    | Text_shadow_current -> set_current ()
    | Text_shadow_current_opacity opacity -> set_current_opacity opacity
    | Text_shadow_inherit -> set_inherit ()
    | Text_shadow_transparent -> set_transparent ()
    | Text_shadow_bracket_hex hex -> set_bracket_hex hex
    | Text_shadow_bracket_hex_opacity (hex, opacity) ->
        set_bracket_hex_opacity hex opacity
    | Text_shadow_bracket_color_var var_expr -> set_bracket_color_var var_expr
    | Text_shadow_bracket_cvar_opacity (var_expr, opacity) ->
        set_bracket_color_var_opacity var_expr opacity
    | Text_shadow_bracket_shadow var_expr ->
        style ~property_rules:text_shadow_property_rules
          [ Css.text_shadow (make_text_shadow_var var_expr) ]
    | Text_shadow_bracket_var var_expr ->
        style ~property_rules:text_shadow_property_rules
          [ Css.text_shadow (make_text_shadow_var var_expr) ]
    | Text_shadow_arbitrary arb -> arbitrary_shadow_style arb
    | Text_shadow_arbitrary_opacity (arb, opacity) ->
        arbitrary_shadow_opacity_style arb opacity

  (* ============ Parsing ============ *)

  let err_not_utility = Error (`Msg "Not a text shadow utility")
  let has_opacity s = String.contains s '/'

  let starts_with prefix s =
    String.length s >= String.length prefix
    && String.sub s 0 (String.length prefix) = prefix

  let is_shadow_value inner =
    (* A shadow value has explicit length dimensions like "10px_10px" *)
    let has_length_unit s =
      let len = String.length s in
      (* Scan past digits (and optional decimal part), return position after *)
      let scan_number i =
        let j = ref i in
        while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
          incr j
        done;
        if !j < len && s.[!j] = '.' then (
          incr j;
          while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
            incr j
          done);
        !j
      in
      let has_unit_at j =
        let rest = if j < len then String.sub s j (min 3 (len - j)) else "" in
        starts_with "px" rest || starts_with "rem" rest || starts_with "em" rest
      in
      let rec check i =
        if i >= len - 1 then false
        else if s.[i] >= '0' && s.[i] <= '9' then
          let j = scan_number (i + 1) in
          has_unit_at j || check j
        else check (i + 1)
      in
      check 0
    in
    let has_typed_prefix =
      starts_with "color:" inner || starts_with "shadow:" inner
    in
    has_length_unit inner && not has_typed_prefix

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "shadow"; "none" ] -> Ok Text_shadow_none
    | [ "text"; "shadow" ] -> Ok (Text_shadow_shape S_md)
    | [ "text"; "shadow"; size_str ] -> (
        let base, opacity = Color.parse_opacity_modifier size_str in
        let shape_opt =
          match base with
          | "2xs" -> Some S_2xs
          | "xs" -> Some S_xs
          | "sm" -> Some S_sm
          | "md" -> Some S_md
          | "lg" -> Some S_lg
          | _ -> Stdlib.Option.None
        in
        match (shape_opt, opacity) with
        | Some shape, Color.No_opacity -> Ok (Text_shadow_shape shape)
        | Some shape, op -> Ok (Text_shadow_shape_opacity (shape, op))
        | Stdlib.Option.None, _ when base = "inherit" -> Ok Text_shadow_inherit
        | Stdlib.Option.None, _ when base = "transparent" ->
            Ok Text_shadow_transparent
        | Stdlib.Option.None, _ when starts_with "current" base -> (
            match opacity with
            | Color.No_opacity when base = "current" -> Ok Text_shadow_current
            | Color.No_opacity -> err_not_utility
            | op -> Ok (Text_shadow_current_opacity op))
        | Stdlib.Option.None, _ when Parse.is_bracket_value base -> (
            let inner = Parse.bracket_inner base in
            if starts_with "color:" inner then
              let var_part = String.sub inner 6 (String.length inner - 6) in
              match opacity with
              | Color.No_opacity -> Ok (Text_shadow_bracket_color_var var_part)
              | op -> Ok (Text_shadow_bracket_cvar_opacity (var_part, op))
            else if starts_with "shadow:" inner then
              let var_part = String.sub inner 7 (String.length inner - 7) in
              Ok (Text_shadow_bracket_shadow var_part)
            else if Parse.is_var inner && not (is_shadow_value inner) then
              Ok (Text_shadow_bracket_var inner)
            else if String.length inner > 0 && inner.[0] = '#' then
              let hex = String.sub inner 1 (String.length inner - 1) in
              match opacity with
              | Color.No_opacity -> Ok (Text_shadow_bracket_hex hex)
              | op -> Ok (Text_shadow_bracket_hex_opacity (hex, op))
            else
              match opacity with
              | Color.No_opacity -> Ok (Text_shadow_arbitrary inner)
              | op -> Ok (Text_shadow_arbitrary_opacity (inner, op)))
        | _ -> err_not_utility)
    | "text" :: "shadow" :: color_parts when List.exists has_opacity color_parts
      -> (
        match Color.shade_and_opacity_of_strings color_parts with
        | Ok (color, shade, opacity) ->
            Ok (Text_shadow_color_opacity (color, shade, opacity))
        | Error e -> Error e)
    | "text" :: "shadow" :: color_parts -> (
        match Color.shade_of_strings color_parts with
        | Ok (color, shade) -> Ok (Text_shadow_color (color, shade))
        | Error e -> Error e)
    | _ -> err_not_utility

  (* ============ Class name generation ============ *)

  let shape_to_string = function
    | S_2xs -> "2xs"
    | S_xs -> "xs"
    | S_sm -> "sm"
    | S_md -> ""
    | S_lg -> "lg"

  let to_class = function
    | Text_shadow_none -> "text-shadow-none"
    | Text_shadow_shape S_md -> "text-shadow"
    | Text_shadow_shape shape -> "text-shadow-" ^ shape_to_string shape
    | Text_shadow_shape_opacity (shape, opacity) ->
        let base =
          match shape with
          | S_md -> "text-shadow"
          | _ -> "text-shadow-" ^ shape_to_string shape
        in
        base ^ "/" ^ Color.pp_opacity opacity
    | Text_shadow_color (c, shade) ->
        "text-shadow-" ^ Color.color_to_string c
        ^ if Color.is_shadeless c then "" else "-" ^ string_of_int shade
    | Text_shadow_color_opacity (c, shade, opacity) ->
        "text-shadow-" ^ Color.color_to_string c
        ^ (if Color.is_shadeless c then "" else "-" ^ string_of_int shade)
        ^ "/" ^ Color.pp_opacity opacity
    | Text_shadow_current -> "text-shadow-current"
    | Text_shadow_current_opacity opacity ->
        "text-shadow-current/" ^ Color.pp_opacity opacity
    | Text_shadow_inherit -> "text-shadow-inherit"
    | Text_shadow_transparent -> "text-shadow-transparent"
    | Text_shadow_bracket_hex hex -> "text-shadow-[#" ^ hex ^ "]"
    | Text_shadow_bracket_hex_opacity (hex, opacity) ->
        "text-shadow-[#" ^ hex ^ "]/" ^ Color.pp_opacity opacity
    | Text_shadow_bracket_color_var var_expr ->
        "text-shadow-[color:" ^ var_expr ^ "]"
    | Text_shadow_bracket_cvar_opacity (var_expr, opacity) ->
        "text-shadow-[color:" ^ var_expr ^ "]/" ^ Color.pp_opacity opacity
    | Text_shadow_bracket_shadow var_expr ->
        "text-shadow-[shadow:" ^ var_expr ^ "]"
    | Text_shadow_bracket_var var_expr -> "text-shadow-[" ^ var_expr ^ "]"
    | Text_shadow_arbitrary arb -> "text-shadow-[" ^ arb ^ "]"
    | Text_shadow_arbitrary_opacity (arb, opacity) ->
        "text-shadow-[" ^ arb ^ "]/" ^ Color.pp_opacity opacity

  (* ============ Suborder ============ *)

  (* Utilities that set --tw-text-shadow-alpha AND text-shadow come before all
     other utilities. Within that group, relative color @supports (lab) come
     first, then color-mix @supports, then no-@supports. *)
  let suborder = function
    | Text_shadow_arbitrary_opacity (arb, _) -> (
        match parse_arbitrary_shadow arb with
        | Some (_, _, _, Arb_var _) -> -3 (* @supports lab *)
        | Some (_, _, _, Arb_none) -> -2 (* @supports color-mix *)
        | Some (_, _, _, Arb_hex _) -> -1 (* no @supports *)
        | Stdlib.Option.None -> 0)
    | Text_shadow_shape_opacity _ -> -1 (* no @supports *)
    | _ -> 0
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let text_shadow_none = utility Text_shadow_none
let text_shadow_2xs = utility (Text_shadow_shape S_2xs)
let text_shadow_xs = utility (Text_shadow_shape S_xs)
let text_shadow_sm = utility (Text_shadow_shape S_sm)
let text_shadow = utility (Text_shadow_shape S_md)
let text_shadow_md = utility (Text_shadow_shape S_md)
let text_shadow_lg = utility (Text_shadow_shape S_lg)
let text_shadow_arbitrary arb = utility (Text_shadow_arbitrary arb)
