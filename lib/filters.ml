(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css
  module Option = Stdlib.Option

  (* [filter] and [backdrop-filter] take the same functions, so a utility names
     its target and the function. What differs between the two targets is stated
     once each: the class prefix ([prefix]), the functions it chains
     ([functions]), the properties it writes ([declarations]), its variable
     family ([make_vars]) and its suborders ([slot]). *)
  type target = Filter | Backdrop

  (* The functions both targets scale by a <number-percentage>. *)
  type amount =
    [ `Brightness | `Contrast | `Grayscale | `Invert | `Saturate | `Sepia ]

  (* The amounts and [backdrop-filter]'s opacity, which scales the same way. *)
  type scaled = [ amount | `Opacity ]

  (* Every function a [--tw-*] variable carries. *)
  type func = [ scaled | `Blur | `Hue_rotate ]
  type blur_size = Xs | Sm | Md | Lg | Xl | Two_xl | Three_xl

  type t =
    | Chain of target
    | Chain_none of target
    | Chain_arbitrary of target * string * Css.filter
    | Chain_raw of target * string * string
      (* filter-[foo]: the family writes one longhand, so a bracket no reader
         took still names it and the value is forwarded verbatim. *)
    | Blur of target
    | Blur_none of target
    | Blur_size of target * blur_size
    (* The author's bracket text travels with the length it denotes, so the
       class name is spelled exactly as it was written. *)
    | Blur_arbitrary of target * string * Css.length
    | Blur_raw of target * string * string
    | Blur_theme of string (* filter only: radius from a --blur-* token *)
    | Amount of target * amount * int
    | Amount_arbitrary of target * amount * string
    | Backdrop_opacity of float
    | Backdrop_opacity_arbitrary of string
    | Hue_rotate of target * int
    (* The author's bracket text travels with the angle it denotes, so the class
       name is spelled exactly as it was written. *)
    | Hue_rotate_arbitrary of target * string * Css.angle
    | Hue_rotate_raw of target * string * string
    | Neg_hue_rotate_arbitrary of target * string * Css.angle
    | Drop_shadow
    | Drop_shadow_xs
    | Drop_shadow_sm
    | Drop_shadow_md
    | Drop_shadow_lg
    | Drop_shadow_xl
    | Drop_shadow_2xl
    | Drop_shadow_multi
    | Drop_shadow_none
    | Drop_shadow_inherit
    | Drop_shadow_named of string
    | Drop_shadow_named_opacity of string * Color.opacity_modifier
    | Drop_shadow_arbitrary of string * Css.filter
    | Drop_shadow_var_opacity of string * Css.filter * Color.opacity_modifier
    | Drop_shadow_arbitrary_opacity of
        string * Css.shadow_body list * Color.opacity_modifier
    | Drop_shadow_raw of string * string
    | Drop_shadow_color of Color.color * int
    | Drop_shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Drop_shadow_opacity of Color.opacity_modifier
    | Drop_shadow_keyword_color of Css.color * string
      (* drop-shadow-current / -transparent: a colour with no theme token *)
    | Drop_shadow_keyword_color_opacity of
        Css.color * string * Color.opacity_modifier
    | Drop_shadow_size_opacity of string * Color.opacity_modifier
    (* A bracket read as a colour, with the text the class wrote *)
    | Drop_shadow_bracket_color of string * Css.color
    | Drop_shadow_bracket_color_opacity of
        string * Css.color * Color.opacity_modifier
    | Drop_shadow_bracket_cvar of string (* [color:var(--c)], the var() text *)
    | Drop_shadow_bracket_cvar_opacity of string * Color.opacity_modifier

  let name = "filters"
  let priority _ = 30
  let prefix = function Filter -> "" | Backdrop -> "backdrop-"

  let func_name : func -> string = function
    | `Blur -> "blur"
    | `Brightness -> "brightness"
    | `Contrast -> "contrast"
    | `Grayscale -> "grayscale"
    | `Hue_rotate -> "hue-rotate"
    | `Invert -> "invert"
    | `Opacity -> "opacity"
    | `Saturate -> "saturate"
    | `Sepia -> "sepia"

  let amounts : amount list =
    [ `Brightness; `Contrast; `Grayscale; `Invert; `Saturate; `Sepia ]

  (* Every function in chain order, which is also the [@property] order. *)
  let all_functions : func list =
    [
      `Blur;
      `Brightness;
      `Contrast;
      `Grayscale;
      `Hue_rotate;
      `Invert;
      `Opacity;
      `Saturate;
      `Sepia;
    ]

  (* The functions a target chains: Tailwind's [filter] chain has no opacity,
     its [backdrop-filter] chain has. *)
  let functions target =
    List.filter
      (fun f -> match (target, f) with Filter, `Opacity -> false | _ -> true)
      all_functions

  (* Register the [--tw-<fn>] and [--tw-backdrop-<fn>] variables in the Var
     system for @property generation. Both targets register every function:
     Tailwind's filter properties include [--tw-opacity], which its chain does
     not read. *)
  let make_vars target =
    let base, family =
      match target with
      | Filter -> (60, `Filter)
      | Backdrop -> (80, `Backdrop_filter)
    in
    List.mapi
      (fun i f ->
        ( f,
          Var.channel ~needs_property:true ~property_order:(base + i) ~family
            Css.Filter
            ("tw-" ^ prefix target ^ func_name f) ))
      all_functions

  let vars =
    let filter_vars = make_vars Filter in
    let backdrop_vars = make_vars Backdrop in
    function Filter -> filter_vars | Backdrop -> backdrop_vars

  let var target (f : func) = List.assoc f (vars target)

  let drop_shadow_var =
    Var.channel ~needs_property:true ~property_order:69 ~family:`Drop_shadow
      Css.Filter "tw-drop-shadow"

  let drop_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:70 ~family:`Drop_shadow
      Css.Color "tw-drop-shadow-color"

  let drop_shadow_alpha_var =
    Var.property_default Css.Percentage ~initial:(Pct 100.) ~property_order:71
      ~family:`Drop_shadow "tw-drop-shadow-alpha"

  let drop_shadow_alpha_decl opacity =
    Var.set drop_shadow_alpha_var (Color.opacity_alpha_value opacity)

  let drop_shadow_size_var =
    Var.channel ~needs_property:true ~property_order:72 ~family:`Drop_shadow
      Css.Filter "tw-drop-shadow-size"

  let property_rule_or_empty var =
    match Var.property_rule var with None -> Css.empty | Some r -> r

  (* The drop-shadow variables ride on the filter chain. *)
  let filter_property_rules =
    Css.concat
      (List.map (fun (_, v) -> property_rule_or_empty v) (vars Filter)
      @ [
          property_rule_or_empty drop_shadow_var;
          property_rule_or_empty drop_shadow_color_var;
          Var.property_rules drop_shadow_alpha_var;
          property_rule_or_empty drop_shadow_size_var;
        ])

  let filter_property_metadata =
    List.map (fun (_, v) -> Var.metadata v) (vars Filter)
    @ [
        Var.metadata drop_shadow_var;
        Var.metadata drop_shadow_color_var;
        Var.metadata drop_shadow_alpha_var;
        Var.metadata drop_shadow_size_var;
      ]

  let backdrop_property_rules =
    Css.concat
      (List.map (fun (_, v) -> property_rule_or_empty v) (vars Backdrop))

  let backdrop_property_metadata =
    List.map (fun (_, v) -> Var.metadata v) (vars Backdrop)

  let property_rules = function
    | Filter -> filter_property_rules
    | Backdrop -> backdrop_property_rules

  let metadata = function
    | Filter -> filter_property_metadata
    | Backdrop -> backdrop_property_metadata

  (* The composable chain, [var(--tw-blur,) var(--tw-brightness,) ...]: every
     function the target has, then the drop shadow on [filter]. *)
  let chain =
    let make target : Css.filter =
      let refs =
        List.map
          (fun f -> Css.filter_var_empty (Var.name (var target f)))
          (functions target)
      in
      match target with
      | Filter ->
          Css.List (refs @ [ Css.filter_var_empty (Var.name drop_shadow_var) ])
      | Backdrop -> Css.List refs
    in
    let filter_chain = make Filter in
    let backdrop_chain = make Backdrop in
    function Filter -> filter_chain | Backdrop -> backdrop_chain

  let filter_chain = chain Filter

  (* The properties a target writes; the backdrop family aliases its property
     for WebKit. *)
  let declarations target (value : Css.filter) =
    match target with
    | Filter -> [ filter value ]
    | Backdrop -> [ Css.webkit_backdrop_filter value; backdrop_filter value ]

  let property_names = function
    | Filter -> [ "filter" ]
    | Backdrop -> [ "-webkit-backdrop-filter"; "backdrop-filter" ]

  (* [decls], then the target's property reading the chain. *)
  let with_chain target decls =
    style ~metadata:(metadata target) ~property_rules:(property_rules target)
      (decls @ declarations target (chain target))

  let var_decl var value =
    fst (Css.var ~layer:"utilities" (Var.name var) Css.Filter value)

  (* Set a function's variable and output the composable chain. *)
  let set target f (value : Css.filter) =
    with_chain target [ var_decl (var target f) value ]

  let set_raw target f value =
    match
      Parse.wrap_declaration_value ~before:(func_name f ^ "(") ~after:")" value
    with
    | None -> style []
    | Some wrapped ->
        with_chain target
          [
            Css.custom_property ~layer:"utilities"
              (Var.css_name (var target f))
              wrapped;
          ]

  (* Generate a theme-layer declaration for a theme variable if its value is
     set. This produces the --name: value entry for the :root, :host block. *)
  let theme_decl_if_set ?theme name =
    match Scheme.theme_value theme name with
    | Some value -> [ Css.custom_property ~layer:"theme" ("--" ^ name) value ]
    | None -> []

  (* Tailwind reads [backdrop-blur-none] from [--backdrop-blur-none], then
     [--blur-none] (themeKeys [--backdrop-blur], [--blur]). Without a token the
     radius is emptied; [blur-none] then writes no [@property] for the chain,
     unlike [backdrop-blur-none]. *)
  let blur_none theme target =
    let tokens =
      match target with
      | Filter -> [ "blur-none" ]
      | Backdrop -> [ "backdrop-blur-none"; "blur-none" ]
    in
    let blur = var target `Blur in
    match
      List.find_opt
        (fun token -> Scheme.theme_value (Some theme) token <> None)
        tokens
    with
    | Some token ->
        let ref_ : Css.length Css.var = Var.theme_ref token in
        with_chain target
          (theme_decl_if_set ~theme token
          @ [ var_decl blur (Css.Blur (Var ref_)) ])
    | None -> (
        let decls =
          Css.custom_property ~layer:"utilities" (Var.css_name blur) " "
          :: declarations target (chain target)
        in
        match target with
        | Filter -> style decls
        | Backdrop ->
            style ~metadata:(metadata target)
              ~property_rules:(property_rules target) decls)

  (* Tailwind v4 default blur theme tokens (in px). Each [.blur-<size>] utility
     binds its theme token and references it in [--tw-blur]; the [.blur] keyword
     utility (no suffix) has no theme token in v4 and emits the literal
     [blur(8px)] inline. *)
  let blur_xs_var = Var.theme Css.Length "blur-xs" ~order:(8, 0)
  let blur_sm_var = Var.theme Css.Length "blur-sm" ~order:(8, 1)

  (* A [--blur-*] token the project declared names a radius the built-in scale
     has no slot for; they share the slot after the scale. *)
  let blur_named_cache = Domain_cache.v 8

  let blur_named_var name =
    Domain_cache.or_add blur_named_cache name (fun () ->
        Var.theme Css.Length ("blur-" ^ name) ~order:(8, 8))

  let blur_md_var = Var.theme Css.Length "blur-md" ~order:(8, 2)
  let blur_lg_var = Var.theme Css.Length "blur-lg" ~order:(8, 3)
  let blur_xl_var = Var.theme Css.Length "blur-xl" ~order:(8, 4)
  let blur_2xl_var = Var.theme Css.Length "blur-2xl" ~order:(8, 5)
  let blur_3xl_var = Var.theme Css.Length "blur-3xl" ~order:(8, 6)
  let blur_sizes = [ Xs; Sm; Md; Lg; Xl; Two_xl; Three_xl ]

  (* A size's class suffix, theme token and default radius in px. *)
  let blur_size = function
    | Xs -> ("xs", blur_xs_var, 4.)
    | Sm -> ("sm", blur_sm_var, 8.)
    | Md -> ("md", blur_md_var, 12.)
    | Lg -> ("lg", blur_lg_var, 16.)
    | Xl -> ("xl", blur_xl_var, 24.)
    | Two_xl -> ("2xl", blur_2xl_var, 40.)
    | Three_xl -> ("3xl", blur_3xl_var, 64.)

  let blur_size_name size =
    let name, _, _ = blur_size size in
    name

  let blur_size_of_name n =
    List.find_opt (fun size -> String.equal (blur_size_name size) n) blur_sizes

  (* Publish the scale through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [blur-[theme(--blur-sm)]] resolves and
     [theme(static)] emits it. *)
  let () =
    List.iter
      (fun size ->
        let _, var, px = blur_size size in
        Theme.register_default var (Css.Px px : Css.length))
      blur_sizes

  (* Both targets bind the unified v4 [--blur-<size>] token and reference it;
     Tailwind dropped the separate [--backdrop-blur-*] tokens, so the default
     theme ships only [--blur-*]. *)
  let blur_sized target size =
    let _, theme_var, default_px = blur_size size in
    let decl, ref_ = Var.binding theme_var (Css.Px default_px) in
    with_chain target
      [ decl; var_decl (var target `Blur) (Css.Blur (Var ref_)) ]

  let is_theme_blur theme n =
    Scheme.theme_value (Some theme) ("blur-" ^ n) <> None

  let blur_theme theme name =
    match Scheme.theme_value (Some theme) ("blur-" ^ name) with
    | None -> style []
    | Some raw -> (
        match Css.parse_length raw with
        | None -> style []
        | Some len ->
            let decl, ref_ = Var.binding (blur_named_var name) len in
            with_chain Filter
              [ decl; var_decl (var Filter `Blur) (Css.Blur (Var ref_)) ])

  (* The length a [blur-[...]] bracket denotes, read with cascade's own grammar
     so calc() and every unit come for free. A var bracket keeps the tw-side
     reference the variable system tracks. [None] is a bracket no length reads
     from, and [of_class] refuses the utility rather than leaving [to_style] to
     raise. *)
  let arbitrary_blur s : Css.length option =
    let inner = Parse.bracket_inner s in
    if Parse.is_var inner then
      let ref_ : Css.length Css.var =
        Var.bracket (Parse.extract_var_name inner)
      in
      Option.Some (Css.Var ref_ : Css.length)
    else Css.parse_length (Parse.decode_underscores inner)

  (* Parse bracket content for angle values (e.g., "45deg") *)
  let parse_bracket_angle s =
    let inner = Parse.bracket_inner s in
    let slen = String.length inner in
    let i = ref 0 in
    while
      !i < slen
      && ((inner.[!i] >= '0' && inner.[!i] <= '9')
         || inner.[!i] = '.'
         || inner.[!i] = '-')
    do
      incr i
    done;
    if !i = 0 || !i = slen then Option.None
    else
      let num_s = String.sub inner 0 !i in
      let unit_s = String.sub inner !i (slen - !i) in
      match Float.of_string_opt num_s with
      | Option.None -> Option.None
      | Option.Some n -> (
          match unit_s with
          | "deg" -> Option.Some (Css.Deg n : Css.angle)
          | "rad" -> Option.Some (Rad n)
          | "turn" -> Option.Some (Turn n)
          | "grad" -> Option.Some (Grad n)
          | _ -> Option.None)

  (* Parse bracket content for number_percentage values *)
  let parse_bracket_numpct_opt inner : Css.number_percentage option =
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      Option.Some (Css.Var (Var.bracket bare) : Css.number_percentage)
    else
      let len = String.length inner in
      if len > 1 && inner.[len - 1] = '%' then
        Option.map
          (fun f : Css.number_percentage -> Pct f)
          (Float.of_string_opt (String.sub inner 0 (len - 1)))
      else
        Option.map
          (fun f : Css.number_percentage -> Num f)
          (Float.of_string_opt inner)

  let amount_filter (f : scaled) np : Css.filter =
    match f with
    | `Brightness -> Css.Brightness np
    | `Contrast -> Css.Contrast np
    | `Grayscale -> Css.Grayscale np
    | `Invert -> Css.Invert np
    | `Opacity -> Css.Opacity np
    | `Saturate -> Css.Saturate np
    | `Sepia -> Css.Sepia np

  (* A bracket amount: a number or percentage, else forwarded verbatim. *)
  let amount_arbitrary target (f : scaled) s =
    let inner = Parse.bracket_inner s in
    match parse_bracket_numpct_opt inner with
    | Some np -> set target (f :> func) (amount_filter f np)
    | None -> set_raw target (f :> func) (Parse.decode_arbitrary_value inner)

  let amount target (f : scaled) pct =
    set target (f :> func) (amount_filter f (Pct pct))

  (* Helper to create an angle with calc for negative values *)
  let make_angle n =
    if n >= 0 then Deg (float_of_int n)
    else Calc (Expr (Val (Deg (float_of_int (abs n))), Mul, Num (-1.)))

  let neg_angle angle : Css.angle = Calc (Expr (Val angle, Mul, Num (-1.)))

  (* Drop-shadow utilities *)
  let bind_drop_shadow value = Var.set drop_shadow_var value
  let bind_drop_shadow_size value = Var.set drop_shadow_size_var value
  let bind_drop_shadow_color value = Var.set drop_shadow_color_var value

  let drop_shadow_size_ref : Css.filter =
    Css.Var (Var.bracket (Var.name drop_shadow_size_var))

  let drop_shadow_theme_ref name : Css.filter =
    Css.Drop_shadow (Css.Var (Var.bracket name) : Css.shadow)

  (* Theme tokens for the sized drop-shadow utilities. Tailwind v4 defines a
     [--drop-shadow-<size>] design token in the theme layer and references it
     from [--tw-drop-shadow]. The family owns slots (7, 20-25), after radius (7,
     0-11) and before ease (7, 30-34). Bound via [Var.binding] so the theme
     declaration is always emitted with its default value and stays overridable
     through an [@theme] token override threaded via [Scheme.t]. *)
  let drop_shadow_xs_var = Var.theme Css.Shadow "drop-shadow-xs" ~order:(7, 20)
  let drop_shadow_sm_var = Var.theme Css.Shadow "drop-shadow-sm" ~order:(7, 21)
  let drop_shadow_md_var = Var.theme Css.Shadow "drop-shadow-md" ~order:(7, 22)
  let drop_shadow_lg_var = Var.theme Css.Shadow "drop-shadow-lg" ~order:(7, 23)
  let drop_shadow_xl_var = Var.theme Css.Shadow "drop-shadow-xl" ~order:(7, 24)
  let drop_shadow_2xl_var = Var.theme Css.Shadow "drop-shadow-2xl" ~order:(7, 25)

  (* Published the same way, so [drop-shadow-[theme(--drop-shadow-md)]]
     resolves. The colours keep the hex spelling the sized utilities bind, which
     is the alpha Tailwind writes as an [rgb()]. *)
  let drop_shadow_scale =
    [
      ("xs", drop_shadow_xs_var, 1., 1., "#0000000d");
      ("sm", drop_shadow_sm_var, 1., 2., "#00000026");
      ("md", drop_shadow_md_var, 3., 3., "#0000001f");
      ("lg", drop_shadow_lg_var, 4., 4., "#00000026");
      ("xl", drop_shadow_xl_var, 9., 7., "#0000001a");
      ("2xl", drop_shadow_2xl_var, 25., 25., "#00000026");
    ]

  let () =
    List.iter
      (fun (_, var, v, blur, hex) ->
        Theme.register_default var
          (Css.shadow ~h_offset:Zero ~v_offset:(Px v) ~blur:(Px blur)
             ~color:(Css.hex hex) ()))
      drop_shadow_scale

  (* The scale's shadow for a size, as the modifier path reads it: the body with
     its own colour, which the alpha then replaces. *)
  let drop_shadow_scale_body size : Css.shadow_body option =
    List.find_map
      (fun (name, _, v, blur, hex) ->
        if String.equal name size then
          match
            Css.shadow ~h_offset:Zero ~v_offset:(Px v) ~blur:(Px blur)
              ~color:(Css.hex hex) ()
          with
          | Css.Shadow body -> Some body
          | _ -> None
        else None)
      drop_shadow_scale

  let drop_shadow_color_ref fallback =
    Var.reference_with_fallback drop_shadow_color_var fallback

  let drop_shadow_filter h v blur fallback =
    Css.Drop_shadow
      (Css.shadow ~h_offset:h ~v_offset:v ?blur
         ~color:(Css.Var (drop_shadow_color_ref fallback))
         ())

  let drop_shadow_none =
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      [
        Css.custom_property ~layer:"utilities" "--tw-drop-shadow" " ";
        filter filter_chain;
      ]

  (* Build the [--tw-drop-shadow-size] filter from a parsed theme shadow body,
     hoisting the body's colour into the [--tw-drop-shadow-color] fallback. A
     body without an explicit colour falls back to [currentcolor]. *)
  let drop_shadow_size_of_body (body : Css.shadow_body) : Css.filter =
    let fallback : Css.color =
      match body.color with Some c -> c | None -> Css.Current
    in
    Css.Drop_shadow
      (Css.shadow ~h_offset:body.h_offset ~v_offset:body.v_offset
         ?blur:body.blur ?spread:body.spread
         ~color:(Css.Var (drop_shadow_color_ref fallback))
         ())

  (* The [--drop-shadow-<name>] theme value behind [drop-shadow-<name>], split
     into shadow bodies. [None] means the theme has no such token, or its value
     is not a shadow: either way the class is not a utility, so [of_class] uses
     this to gate before [to_style] ever sees the name. *)
  let shadow_token_bodies ?theme theme_name :
      (string * Css.shadow_body list) option =
    match Scheme.theme_value theme theme_name with
    | None -> None
    | Some value -> (
        let body_of (sh : Css.shadow) : Css.shadow_body option =
          match sh with Css.Shadow body -> Some body | _ -> None
        in
        let bodies =
          match Parse.shadow value with
          | Some (Css.List shadows) -> List.filter_map body_of shadows
          | Some sh -> ( match body_of sh with Some b -> [ b ] | None -> [])
          | None -> []
        in
        match bodies with [] -> None | _ -> Some (value, bodies))

  let drop_shadow_theme_bodies ?theme name =
    shadow_token_bodies ?theme ("drop-shadow-" ^ name)

  let drop_shadow_size_of_bodies bodies : Css.filter =
    match List.map drop_shadow_size_of_body bodies with
    | [ one ] -> one
    | many -> Css.List many

  (* Bare [.drop-shadow]. In the default Tailwind v4 theme this token is a
     two-shadow stack inlined as a literal (no [--drop-shadow] theme var). When
     a custom theme overrides [--drop-shadow] with a single shadow value, it is
     referenced via [drop-shadow(var(--drop-shadow))] instead. *)
  let drop_shadow_override ?theme bodies =
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      (theme_decl_if_set ?theme "drop-shadow"
      @ [
          bind_drop_shadow_size (drop_shadow_size_of_bodies bodies);
          bind_drop_shadow (drop_shadow_theme_ref "drop-shadow");
          filter filter_chain;
        ])

  (* The default [--drop-shadow] is a two-shadow stack. [size_fallback] is what
     each layer falls back to when no [--tw-drop-shadow-color] is set: the
     token's own alpha for the bare utility, and black at the modifier's alpha
     for [drop-shadow/<n>]. *)
  let drop_shadow_default_size size_fallback : Css.filter =
    let fallback_a, fallback_b =
      match size_fallback with
      | Some c -> (c, c)
      | None -> (Css.hex "#0000001a", Css.hex "#0000000f")
    in
    Css.List
      [
        drop_shadow_filter Zero (Px 1.) (Some (Px 2.)) fallback_a;
        drop_shadow_filter Zero (Px 1.) (Some (Px 1.)) fallback_b;
      ]

  let drop_shadow_default_literal : Css.filter =
    let fallback_a = Css.hex "#0000001a" in
    let fallback_b = Css.hex "#0000000f" in
    Css.List
      [
        Css.Drop_shadow
          (Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 2.)
             ~color:fallback_a ());
        Css.Drop_shadow
          (Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 1.)
             ~color:fallback_b ());
      ]

  let drop_shadow_default () =
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      [
        bind_drop_shadow_size (drop_shadow_default_size None);
        bind_drop_shadow drop_shadow_default_literal;
        filter filter_chain;
      ]

  let drop_shadow_ ?theme () =
    match shadow_token_bodies ?theme "drop-shadow" with
    | Some (_, bodies) -> drop_shadow_override ?theme bodies
    | None -> drop_shadow_default ()

  (* A sized drop-shadow utility: [drop-shadow-{xs,sm,md,lg,xl,2xl}].

     Emits the [--drop-shadow-<size>] theme token with its default value,
     references it from [--tw-drop-shadow], and builds [--tw-drop-shadow-size]
     from the token's shadow - a project override of it first - with the colour
     hoisted into the [--tw-drop-shadow-color] fallback. *)
  let drop_shadow_sized ?theme size =
    match
      List.find_opt
        (fun (name, _, _, _, _) -> String.equal name size)
        drop_shadow_scale
    with
    | None -> invalid_arg ("drop-shadow-" ^ size ^ ": not a size")
    | Some (_, theme_var, v, blur, hex) ->
        let theme_decl, _ref =
          Var.binding theme_var
            (Css.shadow ~h_offset:Zero ~v_offset:(Px v) ~blur:(Px blur)
               ~color:(Css.hex hex) ())
        in
        let bodies =
          match drop_shadow_theme_bodies ?theme size with
          | Some (_, bodies) -> bodies
          | None -> Option.to_list (drop_shadow_scale_body size)
        in
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [
            theme_decl;
            bind_drop_shadow_size (drop_shadow_size_of_bodies bodies);
            bind_drop_shadow (drop_shadow_theme_ref ("drop-shadow-" ^ size));
            filter filter_chain;
          ]

  (* A theme-token drop-shadow such as [drop-shadow-calc] backed by a custom
     [--drop-shadow-<name>] theme value. Parses the theme value into one or more
     shadow bodies, hoists each colour into [--tw-drop-shadow-color], and
     references the token from [--tw-drop-shadow]. *)
  let drop_shadow_named ?theme name =
    let theme_name = "drop-shadow-" ^ name in
    match drop_shadow_theme_bodies ?theme name with
    | None ->
        (* [of_class] resolves the token against the same theme, so the only way
           here is a utility built against one theme and rendered against
           another. *)
        invalid_arg (theme_name ^ ": no such theme token")
    | Some (value, bodies) ->
        let size : Css.filter =
          match List.map drop_shadow_size_of_body bodies with
          | [ single ] -> single
          | many -> Css.List many
        in
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [
            Css.custom_property ~layer:"theme" ("--" ^ theme_name) value;
            bind_drop_shadow_size size;
            bind_drop_shadow (drop_shadow_theme_ref theme_name);
            filter filter_chain;
          ]

  let drop_shadow_multi_ =
    let fallback_a = Css.hex "#0000000d" in
    let fallback_b = Css.hex "#0000001a" in
    let size : Css.filter =
      Css.List
        [
          drop_shadow_filter Zero (Px 1.) (Some (Px 1.)) fallback_a;
          drop_shadow_filter Zero (Px 9.) (Some (Px 7.)) fallback_b;
        ]
    in
    let literal : Css.filter =
      Css.List
        [
          Css.Drop_shadow
            (Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 1.)
               ~color:fallback_a ());
          Css.Drop_shadow
            (Css.shadow ~h_offset:Zero ~v_offset:(Px 9.) ~blur:(Px 7.)
               ~color:fallback_b ());
        ]
    in
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      [
        bind_drop_shadow_size size;
        bind_drop_shadow literal;
        filter filter_chain;
      ]

  (* The [--tw-drop-shadow-size] filter behind [drop-shadow-[...]]: a bracket
     that parses as a [<shadow>] hoists its colour into the
     [--tw-drop-shadow-color] fallback via [drop_shadow_size_of_body],
     defaulting to [currentcolor] when the shadow has none (matching Tailwind -
     not the last space-separated token, which need not be a colour). A bracket
     that is itself a bare [var()] passes through unchanged, since the browser
     resolves the whole shadow from it. [None] means the bracket is not a
     shadow, which [of_class] rejects. *)
  (* The shadow bodies a bracket spells, [None] when it spells no shadow. *)
  let drop_shadow_bodies s : Css.shadow_body list option =
    let inner = Parse.decode_arbitrary_value (Parse.bracket_inner s) in
    match Parse.shadow inner with
    | Some (Css.Shadow body : Css.shadow) -> Some [ body ]
    | Some (Css.List shadows : Css.shadow) -> (
        match
          List.filter_map
            (function (Css.Shadow body : Css.shadow) -> Some body | _ -> None)
            shadows
        with
        | [] -> None
        | bodies -> Some bodies)
    | _ -> None

  let drop_shadow_arbitrary_value s : Css.filter option =
    let inner = Parse.decode_arbitrary_value (Parse.bracket_inner s) in
    match Parse.shadow inner with
    | Some (Css.Var _ as v) -> Some (Css.Drop_shadow v)
    | Some (Css.Shadow body) -> Some (drop_shadow_size_of_body body)
    | Some (Css.List shadows) -> (
        match
          List.filter_map
            (fun (sh : Css.shadow) ->
              match sh with
              | Css.Shadow body -> Some (drop_shadow_size_of_body body)
              | _ -> None)
            shadows
        with
        | [] -> None
        | [ single ] -> Some single
        | many -> Some (Css.List many))
    | Some
        ( Css.Inset _ | Css.None | Css.Inherit | Css.Initial | Css.Unset
        | Css.Revert | Css.Revert_layer )
    | None ->
        None

  let drop_shadow_arbitrary_impl ?(opacity = Color.No_opacity) size =
    let alpha =
      match opacity with
      | Color.No_opacity -> []
      | opacity -> [ drop_shadow_alpha_decl opacity ]
    in
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      (alpha
      @ [
          bind_drop_shadow_size size;
          bind_drop_shadow drop_shadow_size_ref;
          filter filter_chain;
        ])

  let drop_shadow_raw value =
    match
      Parse.wrap_declaration_value ~before:"drop-shadow(" ~after:")" value
    with
    | None -> style []
    | Some wrapped ->
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [
            Css.custom_property ~layer:"utilities"
              (Var.css_name drop_shadow_size_var)
              wrapped;
            bind_drop_shadow drop_shadow_size_ref;
            filter filter_chain;
          ]

  let drop_shadow_inherit_ =
    style ~metadata:filter_property_metadata
      ~property_rules:filter_property_rules
      [
        bind_drop_shadow_color Css.Inherit;
        bind_drop_shadow drop_shadow_size_ref;
      ]

  let drop_shadow_color ?theme c shade =
    let color_name = Color.scheme_color_name c shade in
    let scheme = Scheme.or_default theme in
    (* srgb fallback: the scheme hex when defined, else the colour resolved
       directly (oklch), matching Tailwind. The default scheme has no hex
       colours, so the old [Scheme.hex_color]-only path raised on every
       drop-shadow-<color>. *)
    let fallback_color =
      match Scheme.hex_color scheme color_name with
      | Option.Some hex -> Css.hex hex
      | Option.None -> Color.to_css ?theme c shade
    in
    let decls, color =
      Color.bound ?theme (Color.color_var c shade) fallback_color
    in
    let supports_decl =
      bind_drop_shadow_color
        (Css.color_mix_var_percent ~in_space:Oklab
           ~var_name:"tw-drop-shadow-alpha" color Css.Transparent)
    in
    let supports_block = Color.color_mix_supports [ supports_decl ] in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          (decls @ [ bind_drop_shadow_color fallback_color ]);
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  (* [drop-shadow-current] and [drop-shadow-transparent] name a colour the theme
     has no token for, so the value goes in directly. *)
  let drop_shadow_keyword_color color =
    let supports_block =
      Color.color_mix_supports
        [
          bind_drop_shadow_color
            (Css.color_mix_var_percent ~in_space:Oklab
               ~var_name:"tw-drop-shadow-alpha" color Css.Transparent);
        ]
    in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          [ bind_drop_shadow_color color ];
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  let drop_shadow_keyword_color_opacity color opacity =
    let inner = Color.apply_alpha opacity color in
    let supports_block =
      Color.color_mix_supports
        [
          bind_drop_shadow_color
            (Css.color_mix_var_percent ~in_space:Oklab
               ~var_name:"tw-drop-shadow-alpha" inner Css.Transparent);
        ]
    in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          [ bind_drop_shadow_color color ];
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  (* The colour channel the bracket forms set through the shared builders. *)
  let drop_shadow_channel : Color.channel =
    {
      color = drop_shadow_color_var;
      alpha = drop_shadow_alpha_var;
      property_prefix = "drop-shadow-color";
      metadata = filter_property_metadata;
      property_rules = filter_property_rules;
    }

  (* A colour utility points [--tw-drop-shadow] at the size once the colour and
     its guarded mix are written. *)
  let with_drop_shadow_size colour =
    Group
      [
        colour;
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  let drop_shadow_bracket_color ~theme c =
    with_drop_shadow_size
      (Color.channel_bracket_color ~theme drop_shadow_channel c)

  let drop_shadow_bracket_color_opacity ~theme c opacity =
    with_drop_shadow_size
      (Color.channel_bracket_color_opacity ~theme drop_shadow_channel c opacity)

  let drop_shadow_bracket_var v =
    with_drop_shadow_size (Color.channel_bracket_var drop_shadow_channel v)

  let drop_shadow_bracket_var_opacity v opacity =
    with_drop_shadow_size
      (Color.channel_bracket_var_opacity drop_shadow_channel v opacity)

  let drop_shadow_color_opacity ?theme c shade opacity =
    let color_name = Color.scheme_color_name c shade in
    let scheme = Scheme.or_default theme in
    let value =
      match Scheme.hex_color scheme color_name with
      | Option.Some hex -> Css.hex hex
      | Option.None -> Color.to_css ?theme c shade
    in
    (* The fallback is what a browser without color-mix reads, so it has to be a
       plain hex. [Scheme.hex_color] only holds the hexes a project declared, so
       resolve the palette colour when it has none. A modifier reading a custom
       property has no percentage to fold in, so the fallback is the colour as
       the bare utility writes it. *)
    let fallback_color =
      if Option.is_some (Color.opacity_var_bare_of opacity) then value
      else
        let hex =
          match Scheme.hex_color scheme color_name with
          | Option.Some hex -> hex
          | Option.None ->
              Color.rgb_to_hex (Color.oklch_to_rgb (Color.to_oklch c shade))
        in
        Css.hex (Color.hex_with_alpha hex (Color.opacity_to_percent opacity))
    in
    let decls, color = Color.bound ?theme (Color.color_var c shade) value in
    let supports_decl =
      let inner = Color.mix_alpha opacity color in
      bind_drop_shadow_color
        (Css.color_mix_var_percent ~in_space:Oklab
           ~var_name:"tw-drop-shadow-alpha" inner Css.Transparent)
    in
    let supports_block = Color.color_mix_supports [ supports_decl ] in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          (decls @ [ bind_drop_shadow_color fallback_color ]);
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  (* The default two-layer drop shadow, the value of the deprecated
     [--drop-shadow] token. *)
  let drop_shadow_default_bodies : Css.shadow_body list =
    List.filter_map
      (function (Css.Shadow body : Css.shadow) -> Some body | _ -> None)
      [
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 2.)
          ~color:(Css.hex "#0000001a") ();
        Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 1.)
          ~color:(Css.hex "#0000000f") ();
      ]

  (* A drop shadow list under a modifier: the scale's, an arbitrary one or a
     project token's. Tailwind writes the alpha into every layer's colour the
     way it does for a box shadow; [--tw-drop-shadow] reads the size, or keeps
     the deprecated [--drop-shadow] token's own value for the bare utility. *)
  let drop_shadow_alpha_style ?(lead = []) ~shadow bodies opacity =
    let colour_of (body : Css.shadow_body) : Css.color =
      match body.color with Some c -> c | None -> Css.Current
    in
    let rebuild colours =
      let layers =
        List.map2
          (fun (body : Css.shadow_body) colour ->
            Css.Drop_shadow
              (Css.shadow ~h_offset:body.h_offset ~v_offset:body.v_offset
                 ?blur:body.blur ?spread:body.spread
                 ~color:(Css.Var (drop_shadow_color_ref colour))
                 ()))
          bodies colours
      in
      bind_drop_shadow_size
        (match layers with [ one ] -> one | many -> Css.List many)
    in
    let size_decl, supports =
      Color.shadow_alpha_decls opacity
        ~colours:(List.map colour_of bodies)
        ~rebuild
    in
    let alpha_decl = drop_shadow_alpha_decl opacity in
    let tail_decls = lead @ [ bind_drop_shadow shadow; filter filter_chain ] in
    match supports with
    | Option.None ->
        style ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules
          ((alpha_decl :: [ size_decl ]) @ tail_decls)
    | Option.Some guarded ->
        let unguarded =
          Css.rule ~selector:(Css.Selector.class_ "_") [ alpha_decl; size_decl ]
        in
        style
          ~rules:(Option.Some [ unguarded; guarded ])
          ~metadata:filter_property_metadata
          ~property_rules:filter_property_rules tail_decls

  let drop_shadow_arbitrary_opacity bodies opacity =
    drop_shadow_alpha_style ~shadow:drop_shadow_size_ref bodies opacity

  (* [drop-shadow-<name>/<alpha>] for a project [--drop-shadow-<name>]. *)
  let drop_shadow_named_opacity ?theme name opacity =
    match drop_shadow_theme_bodies ?theme name with
    | None -> invalid_arg ("drop-shadow-" ^ name ^ ": no such theme token")
    | Some (_, bodies) -> drop_shadow_arbitrary_opacity bodies opacity

  (* [drop-shadow-xl/25]: the scale's shadow, a project override of it first,
     with the alpha in place of its colour's own. *)
  let drop_shadow_size_opacity ?theme size opacity =
    let bodies =
      match drop_shadow_theme_bodies ?theme size with
      | Some (_, bodies) -> bodies
      | None -> Option.to_list (drop_shadow_scale_body size)
    in
    drop_shadow_arbitrary_opacity bodies opacity

  (* [drop-shadow/25]: the modifier recolours the shadow, so it applies to the
     size, which is what [--tw-drop-shadow-color] overrides. [--tw-drop-shadow]
     keeps the token's own value: a project that overrides [--drop-shadow] is
     referenced there, and the default two-shadow stack is written out. *)
  let drop_shadow_opacity ?theme opacity =
    match shadow_token_bodies ?theme "drop-shadow" with
    | Some (_, bodies) ->
        drop_shadow_alpha_style
          ~lead:(theme_decl_if_set ?theme "drop-shadow")
          ~shadow:(drop_shadow_theme_ref "drop-shadow")
          bodies opacity
    | None ->
        drop_shadow_alpha_style ~shadow:drop_shadow_default_literal
          drop_shadow_default_bodies opacity

  (* The filter list behind [filter-[...]] and [backdrop-filter-[...]]. A
     bracket spells its spaces with [_], so a chain such as
     [blur(4px)_saturate(150%)] has to be decoded before the filter grammar sees
     it. [None] means the bracket is not a filter list, which [of_class]
     rejects. *)
  let filter_arbitrary_value s : Css.filter option =
    let inner = Parse.bracket_inner s in
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      let ref_ : Css.filter Css.var = Var.bracket bare in
      Some (Css.Var ref_)
    else
      let cursor = Cascade.Cursor.of_string (Parse.decode_underscores inner) in
      match
        Cascade.Cursor.try_parse_full_err Css.Properties.read_filter cursor
      with
      | Ok value -> Some value
      | Error _ -> None

  let to_style theme =
    let drop_shadow_color c shade = drop_shadow_color ~theme c shade in
    let drop_shadow_color_opacity c shade opacity =
      drop_shadow_color_opacity ~theme c shade opacity
    in
    let drop_shadow_ () = drop_shadow_ ~theme () in
    let drop_shadow_named name = drop_shadow_named ~theme name in
    let drop_shadow_named_opacity name op =
      drop_shadow_named_opacity ~theme name op
    in
    let drop_shadow_size_opacity size op =
      drop_shadow_size_opacity ~theme size op
    in
    let drop_shadow_opacity op = drop_shadow_opacity ~theme op in
    function
    (* The chain on its own, every function variable it reads. *)
    | Chain target ->
        style ~metadata:(metadata target)
          ~property_rules:(property_rules target)
          (declarations target (chain target))
    | Chain_none target -> style (declarations target (None : Css.filter))
    | Chain_arbitrary (target, _, value) -> style (declarations target value)
    | Chain_raw (target, _, v) ->
        style
          (List.filter_map
             (fun property -> Parse.opaque_declaration property v)
             (property_names target))
    | Blur target -> set target `Blur (Css.Blur (Px 8.))
    | Blur_none target -> blur_none theme target
    | Blur_size (target, size) -> blur_sized target size
    | Blur_theme name -> blur_theme theme name
    | Blur_arbitrary (target, _, len) -> set target `Blur (Css.Blur len)
    | Blur_raw (target, _, value) -> set_raw target `Blur value
    | Amount (target, f, n) -> amount target (f :> scaled) (float_of_int n)
    | Amount_arbitrary (target, f, s) -> amount_arbitrary target (f :> scaled) s
    | Backdrop_opacity n -> amount Backdrop `Opacity n
    | Backdrop_opacity_arbitrary s -> amount_arbitrary Backdrop `Opacity s
    | Hue_rotate (target, n) ->
        set target `Hue_rotate (Css.Hue_rotate (make_angle n))
    | Hue_rotate_arbitrary (target, _, angle) ->
        set target `Hue_rotate (Css.Hue_rotate angle)
    | Hue_rotate_raw (target, _, value) -> set_raw target `Hue_rotate value
    | Neg_hue_rotate_arbitrary (target, _, angle) ->
        set target `Hue_rotate (Css.Hue_rotate (neg_angle angle))
    | Drop_shadow -> drop_shadow_ ()
    | Drop_shadow_xs -> drop_shadow_sized ~theme "xs"
    | Drop_shadow_sm -> drop_shadow_sized ~theme "sm"
    | Drop_shadow_md -> drop_shadow_sized ~theme "md"
    | Drop_shadow_lg -> drop_shadow_sized ~theme "lg"
    | Drop_shadow_xl -> drop_shadow_sized ~theme "xl"
    | Drop_shadow_2xl -> drop_shadow_sized ~theme "2xl"
    | Drop_shadow_multi -> drop_shadow_multi_
    | Drop_shadow_none -> drop_shadow_none
    | Drop_shadow_inherit -> drop_shadow_inherit_
    | Drop_shadow_named name -> drop_shadow_named name
    | Drop_shadow_named_opacity (name, op) -> drop_shadow_named_opacity name op
    | Drop_shadow_arbitrary (_, size) -> drop_shadow_arbitrary_impl size
    | Drop_shadow_var_opacity (_, size, opacity) ->
        drop_shadow_arbitrary_impl ~opacity size
    | Drop_shadow_arbitrary_opacity (_, bodies, op) ->
        drop_shadow_arbitrary_opacity bodies op
    | Drop_shadow_raw (_, value) -> drop_shadow_raw value
    | Drop_shadow_color (c, shade) -> drop_shadow_color c shade
    | Drop_shadow_color_opacity (c, shade, op) ->
        drop_shadow_color_opacity c shade op
    (* Tailwind reads no alpha off a named token on the bare utility and writes
       the shadow as if no modifier were given. *)
    | Drop_shadow_opacity (Color.Opacity_named _) -> drop_shadow_ ()
    | Drop_shadow_opacity op -> drop_shadow_opacity op
    | Drop_shadow_keyword_color (c, _) -> drop_shadow_keyword_color c
    | Drop_shadow_keyword_color_opacity (c, _, opacity) ->
        drop_shadow_keyword_color_opacity c opacity
    | Drop_shadow_size_opacity (size, op) -> drop_shadow_size_opacity size op
    | Drop_shadow_bracket_color (_, c) -> drop_shadow_bracket_color ~theme c
    | Drop_shadow_bracket_color_opacity (_, c, op) ->
        drop_shadow_bracket_color_opacity ~theme c op
    | Drop_shadow_bracket_cvar v -> drop_shadow_bracket_var v
    | Drop_shadow_bracket_cvar_opacity (v, op) ->
        drop_shadow_bracket_var_opacity v op

  let ( >|= ) = Parse.( >|= )
  let err_not_utility = Error (`Msg "Not a filter utility")

  (* Whether a modifier names an [--opacity-*] token. Tailwind reads no alpha
     off one on a drop-shadow size and compiles nothing for the class, where a
     colour mixes the token in. *)
  let named_alpha = function Color.Opacity_named _ -> true | _ -> false

  (* The size a bracket spells, under a modifier or not. A bracket the shadow
     reader takes whole from a custom property keeps the value and sets the
     alpha channel: there is no colour in it to fold the alpha into. *)
  let parse_drop_shadow_size base opacity =
    match opacity with
    | Color.No_opacity -> (
        match drop_shadow_arbitrary_value base with
        | Option.Some size -> Ok (Drop_shadow_arbitrary (base, size))
        | Option.None -> (
            match
              Parse.arbitrary_declaration_value (Parse.bracket_inner base)
            with
            | Some value -> Ok (Drop_shadow_raw (base, value))
            | None -> err_not_utility))
    | op when named_alpha op -> err_not_utility
    | op -> (
        match drop_shadow_bodies base with
        | Some bodies -> Ok (Drop_shadow_arbitrary_opacity (base, bodies, op))
        | None -> (
            match drop_shadow_arbitrary_value base with
            | Option.Some (Css.Drop_shadow (Css.Var _) as size) ->
                Ok (Drop_shadow_var_opacity (base, size, op))
            | Option.Some _ | Option.None -> err_not_utility))

  (* A bracket that reads as a colour, or carries a [color:] hint, is the
     shadow's colour, as [shadow-[...]] reads its bracket; anything else, a bare
     [var()] included, is its size. *)
  let parse_drop_shadow_bracket ~theme s =
    let base, opacity = Color.parse_opacity_modifier ~theme s in
    let inner = Parse.bracket_inner base in
    match Color.parse_bracket_hint inner with
    | Some (Color.Typed_var v) -> (
        match opacity with
        | Color.No_opacity -> Ok (Drop_shadow_bracket_cvar v)
        | op -> Ok (Drop_shadow_bracket_cvar_opacity (v, op)))
    | Some (Color.Plain_color c) -> (
        match opacity with
        | Color.No_opacity -> Ok (Drop_shadow_bracket_color (inner, c))
        | op -> Ok (Drop_shadow_bracket_color_opacity (inner, c, op)))
    | Some (Color.Bare_var _) | None -> parse_drop_shadow_size base opacity

  (* Tailwind orders a target's functions by name, the order [functions] lists
     them in, and the bare property after them; every backdrop utility follows
     every filter one. Each function is one slot a thousand wide, inside which
     Tailwind uses the natural candidate spelling, which handles bare defaults,
     numeric values, arbitrary values and negative angles without per-value
     keys. On [filter], the drop-shadow suborders (2690-2710) fall between
     contrast and grayscale. *)
  let slot_base = function Filter -> 0 | Backdrop -> 10_000

  let slot target f =
    let functions = functions target in
    let rank =
      match f with
      | `Chain -> List.length functions
      | #func as f -> (
          match List.find_index (( = ) f) functions with
          | Some rank -> rank
          | None -> invalid_arg (prefix target ^ func_name f ^ ": no slot"))
    in
    slot_base target + (1000 * rank)

  let suborder = function
    | Chain t | Chain_none t | Chain_arbitrary (t, _, _) | Chain_raw (t, _, _)
      ->
        slot t `Chain
    | Blur t
    | Blur_none t
    | Blur_size (t, _)
    | Blur_arbitrary (t, _, _)
    | Blur_raw (t, _, _) ->
        slot t `Blur
    | Blur_theme _ -> slot Filter `Blur
    | Amount (t, f, _) | Amount_arbitrary (t, f, _) -> slot t (f :> func)
    | Backdrop_opacity _ | Backdrop_opacity_arbitrary _ ->
        slot Backdrop `Opacity
    | Hue_rotate (t, _)
    | Hue_rotate_arbitrary (t, _, _)
    | Hue_rotate_raw (t, _, _)
    | Neg_hue_rotate_arbitrary (t, _, _) ->
        slot t `Hue_rotate
    | Drop_shadow_opacity _ -> 2690
    | Drop_shadow_size_opacity _ -> 2692
    | Drop_shadow -> 2700
    (* All size candidates share one slot: Tailwind orders them by class name,
       including arbitrary and project-defined values. *)
    | Drop_shadow_2xl | Drop_shadow_arbitrary _ | Drop_shadow_var_opacity _
    | Drop_shadow_arbitrary_opacity _ | Drop_shadow_raw _ | Drop_shadow_lg
    | Drop_shadow_md | Drop_shadow_multi | Drop_shadow_named _
    | Drop_shadow_named_opacity _ | Drop_shadow_sm | Drop_shadow_xs
    | Drop_shadow_xl ->
        2703
    | Drop_shadow_none -> 2708
    (* Every drop-shadow colour shares one slot, after the sizes: Tailwind
       orders them among themselves by class name, which the alphabetical
       tie-break already does. *)
    | Drop_shadow_keyword_color _ | Drop_shadow_keyword_color_opacity _
    | Drop_shadow_inherit | Drop_shadow_color _ | Drop_shadow_color_opacity _
    | Drop_shadow_bracket_color _ | Drop_shadow_bracket_color_opacity _
    | Drop_shadow_bracket_cvar _ | Drop_shadow_bracket_cvar_opacity _ ->
        2710

  (* The bare [blur] and [backdrop-blur] inline the deprecated [--blur] token,
     so a token the [@theme] block removed is refused here rather than read
     through a [var()] the theme layer drops. A project [--blur-<name>] token
     names a [filter] radius only. *)
  let parse_blur theme target rest =
    let name = String.concat "-" rest in
    match (rest, blur_size_of_name name) with
    | [], _ when Scheme.is_removed theme "blur" -> err_not_utility
    | [], _ -> Ok (Blur target)
    | [ "none" ], _ -> Ok (Blur_none target)
    | [ _ ], Some size -> Ok (Blur_size (target, size))
    | _ :: _, None when target = Filter && is_theme_blur theme name ->
        Ok (Blur_theme name)
    | [ s ], None when Parse.is_bracket_value s -> (
        match arbitrary_blur s with
        | Option.Some len -> Ok (Blur_arbitrary (target, s, len))
        | Option.None -> (
            match Parse.arbitrary_declaration_value (Parse.bracket_inner s) with
            | Some value -> Ok (Blur_raw (target, s, value))
            | None ->
                Error (`Msg ("Invalid " ^ prefix target ^ "blur value: " ^ s))))
    | _ -> err_not_utility

  (* [hue-rotate-N], [-hue-rotate-N] and their bracket forms. *)
  let parse_hue_rotate target ~neg rest =
    match rest with
    | [ s ] when Parse.is_bracket_value s -> (
        let spelling = Parse.bracket_inner s in
        match (parse_bracket_angle s, neg) with
        | Option.Some angle, false ->
            Ok (Hue_rotate_arbitrary (target, spelling, angle))
        | Option.Some angle, true ->
            Ok (Neg_hue_rotate_arbitrary (target, spelling, angle))
        | Option.None, false -> (
            match Parse.arbitrary_declaration_value spelling with
            | Some value -> Ok (Hue_rotate_raw (target, spelling, value))
            | None -> err_not_utility)
        | Option.None, true -> err_not_utility)
    | [ n ] when neg ->
        Parse.int_pos ~name:(prefix target ^ "hue-rotate") n >|= fun x ->
        Hue_rotate (target, -x)
    | [ n ] -> Parse.int_any n >|= fun x -> Hue_rotate (target, x)
    | _ -> err_not_utility

  let parse_drop_shadow theme = function
    (* The bare [drop-shadow] inlines the deprecated [--drop-shadow] token, and
       a sized drop shadow under an opacity inlines its own, so a token the
       [@theme] block removed is refused. *)
    | [ "drop"; "shadow" ] when Scheme.is_removed theme "drop-shadow" ->
        err_not_utility
    | [ "drop"; "shadow"; n ]
      when match fst (Color.parse_opacity_modifier n) with
           | ("xs" | "sm" | "md" | "lg" | "xl" | "2xl") as size ->
               Scheme.is_removed theme ("drop-shadow-" ^ size)
           | _ -> false ->
        err_not_utility
    (* Drop shadow with opacity modifier on the base: drop-shadow/25 *)
    | [ "drop"; s ]
      when String.starts_with ~prefix:"shadow/" s && String.length s > 7 -> (
        let _, opacity = Color.parse_opacity_modifier ~theme s in
        match opacity with
        | Color.No_opacity -> err_not_utility
        | op -> Ok (Drop_shadow_opacity op))
    (* Drop shadow *)
    | [ "drop"; "shadow" ] -> Ok Drop_shadow
    | [ "drop"; "shadow"; "xs" ] -> Ok Drop_shadow_xs
    | [ "drop"; "shadow"; "sm" ] -> Ok Drop_shadow_sm
    | [ "drop"; "shadow"; "md" ] -> Ok Drop_shadow_md
    | [ "drop"; "shadow"; "lg" ] -> Ok Drop_shadow_lg
    | [ "drop"; "shadow"; "xl" ] -> Ok Drop_shadow_xl
    | [ "drop"; "shadow"; "2xl" ] -> Ok Drop_shadow_2xl
    | [ "drop"; "shadow"; "multi" ] -> Ok Drop_shadow_multi
    | [ "drop"; "shadow"; "none" ] -> Ok Drop_shadow_none
    | [ "drop"; "shadow"; "inherit" ] -> Ok Drop_shadow_inherit
    | [ "drop"; "shadow"; "current" ] ->
        Ok (Drop_shadow_keyword_color (Css.Current, "current"))
    | [ "drop"; "shadow"; "transparent" ] ->
        Ok (Drop_shadow_keyword_color (Css.Transparent, "transparent"))
    | [ "drop"; "shadow"; s ]
      when Parse.is_bracket_value (fst (Color.parse_opacity_modifier ~theme s))
      ->
        parse_drop_shadow_bracket ~theme s
    | "drop" :: "shadow" :: rest -> (
        let full = String.concat "-" rest in
        let base, opacity = Color.parse_opacity_modifier ~theme full in
        match opacity with
        | Color.No_opacity -> (
            (* Try to parse as color *)
            match
              Color.shade_and_opacity_of_strings ~theme
                (String.split_on_char '-' base)
            with
            | Ok (c, shade, Color.No_opacity) ->
                Ok (Drop_shadow_color (c, shade))
            | Ok (c, shade, op) -> Ok (Drop_shadow_color_opacity (c, shade, op))
            (* Otherwise treat it as a custom [--drop-shadow-<name>] theme
               token. Without such a token the class names nothing: it used to
               parse and then emit an empty rule. *)
            | Error _ when drop_shadow_theme_bodies ~theme full <> Option.None
              ->
                Ok (Drop_shadow_named full)
            | Error _ -> err_not_utility)
        | op -> (
            match drop_shadow_scale_body base with
            | Option.Some _ when named_alpha op -> err_not_utility
            | Option.Some _ -> Ok (Drop_shadow_size_opacity (base, op))
            | Option.None -> (
                if base = "" then Ok (Drop_shadow_opacity op)
                else if drop_shadow_theme_bodies ~theme base <> Option.None then
                  if named_alpha op then err_not_utility
                  else Ok (Drop_shadow_named_opacity (base, op))
                else if base = "inherit" then
                  Ok (Drop_shadow_keyword_color_opacity (Css.Inherit, base, op))
                else if base = "transparent" then
                  Ok
                    (Drop_shadow_keyword_color_opacity
                       (Css.Transparent, base, op))
                else if base = "current" then
                  Ok (Drop_shadow_keyword_color_opacity (Css.Current, base, op))
                else
                  match
                    Color.shade_and_opacity_of_strings ~theme
                      (String.split_on_char '-' base)
                  with
                  | Ok (c, shade, _) ->
                      Ok (Drop_shadow_color_opacity (c, shade, op))
                  | Error _ -> err_not_utility)))
    | _ -> err_not_utility

  let amount_of_name n =
    List.find_opt (fun f -> String.equal (func_name (f :> func)) n) amounts

  (* [grayscale], [invert] and [sepia] are their own [-100]. *)
  let has_bare_form : amount -> bool = function
    | `Grayscale | `Invert | `Sepia -> true
    | `Brightness | `Contrast | `Saturate -> false

  (* A class with its target's prefix taken off. *)
  let parse_function theme target parts =
    let declaration s =
      Parse.arbitrary_declaration_value (Parse.bracket_inner s)
    in
    match parts with
    | [ "filter" ] -> Ok (Chain target)
    | [ "filter"; "none" ] -> Ok (Chain_none target)
    | [ "filter"; s ] when Parse.is_bracket_value s -> (
        match filter_arbitrary_value s with
        | Option.Some value -> Ok (Chain_arbitrary (target, s, value))
        | Option.None -> (
            match declaration s with
            | Some raw -> Ok (Chain_raw (target, s, raw))
            | None -> err_not_utility))
    | "blur" :: rest -> parse_blur theme target rest
    | "hue" :: "rotate" :: rest -> parse_hue_rotate target ~neg:false rest
    | "drop" :: _ when target = Filter -> parse_drop_shadow theme parts
    | [ "opacity"; s ] when target = Backdrop && Parse.is_bracket_value s -> (
        match declaration s with
        | Some _ -> Ok (Backdrop_opacity_arbitrary s)
        | None -> err_not_utility)
    | [ "opacity"; n ] when target = Backdrop -> (
        match Parse.decimal_float n with
        | Some f when f >= 0. -> Ok (Backdrop_opacity f)
        | _ -> err_not_utility)
    | name :: rest -> (
        match (amount_of_name name, rest) with
        | Some f, [ s ] when Parse.is_bracket_value s -> (
            match declaration s with
            | Some _ -> Ok (Amount_arbitrary (target, f, s))
            | None -> err_not_utility)
        | Some f, [ n ] ->
            Parse.int_pos ~name:(prefix target ^ name) n >|= fun x ->
            Amount (target, f, x)
        | Some f, [] when has_bare_form f -> Ok (Amount (target, f, 100))
        | _ -> err_not_utility)
    | [] -> err_not_utility

  let of_class theme class_name =
    match Parse.split_class class_name with
    | "" :: "backdrop" :: "hue" :: "rotate" :: rest ->
        parse_hue_rotate Backdrop ~neg:true rest
    | "" :: "hue" :: "rotate" :: rest -> parse_hue_rotate Filter ~neg:true rest
    | "backdrop" :: parts -> parse_function theme Backdrop parts
    | parts -> parse_function theme Filter parts

  let to_class = function
    | Chain t -> prefix t ^ "filter"
    | Chain_none t -> prefix t ^ "filter-none"
    | Chain_arbitrary (t, s, _) | Chain_raw (t, s, _) ->
        prefix t ^ "filter-" ^ s
    | Blur t -> prefix t ^ "blur"
    | Blur_none t -> prefix t ^ "blur-none"
    | Blur_size (t, size) -> prefix t ^ "blur-" ^ blur_size_name size
    | Blur_arbitrary (t, s, _) | Blur_raw (t, s, _) -> prefix t ^ "blur-" ^ s
    | Blur_theme name -> "blur-" ^ name
    | Amount (t, f, 100) when has_bare_form f ->
        prefix t ^ func_name (f :> func)
    | Amount (t, f, n) ->
        prefix t ^ func_name (f :> func) ^ "-" ^ string_of_int n
    | Amount_arbitrary (t, f, s) -> prefix t ^ func_name (f :> func) ^ "-" ^ s
    | Backdrop_opacity n ->
        "backdrop-opacity-"
        ^
        if Float.is_integer n then string_of_int (Float.to_int n)
        else Css.Pp.string_of_float n
    | Backdrop_opacity_arbitrary s -> "backdrop-opacity-" ^ s
    | Hue_rotate (t, n) ->
        let sign = if n < 0 then "-" else "" in
        sign ^ prefix t ^ "hue-rotate-" ^ string_of_int (abs n)
    | Hue_rotate_arbitrary (t, spelling, _) | Hue_rotate_raw (t, spelling, _) ->
        prefix t ^ "hue-rotate-[" ^ spelling ^ "]"
    | Neg_hue_rotate_arbitrary (t, spelling, _) ->
        "-" ^ prefix t ^ "hue-rotate-[" ^ spelling ^ "]"
    | Drop_shadow -> "drop-shadow"
    | Drop_shadow_xs -> "drop-shadow-xs"
    | Drop_shadow_sm -> "drop-shadow-sm"
    | Drop_shadow_md -> "drop-shadow-md"
    | Drop_shadow_lg -> "drop-shadow-lg"
    | Drop_shadow_xl -> "drop-shadow-xl"
    | Drop_shadow_2xl -> "drop-shadow-2xl"
    | Drop_shadow_multi -> "drop-shadow-multi"
    | Drop_shadow_none -> "drop-shadow-none"
    | Drop_shadow_inherit -> "drop-shadow-inherit"
    | Drop_shadow_named name -> "drop-shadow-" ^ name
    | Drop_shadow_named_opacity (name, op) ->
        "drop-shadow-" ^ name ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_arbitrary (s, _) -> "drop-shadow-" ^ s
    | Drop_shadow_var_opacity (s, _, op) ->
        "drop-shadow-" ^ s ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_arbitrary_opacity (s, _, op) ->
        "drop-shadow-" ^ s ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_raw (s, _) -> "drop-shadow-" ^ s
    | Drop_shadow_color (c, shade) ->
        "drop-shadow-" ^ Color.scheme_color_name c shade
    | Drop_shadow_color_opacity (c, shade, op) ->
        "drop-shadow-"
        ^ Color.scheme_color_name c shade
        ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_opacity op -> "drop-shadow/" ^ Color.pp_opacity op
    | Drop_shadow_keyword_color (_, name) -> "drop-shadow-" ^ name
    | Drop_shadow_keyword_color_opacity (_, name, opacity) ->
        "drop-shadow-" ^ name ^ "/" ^ Color.pp_opacity opacity
    | Drop_shadow_size_opacity (size, op) ->
        "drop-shadow-" ^ size ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_bracket_color (inner, _) -> "drop-shadow-[" ^ inner ^ "]"
    | Drop_shadow_bracket_color_opacity (inner, _, op) ->
        "drop-shadow-[" ^ inner ^ "]" ^ Color.opacity_suffix op
    | Drop_shadow_bracket_cvar v -> "drop-shadow-[color:" ^ v ^ "]"
    | Drop_shadow_bracket_cvar_opacity (v, op) ->
        "drop-shadow-[color:" ^ v ^ "]" ^ Color.opacity_suffix op

  let examples = [ Chain_none Filter; Chain_none Backdrop ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

(** {1 Public API - Utility Values} *)

let utility = Utility_factory.v
let blur_none = utility (Blur_none Filter)
let blur_xs = utility (Blur_size (Filter, Xs))
let blur_sm = utility (Blur_size (Filter, Sm))
let blur = utility (Blur Filter)
let blur_md = utility (Blur_size (Filter, Md))
let blur_lg = utility (Blur_size (Filter, Lg))
let blur_xl = utility (Blur_size (Filter, Xl))
let blur_2xl = utility (Blur_size (Filter, Two_xl))
let blur_3xl = utility (Blur_size (Filter, Three_xl))
let brightness n = utility (Amount (Filter, `Brightness, n))
let contrast n = utility (Amount (Filter, `Contrast, n))
let grayscale n = utility (Amount (Filter, `Grayscale, n))
let saturate n = utility (Amount (Filter, `Saturate, n))
let sepia n = utility (Amount (Filter, `Sepia, n))
let invert n = utility (Amount (Filter, `Invert, n))
let hue_rotate n = utility (Hue_rotate (Filter, n))
let backdrop_blur_none = utility (Blur_none Backdrop)
let backdrop_blur_xs = utility (Blur_size (Backdrop, Xs))
let backdrop_blur_sm = utility (Blur_size (Backdrop, Sm))
let backdrop_blur = utility (Blur Backdrop)
let backdrop_blur_md = utility (Blur_size (Backdrop, Md))
let backdrop_blur_lg = utility (Blur_size (Backdrop, Lg))
let backdrop_blur_xl = utility (Blur_size (Backdrop, Xl))
let backdrop_blur_2xl = utility (Blur_size (Backdrop, Two_xl))
let backdrop_blur_3xl = utility (Blur_size (Backdrop, Three_xl))
let backdrop_brightness n = utility (Amount (Backdrop, `Brightness, n))
let backdrop_contrast n = utility (Amount (Backdrop, `Contrast, n))
let backdrop_opacity n = utility (Backdrop_opacity n)
let backdrop_saturate n = utility (Amount (Backdrop, `Saturate, n))

let backdrop_grayscale ?(n = 100) () =
  utility (Amount (Backdrop, `Grayscale, n))

let backdrop_invert ?(n = 100) () = utility (Amount (Backdrop, `Invert, n))
let backdrop_sepia ?(n = 100) () = utility (Amount (Backdrop, `Sepia, n))
let backdrop_hue_rotate n = utility (Hue_rotate (Backdrop, n))
