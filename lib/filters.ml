(** Filter utilities for visual effects like blur, brightness, and backdrop
    filters. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css
  module Option = Stdlib.Option

  type t =
    | Filter
    | Filter_none
    | Filter_arbitrary of string
    | Blur_none
    | Blur_xs
    | Blur_sm
    | Blur
    | Blur_md
    | Blur_lg
    | Blur_xl
    | Blur_2xl
    | Blur_3xl
    | Blur_arbitrary of string
    | Brightness of int
    | Brightness_arbitrary of string
    | Contrast of int
    | Contrast_arbitrary of string
    | Grayscale of int
    | Grayscale_arbitrary of string
    | Saturate of int
    | Saturate_arbitrary of string
    | Sepia of int
    | Sepia_arbitrary of string
    | Invert of int
    | Invert_arbitrary of string
    | Hue_rotate of int
    | Hue_rotate_arbitrary of Css.angle
    | Neg_hue_rotate_arbitrary of Css.angle
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
    | Drop_shadow_arbitrary of string
    | Drop_shadow_color of Color.color * int
    | Drop_shadow_color_opacity of Color.color * int * Color.opacity_modifier
    | Drop_shadow_opacity of Color.opacity_modifier
    | Backdrop_filter
    | Backdrop_filter_none
    | Backdrop_filter_arbitrary of string
    | Backdrop_blur_none
    | Backdrop_blur_xs
    | Backdrop_blur_sm
    | Backdrop_blur
    | Backdrop_blur_md
    | Backdrop_blur_lg
    | Backdrop_blur_xl
    | Backdrop_blur_2xl
    | Backdrop_blur_3xl
    | Backdrop_blur_arbitrary of string
    | Backdrop_brightness of int
    | Backdrop_brightness_arbitrary of string
    | Backdrop_contrast of int
    | Backdrop_contrast_arbitrary of string
    | Backdrop_opacity of float
    | Backdrop_opacity_arbitrary of string
    | Backdrop_saturate of int
    | Backdrop_saturate_arbitrary of string
    | Backdrop_grayscale of int
    | Backdrop_grayscale_arbitrary of string
    | Backdrop_invert of int
    | Backdrop_invert_arbitrary of string
    | Backdrop_sepia of int
    | Backdrop_sepia_arbitrary of string
    | Backdrop_hue_rotate of int
    | Backdrop_hue_rotate_arbitrary of Css.angle
    | Neg_backdrop_hue_rotate_arbitrary of Css.angle

  type Utility.base += Self of t

  let name = "filters"
  let priority = 28

  (* Register filter variables in the Var system for @property generation *)
  let blur_var =
    Var.channel ~needs_property:true ~property_order:60 ~family:`Filter
      Css.Filter "tw-blur"

  let brightness_var =
    Var.channel ~needs_property:true ~property_order:61 ~family:`Filter
      Css.Filter "tw-brightness"

  let contrast_var =
    Var.channel ~needs_property:true ~property_order:62 ~family:`Filter
      Css.Filter "tw-contrast"

  let grayscale_var =
    Var.channel ~needs_property:true ~property_order:63 ~family:`Filter
      Css.Filter "tw-grayscale"

  let hue_rotate_var =
    Var.channel ~needs_property:true ~property_order:64 ~family:`Filter
      Css.Filter "tw-hue-rotate"

  let invert_var =
    Var.channel ~needs_property:true ~property_order:65 ~family:`Filter
      Css.Filter "tw-invert"

  let opacity_var =
    Var.channel ~needs_property:true ~property_order:66 ~family:`Filter
      Css.Filter "tw-opacity"

  let saturate_var =
    Var.channel ~needs_property:true ~property_order:67 ~family:`Filter
      Css.Filter "tw-saturate"

  let sepia_var =
    Var.channel ~needs_property:true ~property_order:68 ~family:`Filter
      Css.Filter "tw-sepia"

  let drop_shadow_var =
    Var.channel ~needs_property:true ~property_order:69 ~family:`Drop_shadow
      Css.Filter "tw-drop-shadow"

  let drop_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:70 ~family:`Drop_shadow
      Css.Color "tw-drop-shadow-color"

  let drop_shadow_alpha_var =
    Var.property_default Css.Float ~initial:100.0 ~property_order:71
      ~family:`Drop_shadow "tw-drop-shadow-alpha"

  let drop_shadow_size_var =
    Var.channel ~needs_property:true ~property_order:72 ~family:`Drop_shadow
      Css.Filter "tw-drop-shadow-size"

  (* Register backdrop-filter variables in the Var system *)
  let backdrop_blur_var =
    Var.channel ~needs_property:true ~property_order:80 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-blur"

  let backdrop_brightness_var =
    Var.channel ~needs_property:true ~property_order:81 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-brightness"

  let backdrop_contrast_var =
    Var.channel ~needs_property:true ~property_order:82 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-contrast"

  let backdrop_grayscale_var =
    Var.channel ~needs_property:true ~property_order:83 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-grayscale"

  let backdrop_hue_rotate_var =
    Var.channel ~needs_property:true ~property_order:84 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-hue-rotate"

  let backdrop_invert_var =
    Var.channel ~needs_property:true ~property_order:85 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-invert"

  let backdrop_opacity_var =
    Var.channel ~needs_property:true ~property_order:86 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-opacity"

  let backdrop_saturate_var =
    Var.channel ~needs_property:true ~property_order:87 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-saturate"

  let backdrop_sepia_var =
    Var.channel ~needs_property:true ~property_order:88 ~family:`Backdrop_filter
      Css.Filter "tw-backdrop-sepia"

  let property_rule_or_empty var =
    match Var.property_rule var with None -> Css.empty | Some r -> r

  (* Property rules for all filter variables *)
  let filter_property_rules =
    Css.concat
      [
        property_rule_or_empty blur_var;
        property_rule_or_empty brightness_var;
        property_rule_or_empty contrast_var;
        property_rule_or_empty grayscale_var;
        property_rule_or_empty hue_rotate_var;
        property_rule_or_empty invert_var;
        property_rule_or_empty opacity_var;
        property_rule_or_empty saturate_var;
        property_rule_or_empty sepia_var;
        property_rule_or_empty drop_shadow_var;
        property_rule_or_empty drop_shadow_color_var;
        Var.property_rules drop_shadow_alpha_var;
        property_rule_or_empty drop_shadow_size_var;
      ]

  (* Property rules for all backdrop-filter variables *)
  let backdrop_filter_property_rules =
    Css.concat
      [
        property_rule_or_empty backdrop_blur_var;
        property_rule_or_empty backdrop_brightness_var;
        property_rule_or_empty backdrop_contrast_var;
        property_rule_or_empty backdrop_grayscale_var;
        property_rule_or_empty backdrop_hue_rotate_var;
        property_rule_or_empty backdrop_invert_var;
        property_rule_or_empty backdrop_opacity_var;
        property_rule_or_empty backdrop_saturate_var;
        property_rule_or_empty backdrop_sepia_var;
      ]

  (* Composable filter chain: filter: var(--tw-blur, ) var(--tw-brightness, )
     ... *)
  let composable_filter_chain : Css.filter =
    List
      [
        Css.filter_var_empty "tw-blur";
        Css.filter_var_empty "tw-brightness";
        Css.filter_var_empty "tw-contrast";
        Css.filter_var_empty "tw-grayscale";
        Css.filter_var_empty "tw-hue-rotate";
        Css.filter_var_empty "tw-invert";
        Css.filter_var_empty "tw-saturate";
        Css.filter_var_empty "tw-sepia";
        Css.filter_var_empty "tw-drop-shadow";
      ]

  (* Helper: set a --tw-<name> filter var and output composable chain *)
  let filter_var_decl var_name value =
    let name =
      if String.starts_with ~prefix:"--" var_name then
        String.sub var_name 2 (String.length var_name - 2)
      else var_name
    in
    fst (Css.var ~layer:"utilities" name Css.Filter value)

  let set_filter_var var_name (value : Css.filter) =
    style ~property_rules:filter_property_rules
      [ filter_var_decl var_name value; filter composable_filter_chain ]

  (* Generate a theme-layer declaration for a theme variable if its value is
     set. This produces the --name: value entry for the :root, :host block. *)
  let theme_decl_if_set ?theme name =
    match Scheme.theme_value theme name with
    | Some value -> [ Css.custom_property ~layer:"theme" ("--" ^ name) value ]
    | None -> []

  (* Helper: set a --tw-<name> filter var using a theme var reference. Creates
     var(--theme-name) without fallback since the theme defines it. Also
     generates a theme-layer declaration for the theme variable. Returns a
     function (unit -> Style.t) so that theme_decl_if_set is evaluated lazily at
     utility evaluation time, not at module init time. *)
  let set_filter_var_theme ?theme var_name theme_name
      (make_filter : Css.length -> Css.filter) () =
    let ref_ : Css.length Css.var = Var.theme_ref theme_name in
    style ~property_rules:filter_property_rules
      (theme_decl_if_set ?theme theme_name
      @ [
          filter_var_decl var_name (make_filter (Var ref_));
          filter composable_filter_chain;
        ])

  let blur_none ?theme () =
    match Scheme.theme_value theme "blur-none" with
    | Some _ ->
        set_filter_var_theme ?theme "--tw-blur" "blur-none" (fun l -> Blur l) ()
    | None ->
        style
          [
            Css.custom_property ~layer:"utilities" "--tw-blur" " ";
            filter composable_filter_chain;
          ]

  (* Tailwind v4 default blur theme tokens (in px). Each [.blur-<size>] utility
     binds its theme token and references it in [--tw-blur]; the [.blur] keyword
     utility (no suffix) has no theme token in v4 and emits the literal
     [blur(8px)] inline. *)
  let blur_xs_var = Var.theme Css.Length "blur-xs" ~order:(8, 0)
  let blur_sm_var = Var.theme Css.Length "blur-sm" ~order:(8, 1)
  let blur_md_var = Var.theme Css.Length "blur-md" ~order:(8, 2)
  let blur_lg_var = Var.theme Css.Length "blur-lg" ~order:(8, 3)
  let blur_xl_var = Var.theme Css.Length "blur-xl" ~order:(8, 4)
  let blur_2xl_var = Var.theme Css.Length "blur-2xl" ~order:(8, 5)
  let blur_3xl_var = Var.theme Css.Length "blur-3xl" ~order:(8, 6)

  let blur_size_utility (theme_var : Css.length Var.theme) default_px () =
    let decl, ref_ = Var.binding theme_var (Css.Px default_px) in
    style ~property_rules:filter_property_rules
      [
        decl;
        filter_var_decl "--tw-blur" (Blur (Var ref_));
        filter composable_filter_chain;
      ]

  let blur_xs = blur_size_utility blur_xs_var 4.
  let blur_sm = blur_size_utility blur_sm_var 8.
  let blur_md = blur_size_utility blur_md_var 12.
  let blur_lg = blur_size_utility blur_lg_var 16.
  let blur_xl = blur_size_utility blur_xl_var 24.
  let blur_2xl = blur_size_utility blur_2xl_var 40.
  let blur_3xl = blur_size_utility blur_3xl_var 64.

  (* [.blur] (no suffix) is the keyword utility - inline literal blur(8px). *)
  let blur () =
    style ~property_rules:filter_property_rules
      [
        filter_var_decl "--tw-blur" (Blur (Px 8.));
        filter composable_filter_chain;
      ]

  (* Parse bracket content for length values (e.g., "4px", "0.5rem") *)
  let parse_bracket_length s =
    let inner = Parse.bracket_inner s in
    let slen = String.length inner in
    let i = ref 0 in
    while
      !i < slen && ((inner.[!i] >= '0' && inner.[!i] <= '9') || inner.[!i] = '.')
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
          | "px" -> Option.Some (Px n : Css.length)
          | "rem" -> Option.Some (Rem n)
          | "em" -> Option.Some (Em n)
          | _ -> Option.None)

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
  let parse_bracket_numpct inner : Css.number_percentage =
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      Var (Var.bracket bare)
    else
      let len = String.length inner in
      if len > 1 && inner.[len - 1] = '%' then
        let num_s = String.sub inner 0 (len - 1) in
        match Float.of_string_opt num_s with
        | Option.Some f -> Pct f
        | Option.None -> Num 0.
      else
        match Float.of_string_opt inner with
        | Option.Some f -> Num f
        | Option.None -> Num 0.

  let blur_arbitrary s =
    match parse_bracket_length s with
    | Option.Some len -> set_filter_var "--tw-blur" (Blur len)
    | Option.None ->
        let inner = Parse.bracket_inner s in
        if Parse.is_var inner then
          let bare = Parse.extract_var_name inner in
          let ref_ : Css.length Css.var = Var.bracket bare in
          set_filter_var "--tw-blur" (Blur (Var ref_))
        else invalid_arg ("Invalid blur value: " ^ s)

  let brightness_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-brightness" (Brightness np)

  let contrast_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-contrast" (Contrast np)

  let grayscale_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-grayscale" (Grayscale np)

  let saturate_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-saturate" (Saturate np)

  let sepia_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-sepia" (Sepia np)

  let invert_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_filter_var "--tw-invert" (Invert np)

  let brightness n =
    set_filter_var "--tw-brightness" (Brightness (Pct (float_of_int n)))

  let contrast n =
    set_filter_var "--tw-contrast" (Contrast (Pct (float_of_int n)))

  let grayscale n =
    set_filter_var "--tw-grayscale" (Grayscale (Pct (float_of_int n)))

  let saturate n =
    set_filter_var "--tw-saturate" (Saturate (Pct (float_of_int n)))

  let sepia n = set_filter_var "--tw-sepia" (Sepia (Pct (float_of_int n)))
  let invert n = set_filter_var "--tw-invert" (Invert (Pct (float_of_int n)))

  (* Helper to create an angle with calc for negative values *)
  let make_angle n =
    if n >= 0 then Deg (float_of_int n)
    else Calc (Expr (Val (Deg (float_of_int (abs n))), Mul, Num (-1.)))

  let hue_rotate n =
    set_filter_var "--tw-hue-rotate" (Hue_rotate (make_angle n))

  let hue_rotate_arbitrary angle =
    set_filter_var "--tw-hue-rotate" (Hue_rotate angle)

  let neg_hue_rotate_arbitrary angle =
    let neg : Css.angle = Calc (Expr (Val angle, Mul, Num (-1.))) in
    set_filter_var "--tw-hue-rotate" (Hue_rotate neg)

  (* Drop-shadow utilities *)
  let bind_drop_shadow value = fst (Var.binding drop_shadow_var value)
  let bind_drop_shadow_size value = fst (Var.binding drop_shadow_size_var value)

  let bind_drop_shadow_color value =
    fst (Var.binding drop_shadow_color_var value)

  let drop_shadow_size_ref : Css.filter =
    Css.Var (Var.bracket "tw-drop-shadow-size")

  let drop_shadow_theme_ref name : Css.filter =
    Css.Drop_shadow (Css.Var (Var.bracket name) : Css.shadow)

  (* Theme tokens for the sized drop-shadow utilities. Tailwind v4 defines a
     [--drop-shadow-<size>] design token in the theme layer and references it
     from [--tw-drop-shadow]. These tokens sort after border-radius (order 7)
     and before blur (order 8). Bound via [Var.binding] so the theme declaration
     is always emitted with its default value and stays overridable through an
     [@theme] token override threaded via [Scheme.t]. *)
  let drop_shadow_xs_var = Var.theme Css.Shadow "drop-shadow-xs" ~order:(7, 8)
  let drop_shadow_sm_var = Var.theme Css.Shadow "drop-shadow-sm" ~order:(7, 9)
  let drop_shadow_md_var = Var.theme Css.Shadow "drop-shadow-md" ~order:(7, 10)
  let drop_shadow_lg_var = Var.theme Css.Shadow "drop-shadow-lg" ~order:(7, 11)
  let drop_shadow_xl_var = Var.theme Css.Shadow "drop-shadow-xl" ~order:(7, 12)
  let drop_shadow_2xl_var = Var.theme Css.Shadow "drop-shadow-2xl" ~order:(7, 13)

  let drop_shadow_color_ref fallback =
    Var.reference_with_fallback drop_shadow_color_var fallback

  let drop_shadow_filter h v blur fallback =
    Css.Drop_shadow
      (Css.shadow ~h_offset:h ~v_offset:v ?blur
         ~color:(Css.Var (drop_shadow_color_ref fallback))
         ())

  let parse_filter_value css =
    let cursor = Cascade.Cursor.of_string css in
    Cascade.Cursor.try_parse_full_err Css.Properties.read_filter cursor

  let drop_shadow_none =
    style ~property_rules:filter_property_rules
      [
        Css.custom_property ~layer:"utilities" "--tw-drop-shadow" " ";
        filter composable_filter_chain;
      ]

  (* Bare [.drop-shadow]. In the default Tailwind v4 theme this token is a
     two-shadow stack inlined as a literal (no [--drop-shadow] theme var). When
     a custom theme overrides [--drop-shadow] with a single shadow value, it is
     referenced via [drop-shadow(var(--drop-shadow))] instead. *)
  let drop_shadow_override ?theme () =
    style ~property_rules:filter_property_rules
      (theme_decl_if_set ?theme "drop-shadow"
      @ [
          bind_drop_shadow_size
            (drop_shadow_filter Zero (Px 1.) (Some (Px 1.))
               (Css.hex "#0000000d"));
          bind_drop_shadow (drop_shadow_theme_ref "drop-shadow");
          filter composable_filter_chain;
        ])

  let drop_shadow_default () =
    let fallback_a = Css.hex "#0000001a" in
    let fallback_b = Css.hex "#0000000f" in
    let size : Css.filter =
      Css.List
        [
          drop_shadow_filter Zero (Px 1.) (Some (Px 2.)) fallback_a;
          drop_shadow_filter Zero (Px 1.) (Some (Px 1.)) fallback_b;
        ]
    in
    let literal : Css.filter =
      Css.List
        [
          Css.Drop_shadow
            (Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 2.)
               ~color:fallback_a ());
          Css.Drop_shadow
            (Css.shadow ~h_offset:Zero ~v_offset:(Px 1.) ~blur:(Px 1.)
               ~color:fallback_b ());
        ]
    in
    style ~property_rules:filter_property_rules
      [
        bind_drop_shadow_size size;
        bind_drop_shadow literal;
        filter composable_filter_chain;
      ]

  let drop_shadow_ ?theme () =
    match Scheme.theme_value theme "drop-shadow" with
    | Some _ -> drop_shadow_override ?theme ()
    | None -> drop_shadow_default ()

  (* A sized drop-shadow utility: [drop-shadow-{sm,md,lg,xl,2xl}].

     Emits the [--drop-shadow-<size>] theme token with its default value,
     references it from [--tw-drop-shadow], and builds [--tw-drop-shadow-size]
     from the same geometry with the color hoisted into the
     [--tw-drop-shadow-color] fallback. The theme value is bound (always
     present, overridable). [theme_var] carries the design token; [name] is its
     bare name. *)
  let drop_shadow_sized theme_var name ~h ~v ~blur ~color () =
    let theme_decl, _ref =
      Var.binding theme_var (Css.shadow ~h_offset:h ~v_offset:v ~blur ~color ())
    in
    style ~property_rules:filter_property_rules
      [
        theme_decl;
        bind_drop_shadow_size (drop_shadow_filter h v (Some blur) color);
        bind_drop_shadow (drop_shadow_theme_ref name);
        filter composable_filter_chain;
      ]

  let drop_shadow_xs_ () =
    drop_shadow_sized drop_shadow_xs_var "drop-shadow-xs" ~h:Zero ~v:(Px 1.)
      ~blur:(Px 1.) ~color:(Css.hex "#0000000d") ()

  let drop_shadow_sm_ () =
    drop_shadow_sized drop_shadow_sm_var "drop-shadow-sm" ~h:Zero ~v:(Px 1.)
      ~blur:(Px 2.) ~color:(Css.hex "#00000026") ()

  let drop_shadow_md_ () =
    drop_shadow_sized drop_shadow_md_var "drop-shadow-md" ~h:Zero ~v:(Px 3.)
      ~blur:(Px 3.) ~color:(Css.hex "#0000001f") ()

  let drop_shadow_lg_ () =
    drop_shadow_sized drop_shadow_lg_var "drop-shadow-lg" ~h:Zero ~v:(Px 4.)
      ~blur:(Px 4.) ~color:(Css.hex "#00000026") ()

  let drop_shadow_xl_ () =
    drop_shadow_sized drop_shadow_xl_var "drop-shadow-xl" ~h:Zero ~v:(Px 9.)
      ~blur:(Px 7.) ~color:(Css.hex "#0000001a") ()

  let drop_shadow_2xl_ () =
    drop_shadow_sized drop_shadow_2xl_var "drop-shadow-2xl" ~h:Zero ~v:(Px 25.)
      ~blur:(Px 25.) ~color:(Css.hex "#00000026") ()

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

  (* A theme-token drop-shadow such as [drop-shadow-calc] backed by a custom
     [--drop-shadow-<name>] theme value. Parses the theme value into one or more
     shadow bodies, hoists each colour into [--tw-drop-shadow-color], and
     references the token from [--tw-drop-shadow]. *)
  let drop_shadow_named ?theme name =
    let theme_name = "drop-shadow-" ^ name in
    match Scheme.theme_value theme theme_name with
    | None -> style []
    | Some value ->
        let cursor = Cascade.Cursor.of_string value in
        let body_of (sh : Css.shadow) : Css.shadow_body option =
          match sh with Css.Shadow body -> Some body | _ -> None
        in
        let bodies =
          match
            Cascade.Cursor.try_parse_full_err Css.Properties.read_shadow cursor
          with
          | Ok (Css.List shadows) -> List.filter_map body_of shadows
          | Ok sh -> ( match body_of sh with Some b -> [ b ] | None -> [])
          | Error _ -> []
        in
        if bodies = [] then style []
        else
          let size : Css.filter =
            match List.map drop_shadow_size_of_body bodies with
            | [ single ] -> single
            | many -> Css.List many
          in
          style ~property_rules:filter_property_rules
            [
              Css.custom_property ~layer:"theme" ("--" ^ theme_name) value;
              bind_drop_shadow_size size;
              bind_drop_shadow (drop_shadow_theme_ref theme_name);
              filter composable_filter_chain;
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
    style ~property_rules:filter_property_rules
      [
        bind_drop_shadow_size size;
        bind_drop_shadow literal;
        filter composable_filter_chain;
      ]

  let drop_shadow_arbitrary_impl s =
    let inner = Parse.bracket_inner s in
    let inner = String.map (fun c -> if c = '_' then ' ' else c) inner in
    let parts = String.split_on_char ' ' inner in
    let non_color, color =
      match List.rev parts with
      | c :: rest -> (String.concat " " (List.rev rest), c)
      | [] -> (inner, "black")
    in
    match
      parse_filter_value
        ("drop-shadow(" ^ non_color ^ " var(--tw-drop-shadow-color, " ^ color
       ^ "))")
    with
    | Ok size ->
        style ~property_rules:filter_property_rules
          [
            bind_drop_shadow_size size;
            bind_drop_shadow drop_shadow_size_ref;
            filter composable_filter_chain;
          ]
    | Error _ -> style []

  let drop_shadow_inherit_ =
    style ~property_rules:filter_property_rules
      [
        bind_drop_shadow_color Css.Inherit;
        bind_drop_shadow drop_shadow_size_ref;
      ]

  let drop_shadow_color ?theme c shade =
    let color_name = Color.scheme_color_name c shade in
    let scheme = match theme with Some t -> t | None -> Scheme.default in
    (* srgb fallback: the scheme hex when defined, else the colour resolved
       directly (oklch), matching Tailwind. The default scheme has no hex
       colours, so the old [Scheme.hex_color]-only path raised on every
       drop-shadow-<color>. *)
    let fallback_color =
      match Scheme.hex_color scheme color_name with
      | Option.Some hex -> Css.hex hex
      | Option.None -> Color.to_css c shade
    in
    let color_ref : Css.color Css.var = Var.bracket ("color-" ^ color_name) in
    let supports_decl =
      bind_drop_shadow_color
        (Css.color_mix_var_percent ~in_space:Oklab
           ~var_name:"tw-drop-shadow-alpha" (Css.Var color_ref) Css.Transparent)
    in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ supports_decl ] ]
    in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          (theme_decl_if_set ?theme ("color-" ^ color_name)
          @ [ bind_drop_shadow_color fallback_color ]);
        style ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  let drop_shadow_color_opacity ?theme c shade opacity =
    let color_name = Color.scheme_color_name c shade in
    let scheme = match theme with Some t -> t | None -> Scheme.default in
    let percent = Color.opacity_to_percent opacity in
    (* srgb fallback: a scheme hex gets a hex+alpha; without one (the default
       scheme) mix the resolved colour in srgb, matching Tailwind. The old path
       raised when the scheme had no hex. *)
    let fallback_color =
      match Scheme.hex_color scheme color_name with
      | Option.Some hex -> Css.hex (Color.hex_with_alpha hex percent)
      | Option.None ->
          Css.color_mix ~in_space:Srgb (Color.to_css c shade) Css.Transparent
            ~percent1:percent
    in
    let color_ref : Css.color Css.var = Var.bracket ("color-" ^ color_name) in
    let supports_decl =
      let inner =
        Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
          ~percent1:percent
      in
      bind_drop_shadow_color
        (Css.color_mix_var_percent ~in_space:Oklab
           ~var_name:"tw-drop-shadow-alpha" inner Css.Transparent)
    in
    let supports_block =
      Css.supports ~condition:Color.color_mix_supports_condition
        [ Css.rule ~selector:(Css.Selector.class_ "_") [ supports_decl ] ]
    in
    Group
      [
        style ~rules:(Option.Some [ supports_block ])
          (theme_decl_if_set ?theme ("color-" ^ color_name)
          @ [ bind_drop_shadow_color fallback_color ]);
        style ~property_rules:filter_property_rules
          [ bind_drop_shadow drop_shadow_size_ref ];
      ]

  let drop_shadow_opacity ?theme opacity =
    let alpha_str =
      match opacity with
      | Color.Opacity_percent p -> string_of_int (int_of_float p) ^ "%"
      | _ -> "100%"
    in
    let percent =
      match opacity with Color.Opacity_percent p -> p | _ -> 100.
    in
    let fallback = Color.hex_to_oklab_alpha "#000000" (percent /. 100.) in
    style ~property_rules:filter_property_rules
      (theme_decl_if_set ?theme "drop-shadow"
      @ [
          Css.custom_property ~layer:"utilities" "--tw-drop-shadow-alpha"
            alpha_str;
          bind_drop_shadow_size
            (drop_shadow_filter Zero (Px 1.) (Some (Px 1.)) fallback);
          bind_drop_shadow (drop_shadow_theme_ref "drop-shadow");
          filter composable_filter_chain;
        ])

  (* Filter arbitrary - direct value *)
  let filter_arbitrary s =
    let inner = Parse.bracket_inner s in
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      let ref_ : Css.filter Css.var = Var.bracket bare in
      style [ filter (Var ref_) ]
    else
      let cursor = Cascade.Cursor.of_string inner in
      match
        Cascade.Cursor.try_parse_full_err Css.Properties.read_filter cursor
      with
      | Ok value -> style [ filter value ]
      | Error _ -> style []

  (* Composable backdrop-filter chain *)
  let composable_backdrop_filter_chain : Css.filter =
    List
      [
        Css.filter_var_empty "tw-backdrop-blur";
        Css.filter_var_empty "tw-backdrop-brightness";
        Css.filter_var_empty "tw-backdrop-contrast";
        Css.filter_var_empty "tw-backdrop-grayscale";
        Css.filter_var_empty "tw-backdrop-hue-rotate";
        Css.filter_var_empty "tw-backdrop-invert";
        Css.filter_var_empty "tw-backdrop-opacity";
        Css.filter_var_empty "tw-backdrop-saturate";
        Css.filter_var_empty "tw-backdrop-sepia";
      ]

  (* Helper: set a --tw-backdrop-<name> var and output composable backdrop
     chain *)
  let set_backdrop_var var_name (value : Css.filter) =
    style ~property_rules:backdrop_filter_property_rules
      [
        filter_var_decl var_name value;
        Css.webkit_backdrop_filter composable_backdrop_filter_chain;
        backdrop_filter composable_backdrop_filter_chain;
      ]

  let set_backdrop_var_theme ?theme var_name theme_name
      (make_filter : Css.length -> Css.filter) () =
    (* Tailwind uses themeKeys: ['--backdrop-X', '--X'] fallback. Check
       backdrop-specific theme first, then base theme. *)
    let fallback_name =
      let prefix = "backdrop-" in
      let plen = String.length prefix in
      if
        String.length theme_name > plen && String.sub theme_name 0 plen = prefix
      then Some (String.sub theme_name plen (String.length theme_name - plen))
      else None
    in
    let actual_theme_name =
      match Scheme.theme_value theme theme_name with
      | Some _ -> theme_name
      | None -> (
          match fallback_name with
          | Some fb -> (
              match Scheme.theme_value theme fb with
              | Some _ -> fb
              | None -> theme_name)
          | None -> theme_name)
    in
    let ref_ : Css.length Css.var = Var.theme_ref actual_theme_name in
    style ~property_rules:backdrop_filter_property_rules
      (theme_decl_if_set ?theme actual_theme_name
      @ [
          filter_var_decl var_name (make_filter (Var ref_));
          Css.webkit_backdrop_filter composable_backdrop_filter_chain;
          backdrop_filter composable_backdrop_filter_chain;
        ])

  let backdrop_blur_none ?theme () =
    match Scheme.theme_value theme "backdrop-blur-none" with
    | Some _ ->
        set_backdrop_var_theme ?theme "--tw-backdrop-blur" "backdrop-blur-none"
          (fun l -> Blur l)
          ()
    | None -> (
        match Scheme.theme_value theme "blur-none" with
        | Some _ ->
            set_backdrop_var_theme ?theme "--tw-backdrop-blur"
              "backdrop-blur-none"
              (fun l -> Blur l)
              ()
        | None ->
            style ~property_rules:backdrop_filter_property_rules
              [
                Css.custom_property ~layer:"utilities" "--tw-backdrop-blur" " ";
                Css.webkit_backdrop_filter composable_backdrop_filter_chain;
                backdrop_filter composable_backdrop_filter_chain;
              ])

  (* Each backdrop-blur-<size> binds the unified v4 --blur-<size> token and
     references it; Tailwind dropped the separate --backdrop-blur-* tokens, so
     the default theme ships only --blur-*. *)
  let backdrop_blur_size (theme_var : Css.length Var.theme) default_px () =
    let decl, ref_ = Var.binding theme_var (Css.Px default_px) in
    style ~property_rules:backdrop_filter_property_rules
      [
        decl;
        filter_var_decl "--tw-backdrop-blur" (Blur (Var ref_));
        Css.webkit_backdrop_filter composable_backdrop_filter_chain;
        backdrop_filter composable_backdrop_filter_chain;
      ]

  let backdrop_blur_xs = backdrop_blur_size blur_xs_var 4.
  let backdrop_blur_sm = backdrop_blur_size blur_sm_var 8.

  (* [.backdrop-blur] (no suffix) emits literal [blur(8px)] in v4. *)
  let backdrop_blur () =
    style ~property_rules:backdrop_filter_property_rules
      [
        filter_var_decl "--tw-backdrop-blur" (Blur (Px 8.));
        Css.webkit_backdrop_filter composable_backdrop_filter_chain;
        backdrop_filter composable_backdrop_filter_chain;
      ]

  let backdrop_blur_md = backdrop_blur_size blur_md_var 12.
  let backdrop_blur_lg = backdrop_blur_size blur_lg_var 16.
  let backdrop_blur_xl = backdrop_blur_size blur_xl_var 24.
  let backdrop_blur_2xl = backdrop_blur_size blur_2xl_var 40.
  let backdrop_blur_3xl = backdrop_blur_size blur_3xl_var 64.

  let backdrop_blur_arbitrary s =
    match parse_bracket_length s with
    | Option.Some len -> set_backdrop_var "--tw-backdrop-blur" (Blur len)
    | Option.None ->
        let inner = Parse.bracket_inner s in
        if Parse.is_var inner then
          let bare = Parse.extract_var_name inner in
          let ref_ : Css.length Css.var = Var.bracket bare in
          set_backdrop_var "--tw-backdrop-blur" (Blur (Var ref_))
        else invalid_arg ("Invalid backdrop-blur value: " ^ s)

  let backdrop_brightness n =
    set_backdrop_var "--tw-backdrop-brightness"
      (Brightness (Pct (float_of_int n)))

  let backdrop_brightness_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-brightness" (Brightness np)

  let backdrop_contrast n =
    set_backdrop_var "--tw-backdrop-contrast" (Contrast (Pct (float_of_int n)))

  let backdrop_contrast_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-contrast" (Contrast np)

  let backdrop_opacity n =
    set_backdrop_var "--tw-backdrop-opacity" (Opacity (Pct n))

  let backdrop_opacity_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-opacity" (Opacity np)

  let backdrop_saturate n =
    set_backdrop_var "--tw-backdrop-saturate" (Saturate (Pct (float_of_int n)))

  let backdrop_saturate_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-saturate" (Saturate np)

  let backdrop_grayscale_default =
    set_backdrop_var "--tw-backdrop-grayscale" (Grayscale (Pct 100.))

  let backdrop_grayscale n =
    set_backdrop_var "--tw-backdrop-grayscale"
      (Grayscale (Pct (float_of_int n)))

  let backdrop_grayscale_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-grayscale" (Grayscale np)

  let backdrop_invert_default =
    set_backdrop_var "--tw-backdrop-invert" (Invert (Pct 100.))

  let backdrop_invert n =
    set_backdrop_var "--tw-backdrop-invert" (Invert (Pct (float_of_int n)))

  let backdrop_invert_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-invert" (Invert np)

  let backdrop_sepia_default =
    set_backdrop_var "--tw-backdrop-sepia" (Sepia (Pct 100.))

  let backdrop_sepia n =
    set_backdrop_var "--tw-backdrop-sepia" (Sepia (Pct (float_of_int n)))

  let backdrop_sepia_arbitrary s =
    let inner = Parse.bracket_inner s in
    let np = parse_bracket_numpct inner in
    set_backdrop_var "--tw-backdrop-sepia" (Sepia np)

  let backdrop_hue_rotate n =
    set_backdrop_var "--tw-backdrop-hue-rotate" (Hue_rotate (make_angle n))

  let backdrop_hue_rotate_arbitrary angle =
    set_backdrop_var "--tw-backdrop-hue-rotate" (Hue_rotate angle)

  let neg_backdrop_hue_rotate_arbitrary angle =
    let neg : Css.angle = Calc (Expr (Val angle, Mul, Num (-1.))) in
    set_backdrop_var "--tw-backdrop-hue-rotate" (Hue_rotate neg)

  (* Composable filter using all the filter variables *)
  let filter_ =
    style ~property_rules:filter_property_rules
      [ filter composable_filter_chain ]

  let filter_none = style [ filter None ]

  (* Composable backdrop-filter using all the backdrop-filter variables *)
  let backdrop_filter_ =
    style ~property_rules:backdrop_filter_property_rules
      [
        Css.webkit_backdrop_filter composable_backdrop_filter_chain;
        backdrop_filter composable_backdrop_filter_chain;
      ]

  let backdrop_filter_none =
    style [ Css.webkit_backdrop_filter None; backdrop_filter None ]

  let backdrop_filter_arbitrary s =
    let inner = Parse.bracket_inner s in
    if Parse.is_var inner then
      let bare = Parse.extract_var_name inner in
      let ref_ : Css.filter Css.var = Var.bracket bare in
      style
        [ Css.webkit_backdrop_filter (Var ref_); backdrop_filter (Var ref_) ]
    else
      let cursor = Cascade.Cursor.of_string inner in
      match
        Cascade.Cursor.try_parse_full_err Css.Properties.read_filter cursor
      with
      | Ok value ->
          style [ Css.webkit_backdrop_filter value; backdrop_filter value ]
      | Error _ -> style []

  let to_style theme =
    let drop_shadow_color c shade = drop_shadow_color ~theme c shade in
    let drop_shadow_color_opacity c shade opacity =
      drop_shadow_color_opacity ~theme c shade opacity
    in
    let blur_none () = blur_none ~theme () in
    let drop_shadow_ () = drop_shadow_ ~theme () in
    let drop_shadow_named name = drop_shadow_named ~theme name in
    let drop_shadow_opacity op = drop_shadow_opacity ~theme op in
    let backdrop_blur_none () = backdrop_blur_none ~theme () in
    function
    | Filter -> filter_
    | Filter_none -> filter_none
    | Filter_arbitrary s -> filter_arbitrary s
    | Blur_none -> blur_none ()
    | Blur_xs -> blur_xs ()
    | Blur_sm -> blur_sm ()
    | Blur -> blur ()
    | Blur_md -> blur_md ()
    | Blur_lg -> blur_lg ()
    | Blur_xl -> blur_xl ()
    | Blur_2xl -> blur_2xl ()
    | Blur_3xl -> blur_3xl ()
    | Blur_arbitrary s -> blur_arbitrary s
    | Brightness n -> brightness n
    | Brightness_arbitrary s -> brightness_arbitrary s
    | Contrast n -> contrast n
    | Contrast_arbitrary s -> contrast_arbitrary s
    | Grayscale n -> grayscale n
    | Grayscale_arbitrary s -> grayscale_arbitrary s
    | Saturate n -> saturate n
    | Saturate_arbitrary s -> saturate_arbitrary s
    | Sepia n -> sepia n
    | Sepia_arbitrary s -> sepia_arbitrary s
    | Invert n -> invert n
    | Invert_arbitrary s -> invert_arbitrary s
    | Hue_rotate n -> hue_rotate n
    | Hue_rotate_arbitrary angle -> hue_rotate_arbitrary angle
    | Neg_hue_rotate_arbitrary angle -> neg_hue_rotate_arbitrary angle
    | Drop_shadow -> drop_shadow_ ()
    | Drop_shadow_xs -> drop_shadow_xs_ ()
    | Drop_shadow_sm -> drop_shadow_sm_ ()
    | Drop_shadow_md -> drop_shadow_md_ ()
    | Drop_shadow_lg -> drop_shadow_lg_ ()
    | Drop_shadow_xl -> drop_shadow_xl_ ()
    | Drop_shadow_2xl -> drop_shadow_2xl_ ()
    | Drop_shadow_multi -> drop_shadow_multi_
    | Drop_shadow_none -> drop_shadow_none
    | Drop_shadow_inherit -> drop_shadow_inherit_
    | Drop_shadow_named name -> drop_shadow_named name
    | Drop_shadow_arbitrary s -> drop_shadow_arbitrary_impl s
    | Drop_shadow_color (c, shade) -> drop_shadow_color c shade
    | Drop_shadow_color_opacity (c, shade, op) ->
        drop_shadow_color_opacity c shade op
    | Drop_shadow_opacity op -> drop_shadow_opacity op
    | Backdrop_blur_none -> backdrop_blur_none ()
    | Backdrop_blur_xs -> backdrop_blur_xs ()
    | Backdrop_blur_sm -> backdrop_blur_sm ()
    | Backdrop_blur -> backdrop_blur ()
    | Backdrop_blur_md -> backdrop_blur_md ()
    | Backdrop_blur_lg -> backdrop_blur_lg ()
    | Backdrop_blur_xl -> backdrop_blur_xl ()
    | Backdrop_blur_2xl -> backdrop_blur_2xl ()
    | Backdrop_blur_3xl -> backdrop_blur_3xl ()
    | Backdrop_blur_arbitrary s -> backdrop_blur_arbitrary s
    | Backdrop_brightness n -> backdrop_brightness n
    | Backdrop_brightness_arbitrary s -> backdrop_brightness_arbitrary s
    | Backdrop_contrast n -> backdrop_contrast n
    | Backdrop_contrast_arbitrary s -> backdrop_contrast_arbitrary s
    | Backdrop_opacity n -> backdrop_opacity n
    | Backdrop_opacity_arbitrary s -> backdrop_opacity_arbitrary s
    | Backdrop_saturate n -> backdrop_saturate n
    | Backdrop_saturate_arbitrary s -> backdrop_saturate_arbitrary s
    | Backdrop_grayscale n ->
        if n = 100 then backdrop_grayscale_default else backdrop_grayscale n
    | Backdrop_grayscale_arbitrary s -> backdrop_grayscale_arbitrary s
    | Backdrop_invert n ->
        if n = 100 then backdrop_invert_default else backdrop_invert n
    | Backdrop_invert_arbitrary s -> backdrop_invert_arbitrary s
    | Backdrop_sepia n ->
        if n = 100 then backdrop_sepia_default else backdrop_sepia n
    | Backdrop_sepia_arbitrary s -> backdrop_sepia_arbitrary s
    | Backdrop_hue_rotate n -> backdrop_hue_rotate n
    | Backdrop_hue_rotate_arbitrary angle -> backdrop_hue_rotate_arbitrary angle
    | Neg_backdrop_hue_rotate_arbitrary angle ->
        neg_backdrop_hue_rotate_arbitrary angle
    | Backdrop_filter -> backdrop_filter_
    | Backdrop_filter_none -> backdrop_filter_none
    | Backdrop_filter_arbitrary s -> backdrop_filter_arbitrary s

  let ( >|= ) = Parse.( >|= )
  let err_not_utility = Error (`Msg "Not a filter utility")

  let suborder = function
    (* Non-backdrop filters come first, then backdrop filters. Order matches
       Tailwind v4: alphabetical by class name within each filter type. *)
    | Blur -> 0
    | Blur_2xl -> 1
    | Blur_3xl -> 2
    | Blur_arbitrary _ -> 3
    | Blur_lg -> 4
    | Blur_md -> 5
    | Blur_none -> 6
    | Blur_sm -> 7
    | Blur_xl -> 8
    | Blur_xs -> 9
    | Brightness n -> 1000 + n
    | Brightness_arbitrary _ -> 1500
    | Contrast n -> 2000 + n
    | Contrast_arbitrary _ -> 2500
    | Drop_shadow_opacity _ -> 2690
    | Drop_shadow -> 2700
    | Drop_shadow_arbitrary _ -> 2701
    | Drop_shadow_multi -> 2702
    | Drop_shadow_2xl -> 2703
    | Drop_shadow_lg -> 2704
    | Drop_shadow_md -> 2705
    | Drop_shadow_sm -> 2706
    | Drop_shadow_xs -> 2706
    | Drop_shadow_xl -> 2707
    | Drop_shadow_none -> 2708
    | Drop_shadow_inherit -> 2709
    | Drop_shadow_named _ -> 2701
    | Drop_shadow_color _ -> 2710
    | Drop_shadow_color_opacity _ -> 2711
    | Filter -> 9000
    | Filter_arbitrary _ -> 9001
    | Filter_none -> 9002
    | Grayscale n -> 4000 + (100 - n)
    | Grayscale_arbitrary _ -> 4500
    | Hue_rotate n -> 5000 + n
    | Hue_rotate_arbitrary _ -> 5500
    | Neg_hue_rotate_arbitrary _ -> 4990
    | Invert n -> 6000 + (100 - n)
    | Invert_arbitrary _ -> 6500
    | Saturate n -> 7000 + n
    | Saturate_arbitrary _ -> 7500
    | Sepia n -> 8000 + (100 - n)
    | Sepia_arbitrary _ -> 8500
    (* Backdrop filters come after regular filters. Order: backdrop-blur,
       brightness, contrast, filter, grayscale, hue-rotate (neg then pos),
       invert, opacity, saturate, sepia. Within each: arbitrary first, then
       none, then named values. *)
    | Backdrop_blur_arbitrary _ -> 10000
    | Backdrop_blur_none -> 10001
    | Backdrop_blur_xs -> 10002
    | Backdrop_blur_sm -> 10003
    | Backdrop_blur -> 10004
    | Backdrop_blur_md -> 10005
    | Backdrop_blur_lg -> 10006
    | Backdrop_blur_xl -> 10007
    | Backdrop_blur_2xl -> 10008
    | Backdrop_blur_3xl -> 10009
    | Backdrop_brightness n -> 11000 + n
    | Backdrop_brightness_arbitrary _ -> 11500
    | Backdrop_contrast n -> 12000 + n
    | Backdrop_contrast_arbitrary _ -> 12500
    | Backdrop_filter -> 21000
    | Backdrop_filter_arbitrary _ -> 21001
    | Backdrop_filter_none -> 21002
    | Backdrop_grayscale n -> 14000 + (100 - n)
    | Backdrop_grayscale_arbitrary _ -> 14500
    | Neg_backdrop_hue_rotate_arbitrary _ -> 14990
    | Backdrop_hue_rotate n -> 15000 + n
    | Backdrop_hue_rotate_arbitrary _ -> 15500
    | Backdrop_invert n -> 16000 + (100 - n)
    | Backdrop_invert_arbitrary _ -> 16500
    | Backdrop_opacity n -> 17000 + Float.to_int (n *. 10.)
    | Backdrop_opacity_arbitrary _ -> 18100
    | Backdrop_saturate n -> 19000 + n
    | Backdrop_saturate_arbitrary _ -> 19500
    | Backdrop_sepia n -> 20000 + (100 - n)
    | Backdrop_sepia_arbitrary _ -> 20500

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "filter" ] -> Ok Filter
    | [ "filter"; "none" ] -> Ok Filter_none
    | [ "filter"; s ] when Parse.is_bracket_value s -> Ok (Filter_arbitrary s)
    | [ "backdrop"; "filter" ] -> Ok Backdrop_filter
    | [ "backdrop"; "filter"; "none" ] -> Ok Backdrop_filter_none
    | [ "backdrop"; "filter"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_filter_arbitrary s)
    | [ "blur"; "none" ] -> Ok Blur_none
    | [ "blur"; "xs" ] -> Ok Blur_xs
    | [ "blur"; "sm" ] -> Ok Blur_sm
    | [ "blur" ] -> Ok Blur
    | [ "blur"; "md" ] -> Ok Blur_md
    | [ "blur"; "lg" ] -> Ok Blur_lg
    | [ "blur"; "xl" ] -> Ok Blur_xl
    | [ "blur"; "2xl" ] -> Ok Blur_2xl
    | [ "blur"; "3xl" ] -> Ok Blur_3xl
    | [ "blur"; s ] when Parse.is_bracket_value s -> Ok (Blur_arbitrary s)
    | [ "brightness"; s ] when Parse.is_bracket_value s ->
        Ok (Brightness_arbitrary s)
    | [ "brightness"; n ] ->
        Parse.int_pos ~name:"brightness" n >|= fun x -> Brightness x
    | [ "contrast"; s ] when Parse.is_bracket_value s ->
        Ok (Contrast_arbitrary s)
    | [ "contrast"; n ] ->
        Parse.int_pos ~name:"contrast" n >|= fun x -> Contrast x
    | [ "grayscale"; s ] when Parse.is_bracket_value s ->
        Ok (Grayscale_arbitrary s)
    | [ "grayscale"; n ] ->
        Parse.int_pos ~name:"grayscale" n >|= fun x -> Grayscale x
    | [ "grayscale" ] -> Ok (Grayscale 100)
    | [ "saturate"; s ] when Parse.is_bracket_value s ->
        Ok (Saturate_arbitrary s)
    | [ "saturate"; n ] ->
        Parse.int_pos ~name:"saturate" n >|= fun x -> Saturate x
    | [ "sepia"; s ] when Parse.is_bracket_value s -> Ok (Sepia_arbitrary s)
    | [ "sepia"; n ] -> Parse.int_pos ~name:"sepia" n >|= fun x -> Sepia x
    | [ "sepia" ] -> Ok (Sepia 100)
    | [ "invert"; s ] when Parse.is_bracket_value s -> Ok (Invert_arbitrary s)
    | [ "invert"; n ] -> Parse.int_pos ~name:"invert" n >|= fun x -> Invert x
    | [ "invert" ] -> Ok (Invert 100)
    | [ "hue"; "rotate"; s ] when Parse.is_bracket_value s -> (
        match parse_bracket_angle s with
        | Option.Some angle -> Ok (Hue_rotate_arbitrary angle)
        | Option.None -> err_not_utility)
    | [ "hue"; "rotate"; n ] -> Parse.int_any n >|= fun x -> Hue_rotate x
    (* Negative hue-rotate: -hue-rotate-N or -hue-rotate-[Ndeg] *)
    | [ ""; "hue"; "rotate"; s ] when Parse.is_bracket_value s -> (
        match parse_bracket_angle s with
        | Option.Some angle -> Ok (Neg_hue_rotate_arbitrary angle)
        | Option.None -> err_not_utility)
    | [ ""; "hue"; "rotate"; n ] ->
        Parse.int_pos ~name:"hue-rotate" n >|= fun x -> Hue_rotate (-x)
    (* Drop shadow with opacity modifier on the base: drop-shadow/25 *)
    | [ "drop"; s ] when String.length s > 7 && String.sub s 0 7 = "shadow/"
      -> (
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
    | [ "drop"; "shadow"; s ] when Parse.is_bracket_value s ->
        Ok (Drop_shadow_arbitrary s)
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
               token; the theme value is resolved at emission time. *)
            | Error _ -> Ok (Drop_shadow_named full))
        | op -> (
            if base = "" then Ok (Drop_shadow_opacity op)
            else
              match
                Color.shade_and_opacity_of_strings
                  (String.split_on_char '-' base)
              with
              | Ok (c, shade, _) ->
                  Ok (Drop_shadow_color_opacity (c, shade, op))
              | Error _ -> err_not_utility))
    | [ "backdrop"; "blur"; "none" ] -> Ok Backdrop_blur_none
    | [ "backdrop"; "blur"; "xs" ] -> Ok Backdrop_blur_xs
    | [ "backdrop"; "blur"; "sm" ] -> Ok Backdrop_blur_sm
    | [ "backdrop"; "blur" ] -> Ok Backdrop_blur
    | [ "backdrop"; "blur"; "md" ] -> Ok Backdrop_blur_md
    | [ "backdrop"; "blur"; "lg" ] -> Ok Backdrop_blur_lg
    | [ "backdrop"; "blur"; "xl" ] -> Ok Backdrop_blur_xl
    | [ "backdrop"; "blur"; "2xl" ] -> Ok Backdrop_blur_2xl
    | [ "backdrop"; "blur"; "3xl" ] -> Ok Backdrop_blur_3xl
    | [ "backdrop"; "blur"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_blur_arbitrary s)
    | [ "backdrop"; "brightness"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_brightness_arbitrary s)
    | [ "backdrop"; "brightness"; n ] ->
        Parse.int_pos ~name:"backdrop-brightness" n >|= fun x ->
        Backdrop_brightness x
    | [ "backdrop"; "contrast"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_contrast_arbitrary s)
    | [ "backdrop"; "contrast"; n ] ->
        Parse.int_pos ~name:"backdrop-contrast" n >|= fun x ->
        Backdrop_contrast x
    | [ "backdrop"; "opacity"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_opacity_arbitrary s)
    | [ "backdrop"; "opacity"; n ] -> (
        match float_of_string_opt n with
        | Some f when f >= 0. -> Ok (Backdrop_opacity f)
        | _ -> err_not_utility)
    | [ "backdrop"; "saturate"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_saturate_arbitrary s)
    | [ "backdrop"; "saturate"; n ] ->
        Parse.int_pos ~name:"backdrop-saturate" n >|= fun x ->
        Backdrop_saturate x
    | [ "backdrop"; "grayscale"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_grayscale_arbitrary s)
    | [ "backdrop"; "grayscale"; n ] ->
        Parse.int_pos ~name:"backdrop-grayscale" n >|= fun x ->
        Backdrop_grayscale x
    | [ "backdrop"; "grayscale" ] -> Ok (Backdrop_grayscale 100)
    | [ "backdrop"; "invert"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_invert_arbitrary s)
    | [ "backdrop"; "invert"; n ] ->
        Parse.int_pos ~name:"backdrop-invert" n >|= fun x -> Backdrop_invert x
    | [ "backdrop"; "invert" ] -> Ok (Backdrop_invert 100)
    | [ "backdrop"; "sepia"; s ] when Parse.is_bracket_value s ->
        Ok (Backdrop_sepia_arbitrary s)
    | [ "backdrop"; "sepia"; n ] ->
        Parse.int_pos ~name:"backdrop-sepia" n >|= fun x -> Backdrop_sepia x
    | [ "backdrop"; "sepia" ] -> Ok (Backdrop_sepia 100)
    | [ "backdrop"; "hue"; "rotate"; s ] when Parse.is_bracket_value s -> (
        match parse_bracket_angle s with
        | Option.Some angle -> Ok (Backdrop_hue_rotate_arbitrary angle)
        | Option.None -> err_not_utility)
    | [ "backdrop"; "hue"; "rotate"; n ] ->
        Parse.int_any n >|= fun x -> Backdrop_hue_rotate x
    (* Negative backdrop hue-rotate *)
    | [ ""; "backdrop"; "hue"; "rotate"; s ] when Parse.is_bracket_value s -> (
        match parse_bracket_angle s with
        | Option.Some angle -> Ok (Neg_backdrop_hue_rotate_arbitrary angle)
        | Option.None -> err_not_utility)
    | [ ""; "backdrop"; "hue"; "rotate"; n ] ->
        Parse.int_pos ~name:"backdrop-hue-rotate" n >|= fun x ->
        Backdrop_hue_rotate (-x)
    | _ -> err_not_utility

  let pp_angle_bracket = function
    | Css.Deg n ->
        let s = string_of_float n in
        let s =
          if String.length s > 0 && s.[String.length s - 1] = '.' then
            String.sub s 0 (String.length s - 1)
          else s
        in
        s ^ "deg"
    | Rad n -> string_of_float n ^ "rad"
    | Turn n -> string_of_float n ^ "turn"
    | Grad n -> string_of_float n ^ "grad"
    | _ -> "0deg"

  let to_class = function
    | Filter -> "filter"
    | Filter_none -> "filter-none"
    | Filter_arbitrary s -> "filter-" ^ s
    | Backdrop_filter -> "backdrop-filter"
    | Backdrop_filter_none -> "backdrop-filter-none"
    | Backdrop_filter_arbitrary s -> "backdrop-filter-" ^ s
    | Blur_none -> "blur-none"
    | Blur_xs -> "blur-xs"
    | Blur_sm -> "blur-sm"
    | Blur -> "blur"
    | Blur_md -> "blur-md"
    | Blur_lg -> "blur-lg"
    | Blur_xl -> "blur-xl"
    | Blur_2xl -> "blur-2xl"
    | Blur_3xl -> "blur-3xl"
    | Blur_arbitrary s -> "blur-" ^ s
    | Brightness n -> "brightness-" ^ string_of_int n
    | Brightness_arbitrary s -> "brightness-" ^ s
    | Contrast n -> "contrast-" ^ string_of_int n
    | Contrast_arbitrary s -> "contrast-" ^ s
    | Grayscale 0 -> "grayscale-0"
    | Grayscale 100 -> "grayscale"
    | Grayscale n -> "grayscale-" ^ string_of_int n
    | Grayscale_arbitrary s -> "grayscale-" ^ s
    | Saturate n -> "saturate-" ^ string_of_int n
    | Saturate_arbitrary s -> "saturate-" ^ s
    | Sepia 0 -> "sepia-0"
    | Sepia 100 -> "sepia"
    | Sepia n -> "sepia-" ^ string_of_int n
    | Sepia_arbitrary s -> "sepia-" ^ s
    | Invert 0 -> "invert-0"
    | Invert 100 -> "invert"
    | Invert n -> "invert-" ^ string_of_int n
    | Invert_arbitrary s -> "invert-" ^ s
    | Hue_rotate n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "hue-rotate-" ^ string_of_int (abs n)
    | Hue_rotate_arbitrary angle ->
        "hue-rotate-[" ^ pp_angle_bracket angle ^ "]"
    | Neg_hue_rotate_arbitrary angle ->
        "-hue-rotate-[" ^ pp_angle_bracket angle ^ "]"
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
    | Drop_shadow_arbitrary s -> "drop-shadow-" ^ s
    | Drop_shadow_color (c, shade) ->
        "drop-shadow-" ^ Color.scheme_color_name c shade
    | Drop_shadow_color_opacity (c, shade, op) ->
        "drop-shadow-"
        ^ Color.scheme_color_name c shade
        ^ "/" ^ Color.pp_opacity op
    | Drop_shadow_opacity op -> "drop-shadow/" ^ Color.pp_opacity op
    | Backdrop_blur_none -> "backdrop-blur-none"
    | Backdrop_blur_xs -> "backdrop-blur-xs"
    | Backdrop_blur_sm -> "backdrop-blur-sm"
    | Backdrop_blur -> "backdrop-blur"
    | Backdrop_blur_md -> "backdrop-blur-md"
    | Backdrop_blur_lg -> "backdrop-blur-lg"
    | Backdrop_blur_xl -> "backdrop-blur-xl"
    | Backdrop_blur_2xl -> "backdrop-blur-2xl"
    | Backdrop_blur_3xl -> "backdrop-blur-3xl"
    | Backdrop_blur_arbitrary s -> "backdrop-blur-" ^ s
    | Backdrop_brightness n -> "backdrop-brightness-" ^ string_of_int n
    | Backdrop_brightness_arbitrary s -> "backdrop-brightness-" ^ s
    | Backdrop_contrast n -> "backdrop-contrast-" ^ string_of_int n
    | Backdrop_contrast_arbitrary s -> "backdrop-contrast-" ^ s
    | Backdrop_opacity n ->
        "backdrop-opacity-"
        ^
        if Float.is_integer n then string_of_int (Float.to_int n)
        else Css.Pp.string_of_float n
    | Backdrop_opacity_arbitrary s -> "backdrop-opacity-" ^ s
    | Backdrop_saturate n -> "backdrop-saturate-" ^ string_of_int n
    | Backdrop_saturate_arbitrary s -> "backdrop-saturate-" ^ s
    | Backdrop_grayscale 0 -> "backdrop-grayscale-0"
    | Backdrop_grayscale 100 -> "backdrop-grayscale"
    | Backdrop_grayscale n -> "backdrop-grayscale-" ^ string_of_int n
    | Backdrop_grayscale_arbitrary s -> "backdrop-grayscale-" ^ s
    | Backdrop_invert 0 -> "backdrop-invert-0"
    | Backdrop_invert 100 -> "backdrop-invert"
    | Backdrop_invert n -> "backdrop-invert-" ^ string_of_int n
    | Backdrop_invert_arbitrary s -> "backdrop-invert-" ^ s
    | Backdrop_sepia 0 -> "backdrop-sepia-0"
    | Backdrop_sepia 100 -> "backdrop-sepia"
    | Backdrop_sepia n -> "backdrop-sepia-" ^ string_of_int n
    | Backdrop_sepia_arbitrary s -> "backdrop-sepia-" ^ s
    | Backdrop_hue_rotate n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "backdrop-hue-rotate-" ^ string_of_int (abs n)
    | Backdrop_hue_rotate_arbitrary angle ->
        "backdrop-hue-rotate-[" ^ pp_angle_bracket angle ^ "]"
    | Neg_backdrop_hue_rotate_arbitrary angle ->
        "-backdrop-hue-rotate-[" ^ pp_angle_bracket angle ^ "]"
end

open Handler

let () = Utility.register (module Handler)

(** {1 Public API - Utility Values} *)

let utility x = Utility.base (Self x)
let blur_none = utility Blur_none
let blur_xs = utility Blur_xs
let blur_sm = utility Blur_sm
let blur = utility Blur
let blur_md = utility Blur_md
let blur_lg = utility Blur_lg
let blur_xl = utility Blur_xl
let blur_2xl = utility Blur_2xl
let blur_3xl = utility Blur_3xl
let brightness n = utility (Brightness n)
let contrast n = utility (Contrast n)
let grayscale n = utility (Grayscale n)
let saturate n = utility (Saturate n)
let sepia n = utility (Sepia n)
let invert n = utility (Invert n)
let hue_rotate n = utility (Hue_rotate n)
let backdrop_blur_none = utility Backdrop_blur_none
let backdrop_blur_xs = utility Backdrop_blur_xs
let backdrop_blur_sm = utility Backdrop_blur_sm
let backdrop_blur = utility Backdrop_blur
let backdrop_blur_md = utility Backdrop_blur_md
let backdrop_blur_lg = utility Backdrop_blur_lg
let backdrop_blur_xl = utility Backdrop_blur_xl
let backdrop_blur_2xl = utility Backdrop_blur_2xl
let backdrop_blur_3xl = utility Backdrop_blur_3xl
let backdrop_brightness n = utility (Backdrop_brightness n)
let backdrop_contrast n = utility (Backdrop_contrast n)
let backdrop_opacity n = utility (Backdrop_opacity n)
let backdrop_saturate n = utility (Backdrop_saturate n)
let backdrop_grayscale ?(n = 100) () = utility (Backdrop_grayscale n)
let backdrop_invert ?(n = 100) () = utility (Backdrop_invert n)
let backdrop_sepia ?(n = 100) () = utility (Backdrop_sepia n)
let backdrop_hue_rotate n = utility (Backdrop_hue_rotate n)
