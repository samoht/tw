(** Transform utilities for 2D and 3D transformations. *)

module Css = Cascade.Css

(** Error helpers. *)
let err_not_utility = Error (`Msg "Not a transform utility")

module Handler = struct
  open Style

  (* Capture the project Pp before [open Css] shadows it with Css.Pp. *)
  let pp_float = Pp.float

  open Css

  (* The four operations Tailwind spells [<op>-<value>] and
     [<op>-<axis>-<value>]. One utility is an operation, an axis, a sign and
     what follows; what differs between operations is stated once, in the
     readers and style builders below, rather than once per axis. *)
  type op = Translate | Scale | Rotate | Skew

  (* [All] is the bare form: both translate axes, every scale axis, both skew
     channels, and the [rotate] property itself. *)
  type axis = All | X | Y | Z
  type sign = Pos | Neg

  type amount =
    | Int of int (* the magnitude; the sign is the utility's *)
    | Step of float (* a fractional spacing step, translate only *)
    | Full
    | Pixel
    | Fraction of int * int
    | Bare_var of string (* [(--name)], rotate only, held as [--name] *)
    | (* The text as written, which the class name is spelled from and
         [to_style] reads with the operation's own grammar. *)
      Bracket of string

  (* Alphabetical by class suffix, the order Tailwind sorts each band in. *)
  type position =
    | Bottom
    | Bottom_left
    | Bottom_right
    | Center
    | Left
    | Right
    | Top
    | Top_left
    | Top_right

  type depth = Dramatic | Near | Normal | Midrange | Distant

  type t =
    | Op of op * axis * sign * amount
    | Rotate_none
    | Translate_none
    | Translate_3d
    | Scale_none
    | Scale_3d
    | Perspective_none
    | (* depth named by a project [--perspective-*] token *)
      Perspective_theme of string
    | Perspective of depth
    | Perspective_arbitrary of string * Css.length
    | Perspective_raw of string * string
    | Perspective_origin of position
    (* The author's bracket text travels with the value it denotes, so the class
       name is spelled exactly as it was written. *)
    | Perspective_origin_arbitrary of string * Css.perspective_origin
    | Perspective_origin_raw of string * string
    | (* Transform style *)
      Transform_style_3d
    | Transform_style_flat
    | (* Transform box *)
      Transform_box_border
    | Transform_box_content
    | Transform_box_fill
    | Transform_box_stroke
    | Transform_box_view
    | (* Backface visibility *)
      Backface_visible
    | Backface_hidden
    | (* Transform control *)
      Transform
    | Transform_cpu
    | Transform_none
    | Transform_gpu
    | Transform_arbitrary of string * Css.transform list
    | Transform_raw of string * string
      (* A bracket no reader took, on any of these families: the class names one
         longhand or one channel, so the value goes there verbatim - Tailwind's
         token-stream contract. *)
    | Origin of position
    | Origin_arbitrary of string * Css.transform_origin
    | Origin_raw of string * string

  let name = "transforms"

  (* The transform chain sits before animations. Controls that Tailwind sorts
     through a later property band must not travel with it: origin precedes the
     chain, backface follows selection, perspective follows backface, and
     transform box/style follow text-shadow. *)
  let priority = function
    | Origin _ | Origin_arbitrary _ | Origin_raw _ -> 8
    | Backface_visible | Backface_hidden -> 38
    | Perspective_none | Perspective_theme _ | Perspective _
    | Perspective_arbitrary _ | Perspective_raw _ | Perspective_origin _
    | Perspective_origin_arbitrary _ | Perspective_origin_raw _ ->
        40
    | Transform_style_3d | Transform_style_flat | Transform_box_border
    | Transform_box_content | Transform_box_fill | Transform_box_stroke
    | Transform_box_view ->
        42
    | _ -> 9

  (* Tailwind v4 uses rotate-x/y/z and skew-x/y variables for the transform
     utility. These variables contain the full transform function values, e.g.:
     --tw-rotate-x: rotateX(45deg) --tw-skew-x: skewX(10deg) *)
  (* Property ordering to match Tailwind v4:
     translate (0-2), scale (3-5), then other properties follow *)

  (* Translate variables - use property_default pattern with initial 0 *)
  (* Translate properties don't appear in @layer properties in standard Tailwind - only used locally *)
  let tw_translate_x_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:0 ~family:`Translate "tw-translate-x"

  let tw_translate_y_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:1 ~family:`Translate "tw-translate-y"

  let tw_translate_z_var =
    Var.property_default Css.Length ~initial:Css.Zero ~universal:true
      ~property_order:2 ~family:`Translate "tw-translate-z"

  (* Scale variables - property_order (6-8) comes after duration (5) *)
  let tw_scale_x_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:6 ~family:`Scale "tw-scale-x"

  let tw_scale_y_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:7 ~family:`Scale "tw-scale-y"

  let tw_scale_z_var =
    Var.property_default Css.Number_percentage ~initial:(Num 1.0)
      ~universal:true ~property_order:8 ~family:`Scale "tw-scale-z"

  (* Rotate and skew properties appear FIRST in @layer properties (0-4) *)
  let tw_rotate_x_var =
    Var.channel ~needs_property:true ~property_order:0 ~family:`Rotate
      Css.Transform "tw-rotate-x"

  let tw_rotate_y_var =
    Var.channel ~needs_property:true ~property_order:1 ~family:`Rotate
      Css.Transform "tw-rotate-y"

  let tw_rotate_z_var =
    Var.channel ~needs_property:true ~property_order:2 ~family:`Rotate
      Css.Transform "tw-rotate-z"

  let tw_skew_x_var =
    Var.channel ~needs_property:true ~property_order:3 ~family:`Skew
      Css.Transform "tw-skew-x"

  let tw_skew_y_var =
    Var.channel ~needs_property:true ~property_order:4 ~family:`Skew
      Css.Transform "tw-skew-y"

  (* Perspective theme variables, ordered by value to match Tailwind's theme.
     The family owns slots (8, 10-16), after blur (8, 0-8). *)
  let perspective_dramatic_var =
    Var.theme Css.Length "perspective-dramatic" ~order:(8, 10)

  let perspective_near_var =
    Var.theme Css.Length "perspective-near" ~order:(8, 11)

  let perspective_normal_var =
    Var.theme Css.Length "perspective-normal" ~order:(8, 12)

  let perspective_midrange_var =
    Var.theme Css.Length "perspective-midrange" ~order:(8, 13)

  let perspective_distant_var =
    Var.theme Css.Length "perspective-distant" ~order:(8, 14)

  (* A [--perspective-*] token the project declared names a depth the built-in
     scale has no slot for; they share the slot after the scale. *)
  let perspective_named_cache = Domain_cache.v 8

  let perspective_named_var name =
    Domain_cache.or_add perspective_named_cache name (fun () ->
        Var.theme Css.Length ("perspective-" ^ name) ~order:(8, 16))

  let perspective_none_var =
    Var.theme Css.Length "perspective-none" ~order:(8, 15)

  let depths = [ Dramatic; Near; Normal; Midrange; Distant ]

  let depth_name = function
    | Dramatic -> "dramatic"
    | Near -> "near"
    | Normal -> "normal"
    | Midrange -> "midrange"
    | Distant -> "distant"

  (* Each depth's token and the length Tailwind's default theme gives it. *)
  let depth_token = function
    | Dramatic -> (perspective_dramatic_var, 100.)
    | Near -> (perspective_near_var, 300.)
    | Normal -> (perspective_normal_var, 500.)
    | Midrange -> (perspective_midrange_var, 800.)
    | Distant -> (perspective_distant_var, 1200.)

  (* Publish the scale through the theme-token registry, the way rule.ml
     publishes the breakpoints, so [perspective-[theme(--perspective-normal)]]
     resolves and [theme(static)] emits it. [--perspective-none] is not in
     Tailwind's default theme - the utility writes [perspective: none] - so it
     stays out. *)
  let () =
    List.iter
      (fun depth ->
        let var, px = depth_token depth in
        Theme.register_default var (Css.Px px : Css.length))
      depths

  (** {1 Helpers} *)

  let collect_property_rules vars =
    List.filter_map Var.property_rule vars |> concat

  (* Property rules for rotate/skew transform chain *)
  let rotate_skew_props =
    collect_property_rules
      [
        tw_rotate_x_var;
        tw_rotate_y_var;
        tw_rotate_z_var;
        tw_skew_x_var;
        tw_skew_y_var;
      ]

  let translate_props =
    collect_property_rules
      [ tw_translate_x_var; tw_translate_y_var; tw_translate_z_var ]

  let scale_props =
    collect_property_rules [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]

  (* The five rotate and skew channels with an empty fallback each, which is
     what every [transform] this family writes reads. *)
  let chain_refs : Css.transform list =
    [
      Var (Var.reference_with_empty_fallback tw_rotate_x_var);
      Var (Var.reference_with_empty_fallback tw_rotate_y_var);
      Var (Var.reference_with_empty_fallback tw_rotate_z_var);
      Var (Var.reference_with_empty_fallback tw_skew_x_var);
      Var (Var.reference_with_empty_fallback tw_skew_y_var);
    ]

  let rotate_skew_chain = transforms chain_refs

  let translate_xy_refs =
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    Css.translate (XY (Var tx_ref, Var ty_ref))

  let translate_xyz_refs =
    let tx_ref = Var.reference tw_translate_x_var in
    let ty_ref = Var.reference tw_translate_y_var in
    let tz_ref = Var.reference tw_translate_z_var in
    Css.translate (XYZ (Var tx_ref, Var ty_ref, Var tz_ref))

  let scale_xy_refs =
    let x = Var.reference tw_scale_x_var in
    let y = Var.reference tw_scale_y_var in
    Css.scale (XY (Var x, Var y))

  let scale_xyz_refs =
    let x = Var.reference tw_scale_x_var in
    let y = Var.reference tw_scale_y_var in
    let z = Var.reference tw_scale_z_var in
    Css.scale (XYZ (Var x, Var y, Var z))

  let translate_vars = function
    | All -> [ tw_translate_x_var; tw_translate_y_var ]
    | X -> [ tw_translate_x_var ]
    | Y -> [ tw_translate_y_var ]
    | Z -> [ tw_translate_z_var ]

  let scale_vars = function
    | All -> [ tw_scale_x_var; tw_scale_y_var; tw_scale_z_var ]
    | X -> [ tw_scale_x_var ]
    | Y -> [ tw_scale_y_var ]
    | Z -> [ tw_scale_z_var ]

  (* A rotate or skew channel: its variable, the function it holds and the
     transform that function is. *)
  let channels op axis =
    let rotate_x =
      (tw_rotate_x_var, "rotateX", fun a : Css.transform -> Rotate_x a)
    and rotate_y =
      (tw_rotate_y_var, "rotateY", fun a : Css.transform -> Rotate_y a)
    and rotate_z =
      (tw_rotate_z_var, "rotateZ", fun a : Css.transform -> Rotate_z a)
    and skew_x = (tw_skew_x_var, "skewX", fun a : Css.transform -> Skew_x a)
    and skew_y = (tw_skew_y_var, "skewY", fun a : Css.transform -> Skew_y a) in
    match (op, axis) with
    | Rotate, X -> [ rotate_x ]
    | Rotate, Y -> [ rotate_y ]
    | Rotate, Z -> [ rotate_z ]
    | Skew, X -> [ skew_x ]
    | Skew, Y -> [ skew_y ]
    | Skew, All -> [ skew_x; skew_y ]
    | (Translate | Scale), _ | Rotate, All | Skew, Z -> []

  let signed sign n = match sign with Pos -> n | Neg -> -n
  let signed_float sign f = match sign with Pos -> f | Neg -> -.f

  (* Helper to create an angle with calc for negative values *)
  let make_angle deg =
    if deg >= 0 then Deg (float_of_int deg)
    else Calc (Expr (Val (Deg (float_of_int (abs deg))), Mul, Num (-1.)))

  (* Helper to create a percentage with calc for negative values *)
  let make_pct n : Css.number_percentage =
    if n >= 0 then Pct (float_of_int n)
    else Calc (Expr (Val (Pct (float_of_int (abs n))), Mul, Num (-1.)))

  (* [(--name)] as an angle, negated as [calc(var(--name) * -1)]. *)
  let bare_var_angle sign name : Css.angle =
    let bare = String.sub name 2 (String.length name - 2) in
    match sign with
    | Pos -> Var (Var.bracket bare)
    | Neg -> Calc (Expr (Var (Var.bracket bare), Mul, Num (-1.)))

  (* The length a bracket spells as a signed decimal under one of the units a
     translate or a perspective takes. *)
  let length_of_inner inner : Css.length option =
    let slen = String.length inner in
    (* Allow a leading minus so negative arbitrary values (translate-x-[-0.5px],
       translate-y-[-110%]) parse. *)
    let i = ref (if slen > 0 && inner.[0] = '-' then 1 else 0) in
    while
      !i < slen && ((inner.[!i] >= '0' && inner.[!i] <= '9') || inner.[!i] = '.')
    do
      incr i
    done;
    if !i = 0 || !i = slen then None
    else
      let unit_s = String.sub inner !i (slen - !i) in
      match Float.of_string_opt (String.sub inner 0 !i) with
      | Option.None -> None
      | Option.Some n -> (
          match unit_s with
          | "px" -> Some (Px n : Css.length)
          | "rem" -> Some (Rem n)
          | "em" -> Some (Em n)
          | "vw" -> Some (Vw n)
          | "vh" -> Some (Vh n)
          | "cm" -> Some (Cm n)
          | "mm" -> Some (Mm n)
          | "pt" -> Some (Pt n)
          | "%" -> Some (Pct n)
          | _ -> None)

  (* The angle a bracket spells: a signed decimal under one of the four angle
     units. *)
  let angle_of_inner inner : Css.angle option =
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
    if !i = 0 || !i = slen then None
    else
      let num_s = String.sub inner 0 !i in
      let unit_s = String.sub inner !i (slen - !i) in
      match (Float.of_string_opt num_s, unit_s) with
      | Some n, "deg" -> Some (Deg n)
      | Some n, "rad" -> Some (Rad n)
      | Some n, "turn" -> Some (Turn n)
      | Some n, "grad" -> Some (Grad n)
      | _ -> None

  (* The number a bracket spells, when it is a plain decimal. A spelling only
     OCaml's number reader takes ([0x4]) is not a number under another name. *)
  let number_of_inner v = Parse.decimal_float (Parse.decode_arbitrary_value v)

  (* [scale-[1.5]] and [scale-[1_2_3]]: one factor or one per axis. *)
  let scale_factors inner =
    match List.filter (( <> ) "") (String.split_on_char '_' inner) with
    | [ x; y; z ] -> (
        match
          (Float.of_string_opt x, Float.of_string_opt y, Float.of_string_opt z)
        with
        | Some x, Some y, Some z -> Some (`Nums (x, y, z))
        | _ -> None)
    | [ _ ] -> Option.map (fun f -> `Num f) (Float.of_string_opt inner)
    | _ -> None

  (* The 3D form is four components, [x y z <angle>]. [_] separates them and
     [\_] is a literal underscore inside one, so the value is decoded before it
     is split. What the axes take is a CSS number, which OCaml's own float
     reader is wider than: it reads [1_2] as 12, where the value is meant to
     reach the sheet as written. *)
  let rotate_axis_angle inner =
    match String.split_on_char ' ' (Parse.decode_underscores inner) with
    | [ x; y; z; a ] -> (
        match
          ( Parse.decimal_float x,
            Parse.decimal_float y,
            Parse.decimal_float z,
            angle_of_inner a )
        with
        | Some x, Some y, Some z, Some a -> Some (`Axis (x, y, z, a))
        | _ -> None)
    | _ -> None

  (* What a positive bracket on [op] along [axis] denotes, read with the grammar
     the operation's value has. A bracket no reader takes goes to the channel or
     the property as written; [None] is one that is no declaration value. The
     scale-z channel takes the bracket unvalidated, since its [@property] uses
     [syntax: "*"]. *)
  let read op axis inner =
    let typed =
      match (op, axis) with
      | Translate, (All | X | Y) ->
          Option.map (fun l -> `Length l) (length_of_inner inner)
      | Translate, Z ->
          Option.map (fun l -> `Length l) (Parse.arbitrary_length inner)
      | Scale, All -> scale_factors inner
      | Scale, (X | Y) -> Option.map (fun f -> `Num f) (number_of_inner inner)
      | Scale, Z -> None
      | Rotate, All -> (
          match rotate_axis_angle inner with
          | Some _ as r -> r
          | None -> Option.map (fun a -> `Angle a) (angle_of_inner inner))
      | (Rotate | Skew), _ ->
          Option.map (fun a -> `Angle a) (angle_of_inner inner)
    in
    match (typed, op, axis) with
    | Some _, _, _ -> typed
    | None, Scale, Z ->
        let value = Parse.decode_arbitrary_value inner in
        if Parse.is_declaration_value value then Some (`Raw value) else None
    | None, _, _ ->
        Option.map (fun v -> `Raw v) (Parse.arbitrary_declaration_value inner)

  (* A bracket a negative form takes: one holding a value, which every spelling
     of one becomes [calc(<value> * -1)]. *)
  let neg_bracket value =
    Parse.is_bracket_value value
    && Option.is_some
         (Parse.arbitrary_declaration_value (Parse.bracket_inner value))

  (* The bracket [s] negated the way Tailwind writes a negative candidate: the
     value [read] takes, with any data-type hint taken off, comes back [`Typed]
     for the family to negate as its type, and every other value comes back
     [`Raw], the token stream [calc(<value> * -1)] between [before] and [after],
     which is the function call a channel holds. [None] is a bracket that names
     no value, which the reader refused. *)
  let neg_bracket_value ?(before = "") ?(after = "") read s =
    match Option.bind (Parse.value_after_hint s) read with
    | Some v -> Some (`Typed v)
    | None ->
        Option.bind (Parse.arbitrary_declaration_value s) (fun v ->
            Option.map
              (fun raw -> `Raw raw)
              (Parse.wrap_declaration_value ~before:(before ^ "calc(")
                 ~after:(" * -1)" ^ after) v))

  (* [var] set to the bracket [s] negated: a value [read] takes goes through
     [neg], the family's own negation, and a token stream is written as it
     is. *)
  let neg_channel_decl ?before ?after read neg var s =
    match neg_bracket_value ?before ?after read s with
    | Some (`Typed v) -> Some (Var.set var (neg v))
    | Some (`Raw v) ->
        Some (Css.custom_property ~layer:"utilities" (Var.css_name var) v)
    | None -> None

  let neg_length (l : Css.length) : Css.length =
    Calc (Calc.mul (Calc.length l) (Calc.float (-1.)))

  let neg_number f : Css.number_percentage =
    Calc (Calc.mul (Calc.float f) (Calc.float (-1.)))

  let neg_angle (a : Css.angle) : Css.angle =
    Calc (Calc.mul (Val a) (Calc.float (-1.)))

  (* The channels a negative bracket sets, beside [rest]; a bracket one channel
     refuses writes nothing. *)
  let neg_style ~property_rules metadata decls rest =
    if List.for_all Option.is_some decls then
      style ~property_rules ~metadata (List.filter_map Fun.id decls @ rest)
    else style []

  (* [var] set to the token stream [value] as written. *)
  let raw_binding value var =
    Css.custom_property ~layer:"utilities" (Var.css_name var) value

  let opaque_style property value =
    match Parse.opaque_declaration property value with
    | Some declaration -> style [ declaration ]
    | None -> style []

  (** {1 Operation styles} *)

  (* The spacing step comes from [Theme], as every spacing family's does, so a
     project's [--spacing-N] token and an inline [--spacing] reach a translate
     the way they reach a padding. [calc(var(--spacing) * 0)] is zero whatever
     the spacing is, and Tailwind writes that zero with a unit: the target is a
     [--tw-*] custom property, an opaque token stream where [0] and [0px] are
     not the same token, which is the [0px] [Theme] writes. One axis takes the
     unit shortcuts [Theme.spacing_calc] has; the bare form and the z axis write
     the product. *)
  let translate_style theme axis sign amount =
    let vars = translate_vars axis in
    let refs = if axis = Z then translate_xyz_refs else translate_xy_refs in
    let set ?(spacing = []) value =
      style ~property_rules:translate_props
        (spacing @ List.map (fun var -> Var.set var value) vars @ [ refs ])
    in
    match (sign, amount) with
    | _, Int n ->
        let n = signed sign n in
        let decl, value =
          match axis with
          | X | Y -> Theme.spacing_calc ~theme n
          | All | Z -> Theme.spacing_product ~theme (float_of_int n)
        in
        set ~spacing:[ decl ] value
    | _, Step f ->
        let f = signed_float sign f in
        let decl, value =
          match axis with
          | X | Y -> Theme.spacing_calc_float ~theme f
          | All | Z -> Theme.spacing_product ~theme f
        in
        set ~spacing:[ decl ] value
    | _, Full -> set (Pct (signed_float sign 100.))
    | _, Pixel -> set (Px (signed_float sign 1.))
    | Pos, Fraction (n, d) -> set (Parse.fraction_length n d)
    | Neg, Fraction (n, d) -> set (Parse.neg_fraction_length n d)
    | Neg, Bracket s ->
        neg_style ~property_rules:translate_props
          (List.map Var.metadata vars)
          (List.map
             (fun var ->
               neg_channel_decl Parse.arbitrary_length neg_length var s)
             vars)
          [ refs ]
    | Pos, Bracket s -> (
        match read Translate axis s with
        | Some (`Length l) -> set l
        | Some (`Raw v) ->
            style ~property_rules:translate_props
              ~metadata:(List.map Var.metadata vars)
              (List.map (raw_binding v) vars @ [ refs ])
        | _ -> style [])
    | _, Bare_var _ -> style []

  (* Every axis form writes its channel and the [scale] composition reading it;
     the bare form's bracket writes [scale] itself. *)
  let scale_style axis sign amount =
    let vars = scale_vars axis in
    let refs = if axis = Z then scale_xyz_refs else scale_xy_refs in
    let set value =
      style ~property_rules:scale_props
        (List.map (fun var -> Var.set var value) vars @ [ refs ])
    in
    match (axis, sign, amount) with
    | _, _, Int n -> set (make_pct (signed sign n))
    | All, Neg, Bracket s -> (
        match neg_bracket_value number_of_inner s with
        | Some (`Typed f) -> style [ Css.scale (Css.X (neg_number f)) ]
        | Some (`Raw v) -> opaque_style "scale" v
        | None -> style [])
    | _, Neg, Bracket s ->
        neg_style ~property_rules:scale_props
          (List.map Var.metadata vars)
          (List.map
             (fun var -> neg_channel_decl number_of_inner neg_number var s)
             vars)
          [ refs ]
    | All, Pos, Bracket s -> (
        match read Scale All s with
        | Some (`Num f) -> style [ Css.scale (Css.X (Num f)) ]
        | Some (`Nums (x, y, z)) ->
            style [ Css.scale (XYZ (Num x, Num y, Num z)) ]
        | Some (`Raw v) -> opaque_style "scale" v
        | _ -> style [])
    | _, Pos, Bracket s -> (
        match read Scale axis s with
        | Some (`Num f) -> set (Num f)
        (* No metadata of its own: Tailwind lists the scale channels x, y, z in
           the properties layer whichever one a bracket sets, the order the
           references give. *)
        | Some (`Raw v) ->
            style ~property_rules:scale_props
              (List.map (raw_binding v) vars @ [ refs ])
        | _ -> style [])
    | _, _, (Step _ | Full | Pixel | Fraction _ | Bare_var _) -> style []

  (* The bare rotate writes the [rotate] property, not a channel. *)
  let rotate_style sign amount =
    match (sign, amount) with
    | _, Int n ->
        style [ Css.rotate (Angle (Deg (float_of_int (signed sign n)))) ]
    | _, Bare_var name ->
        style [ Css.rotate (Angle (bare_var_angle sign name)) ]
    | Neg, Bracket s -> (
        match neg_bracket_value angle_of_inner s with
        | Some (`Typed a) -> style [ Css.rotate (Angle (neg_angle a)) ]
        | Some (`Raw v) -> opaque_style "rotate" v
        | None -> style [])
    | Pos, Bracket s -> (
        match read Rotate All s with
        | Some (`Angle a) -> style [ Css.rotate (Angle a) ]
        | Some (`Axis (x, y, z, a)) -> style [ Css.rotate (Axis (x, y, z, a)) ]
        | Some (`Raw v) -> opaque_style "rotate" v
        | _ -> style [])
    | _, (Step _ | Full | Pixel | Fraction _) -> style []

  (* A rotate or skew channel utility: each channel it names takes its function
     around the angle, beside the chain reading all five. A bracket no angle
     reader took still reaches the channel, wrapped in the function. *)
  let chain_style op axis sign amount =
    let channels = channels op axis in
    let set angle =
      style ~property_rules:rotate_skew_props
        (List.map
           (fun (var, _, transform) -> Var.set var (transform angle))
           channels
        @ [ rotate_skew_chain ])
    in
    match (sign, amount) with
    | _, Int n -> set (make_angle (signed sign n))
    | _, Bare_var name -> set (bare_var_angle sign name)
    | Neg, Bracket s ->
        let decl (var, fn, transform) =
          neg_channel_decl ~before:(fn ^ "(") ~after:")" angle_of_inner
            (fun a -> transform (neg_angle a))
            var s
        in
        neg_style ~property_rules:rotate_skew_props
          (List.map (fun (var, _, _) -> Var.metadata var) channels)
          (List.map decl channels) [ rotate_skew_chain ]
    | Pos, Bracket s -> (
        match read op axis s with
        | Some (`Angle a) -> set a
        | Some (`Raw value) ->
            let binding (var, fn, _) =
              Parse.wrap_declaration_value ~before:(fn ^ "(") ~after:")" value
              |> Option.map
                   (Css.custom_property ~layer:"utilities" (Var.css_name var))
            in
            style ~property_rules:rotate_skew_props
              ~metadata:
                (List.map (fun (var, _, _) -> Var.metadata var) channels)
              (List.filter_map binding channels @ [ rotate_skew_chain ])
        | _ -> style [])
    | _, (Step _ | Full | Pixel | Fraction _) -> style []

  let op_style theme op axis sign amount =
    match (op, axis) with
    | Translate, _ -> translate_style theme axis sign amount
    | Scale, _ -> scale_style axis sign amount
    | Rotate, All -> rotate_style sign amount
    | (Rotate | Skew), _ -> chain_style op axis sign amount

  (** {1 Perspective and origin} *)

  (* A project that declares [--perspective-none] chooses the length the utility
     resolves to, so read its value with the CSS length grammar rather than a
     px-only test whose failure arm was a fabricated zero. A value the grammar
     cannot read falls back to the utility's own meaning and still reaches the
     sheet unaltered, since the theme layer restores the project's own text over
     the bound value. *)
  let perspective_none ?theme () =
    match Scheme.theme_value theme "perspective-none" with
    | Some value ->
        let len =
          Option.value
            (Css.parse_length (String.trim value))
            ~default:(None : Css.length)
        in
        let decl, r = Var.binding perspective_none_var len in
        style (decl :: [ Css.perspective (Var r) ])
    | None -> style [ Css.perspective None ]

  let is_theme_perspective theme n =
    Scheme.theme_value (Some theme) ("perspective-" ^ n) <> None

  let perspective_theme theme name =
    match Scheme.theme_value (Some theme) ("perspective-" ^ name) with
    | None -> style []
    | Some raw -> (
        match Css.parse_length raw with
        | None -> style []
        | Some len ->
            let decl, r = Var.binding (perspective_named_var name) len in
            style (decl :: [ Css.perspective (Var r) ]))

  let perspective_depth depth =
    let var, px = depth_token depth in
    let decl, r = Var.binding var (Px px) in
    style (decl :: [ Css.perspective (Var r) ])

  let positions =
    [
      Bottom;
      Bottom_left;
      Bottom_right;
      Center;
      Left;
      Right;
      Top;
      Top_left;
      Top_right;
    ]

  let position_name = function
    | Bottom -> "bottom"
    | Bottom_left -> "bottom-left"
    | Bottom_right -> "bottom-right"
    | Center -> "center"
    | Left -> "left"
    | Right -> "right"
    | Top -> "top"
    | Top_left -> "top-left"
    | Top_right -> "top-right"

  let position_rank = function
    | Bottom -> 0
    | Bottom_left -> 1
    | Bottom_right -> 2
    | Center -> 3
    | Left -> 4
    | Right -> 5
    | Top -> 6
    | Top_left -> 7
    | Top_right -> 8

  let position_of_parts parts =
    List.find_opt
      (fun p -> String.split_on_char '-' (position_name p) = parts)
      positions

  (* The keyword a position writes, and the same position as each property's
     value. *)
  let position_css = function
    | Bottom -> "bottom"
    | Bottom_left -> "0 100%"
    | Bottom_right -> "100% 100%"
    | Center -> "center"
    | Left -> "0"
    | Right -> "100%"
    | Top -> "top"
    | Top_left -> "0 0"
    | Top_right -> "100% 0"

  let transform_origin_of : position -> Css.transform_origin = function
    | Bottom -> Css.Bottom
    | Bottom_left -> XY (Zero, Pct 100.)
    | Bottom_right -> XY (Pct 100., Pct 100.)
    | Center -> Css.Center
    | Left -> Css.X Zero
    | Right -> Css.X (Pct 100.)
    | Top -> Css.Top
    | Top_left -> XY (Zero, Zero)
    | Top_right -> XY (Pct 100., Zero)

  let perspective_origin_of : position -> Css.perspective_origin = function
    | Bottom -> Css.Bottom
    | Bottom_left -> XY (Zero, Pct 100.)
    | Bottom_right -> XY (Pct 100., Pct 100.)
    | Center -> Css.Center
    | Left -> Single Zero
    | Right -> Single (Pct 100.)
    | Top -> Css.Top
    | Top_left -> XY (Zero, Zero)
    | Top_right -> XY (Pct 100., Zero)

  let perspective_origin_style ?theme position =
    let name = "perspective-origin-" ^ position_name position in
    let default = perspective_origin_of position in
    let default_css = position_css position in
    match Scheme.theme_value theme name with
    | Some value_str ->
        let decl = Css.custom_property ~layer:"theme" ("--" ^ name) value_str in
        let ref : Css.perspective_origin =
          Var (Var.theme_ref name ~default ~default_css)
        in
        let perspective_ref : Css.length =
          Css.Var
            (Var.theme_ref name ~default:(Css.Zero : Css.length) ~default_css)
        in
        style [ decl; perspective_origin ref; Css.perspective perspective_ref ]
    (* Without a theme override Tailwind writes the keyword, not a reference to
       a token nothing declares. *)
    | None -> style [ perspective_origin default ]

  (* Tailwind v4 uses theme variable references for origin utilities when theme
     variables are defined. *)
  let origin_style ?theme position =
    let name = "transform-origin-" ^ position_name position in
    let default = transform_origin_of position in
    match Scheme.theme_value theme name with
    | Some value_str ->
        let decl = Css.custom_property ~layer:"theme" ("--" ^ name) value_str in
        let ref : Css.transform_origin Css.var =
          Var.theme_ref name ~default ~default_css:(position_css position)
        in
        style [ decl; transform_origin (Var ref) ]
    | None -> style [ transform_origin default ]

  (** {1 Transform Control Utilities} *)

  (* Tailwind v4 transform utility uses individual rotate-x/y/z and skew-x/y
     variables with empty fallbacks. The output is: transform:
     var(--tw-rotate-x,) var(--tw-rotate-y,) var(--tw-rotate-z,)
     var(--tw-skew-x,) var(--tw-skew-y,); When a variable is set (e.g.,
     --tw-rotate-x: rotateX(45deg)), it becomes part of the transform.

     [transform-cpu] writes what [transform] writes, and [transform-gpu] adds
     translateZ(0) for GPU acceleration. The [@property] block for the five
     channels belongs to [transform] alone, though: Tailwind declares them there
     and neither of the other two carries them, so asking for the rules there
     put a whole [@layer properties] in a sheet whose only transform utility was
     one of those. *)
  let transform = style ~property_rules:rotate_skew_props [ rotate_skew_chain ]
  let transform_cpu = style [ rotate_skew_chain ]
  let transform_gpu = style [ transforms (Translate_z Zero :: chain_refs) ]

  (** {1 Utility Conversion Functions} *)

  let to_style theme = function
    | Op (op, axis, sign, amount) -> op_style theme op axis sign amount
    | Rotate_none -> style [ Css.rotate None ]
    | Translate_none -> style [ Css.translate None ]
    | Translate_3d ->
        style ~property_rules:translate_props [ translate_xyz_refs ]
    | Scale_none -> style [ Css.scale None ]
    | Scale_3d -> style ~property_rules:scale_props [ scale_xyz_refs ]
    | Perspective_none -> perspective_none ~theme ()
    | Perspective_theme name -> perspective_theme theme name
    | Perspective depth -> perspective_depth depth
    | Perspective_arbitrary (_, len) -> style [ Css.perspective len ]
    | Perspective_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "perspective" v))
    | Perspective_origin p -> perspective_origin_style ~theme p
    | Perspective_origin_arbitrary (_, po) -> style [ perspective_origin po ]
    | Perspective_origin_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "perspective-origin" v))
    | Transform_style_3d -> style [ transform_style Preserve_3d ]
    | Transform_style_flat -> style [ transform_style Flat ]
    | Transform_box_border -> style [ Css.transform_box Border_box ]
    | Transform_box_content -> style [ Css.transform_box Content_box ]
    | Transform_box_fill -> style [ Css.transform_box Fill_box ]
    | Transform_box_stroke -> style [ Css.transform_box Stroke_box ]
    | Transform_box_view -> style [ Css.transform_box View_box ]
    | Backface_visible -> style [ backface_visibility Visible ]
    | Backface_hidden -> style [ backface_visibility Hidden ]
    | Transform -> transform
    | Transform_cpu -> transform_cpu
    | Transform_none -> style [ Css.transform None ]
    | Transform_gpu -> transform_gpu
    | Transform_arbitrary (_, ts) -> style [ transforms ts ]
    | Transform_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "transform" v))
    | Origin p -> origin_style ~theme p
    | Origin_arbitrary (_, t) -> style [ transform_origin t ]
    | Origin_raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "transform-origin" v))

  (* Each operation and axis owns a band: its negative candidates take the first
     slot and its positive ones the next, sharing the slot whatever the value,
     since Tailwind orders a slot by the candidate's spelling. Baking the
     magnitude into the suborder reverses negatives and lets a large value spill
     into the next axis. Combined skew precedes skew-x, then skew-y. *)
  let band op axis =
    match (op, axis) with
    | Translate, All -> 20
    | Translate, X -> 100
    | Translate, Y -> 200
    | Translate, Z -> 300
    | Scale, All -> 400
    | Scale, X -> 500
    | Scale, Y -> 600
    | Scale, Z -> 700
    | Rotate, All -> 799
    | Rotate, X -> 900
    | Rotate, Y -> 1000
    | Rotate, Z -> 1100
    | Skew, All -> 1200
    | Skew, X -> 1210
    | Skew, (Y | Z) -> 1300

  (* The bare rotate splits its positive slot: a bare variable, then named
     values, then the arbitrary/none tail. *)
  let op_suborder op axis sign amount =
    match (op, axis, sign, amount) with
    | _, _, Neg, _ -> band op axis
    | Rotate, All, Pos, Bare_var _ -> band op axis + 1
    | Rotate, All, Pos, Int _ -> band op axis + 2
    | Rotate, All, Pos, _ -> band op axis + 3
    | _, _, Pos, _ -> band op axis + 1

  let suborder = function
    | Transform -> 2000
    | Transform_arbitrary _ | Transform_raw _ -> 2001
    | Transform_cpu -> 2002
    | Transform_gpu -> 2003
    | Transform_none -> 2004
    | Op (op, axis, sign, amount) -> op_suborder op axis sign amount
    | Translate_3d | Translate_none -> 302
    | Scale_3d | Scale_none -> 702
    | Rotate_none -> 802
    (* Perspective depths are one candidate band; Tailwind orders their class
       spellings rather than their semantic distance. *)
    | Perspective_arbitrary _ | Perspective_raw _ | Perspective _
    | Perspective_none | Perspective_theme _ ->
        1400
    | Perspective_origin_arbitrary _ | Perspective_origin_raw _ -> 1499
    | Perspective_origin p -> 1500 + position_rank p
    (* Alphabetical by class name: backface-hidden, backface-visible,
       transform-3d, transform-border, ..., transform-flat, ...,
       transform-view *)
    | Backface_hidden -> 1600
    | Backface_visible -> 1601
    | Transform_style_3d -> 1602
    | Transform_box_border -> 1603
    | Transform_box_content -> 1604
    | Transform_box_fill -> 1605
    | Transform_style_flat -> 1606
    | Transform_box_stroke -> 1607
    | Transform_box_view -> 1608
    | Origin_arbitrary _ | Origin_raw _ -> 1699
    | Origin p -> 1700 + position_rank p

  (** {1 Parsing Functions} *)

  let ( >|= ) = Parse.( >|= )

  (* A fractional spacing step: [0.5], [2.5]. Integers keep the existing
     path. *)
  let parse_spacing_step s =
    match Parse.decimal_float s with
    | Some f when f > 0. && not (Float.is_integer f) -> Some f
    | _ -> None

  (* The value a bracket denotes, read with the grammar cascade already has for
     the property, off the arbitrary-value pipeline every other family reads:
     [_] stands for a space, [--spacing(n)] expands, and a binary [+] gets the
     spaces CSS math wants, so [calc(1px+1px)] is a value rather than a parse
     error. [None] is a bracket that grammar refuses, and [of_class] declines
     the utility rather than leaving [to_style] to raise.

     A data-type hint chooses which longhand a bracket lands in and says nothing
     about the value. Every family reading through here writes one longhand, so
     the hint lands there and [read] is handed what follows it. A bracket whose
     hint is empty names no utility, which is the other [None]. The caller keeps
     the bracket whole for the class name, which is what the markup carries. *)
  let arbitrary_value read inner =
    Option.bind (Parse.value_after_hint inner) (fun value ->
        let cursor =
          Cascade.Cursor.of_string (Parse.decode_arbitrary_value value)
        in
        match Cascade.Cursor.try_parse_full_err read cursor with
        | Ok v -> Some v
        | Error _ -> None)

  let op_of_string = function
    | "translate" -> Some Translate
    | "scale" -> Some Scale
    | "rotate" -> Some Rotate
    | "skew" -> Some Skew
    | _ -> None

  let axis_of_string op a =
    match (op, a) with
    | _, "x" -> Some X
    | _, "y" -> Some Y
    | (Translate | Scale | Rotate), "z" -> Some Z
    | _ -> None

  (* The translate values a scale step does not cover: [full] on X, Y and both,
     [px] on every axis, a fraction (Tailwind writes [calc(n / m * 100%)] out
     rather than folding it, so a zero on either side is one) on X, Y and both,
     and a fractional spacing step on one axis. *)
  let translate_amount axis v =
    match (axis, v) with
    | (All | X | Y), "full" -> Some (Ok Full)
    | _, "px" -> Some (Ok Pixel)
    | (All | X | Y), _ when String.contains v '/' ->
        Some
          (match Parse.fraction v with
          | Some (n, d) -> Ok (Fraction (n, d))
          | None -> err_not_utility)
    | (X | Y | Z), _ -> Option.map (fun f -> Ok (Step f)) (parse_spacing_step v)
    | All, _ -> None

  (* [op] along [axis] with [v] after it, positive or negative. A bracket is
     taken when the operation's reader takes it, or, for a negative, when it
     holds a value at all. *)
  let op_of_value op axis sign v =
    let ok amount = Ok (Op (op, axis, sign, amount)) in
    if op = Rotate && Parse.is_bare_var v then
      ok (Bare_var (Parse.bare_var_inner v))
    else if Parse.is_bracket_value v then
      let inner = Parse.bracket_inner v in
      match sign with
      | Neg when neg_bracket v -> ok (Bracket inner)
      | Pos when Option.is_some (read op axis inner) -> ok (Bracket inner)
      | Neg | Pos -> err_not_utility
    else
      let value = if op = Translate then translate_amount axis v else None in
      match value with
      | Some amount -> Result.bind amount ok
      | None ->
          Parse.int_any v >|= fun n ->
          (* [-rotate-0] and the axis forms fold to the positive zero, class
             name included; only the bare translate keeps [-translate-0]. *)
          let sign =
            if n = 0 && not (op = Translate && axis = All) then Pos else sign
          in
          Op (op, axis, sign, Int n)

  let op_of_parts op sign = function
    | [ v ] -> op_of_value op All sign v
    | [ a; v ] when Option.is_some (axis_of_string op a) ->
        op_of_value op (Option.get (axis_of_string op a)) sign v
    (* The bare translate reads every later part as one bracket. *)
    | _ :: _ :: _ as rest when op = Translate && sign = Pos ->
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then op_of_value op All sign value
        else err_not_utility
    | _ -> err_not_utility

  (* A bracket after a property-valued family: the value its grammar reads, or
     the token stream as written. *)
  let bracket_of_parts ~arbitrary ~raw read parts =
    let value = String.concat "-" parts in
    if Parse.is_bracket_value value then
      let inner = Parse.bracket_inner value in
      match arbitrary_value read inner with
      | Some v -> Ok (arbitrary inner v)
      | None -> (
          match Parse.arbitrary_declaration_value inner with
          | Some value -> Ok (raw inner value)
          | None -> err_not_utility)
    else err_not_utility

  (* A named depth, then a project's [--perspective-*] token, then a bracket. *)
  let perspective_of_parts theme rest =
    let value = String.concat "-" rest in
    match List.find_opt (fun d -> [ depth_name d ] = rest) depths with
    | Some d -> Ok (Perspective d)
    | None when is_theme_perspective theme value -> Ok (Perspective_theme value)
    | None when not (Parse.is_bracket_value value) -> err_not_utility
    | None -> (
        let inner = Parse.bracket_inner value in
        match length_of_inner inner with
        | Some len -> Ok (Perspective_arbitrary (inner, len))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some v -> Ok (Perspective_raw (inner, v))
            | None -> err_not_utility))

  let of_class theme class_name =
    match Parse.split_class class_name with
    | [ "translate"; "none" ] -> Ok Translate_none
    | [ "translate"; "3d" ] -> Ok Translate_3d
    | [ "scale"; "none" ] -> Ok Scale_none
    | [ "scale"; "3d" ] -> Ok Scale_3d
    | [ "rotate"; "none" ] -> Ok Rotate_none
    | "" :: op :: rest when Option.is_some (op_of_string op) ->
        op_of_parts (Option.get (op_of_string op)) Neg rest
    | op :: rest when Option.is_some (op_of_string op) ->
        op_of_parts (Option.get (op_of_string op)) Pos rest
    | [ "perspective"; "none" ] -> Ok Perspective_none
    | "perspective" :: "origin" :: rest -> (
        match position_of_parts rest with
        | Some p -> Ok (Perspective_origin p)
        | None ->
            bracket_of_parts
              ~arbitrary:(fun s v -> Perspective_origin_arbitrary (s, v))
              ~raw:(fun s v -> Perspective_origin_raw (s, v))
              Css.Properties.read_perspective_origin rest)
    | "perspective" :: (_ :: _ as rest) -> perspective_of_parts theme rest
    | [ "transform"; "style"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "style"; "flat" ] -> Ok Transform_style_flat
    | [ "transform"; "3d" ] -> Ok Transform_style_3d
    | [ "transform"; "flat" ] -> Ok Transform_style_flat
    | [ "backface"; "hidden" ] -> Ok Backface_hidden
    | [ "backface"; "visible" ] -> Ok Backface_visible
    | [ "transform"; "border" ] -> Ok Transform_box_border
    | [ "transform"; "content" ] -> Ok Transform_box_content
    | [ "transform"; "fill" ] -> Ok Transform_box_fill
    | [ "transform"; "stroke" ] -> Ok Transform_box_stroke
    | [ "transform"; "view" ] -> Ok Transform_box_view
    | [ "transform"; value ] when Parse.is_bracket_value value ->
        bracket_of_parts
          ~arbitrary:(fun s v -> Transform_arbitrary (s, v))
          ~raw:(fun s v -> Transform_raw (s, v))
          Css.Properties.read_transforms [ value ]
    | [ "transform" ] -> Ok Transform
    | [ "transform"; "cpu" ] -> Ok Transform_cpu
    | [ "transform"; "none" ] -> Ok Transform_none
    | [ "transform"; "gpu" ] -> Ok Transform_gpu
    | "origin" :: rest -> (
        match position_of_parts rest with
        | Some p -> Ok (Origin p)
        | None ->
            bracket_of_parts
              ~arbitrary:(fun s v -> Origin_arbitrary (s, v))
              ~raw:(fun s v -> Origin_raw (s, v))
              Css.Properties.read_transform_origin rest)
    | _ -> err_not_utility

  let op_name = function
    | Translate -> "translate"
    | Scale -> "scale"
    | Rotate -> "rotate"
    | Skew -> "skew"

  let axis_suffix = function All -> "" | X -> "-x" | Y -> "-y" | Z -> "-z"

  (* Tailwind writes a fractional step as the author did, so keep the trailing
     digits. *)
  let amount_class = function
    | Int n -> string_of_int n
    | Step f -> pp_float f
    | Full -> "full"
    | Pixel -> "px"
    | Fraction (n, d) -> string_of_int n ^ "/" ^ string_of_int d
    | Bare_var name -> "(" ^ name ^ ")"
    | Bracket s -> "[" ^ s ^ "]"

  let to_class = function
    | Op (op, axis, sign, amount) ->
        (match sign with Neg -> "-" | Pos -> "")
        ^ op_name op ^ axis_suffix axis ^ "-" ^ amount_class amount
    | Rotate_none -> "rotate-none"
    | Translate_none -> "translate-none"
    | Translate_3d -> "translate-3d"
    | Scale_none -> "scale-none"
    | Scale_3d -> "scale-3d"
    | Perspective_none -> "perspective-none"
    | Perspective_theme name -> "perspective-" ^ name
    | Perspective depth -> "perspective-" ^ depth_name depth
    | Perspective_arbitrary (raw, _) | Perspective_raw (raw, _) ->
        "perspective-[" ^ raw ^ "]"
    | Perspective_origin p -> "perspective-origin-" ^ position_name p
    | Perspective_origin_arbitrary (s, _) | Perspective_origin_raw (s, _) ->
        "perspective-origin-[" ^ s ^ "]"
    | Transform_style_3d -> "transform-3d"
    | Transform_style_flat -> "transform-flat"
    | Transform_box_border -> "transform-border"
    | Transform_box_content -> "transform-content"
    | Transform_box_fill -> "transform-fill"
    | Transform_box_stroke -> "transform-stroke"
    | Transform_box_view -> "transform-view"
    | Backface_visible -> "backface-visible"
    | Backface_hidden -> "backface-hidden"
    | Transform -> "transform"
    | Transform_cpu -> "transform-cpu"
    | Transform_none -> "transform-none"
    | Transform_gpu -> "transform-gpu"
    | Transform_arbitrary (s, _) | Transform_raw (s, _) ->
        "transform-[" ^ s ^ "]"
    | Origin p -> "origin-" ^ position_name p
    | Origin_arbitrary (s, _) | Origin_raw (s, _) -> "origin-[" ^ s ^ "]"

  let examples =
    [
      Origin Top;
      Perspective_none;
      Perspective_origin Top;
      Backface_hidden;
      Transform_none;
      Transform_style_flat;
      Op (Rotate, All, Pos, Int 45);
      Op (Scale, All, Pos, Int 50);
      Op (Translate, X, Pos, Int 2);
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v

(* A signed scale value, its sign written as the class's leading [-]. *)
let signed_utility op axis n =
  utility (Op (op, axis, (if n < 0 then Neg else Pos), Int (abs n)))

(* A whole-number float keeps the int constructor's own 0/1 shortcuts
   (calc(var(--spacing)) rather than calc(var(--spacing) * 1)); only a genuine
   fractional value needs the step form. *)
let step axis f =
  if Float.is_integer f then signed_utility Translate axis (int_of_float f)
  else
    utility
      (Op (Translate, axis, (if f < 0. then Neg else Pos), Step (Float.abs f)))

let rotate = signed_utility Rotate All
let translate_x = signed_utility Translate X
let translate_x' = step X
let translate_y = signed_utility Translate Y
let translate_y' = step Y
let translate_z = signed_utility Translate Z
let translate_z' = step Z

let translate_fraction axis sign num denom =
  utility (Op (Translate, axis, sign, Fraction (num, denom)))

let translate_x_fraction = translate_fraction X Pos
let translate_y_fraction = translate_fraction Y Pos
let neg_translate_x_1_2 = translate_fraction X Neg 1 2
let neg_translate_y_1_2 = translate_fraction Y Neg 1 2
let scale = signed_utility Scale All
let scale_x = signed_utility Scale X
let scale_y = signed_utility Scale Y
let scale_z = signed_utility Scale Z
let skew_x = signed_utility Skew X
let skew_y = signed_utility Skew Y
let rotate_x = signed_utility Rotate X
let rotate_y = signed_utility Rotate Y
let rotate_z = signed_utility Rotate Z
let perspective_none = utility Perspective_none
let perspective_dramatic = utility (Perspective Dramatic)
let perspective_near = utility (Perspective Near)
let perspective_normal = utility (Perspective Normal)
let perspective_midrange = utility (Perspective Midrange)
let perspective_distant = utility (Perspective Distant)
let perspective_origin_center = utility (Perspective_origin Center)
let perspective_origin_top = utility (Perspective_origin Top)
let perspective_origin_bottom = utility (Perspective_origin Bottom)
let perspective_origin_left = utility (Perspective_origin Left)
let perspective_origin_right = utility (Perspective_origin Right)
let transform_style_3d = utility Transform_style_3d
let transform_style_flat = utility Transform_style_flat
let transform_box_border = utility Transform_box_border
let transform_box_content = utility Transform_box_content
let transform_box_fill = utility Transform_box_fill
let transform_box_stroke = utility Transform_box_stroke
let transform_box_view = utility Transform_box_view
let backface_visible = utility Backface_visible
let backface_hidden = utility Backface_hidden
let transform = utility Transform
let transform_none = utility Transform_none
let transform_gpu = utility Transform_gpu
let origin_center = utility (Origin Center)
let origin_top = utility (Origin Top)
let origin_bottom = utility (Origin Bottom)
let origin_left = utility (Origin Left)
let origin_right = utility (Origin Right)
let origin_top_left = utility (Origin Top_left)
let origin_top_right = utility (Origin Top_right)
let origin_bottom_left = utility (Origin Bottom_left)
let origin_bottom_right = utility (Origin Bottom_right)
