(** Border utilities for border styles, widths, and radii

    What's included:
    - Border widths: `border`, `border-0/2/4/8`, and side/axis variants.
    - Border styles: `border-solid/dashed/dotted/double/none` via a CSS var.
    - Border radius: `rounded-*` and `rounded-full`.
    - Side-specific border radius: `rounded-t-*`, `rounded-r-*`, `rounded-b-*`,
      `rounded-l-*` for top/right/bottom/left sides.
    - Corner-specific border radius: `rounded-tl-*`, `rounded-tr-*`,
      `rounded-br-*`, `rounded-bl-*` for individual corners.
    - Outline offsets and simple ring helpers.

    Parsing contract (`of_string`):
    - Accepts tokens like ["border"], ["border"; "t"], ["rounded"; "md"],
      ["ring"; "2"], ["outline"; "offset"; "4"]. Unknown tokens yield `Error
      (`Msg "Not a border utility")`. *)

module Css = Cascade.Css

(* Resolve the optionally-threaded theme, defaulting to the base scheme. *)
let resolve_scheme = function Some s -> s | None -> Scheme.default

(* Which corners a [rounded-*] utility rounds: every corner, one physical or
   logical side, or a single corner. *)
module Corner = struct
  type t =
    | All
    | Start
    | End
    | Top
    | Right
    | Bottom
    | Left
    | Start_start
    | Start_end
    | End_end
    | End_start
    | Top_left
    | Top_right
    | Bottom_right
    | Bottom_left
end

(* Border-radius t-shirt sizes. [Rsz_default] is the bare [rounded] (no size
   suffix). [Rsz_arbitrary] carries the bracket inner value. The [Rsz_] prefix
   avoids clashing with [Option.None]. *)
type rounded_size =
  | Rsz_none
  | Rsz_xs
  | Rsz_sm
  | Rsz_default
  | Rsz_md
  | Rsz_lg
  | Rsz_xl
  | Rsz_2xl
  | Rsz_3xl
  | Rsz_4xl
  | Rsz_full
  | Rsz_theme of string
    (* radius named by a project [--radius-*] token (rounded-blob) *)
  | Rsz_arbitrary of string * Css.length

module Handler = struct
  open Style
  open Css

  type t =
    | (* Border width utilities *)
      Border
    | Border_0
    | Border_2
    | Border_4
    | Border_8
    | Border_width of int
    | Border_width_bracket of string * Css.border_width
    | Border_side_width_bracket of string * string * Css.border_width
      (* side ("t"/"r"/"b"/"l"), arbitrary width inner: border-t-[1px] *)
    | Border_side_width of string * int
      (* side ("t"/"r"/"b"/"l") with a width Tailwind spells in px: border-t-16.
         The 0/2/4/8 constructors below keep their own names, as Border_0/2/4/8
         do beside Border_width. *)
    | (* Border side/axis utilities *)
      Border_t
    | Border_r
    | Border_b
    | Border_l
    | Border_x
    | Border_y
    | Border_x_width of int
    | Border_y_width of int
    | (* Logical single-side borders: inline/block start/end *)
      Border_s
    | Border_e
    | Border_bs
    | Border_be
    | Border_s_width of int
    | Border_e_width of int
    | Border_bs_width of int
    | Border_be_width of int
    | (* Border style utilities *)
      Border_solid
    | Border_dashed
    | Border_dotted
    | Border_double
    | Border_hidden
    | Border_none
    | (* Border radius utilities. One parametric variant over (position, size);
         the bare [rounded] is [Rounded (Corner.All, Rsz_default)] and arbitrary
         brackets are [Rounded (pos, Rsz_arbitrary inner)]. *)
      Rounded of Corner.t * rounded_size
    | (* Outline utilities *)
      Outline
    | Outline_0
    | Outline_width of int (* outline-1, outline-2, outline-4, outline-8 *)
    | Outline_width_bracket of
        string * Css.length (* outline-[12px], outline-[1.5], etc. *)
    | Outline_width_var of string (* outline-[length:var(...)], etc. *)
    (* Outline style utilities (dashed, dotted, etc.) are in
       Outline_style_handler *)
    | Outline_hidden
    | Outline_offset of int
    | Outline_offset_var of string (* outline-offset-[var(--value)] *)
    | Outline_offset_arbitrary of string (* outline-offset-[3px] *)
    | Neg_outline_offset of int
    | Neg_outline_offset_var of string (* -outline-offset-[var(--value)] *)

  type Utility.base += Self of t

  let name = "borders"

  (* border-width/color/radius sort at priority 19. outline-* (canonical rank
     ~92) comes much later - after box-shadow and ring (effects, priority 27),
     before the filters (priority 30) - so the outline variants return priority
     28. *)
  let priority = function
    | Outline | Outline_0 | Outline_width _ | Outline_width_bracket _
    | Outline_width_var _ | Outline_hidden | Outline_offset _
    | Outline_offset_var _ | Outline_offset_arbitrary _ | Neg_outline_offset _
    | Neg_outline_offset_var _ ->
        28
    | _ -> 19

  (* Create border style variable with @property for utilities that reference
     it. Position 6 to match Tailwind's order after translate (0-2) and scale
     (3-5). *)
  let border_style_var =
    Var.property_default Css.Border_style
      ~initial:(Solid : Css.border_style)
      ~property_order:6 ~family:`Border "tw-border-style"

  (* Helper for border utilities that reference the variable with @property
     default *)
  let make_border_util additional_props =
    let border_ref = Var.reference border_style_var in
    let property_rule =
      match Var.property_rule border_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule
      (border_style (Var border_ref) :: additional_props)

  (* Helper for border style utilities that set the variable *)
  let border_style_util border_style_value =
    let decl, _ = Var.binding border_style_var border_style_value in
    style [ decl; border_style border_style_value ]

  let border_default ?theme () =
    make_border_util
      [
        Css.border_width
          (Px (float_of_int (resolve_scheme theme).default_border_width));
      ]

  let border_0 = make_border_util [ Css.border_width (Px 0.) ]
  let border_2 = make_border_util [ Css.border_width (Px 2.) ]
  let border_4 = make_border_util [ Css.border_width (Px 4.) ]
  let border_8 = make_border_util [ Css.border_width (Px 8.) ]
  let border_n n = make_border_util [ Css.border_width (Px (float_of_int n)) ]

  let border_width_bracket_style width =
    make_border_util [ Css.border_width width ]

  (* Helper for border side utilities that reference the variable with @property
     default *)
  let make_side_util side_props_fn =
    let border_ref = Var.reference border_style_var in
    let property_rule =
      match Var.property_rule border_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule (side_props_fn border_ref)

  let border_t =
    make_side_util (fun border_var ->
        [ border_top_style (Var border_var); border_top_width (Px 1.) ])

  let border_r =
    make_side_util (fun border_var ->
        [ border_right_style (Var border_var); border_right_width (Px 1.) ])

  let border_b =
    make_side_util (fun border_var ->
        [ border_bottom_style (Var border_var); border_bottom_width (Px 1.) ])

  let border_l =
    make_side_util (fun border_var ->
        [ border_left_style (Var border_var); border_left_width (Px 1.) ])

  (* v4 axis borders paint the logical inline and block axes via the
     border-inline and border-block longhands, not the two physical edges. *)
  let border_inline_axis border_var w =
    [
      border_inline_style (Var border_var);
      border_inline_width (logical_border_width (Px w));
    ]

  let border_block_axis border_var w =
    [
      border_block_style (Var border_var);
      border_block_width (logical_border_width (Px w));
    ]

  let border_x = make_side_util (fun v -> border_inline_axis v 1.)
  let border_y = make_side_util (fun v -> border_block_axis v 1.)
  let border_x_width n = make_side_util (fun v -> border_inline_axis v n)
  let border_y_width n = make_side_util (fun v -> border_block_axis v n)

  (* Logical single-side borders paint one inline/block edge, unlike the axis
     utilities above which paint both edges of an axis. *)
  let border_s_of w =
    make_side_util (fun v ->
        [ border_inline_start_style (Var v); border_inline_start_width (Px w) ])

  let border_e_of w =
    make_side_util (fun v ->
        [ border_inline_end_style (Var v); border_inline_end_width (Px w) ])

  let border_bs_of w =
    make_side_util (fun v ->
        [ border_block_start_style (Var v); border_block_start_width (Px w) ])

  let border_be_of w =
    make_side_util (fun v ->
        [ border_block_end_style (Var v); border_block_end_width (Px w) ])

  let border_s = border_s_of 1.
  let border_e = border_e_of 1.
  let border_bs = border_bs_of 1.
  let border_be = border_be_of 1.
  let border_s_width n = border_s_of (float_of_int n)
  let border_e_width n = border_e_of (float_of_int n)
  let border_bs_width n = border_bs_of (float_of_int n)
  let border_be_width n = border_be_of (float_of_int n)

  (** Border side utilities with specific widths *)
  let border_solid = border_style_util Solid

  let border_dashed = border_style_util Dashed
  let border_dotted = border_style_util Dotted
  let border_double = border_style_util Double
  let border_hidden = border_style_util Hidden
  let border_none = border_style_util None

  (* Arbitrary radii go through the same normalisation as the other arbitrary
     length utilities, so calc() and var() references parse. *)
  let parse_length str : length option =
    Css.parse_length
      (Parse.normalize_css_math_operators (Parse.decode_arbitrary_value str))

  (* A bare number is not a CSS length, but Tailwind admits [border-[3]] and
     emits it as a width, so it keeps the px reading it has always had. *)
  let parse_bare_number str =
    let is_number_char c = (c >= '0' && c <= '9') || c = '.' || c = '-' in
    if str <> "" && String.for_all is_number_char str then
      float_of_string_opt str
    else None

  (* [border-[X]] is a width or a colour, and the class grammar has to say which
     before the value is read. The three CSS line-width keywords are the only
     idents on the width side. *)
  let is_line_width_keyword = function
    | "thin" | "medium" | "thick" -> true
    | _ -> false

  (* cascade's own border-width reader answers the line-width keywords, every
     unit [Css.border_width] names, [calc()] and [var()]. The units only
     [Css.length] has (the dynamic-viewport and container-query families) are
     not border widths there either, so such a bracket is still refused. *)
  let parse_border_width inner : Css.border_width option =
    let cursor =
      Cascade.Cursor.of_string
        (Parse.normalize_css_math_operators
           (Parse.decode_arbitrary_value inner))
    in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_border_width cursor
    with
    | Ok w -> Some w
    | Error _ -> (
        match parse_bare_number inner with
        | Some f -> Some (Px f)
        | None -> None)

  let parse_outline_width inner : Css.length option =
    match parse_length inner with
    | Some _ as len -> len
    | None -> (
        match parse_bare_number inner with
        | Some f -> Some (Px f)
        | None -> None)

  (* A [--radius-*] token the project declared names a radius the built-in scale
     has no slot for; they share the slot after the scale, the way the project's
     font families and text sizes share theirs. *)
  let radius_named_cache : (string, Css.length Var.theme) Hashtbl.t =
    Hashtbl.create 8

  let radius_named_var name =
    match Hashtbl.find_opt radius_named_cache name with
    | Some var -> var
    | None ->
        let var = Var.theme Css.Length ("radius-" ^ name) ~order:(7, 11) in
        Hashtbl.add radius_named_cache name var;
        var

  let radius_none_var = Var.theme Css.Length "radius-none" ~order:(7, 0)
  let radius_full_var = Var.theme Css.Length "radius-full" ~order:(7, 1)
  let radius_xs_var = Var.theme Css.Length "radius-xs" ~order:(7, 2)
  let radius_sm_var = Var.theme Css.Length "radius-sm" ~order:(7, 3)
  let radius_var = Var.theme Css.Length "radius" ~order:(7, 4)
  let radius_md_var = Var.theme Css.Length "radius-md" ~order:(7, 5)
  let radius_lg_var = Var.theme Css.Length "radius-lg" ~order:(7, 6)
  let radius_xl_var = Var.theme Css.Length "radius-xl" ~order:(7, 7)
  let radius_2xl_var = Var.theme Css.Length "radius-2xl" ~order:(7, 8)
  let radius_3xl_var = Var.theme Css.Length "radius-3xl" ~order:(7, 9)
  let radius_4xl_var = Var.theme Css.Length "radius-4xl" ~order:(7, 10)

  let radius_value len =
    Css.Radius { horizontal = [ Css.Length len ]; vertical = None }

  (* Tailwind's --radius-full default is calc(infinity * 1px); FLT_MAX is the
     canonical large value that infinity folds to, so the all-corners and
     per-side/corner "full" utilities share it. *)
  let radius_full_len : Css.length = Css.Px 3.40282e38

  (* Map a [Corner.t] to the corner/side radius declarations for a given length.
     Shared by both the sized and arbitrary radius utilities; [Corner.All] uses
     the [border-radius] shorthand, all others target the matching
     corner/logical properties. *)
  let radius_decls_for_position pos (len : Css.length) : Css.declaration list =
    match pos with
    | Corner.All -> [ Css.border_radius (radius_value len) ]
    | Corner.Top ->
        [ Css.border_top_left_radius len; Css.border_top_right_radius len ]
    | Corner.Right ->
        [ Css.border_top_right_radius len; Css.border_bottom_right_radius len ]
    | Corner.Bottom ->
        [
          Css.border_bottom_right_radius len; Css.border_bottom_left_radius len;
        ]
    | Corner.Left ->
        [ Css.border_top_left_radius len; Css.border_bottom_left_radius len ]
    | Corner.Top_left -> [ Css.border_top_left_radius len ]
    | Corner.Top_right -> [ Css.border_top_right_radius len ]
    | Corner.Bottom_right -> [ Css.border_bottom_right_radius len ]
    | Corner.Bottom_left -> [ Css.border_bottom_left_radius len ]
    | Corner.Start ->
        [ Css.border_start_start_radius len; Css.border_end_start_radius len ]
    | Corner.End ->
        [ Css.border_start_end_radius len; Css.border_end_end_radius len ]
    | Corner.Start_start -> [ Css.border_start_start_radius len ]
    | Corner.Start_end -> [ Css.border_start_end_radius len ]
    | Corner.End_end -> [ Css.border_end_end_radius len ]
    | Corner.End_start -> [ Css.border_end_start_radius len ]

  (* Per-side/corner "full"/"none" radius: reference var(--radius-X) when the
     active theme defines that radius (e.g. an @theme override, as the upstream
     fixtures use), otherwise inline the literal -- matching Tailwind, which
     inlines calc(infinity*1px)/0 by default but keys off the token when set. *)
  let scheme_keyed_radius ?theme key var ~(default : Css.length) pos =
    match Scheme.radius (resolve_scheme theme) key with
    | Some explicit ->
        let decl, r = Var.binding var explicit in
        style (decl :: radius_decls_for_position pos (Var r : Css.length))
    | None -> style (radius_decls_for_position pos default)

  (* Helper: return (decls, length_val) for the bare --radius variable. When
     theme_value "radius" is set (e.g. @config theme), emit the var declaration
     and use var(--radius). Otherwise inline .25rem directly. *)
  let radius_decl_and_val ?theme () : Css.declaration list * Css.length =
    if Scheme.theme_value theme "radius" <> None then
      let decl, r = Var.binding radius_var (Rem 0.25) in
      ([ decl ], Css.Var r)
    else ([], Css.Rem 0.25)

  (* Sized radius (xs/sm/md/lg/xl/2xl/3xl/4xl): always bind the theme var with
     its default and reference it. *)
  let sized_radius var default pos =
    let decl, r = Var.binding var default in
    style (decl :: radius_decls_for_position pos (Var r : Css.length))

  (* The single parametric radius style covering every (position, size) pair. *)
  let rounded_style ?theme pos size =
    match size with
    | Rsz_none ->
        scheme_keyed_radius ?theme "none" radius_none_var ~default:Css.Zero pos
    | Rsz_full ->
        scheme_keyed_radius ?theme "full" radius_full_var
          ~default:radius_full_len pos
    | Rsz_default ->
        let extra, v = radius_decl_and_val ?theme () in
        style (extra @ radius_decls_for_position pos v)
    | Rsz_xs -> sized_radius radius_xs_var (Css.Rem 0.125) pos
    | Rsz_sm -> sized_radius radius_sm_var (Css.Rem 0.25) pos
    | Rsz_md -> sized_radius radius_md_var (Css.Rem 0.375) pos
    | Rsz_lg -> sized_radius radius_lg_var (Css.Rem 0.5) pos
    | Rsz_xl -> sized_radius radius_xl_var (Css.Rem 0.75) pos
    | Rsz_2xl -> sized_radius radius_2xl_var (Css.Rem 1.0) pos
    | Rsz_3xl -> sized_radius radius_3xl_var (Css.Rem 1.5) pos
    | Rsz_4xl -> sized_radius radius_4xl_var (Css.Rem 2.0) pos
    | Rsz_theme name -> (
        let token = "radius-" ^ name in
        match Scheme.theme_value theme token with
        | None -> style []
        | Some raw -> (
            match Css.parse_length raw with
            | None -> style []
            | Some len ->
                if Scheme.is_inline_token (resolve_scheme theme) token then
                  style (radius_decls_for_position pos len)
                else
                  let decl, r = Var.binding (radius_named_var name) len in
                  style
                    (decl :: radius_decls_for_position pos (Var r : Css.length))
            ))
    | Rsz_arbitrary (_, len) -> style (radius_decls_for_position pos len)

  (* Outline style variable - used by outline utilities that set the style *)
  let outline_style_var =
    (* Tailwind emits @property --tw-outline-style after the ring group and
       before the filters. property_order 21 places it just after
       --tw-ring-offset-shadow (20) in the cross-family comparison; the earlier
       value of 0 wrongly sorted it ahead of the whole ring/border block. *)
    Var.property_default Css.Outline_style
      ~initial:(Solid : Css.outline_style)
      ~property_order:21 ~family:`Border "tw-outline-style"

  (* Base outline utility - sets outline-style from var and width from scheme *)
  let outline ?theme () =
    let oref = Var.reference outline_style_var in
    let property_rule =
      match Var.property_rule outline_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    let width = float_of_int (resolve_scheme theme).default_outline_width in
    style ~property_rules:property_rule
      [ Css.outline_style (Css.Var oref); Css.outline_width (Px width) ]

  (* Outline-0: style from var + width: 0 *)
  let outline_0 =
    let oref = Var.reference outline_style_var in
    let property_rule =
      match Var.property_rule outline_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule
      [ Css.outline_style (Css.Var oref); Css.outline_width Zero ]

  (* Outline numeric width: outline-1, outline-2, outline-4, outline-8 *)
  let outline_width_style n =
    let oref = Var.reference outline_style_var in
    let property_rule =
      match Var.property_rule outline_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule
      [
        Css.outline_style (Css.Var oref);
        Css.outline_width (Px (float_of_int n));
      ]

  (* Outline bracket width: outline-[12px], outline-[1.5], outline-[50%] *)
  let outline_width_bracket_style (width : Css.length) =
    let oref = Var.reference outline_style_var in
    let property_rule =
      match Var.property_rule outline_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    style ~property_rules:property_rule
      [ Css.outline_style (Css.Var oref); Css.outline_width width ]

  (* Outline typed bracket width: outline-[length:var(...)], etc. *)
  let outline_width_var_style v =
    let oref = Var.reference outline_style_var in
    let property_rule =
      match Var.property_rule outline_style_var with
      | Some rule -> rule
      | None -> Css.empty
    in
    (* Strip type prefix like "length:" before extracting var name *)
    let var_part =
      match String.index_opt v ':' with
      | Some i -> String.sub v (i + 1) (String.length v - i - 1)
      | None -> v
    in
    let bare_name = Parse.extract_var_name var_part in
    let var_ref : Css.length Css.var = Var.bracket bare_name in
    style ~merge_key:"outline-" ~property_rules:property_rule
      [ Css.outline_style (Css.Var oref); Css.outline_width (Var var_ref) ]

  (* Outline style utilities that set the variable *)
  let outline_hidden =
    let decl, _ = Var.binding outline_style_var Css.None in
    (* Base style: outline-style: none *)
    (* In forced-colors mode, reset outline with shorthand + offset *)
    let forced_colors_active =
      Css.Media.Cond
        (Css.Media.Feature
           (Css.Media.Plain
              (Css.Media.Forced_colors, Css.Media.Ident Css.Media.Active)))
    in
    let media_rule =
      Css.media ~condition:forced_colors_active
        [
          Css.rule ~selector:(Css.Selector.class_ "_")
            [
              Css.outline_offset (Px 2.);
              Css.outline
                (Shorthand
                   {
                     width = Some (Px 2.);
                     style = Some Solid;
                     color = Some (Css.hex "#0000");
                   });
            ];
        ]
    in
    style ~rules:(Some [ media_rule ]) [ decl; Css.outline_style Css.None ]

  (* Outline offset accepts any bare integer, like the border widths. *)
  let outline_offset_px n = style [ Css.outline_offset (Px (float_of_int n)) ]

  (* Negative outline offset - use calc(Npx * -1) format *)
  let neg_outline_offset n =
    let calc : Css.length =
      Calc (Expr (Val (Px (float_of_int n)), Mul, Num (-1.)))
    in
    style [ Css.outline_offset calc ]

  let outline_offset_var_style v =
    let bare_name = Parse.extract_var_name v in
    let var_ref : Css.length Css.var = Var.bracket bare_name in
    style [ Css.outline_offset (Var var_ref) ]

  let neg_outline_offset_var_style v =
    let bare_name = Parse.extract_var_name v in
    let neg_len : Css.length =
      Calc (Calc.mul (Calc.var bare_name) (Calc.float (-1.)))
    in
    style [ Css.outline_offset neg_len ]

  let to_style theme : t -> Style.t =
    let border_default () = border_default ~theme () in
    let outline () = outline ~theme () in
    function
    (* Border width utilities *)
    | Border -> border_default ()
    | Border_0 -> border_0
    | Border_2 -> border_2
    | Border_4 -> border_4
    | Border_8 -> border_8
    | Border_width n -> border_n n
    | Border_width_bracket (_, w) -> border_width_bracket_style w
    | Border_side_width_bracket (side, _, w) ->
        make_side_util (fun bv ->
            match side with
            | "t" -> [ border_top_style (Var bv); border_top_width w ]
            | "r" -> [ border_right_style (Var bv); border_right_width w ]
            | "b" -> [ border_bottom_style (Var bv); border_bottom_width w ]
            | _ -> [ border_left_style (Var bv); border_left_width w ])
    | Border_side_width (side, n) ->
        let w : Css.border_width = Px (float_of_int n) in
        make_side_util (fun bv ->
            match side with
            | "t" -> [ border_top_style (Var bv); border_top_width w ]
            | "r" -> [ border_right_style (Var bv); border_right_width w ]
            | "b" -> [ border_bottom_style (Var bv); border_bottom_width w ]
            | _ -> [ border_left_style (Var bv); border_left_width w ])
    (* Border side/axis utilities *)
    | Border_t -> border_t
    | Border_r -> border_r
    | Border_b -> border_b
    | Border_l -> border_l
    | Border_x -> border_x
    | Border_y -> border_y
    | Border_x_width n -> border_x_width (float_of_int n)
    | Border_y_width n -> border_y_width (float_of_int n)
    | Border_s -> border_s
    | Border_e -> border_e
    | Border_bs -> border_bs
    | Border_be -> border_be
    | Border_s_width n -> border_s_width n
    | Border_e_width n -> border_e_width n
    | Border_bs_width n -> border_bs_width n
    | Border_be_width n -> border_be_width n
    (* Border style utilities *)
    | Border_solid -> border_solid
    | Border_dashed -> border_dashed
    | Border_dotted -> border_dotted
    | Border_double -> border_double
    | Border_hidden -> border_hidden
    | Border_none -> border_none
    (* Border radius utilities (parametric) *)
    | Rounded (pos, size) -> rounded_style ~theme pos size
    (* Outline utilities *)
    | Outline -> outline ()
    | Outline_0 -> outline_0
    | Outline_width n -> outline_width_style n
    | Outline_width_bracket (_, len) -> outline_width_bracket_style len
    | Outline_width_var v -> outline_width_var_style v
    | Outline_hidden -> outline_hidden
    | Outline_offset n -> outline_offset_px n
    | Outline_offset_var v -> outline_offset_var_style v
    | Outline_offset_arbitrary v -> (
        match parse_length v with
        | Some l -> style [ Css.outline_offset l ]
        | None -> style [ Css.outline_offset (Px 0.) ])
    | Neg_outline_offset n -> neg_outline_offset n
    | Neg_outline_offset_var v -> neg_outline_offset_var_style v

  let err_not_utility = Error (`Msg "Not a border utility")

  (* Parse the side/corner segment of a class into the corners it rounds. *)
  let corner_of_string = function
    | "s" -> Some Corner.Start
    | "e" -> Some Corner.End
    | "t" -> Some Corner.Top
    | "r" -> Some Corner.Right
    | "b" -> Some Corner.Bottom
    | "l" -> Some Corner.Left
    | "ss" -> Some Corner.Start_start
    | "se" -> Some Corner.Start_end
    | "ee" -> Some Corner.End_end
    | "es" -> Some Corner.End_start
    | "tl" -> Some Corner.Top_left
    | "tr" -> Some Corner.Top_right
    | "br" -> Some Corner.Bottom_right
    | "bl" -> Some Corner.Bottom_left
    | _ -> None

  (* Parse a rounded size token (the t-shirt segment of a class). *)
  let rounded_size_of_string = function
    | "none" -> Some Rsz_none
    | "xs" -> Some Rsz_xs
    | "sm" -> Some Rsz_sm
    | "md" -> Some Rsz_md
    | "lg" -> Some Rsz_lg
    | "xl" -> Some Rsz_xl
    | "2xl" -> Some Rsz_2xl
    | "3xl" -> Some Rsz_3xl
    | "4xl" -> Some Rsz_4xl
    | "full" -> Some Rsz_full
    | _ -> None

  (* A radius the project named in its [@theme]. The built-in scale is published
     through the same registry, so a built-in name is answered by
     [rounded_size_of_string] before this is consulted. *)
  let is_theme_radius theme n =
    rounded_size_of_string n = None
    && Scheme.theme_value (Some theme) ("radius-" ^ n) <> None

  (* Forty apart, so a side's widths and its bracket never reach the next. *)
  let side_band = function "t" -> 1160 | "r" -> 1200 | "b" -> 1240 | _ -> 1280

  let suborder = function
    (* Border radius utilities - flat suborder per position group for natural
       sort by class name *)
    | Rounded (pos, _) -> (
        (* Position groups sort by the CSS properties they write, matching
           Tailwind: the logical corners (start/end) first, then the physical
           ones grouped by their first corner clockwise -- top, then left, then
           right, then bottom. Values within a group share a suborder and
           tie-break by class name. *)
        match pos with
        | Corner.All -> 0
        | Corner.Start -> 10
        | Corner.Start_start -> 20
        | Corner.End -> 30
        | Corner.Start_end -> 40
        | Corner.End_end -> 50
        | Corner.End_start -> 60
        | Corner.Top -> 100
        | Corner.Left -> 110
        | Corner.Top_left -> 120
        | Corner.Right -> 130
        | Corner.Top_right -> 140
        | Corner.Bottom -> 150
        | Corner.Bottom_right -> 160
        | Corner.Bottom_left -> 170)
    (* Border width utilities (1000-1099) *)
    | Border -> 1000
    | Border_0 -> 1001
    | Border_2 -> 1002
    | Border_4 -> 1003
    | Border_8 -> 1004
    | Border_width n -> 1001 + n
    | Border_width_bracket _ -> 1005
    (* Border sides are side-major (Tailwind groups each side's bare width, its
       numeric widths and its arbitrary width together), and the axes and
       logical sides come first: x, y, s, e, bs, be, then t, r, b, l. The order
       decides the width, since border-y and border-b both write the bottom one.
       Within a side: bare, 0, 2, 4, 8, arbitrary. *)
    | Border_x -> 1100
    | Border_x_width n -> 1100 + n
    | Border_y -> 1110
    | Border_y_width n -> 1110 + n
    | Border_s -> 1120
    | Border_s_width n -> 1120 + n
    | Border_e -> 1130
    | Border_e_width n -> 1130 + n
    | Border_bs -> 1140
    | Border_bs_width n -> 1140 + n
    | Border_be -> 1150
    | Border_be_width n -> 1150 + n
    (* Each side gets a band of its own: the bare side leads, then the widths in
       numeric order, then the bracket. They all write one property, so the
       order decides which one wins, and the band has to be wide enough for a
       width the scale does not name - [border-t-6] used to sort past the
       bracket. *)
    | Border_side_width_bracket (side, _, _) -> side_band side + 30
    | Border_side_width (side, n) -> side_band side + 1 + n
    | Border_t -> side_band "t"
    | Border_r -> side_band "r"
    | Border_b -> side_band "b"
    | Border_l -> side_band "l"
    (* Border style utilities (1400-1499) - alphabetical *)
    | Border_dashed -> 1400
    | Border_dotted -> 1401
    | Border_double -> 1402
    | Border_hidden -> 1403
    | Border_none -> 1404
    | Border_solid -> 1405
    (* Outline utilities — hidden first, then width, then offset. The colors
       (color.ml, 3000-28000) and the styles (Outline_style_handler,
       30000-30004) close the family. *)
    | Outline_hidden -> 1990
    | Outline -> 1999
    | Outline_0 -> 2000
    | Outline_width n -> 2000 + n
    | Outline_width_bracket _ -> 2009
    | Outline_width_var _ -> 2010
    (* Outline offset — negatives before positives *)
    | Neg_outline_offset n -> 2200 + n
    | Neg_outline_offset_var _ -> 2209
    | Outline_offset n -> 2210 + n
    | Outline_offset_var _ -> 2299
    | Outline_offset_arbitrary _ -> 2215

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "border" ] -> Ok Border
    | [ "border"; "0" ] -> Ok Border_0
    | [ "border"; "2" ] -> Ok Border_2
    | [ "border"; "4" ] -> Ok Border_4
    | [ "border"; "8" ] -> Ok Border_8
    | [ "border"; n ]
      when match Parse.decimal_int n with Some w -> w > 0 | None -> false ->
        Ok (Border_width (int_of_string n))
    | [ "border"; "t" ] -> Ok Border_t
    | [ "border"; "r" ] -> Ok Border_r
    | [ "border"; "b" ] -> Ok Border_b
    | [ "border"; "l" ] -> Ok Border_l
    | [ "border"; "x" ] -> Ok Border_x
    | [ "border"; "y" ] -> Ok Border_y
    | [ "border"; "x"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_x_width (int_of_string n))
    | [ "border"; "y"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_y_width (int_of_string n))
    | [ "border"; "s" ] -> Ok Border_s
    | [ "border"; "e" ] -> Ok Border_e
    | [ "border"; "bs" ] -> Ok Border_bs
    | [ "border"; "be" ] -> Ok Border_be
    | [ "border"; "s"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_s_width (int_of_string n))
    | [ "border"; "e"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_e_width (int_of_string n))
    | [ "border"; "bs"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_bs_width (int_of_string n))
    | [ "border"; "be"; n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_be_width (int_of_string n))
    | [ "border"; (("t" | "r" | "b" | "l") as side); n ]
      when match Parse.decimal_int n with Some w -> w >= 0 | None -> false ->
        Ok (Border_side_width (side, int_of_string n))
    | [ "border"; "solid" ] -> Ok Border_solid
    | [ "border"; "dashed" ] -> Ok Border_dashed
    | [ "border"; "dotted" ] -> Ok Border_dotted
    | [ "border"; "double" ] -> Ok Border_double
    | [ "border"; "hidden" ] -> Ok Border_hidden
    | [ "border"; "none" ] -> Ok Border_none
    | [ "border"; v ] when Parse.is_bracket_value v ->
        let inner = Parse.bracket_inner v in
        let is_numeric_start c = (c >= '0' && c <= '9') || c = '.' || c = '-' in
        let is_width =
          (String.length inner > 0 && is_numeric_start inner.[0])
          || Parse.starts_with_math_function inner
          || is_line_width_keyword inner
        in
        if is_width then
          match parse_border_width inner with
          | Some w -> Ok (Border_width_bracket (inner, w))
          | None -> err_not_utility
        else err_not_utility
    | [ "border"; side; v ]
      when (match side with "t" | "r" | "b" | "l" -> true | _ -> false)
           && Parse.is_bracket_value v ->
        let inner = Parse.bracket_inner v in
        let is_numeric_start c = (c >= '0' && c <= '9') || c = '.' in
        let is_width =
          (String.length inner > 0 && is_numeric_start inner.[0])
          || Parse.starts_with_math_function inner
          || is_line_width_keyword inner
        in
        if is_width then
          match parse_border_width inner with
          | Some w -> Ok (Border_side_width_bracket (side, inner, w))
          | None -> err_not_utility
        else err_not_utility
    (* Border radius utilities (parametric). [rounded] / [rounded-<size>] target
       all corners; [rounded-<pos>] / [rounded-<pos>-<size>] target a side or
       corner. Sizes and positions are disjoint token sets. *)
    | [ "rounded" ] -> Ok (Rounded (Corner.All, Rsz_default))
    | [ "rounded"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        match parse_length inner with
        | Some len -> Ok (Rounded (Corner.All, Rsz_arbitrary (inner, len)))
        | None -> err_not_utility)
    | [ "rounded"; tok ] -> (
        match rounded_size_of_string tok with
        | Some size -> Ok (Rounded (Corner.All, size))
        | None -> (
            match corner_of_string tok with
            | Some pos -> Ok (Rounded (pos, Rsz_default))
            | None ->
                if is_theme_radius theme tok then
                  Ok (Rounded (Corner.All, Rsz_theme tok))
                else err_not_utility))
    | [ "rounded"; pos; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        match (corner_of_string pos, parse_length inner) with
        | Some pos, Some len -> Ok (Rounded (pos, Rsz_arbitrary (inner, len)))
        | _ -> err_not_utility)
    | [ "rounded"; pos; size ] -> (
        match (corner_of_string pos, rounded_size_of_string size) with
        | Some pos, Some size -> Ok (Rounded (pos, size))
        | Some pos, None when is_theme_radius theme size ->
            Ok (Rounded (pos, Rsz_theme size))
        | _ -> err_not_utility)
    | [ "outline" ] -> Ok Outline
    | [ "outline"; "0" ] -> Ok Outline_0
    | [ "outline"; n ]
      when match Parse.decimal_int n with Some w -> w > 0 | None -> false ->
        Ok (Outline_width (int_of_string n))
    | [ "outline"; v ] when Parse.is_bracket_value v ->
        let inner = Parse.bracket_inner v in
        let starts prefix s =
          String.length s >= String.length prefix
          && String.sub s 0 (String.length prefix) = prefix
        in
        if starts "length:" inner then Ok (Outline_width_var inner)
        else if starts "number:" inner then Ok (Outline_width_var inner)
        else if starts "percentage:" inner then Ok (Outline_width_var inner)
        else if starts "var(" inner then
          (* Bare var() defaults to color for outline, not width *)
          err_not_utility
        else if starts "color:" inner then err_not_utility
        else if starts "#" inner then err_not_utility
        else
          (* Only accept if it looks like a number/length/percentage, not a
             named color like "black" *)
          let is_numeric_start c =
            (c >= '0' && c <= '9') || c = '.' || c = '-'
          in
          if
            (String.length inner > 0 && is_numeric_start inner.[0])
            || Parse.starts_with_math_function inner
          then
            match parse_outline_width inner with
            | Some len -> Ok (Outline_width_bracket (inner, len))
            | None -> err_not_utility
          else err_not_utility
    (* outline-none/solid/dashed/dotted/double handled by
       Outline_style_handler *)
    | [ "outline"; "hidden" ] -> Ok Outline_hidden
    | [ "outline"; "offset"; v ] when Parse.is_bracket_value v ->
        let inner = Parse.bracket_inner v in
        if Parse.is_var inner then Ok (Outline_offset_var inner)
        else if parse_length inner <> None then
          Ok (Outline_offset_arbitrary inner)
        else err_not_utility
    | [ "outline"; "offset"; n ] -> (
        match Parse.decimal_int n with
        | Some i when i >= 0 -> Ok (Outline_offset i)
        | _ -> err_not_utility)
    (* Negative outline offset: -outline-offset-N starts with empty string *)
    | "" :: "outline" :: "offset" :: rest when rest <> [] -> (
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then
          let inner = Parse.bracket_inner value in
          if Parse.is_var inner then Ok (Neg_outline_offset_var inner)
          else err_not_utility
        else
          match Parse.decimal_int value with
          | Some i when i > 0 -> Ok (Neg_outline_offset i)
          | _ -> err_not_utility)
    (* ring* handled in Effects *)
    | _ -> err_not_utility

  let to_class = function
    | Border -> "border"
    | Border_0 -> "border-0"
    | Border_2 -> "border-2"
    | Border_4 -> "border-4"
    | Border_8 -> "border-8"
    | Border_width n -> "border-" ^ string_of_int n
    | Border_width_bracket (v, _) -> "border-[" ^ v ^ "]"
    | Border_side_width_bracket (side, inner, _) ->
        "border-" ^ side ^ "-[" ^ inner ^ "]"
    | Border_side_width (side, n) -> "border-" ^ side ^ "-" ^ string_of_int n
    | Border_t -> "border-t"
    | Border_r -> "border-r"
    | Border_b -> "border-b"
    | Border_l -> "border-l"
    | Border_x -> "border-x"
    | Border_y -> "border-y"
    | Border_x_width n -> "border-x-" ^ string_of_int n
    | Border_y_width n -> "border-y-" ^ string_of_int n
    | Border_s -> "border-s"
    | Border_e -> "border-e"
    | Border_bs -> "border-bs"
    | Border_be -> "border-be"
    | Border_s_width n -> "border-s-" ^ string_of_int n
    | Border_e_width n -> "border-e-" ^ string_of_int n
    | Border_bs_width n -> "border-bs-" ^ string_of_int n
    | Border_be_width n -> "border-be-" ^ string_of_int n
    | Border_solid -> "border-solid"
    | Border_dashed -> "border-dashed"
    | Border_dotted -> "border-dotted"
    | Border_double -> "border-double"
    | Border_hidden -> "border-hidden"
    | Border_none -> "border-none"
    | Rounded (pos, size) ->
        let pos_str =
          match pos with
          | Corner.All -> ""
          | Corner.Start -> "-s"
          | Corner.End -> "-e"
          | Corner.Top -> "-t"
          | Corner.Right -> "-r"
          | Corner.Bottom -> "-b"
          | Corner.Left -> "-l"
          | Corner.Start_start -> "-ss"
          | Corner.Start_end -> "-se"
          | Corner.End_end -> "-ee"
          | Corner.End_start -> "-es"
          | Corner.Top_left -> "-tl"
          | Corner.Top_right -> "-tr"
          | Corner.Bottom_right -> "-br"
          | Corner.Bottom_left -> "-bl"
        in
        let size_str =
          match size with
          | Rsz_default -> ""
          | Rsz_none -> "-none"
          | Rsz_xs -> "-xs"
          | Rsz_sm -> "-sm"
          | Rsz_md -> "-md"
          | Rsz_lg -> "-lg"
          | Rsz_xl -> "-xl"
          | Rsz_2xl -> "-2xl"
          | Rsz_3xl -> "-3xl"
          | Rsz_4xl -> "-4xl"
          | Rsz_full -> "-full"
          | Rsz_theme name -> "-" ^ name
          | Rsz_arbitrary (raw, _) -> "-[" ^ raw ^ "]"
        in
        "rounded" ^ pos_str ^ size_str
    | Outline -> "outline"
    | Outline_0 -> "outline-0"
    | Outline_width n -> "outline-" ^ string_of_int n
    | Outline_width_bracket (v, _) -> "outline-[" ^ v ^ "]"
    | Outline_width_var v -> "outline-[" ^ v ^ "]"
    | Outline_hidden -> "outline-hidden"
    | Outline_offset n -> "outline-offset-" ^ string_of_int n
    | Outline_offset_var v -> "outline-offset-[" ^ v ^ "]"
    | Outline_offset_arbitrary v -> "outline-offset-[" ^ v ^ "]"
    | Neg_outline_offset n -> "-outline-offset-" ^ string_of_int n
    | Neg_outline_offset_var v -> "-outline-offset-[" ^ v ^ "]"

  let examples =
    [
      Border_0;
      Border_x;
      Border_y;
      Border_solid;
      Outline_width 2;
      Outline_offset 2;
      Rounded (Corner.All, Rsz_sm);
    ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)

(** Separate handler for outline style utilities that need to come after outline
    colors (priority 23) AND after ring/shadow effects (priority 25). These are:
    outline-dashed, outline-dotted, outline-double, outline-none, outline-solid.
*)
module Outline_style_handler = struct
  open Style

  type t = Dashed | Dotted | Double | None_ | Solid
  type Utility.base += Self of t

  let name = "outline_style"
  let priority _ = 28

  let to_style _theme = function
    | Dashed ->
        let decl, _ = Var.binding Handler.outline_style_var Css.Dashed in
        style [ decl; Css.outline_style Css.Dashed ]
    | Dotted ->
        let decl, _ = Var.binding Handler.outline_style_var Css.Dotted in
        style [ decl; Css.outline_style Css.Dotted ]
    | Double ->
        let decl, _ = Var.binding Handler.outline_style_var Css.Double in
        style [ decl; Css.outline_style Css.Double ]
    | None_ ->
        let decl, _ = Var.binding Handler.outline_style_var Css.None in
        style [ decl; Css.outline_style Css.None ]
    | Solid ->
        let decl, _ = Var.binding Handler.outline_style_var Css.Solid in
        style [ decl; Css.outline_style Css.Solid ]

  (* outline-style closes the outline family at priority 28, after the widths
     and offsets here and after color.ml's outline colors (3000-28000);
     alphabetical: dashed, dotted, double, none, solid. *)
  let suborder = function
    | Dashed -> 30000
    | Dotted -> 30001
    | Double -> 30002
    | None_ -> 30003
    | Solid -> 30004

  let err_not_utility = Error (`Msg "Not an outline style utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "outline"; "dashed" ] -> Ok Dashed
    | [ "outline"; "dotted" ] -> Ok Dotted
    | [ "outline"; "double" ] -> Ok Double
    | [ "outline"; "none" ] -> Ok None_
    | [ "outline"; "solid" ] -> Ok Solid
    | _ -> err_not_utility

  let to_class = function
    | Dashed -> "outline-dashed"
    | Dotted -> "outline-dotted"
    | Double -> "outline-double"
    | None_ -> "outline-none"
    | Solid -> "outline-solid"

  let examples = [ Solid ]
end

let () = Utility.register (module Outline_style_handler)
let outline_style_utility x = Utility.base (Outline_style_handler.Self x)

(** {1 Border Width Utilities} *)

let border = utility Border
let border_0 = utility Border_0
let border_2 = utility Border_2
let border_4 = utility Border_4
let border_8 = utility Border_8
let border_t = utility Border_t
let border_r = utility Border_r
let border_b = utility Border_b
let border_l = utility Border_l
let border_x = utility Border_x
let border_y = utility Border_y

(** {1 Border Style Utilities} *)

let border_solid = utility Border_solid
let border_dashed = utility Border_dashed
let border_dotted = utility Border_dotted
let border_double = utility Border_double
let border_hidden = utility Border_hidden
let border_none = utility Border_none

(** {1 Border Color Utilities} *)

(* Border width utilities with semantic names matching tw.mli *)
let border_xs = border (* 1px *)
let border_sm = border_2 (* 2px *)
let border_md = border_4 (* 4px *)
let border_lg = border_4 (* 4px *)
let border_xl = border_8 (* 8px *)
let border_2xl = border_8 (* 8px *)
let border_3xl = border_8 (* 8px *)
let border_full = border_8 (* 8px *)

(** {1 Border Radius Utilities} *)

let rounded = utility (Rounded (Corner.All, Rsz_default))
let rounded_none = utility (Rounded (Corner.All, Rsz_none))
let rounded_xs = utility (Rounded (Corner.All, Rsz_xs))
let rounded_sm = utility (Rounded (Corner.All, Rsz_sm))
let rounded_md = utility (Rounded (Corner.All, Rsz_md))
let rounded_lg = utility (Rounded (Corner.All, Rsz_lg))
let rounded_xl = utility (Rounded (Corner.All, Rsz_xl))
let rounded_2xl = utility (Rounded (Corner.All, Rsz_2xl))
let rounded_3xl = utility (Rounded (Corner.All, Rsz_3xl))
let rounded_4xl = utility (Rounded (Corner.All, Rsz_4xl))
let rounded_full = utility (Rounded (Corner.All, Rsz_full))

(** {2 Side-specific rounded utilities - top} *)

let rounded_t = utility (Rounded (Corner.Top, Rsz_default))
let rounded_t_none = utility (Rounded (Corner.Top, Rsz_none))
let rounded_t_xs = utility (Rounded (Corner.Top, Rsz_xs))
let rounded_t_sm = utility (Rounded (Corner.Top, Rsz_sm))
let rounded_t_md = utility (Rounded (Corner.Top, Rsz_md))
let rounded_t_lg = utility (Rounded (Corner.Top, Rsz_lg))
let rounded_t_xl = utility (Rounded (Corner.Top, Rsz_xl))
let rounded_t_2xl = utility (Rounded (Corner.Top, Rsz_2xl))
let rounded_t_3xl = utility (Rounded (Corner.Top, Rsz_3xl))
let rounded_t_4xl = utility (Rounded (Corner.Top, Rsz_4xl))
let rounded_t_full = utility (Rounded (Corner.Top, Rsz_full))

(** {2 Side-specific rounded utilities - right} *)

let rounded_r = utility (Rounded (Corner.Right, Rsz_default))
let rounded_r_none = utility (Rounded (Corner.Right, Rsz_none))
let rounded_r_xs = utility (Rounded (Corner.Right, Rsz_xs))
let rounded_r_sm = utility (Rounded (Corner.Right, Rsz_sm))
let rounded_r_md = utility (Rounded (Corner.Right, Rsz_md))
let rounded_r_lg = utility (Rounded (Corner.Right, Rsz_lg))
let rounded_r_xl = utility (Rounded (Corner.Right, Rsz_xl))
let rounded_r_2xl = utility (Rounded (Corner.Right, Rsz_2xl))
let rounded_r_3xl = utility (Rounded (Corner.Right, Rsz_3xl))
let rounded_r_4xl = utility (Rounded (Corner.Right, Rsz_4xl))
let rounded_r_full = utility (Rounded (Corner.Right, Rsz_full))

(** {2 Side-specific rounded utilities - bottom} *)

let rounded_b = utility (Rounded (Corner.Bottom, Rsz_default))
let rounded_b_none = utility (Rounded (Corner.Bottom, Rsz_none))
let rounded_b_xs = utility (Rounded (Corner.Bottom, Rsz_xs))
let rounded_b_sm = utility (Rounded (Corner.Bottom, Rsz_sm))
let rounded_b_md = utility (Rounded (Corner.Bottom, Rsz_md))
let rounded_b_lg = utility (Rounded (Corner.Bottom, Rsz_lg))
let rounded_b_xl = utility (Rounded (Corner.Bottom, Rsz_xl))
let rounded_b_2xl = utility (Rounded (Corner.Bottom, Rsz_2xl))
let rounded_b_3xl = utility (Rounded (Corner.Bottom, Rsz_3xl))
let rounded_b_4xl = utility (Rounded (Corner.Bottom, Rsz_4xl))
let rounded_b_full = utility (Rounded (Corner.Bottom, Rsz_full))

(** {2 Side-specific rounded utilities - left} *)

let rounded_l = utility (Rounded (Corner.Left, Rsz_default))
let rounded_l_none = utility (Rounded (Corner.Left, Rsz_none))
let rounded_l_xs = utility (Rounded (Corner.Left, Rsz_xs))
let rounded_l_sm = utility (Rounded (Corner.Left, Rsz_sm))
let rounded_l_md = utility (Rounded (Corner.Left, Rsz_md))
let rounded_l_lg = utility (Rounded (Corner.Left, Rsz_lg))
let rounded_l_xl = utility (Rounded (Corner.Left, Rsz_xl))
let rounded_l_2xl = utility (Rounded (Corner.Left, Rsz_2xl))
let rounded_l_3xl = utility (Rounded (Corner.Left, Rsz_3xl))
let rounded_l_4xl = utility (Rounded (Corner.Left, Rsz_4xl))
let rounded_l_full = utility (Rounded (Corner.Left, Rsz_full))

(** {2 Corner-specific rounded utilities - top-left} *)

let rounded_tl = utility (Rounded (Corner.Top_left, Rsz_default))
let rounded_tl_none = utility (Rounded (Corner.Top_left, Rsz_none))
let rounded_tl_xs = utility (Rounded (Corner.Top_left, Rsz_xs))
let rounded_tl_sm = utility (Rounded (Corner.Top_left, Rsz_sm))
let rounded_tl_md = utility (Rounded (Corner.Top_left, Rsz_md))
let rounded_tl_lg = utility (Rounded (Corner.Top_left, Rsz_lg))
let rounded_tl_xl = utility (Rounded (Corner.Top_left, Rsz_xl))
let rounded_tl_2xl = utility (Rounded (Corner.Top_left, Rsz_2xl))
let rounded_tl_3xl = utility (Rounded (Corner.Top_left, Rsz_3xl))
let rounded_tl_4xl = utility (Rounded (Corner.Top_left, Rsz_4xl))
let rounded_tl_full = utility (Rounded (Corner.Top_left, Rsz_full))

(** {2 Corner-specific rounded utilities - top-right} *)

let rounded_tr = utility (Rounded (Corner.Top_right, Rsz_default))
let rounded_tr_none = utility (Rounded (Corner.Top_right, Rsz_none))
let rounded_tr_xs = utility (Rounded (Corner.Top_right, Rsz_xs))
let rounded_tr_sm = utility (Rounded (Corner.Top_right, Rsz_sm))
let rounded_tr_md = utility (Rounded (Corner.Top_right, Rsz_md))
let rounded_tr_lg = utility (Rounded (Corner.Top_right, Rsz_lg))
let rounded_tr_xl = utility (Rounded (Corner.Top_right, Rsz_xl))
let rounded_tr_2xl = utility (Rounded (Corner.Top_right, Rsz_2xl))
let rounded_tr_3xl = utility (Rounded (Corner.Top_right, Rsz_3xl))
let rounded_tr_4xl = utility (Rounded (Corner.Top_right, Rsz_4xl))
let rounded_tr_full = utility (Rounded (Corner.Top_right, Rsz_full))

(** {2 Corner-specific rounded utilities - bottom-right} *)

let rounded_br = utility (Rounded (Corner.Bottom_right, Rsz_default))
let rounded_br_none = utility (Rounded (Corner.Bottom_right, Rsz_none))
let rounded_br_xs = utility (Rounded (Corner.Bottom_right, Rsz_xs))
let rounded_br_sm = utility (Rounded (Corner.Bottom_right, Rsz_sm))
let rounded_br_md = utility (Rounded (Corner.Bottom_right, Rsz_md))
let rounded_br_lg = utility (Rounded (Corner.Bottom_right, Rsz_lg))
let rounded_br_xl = utility (Rounded (Corner.Bottom_right, Rsz_xl))
let rounded_br_2xl = utility (Rounded (Corner.Bottom_right, Rsz_2xl))
let rounded_br_3xl = utility (Rounded (Corner.Bottom_right, Rsz_3xl))
let rounded_br_4xl = utility (Rounded (Corner.Bottom_right, Rsz_4xl))
let rounded_br_full = utility (Rounded (Corner.Bottom_right, Rsz_full))

(** {2 Corner-specific rounded utilities - bottom-left} *)

let rounded_bl = utility (Rounded (Corner.Bottom_left, Rsz_default))
let rounded_bl_none = utility (Rounded (Corner.Bottom_left, Rsz_none))
let rounded_bl_xs = utility (Rounded (Corner.Bottom_left, Rsz_xs))
let rounded_bl_sm = utility (Rounded (Corner.Bottom_left, Rsz_sm))
let rounded_bl_md = utility (Rounded (Corner.Bottom_left, Rsz_md))
let rounded_bl_lg = utility (Rounded (Corner.Bottom_left, Rsz_lg))
let rounded_bl_xl = utility (Rounded (Corner.Bottom_left, Rsz_xl))
let rounded_bl_2xl = utility (Rounded (Corner.Bottom_left, Rsz_2xl))
let rounded_bl_3xl = utility (Rounded (Corner.Bottom_left, Rsz_3xl))
let rounded_bl_4xl = utility (Rounded (Corner.Bottom_left, Rsz_4xl))
let rounded_bl_full = utility (Rounded (Corner.Bottom_left, Rsz_full))

(** {2 Logical property rounded utilities - start} *)

let rounded_s = utility (Rounded (Corner.Start, Rsz_default))
let rounded_s_none = utility (Rounded (Corner.Start, Rsz_none))
let rounded_s_xs = utility (Rounded (Corner.Start, Rsz_xs))
let rounded_s_sm = utility (Rounded (Corner.Start, Rsz_sm))
let rounded_s_md = utility (Rounded (Corner.Start, Rsz_md))
let rounded_s_lg = utility (Rounded (Corner.Start, Rsz_lg))
let rounded_s_xl = utility (Rounded (Corner.Start, Rsz_xl))
let rounded_s_2xl = utility (Rounded (Corner.Start, Rsz_2xl))
let rounded_s_3xl = utility (Rounded (Corner.Start, Rsz_3xl))
let rounded_s_4xl = utility (Rounded (Corner.Start, Rsz_4xl))
let rounded_s_full = utility (Rounded (Corner.Start, Rsz_full))

(** {2 Logical property rounded utilities - end} *)

let rounded_e = utility (Rounded (Corner.End, Rsz_default))
let rounded_e_none = utility (Rounded (Corner.End, Rsz_none))
let rounded_e_xs = utility (Rounded (Corner.End, Rsz_xs))
let rounded_e_sm = utility (Rounded (Corner.End, Rsz_sm))
let rounded_e_md = utility (Rounded (Corner.End, Rsz_md))
let rounded_e_lg = utility (Rounded (Corner.End, Rsz_lg))
let rounded_e_xl = utility (Rounded (Corner.End, Rsz_xl))
let rounded_e_2xl = utility (Rounded (Corner.End, Rsz_2xl))
let rounded_e_3xl = utility (Rounded (Corner.End, Rsz_3xl))
let rounded_e_4xl = utility (Rounded (Corner.End, Rsz_4xl))
let rounded_e_full = utility (Rounded (Corner.End, Rsz_full))

(** {2 Logical corner rounded utilities - start-start} *)

let rounded_ss = utility (Rounded (Corner.Start_start, Rsz_default))
let rounded_ss_none = utility (Rounded (Corner.Start_start, Rsz_none))
let rounded_ss_xs = utility (Rounded (Corner.Start_start, Rsz_xs))
let rounded_ss_sm = utility (Rounded (Corner.Start_start, Rsz_sm))
let rounded_ss_md = utility (Rounded (Corner.Start_start, Rsz_md))
let rounded_ss_lg = utility (Rounded (Corner.Start_start, Rsz_lg))
let rounded_ss_xl = utility (Rounded (Corner.Start_start, Rsz_xl))
let rounded_ss_2xl = utility (Rounded (Corner.Start_start, Rsz_2xl))
let rounded_ss_3xl = utility (Rounded (Corner.Start_start, Rsz_3xl))
let rounded_ss_4xl = utility (Rounded (Corner.Start_start, Rsz_4xl))
let rounded_ss_full = utility (Rounded (Corner.Start_start, Rsz_full))

(** {2 Logical corner rounded utilities - start-end} *)

let rounded_se = utility (Rounded (Corner.Start_end, Rsz_default))
let rounded_se_none = utility (Rounded (Corner.Start_end, Rsz_none))
let rounded_se_xs = utility (Rounded (Corner.Start_end, Rsz_xs))
let rounded_se_sm = utility (Rounded (Corner.Start_end, Rsz_sm))
let rounded_se_md = utility (Rounded (Corner.Start_end, Rsz_md))
let rounded_se_lg = utility (Rounded (Corner.Start_end, Rsz_lg))
let rounded_se_xl = utility (Rounded (Corner.Start_end, Rsz_xl))
let rounded_se_2xl = utility (Rounded (Corner.Start_end, Rsz_2xl))
let rounded_se_3xl = utility (Rounded (Corner.Start_end, Rsz_3xl))
let rounded_se_4xl = utility (Rounded (Corner.Start_end, Rsz_4xl))
let rounded_se_full = utility (Rounded (Corner.Start_end, Rsz_full))

(** {2 Logical corner rounded utilities - end-end} *)

let rounded_ee = utility (Rounded (Corner.End_end, Rsz_default))
let rounded_ee_none = utility (Rounded (Corner.End_end, Rsz_none))
let rounded_ee_xs = utility (Rounded (Corner.End_end, Rsz_xs))
let rounded_ee_sm = utility (Rounded (Corner.End_end, Rsz_sm))
let rounded_ee_md = utility (Rounded (Corner.End_end, Rsz_md))
let rounded_ee_lg = utility (Rounded (Corner.End_end, Rsz_lg))
let rounded_ee_xl = utility (Rounded (Corner.End_end, Rsz_xl))
let rounded_ee_2xl = utility (Rounded (Corner.End_end, Rsz_2xl))
let rounded_ee_3xl = utility (Rounded (Corner.End_end, Rsz_3xl))
let rounded_ee_4xl = utility (Rounded (Corner.End_end, Rsz_4xl))
let rounded_ee_full = utility (Rounded (Corner.End_end, Rsz_full))

(** {2 Logical corner rounded utilities - end-start} *)

let rounded_es = utility (Rounded (Corner.End_start, Rsz_default))
let rounded_es_none = utility (Rounded (Corner.End_start, Rsz_none))
let rounded_es_xs = utility (Rounded (Corner.End_start, Rsz_xs))
let rounded_es_sm = utility (Rounded (Corner.End_start, Rsz_sm))
let rounded_es_md = utility (Rounded (Corner.End_start, Rsz_md))
let rounded_es_lg = utility (Rounded (Corner.End_start, Rsz_lg))
let rounded_es_xl = utility (Rounded (Corner.End_start, Rsz_xl))
let rounded_es_2xl = utility (Rounded (Corner.End_start, Rsz_2xl))
let rounded_es_3xl = utility (Rounded (Corner.End_start, Rsz_3xl))
let rounded_es_4xl = utility (Rounded (Corner.End_start, Rsz_4xl))
let rounded_es_full = utility (Rounded (Corner.End_start, Rsz_full))

(** {1 Outline Utilities} *)

let outline = utility Outline
let outline_none = outline_style_utility Outline_style_handler.None_
let outline_offset_0 = utility (Outline_offset 0)
let outline_offset_1 = utility (Outline_offset 1)
let outline_offset_2 = utility (Outline_offset 2)
let outline_offset_4 = utility (Outline_offset 4)
let outline_offset_8 = utility (Outline_offset 8)
let border_style_var = Handler.border_style_var
let parse_border_width = Handler.parse_border_width
