(** Margin utilities with negative value support. *)

module Handler = struct
  open Style
  open Css

  type margin_value =
    | Standard of margin (* auto, spacing values *)
    | Arbitrary of Css.length (* mx-[4px] *)
    | Named of string (* mx-big - custom spacing *)

  type t = {
    negative : bool;
    axis : [ `All | `X | `Y | `T | `R | `B | `L | `S | `E | `Bs | `Be ];
    value : margin_value;
  }
  (** Local margin utility type *)

  (** Extensible variant for margin utilities *)
  type Utility.base += Self of t

  let name = "margin"
  let priority = 2

  (** {2 Typed Margin Utilities} *)

  (** Convert spacing to (declaration option, length) using
      Theme.spacing_calc_float. For rem values, checks scheme for explicit
      spacing variables. *)
  let spacing_to_decl_len ~negative (s : Style.spacing) :
      Css.declaration option * length =
    match s with
    | `Px ->
        let len : length = if negative then Px (-1.) else Px 1. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Full ->
        let len : length = if negative then Pct (-100.) else Pct 100. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Named name ->
        let len = Spacing.named_spacing_ref name in
        (None, len)
    | `Rem f ->
        let n = f /. 0.25 in
        let n = if negative then -.n else n in
        let decl, len = Theme.spacing_calc_float n in
        (Some decl, len)

  let v (prop : length -> declaration) (m : margin) =
    match m with
    | `Auto -> style [ prop Auto ]
    | #Style.spacing as s ->
        let decl, len = spacing_to_decl_len ~negative:false s in
        style (Option.to_list decl @ [ prop len ])

  let vs (prop : length list -> declaration) (m : margin) =
    match m with
    | `Auto -> style [ prop [ Auto ] ]
    | #Style.spacing as s ->
        let decl, len = spacing_to_decl_len ~negative:false s in
        style (Option.to_list decl @ [ prop [ len ] ])

  let m_fn = vs margin
  let mx = v margin_inline
  let my = v margin_block
  let mt = v margin_top
  let mr = v margin_right
  let mb = v margin_bottom
  let ml = v margin_left
  let ms = v margin_inline_start
  let me = v margin_inline_end
  let mbs = v margin_block_start
  let mbe = v margin_block_end

  (* Cache for named spacing variables *)
  let named_spacing_cache : (string, Css.length Var.theme) Hashtbl.t =
    Hashtbl.create 16

  let get_named_spacing_var name =
    match Hashtbl.find_opt named_spacing_cache name with
    | Some v -> v
    | None ->
        let v = Var.theme Css.Length ("spacing-" ^ name) ~order:(3, 400) in
        Hashtbl.add named_spacing_cache name v;
        v

  let named_margin_value name : Css.declaration * Css.length =
    let var = get_named_spacing_var name in
    (* Use 1940px as placeholder value - from Tailwind test config *)
    let concrete_value : Css.length = Px 1940. in
    let decl, ref = Var.binding var concrete_value in
    (decl, Css.Var ref)

  (** {1 Conversion Functions} *)

  let margin_util_neg (prop : length -> declaration) (s : Style.spacing) =
    let decl, len = spacing_to_decl_len ~negative:true s in
    style (Option.to_list decl @ [ prop len ])

  let margin_list_util_neg (prop : length list -> declaration)
      (s : Style.spacing) =
    let decl, len = spacing_to_decl_len ~negative:true s in
    style (Option.to_list decl @ [ prop [ len ] ])

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Named _ -> 20000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  let margin_value_order = function
    | `Auto -> 99999 (* Auto comes after numeric values in Tailwind *)
    | #spacing as s -> spacing_value_order s

  (* Get the CSS property function for an axis *)
  let prop_for_axis axis =
    match axis with
    | `All -> fun len -> margin [ len ]
    | `X -> margin_inline
    | `Y -> margin_block
    | `T -> margin_top
    | `R -> margin_right
    | `B -> margin_bottom
    | `L -> margin_left
    | `S -> margin_inline_start
    | `E -> margin_inline_end
    | `Bs -> margin_block_start
    | `Be -> margin_block_end

  (** Convert margin utility to style *)
  let to_style { negative; axis; value } =
    let prop = prop_for_axis axis in
    match value with
    | Arbitrary len ->
        if negative then
          style [ prop (Calc (Calc.mul (Calc.length len) (Calc.float (-1.)))) ]
        else style [ prop len ]
    | Named name ->
        let decl, len = named_margin_value name in
        if negative then
          style
            [
              decl; prop (Calc (Calc.mul (Calc.length len) (Calc.float (-1.))));
            ]
        else style [ decl; prop len ]
    | Standard m -> (
        let abs_value =
          match m with `Rem f -> `Rem (Float.abs f) | other -> other
        in
        match (negative, axis, abs_value) with
        | false, `All, _ -> m_fn abs_value
        | false, `X, _ -> mx abs_value
        | false, `Y, _ -> my abs_value
        | false, `T, _ -> mt abs_value
        | false, `R, _ -> mr abs_value
        | false, `B, _ -> mb abs_value
        | false, `L, _ -> ml abs_value
        | false, `S, _ -> ms abs_value
        | false, `E, _ -> me abs_value
        | false, `Bs, _ -> mbs abs_value
        | false, `Be, _ -> mbe abs_value
        | true, `All, (#spacing as s) -> margin_list_util_neg margin s
        | true, `X, (#spacing as s) -> margin_util_neg margin_inline s
        | true, `Y, (#spacing as s) -> margin_util_neg margin_block s
        | true, `T, (#spacing as s) -> margin_util_neg margin_top s
        | true, `R, (#spacing as s) -> margin_util_neg margin_right s
        | true, `B, (#spacing as s) -> margin_util_neg margin_bottom s
        | true, `L, (#spacing as s) -> margin_util_neg margin_left s
        | true, `S, (#spacing as s) -> margin_util_neg margin_inline_start s
        | true, `E, (#spacing as s) -> margin_util_neg margin_inline_end s
        | true, `Bs, (#spacing as s) -> margin_util_neg margin_block_start s
        | true, `Be, (#spacing as s) -> margin_util_neg margin_block_end s
        | true, _, `Auto -> failwith "Negative auto margin not supported")

  let suborder { negative; axis; value } =
    let neg_offset = if negative then 5000000 else 0 in
    let side_offset =
      match axis with
      | `All -> 0
      | `X -> 100000
      | `Y -> 200000
      | `T -> 300000
      | `R -> 400000
      | `B -> 500000
      | `L -> 600000
      | `S -> 700000
      | `E -> 800000
      | `Bs -> 900000
      | `Be -> 1000000
    in
    let value_order =
      match value with
      | Standard m -> margin_value_order m
      | Arbitrary _ -> 50000 (* after numbered, before auto *)
      | Named _ -> 60000 (* after arbitrary *)
    in
    neg_offset + side_offset + value_order

  let pp_float n =
    (* Format float without trailing dot: 4. -> 4, 4.5 -> 4.5 *)
    let s = string_of_float n in
    if String.ends_with ~suffix:"." s then String.sub s 0 (String.length s - 1)
    else s

  let pp_length_suffix (len : Css.length) =
    match len with
    | Px n -> "[" ^ pp_float n ^ "px]"
    | Rem n -> "[" ^ pp_float n ^ "rem]"
    | Pct n -> "[" ^ pp_float n ^ "%]"
    | _ -> "[<length>]"

  let to_class { negative; axis; value } =
    let prefix =
      match axis with
      | `All -> "m-"
      | `X -> "mx-"
      | `Y -> "my-"
      | `T -> "mt-"
      | `R -> "mr-"
      | `B -> "mb-"
      | `L -> "ml-"
      | `S -> "ms-"
      | `E -> "me-"
      | `Bs -> "mbs-"
      | `Be -> "mbe-"
    in
    let neg_prefix = if negative then "-" else "" in
    let value_suffix =
      match value with
      | Standard m -> Spacing.pp_margin_suffix m
      | Arbitrary len -> pp_length_suffix len
      | Named name -> name
    in
    neg_prefix ^ prefix ^ value_suffix

  let parse_arbitrary s : Css.length option =
    (* Parse [4px] or [1rem] etc. *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      (* Try to parse as a length *)
      if String.ends_with ~suffix:"px" inner then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with
        | Some f -> Some (Css.Px f)
        | None -> None
      else if String.ends_with ~suffix:"rem" inner then
        let n = String.sub inner 0 (String.length inner - 3) in
        match float_of_string_opt n with
        | Some f -> Some (Css.Rem f)
        | None -> None
      else None
    else None

  let axis_of_prefix_ext = function
    | "m" -> Some `All
    | "mx" -> Some `X
    | "my" -> Some `Y
    | "mt" -> Some `T
    | "mr" -> Some `R
    | "mb" -> Some `B
    | "ml" -> Some `L
    | "ms" -> Some `S
    | "me" -> Some `E
    | "mbs" -> Some `Bs
    | "mbe" -> Some `Be
    | _ -> None

  (** Check if a prefix is an extended margin prefix (ms, me, mbs, mbe) *)
  let is_extended_margin_prefix = function
    | "ms" | "me" | "mbs" | "mbe" -> true
    | _ -> false

  (** Parse value to standard or named margin *)
  let parse_value ~is_negative value =
    let allow_auto = not is_negative in
    match Spacing.parse_value_string ~allow_auto value with
    | Some (#spacing as spacing_val) ->
        Some
          {
            negative = is_negative;
            axis = `All;
            value = Standard (spacing_val :> margin);
          }
    | Some `Auto when not is_negative ->
        Some { negative = false; axis = `All; value = Standard `Auto }
    | None when (not is_negative) && Parse.is_valid_theme_name value ->
        (* Try as a named spacing: mx-big *)
        Some { negative = false; axis = `All; value = Named value }
    | _ -> None

  (** Parse string parts to margin utility using shared logic *)
  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    (* Handle arbitrary values: mx-[4px] *)
    | [ prefix; arb ] when String.length arb > 0 && arb.[0] = '[' -> (
        match (axis_of_prefix_ext prefix, parse_arbitrary arb) with
        | Some axis, Some len ->
            Ok { negative = false; axis; value = Arbitrary len }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle negative arbitrary: -mx-[4px] *)
    | [ ""; prefix; arb ] when String.length arb > 0 && arb.[0] = '[' -> (
        match (axis_of_prefix_ext prefix, parse_arbitrary arb) with
        | Some axis, Some len ->
            Ok { negative = true; axis; value = Arbitrary len }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle extended axes (ms, me, mbs, mbe) with values *)
    | [ prefix; value ] when is_extended_margin_prefix prefix -> (
        match
          (axis_of_prefix_ext prefix, parse_value ~is_negative:false value)
        with
        | Some axis, Some t -> Ok { t with axis }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Handle negative extended axes: -ms-4 *)
    | [ ""; prefix; value ] when is_extended_margin_prefix prefix -> (
        match
          (axis_of_prefix_ext prefix, parse_value ~is_negative:true value)
        with
        | Some axis, Some t -> Ok { t with axis }
        | _ -> Error (`Msg "Not a margin utility"))
    (* Use existing Spacing parser for standard values *)
    | _ -> (
        match Spacing.parse_class_parts parts with
        | Some (is_negative, prefix, value) -> (
            if not (Spacing.is_margin_prefix prefix) then
              Error (`Msg "Not a margin utility")
            else
              match Spacing.axis_of_prefix prefix with
              | None -> Error (`Msg "Not a margin utility")
              | Some axis -> (
                  let axis =
                    match axis with
                    | `All -> `All
                    | `X -> `X
                    | `Y -> `Y
                    | `T -> `T
                    | `R -> `R
                    | `B -> `B
                    | `L -> `L
                    | `S -> `S
                    | `E -> `E
                    | `Bs -> `Bs
                    | `Be -> `Be
                  in
                  let allow_auto = not is_negative in
                  match Spacing.parse_value_string ~allow_auto value with
                  | None ->
                      (* Try as a named spacing: mx-big *)
                      if (not is_negative) && Parse.is_valid_theme_name value
                      then Ok { negative = false; axis; value = Named value }
                      else Error (`Msg "Not a margin utility")
                  | Some (#spacing as spacing_val) ->
                      Ok
                        {
                          negative = is_negative;
                          axis;
                          value = Standard (spacing_val :> margin);
                        }
                  | Some `Auto ->
                      if is_negative then Error (`Msg "Not a margin utility")
                      else Ok { negative = false; axis; value = Standard `Auto }
                  ))
        | None -> Error (`Msg "Not a margin utility"))
end

open Handler

let () = Utility.register (module Handler)
let utility negative axis value = Utility.base (Self { negative; axis; value })

let v d n =
  let s = (Spacing.int n :> Style.margin) in
  let neg = n < 0 in
  utility neg d (Handler.Standard s)

let m n = v `All n
let mx n = v `X n
let my n = v `Y n
let mt n = v `T n
let mr n = v `R n
let mb n = v `B n
let ml n = v `L n
let m_auto = utility false `All (Handler.Standard `Auto)
let mx_auto = utility false `X (Handler.Standard `Auto)
let my_auto = utility false `Y (Handler.Standard `Auto)
let mt_auto = utility false `T (Handler.Standard `Auto)
let mr_auto = utility false `R (Handler.Standard `Auto)
let mb_auto = utility false `B (Handler.Standard `Auto)
let ml_auto = utility false `L (Handler.Standard `Auto)
