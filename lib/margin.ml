(** Margin utilities with negative value support. *)

module Handler = struct
  open Style
  open Css

  type t = {
    negative : bool;
    axis : [ `All | `X | `Y | `T | `R | `B | `L ];
    value : margin;
  }
  (** Local margin utility type *)

  (** Extensible variant for margin utilities *)
  type Utility.base += Self of t

  let name = "margin"
  let priority = 2

  (** {2 Typed Margin Utilities} *)

  let v prop (m : margin) =
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style [ prop len ]
    | #spacing -> style [ spacing_decl; prop len ]

  let vs prop (m : margin) =
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style [ prop [ len ] ]
    | #spacing -> style [ spacing_decl; prop [ len ] ]

  let m = vs margin
  let mx = v margin_inline
  let my = v margin_block
  let mt = v margin_top
  let mr = v margin_right
  let mb = v margin_bottom
  let ml = v margin_left

  (** {1 Conversion Functions} *)

  let margin_util_neg prop (m : margin) =
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style [ prop len ]
    | #spacing -> style [ spacing_decl; prop len ]

  let margin_list_util_neg prop (m : margin) =
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style [ prop [ len ] ]
    | #spacing -> style [ spacing_decl; prop [ len ] ]

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  let margin_value_order = function
    | `Auto -> 0
    | #spacing as s -> spacing_value_order s

  (** Convert margin utility to style *)
  let to_style { negative; axis; value } =
    let abs_value =
      match value with `Rem f -> `Rem (Float.abs f) | other -> other
    in
    match (negative, axis, abs_value) with
    | false, `All, _ -> m abs_value
    | false, `X, _ -> mx abs_value
    | false, `Y, _ -> my abs_value
    | false, `T, _ -> mt abs_value
    | false, `R, _ -> mr abs_value
    | false, `B, _ -> mb abs_value
    | false, `L, _ -> ml abs_value
    | true, `All, (#spacing as s) -> margin_list_util_neg margin s
    | true, `X, (#spacing as s) -> margin_util_neg margin_inline s
    | true, `Y, (#spacing as s) -> margin_util_neg margin_block s
    | true, `T, (#spacing as s) -> margin_util_neg margin_top s
    | true, `R, (#spacing as s) -> margin_util_neg margin_right s
    | true, `B, (#spacing as s) -> margin_util_neg margin_bottom s
    | true, `L, (#spacing as s) -> margin_util_neg margin_left s
    | true, _, `Auto -> failwith "Negative auto margin not supported"

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
    in
    neg_offset + side_offset + margin_value_order value

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
    in
    let neg_prefix = if negative then "-" else "" in
    neg_prefix ^ prefix ^ Spacing.pp_margin_suffix value

  (** Parse string parts to margin utility using shared logic *)
  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match Spacing.parse_class_parts parts with
    | Some (is_negative, prefix, value) -> (
        if
          (* Only accept margin prefixes *)
          not (Spacing.is_margin_prefix prefix)
        then Error (`Msg "Not a margin utility")
        else
          match Spacing.axis_of_prefix prefix with
          | None -> Error (`Msg "Not a margin utility")
          | Some axis -> (
              let allow_auto = not is_negative in
              match Spacing.parse_value_string ~allow_auto value with
              | None -> Error (`Msg "Not a margin utility")
              | Some (#spacing as spacing_val) ->
                  Ok { negative = is_negative; axis; value = spacing_val }
              | Some `Auto ->
                  if is_negative then Error (`Msg "Not a margin utility")
                  else Ok { negative = false; axis; value = `Auto }))
    | None -> Error (`Msg "Not a margin utility")
end

open Handler

let () = Utility.register (module Handler)
let utility negative axis value = Utility.base (Self { negative; axis; value })

let v d n =
  let s = (Spacing.int n :> Style.margin) in
  let neg = n < 0 in
  utility neg d s

let m n = v `All n
let mx n = v `X n
let my n = v `Y n
let mt n = v `T n
let mr n = v `R n
let mb n = v `B n
let ml n = v `L n
let m_auto = utility false `All `Auto
let mx_auto = utility false `X `Auto
let my_auto = utility false `Y `Auto
let mt_auto = utility false `T `Auto
let mr_auto = utility false `R `Auto
let mb_auto = utility false `B `Auto
let ml_auto = utility false `L `Auto
