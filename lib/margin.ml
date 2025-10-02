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

  let priority = 2

  (** {2 Typed Margin Utilities} *)

  let v prefix prop (m : margin) =
    let class_name = prefix ^ Spacing.pp_margin_suffix m in
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style class_name [ prop len ]
    | #spacing -> style class_name [ spacing_decl; prop len ]

  let vs prefix prop (m : margin) =
    let class_name = prefix ^ Spacing.pp_margin_suffix m in
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style class_name [ prop [ len ] ]
    | #spacing -> style class_name [ spacing_decl; prop [ len ] ]

  let m = vs "m-" margin
  let mx = v "mx-" margin_inline
  let my = v "my-" margin_block
  let mt = v "mt-" margin_top
  let mr = v "mr-" margin_right
  let mb = v "mb-" margin_bottom
  let ml = v "ml-" margin_left

  (** {1 Conversion Functions} *)

  let margin_util_neg prefix prop (m : margin) =
    let class_name = "-" ^ prefix ^ Spacing.pp_margin_suffix m in
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style class_name [ prop len ]
    | #spacing -> style class_name [ spacing_decl; prop len ]

  let margin_list_util_neg prefix prop (m : margin) =
    let class_name = "-" ^ prefix ^ Spacing.pp_margin_suffix m in
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.margin_to_length spacing_ref m in
    match m with
    | `Auto -> style class_name [ prop [ len ] ]
    | #spacing -> style class_name [ spacing_decl; prop [ len ] ]

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
    | true, `All, (#spacing as s) -> margin_list_util_neg "m-" margin s
    | true, `X, (#spacing as s) -> margin_util_neg "mx-" margin_inline s
    | true, `Y, (#spacing as s) -> margin_util_neg "my-" margin_block s
    | true, `T, (#spacing as s) -> margin_util_neg "mt-" margin_top s
    | true, `R, (#spacing as s) -> margin_util_neg "mr-" margin_right s
    | true, `B, (#spacing as s) -> margin_util_neg "mb-" margin_bottom s
    | true, `L, (#spacing as s) -> margin_util_neg "ml-" margin_left s
    | true, _, `Auto -> failwith "Negative auto margin not supported"

  let suborder { negative; axis; value } =
    let neg_offset = if negative then 5000 else 0 in
    let side_offset =
      match axis with
      | `All -> 0
      | `X -> 1000
      | `Y -> 2000
      | `T -> 3000
      | `R -> 4000
      | `B -> 5000
      | `L -> 6000
    in
    neg_offset + side_offset + margin_value_order value

  (** Parse string parts to margin utility *)
  let of_string parts =
    let parse_margin_value value =
      if value = "px" then Some `Px
      else if value = "auto" then Some `Auto
      else
        match Parse.spacing_value ~name:"margin" value with
        | Ok f -> Some (`Rem (f *. 0.25))
        | Error _ -> None
    in
    let parse_spacing_only value =
      match Parse.spacing_value ~name:"margin" value with
      | Ok f -> Some (`Rem (f *. 0.25))
      | Error _ -> None
    in
    let parse_positive _prefix axis value =
      match parse_margin_value value with
      | Some v -> Ok { negative = false; axis; value = v }
      | None -> Error (`Msg "Not a margin utility")
    in
    let parse_negative axis value =
      match parse_spacing_only value with
      | Some v -> Ok { negative = true; axis; value = v }
      | None -> Error (`Msg "Not a margin utility")
    in
    match parts with
    | [ "m"; value ] -> parse_positive "m" `All value
    | [ "mx"; value ] -> parse_positive "mx" `X value
    | [ "my"; value ] -> parse_positive "my" `Y value
    | [ "mt"; value ] -> parse_positive "mt" `T value
    | [ "mr"; value ] -> parse_positive "mr" `R value
    | [ "mb"; value ] -> parse_positive "mb" `B value
    | [ "ml"; value ] -> parse_positive "ml" `L value
    | [ "-m"; value ] -> parse_negative `All value
    | [ "-mx"; value ] -> parse_negative `X value
    | [ "-my"; value ] -> parse_negative `Y value
    | [ "-mt"; value ] -> parse_negative `T value
    | [ "-mr"; value ] -> parse_negative `R value
    | [ "-mb"; value ] -> parse_negative `B value
    | [ "-ml"; value ] -> parse_negative `L value
    | _ -> Error (`Msg "Not a margin utility")
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
