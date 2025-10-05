(** Padding utilities. *)

module Handler = struct
  open Style
  open Css

  type t = { axis : [ `All | `X | `Y | `T | `R | `B | `L ]; value : spacing }
  type Utility.base += Self of t

  let name = "padding"
  let priority = 15

  let to_class { axis; value } =
    let prefix =
      match axis with
      | `All -> "p-"
      | `X -> "px-"
      | `Y -> "py-"
      | `T -> "pt-"
      | `R -> "pr-"
      | `B -> "pb-"
      | `L -> "pl-"
    in
    prefix ^ Spacing.pp_spacing_suffix value

  let v prop t =
    let spacing_decl, spacing_ref =
      Var.binding Spacing.spacing_var (Rem 0.25)
    in
    let len = Spacing.to_length spacing_ref t.value in
    match t.value with
    | `Rem _ -> style [ spacing_decl; prop len ]
    | _ -> style [ prop len ]

  let vs prop t = v (fun n -> prop [ n ]) t

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  let to_style t =
    match t.axis with
    | `All -> vs padding t
    | `X -> v padding_inline t
    | `Y -> v padding_block t
    | `T -> v padding_top t
    | `R -> v padding_bottom t
    | `B -> v padding_bottom t
    | `L -> v padding_left t

  let suborder { axis; value } =
    let side_offset =
      match axis with
      | `All -> 0
      | `X -> 10000
      | `Y -> 20000
      | `T -> 30000
      | `R -> 40000
      | `B -> 50000
      | `L -> 60000
    in
    side_offset + spacing_value_order value

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match Spacing.parse_class_parts parts with
    | Some (true, _, _) ->
        (* Padding doesn't support negative values *)
        Error (`Msg "Not a padding utility")
    | Some (false, prefix, value) -> (
        if
          (* Only accept padding prefixes *)
          Spacing.is_margin_prefix prefix
        then Error (`Msg "Not a padding utility")
        else
          match Spacing.axis_of_prefix prefix with
          | None -> Error (`Msg "Not a padding utility")
          | Some axis -> (
              match Spacing.parse_value_string ~allow_auto:false value with
              | None -> Error (`Msg "Not a padding utility")
              | Some (#spacing as spacing_val) ->
                  Ok { axis; value = spacing_val }
              | Some `Auto ->
                  (* Auto not allowed for padding *)
                  Error (`Msg "Not a padding utility")))
    | None -> Error (`Msg "Not a padding utility")
end

open Handler

let () = Utility.register (module Handler)
let utility axis value = Utility.base (Self { axis; value })
let p n = utility `All (Spacing.int n)
let px n = utility `X (Spacing.int n)
let py n = utility `Y (Spacing.int n)
let pt n = utility `T (Spacing.int n)
let pr n = utility `R (Spacing.int n)
let pb n = utility `B (Spacing.int n)
let pl n = utility `L (Spacing.int n)
let p_px = utility `All `Px
let p_full = utility `All `Full
let px_px = utility `X `Px
let px_full = utility `X `Full
let py_px = utility `Y `Px
let py_full = utility `Y `Full
let pt_px = utility `T `Px
let pt_full = utility `T `Full
let pr_px = utility `R `Px
let pr_full = utility `R `Full
let pb_px = utility `B `Px
let pb_full = utility `B `Full
let pl_px = utility `L `Px
let pl_full = utility `L `Full
