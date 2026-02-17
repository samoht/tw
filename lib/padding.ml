(** Padding utilities. *)

module Handler = struct
  open Style
  open Css

  type t = {
    axis : [ `All | `X | `Y | `T | `R | `B | `L | `S | `E | `Bs | `Be ];
    value : spacing;
  }

  type Utility.base += Self of t

  let name = "padding"
  let priority = 21

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
      | `S -> "ps-"
      | `E -> "pe-"
      | `Bs -> "pbs-"
      | `Be -> "pbe-"
    in
    prefix ^ Spacing.pp_spacing_suffix value

  (** Convert spacing to (declaration option, length) using
      Theme.spacing_calc_float. *)
  let spacing_to_decl_len (s : Style.spacing) : Css.declaration option * length
      =
    match s with
    | `Px ->
        let len : length = Px 1. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Full ->
        let len : length = Pct 100. in
        let decl, _ = Var.binding Spacing.spacing_var (Rem 0.25) in
        (Some decl, len)
    | `Named name ->
        let len = Spacing.named_spacing_ref name in
        (None, len)
    | `Rem f ->
        let n = f /. 0.25 in
        let decl, len = Theme.spacing_calc_float n in
        (Some decl, len)

  let v (prop : length -> declaration) t =
    let decl, len = spacing_to_decl_len t.value in
    style (Option.to_list decl @ [ prop len ])

  let vs (prop : length list -> declaration) t =
    let decl, len = spacing_to_decl_len t.value in
    style (Option.to_list decl @ [ prop [ len ] ])

  let spacing_value_order = function
    | `Px -> 1
    | `Full -> 10000
    | `Named _ -> 20000
    | `Rem f ->
        let units = f /. 0.25 in
        int_of_float (units *. 10.)

  let to_style t =
    match t.axis with
    | `All -> vs padding t
    | `X -> v padding_inline t
    | `Y -> v padding_block t
    | `T -> v padding_top t
    | `R -> v padding_right t
    | `B -> v padding_bottom t
    | `L -> v padding_left t
    | `S -> v padding_inline_start t
    | `E -> v padding_inline_end t
    | `Bs -> v padding_block_start t
    | `Be -> v padding_block_end t

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
      | `S -> 70000
      | `E -> 80000
      | `Bs -> 90000
      | `Be -> 100000
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
