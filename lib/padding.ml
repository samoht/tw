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
      | `All -> 905
      | `X -> 910
      | `Y -> 915
      | `T -> 920
      | `R -> 925
      | `B -> 930
      | `L -> 935
    in
    side_offset + (spacing_value_order value / 1000)

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    let parse_class = function
      | [ "p"; value ] -> (
          match Parse.spacing_value ~name:"padding" value with
          | Ok f -> Ok { axis = `All; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `All; value = `Px }
              else if value = "full" then Ok { axis = `All; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "px"; value ] -> (
          match Parse.spacing_value ~name:"padding-x" value with
          | Ok f -> Ok { axis = `X; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `X; value = `Px }
              else if value = "full" then Ok { axis = `X; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "py"; value ] -> (
          match Parse.spacing_value ~name:"padding-y" value with
          | Ok f -> Ok { axis = `Y; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `Y; value = `Px }
              else if value = "full" then Ok { axis = `Y; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "pt"; value ] -> (
          match Parse.spacing_value ~name:"padding-top" value with
          | Ok f -> Ok { axis = `T; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `T; value = `Px }
              else if value = "full" then Ok { axis = `T; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "pr"; value ] -> (
          match Parse.spacing_value ~name:"padding-right" value with
          | Ok f -> Ok { axis = `R; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `R; value = `Px }
              else if value = "full" then Ok { axis = `R; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "pb"; value ] -> (
          match Parse.spacing_value ~name:"padding-bottom" value with
          | Ok f -> Ok { axis = `B; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `B; value = `Px }
              else if value = "full" then Ok { axis = `B; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | [ "pl"; value ] -> (
          match Parse.spacing_value ~name:"padding-left" value with
          | Ok f -> Ok { axis = `L; value = `Rem (f *. 0.25) }
          | Error _ ->
              if value = "px" then Ok { axis = `L; value = `Px }
              else if value = "full" then Ok { axis = `L; value = `Full }
              else Error (`Msg "Not a padding utility"))
      | _ -> Error (`Msg "Not a padding utility")
    in
    parse_class parts
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
