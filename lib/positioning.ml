(** Positioning utilities for controlling element placement *)

open Core
open Css
module Parse = Parse

(** {1 Helper Functions} *)

(* Helper to create spacing values - handles negative values *)
let spacing_value n =
  let spacing_rem = Rem (float_of_int (abs n) *. 0.25) in
  if n < 0 then 
    Calc (Calc.mul (Calc.float (-1.0)) (Calc.length spacing_rem))
  else 
    spacing_rem

(** {1 Inset Utilities} *)

let inset_0 =
  style "inset-0"
    [ Css.top Zero; Css.right Zero; Css.bottom Zero; Css.left Zero ]

let inset_x_0 = style "inset-x-0" [ Css.left Zero; Css.right Zero ]
let inset_y_0 = style "inset-y-0" [ Css.bottom Zero; Css.top Zero ]

let inset n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name
    [ Css.top value; Css.right value; Css.bottom value; Css.left value ]

(** {1 Individual Side Positioning} *)

let top n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ Css.top value ]

let right n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ Css.right value ]

let bottom n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ Css.bottom value ]

let left n =
  let prefix = if n < 0 then "-" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ Css.left value ]

(** {1 Fractional Positioning} *)

let top_1_2 = style "top-1/2" [ Css.top (Pct 50.0) ]
let left_1_2 = style "left-1/2" [ Css.left (Pct 50.0) ]

(** {1 Z-Index} *)

let z n =
  let class_name = "z-" ^ string_of_int n in
  style class_name [ Css.z_index n ]

(** {1 Parsing Functions} *)

let int_of_string_with_sign = Parse.int_any

let of_string = function
  | [ "inset"; "0" ] -> Ok inset_0
  | [ "inset"; "x"; "0" ] -> Ok inset_x_0
  | [ "inset"; "y"; "0" ] -> Ok inset_y_0
  | [ "inset"; n ] -> int_of_string_with_sign n |> Result.map inset
  | [ "-"; "inset"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> inset (-x))
  | [ "top"; "1/2" ] -> Ok top_1_2
  | [ "top"; n ] -> int_of_string_with_sign n |> Result.map top
  | [ "-"; "top"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> top (-x))
  | [ "right"; n ] -> int_of_string_with_sign n |> Result.map right
  | [ "-"; "right"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> right (-x))
  | [ "bottom"; n ] -> int_of_string_with_sign n |> Result.map bottom
  | [ "-"; "bottom"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> bottom (-x))
  | [ "left"; "1/2" ] -> Ok left_1_2
  | [ "left"; n ] -> int_of_string_with_sign n |> Result.map left
  | [ "-"; "left"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> left (-x))
  | [ "z"; n ] -> int_of_string_with_sign n |> Result.map z
  | _ -> Error (`Msg "Not a positioning utility")
