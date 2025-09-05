(** Positioning utilities for controlling element placement *)

open Core
open Css
module Parse = Parse

(** {1 Helper Functions} *)

(* Get or create the spacing theme variable *)
let spacing_def, spacing_var = Var.theme Var.Spacing (Rem 0.25)

(* Helper to create spacing values using calc(var(--spacing) * n) *)
let spacing_value n : Css.length =
  Calc (Calc.mul (Calc.length (Var spacing_var)) (Calc.float (float_of_int n)))

(** {1 Inset Utilities} *)

let inset_0 =
  let zero_value = spacing_value 0 in
  style "inset-0"
    [
      spacing_def;
      Css.top zero_value;
      Css.right zero_value;
      Css.bottom zero_value;
      Css.left zero_value;
    ]

let inset_x_0 =
  let zero_value = spacing_value 0 in
  style "inset-x-0" [ spacing_def; Css.left zero_value; Css.right zero_value ]

let inset_y_0 =
  let zero_value = spacing_value 0 in
  style "inset-y-0" [ spacing_def; Css.bottom zero_value; Css.top zero_value ]

let inset n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name
    [
      spacing_def;
      Css.top value;
      Css.right value;
      Css.bottom value;
      Css.left value;
    ]

(** {1 Individual Side Positioning} *)

let top n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ spacing_def; Css.top value ]

let right n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ spacing_def; Css.right value ]

let bottom n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ spacing_def; Css.bottom value ]

let left n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  let value = spacing_value n in
  style class_name [ spacing_def; Css.left value ]

(** {1 Fractional Positioning} *)

let top_1_2 = style "top-1/2" [ Css.top (Pct 50.0) ]
let left_1_2 = style "left-1/2" [ Css.left (Pct 50.0) ]

(** {1 Z-Index} *)

let z n =
  let class_name = "z-" ^ string_of_int n in
  style class_name [ Css.z_index (Css.Index n) ]

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
