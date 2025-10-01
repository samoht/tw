(** Positioning utilities for controlling element placement *)

open Style
open Css
module Parse = Parse

(** {1 Positioning Utility Type} *)

type utility =
  | Static
  | Relative
  | Absolute
  | Fixed
  | Sticky
  | Inset_0
  | Inset_x_0
  | Inset_y_0
  | Inset of int
  | Top of int
  | Top_1_2
  | Right of int
  | Bottom of int
  | Left of int
  | Left_1_2
  | Z of int

(** {1 Helper Functions} *)

(* Use shared spacing variable from Theme *)
let spacing_var = Theme.spacing_var

(** {1 Position Utilities} *)

let static = style "static" [ position Static ]
let relative = style "relative" [ position Relative ]
let absolute = style "absolute" [ position Absolute ]
let fixed = style "fixed" [ position Fixed ]
let sticky = style "sticky" [ position Sticky ]

(* Helper to create spacing values using calc(var(--spacing) * n) *)
let spacing_value n : Css.declaration * Css.length =
  let decl, spacing_ref = Var.binding spacing_var (Rem 0.25) in
  ( decl,
    Calc
      (Calc.mul (Calc.length (Var spacing_ref)) (Calc.float (float_of_int n)))
  )

(** {1 Inset Utilities} *)

let inset_0 =
  let decl, zero_value = spacing_value 0 in
  style "inset-0"
    (decl
    :: [
         Css.top zero_value;
         Css.right zero_value;
         Css.bottom zero_value;
         Css.left zero_value;
       ])

let inset_x_0 =
  let decl, zero_value = spacing_value 0 in
  style "inset-x-0" (decl :: [ Css.left zero_value; Css.right zero_value ])

let inset_y_0 =
  let decl, zero_value = spacing_value 0 in
  style "inset-y-0" (decl :: [ Css.bottom zero_value; Css.top zero_value ])

let inset n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
  let decl, value = spacing_value n in
  style class_name
    (decl
    :: [ Css.top value; Css.right value; Css.bottom value; Css.left value ])

(** {1 Individual Side Positioning} *)

let top n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
  let decl, value = spacing_value n in
  style class_name (decl :: [ Css.top value ])

let right n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
  let decl, value = spacing_value n in
  style class_name (decl :: [ Css.right value ])

let bottom n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
  let decl, value = spacing_value n in
  style class_name (decl :: [ Css.bottom value ])

let left n =
  let prefix = if n < 0 then "--" else "" in
  let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
  let decl, value = spacing_value n in
  style class_name (decl :: [ Css.left value ])

(** {1 Fractional Positioning} *)

let top_1_2 = style "top-1/2" [ Css.top (Pct 50.0) ]
let left_1_2 = style "left-1/2" [ Css.left (Pct 50.0) ]

(** {1 Z-Index} *)

let z n =
  let class_name = "z-" ^ string_of_int n in
  style class_name [ Css.z_index (Css.Index n) ]

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Static -> static
  | Relative -> relative
  | Absolute -> absolute
  | Fixed -> fixed
  | Sticky -> sticky
  | Inset_0 -> inset_0
  | Inset_x_0 -> inset_x_0
  | Inset_y_0 -> inset_y_0
  | Inset n -> inset n
  | Top n -> top n
  | Top_1_2 -> top_1_2
  | Right n -> right n
  | Bottom n -> bottom n
  | Left n -> left n
  | Left_1_2 -> left_1_2
  | Z n -> z n

let int_of_string_with_sign = Parse.int_any

let of_string = function
  | [ "static" ] -> Ok Static
  | [ "relative" ] -> Ok Relative
  | [ "absolute" ] -> Ok Absolute
  | [ "fixed" ] -> Ok Fixed
  | [ "sticky" ] -> Ok Sticky
  | [ "inset"; "0" ] -> Ok Inset_0
  | [ "inset"; "x"; "0" ] -> Ok Inset_x_0
  | [ "inset"; "y"; "0" ] -> Ok Inset_y_0
  | [ "inset"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Inset x)
  | [ "-"; "inset"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Inset (-x))
  | [ "top"; "1/2" ] -> Ok Top_1_2
  | [ "top"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Top x)
  | [ "-"; "top"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Top (-x))
  | [ "right"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Right x)
  | [ "-"; "right"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Right (-x))
  | [ "bottom"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Bottom x)
  | [ "-"; "bottom"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Bottom (-x))
  | [ "left"; "1/2" ] -> Ok Left_1_2
  | [ "left"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Left x)
  | [ "-"; "left"; n ] ->
      int_of_string_with_sign n |> Result.map (fun x -> Left (-x))
  | [ "z"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Z x)
  | _ -> Error (`Msg "Not a positioning utility")

let suborder = function
  | Static -> 0
  | Relative -> 1
  | Absolute -> 2
  | Fixed -> 3
  | Sticky -> 4
  | Inset_0 -> 100
  | Inset_x_0 -> 101
  | Inset_y_0 -> 102
  | Inset n -> 200 + n
  | Top_1_2 -> 300
  | Top n -> 400 + n
  | Right n -> 500 + n
  | Bottom n -> 600 + n
  | Left_1_2 -> 700
  | Left n -> 800 + n
  | Z n -> 900 + n
