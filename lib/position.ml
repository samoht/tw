(** Positioning utilities for controlling element placement *)

(** {1 Helper Functions} *)

(* Use shared spacing variable from Theme *)
let spacing_var = Theme.spacing_var

let spacing_value n : Css.declaration * Css.length =
  let decl, spacing_ref = Var.binding spacing_var (Css.Rem 0.25) in
  ( decl,
    Css.Calc
      (Css.Calc.mul
         (Css.Calc.length (Css.Var spacing_ref))
         (Css.Calc.float (float_of_int n))) )

module Handler = struct
  open Style
  open Css

  (** Local position utility type *)
  type t =
    | Position_static
    | Position_relative
    | Position_absolute
    | Position_fixed
    | Position_sticky
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

  (** Extensible variant for position utilities *)
  type Utility.base += Self of t

  (** Priority for position utilities *)
  let priority = 0

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Position_static -> style "static" [ position Static ]
    | Position_relative -> style "relative" [ position Relative ]
    | Position_absolute -> style "absolute" [ position Absolute ]
    | Position_fixed -> style "fixed" [ position Fixed ]
    | Position_sticky -> style "sticky" [ position Sticky ]
    | Inset_0 ->
        let decl, zero_value = spacing_value 0 in
        style "inset-0"
          (decl
          :: [
               Css.top zero_value;
               Css.right zero_value;
               Css.bottom zero_value;
               Css.left zero_value;
             ])
    | Inset_x_0 ->
        let decl, zero_value = spacing_value 0 in
        style "inset-x-0" (decl :: [ Css.left zero_value; Css.right zero_value ])
    | Inset_y_0 ->
        let decl, zero_value = spacing_value 0 in
        style "inset-y-0" (decl :: [ Css.bottom zero_value; Css.top zero_value ])
    | Inset n ->
        let prefix = if n < 0 then "--" else "" in
        let class_name = prefix ^ "inset-" ^ string_of_int (abs n) in
        let decl, value = spacing_value n in
        style class_name
          (decl
          :: [
               Css.top value; Css.right value; Css.bottom value; Css.left value;
             ])
    | Top n ->
        let prefix = if n < 0 then "--" else "" in
        let class_name = prefix ^ "top-" ^ string_of_int (abs n) in
        let decl, value = spacing_value n in
        style class_name (decl :: [ Css.top value ])
    | Top_1_2 -> style "top-1/2" [ Css.top (Pct 50.0) ]
    | Right n ->
        let prefix = if n < 0 then "--" else "" in
        let class_name = prefix ^ "right-" ^ string_of_int (abs n) in
        let decl, value = spacing_value n in
        style class_name (decl :: [ Css.right value ])
    | Bottom n ->
        let prefix = if n < 0 then "--" else "" in
        let class_name = prefix ^ "bottom-" ^ string_of_int (abs n) in
        let decl, value = spacing_value n in
        style class_name (decl :: [ Css.bottom value ])
    | Left n ->
        let prefix = if n < 0 then "--" else "" in
        let class_name = prefix ^ "left-" ^ string_of_int (abs n) in
        let decl, value = spacing_value n in
        style class_name (decl :: [ Css.left value ])
    | Left_1_2 -> style "left-1/2" [ Css.left (Pct 50.0) ]
    | Z n ->
        let class_name = "z-" ^ string_of_int n in
        style class_name [ Css.z_index (Css.Index n) ]

  let int_of_string_with_sign = Parse.int_any

  let of_string = function
    | [ "static" ] -> Ok Position_static
    | [ "relative" ] -> Ok Position_relative
    | [ "absolute" ] -> Ok Position_absolute
    | [ "fixed" ] -> Ok Position_fixed
    | [ "sticky" ] -> Ok Position_sticky
    | [ "inset"; "0" ] -> Ok Inset_0
    | [ "inset"; "x"; "0" ] -> Ok Inset_x_0
    | [ "inset"; "y"; "0" ] -> Ok Inset_y_0
    | [ "inset"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset x)
    | [ "-"; "inset"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset (-x))
    | [ "top"; "1/2" ] -> Ok Top_1_2
    | [ "top"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Top x)
    | [ "-"; "top"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Top (-x))
    | [ "right"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Right x)
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
    | _ -> Error (`Msg "Not a position utility")

  let suborder = function
    | Position_static -> 0
    | Position_relative -> 1
    | Position_absolute -> 2
    | Position_fixed -> 3
    | Position_sticky -> 4
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
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API combinators *)
let utility x = Utility.base (Self x)

let static = utility Position_static
let relative = utility Position_relative
let absolute = utility Position_absolute
let fixed = utility Position_fixed
let sticky = utility Position_sticky
let inset n = utility (Inset n)
let inset_0 = utility Inset_0
let inset_x_0 = utility Inset_x_0
let inset_y_0 = utility Inset_y_0
let top n = utility (Top n)
let right n = utility (Right n)
let bottom n = utility (Bottom n)
let left n = utility (Left n)
let top_1_2 = utility Top_1_2
let left_1_2 = utility Left_1_2
let z n = utility (Z n)
