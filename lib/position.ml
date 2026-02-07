(** Positioning utilities for controlling element placement *)

(** {1 Helper Functions} *)

(* Create a spacing value using the named spacing variable --spacing-N. Returns
   the theme declaration and a length that references the variable. For negative
   values, uses calc(var(--spacing-N) * -1) where N is absolute. *)
let spacing_value n : Css.declaration * Css.length =
  let abs_n = abs n in
  let spacing_var = Theme.get_spacing_var abs_n in
  let concrete_value = Theme.spacing_value abs_n in
  let decl, spacing_ref = Var.binding spacing_var concrete_value in
  let len : Css.length =
    if n < 0 then
      (* For negative: calc(var(--spacing-N) * -1) *)
      Css.Calc
        (Css.Calc.mul
           (Css.Calc.length (Css.Var spacing_ref))
           (Css.Calc.float (-1.)))
    else Css.Var spacing_ref
  in
  (decl, len)

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
    | Inset_auto
    | Inset_full
    | Inset_3_4
    | Inset_x_auto
    | Inset_x_full
    | Inset_x_3_4
    | Inset_y_auto
    | Inset_y_full
    | Inset_y_3_4
    | Inset of int
    | Inset_x of int
    | Inset_y of int
    | Top of int
    | Top_1_2
    | Top_auto
    | Top_full
    | Top_3_4
    | Right of int
    | Right_auto
    | Right_full
    | Right_3_4
    | Bottom of int
    | Bottom_auto
    | Bottom_full
    | Bottom_3_4
    | Left of int
    | Left_1_2
    | Left_auto
    | Left_full
    | Left_3_4
    | Start of int
    | Start_auto
    | Start_full
    | Start_3_4
    | End of int
    | End_auto
    | End_full
    | End_3_4
    | Z of int

  (** Extensible variant for position utilities *)
  type Utility.base += Self of t

  let name = "position"

  (** Priority for position utilities *)
  let priority = 0

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Position_static -> style [ position Static ]
    | Position_relative -> style [ position Relative ]
    | Position_absolute -> style [ position Absolute ]
    | Position_fixed -> style [ position Fixed ]
    | Position_sticky -> style [ position Sticky ]
    | Inset_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.inset zero_value ])
    | Inset_x_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.left zero_value; Css.right zero_value ])
    | Inset_y_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.bottom zero_value; Css.top zero_value ])
    | Inset_auto -> style [ Css.inset Auto ]
    | Inset_full -> style [ Css.inset (Pct 100.0) ]
    | Inset_3_4 -> style [ Css.inset (Pct 75.0) ]
    | Inset_x_auto -> style [ Css.inset_inline Auto ]
    | Inset_x_full -> style [ Css.inset_inline (Pct 100.0) ]
    | Inset_x_3_4 -> style [ Css.inset_inline (Pct 75.0) ]
    | Inset_y_auto -> style [ Css.inset_block Auto ]
    | Inset_y_full -> style [ Css.inset_block (Pct 100.0) ]
    | Inset_y_3_4 -> style [ Css.inset_block (Pct 75.0) ]
    | Inset n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset value ])
    | Inset_x n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline value ])
    | Inset_y n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block value ])
    | Top n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.top value ])
    | Top_1_2 -> style [ Css.top (Pct 50.0) ]
    | Top_auto -> style [ Css.top Auto ]
    | Top_full -> style [ Css.top (Pct 100.0) ]
    | Top_3_4 -> style [ Css.top (Pct 75.0) ]
    | Right n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.right value ])
    | Right_auto -> style [ Css.right Auto ]
    | Right_full -> style [ Css.right (Pct 100.0) ]
    | Right_3_4 -> style [ Css.right (Pct 75.0) ]
    | Bottom n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.bottom value ])
    | Bottom_auto -> style [ Css.bottom Auto ]
    | Bottom_full -> style [ Css.bottom (Pct 100.0) ]
    | Bottom_3_4 -> style [ Css.bottom (Pct 75.0) ]
    | Left n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.left value ])
    | Left_1_2 -> style [ Css.left (Pct 50.0) ]
    | Left_auto -> style [ Css.left Auto ]
    | Left_full -> style [ Css.left (Pct 100.0) ]
    | Left_3_4 -> style [ Css.left (Pct 75.0) ]
    | Start n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_start value ])
    | Start_auto -> style [ Css.inset_inline_start Auto ]
    | Start_full -> style [ Css.inset_inline_start (Pct 100.0) ]
    | Start_3_4 -> style [ Css.inset_inline_start (Pct 75.0) ]
    | End n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_end value ])
    | End_auto -> style [ Css.inset_inline_end Auto ]
    | End_full -> style [ Css.inset_inline_end (Pct 100.0) ]
    | End_3_4 -> style [ Css.inset_inline_end (Pct 75.0) ]
    | Z n -> style [ Css.z_index (Css.Index n) ]

  let int_of_string_with_sign = Parse.int_any

  let suborder = function
    | Position_absolute -> 0
    | Position_fixed -> 1
    | Position_relative -> 2
    | Position_static -> 3
    | Position_sticky -> 4
    | Inset_0 -> 100
    | Inset_x_0 -> 101
    | Inset_y_0 -> 102
    | Inset_3_4 -> 103
    | Inset_auto -> 104
    | Inset_full -> 105
    | Inset_x_3_4 -> 106
    | Inset_x_auto -> 107
    | Inset_x_full -> 108
    | Inset_y_3_4 -> 109
    | Inset_y_auto -> 110
    | Inset_y_full -> 111
    | Inset n -> 200 + n
    | Inset_x n -> 210 + n
    | Inset_y n -> 220 + n
    | Top_1_2 -> 300
    | Top_3_4 -> 301
    | Top_auto -> 302
    | Top_full -> 303
    | Top n -> 400 + n
    | Right_3_4 -> 450
    | Right_auto -> 451
    | Right_full -> 452
    | Right n -> 500 + n
    | Bottom_3_4 -> 550
    | Bottom_auto -> 551
    | Bottom_full -> 552
    | Bottom n -> 600 + n
    | Left_1_2 -> 700
    | Left_3_4 -> 701
    | Left_auto -> 702
    | Left_full -> 703
    | Start_3_4 -> 750
    | Start_auto -> 751
    | Start_full -> 752
    | Start n -> 800 + n
    | End_3_4 -> 850
    | End_auto -> 851
    | End_full -> 852
    | End n -> 900 + n
    | Left n -> 800 + n
    | Z n -> 900 + n

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "static" ] -> Ok Position_static
    | [ "relative" ] -> Ok Position_relative
    | [ "absolute" ] -> Ok Position_absolute
    | [ "fixed" ] -> Ok Position_fixed
    | [ "sticky" ] -> Ok Position_sticky
    | [ "inset"; "0" ] -> Ok Inset_0
    | [ "inset"; "x"; "0" ] -> Ok Inset_x_0
    | [ "inset"; "y"; "0" ] -> Ok Inset_y_0
    | [ "inset"; "auto" ] -> Ok Inset_auto
    | [ "inset"; "full" ] -> Ok Inset_full
    | [ "inset"; "3/4" ] -> Ok Inset_3_4
    | [ "inset"; "x"; "auto" ] -> Ok Inset_x_auto
    | [ "inset"; "x"; "full" ] -> Ok Inset_x_full
    | [ "inset"; "x"; "3/4" ] -> Ok Inset_x_3_4
    | [ "inset"; "y"; "auto" ] -> Ok Inset_y_auto
    | [ "inset"; "y"; "full" ] -> Ok Inset_y_full
    | [ "inset"; "y"; "3/4" ] -> Ok Inset_y_3_4
    | [ "inset"; "x"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_x x)
    | [ ""; "inset"; "x"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_x (-x))
    | [ "inset"; "y"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_y x)
    | [ ""; "inset"; "y"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_y (-x))
    | [ "inset"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset x)
    | [ ""; "inset"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset (-x))
    | [ "top"; "1/2" ] -> Ok Top_1_2
    | [ "top"; "3/4" ] -> Ok Top_3_4
    | [ "top"; "auto" ] -> Ok Top_auto
    | [ "top"; "full" ] -> Ok Top_full
    | [ "top"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Top x)
    | [ ""; "top"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Top (-x))
    | [ "right"; "3/4" ] -> Ok Right_3_4
    | [ "right"; "auto" ] -> Ok Right_auto
    | [ "right"; "full" ] -> Ok Right_full
    | [ "right"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Right x)
    | [ ""; "right"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Right (-x))
    | [ "bottom"; "3/4" ] -> Ok Bottom_3_4
    | [ "bottom"; "auto" ] -> Ok Bottom_auto
    | [ "bottom"; "full" ] -> Ok Bottom_full
    | [ "bottom"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Bottom x)
    | [ ""; "bottom"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Bottom (-x))
    | [ "left"; "1/2" ] -> Ok Left_1_2
    | [ "left"; "3/4" ] -> Ok Left_3_4
    | [ "left"; "auto" ] -> Ok Left_auto
    | [ "left"; "full" ] -> Ok Left_full
    | [ "left"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Left x)
    | [ ""; "left"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Left (-x))
    | [ "start"; "3/4" ] -> Ok Start_3_4
    | [ "start"; "auto" ] -> Ok Start_auto
    | [ "start"; "full" ] -> Ok Start_full
    | [ "start"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Start x)
    | [ ""; "start"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Start (-x))
    | [ "end"; "3/4" ] -> Ok End_3_4
    | [ "end"; "auto" ] -> Ok End_auto
    | [ "end"; "full" ] -> Ok End_full
    | [ "end"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> End x)
    | [ ""; "end"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> End (-x))
    | [ "z"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Z x)
    | _ -> Error (`Msg "Not a position utility")

  let to_class = function
    | Position_static -> "static"
    | Position_relative -> "relative"
    | Position_absolute -> "absolute"
    | Position_fixed -> "fixed"
    | Position_sticky -> "sticky"
    | Inset_0 -> "inset-0"
    | Inset_x_0 -> "inset-x-0"
    | Inset_y_0 -> "inset-y-0"
    | Inset_auto -> "inset-auto"
    | Inset_full -> "inset-full"
    | Inset_3_4 -> "inset-3/4"
    | Inset_x_auto -> "inset-x-auto"
    | Inset_x_full -> "inset-x-full"
    | Inset_x_3_4 -> "inset-x-3/4"
    | Inset_y_auto -> "inset-y-auto"
    | Inset_y_full -> "inset-y-full"
    | Inset_y_3_4 -> "inset-y-3/4"
    | Inset n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-" ^ string_of_int (abs n)
    | Inset_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-x-" ^ string_of_int (abs n)
    | Inset_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-y-" ^ string_of_int (abs n)
    | Top_1_2 -> "top-1/2"
    | Top_3_4 -> "top-3/4"
    | Top_auto -> "top-auto"
    | Top_full -> "top-full"
    | Top n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "top-" ^ string_of_int (abs n)
    | Right_3_4 -> "right-3/4"
    | Right_auto -> "right-auto"
    | Right_full -> "right-full"
    | Right n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "right-" ^ string_of_int (abs n)
    | Bottom_3_4 -> "bottom-3/4"
    | Bottom_auto -> "bottom-auto"
    | Bottom_full -> "bottom-full"
    | Bottom n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "bottom-" ^ string_of_int (abs n)
    | Left_1_2 -> "left-1/2"
    | Left_3_4 -> "left-3/4"
    | Left_auto -> "left-auto"
    | Left_full -> "left-full"
    | Left n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "left-" ^ string_of_int (abs n)
    | Start_3_4 -> "start-3/4"
    | Start_auto -> "start-auto"
    | Start_full -> "start-full"
    | Start n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "start-" ^ string_of_int (abs n)
    | End_3_4 -> "end-3/4"
    | End_auto -> "end-auto"
    | End_full -> "end-full"
    | End n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "end-" ^ string_of_int (abs n)
    | Z n -> "z-" ^ string_of_int n
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
