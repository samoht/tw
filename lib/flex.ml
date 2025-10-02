(** Flexbox utilities

    @see <https://tailwindcss.com/docs/flex> Tailwind CSS Flex documentation
    @see <https://tailwindcss.com/docs/flex-direction> Flex Direction
    @see <https://tailwindcss.com/docs/flex-wrap> Flex Wrap
    @see <https://tailwindcss.com/docs/flex-grow> Flex Grow
    @see <https://tailwindcss.com/docs/flex-shrink> Flex Shrink *)

open Utility
open Style
open Css

(** Local flex utility type *)
type t =
  (* Display *)
  | Flex
  | Inline_flex
  (* Direction *)
  | Flex_row
  | Flex_row_reverse
  | Flex_col
  | Flex_col_reverse
  (* Wrap *)
  | Flex_wrap
  | Flex_wrap_reverse
  | Flex_nowrap
  (* Flex shortcuts *)
  | Flex_1
  | Flex_auto
  | Flex_initial
  | Flex_none
  (* Grow *)
  | Flex_grow
  | Flex_grow_0
  (* Shrink *)
  | Flex_shrink
  | Flex_shrink_0
  (* Basis *)
  | Basis_0
  | Basis_1
  | Basis_auto
  | Basis_full
  (* Order *)
  | Order of int
  | Order_first
  | Order_last
  | Order_none

(** Extensible variant for flex utilities *)
type Utility.base += Flex of t

let wrap x = Flex x
let unwrap = function Flex x -> Some x | _ -> None
let base x = Utility.base (wrap x)

(** Helper functions returning Style.t *)

(* Display *)
let flex' = style "flex" [ display Flex ]
let inline_flex' = style "inline-flex" [ display Inline_flex ]

(* Direction *)
let flex_row' = style "flex-row" [ flex_direction Row ]
let flex_row_reverse' = style "flex-row-reverse" [ flex_direction Row_reverse ]
let flex_col' = style "flex-col" [ flex_direction Column ]

let flex_col_reverse' =
  style "flex-col-reverse" [ flex_direction Column_reverse ]

(* Wrap *)
let flex_wrap' = style "flex-wrap" [ flex_wrap Wrap ]
let flex_wrap_reverse' = style "flex-wrap-reverse" [ flex_wrap Wrap_reverse ]
let flex_nowrap' = style "flex-nowrap" [ flex_wrap Nowrap ]

(* Flex shortcuts *)
let flex_1' = style "flex-1" [ flex (Full (1.0, 1.0, Pct 0.0)) ]
let flex_auto' = style "flex-auto" [ flex Auto ]
let flex_initial' = style "flex-initial" [ flex Initial ]
let flex_none' = style "flex-none" [ flex None ]

(* Grow *)
let flex_grow' = style "flex-grow" [ flex_grow 1.0 ]
let flex_grow_0' = style "flex-grow-0" [ flex_grow 0.0 ]

(* Shrink *)
let flex_shrink' = style "flex-shrink" [ flex_shrink 1.0 ]
let flex_shrink_0' = style "flex-shrink-0" [ flex_shrink 0.0 ]

(* Basis *)
let basis_0' = style "basis-0" [ flex_basis Zero ]
let basis_1' = style "basis-1" [ flex_basis (Pct 100.0) ]
let basis_auto' = style "basis-auto" [ flex_basis Auto ]
let basis_full' = style "basis-full" [ flex_basis (Pct 100.0) ]

(* Order *)
let order' n = style ("order-" ^ string_of_int n) [ order n ]
let order_first' = style "order-first" [ order (-9999) ]
let order_last' = style "order-last" [ order 9999 ]
let order_none' = style "order-none" [ order 0 ]

(** Public API returning Utility.t *)
let flex = base Flex

let inline_flex = base Inline_flex
let flex_row = base Flex_row
let flex_row_reverse = base Flex_row_reverse
let flex_col = base Flex_col
let flex_col_reverse = base Flex_col_reverse
let flex_wrap = base Flex_wrap
let flex_wrap_reverse = base Flex_wrap_reverse
let flex_nowrap = base Flex_nowrap
let flex_1 = base Flex_1
let flex_auto = base Flex_auto
let flex_initial = base Flex_initial
let flex_none = base Flex_none
let flex_grow = base Flex_grow
let flex_grow_0 = base Flex_grow_0
let flex_shrink = base Flex_shrink
let flex_shrink_0 = base Flex_shrink_0
let basis_0 = base Basis_0
let basis_1 = base Basis_1
let basis_auto = base Basis_auto
let basis_full = base Basis_full
let order n = base (Order n)
let order_first = base Order_first
let order_last = base Order_last
let order_none = base Order_none
let err_not_utility = Error (`Msg "Not a flex utility")

let to_style : t -> Style.t = function
  | Flex -> flex'
  | Inline_flex -> inline_flex'
  | Flex_row -> flex_row'
  | Flex_row_reverse -> flex_row_reverse'
  | Flex_col -> flex_col'
  | Flex_col_reverse -> flex_col_reverse'
  | Flex_wrap -> flex_wrap'
  | Flex_wrap_reverse -> flex_wrap_reverse'
  | Flex_nowrap -> flex_nowrap'
  | Flex_1 -> flex_1'
  | Flex_auto -> flex_auto'
  | Flex_initial -> flex_initial'
  | Flex_none -> flex_none'
  | Flex_grow -> flex_grow'
  | Flex_grow_0 -> flex_grow_0'
  | Flex_shrink -> flex_shrink'
  | Flex_shrink_0 -> flex_shrink_0'
  | Basis_0 -> basis_0'
  | Basis_1 -> basis_1'
  | Basis_auto -> basis_auto'
  | Basis_full -> basis_full'
  | Order n -> order' n
  | Order_first -> order_first'
  | Order_last -> order_last'
  | Order_none -> order_none'

(** Parse string parts to flex utility *)
let of_string : string list -> (t, _) result = function
  | [ "flex" ] -> Ok Flex
  | [ "inline"; "flex" ] -> Ok Inline_flex
  | [ "flex"; "row" ] -> Ok Flex_row
  | [ "flex"; "row"; "reverse" ] -> Ok Flex_row_reverse
  | [ "flex"; "col" ] -> Ok Flex_col
  | [ "flex"; "col"; "reverse" ] -> Ok Flex_col_reverse
  | [ "flex"; "wrap" ] -> Ok Flex_wrap
  | [ "flex"; "wrap"; "reverse" ] -> Ok Flex_wrap_reverse
  | [ "flex"; "nowrap" ] -> Ok Flex_nowrap
  | [ "flex"; "1" ] -> Ok Flex_1
  | [ "flex"; "auto" ] -> Ok Flex_auto
  | [ "flex"; "initial" ] -> Ok Flex_initial
  | [ "flex"; "none" ] -> Ok Flex_none
  | [ "flex"; "grow" ] -> Ok Flex_grow
  | [ "flex"; "grow"; "0" ] -> Ok Flex_grow_0
  | [ "flex"; "shrink" ] -> Ok Flex_shrink
  | [ "flex"; "shrink"; "0" ] -> Ok Flex_shrink_0
  | [ "basis"; "0" ] -> Ok Basis_0
  | [ "basis"; "1" ] -> Ok Basis_1
  | [ "basis"; "auto" ] -> Ok Basis_auto
  | [ "basis"; "full" ] -> Ok Basis_full
  | [ "order"; "first" ] -> Ok Order_first
  | [ "order"; "last" ] -> Ok Order_last
  | [ "order"; "none" ] -> Ok Order_none
  | [ "order"; n ] -> (
      match int_of_string_opt n with
      | Some n -> Ok (Order n)
      | _ -> err_not_utility)
  | _ -> err_not_utility

(** Suborder for flex utilities *)
let suborder : t -> int = function
  (* Display *)
  | Flex -> 0
  | Inline_flex -> 1
  (* Direction *)
  | Flex_row -> 10
  | Flex_row_reverse -> 11
  | Flex_col -> 12
  | Flex_col_reverse -> 13
  (* Wrap *)
  | Flex_wrap -> 20
  | Flex_wrap_reverse -> 21
  | Flex_nowrap -> 22
  (* Flex shortcuts *)
  | Flex_1 -> 30
  | Flex_auto -> 31
  | Flex_initial -> 32
  | Flex_none -> 33
  (* Grow *)
  | Flex_grow -> 40
  | Flex_grow_0 -> 41
  (* Shrink *)
  | Flex_shrink -> 50
  | Flex_shrink_0 -> 51
  (* Basis *)
  | Basis_0 -> 60
  | Basis_1 -> 61
  | Basis_auto -> 62
  | Basis_full -> 63
  (* Order *)
  | Order_none -> 70 (* order-0 *)
  | Order n -> 70 + n
  | Order_first -> 100
  | Order_last -> 101

(** Priority for flex utilities *)
let priority = 50

let () =
  Utility.register ~wrap ~unwrap { to_style; priority; suborder; of_string }

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end
