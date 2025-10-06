(** Flexbox property utilities (direction, wrap, grow, shrink, basis, order).

    These utilities control flexbox behavior and come after sizing utilities in
    the cascade order. For flex display utilities (flex, inline-flex), see Flex
    module. *)

module Handler = struct
  open Style
  open Css

  type t =
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

  type Utility.base += Self of t

  let name = "flex_props"

  (** Priority 11 - after sizing (5) and grid-template (10), same as alignment,
      before gap (12) *)
  let priority = 11

  (* Direction *)
  let flex_row = style [ flex_direction Row ]
  let flex_row_reverse = style [ flex_direction Row_reverse ]
  let flex_col = style [ flex_direction Column ]
  let flex_col_reverse = style [ flex_direction Column_reverse ]

  (* Wrap *)
  let flex_wrap_utility = style [ flex_wrap Wrap ]
  let flex_wrap_reverse_utility = style [ flex_wrap Wrap_reverse ]
  let flex_nowrap_utility = style [ flex_wrap Nowrap ]

  (* Flex shortcuts *)
  let flex_1 = style [ flex (Full (1.0, 1.0, Pct 0.0)) ]
  let flex_auto = style [ flex Auto ]
  let flex_initial = style [ flex Initial ]
  let flex_none = style [ flex None ]

  (* Grow *)
  let flex_grow_utility = style [ flex_grow 1.0 ]
  let flex_grow_0_utility = style [ flex_grow 0.0 ]

  (* Shrink *)
  let flex_shrink_utility = style [ flex_shrink 1.0 ]
  let flex_shrink_0_utility = style [ flex_shrink 0.0 ]

  (* Basis *)
  let basis_0 = style [ flex_basis Zero ]
  let basis_1 = style [ flex_basis (Pct 100.0) ]
  let basis_auto = style [ flex_basis Auto ]
  let basis_full = style [ flex_basis (Pct 100.0) ]

  (* Order *)
  let order_style n = style [ order n ]
  let order_first = style [ order (-9999) ]
  let order_last = style [ order 9999 ]
  let order_none = style [ order 0 ]

  let to_style = function
    | Flex_row -> flex_row
    | Flex_row_reverse -> flex_row_reverse
    | Flex_col -> flex_col
    | Flex_col_reverse -> flex_col_reverse
    | Flex_wrap -> flex_wrap_utility
    | Flex_wrap_reverse -> flex_wrap_reverse_utility
    | Flex_nowrap -> flex_nowrap_utility
    | Flex_1 -> flex_1
    | Flex_auto -> flex_auto
    | Flex_initial -> flex_initial
    | Flex_none -> flex_none
    | Flex_grow -> flex_grow_utility
    | Flex_grow_0 -> flex_grow_0_utility
    | Flex_shrink -> flex_shrink_utility
    | Flex_shrink_0 -> flex_shrink_0_utility
    | Basis_0 -> basis_0
    | Basis_1 -> basis_1
    | Basis_auto -> basis_auto
    | Basis_full -> basis_full
    | Order n -> order_style n
    | Order_first -> order_first
    | Order_last -> order_last
    | Order_none -> order_none

  let suborder : t -> int = function
    (* Direction - alphabetical order: col, col-reverse, row, row-reverse *)
    | Flex_col -> 0
    | Flex_col_reverse -> 1
    | Flex_row -> 2
    | Flex_row_reverse -> 3
    (* Wrap - alphabetical order: nowrap, wrap, wrap-reverse *)
    | Flex_nowrap -> 10
    | Flex_wrap -> 11
    | Flex_wrap_reverse -> 12
    (* Flex shortcuts *)
    | Flex_1 -> 20
    | Flex_auto -> 21
    | Flex_initial -> 22
    | Flex_none -> 23
    (* Grow *)
    | Flex_grow -> 30
    | Flex_grow_0 -> 31
    (* Shrink *)
    | Flex_shrink -> 40
    | Flex_shrink_0 -> 41
    (* Basis *)
    | Basis_0 -> 50
    | Basis_1 -> 51
    | Basis_auto -> 52
    | Basis_full -> 53
    (* Order *)
    | Order_none -> 60 (* order-0 *)
    | Order n -> 60 + n
    | Order_first -> 90
    | Order_last -> 91

  let err_not_utility = Error (`Msg "Not a flex property utility")

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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
        | Some n when n >= 1 && n <= 6 -> Ok (Order n)
        | _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    (* Direction *)
    | Flex_row -> "flex-row"
    | Flex_row_reverse -> "flex-row-reverse"
    | Flex_col -> "flex-col"
    | Flex_col_reverse -> "flex-col-reverse"
    (* Wrap *)
    | Flex_wrap -> "flex-wrap"
    | Flex_wrap_reverse -> "flex-wrap-reverse"
    | Flex_nowrap -> "flex-nowrap"
    (* Flex shortcuts *)
    | Flex_1 -> "flex-1"
    | Flex_auto -> "flex-auto"
    | Flex_initial -> "flex-initial"
    | Flex_none -> "flex-none"
    (* Grow *)
    | Flex_grow -> "flex-grow"
    | Flex_grow_0 -> "flex-grow-0"
    (* Shrink *)
    | Flex_shrink -> "flex-shrink"
    | Flex_shrink_0 -> "flex-shrink-0"
    (* Basis *)
    | Basis_0 -> "basis-0"
    | Basis_1 -> "basis-1"
    | Basis_auto -> "basis-auto"
    | Basis_full -> "basis-full"
    (* Order *)
    | Order n -> "order-" ^ string_of_int n
    | Order_first -> "order-first"
    | Order_last -> "order-last"
    | Order_none -> "order-none"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex_row = utility Flex_row
let flex_row_reverse = utility Flex_row_reverse
let flex_col = utility Flex_col
let flex_col_reverse = utility Flex_col_reverse
let flex_wrap = utility Flex_wrap
let flex_wrap_reverse = utility Flex_wrap_reverse
let flex_nowrap = utility Flex_nowrap
let flex_1 = utility Flex_1
let flex_auto = utility Flex_auto
let flex_initial = utility Flex_initial
let flex_none = utility Flex_none
let flex_grow = utility Flex_grow
let flex_grow_0 = utility Flex_grow_0
let flex_shrink = utility Flex_shrink
let flex_shrink_0 = utility Flex_shrink_0
let basis_0 = utility Basis_0
let basis_1 = utility Basis_1
let basis_auto = utility Basis_auto
let basis_full = utility Basis_full
let order n = utility (Order n)
let order_first = utility Order_first
let order_last = utility Order_last
let order_none = utility Order_none
