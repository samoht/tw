(** Flexbox property utilities (grow, shrink, basis, order, flex shortcuts).

    These utilities control flexbox item behavior and come after sizing
    utilities in the cascade order. For flex display utilities (flex,
    inline-flex), see Flex module. For direction/wrap utilities, see Flex_layout
    module. *)

module Handler = struct
  open Style
  open Css

  type t =
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

  (** Priority 7 - after sizing (6), before transforms (9) *)
  let priority = 7

  (* Flex shortcuts *)
  let flex_1 = style [ flex (Grow 1.0) ]
  let flex_auto = style [ flex Auto ]
  let flex_initial = style [ flex (Full (0., 1., Auto)) ]
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
    (* Order - comes first in Tailwind's output. Ordering: order-1..6,
       order-first, order-last, order-none *)
    | Order n -> n
    | Order_first -> 7
    | Order_last -> 8
    | Order_none -> 9
    (* Flex shortcuts - alphabetical: 1, auto, initial, none *)
    | Flex_1 -> 20
    | Flex_auto -> 21
    | Flex_initial -> 22
    | Flex_none -> 23
    (* Shrink - alphabetical: shrink, shrink-0 *)
    | Flex_shrink -> 30
    | Flex_shrink_0 -> 31
    (* Grow - alphabetical: grow, grow-0 *)
    | Flex_grow -> 40
    | Flex_grow_0 -> 41
    (* Basis - alphabetical: 0, 1, auto, full *)
    | Basis_0 -> 50
    | Basis_1 -> 51
    | Basis_auto -> 52
    | Basis_full -> 53

  let err_not_utility = Error (`Msg "Not a flex property utility")

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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
        | Some n when n >= 1 -> Ok (Order n)
        | _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
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
