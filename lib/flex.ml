(** Flexbox display utilities (flex, inline-flex).

    For flexbox property utilities (direction, wrap, grow, shrink, basis,
    order), see Flex_props module. *)

module Handler = struct
  open Style
  open Css

  type t = Flex | Inline_flex
  type Utility.base += Self of t

  (** Priority for flex display utilities. Display utilities all share priority
      4 and are ordered alphabetically by suborder. *)
  let name = "flex"

  let priority = 4
  let flex = style [ display Flex ]
  let inline_flex = style [ display Inline_flex ]
  let to_style = function Flex -> flex | Inline_flex -> inline_flex

  let suborder = function
    (* Display utilities - alphabetical order among all display utilities:
       block(1), flex(2), grid(3), inline(4), inline-block(5/6),
       inline-flex(7) *)
    | Flex -> 2
    | Inline_flex -> 7

  let err_not_utility = Error (`Msg "Not a flex display utility")

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "flex" ] -> Ok Flex
    | [ "inline"; "flex" ] -> Ok Inline_flex
    | _ -> err_not_utility

  let to_class = function Flex -> "flex" | Inline_flex -> "inline-flex"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex = utility Flex
let inline_flex = utility Inline_flex

(** Re-export Flex_layout utilities for backward compatibility *)
let flex_row = Flex_layout.flex_row

let flex_row_reverse = Flex_layout.flex_row_reverse
let flex_col = Flex_layout.flex_col
let flex_col_reverse = Flex_layout.flex_col_reverse
let flex_wrap = Flex_layout.flex_wrap
let flex_wrap_reverse = Flex_layout.flex_wrap_reverse
let flex_nowrap = Flex_layout.flex_nowrap

(** Re-export Flex_props utilities for backward compatibility *)
let flex_1 = Flex_props.flex_1

let flex_auto = Flex_props.flex_auto
let flex_initial = Flex_props.flex_initial
let flex_none = Flex_props.flex_none
let flex_grow = Flex_props.flex_grow
let flex_grow_0 = Flex_props.flex_grow_0
let flex_shrink = Flex_props.flex_shrink
let flex_shrink_0 = Flex_props.flex_shrink_0
let basis_0 = Flex_props.basis_0
let basis_1 = Flex_props.basis_1
let basis_auto = Flex_props.basis_auto
let basis_full = Flex_props.basis_full
let order = Flex_props.order
let order_first = Flex_props.order_first
let order_last = Flex_props.order_last
let order_none = Flex_props.order_none
