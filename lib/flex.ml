(** Flexbox utilities

    @see <https://tailwindcss.com/docs/flex> Tailwind CSS Flex documentation
    @see <https://tailwindcss.com/docs/flex-direction> Flex Direction
    @see <https://tailwindcss.com/docs/flex-wrap> Flex Wrap
    @see <https://tailwindcss.com/docs/flex-grow> Flex Grow
    @see <https://tailwindcss.com/docs/flex-shrink> Flex Shrink *)

module Handler = struct
  open Style
  open Css

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

  type Utility.base += Self of t

  (** Priority for flex utilities. Set to 4 to match other display utilities
      (block, inline, grid, etc.) since flex and inline-flex set the display
      property. *)
  let name = "flex"

  let priority = 4

  (* Helper functions returning Style.t *)

  (* Display *)
  let flex = style [ Css.display Flex ]
  let inline_flex = style [ Css.display Inline_flex ]

  (* Direction *)
  let flex_row = style [ Css.flex_direction Row ]
  let flex_row_reverse = style [ Css.flex_direction Row_reverse ]
  let flex_col = style [ Css.flex_direction Column ]
  let flex_col_reverse = style [ Css.flex_direction Column_reverse ]

  (* Wrap *)
  let flex_wrap = style [ Css.flex_wrap Wrap ]
  let flex_wrap_reverse = style [ Css.flex_wrap Wrap_reverse ]
  let flex_nowrap = style [ Css.flex_wrap Nowrap ]

  (* Flex shortcuts *)
  let flex_1 = style [ Css.flex (Full (1.0, 1.0, Pct 0.0)) ]
  let flex_auto = style [ Css.flex Auto ]
  let flex_initial = style [ Css.flex Initial ]
  let flex_none = style [ Css.flex None ]

  (* Grow *)
  let flex_grow = style [ Css.flex_grow 1.0 ]
  let flex_grow_0 = style [ Css.flex_grow 0.0 ]

  (* Shrink *)
  let flex_shrink = style [ Css.flex_shrink 1.0 ]
  let flex_shrink_0 = style [ Css.flex_shrink 0.0 ]

  (* Basis *)
  let basis_0 = style [ Css.flex_basis Zero ]
  let basis_1 = style [ Css.flex_basis (Pct 100.0) ]
  let basis_auto = style [ Css.flex_basis Auto ]
  let basis_full = style [ Css.flex_basis (Pct 100.0) ]

  (* Order *)
  let order_style n = style [ Css.order n ]
  let order_first = style [ Css.order (-9999) ]
  let order_last = style [ Css.order 9999 ]
  let order_none = style [ Css.order 0 ]

  let to_style = function
    | Flex -> flex
    | Inline_flex -> inline_flex
    | Flex_row -> flex_row
    | Flex_row_reverse -> flex_row_reverse
    | Flex_col -> flex_col
    | Flex_col_reverse -> flex_col_reverse
    | Flex_wrap -> flex_wrap
    | Flex_wrap_reverse -> flex_wrap_reverse
    | Flex_nowrap -> flex_nowrap
    | Flex_1 -> flex_1
    | Flex_auto -> flex_auto
    | Flex_initial -> flex_initial
    | Flex_none -> flex_none
    | Flex_grow -> flex_grow
    | Flex_grow_0 -> flex_grow_0
    | Flex_shrink -> flex_shrink
    | Flex_shrink_0 -> flex_shrink_0
    | Basis_0 -> basis_0
    | Basis_1 -> basis_1
    | Basis_auto -> basis_auto
    | Basis_full -> basis_full
    | Order n -> order_style n
    | Order_first -> order_first
    | Order_last -> order_last
    | Order_none -> order_none

  let suborder : t -> int = function
    (* Display - suborder matches alphabetical position across all display
       utilities *)
    | Flex -> 2
    | Inline_flex -> 7
    (* Direction - alphabetical order: col, col-reverse, row, row-reverse *)
    | Flex_col -> 10
    | Flex_col_reverse -> 11
    | Flex_row -> 12
    | Flex_row_reverse -> 13
    (* Wrap - alphabetical order: nowrap, wrap, wrap-reverse *)
    | Flex_nowrap -> 20
    | Flex_wrap -> 21
    | Flex_wrap_reverse -> 22
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

  let of_class class_name =
    let err_not_utility = Error (`Msg "Not a flex utility") in
    let parts = String.split_on_char '-' class_name in
    match parts with
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
        | Some n when n >= 1 && n <= 6 -> Ok (Order n)
        | _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class = function
    (* Display *)
    | Flex -> "flex"
    | Inline_flex -> "inline-flex"
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

(** Register the flex utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let flex = utility Flex
let inline_flex = utility Inline_flex
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
