(** Flexbox layout utilities (direction, wrap).

    These utilities control flex container layout direction and wrap behavior.
    They come after grid-template utilities in Tailwind's ordering. *)

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

  type Utility.base += Self of t

  let name = "flex_layout"

  (** Priority 16 - after grid_template (15), before alignment (17) *)
  let priority = 16

  (* Direction *)
  let flex_row = style [ flex_direction Row ]
  let flex_row_reverse = style [ flex_direction Row_reverse ]
  let flex_col = style [ flex_direction Column ]
  let flex_col_reverse = style [ flex_direction Column_reverse ]

  (* Wrap *)
  let flex_wrap_utility = style [ flex_wrap Wrap ]
  let flex_wrap_reverse_utility = style [ flex_wrap Wrap_reverse ]
  let flex_nowrap_utility = style [ flex_wrap Nowrap ]

  let to_style = function
    | Flex_row -> flex_row
    | Flex_row_reverse -> flex_row_reverse
    | Flex_col -> flex_col
    | Flex_col_reverse -> flex_col_reverse
    | Flex_wrap -> flex_wrap_utility
    | Flex_wrap_reverse -> flex_wrap_reverse_utility
    | Flex_nowrap -> flex_nowrap_utility

  let suborder : t -> int = function
    (* Direction - alphabetical: col, col-reverse, row, row-reverse *)
    | Flex_col -> 0
    | Flex_col_reverse -> 1
    | Flex_row -> 2
    | Flex_row_reverse -> 3
    (* Wrap - alphabetical: nowrap, wrap, wrap-reverse *)
    | Flex_nowrap -> 10
    | Flex_wrap -> 11
    | Flex_wrap_reverse -> 12

  let err_not_utility = Error (`Msg "Not a flex layout utility")

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
    | _ -> err_not_utility

  let to_class = function
    | Flex_row -> "flex-row"
    | Flex_row_reverse -> "flex-row-reverse"
    | Flex_col -> "flex-col"
    | Flex_col_reverse -> "flex-col-reverse"
    | Flex_wrap -> "flex-wrap"
    | Flex_wrap_reverse -> "flex-wrap-reverse"
    | Flex_nowrap -> "flex-nowrap"
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
