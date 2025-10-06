(** Grid display utilities (grid, inline-grid).

    For grid item placement utilities (col-{i n}, row-{i n}), see Grid_item
    module. *)

module Handler = struct
  open Style
  open Css

  type t = Grid | Inline_grid
  type Utility.base += Self of t

  (** Priority for grid display utilities. Set to 4 to match other display
      utilities (block, inline, flex, etc.) since grid and inline-grid set the
      display property. *)
  let name = "grid"

  let priority = 4
  let grid = style [ display Grid ]
  let inline_grid = style [ display Inline_grid ]
  let to_style = function Grid -> grid | Inline_grid -> inline_grid

  let suborder = function
    (* Display utilities - alphabetical order among all display utilities:
       block(1), flex(2), grid(3), inline(4), inline-block(5/6), inline-flex(7),
       inline-grid(8) *)
    | Grid -> 3
    | Inline_grid -> 8

  let err_not_utility = Error (`Msg "Not a grid display utility")

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "grid" ] -> Ok Grid
    | [ "inline"; "grid" ] -> Ok Inline_grid
    | _ -> err_not_utility

  let to_class = function Grid -> "grid" | Inline_grid -> "inline-grid"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let grid = utility Grid
let inline_grid = utility Inline_grid
