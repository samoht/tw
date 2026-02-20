(** Grid display utilities (grid, inline-grid).

    For grid item placement utilities (col-{i n}, row-{i n}), see Grid_item
    module. *)

module Handler = struct
  open Style
  open Css

  type t = Grid | Inline_grid
  type Utility.base += Self of t

  (** Priority for grid display utilities. Display utilities all share priority
      4 and are ordered alphabetically by suborder. *)
  let name = "grid"

  let priority = 4
  let grid = style [ display Grid ]
  let inline_grid = style [ display Inline_grid ]
  let to_style = function Grid -> grid | Inline_grid -> inline_grid

  let suborder = function
    (* Alphabetical among all display utilities (shared priority 4). block=1,
       contents=2, flex=3, flow-root=4, grid=5, hidden=6, inline=7,
       inline-block=8, inline-flex=9, inline-grid=10 *)
    | Grid -> 5
    | Inline_grid -> 10

  let err_not_utility = Error (`Msg "Not a grid display utility")

  let of_class class_name =
    let parts = Parse.split_class class_name in
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
