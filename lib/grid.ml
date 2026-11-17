module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Grid | Inline_grid

  let name = "grid"
  let priority _ = 4
  let grid = style [ display Grid ]
  let inline_grid = style [ display Inline_grid ]
  let to_style _theme = function Grid -> grid | Inline_grid -> inline_grid

  let suborder = function
    (* Alphabetical among all display utilities (shared priority 4). block=1,
       contents=2, flex=3, flow-root=4, grid=5, hidden=6, inline=7,
       inline-block=8, inline-flex=9, inline-grid=10 *)
    | Grid -> 5
    | Inline_grid -> 10

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "grid" ] -> Ok Grid
    | [ "inline"; "grid" ] -> Ok Inline_grid
    | _ -> Utility.unclaimed

  let to_class = function Grid -> "grid" | Inline_grid -> "inline-grid"
  let examples = [ Grid ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let grid = utility Grid
let inline_grid = utility Inline_grid
