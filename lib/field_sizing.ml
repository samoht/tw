(** Field sizing utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Content | Fixed

  let name = "field_sizing"
  let priority _ = 5
  let suborder = function Content -> 0 | Fixed -> 1

  let to_class = function
    | Content -> "field-sizing-content"
    | Fixed -> "field-sizing-fixed"

  let to_style _theme = function
    | Content -> style [ field_sizing Content ]
    | Fixed -> style [ field_sizing Fixed ]

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "field"; "sizing"; "content" ] -> Ok Content
    | [ "field"; "sizing"; "fixed" ] -> Ok Fixed
    | _ -> Error (`Msg "Not a field-sizing utility")

  let examples = [ Content ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let field_sizing_content = utility Content
let field_sizing_fixed = utility Fixed
