(** Field sizing utilities *)

module Handler = struct
  open Style
  open Css

  type t = Content | Fixed
  type Utility.base += Self of t

  let name = "field_sizing"
  let priority = 2
  let suborder = function Content -> 0 | Fixed -> 1

  let to_class = function
    | Content -> "field-sizing-content"
    | Fixed -> "field-sizing-fixed"

  let to_style = function
    | Content -> style [ field_sizing Content ]
    | Fixed -> style [ field_sizing Fixed ]

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "field"; "sizing"; "content" ] -> Ok Content
    | [ "field"; "sizing"; "fixed" ] -> Ok Fixed
    | _ -> Error (`Msg "Not a field-sizing utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let field_sizing_content = utility Content
let field_sizing_fixed = utility Fixed
