(** Box sizing utilities *)

module Handler = struct
  open Style
  open Css

  type t = Border | Content
  type Utility.base += Self of t

  let name = "box_sizing"
  let priority = 2
  let suborder = function Border -> 0 | Content -> 1
  let to_class = function Border -> "box-border" | Content -> "box-content"

  let to_style = function
    | Border -> style [ box_sizing Border_box ]
    | Content -> style [ box_sizing Content_box ]

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "box"; "border" ] -> Ok Border
    | [ "box"; "content" ] -> Ok Content
    | _ -> Error (`Msg "Not a box-sizing utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let box_border = utility Border
let box_content = utility Content
