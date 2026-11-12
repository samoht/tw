(** Text transform utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Uppercase | Lowercase | Capitalize | Normal_case

  let name = "text_transform"

  (* Tailwind ranks [text-transform] after the colour block, in the priority-26
     band the late text families share. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a text-transform utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "uppercase" ] -> Ok Uppercase
    | [ "lowercase" ] -> Ok Lowercase
    | [ "capitalize" ] -> Ok Capitalize
    | [ "normal"; "case" ] -> Ok Normal_case
    | _ -> err_not_utility

  let to_class = function
    | Uppercase -> "uppercase"
    | Lowercase -> "lowercase"
    | Capitalize -> "capitalize"
    | Normal_case -> "normal-case"

  let suborder = function
    (* Text transform - alphabetical order *)
    | Capitalize -> 8360
    | Lowercase -> 8361
    | Normal_case -> 8362
    | Uppercase -> 8363

  let uppercase = style [ text_transform (Case Uppercase) ]
  let lowercase = style [ text_transform (Case Lowercase) ]
  let capitalize = style [ text_transform (Case Capitalize) ]
  let normal_case = style [ text_transform None ]

  let to_style _theme = function
    | Uppercase -> uppercase
    | Lowercase -> lowercase
    | Capitalize -> capitalize
    | Normal_case -> normal_case

  let examples = [ Capitalize ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let uppercase = utility Uppercase
let lowercase = utility Lowercase
let capitalize = utility Capitalize
let normal_case = utility Normal_case
