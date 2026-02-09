(** Touch-action utilities

    @see <https://tailwindcss.com/docs/touch-action>
      Tailwind CSS Touch Action documentation *)

module Handler = struct
  open Style
  open Css

  type t =
    | Touch_auto
    | Touch_none
    | Touch_manipulation
    | Touch_pan_x
    | Touch_pan_y

  type Utility.base += Self of t

  let name = "touch"
  let priority = 12

  let to_class = function
    | Touch_auto -> "touch-auto"
    | Touch_none -> "touch-none"
    | Touch_manipulation -> "touch-manipulation"
    | Touch_pan_x -> "touch-pan-x"
    | Touch_pan_y -> "touch-pan-y"

  let to_style = function
    | Touch_auto -> style [ touch_action Auto ]
    | Touch_none -> style [ touch_action None ]
    | Touch_manipulation -> style [ touch_action Manipulation ]
    | Touch_pan_x -> style [ touch_action Pan_x ]
    | Touch_pan_y -> style [ touch_action Pan_y ]

  let suborder = function
    | Touch_auto -> 0
    | Touch_none -> 1
    | Touch_manipulation -> 2
    | Touch_pan_x -> 3
    | Touch_pan_y -> 4

  let of_class class_name =
    match class_name with
    | "touch-auto" -> Ok Touch_auto
    | "touch-none" -> Ok Touch_none
    | "touch-manipulation" -> Ok Touch_manipulation
    | "touch-pan-x" -> Ok Touch_pan_x
    | "touch-pan-y" -> Ok Touch_pan_y
    | _ -> Error (`Msg "Not a touch-action utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let touch_auto = utility Touch_auto
let touch_none = utility Touch_none
let touch_manipulation = utility Touch_manipulation
let touch_pan_x = utility Touch_pan_x
let touch_pan_y = utility Touch_pan_y
