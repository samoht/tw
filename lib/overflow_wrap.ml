(** Overflow wrap utilities for controlling word wrapping behavior.

    What's included:
    - `wrap-normal` - Default word wrapping.
    - `wrap-break-word` - Break words to prevent overflow.
    - `wrap-anywhere` - Break at any character to prevent overflow. *)

module Handler = struct
  open Style
  open Css

  type t = Normal | Break_word | Anywhere
  type Utility.base += Self of t

  let name = "overflow_wrap"

  (* Typography-adjacent priority *)
  let priority = 10
  let suborder = function Anywhere -> 0 | Break_word -> 1 | Normal -> 2

  let to_class = function
    | Normal -> "wrap-normal"
    | Break_word -> "wrap-break-word"
    | Anywhere -> "wrap-anywhere"

  let to_style = function
    | Normal -> style [ overflow_wrap Normal ]
    | Break_word -> style [ overflow_wrap Break_word ]
    | Anywhere -> style [ overflow_wrap Anywhere ]

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "wrap"; "normal" ] -> Ok Normal
    | [ "wrap"; "break"; "word" ] -> Ok Break_word
    | [ "wrap"; "anywhere" ] -> Ok Anywhere
    | _ -> Error (`Msg "Not an overflow-wrap utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let wrap_normal = utility Normal
let wrap_break_word = utility Break_word
let wrap_anywhere = utility Anywhere
