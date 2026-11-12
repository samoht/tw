(** Font smoothing utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t = Antialiased | Subpixel_antialiased

  let name = "font_smoothing"

  (* The smoothing rules sit in the priority-26 band the late text families
     share, ahead of the placeholder pseudo-element rules. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a font-smoothing utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "antialiased" ] -> Ok Antialiased
    | [ "subpixel"; "antialiased" ] -> Ok Subpixel_antialiased
    | _ -> err_not_utility

  let to_class = function
    | Antialiased -> "antialiased"
    | Subpixel_antialiased -> "subpixel-antialiased"

  let suborder = function
    (* Both simple smoothing rules lead the placeholder pseudo-element band.
       Keep them in their own slot: variant rules use their selector to settle
       ties, while base rules retain handler order. *)
    | Antialiased -> 79999
    | Subpixel_antialiased -> 79999

  let antialiased =
    style
      [ webkit_font_smoothing Antialiased; moz_osx_font_smoothing Grayscale ]

  let subpixel_antialiased =
    style [ webkit_font_smoothing Auto; moz_osx_font_smoothing Auto ]

  let to_style _theme = function
    | Antialiased -> antialiased
    | Subpixel_antialiased -> subpixel_antialiased

  let examples = [ Antialiased ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let antialiased = utility Antialiased
let subpixel_antialiased = utility Subpixel_antialiased
