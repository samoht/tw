(** Scrollbar utilities (scrollbar-width, scrollbar-gutter) *)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t =
    | Width_auto
    | Width_none
    | Width_thin
    | Gutter_auto
    | Gutter_stable
    | Gutter_both

  type Utility.base += Self of t

  let name = "scrollbar"
  let priority = 2

  let suborder = function
    | Width_auto -> 0
    | Width_none -> 1
    | Width_thin -> 2
    | Gutter_auto -> 10
    | Gutter_both -> 11
    | Gutter_stable -> 12

  let to_class = function
    | Width_auto -> "scrollbar-auto"
    | Width_none -> "scrollbar-none"
    | Width_thin -> "scrollbar-thin"
    | Gutter_auto -> "scrollbar-gutter-auto"
    | Gutter_stable -> "scrollbar-gutter-stable"
    | Gutter_both -> "scrollbar-gutter-both"

  let to_style = function
    | Width_auto -> style [ Css.scrollbar_width Auto ]
    | Width_none -> style [ Css.scrollbar_width None ]
    | Width_thin -> style [ Css.scrollbar_width Thin ]
    | Gutter_auto -> style [ Css.scrollbar_gutter Auto ]
    | Gutter_stable -> style [ Css.scrollbar_gutter Stable ]
    | Gutter_both -> style [ Css.scrollbar_gutter Stable_both_edges ]

  let of_class class_name =
    match Parse.split_class class_name with
    | [ "scrollbar"; "auto" ] -> Ok Width_auto
    | [ "scrollbar"; "none" ] -> Ok Width_none
    | [ "scrollbar"; "thin" ] -> Ok Width_thin
    | [ "scrollbar"; "gutter"; "auto" ] -> Ok Gutter_auto
    | [ "scrollbar"; "gutter"; "stable" ] -> Ok Gutter_stable
    | [ "scrollbar"; "gutter"; "both" ] -> Ok Gutter_both
    | _ -> Error (`Msg "Not a scrollbar utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
