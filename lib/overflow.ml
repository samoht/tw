(** Overflow utilities for controlling element overflow behavior.

    These utilities are in their own module to get correct ordering - they
    should appear after display, sizing, flex, alignment, and gap utilities but
    before borders. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Auto
    | Hidden
    | Clip
    | Visible
    | Scroll
    | X_auto
    | X_clip
    | X_hidden
    | X_visible
    | X_scroll
    | Y_auto
    | Y_clip
    | Y_hidden
    | Y_visible
    | Y_scroll

  type Utility.base += Self of t

  let name = "overflow"

  (* Overflow comes after alignment (17) in Tailwind's utility ordering. *)
  let priority = 18

  (* Keep stable relative order among overflow variants. *)
  let suborder = function
    | Auto -> 550
    | Clip -> 551
    | Hidden -> 552
    | Scroll -> 553
    | Visible -> 554
    | X_auto -> 555
    | X_clip -> 556
    | X_hidden -> 557
    | X_scroll -> 558
    | X_visible -> 559
    | Y_auto -> 560
    | Y_clip -> 561
    | Y_hidden -> 562
    | Y_scroll -> 563
    | Y_visible -> 564

  let to_class = function
    | Auto -> "overflow-auto"
    | Hidden -> "overflow-hidden"
    | Clip -> "overflow-clip"
    | Visible -> "overflow-visible"
    | Scroll -> "overflow-scroll"
    | X_auto -> "overflow-x-auto"
    | X_clip -> "overflow-x-clip"
    | X_hidden -> "overflow-x-hidden"
    | X_visible -> "overflow-x-visible"
    | X_scroll -> "overflow-x-scroll"
    | Y_auto -> "overflow-y-auto"
    | Y_clip -> "overflow-y-clip"
    | Y_hidden -> "overflow-y-hidden"
    | Y_visible -> "overflow-y-visible"
    | Y_scroll -> "overflow-y-scroll"

  let to_style = function
    | Auto -> style [ overflow Auto ]
    | Hidden -> style [ overflow Hidden ]
    | Clip -> style [ overflow Clip ]
    | Visible -> style [ overflow Visible ]
    | Scroll -> style [ overflow Scroll ]
    | X_auto -> style [ overflow_x Auto ]
    | X_clip -> style [ overflow_x Clip ]
    | X_hidden -> style [ overflow_x Hidden ]
    | X_visible -> style [ overflow_x Visible ]
    | X_scroll -> style [ overflow_x Scroll ]
    | Y_auto -> style [ overflow_y Auto ]
    | Y_clip -> style [ overflow_y Clip ]
    | Y_hidden -> style [ overflow_y Hidden ]
    | Y_visible -> style [ overflow_y Visible ]
    | Y_scroll -> style [ overflow_y Scroll ]

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "overflow"; "auto" ] -> Ok Auto
    | [ "overflow"; "hidden" ] -> Ok Hidden
    | [ "overflow"; "clip" ] -> Ok Clip
    | [ "overflow"; "visible" ] -> Ok Visible
    | [ "overflow"; "scroll" ] -> Ok Scroll
    | [ "overflow"; "x"; "auto" ] -> Ok X_auto
    | [ "overflow"; "x"; "clip" ] -> Ok X_clip
    | [ "overflow"; "x"; "hidden" ] -> Ok X_hidden
    | [ "overflow"; "x"; "visible" ] -> Ok X_visible
    | [ "overflow"; "x"; "scroll" ] -> Ok X_scroll
    | [ "overflow"; "y"; "auto" ] -> Ok Y_auto
    | [ "overflow"; "y"; "clip" ] -> Ok Y_clip
    | [ "overflow"; "y"; "hidden" ] -> Ok Y_hidden
    | [ "overflow"; "y"; "visible" ] -> Ok Y_visible
    | [ "overflow"; "y"; "scroll" ] -> Ok Y_scroll
    | _ -> Error (`Msg "Not an overflow utility")
end

open Handler

let () = Utility.register (module Handler)

(** {1 Public API} *)

let utility x = Utility.base (Self x)
let overflow_auto = utility Auto
let overflow_hidden = utility Hidden
let overflow_clip = utility Clip
let overflow_visible = utility Visible
let overflow_scroll = utility Scroll
let overflow_x_auto = utility X_auto
let overflow_x_clip = utility X_clip
let overflow_x_hidden = utility X_hidden
let overflow_x_visible = utility X_visible
let overflow_x_scroll = utility X_scroll
let overflow_y_auto = utility Y_auto
let overflow_y_clip = utility Y_clip
let overflow_y_hidden = utility Y_hidden
let overflow_y_visible = utility Y_visible
let overflow_y_scroll = utility Y_scroll
