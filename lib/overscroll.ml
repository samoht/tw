(** Overscroll behavior utilities for controlling scroll chaining.

    What's included:
    - `overscroll-auto` - Default scroll chaining behavior.
    - `overscroll-contain` - Prevent scroll chaining to parent.
    - `overscroll-none` - Prevent scroll chaining and overscroll effects.
    - `overscroll-x-*`, `overscroll-y-*` - Axis-specific overscroll behavior.

    Parsing contract ([of_class]):
    - Accepts ["overscroll"; value] and ["overscroll"; axis; value].
    - Unknown tokens yield [Error (`Msg "Not an overscroll utility")]. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Auto
    | Contain
    | None_
    | X_auto
    | X_contain
    | X_none
    | Y_auto
    | Y_contain
    | Y_none

  type Utility.base += Self of t

  let name = "overscroll"

  (* Same priority as overflow (18) - these are related utilities *)
  let priority = 18

  let suborder = function
    | Auto -> 600
    | Contain -> 601
    | None_ -> 602
    | X_auto -> 603
    | X_contain -> 604
    | X_none -> 605
    | Y_auto -> 606
    | Y_contain -> 607
    | Y_none -> 608

  let to_class = function
    | Auto -> "overscroll-auto"
    | Contain -> "overscroll-contain"
    | None_ -> "overscroll-none"
    | X_auto -> "overscroll-x-auto"
    | X_contain -> "overscroll-x-contain"
    | X_none -> "overscroll-x-none"
    | Y_auto -> "overscroll-y-auto"
    | Y_contain -> "overscroll-y-contain"
    | Y_none -> "overscroll-y-none"

  let to_style = function
    | Auto -> style [ overscroll_behavior Auto ]
    | Contain -> style [ overscroll_behavior Contain ]
    | None_ -> style [ overscroll_behavior None ]
    | X_auto -> style [ overscroll_behavior_x Auto ]
    | X_contain -> style [ overscroll_behavior_x Contain ]
    | X_none -> style [ overscroll_behavior_x None ]
    | Y_auto -> style [ overscroll_behavior_y Auto ]
    | Y_contain -> style [ overscroll_behavior_y Contain ]
    | Y_none -> style [ overscroll_behavior_y None ]

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "overscroll"; "auto" ] -> Ok Auto
    | [ "overscroll"; "contain" ] -> Ok Contain
    | [ "overscroll"; "none" ] -> Ok None_
    | [ "overscroll"; "x"; "auto" ] -> Ok X_auto
    | [ "overscroll"; "x"; "contain" ] -> Ok X_contain
    | [ "overscroll"; "x"; "none" ] -> Ok X_none
    | [ "overscroll"; "y"; "auto" ] -> Ok Y_auto
    | [ "overscroll"; "y"; "contain" ] -> Ok Y_contain
    | [ "overscroll"; "y"; "none" ] -> Ok Y_none
    | _ -> Error (`Msg "Not an overscroll utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let overscroll_auto = utility Auto
let overscroll_contain = utility Contain
let overscroll_none = utility None_
let overscroll_x_auto = utility X_auto
let overscroll_x_contain = utility X_contain
let overscroll_x_none = utility X_none
let overscroll_y_auto = utility Y_auto
let overscroll_y_contain = utility Y_contain
let overscroll_y_none = utility Y_none
