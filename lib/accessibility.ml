(** Accessibility utilities

    What's included:
    - `forced-color-adjust-auto`, `forced-color-adjust-none` - Control forced
      color adjustment behavior.

    What's not:
    - Other accessibility properties beyond forced-color-adjust.

    Parsing contract (`of_string`):
    - Accepts ["forced"; "color"; "adjust"; "auto" | "none"]. Unknown tokens
      yield `Error (`Msg "Not an accessibility utility")`. *)

open Style
open Css

module Handler = struct
  type t = Forced_color_adjust_auto | Forced_color_adjust_none
  type Utility.base += Self of t

  let priority = 800

  let to_style = function
    | Forced_color_adjust_auto ->
        style "forced-color-adjust-auto" [ forced_color_adjust Auto ]
    | Forced_color_adjust_none ->
        style "forced-color-adjust-none" [ forced_color_adjust None ]

  let suborder = function
    | Forced_color_adjust_auto -> 0
    | Forced_color_adjust_none -> 1

  let of_string = function
    | [ "forced"; "color"; "adjust"; "auto" ] -> Ok Forced_color_adjust_auto
    | [ "forced"; "color"; "adjust"; "none" ] -> Ok Forced_color_adjust_none
    | _ -> Error (`Msg "Not an accessibility utility")
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

let forced_color_adjust_auto = utility Forced_color_adjust_auto
let forced_color_adjust_none = utility Forced_color_adjust_none
