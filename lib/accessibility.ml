(** Accessibility utilities

    What's included:
    - `forced-color-adjust-auto`, `forced-color-adjust-none` - Control forced
      color adjustment behavior.

    What's not:
    - Other accessibility properties beyond forced-color-adjust.

    Parsing contract (`of_string`):
    - Accepts ["forced"; "color"; "adjust"; "auto" | "none"]. Unknown tokens
      yield `Error (`Msg "Not an accessibility utility")`. *)

module Css = Cascade.Css
open Style

module Handler = struct
  type t = Auto | No_adjust
  type Utility.base += Self of t

  let name = "accessibility"
  let priority _ = 29

  let to_class = function
    | Auto -> "forced-color-adjust-auto"
    | No_adjust -> "forced-color-adjust-none"

  let to_style _theme = function
    | Auto -> style [ Css.forced_color_adjust Css.Auto ]
    | No_adjust -> style [ Css.forced_color_adjust Css.None ]

  let suborder = function Auto -> 0 | No_adjust -> 1

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "forced"; "color"; "adjust"; "auto" ] -> Ok Auto
    | [ "forced"; "color"; "adjust"; "none" ] -> Ok No_adjust
    | _ -> Error (`Msg "Not an accessibility utility")

  let examples = [ Auto ]
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let forced_color_adjust_auto = utility Auto
let forced_color_adjust_none = utility No_adjust
