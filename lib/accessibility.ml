module Css = Cascade.Css
open Style

module Handler = struct
  type t = Auto | No_adjust

  let name = "accessibility"
  let priority _ = 36

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
    | _ -> Utility.unclaimed

  let examples = [ Auto ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let forced_color_adjust_auto = utility Auto
let forced_color_adjust_none = utility No_adjust
