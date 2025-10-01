(** Accessibility utilities *)

open Style
open Css

(** {1 Accessibility Utility Type} *)

type utility = Forced_color_adjust_auto | Forced_color_adjust_none

let forced_color_adjust_auto =
  style "forced-color-adjust-auto" [ forced_color_adjust Auto ]

let forced_color_adjust_none =
  style "forced-color-adjust-none" [ forced_color_adjust None ]

(** {1 Utility Conversion Functions} *)

let to_style = function
  | Forced_color_adjust_auto -> forced_color_adjust_auto
  | Forced_color_adjust_none -> forced_color_adjust_none

let of_string = function
  | [ "forced"; "color"; "adjust"; "auto" ] -> Ok Forced_color_adjust_auto
  | [ "forced"; "color"; "adjust"; "none" ] -> Ok Forced_color_adjust_none
  | _ -> Error (`Msg "Not an accessibility utility")

let suborder = function
  | Forced_color_adjust_auto -> 0
  | Forced_color_adjust_none -> 1
