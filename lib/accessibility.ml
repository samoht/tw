(** Accessibility utilities *)

open Core
open Css

let forced_color_adjust_auto =
  style "forced-color-adjust-auto" [ forced_color_adjust Auto ]

let forced_color_adjust_none =
  style "forced-color-adjust-none" [ forced_color_adjust None ]

(** {1 Parsing Functions} *)

let of_string = function
  | [ "forced-color-adjust"; "auto" ] -> Ok forced_color_adjust_auto
  | [ "forced-color-adjust"; "none" ] -> Ok forced_color_adjust_none
  | parts ->
      Error
        (`Msg
           (Pp.str
              [ "Unknown accessibility utility: "; String.concat "-" parts ]))
