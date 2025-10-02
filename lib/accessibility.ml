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

(** Local accessibility utility type *)
type t = Forced_color_adjust_auto | Forced_color_adjust_none

(** Extensible variant for accessibility utilities *)
type Utility.base += Accessibility of t

(** Error helper *)
let err_not_utility = Error (`Msg "Not an accessibility utility")

(** Typed conversion functions *)
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
  | _ -> err_not_utility

(** Priority for accessibility utilities *)
let priority = 800

(** Typed handler *)
let handler : t Utility.handler = { to_style; priority; suborder; of_string }

(** Wrapper functions for extensible variant *)
let wrap x = Accessibility x

let unwrap = function Accessibility x -> Some x | _ -> None

(** Public API *)
let utility x = Utility.base (Accessibility x)

let forced_color_adjust_auto = utility Forced_color_adjust_auto
let forced_color_adjust_none = utility Forced_color_adjust_none

(** Register handler with Utility system *)
let () = Utility.register ~wrap ~unwrap handler
