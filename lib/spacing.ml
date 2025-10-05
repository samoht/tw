(** Shared spacing utilities for padding, margin, and gap *)

open Style
open Css

(** {1 Spacing Variable} *)

let spacing_var = Theme.spacing_var

(** {1 Class Name Formatting} *)

let pp_spacing_suffix : spacing -> string = function
  | `Px -> "px"
  | `Full -> "full"
  | `Rem f ->
      let scale = f /. 0.25 in
      if Float.is_integer scale then string_of_int (abs (int_of_float scale))
      else
        let abs_scale = Float.abs scale in
        let s = string_of_float abs_scale in
        if String.contains s '.' then
          let len = String.length s in
          let rec find_end i =
            if i < 0 then 0
            else if s.[i] = '0' then find_end (i - 1)
            else if s.[i] = '.' then i
            else i + 1
          in
          String.sub s 0 (find_end (len - 1))
        else s

let pp_margin_suffix : margin -> string = function
  | `Auto -> "auto"
  | #spacing as s -> pp_spacing_suffix s

(** {1 CSS Conversion} *)

let to_length spacing_ref : spacing -> length = function
  | `Px -> Px 1.
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = int_of_float (f /. 0.25) in
      Calc
        (Calc.mul (Calc.length (Var spacing_ref)) (Calc.float (float_of_int n)))

let margin_to_length spacing_ref : margin -> length = function
  | `Auto -> Auto
  | `Px -> Px 1.
  | `Full -> Pct 100.0
  | `Rem f ->
      let n = f /. 0.25 in
      Calc (Calc.mul (Calc.length (Var spacing_ref)) (Calc.float n))

(** {1 Spacing Constructors} *)

let int n = `Rem (float_of_int n *. 0.25)

(** {1 Shared Parsing Logic} *)

(** Parse a spacing value from a string, with optional support for auto *)
let parse_value_string ~allow_auto value : margin option =
  if value = "px" then Some `Px
  else if value = "full" then Some `Full
  else if allow_auto && value = "auto" then Some `Auto
  else
    match Parse.spacing_value ~name:"spacing" value with
    | Ok f -> Some (`Rem (f *. 0.25))
    | Error _ -> None

type axis = [ `All | `X | `Y | `T | `R | `B | `L ]
(** Parse axis from a prefix string *)

let axis_of_prefix = function
  | "p" | "m" -> Some `All
  | "px" | "mx" -> Some `X
  | "py" | "my" -> Some `Y
  | "pt" | "mt" -> Some `T
  | "pr" | "mr" -> Some `R
  | "pb" | "mb" -> Some `B
  | "pl" | "ml" -> Some `L
  | _ -> None

(** Check if a prefix is for margin (vs padding) *)
let is_margin_prefix = function
  | "m" | "mx" | "my" | "mt" | "mr" | "mb" | "ml" -> true
  | _ -> false

(** Parse class name parts into (is_negative, prefix, value) *)
let parse_class_parts parts =
  match parts with
  | [ ""; prefix; value ] -> Some (true, prefix, value)
  | [ prefix; value ] -> Some (false, prefix, value)
  | _ -> None
