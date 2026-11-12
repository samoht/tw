(** Vertical align utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Baseline
    | Top
    | Middle
    | Bottom
    | Text_top
    | Text_bottom
    | Sub
    | Super
    | Arbitrary_var of string
    | Raw of string * string
  (* align-[foo]: one longhand, so the value is forwarded verbatim. *)

  let name = "vertical_align"

  (* Tailwind ranks [vertical-align] at 283, right after text-align (281) and
     text-indent (282), so it sorts with them at priority 24. *)
  let priority _ = 24
  let err_not_utility = Error (`Msg "Not a vertical-align utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "align"; v ] when Parse.is_bracket_var v ->
        Ok (Arbitrary_var (Parse.bracket_inner v))
    | [ "align"; v ] when Parse.is_bracket_value v -> (
        let inner = Parse.bracket_inner v in
        match Parse.arbitrary_declaration_value inner with
        | Some raw -> Ok (Raw (inner, raw))
        | None -> err_not_utility)
    | [ "align"; "baseline" ] -> Ok Baseline
    | [ "align"; "top" ] -> Ok Top
    | [ "align"; "middle" ] -> Ok Middle
    | [ "align"; "bottom" ] -> Ok Bottom
    | [ "align"; "text"; "top" ] -> Ok Text_top
    | [ "align"; "text"; "bottom" ] -> Ok Text_bottom
    | [ "align"; "sub" ] -> Ok Sub
    | [ "align"; "super" ] -> Ok Super
    | _ -> err_not_utility

  let to_class = function
    | Arbitrary_var s -> "align-[" ^ s ^ "]"
    | Raw (s, _) -> "align-[" ^ s ^ "]"
    | Baseline -> "align-baseline"
    | Top -> "align-top"
    | Middle -> "align-middle"
    | Bottom -> "align-bottom"
    | Text_top -> "align-text-top"
    | Text_bottom -> "align-text-bottom"
    | Sub -> "align-sub"
    | Super -> "align-super"

  let suborder = function
    (* Vertical align (priority 24) - between text-align (~1005) and font-family
       (~1501) in lib/typography.ml's suborder space. Alphabetical. *)
    | Arbitrary_var _ | Raw _ -> 1200
    | Baseline -> 1200
    | Bottom -> 1201
    | Middle -> 1202
    | Sub -> 1203
    | Super -> 1204
    | Text_bottom -> 1205
    | Text_top -> 1206
    | Top -> 1207

  let align_baseline = style [ vertical_align Baseline ]
  let align_top = style [ vertical_align Top ]
  let align_middle = style [ vertical_align Middle ]
  let align_bottom = style [ vertical_align Bottom ]
  let align_text_top = style [ vertical_align Text_top ]
  let align_text_bottom = style [ vertical_align Text_bottom ]
  let align_sub = style [ vertical_align Sub ]
  let align_super = style [ vertical_align Super ]

  let to_style _theme = function
    | Arbitrary_var var_str ->
        let bare_name = Parse.extract_var_name var_str in
        style [ vertical_align (Var (Var.bracket bare_name)) ]
    | Raw (_, v) ->
        style (Option.to_list (Parse.opaque_declaration "vertical-align" v))
    | Baseline -> align_baseline
    | Top -> align_top
    | Middle -> align_middle
    | Bottom -> align_bottom
    | Text_top -> align_text_top
    | Text_bottom -> align_text_bottom
    | Sub -> align_sub
    | Super -> align_super

  let examples = [ Baseline ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let align_baseline = utility Baseline
let align_top = utility Top
let align_middle = utility Middle
let align_bottom = utility Bottom
let align_text_top = utility Text_top
let align_text_bottom = utility Text_bottom
let align_sub = utility Sub
let align_super = utility Super
