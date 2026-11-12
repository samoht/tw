(** Font stretch utilities. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Ultra_condensed
    | Extra_condensed
    | Condensed
    | Semi_condensed
    | Normal
    | Semi_expanded
    | Expanded
    | Extra_expanded
    | Ultra_expanded
    | Percent of int

  let name = "font_stretch"

  (* Font stretch sorts in the priority-26 band the late text families share,
     right after font style. *)
  let priority _ = 26
  let err_not_utility = Error (`Msg "Not a font-stretch utility")
  let ( >|= ) = Parse.( >|= )

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "font"; "stretch"; "ultra"; "condensed" ] -> Ok Ultra_condensed
    | [ "font"; "stretch"; "extra"; "condensed" ] -> Ok Extra_condensed
    | [ "font"; "stretch"; "condensed" ] -> Ok Condensed
    | [ "font"; "stretch"; "semi"; "condensed" ] -> Ok Semi_condensed
    | [ "font"; "stretch"; "normal" ] -> Ok Normal
    | [ "font"; "stretch"; "semi"; "expanded" ] -> Ok Semi_expanded
    | [ "font"; "stretch"; "expanded" ] -> Ok Expanded
    | [ "font"; "stretch"; "extra"; "expanded" ] -> Ok Extra_expanded
    | [ "font"; "stretch"; "ultra"; "expanded" ] -> Ok Ultra_expanded
    | [ "font"; "stretch"; n ] when String.ends_with ~suffix:"%" n ->
        let n = String.sub n 0 (String.length n - 1) in
        Parse.int_bounded ~name:"font-stretch" ~min:50 ~max:200 n >|= fun i ->
        Percent i
    | _ -> err_not_utility

  let to_class = function
    | Ultra_condensed -> "font-stretch-ultra-condensed"
    | Extra_condensed -> "font-stretch-extra-condensed"
    | Condensed -> "font-stretch-condensed"
    | Semi_condensed -> "font-stretch-semi-condensed"
    | Normal -> "font-stretch-normal"
    | Semi_expanded -> "font-stretch-semi-expanded"
    | Expanded -> "font-stretch-expanded"
    | Extra_expanded -> "font-stretch-extra-expanded"
    | Ultra_expanded -> "font-stretch-ultra-expanded"
    | Percent n -> "font-stretch-" ^ string_of_int n ^ "%"

  let suborder = function
    (* Font stretch shares one property slot after font style. *)
    | Percent _ | Condensed | Expanded | Extra_condensed | Extra_expanded
    | Normal | Semi_condensed | Semi_expanded | Ultra_condensed | Ultra_expanded
      ->
        8382

  let font_stretch_ultra_condensed = style [ font_stretch Ultra_condensed ]
  let font_stretch_extra_condensed = style [ font_stretch Extra_condensed ]
  let font_stretch_condensed = style [ font_stretch Condensed ]
  let font_stretch_semi_condensed = style [ font_stretch Semi_condensed ]
  let font_stretch_normal = style [ font_stretch Normal ]
  let font_stretch_semi_expanded = style [ font_stretch Semi_expanded ]
  let font_stretch_expanded = style [ font_stretch Expanded ]
  let font_stretch_extra_expanded = style [ font_stretch Extra_expanded ]
  let font_stretch_ultra_expanded = style [ font_stretch Ultra_expanded ]
  let font_stretch_percent n = style [ font_stretch (Pct (float_of_int n)) ]

  let to_style _theme = function
    | Ultra_condensed -> font_stretch_ultra_condensed
    | Extra_condensed -> font_stretch_extra_condensed
    | Condensed -> font_stretch_condensed
    | Semi_condensed -> font_stretch_semi_condensed
    | Normal -> font_stretch_normal
    | Semi_expanded -> font_stretch_semi_expanded
    | Expanded -> font_stretch_expanded
    | Extra_expanded -> font_stretch_extra_expanded
    | Ultra_expanded -> font_stretch_ultra_expanded
    | Percent n -> font_stretch_percent n

  let examples = [ Normal ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let font_stretch_ultra_condensed = utility Ultra_condensed
let font_stretch_extra_condensed = utility Extra_condensed
let font_stretch_condensed = utility Condensed
let font_stretch_semi_condensed = utility Semi_condensed
let font_stretch_normal = utility Normal
let font_stretch_semi_expanded = utility Semi_expanded
let font_stretch_expanded = utility Expanded
let font_stretch_extra_expanded = utility Extra_expanded
let font_stretch_ultra_expanded = utility Ultra_expanded
let font_stretch_percent n = utility (Percent n)
