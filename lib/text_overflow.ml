(** Text overflow utilities, [truncate] included. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Text_ellipsis
    | Overflow_ellipsis (* legacy alias for text-ellipsis, keeps its name *)
    | Text_clip
    | Truncate

  let name = "text_overflow"

  (* text-overflow sorts in the priority-26 band the late text families share;
     [truncate] also writes [overflow] and sorts just before the overflow
     utilities (priority 18). *)
  let priority = function Truncate -> 17 | _ -> 26
  let err_not_utility = Error (`Msg "Not a text-overflow utility")

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "ellipsis" ] -> Ok Text_ellipsis
    | [ "overflow"; "ellipsis" ] -> Ok Overflow_ellipsis
    | [ "text"; "clip" ] -> Ok Text_clip
    | [ "truncate" ] -> Ok Truncate
    | _ -> err_not_utility

  let to_class = function
    | Text_ellipsis -> "text-ellipsis"
    | Overflow_ellipsis -> "overflow-ellipsis"
    | Text_clip -> "text-clip"
    | Truncate -> "truncate"

  let suborder = function
    (* The legacy overflow-ellipsis alias leads the canonical text-overflow
       candidates. Truncate sorts before overflow (priority 18) via a suborder
       above alignment/gap's range. Tailwind ranks [text-overflow] at 293,
       between the word-wrapping families (291/292) and hyphens (294) in
       lib/line_breaking.ml - it already lands there without adjustment. *)
    | Overflow_ellipsis -> 8319
    | Text_clip -> 8320
    | Text_ellipsis -> 8321
    | Truncate -> 9_000_000 (* priority 17, after alignment/gap (max ~2100) *)

  let text_ellipsis = style [ text_overflow Ellipsis ]
  let text_clip = style [ text_overflow Clip ]

  let truncate =
    style [ text_overflow Ellipsis; white_space Nowrap; overflow Hidden ]

  let to_style _theme = function
    | Text_ellipsis -> text_ellipsis
    | Overflow_ellipsis -> text_ellipsis
    | Text_clip -> text_clip
    | Truncate -> truncate

  let examples = [ Text_ellipsis ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let text_ellipsis = utility Text_ellipsis
let text_clip = utility Text_clip
