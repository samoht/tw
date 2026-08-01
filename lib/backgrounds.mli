(** Background and gradient utilities

    https://tailwindcss.com/docs/background-image
    https://tailwindcss.com/docs/gradient-color-stops *)

open Cascade
open Utility

(** {1 Utility Types} *)

type direction =
  | Bottom
  | Bottom_right
  | Right
  | Top_right
  | Top
  | Top_left
  | Left
  | Bottom_left

val bg : ?opacity:int -> ?shade:Color.shade -> Color.color -> t
(** [bg color] sets the background color. [shade] defaults to 500. [opacity]
    sets the alpha modifier (0-100). *)

val bg_gradient_to : direction -> t
(** [bg_gradient_to dir] sets gradient direction. Prefer this typed variant over
    the fixed functions when composing logic. *)

val from_color : ?shade:Color.shade -> Color.color -> t
(** [from_color ?shade color] sets the gradient "from" color (start stop).
    [shade] selects a color shade (e.g., 50..900) when using Tailwind colors. *)

val via_color : ?shade:Color.shade -> Color.color -> t
(** [via_color ?shade color] sets the gradient "via" color (middle stop). *)

val to_color : ?shade:Color.shade -> Color.color -> t
(** [to_color ?shade color] sets the gradient "to" color (end stop). *)

module Handler : sig
  include Utility.Handler

  (** Gradient variables for use by other modules (e.g., transition-colors) *)

  val gradient_from_var : Css.color Var.property_default
  (** [gradient_from_var] is the [--tw-gradient-from] variable. *)

  val gradient_via_var : Css.color Var.property_default
  (** [gradient_via_var] is the [--tw-gradient-via] variable. *)

  val gradient_to_var : Css.color Var.property_default
  (** [gradient_to_var] is the [--tw-gradient-to] variable. *)
end

(** {1 Background Origin, Clip, Size, Attachment and Repeat} *)

val bg_origin_border : t
(** [bg_origin_border] sets [background-origin: border-box]. *)

val bg_origin_padding : t
(** [bg_origin_padding] sets [background-origin: padding-box]. *)

val bg_origin_content : t
(** [bg_origin_content] sets [background-origin: content-box]. *)

val bg_clip_border : t
(** [bg_clip_border] sets [background-clip: border-box]. *)

val bg_clip_padding : t
(** [bg_clip_padding] sets [background-clip: padding-box]. *)

val bg_clip_content : t
(** [bg_clip_content] sets [background-clip: content-box]. *)

val bg_clip_text : t
(** [bg_clip_text] sets [background-clip: text]. *)

val bg_auto : t
(** [bg_auto] sets [background-size: auto]. *)

val bg_cover : t
(** [bg_cover] sets [background-size: cover]. *)

val bg_contain : t
(** [bg_contain] sets [background-size: contain]. *)

val bg_fixed : t
(** [bg_fixed] sets [background-attachment: fixed]. *)

val bg_local : t
(** [bg_local] sets [background-attachment: local]. *)

val bg_scroll : t
(** [bg_scroll] sets [background-attachment: scroll]. *)

val bg_repeat : t
(** [bg_repeat] sets [background-repeat: repeat]. *)

val bg_no_repeat : t
(** [bg_no_repeat] sets [background-repeat: no-repeat]. *)

val bg_repeat_x : t
(** [bg_repeat_x] sets [background-repeat: repeat-x]. *)

val bg_repeat_y : t
(** [bg_repeat_y] sets [background-repeat: repeat-y]. *)

val bg_repeat_round : t
(** [bg_repeat_round] sets [background-repeat: round]. *)

val bg_repeat_space : t
(** [bg_repeat_space] sets [background-repeat: space]. *)

(** {1 Background Position} *)

val bg_bottom : t
(** [bg_bottom] sets [background-position: bottom]. *)

val bg_bottom_left : t
(** [bg_bottom_left] sets [background-position: bottom left]. *)

val bg_bottom_right : t
(** [bg_bottom_right] sets [background-position: bottom right]. *)

val bg_center : t
(** [bg_center] sets [background-position: center]. *)

val bg_left : t
(** [bg_left] sets [background-position: left]. *)

val bg_left_bottom : t
(** [bg_left_bottom] sets [background-position: left bottom]. *)

val bg_left_top : t
(** [bg_left_top] sets [background-position: left top]. *)

val bg_right : t
(** [bg_right] sets [background-position: right]. *)

val bg_right_bottom : t
(** [bg_right_bottom] sets [background-position: right bottom]. *)

val bg_right_top : t
(** [bg_right_top] sets [background-position: right top]. *)

val bg_top : t
(** [bg_top] sets [background-position: top]. *)

val bg_top_left : t
(** [bg_top_left] sets [background-position: top left]. *)

val bg_top_right : t
(** [bg_top_right] sets [background-position: top right]. *)
