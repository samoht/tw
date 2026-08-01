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
(** [bg_origin_border] sets [background-origin] to [border-box]. *)

val bg_origin_padding : t
(** [bg_origin_padding] sets [background-origin] to [padding-box]. *)

val bg_origin_content : t
(** [bg_origin_content] sets [background-origin] to [content-box]. *)

val bg_clip_border : t
(** [bg_clip_border] sets [background-clip] to [border-box]. *)

val bg_clip_padding : t
(** [bg_clip_padding] sets [background-clip] to [padding-box]. *)

val bg_clip_content : t
(** [bg_clip_content] sets [background-clip] to [content-box]. *)

val bg_clip_text : t
(** [bg_clip_text] sets [background-clip] to [text]. *)

val bg_auto : t
(** [bg_auto] sets [background-size] to [auto]. *)

val bg_cover : t
(** [bg_cover] sets [background-size] to [cover]. *)

val bg_contain : t
(** [bg_contain] sets [background-size] to [contain]. *)

val bg_fixed : t
(** [bg_fixed] sets [background-attachment] to [fixed]. *)

val bg_local : t
(** [bg_local] sets [background-attachment] to [local]. *)

val bg_scroll : t
(** [bg_scroll] sets [background-attachment] to [scroll]. *)

val bg_repeat : t
(** [bg_repeat] sets [background-repeat] to [repeat]. *)

val bg_no_repeat : t
(** [bg_no_repeat] sets [background-repeat] to [no-repeat]. *)

val bg_repeat_x : t
(** [bg_repeat_x] sets [background-repeat] to [repeat-x]. *)

val bg_repeat_y : t
(** [bg_repeat_y] sets [background-repeat] to [repeat-y]. *)

val bg_repeat_round : t
(** [bg_repeat_round] sets [background-repeat] to [round]. *)

val bg_repeat_space : t
(** [bg_repeat_space] sets [background-repeat] to [space]. *)

(** {1 Background Position} *)

val bg_bottom : t
(** [bg_bottom] sets [background-position] to [bottom]. *)

val bg_bottom_left : t
(** [bg_bottom_left] sets [background-position] to [bottom left]. *)

val bg_bottom_right : t
(** [bg_bottom_right] sets [background-position] to [bottom right]. *)

val bg_center : t
(** [bg_center] sets [background-position] to [center]. *)

val bg_left : t
(** [bg_left] sets [background-position] to [left]. *)

val bg_left_bottom : t
(** [bg_left_bottom] sets [background-position] to [left bottom]. *)

val bg_left_top : t
(** [bg_left_top] sets [background-position] to [left top]. *)

val bg_right : t
(** [bg_right] sets [background-position] to [right]. *)

val bg_right_bottom : t
(** [bg_right_bottom] sets [background-position] to [right bottom]. *)

val bg_right_top : t
(** [bg_right_top] sets [background-position] to [right top]. *)

val bg_top : t
(** [bg_top] sets [background-position] to [top]. *)

val bg_top_left : t
(** [bg_top_left] sets [background-position] to [top left]. *)

val bg_top_right : t
(** [bg_top_right] sets [background-position] to [top right]. *)
