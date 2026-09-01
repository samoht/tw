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

val bg : ?opacity:int -> ?shade:int -> Color.color -> t
(** [bg color] sets the background color. [shade] defaults to 500. [opacity]
    sets the alpha modifier (0-100). *)

val bg_transparent : t
(** [bg_transparent] makes the background fully transparent. *)

val bg_current : t
(** [bg_current] uses [currentColor] for the background. *)

(** {1 Background Attachment} *)

val bg_fixed : t
(** [bg_fixed] fixes the background relative to the viewport. *)

val bg_local : t
(** [bg_local] scrolls the background with the element's contents. *)

val bg_scroll : t
(** [bg_scroll] fixes the background relative to the element. *)

(** {1 Background Clip} *)

val bg_clip_border : t
(** [bg_clip_border] clips the background to the border box. *)

val bg_clip_padding : t
(** [bg_clip_padding] clips the background to the padding box. *)

val bg_clip_content : t
(** [bg_clip_content] clips the background to the content box. *)

val bg_clip_text : t
(** [bg_clip_text] clips the background to the foreground text. *)

(** {1 Background Origin} *)

val bg_origin_border : t
(** [bg_origin_border] positions the background from the border box. *)

val bg_origin_padding : t
(** [bg_origin_padding] positions the background from the padding box. *)

val bg_origin_content : t
(** [bg_origin_content] positions the background from the content box. *)

(** {1 Background Position} *)

val bg_bottom : t
(** [bg_bottom] positions the background at the bottom. *)

val bg_bottom_left : t
(** [bg_bottom_left] positions the background at the bottom left. *)

val bg_bottom_right : t
(** [bg_bottom_right] positions the background at the bottom right. *)

val bg_center : t
(** [bg_center] centers the background. *)

val bg_left : t
(** [bg_left] positions the background at the left. *)

val bg_left_bottom : t
(** [bg_left_bottom] positions the background at the left bottom. *)

val bg_left_top : t
(** [bg_left_top] positions the background at the left top. *)

val bg_right : t
(** [bg_right] positions the background at the right. *)

val bg_right_bottom : t
(** [bg_right_bottom] positions the background at the right bottom. *)

val bg_right_top : t
(** [bg_right_top] positions the background at the right top. *)

val bg_top : t
(** [bg_top] positions the background at the top. *)

val bg_top_left : t
(** [bg_top_left] positions the background at the top left. *)

val bg_top_right : t
(** [bg_top_right] positions the background at the top right. *)

(** {1 Background Repeat} *)

val bg_repeat : t
(** [bg_repeat] repeats the background on both axes. *)

val bg_no_repeat : t
(** [bg_no_repeat] prevents the background from repeating. *)

val bg_repeat_x : t
(** [bg_repeat_x] repeats the background horizontally. *)

val bg_repeat_y : t
(** [bg_repeat_y] repeats the background vertically. *)

val bg_repeat_round : t
(** [bg_repeat_round] repeats and scales the background to avoid clipping. *)

val bg_repeat_space : t
(** [bg_repeat_space] repeats the background with space between tiles. *)

(** {1 Background Size} *)

val bg_auto : t
(** [bg_auto] uses the background image's intrinsic size. *)

val bg_cover : t
(** [bg_cover] scales the background to cover its container. *)

val bg_contain : t
(** [bg_contain] scales the background to fit inside its container. *)

val bg_gradient_to : direction -> t
(** [bg_gradient_to dir] sets gradient direction. Prefer this typed variant over
    the fixed functions when composing logic. *)

val from_color : ?shade:int -> Color.color -> t
(** [from_color ?shade color] sets the gradient "from" color (start stop).
    [shade] selects a color shade (e.g., 50..900) when using Tailwind colors. *)

val via_color : ?shade:int -> Color.color -> t
(** [via_color ?shade color] sets the gradient "via" color (middle stop). *)

val to_color : ?shade:int -> Color.color -> t
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
