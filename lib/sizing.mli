(** Sizing utilities for width and height *)

open Core

(** {1 Width Utilities} *)

val w' : size -> t
(** [w' sz] sets width to predefined size [sz] (e.g., [`Sm], [`Full]). *)

val w : int -> t
(** [w n] sets width using spacing scale (n * 0.25rem). *)

val w_auto : t
(** Sets width to auto. *)

val w_full : t
(** Sets width to 100%. *)

val w_screen : t
(** Sets width to 100vw. *)

val w_min : t
(** Sets width to min-content. *)

val w_max : t
(** Sets width to max-content. *)

val w_fit : t
(** Sets width to fit-content. *)

val w_1_2 : t
(** Sets width to 1/2. *)

val w_1_3 : t
(** Sets width to 1/3. *)

val w_2_3 : t
(** Sets width to 2/3. *)

val w_1_4 : t
(** Sets width to 1/4. *)

val w_3_4 : t
(** Sets width to 3/4. *)

val w_1_5 : t
(** Sets width to 1/5. *)

val w_2_5 : t
(** Sets width to 2/5. *)

val w_3_5 : t
(** Sets width to 3/5. *)

val w_4_5 : t
(** Sets width to 4/5. *)

val h' : size -> t
(** [h' sz] sets height to predefined size [sz]. *)

val h : int -> t
(** [h n] sets height using spacing scale (n * 0.25rem). *)

val h_auto : t
(** Sets height to auto. *)

val h_full : t
(** Sets height to 100%. *)

val h_screen : t
(** Sets height to 100vh. *)

val h_min : t
(** Sets height to min-content. *)

val h_max : t
(** Sets height to max-content. *)

val h_fit : t
(** Sets height to fit-content. *)

val h_1_2 : t
(** Sets height to 1/2. *)

val h_1_3 : t
(** Sets height to 1/3. *)

val h_2_3 : t
(** Sets height to 2/3. *)

val h_1_4 : t
(** Sets height to 1/4. *)

val h_3_4 : t
(** Sets height to 3/4. *)

val h_1_5 : t
(** Sets height to 1/5. *)

val h_2_5 : t
(** Sets height to 2/5. *)

val h_3_5 : t
(** Sets height to 3/5. *)

val h_4_5 : t
(** Sets height to 4/5. *)

val min_w' : size -> t
(** [min_w' sz] sets min-width to predefined size [sz]. *)

val min_w : int -> t
(** [min_w n] sets min-width using spacing scale. *)

val min_w_0 : t
(** Sets min-width to 0. *)

val min_w_full : t
(** Sets min-width to 100%. *)

val min_w_min : t
(** Sets min-width to min-content. *)

val min_w_max : t
(** Sets min-width to max-content. *)

val min_w_fit : t
(** Sets min-width to fit-content. *)

val max_w' : size -> t
(** [max_w' sz] sets max-width to predefined size [sz]. *)

val max_w : int -> t
(** [max_w n] sets max-width using spacing scale. *)

val max_w_none : t
(** Removes max-width constraint. *)

val max_w_xs : t
(** Sets max-width to xs container width. *)

val max_w_sm : t
(** Sets max-width to sm container width. *)

val max_w_md : t
(** Sets max-width to md container width. *)

val max_w_lg : t
(** Sets max-width to lg container width. *)

val max_w_xl : t
(** Sets max-width to xl container width. *)

val max_w_2xl : t
(** Sets max-width to 2xl container width. *)

val max_w_3xl : t
(** Sets max-width to 3xl container width. *)

val max_w_4xl : t
(** Sets max-width to 4xl container width. *)

val max_w_5xl : t
(** Sets max-width to 5xl container width. *)

val max_w_6xl : t
(** Sets max-width to 6xl container width. *)

val max_w_7xl : t
(** Sets max-width to 7xl container width. *)

val max_w_full : t
(** Sets max-width to 100%. *)

val max_w_min : t
(** Sets max-width to min-content. *)

val max_w_max : t
(** Sets max-width to max-content. *)

val max_w_fit : t
(** Sets max-width to fit-content. *)

val max_w_prose : t
(** Sets max-width to a readable measure for prose. *)

val max_w_screen_sm : t
(** Sets max-width to small breakpoint width. *)

val max_w_screen_md : t
(** Sets max-width to medium breakpoint width. *)

val max_w_screen_lg : t
(** Sets max-width to large breakpoint width. *)

val max_w_screen_xl : t
(** Sets max-width to extra-large breakpoint width. *)

val max_w_screen_2xl : t
(** Sets max-width to 2× extra-large breakpoint width. *)

val min_h' : size -> t
(** [min_h' sz] sets min-height to predefined size [sz]. *)

val min_h : int -> t
(** [min_h n] sets min-height using spacing scale. *)

val min_h_0 : t
(** Sets min-height to 0. *)

val min_h_full : t
(** Sets min-height to 100%. *)

val min_h_screen : t
(** Sets min-height to 100vh. *)

val min_h_min : t
(** Sets min-height to min-content. *)

val min_h_max : t
(** Sets min-height to max-content. *)

val min_h_fit : t
(** Sets min-height to fit-content. *)

val max_h' : size -> t
(** [max_h' sz] sets max-height to predefined size [sz]. *)

val max_h : int -> t
(** [max_h n] sets max-height using spacing scale. *)

val max_h_none : t
(** Removes max-height constraint. *)

val max_h_full : t
(** Sets max-height to 100%. *)

val max_h_screen : t
(** Sets max-height to 100vh. *)

val max_h_min : t
(** Sets max-height to min-content. *)

val max_h_max : t
(** Sets max-height to max-content. *)

val max_h_fit : t
(** Sets max-height to fit-content. *)

(** {1 String Parsing} *)

val of_string : string list -> (t, [ `Msg of string ]) result
(** [of_string parts] parses a list of strings into a sizing utility. Returns
    [Ok t] if successful, or [Error msg] if the input is invalid. *)
