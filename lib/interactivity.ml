(** Interactivity utilities for selection, scroll behavior, and more

    What's included:
    - Text selection, scroll behavior, scroll snap, resize.

    What's not:
    - Browser-specific interaction properties not surfaced in `Css`.
    - Cursor utilities (now in Cursor module).

    Parsing contract (`of_string`):
    - Accepts tokens like ["select"; "none"], ["scroll"; "smooth"],
      ["snap"; "x"], ["resize"; "y"], etc. Unknown tokens yield `Error (`Msg
      "Not an interactivity utility")`. *)

module Handler = struct
  open Style
  open Css

  let err_not_utility = Error (`Msg "Not an interactivity utility")

  type t =
    | Select_none
    | Select_text
    | Select_all
    | Select_auto
    | Scroll_auto
    | Scroll_smooth
    | Snap_start
    | Snap_end
    | Snap_center
    | Snap_none
    | Snap_x
    | Snap_y
    | Snap_both
    | Snap_mandatory
    | Snap_proximity
    | Snap_align_none
    | Snap_normal
    | Snap_always
    | Resize_none
    | Resize
    | Resize_x
    | Resize_y
    | Pointer_events_none
    | Pointer_events_auto
    | Appearance_none
    | Will_change_auto
    | Will_change_scroll
    | Will_change_contents
    | Will_change_transform
    | Group  (** Marker class for group parent *)
    | Peer  (** Marker class for peer sibling *)

  type Utility.base += Self of t

  let priority = 800
  let select_none_s = style "select-none" [ user_select None ]
  let select_text_s = style "select-text" [ user_select Text ]
  let select_all_s = style "select-all" [ user_select All ]
  let select_auto_s = style "select-auto" [ user_select Auto ]
  let scroll_auto_s = style "scroll-auto" [ scroll_behavior Auto ]
  let scroll_smooth_s = style "scroll-smooth" [ scroll_behavior Smooth ]

  (* Reference to the global scroll snap strictness variable *)
  let scroll_snap_strictness_var =
    Var.property_default Css.Scroll_snap_strictness ~initial:Proximity
      "tw-scroll-snap-strictness"

  let snap_start_s = style "snap-start" [ scroll_snap_align Start ]
  let snap_end_s = style "snap-end" [ scroll_snap_align End ]
  let snap_center_s = style "snap-center" [ scroll_snap_align Center ]
  let snap_none_s = style "snap-none" [ scroll_snap_type (Axis None) ]

  (* For snap-x, snap-y, snap-both we compose the axis with a variable reference
     to strictness *)
  let snap_x_s =
    let ref_ = Var.reference scroll_snap_strictness_var in
    let property_rules =
      match Var.property_rule scroll_snap_strictness_var with
      | Some r -> r
      | None -> Css.empty
    in
    style "snap-x" ~property_rules
      [ scroll_snap_type (Axis_with_strictness (X, Var ref_)) ]

  let snap_y_s =
    let ref_ = Var.reference scroll_snap_strictness_var in
    let property_rules =
      match Var.property_rule scroll_snap_strictness_var with
      | Some r -> r
      | None -> Css.empty
    in
    style "snap-y" ~property_rules
      [ scroll_snap_type (Axis_with_strictness (Y, Var ref_)) ]

  let snap_both_s =
    let ref_ = Var.reference scroll_snap_strictness_var in
    let property_rules =
      match Var.property_rule scroll_snap_strictness_var with
      | Some r -> r
      | None -> Css.empty
    in
    style "snap-both" ~property_rules
      [ scroll_snap_type (Axis_with_strictness (Both, Var ref_)) ]

  let snap_mandatory_s =
    let d, _ = Var.binding scroll_snap_strictness_var Mandatory in
    let property_rules =
      match Var.property_rule scroll_snap_strictness_var with
      | Some r -> r
      | None -> Css.empty
    in
    style "snap-mandatory" ~property_rules (d :: [])

  let snap_proximity_s =
    let d, _ = Var.binding scroll_snap_strictness_var Proximity in
    let property_rules =
      match Var.property_rule scroll_snap_strictness_var with
      | Some r -> r
      | None -> Css.empty
    in
    style "snap-proximity" ~property_rules (d :: [])

  let snap_align_none_s = style "snap-align-none" [ scroll_snap_align None ]
  let snap_normal_s = style "snap-normal" [ scroll_snap_stop Normal ]
  let snap_always_s = style "snap-always" [ scroll_snap_stop Always ]
  let resize_none_s = style "resize-none" [ Css.resize None ]
  let resize_s = style "resize" [ Css.resize Both ]
  let resize_x_s = style "resize-x" [ Css.resize Horizontal ]
  let resize_y_s = style "resize-y" [ Css.resize Vertical ]

  (* Additional utilities *)
  let pointer_events_none_s =
    style "pointer-events-none" [ pointer_events None ]

  let pointer_events_auto_s =
    style "pointer-events-auto" [ pointer_events Auto ]

  let appearance_none_s = style "appearance-none" [ appearance None ]
  let will_change_auto_s = style "will-change-auto" [ will_change "auto" ]

  let will_change_scroll_s =
    style "will-change-scroll" [ will_change "scroll-position" ]

  let will_change_contents_s =
    style "will-change-contents" [ will_change "contents" ]

  let will_change_transform_s =
    style "will-change-transform" [ will_change "transform" ]

  let group_s = style "group" []
  let peer_s = style "peer" []

  let to_style = function
    | Select_none -> select_none_s
    | Select_text -> select_text_s
    | Select_all -> select_all_s
    | Select_auto -> select_auto_s
    | Scroll_auto -> scroll_auto_s
    | Scroll_smooth -> scroll_smooth_s
    | Snap_start -> snap_start_s
    | Snap_end -> snap_end_s
    | Snap_center -> snap_center_s
    | Snap_none -> snap_none_s
    | Snap_x -> snap_x_s
    | Snap_y -> snap_y_s
    | Snap_both -> snap_both_s
    | Snap_mandatory -> snap_mandatory_s
    | Snap_proximity -> snap_proximity_s
    | Snap_align_none -> snap_align_none_s
    | Snap_normal -> snap_normal_s
    | Snap_always -> snap_always_s
    | Resize_none -> resize_none_s
    | Resize -> resize_s
    | Resize_x -> resize_x_s
    | Resize_y -> resize_y_s
    | Pointer_events_none -> pointer_events_none_s
    | Pointer_events_auto -> pointer_events_auto_s
    | Appearance_none -> appearance_none_s
    | Will_change_auto -> will_change_auto_s
    | Will_change_scroll -> will_change_scroll_s
    | Will_change_contents -> will_change_contents_s
    | Will_change_transform -> will_change_transform_s
    | Group -> group_s
    | Peer -> peer_s

  let of_string = function
    | [ "select"; "none" ] -> Ok Select_none
    | [ "select"; "text" ] -> Ok Select_text
    | [ "select"; "all" ] -> Ok Select_all
    | [ "select"; "auto" ] -> Ok Select_auto
    | [ "scroll"; "auto" ] -> Ok Scroll_auto
    | [ "scroll"; "smooth" ] -> Ok Scroll_smooth
    | [ "snap"; "start" ] -> Ok Snap_start
    | [ "snap"; "end" ] -> Ok Snap_end
    | [ "snap"; "center" ] -> Ok Snap_center
    | [ "snap"; "none" ] -> Ok Snap_none
    | [ "snap"; "x" ] -> Ok Snap_x
    | [ "snap"; "y" ] -> Ok Snap_y
    | [ "snap"; "both" ] -> Ok Snap_both
    | [ "snap"; "mandatory" ] -> Ok Snap_mandatory
    | [ "snap"; "proximity" ] -> Ok Snap_proximity
    | [ "snap"; "align"; "none" ] -> Ok Snap_align_none
    | [ "snap"; "normal" ] -> Ok Snap_normal
    | [ "snap"; "always" ] -> Ok Snap_always
    | [ "resize"; "none" ] -> Ok Resize_none
    | [ "resize" ] -> Ok Resize
    | [ "resize"; "x" ] -> Ok Resize_x
    | [ "resize"; "y" ] -> Ok Resize_y
    | [ "pointer"; "events"; "none" ] -> Ok Pointer_events_none
    | [ "pointer"; "events"; "auto" ] -> Ok Pointer_events_auto
    | [ "appearance"; "none" ] -> Ok Appearance_none
    | [ "will"; "change"; "auto" ] -> Ok Will_change_auto
    | [ "will"; "change"; "scroll" ] -> Ok Will_change_scroll
    | [ "will"; "change"; "contents" ] -> Ok Will_change_contents
    | [ "will"; "change"; "transform" ] -> Ok Will_change_transform
    | [ "group" ] -> Ok Group
    | [ "peer" ] -> Ok Peer
    | _ -> err_not_utility

  let suborder = function
    | Select_all -> 0
    | Select_auto -> 1
    | Select_none -> 2
    | Select_text -> 3
    | Scroll_auto -> 4
    | Scroll_smooth -> 16
    | Snap_align_none -> 17
    | Snap_always -> 18
    | Snap_both -> 19
    | Snap_center -> 20
    | Snap_end -> 21
    | Snap_mandatory -> 22
    | Snap_none -> 23
    | Snap_normal -> 24
    | Snap_proximity -> 25
    | Snap_start -> 26
    | Snap_x -> 27
    | Snap_y -> 28
    | Resize -> 29
    | Resize_none -> 30
    | Resize_x -> 31
    | Resize_y -> 32
    | Pointer_events_auto -> 33
    | Pointer_events_none -> 34
    | Appearance_none -> 35
    | Will_change_auto -> 36
    | Will_change_contents -> 37
    | Will_change_scroll -> 38
    | Will_change_transform -> 39
    | Group -> 40
    | Peer -> 41
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let select_none = utility Select_none
let select_text = utility Select_text
let select_all = utility Select_all
let select_auto = utility Select_auto
let scroll_auto = utility Scroll_auto
let scroll_smooth = utility Scroll_smooth
let snap_start = utility Snap_start
let snap_end = utility Snap_end
let snap_center = utility Snap_center
let snap_none = utility Snap_none
let snap_x = utility Snap_x
let snap_y = utility Snap_y
let snap_both = utility Snap_both
let snap_mandatory = utility Snap_mandatory
let snap_proximity = utility Snap_proximity
let snap_align_none = utility Snap_align_none
let snap_normal = utility Snap_normal
let snap_always = utility Snap_always
let resize_none = utility Resize_none
let resize = utility Resize
let resize_x = utility Resize_x
let resize_y = utility Resize_y
let pointer_events_none = utility Pointer_events_none
let pointer_events_auto = utility Pointer_events_auto
let appearance_none = utility Appearance_none
let will_change_auto = utility Will_change_auto
let will_change_scroll = utility Will_change_scroll
let will_change_contents = utility Will_change_contents
let will_change_transform = utility Will_change_transform
let group = utility Group
let peer = utility Peer
