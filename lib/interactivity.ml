(** Interactivity utilities for selection, scroll behavior, and more

    What's included:
    - Text selection, scroll behavior, scroll snap, resize.

    What's not:
    - Browser-specific interaction properties not surfaced in `Css`.
    - Cursor utilities (now in Cursor module).

    Parsing contract (`of_string`):
    - Accepts tokens like ["select"; "none"], ["scroll"; "smooth"], ["snap"; "x"],
      ["resize"; "y"], etc. Unknown tokens yield `Error (`Msg "Not an
      interactivity utility")`. *)

open Style
open Css

(** {1 Utility Type} *)

type utility =
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

(** {1 User Select Utilities} *)

let select_none = style "select-none" [ user_select None ]
let select_text = style "select-text" [ user_select Text ]
let select_all = style "select-all" [ user_select All ]
let select_auto = style "select-auto" [ user_select Auto ]


(** {1 Scroll Behavior Utilities} *)

let scroll_auto = style "scroll-auto" [ scroll_behavior Auto ]
let scroll_smooth = style "scroll-smooth" [ scroll_behavior Smooth ]

(** {1 Scroll Snap Utilities} *)

(* Reference to the global scroll snap strictness variable *)
let scroll_snap_strictness_var =
  Var.property_default Css.Scroll_snap_strictness ~initial:Proximity
    "tw-scroll-snap-strictness"

let snap_start = style "snap-start" [ scroll_snap_align Start ]
let snap_end = style "snap-end" [ scroll_snap_align End ]
let snap_center = style "snap-center" [ scroll_snap_align Center ]
let snap_none = style "snap-none" [ scroll_snap_type (Axis None) ]

(* For snap-x, snap-y, snap-both we compose the axis with a variable reference
   to strictness *)
let snap_x =
  let ref_ = Var.reference scroll_snap_strictness_var in
  let property_rules =
    match Var.property_rule scroll_snap_strictness_var with
    | Some r -> r
    | None -> Css.empty
  in
  style "snap-x" ~property_rules
    [ scroll_snap_type (Axis_with_strictness (X, Var ref_)) ]

let snap_y =
  let ref_ = Var.reference scroll_snap_strictness_var in
  let property_rules =
    match Var.property_rule scroll_snap_strictness_var with
    | Some r -> r
    | None -> Css.empty
  in
  style "snap-y" ~property_rules
    [ scroll_snap_type (Axis_with_strictness (Y, Var ref_)) ]

let snap_both =
  let ref_ = Var.reference scroll_snap_strictness_var in
  let property_rules =
    match Var.property_rule scroll_snap_strictness_var with
    | Some r -> r
    | None -> Css.empty
  in
  style "snap-both" ~property_rules
    [ scroll_snap_type (Axis_with_strictness (Both, Var ref_)) ]

let snap_mandatory =
  let d, _ = Var.binding scroll_snap_strictness_var Mandatory in
  let property_rules =
    match Var.property_rule scroll_snap_strictness_var with
    | Some r -> r
    | None -> Css.empty
  in
  style "snap-mandatory" ~property_rules (d :: [])

let snap_proximity =
  let d, _ = Var.binding scroll_snap_strictness_var Proximity in
  let property_rules =
    match Var.property_rule scroll_snap_strictness_var with
    | Some r -> r
    | None -> Css.empty
  in
  style "snap-proximity" ~property_rules (d :: [])

let snap_align_none = style "snap-align-none" [ scroll_snap_align None ]
let snap_normal = style "snap-normal" [ scroll_snap_stop Normal ]
let snap_always = style "snap-always" [ scroll_snap_stop Always ]

(** {1 Resize Utilities} *)

let resize_none = style "resize-none" [ Css.resize None ]
let resize = style "resize" [ Css.resize Both ]
let resize_x = style "resize-x" [ Css.resize Horizontal ]
let resize_y = style "resize-y" [ Css.resize Vertical ]

(* Additional utilities *)
let pointer_events_none = style "pointer-events-none" [ pointer_events None ]
let pointer_events_auto = style "pointer-events-auto" [ pointer_events Auto ]
let appearance_none = style "appearance-none" [ appearance None ]
let will_change_auto = style "will-change-auto" [ will_change "auto" ]

let will_change_scroll =
  style "will-change-scroll" [ will_change "scroll-position" ]

let will_change_contents =
  style "will-change-contents" [ will_change "contents" ]

let will_change_transform =
  style "will-change-transform" [ will_change "transform" ]

(** {1 Conversion Functions} *)

let to_style = function
  | Select_none -> select_none
  | Select_text -> select_text
  | Select_all -> select_all
  | Select_auto -> select_auto
  | Scroll_auto -> scroll_auto
  | Scroll_smooth -> scroll_smooth
  | Snap_start -> snap_start
  | Snap_end -> snap_end
  | Snap_center -> snap_center
  | Snap_none -> snap_none
  | Snap_x -> snap_x
  | Snap_y -> snap_y
  | Snap_both -> snap_both
  | Snap_mandatory -> snap_mandatory
  | Snap_proximity -> snap_proximity
  | Snap_align_none -> snap_align_none
  | Snap_normal -> snap_normal
  | Snap_always -> snap_always
  | Resize_none -> resize_none
  | Resize -> resize
  | Resize_x -> resize_x
  | Resize_y -> resize_y
  | Pointer_events_none -> pointer_events_none
  | Pointer_events_auto -> pointer_events_auto
  | Appearance_none -> appearance_none
  | Will_change_auto -> will_change_auto
  | Will_change_scroll -> will_change_scroll
  | Will_change_contents -> will_change_contents
  | Will_change_transform -> will_change_transform

(** {1 Parsing Functions} *)

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
  | _ -> Error (`Msg "Not an interactivity utility")

(** {1 Suborder Function} *)

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
