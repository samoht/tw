(** Flexbox utilities for controlling flex layout *)

open Core
open Css

(** Helper functions from sizing *)
let spacing_calc n =
  Calc (Expr (Calc.var "spacing", Mult, Calc.float (float_of_int n)))

let spacing_var n =
  Core.Spacing
    { multiplier = n; value = string_of_float (float_of_int n *. 0.25) ^ "rem" }

(** {1 Display Utilities} *)

let flex = style "flex" [ display Flex ]
let inline_flex = style "inline-flex" [ display Inline_flex ]

(** {1 Flex Direction} *)

let flex_row = style "flex-row" [ Flex.direction Row ]
let flex_row_reverse = style "flex-row-reverse" [ Flex.direction Row_reverse ]
let flex_col = style "flex-col" [ Flex.direction Column ]

let flex_col_reverse =
  style "flex-col-reverse" [ Flex.direction Column_reverse ]

(** {1 Flex Wrap} *)

let flex_wrap = style "flex-wrap" [ Flex.wrap Wrap ]
let flex_wrap_reverse = style "flex-wrap-reverse" [ Flex.wrap Wrap_reverse ]
let flex_nowrap = style "flex-nowrap" [ Flex.wrap Nowrap ]

(** {1 Flex} *)

let flex_1 = style "flex-1" [ Flex.flex (Grow 1.0) ]
let flex_auto = style "flex-auto" [ Flex.flex Auto ]
let flex_initial = style "flex-initial" [ Flex.flex Initial ]
let flex_none = style "flex-none" [ Flex.flex None ]

(** {1 Flex Grow} *)

let flex_grow = style "flex-grow" [ Flex.grow 1.0 ]
let flex_grow_0 = style "flex-grow-0" [ Flex.grow 0.0 ]

(** {1 Flex Shrink} *)

let flex_shrink = style "flex-shrink" [ Flex.shrink 1.0 ]
let flex_shrink_0 = style "flex-shrink-0" [ Flex.shrink 0.0 ]

(** {1 Flex Basis} *)

let basis_0 = style "basis-0" [ Flex.basis Zero ]
let basis_1 = style "basis-1" [ Flex.basis (Rem 0.25) ]
let basis_auto = style "basis-auto" [ Flex.basis Auto ]
let basis_full = style "basis-full" [ Flex.basis (Pct 100.0) ]

(** {1 Order} *)

let order_1 = style "order-1" [ Flex.order 1 ]
let order_2 = style "order-2" [ Flex.order 2 ]
let order_3 = style "order-3" [ Flex.order 3 ]
let order_4 = style "order-4" [ Flex.order 4 ]
let order_5 = style "order-5" [ Flex.order 5 ]
let order_6 = style "order-6" [ Flex.order 6 ]
let order_first = style "order-first" [ Flex.order min_int ]
let order_last = style "order-last" [ Flex.order max_int ]
let order_none = style "order-none" [ Flex.order 0 ]

(** {1 Align Items} *)

let items_start = style "items-start" [ align_items Flex_start ]
let items_end = style "items-end" [ align_items Flex_end ]
let items_center = style "items-center" [ align_items Center ]
let items_baseline = style "items-baseline" [ align_items Baseline ]
let items_stretch = style "items-stretch" [ align_items Stretch ]

(** {1 Justify Content} *)

let justify_start = style "justify-start" [ justify_content Flex_start ]
let justify_end = style "justify-end" [ justify_content Flex_end ]
let justify_center = style "justify-center" [ justify_content Center ]
let justify_between = style "justify-between" [ justify_content Space_between ]
let justify_around = style "justify-around" [ justify_content Space_around ]
let justify_evenly = style "justify-evenly" [ justify_content Space_evenly ]

(** {1 Align Content} *)

let content_start = style "content-start" [ align_content Flex_start ]
let content_end = style "content-end" [ align_content Flex_end ]
let content_center = style "content-center" [ align_content Center ]
let content_between = style "content-between" [ align_content Space_between ]
let content_around = style "content-around" [ align_content Space_around ]
let content_evenly = style "content-evenly" [ align_content Space_evenly ]
let content_stretch = style "content-stretch" [ align_content Stretch ]

(** {1 Align Self} *)

let self_auto = style "self-auto" [ align_self Auto ]
let self_start = style "self-start" [ align_self Flex_start ]
let self_end = style "self-end" [ align_self Flex_end ]
let self_center = style "self-center" [ align_self Center ]
let self_baseline = style "self-baseline" [ align_self Baseline ]
let self_stretch = style "self-stretch" [ align_self Stretch ]

(** {1 Gap} *)

let gap n =
  let class_name = "gap-" ^ string_of_int n in
  let css_value = spacing_calc n in
  style
    ~vars:[ spacing_var n ]
    class_name
    [ row_gap css_value; column_gap css_value ]

let gap_x n =
  let class_name = "gap-x-" ^ string_of_int n in
  let css_value = spacing_calc n in
  style ~vars:[ spacing_var n ] class_name [ column_gap css_value ]

let gap_y n =
  let class_name = "gap-y-" ^ string_of_int n in
  let css_value = spacing_calc n in
  style ~vars:[ spacing_var n ] class_name [ row_gap css_value ]

(** {1 String Parsing} *)

let of_string = function
  (* Display *)
  | [ "flex" ] -> Ok flex
  | [ "inline"; "flex" ] -> Ok inline_flex
  (* Flex direction *)
  | [ "flex"; "row" ] -> Ok flex_row
  | [ "flex"; "row"; "reverse" ] -> Ok flex_row_reverse
  | [ "flex"; "col" ] -> Ok flex_col
  | [ "flex"; "col"; "reverse" ] -> Ok flex_col_reverse
  (* Flex wrap *)
  | [ "flex"; "wrap" ] -> Ok flex_wrap
  | [ "flex"; "wrap"; "reverse" ] -> Ok flex_wrap_reverse
  | [ "flex"; "nowrap" ] -> Ok flex_nowrap
  (* Flex *)
  | [ "flex"; "1" ] -> Ok flex_1
  | [ "flex"; "auto" ] -> Ok flex_auto
  | [ "flex"; "initial" ] -> Ok flex_initial
  | [ "flex"; "none" ] -> Ok flex_none
  (* Flex grow *)
  | [ "flex"; "grow" ] -> Ok flex_grow
  | [ "flex"; "grow"; "0" ] -> Ok flex_grow_0
  (* Flex shrink *)
  | [ "flex"; "shrink" ] -> Ok flex_shrink
  | [ "flex"; "shrink"; "0" ] -> Ok flex_shrink_0
  (* Flex basis *)
  | [ "basis"; "0" ] -> Ok basis_0
  | [ "basis"; "1" ] -> Ok basis_1
  | [ "basis"; "auto" ] -> Ok basis_auto
  | [ "basis"; "full" ] -> Ok basis_full
  (* Order *)
  | [ "order"; "1" ] -> Ok order_1
  | [ "order"; "2" ] -> Ok order_2
  | [ "order"; "3" ] -> Ok order_3
  | [ "order"; "4" ] -> Ok order_4
  | [ "order"; "5" ] -> Ok order_5
  | [ "order"; "6" ] -> Ok order_6
  | [ "order"; "first" ] -> Ok order_first
  | [ "order"; "last" ] -> Ok order_last
  | [ "order"; "none" ] -> Ok order_none
  (* Align items *)
  | [ "items"; "start" ] -> Ok items_start
  | [ "items"; "end" ] -> Ok items_end
  | [ "items"; "center" ] -> Ok items_center
  | [ "items"; "baseline" ] -> Ok items_baseline
  | [ "items"; "stretch" ] -> Ok items_stretch
  (* Justify content *)
  | [ "justify"; "start" ] -> Ok justify_start
  | [ "justify"; "end" ] -> Ok justify_end
  | [ "justify"; "center" ] -> Ok justify_center
  | [ "justify"; "between" ] -> Ok justify_between
  | [ "justify"; "around" ] -> Ok justify_around
  | [ "justify"; "evenly" ] -> Ok justify_evenly
  (* Align content *)
  | [ "content"; "start" ] -> Ok content_start
  | [ "content"; "end" ] -> Ok content_end
  | [ "content"; "center" ] -> Ok content_center
  | [ "content"; "between" ] -> Ok content_between
  | [ "content"; "around" ] -> Ok content_around
  | [ "content"; "evenly" ] -> Ok content_evenly
  | [ "content"; "stretch" ] -> Ok content_stretch
  (* Align self *)
  | [ "self"; "auto" ] -> Ok self_auto
  | [ "self"; "start" ] -> Ok self_start
  | [ "self"; "end" ] -> Ok self_end
  | [ "self"; "center" ] -> Ok self_center
  | [ "self"; "baseline" ] -> Ok self_baseline
  | [ "self"; "stretch" ] -> Ok self_stretch
  (* Gap *)
  | [ "gap"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 && i <= 96 -> Ok (gap i)
      | _ -> Error (`Msg ("Invalid gap value: " ^ n)))
  | [ "gap"; "x"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 && i <= 96 -> Ok (gap_x i)
      | _ -> Error (`Msg ("Invalid gap-x value: " ^ n)))
  | [ "gap"; "y"; n ] -> (
      match int_of_string_opt n with
      | Some i when i >= 0 && i <= 96 -> Ok (gap_y i)
      | _ -> Error (`Msg ("Invalid gap-y value: " ^ n)))
  | _ -> Error (`Msg "Not a flexbox utility")
