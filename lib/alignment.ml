(** Alignment utilities for flexbox and grid layouts

    What's included:
    - `justify-*` - Justify content, items, and self alignment.
    - `items-*` - Align items in flex/grid containers.
    - `content-*` - Align content in flex/grid containers.
    - `self-*` - Align self in flex/grid items.
    - `place-*` - Shorthand for align and justify.

    What's not:
    - Advanced grid alignment features not covered by basic flex/grid alignment.

    Parsing contract (`of_string`):
    - Accepts ["justify"; ...], ["items"; ...], ["content"; ...], ["self"; ...],
      ["place"; ...]. Unknown tokens yield `Error (`Msg "Not an alignment
      utility")`. *)

module Handler = struct
  open Style
  open Css

  type t =
    | (* Justify content *)
      Justify_start
    | Justify_end
    | Justify_center
    | Justify_between
    | Justify_around
    | Justify_evenly
    | (* Align items *)
      Items_start
    | Items_end
    | Items_center
    | Items_baseline
    | Items_stretch
    | (* Align content *)
      Content_start
    | Content_end
    | Content_center
    | Content_between
    | Content_around
    | Content_evenly
    | Content_stretch
    | (* Align self *)
      Self_auto
    | Self_start
    | Self_end
    | Self_center
    | Self_baseline
    | Self_stretch
    | (* Justify items *)
      Justify_items_start
    | Justify_items_end
    | Justify_items_center
    | Justify_items_stretch
    | (* Justify self *)
      Justify_self_auto
    | Justify_self_start
    | Justify_self_end
    | Justify_self_center
    | Justify_self_stretch
    | (* Place content *)
      Place_content_start
    | Place_content_end
    | Place_content_center
    | Place_content_between
    | Place_content_around
    | Place_content_evenly
    | Place_content_stretch
    | (* Place items *)
      Place_items_start
    | Place_items_end
    | Place_items_center
    | Place_items_stretch
    | (* Place self *)
      Place_self_auto
    | Place_self_start
    | Place_self_end
    | Place_self_center
    | Place_self_stretch

  type Utility.base += Self of t

  let priority = 10
  let justify_start = style "justify-start" [ justify_content Flex_start ]
  let justify_end = style "justify-end" [ justify_content Flex_end ]
  let justify_center = style "justify-center" [ justify_content Center ]

  let justify_between =
    style "justify-between" [ justify_content Space_between ]

  let justify_around = style "justify-around" [ justify_content Space_around ]
  let justify_evenly = style "justify-evenly" [ justify_content Space_evenly ]
  let items_start = style "items-start" [ align_items Flex_start ]
  let items_end = style "items-end" [ align_items Flex_end ]
  let items_center = style "items-center" [ align_items Center ]
  let items_baseline = style "items-baseline" [ align_items Baseline ]
  let items_stretch = style "items-stretch" [ align_items Stretch ]
  let content_start = style "content-start" [ align_content Start ]
  let content_end = style "content-end" [ align_content End ]
  let content_center = style "content-center" [ align_content Center ]
  let content_between = style "content-between" [ align_content Space_between ]
  let content_around = style "content-around" [ align_content Space_around ]
  let content_evenly = style "content-evenly" [ align_content Space_evenly ]
  let content_stretch = style "content-stretch" [ align_content Stretch ]
  let self_auto = style "self-auto" [ align_self Auto ]
  let self_start = style "self-start" [ align_self Flex_start ]
  let self_end = style "self-end" [ align_self Flex_end ]
  let self_center = style "self-center" [ align_self Center ]
  let self_baseline = style "self-baseline" [ align_self Baseline ]
  let self_stretch = style "self-stretch" [ align_self Stretch ]
  let justify_items_start = style "justify-items-start" [ justify_items Start ]
  let justify_items_end = style "justify-items-end" [ justify_items End ]

  let justify_items_center =
    style "justify-items-center" [ justify_items Center ]

  let justify_items_stretch =
    style "justify-items-stretch" [ justify_items Stretch ]

  let justify_self_auto = style "justify-self-auto" [ justify_self Auto ]

  let justify_self_start =
    style "justify-self-start" [ justify_self Flex_start ]

  let justify_self_end = style "justify-self-end" [ justify_self Flex_end ]
  let justify_self_center = style "justify-self-center" [ justify_self Center ]

  let justify_self_stretch =
    style "justify-self-stretch" [ justify_self Stretch ]

  let place_content_start = style "place-content-start" [ place_content Start ]
  let place_content_end = style "place-content-end" [ place_content End ]

  let place_content_center =
    style "place-content-center" [ place_content Center ]

  let place_content_between =
    style "place-content-between" [ place_content Space_between ]

  let place_content_around =
    style "place-content-around" [ place_content Space_around ]

  let place_content_evenly =
    style "place-content-evenly" [ place_content Space_evenly ]

  let place_content_stretch =
    style "place-content-stretch" [ place_content Stretch ]

  let place_items_start = style "place-items-start" [ place_items Start ]
  let place_items_end = style "place-items-end" [ place_items End ]
  let place_items_center = style "place-items-center" [ place_items Center ]
  let place_items_stretch = style "place-items-stretch" [ place_items Stretch ]
  let place_self_auto = style "place-self-auto" [ place_self (Auto, Auto) ]
  let place_self_start = style "place-self-start" [ place_self (Start, Start) ]
  let place_self_end = style "place-self-end" [ place_self (End, End) ]

  let place_self_center =
    style "place-self-center" [ place_self (Center, Center) ]

  let place_self_stretch =
    style "place-self-stretch" [ place_self (Stretch, Stretch) ]

  let to_style = function
    (* Justify content *)
    | Justify_start -> justify_start
    | Justify_end -> justify_end
    | Justify_center -> justify_center
    | Justify_between -> justify_between
    | Justify_around -> justify_around
    | Justify_evenly -> justify_evenly
    (* Align items *)
    | Items_start -> items_start
    | Items_end -> items_end
    | Items_center -> items_center
    | Items_baseline -> items_baseline
    | Items_stretch -> items_stretch
    (* Align content *)
    | Content_start -> content_start
    | Content_end -> content_end
    | Content_center -> content_center
    | Content_between -> content_between
    | Content_around -> content_around
    | Content_evenly -> content_evenly
    | Content_stretch -> content_stretch
    (* Align self *)
    | Self_auto -> self_auto
    | Self_start -> self_start
    | Self_end -> self_end
    | Self_center -> self_center
    | Self_baseline -> self_baseline
    | Self_stretch -> self_stretch
    (* Justify items *)
    | Justify_items_start -> justify_items_start
    | Justify_items_end -> justify_items_end
    | Justify_items_center -> justify_items_center
    | Justify_items_stretch -> justify_items_stretch
    (* Justify self *)
    | Justify_self_auto -> justify_self_auto
    | Justify_self_start -> justify_self_start
    | Justify_self_end -> justify_self_end
    | Justify_self_center -> justify_self_center
    | Justify_self_stretch -> justify_self_stretch
    (* Place content *)
    | Place_content_start -> place_content_start
    | Place_content_end -> place_content_end
    | Place_content_center -> place_content_center
    | Place_content_between -> place_content_between
    | Place_content_around -> place_content_around
    | Place_content_evenly -> place_content_evenly
    | Place_content_stretch -> place_content_stretch
    (* Place items *)
    | Place_items_start -> place_items_start
    | Place_items_end -> place_items_end
    | Place_items_center -> place_items_center
    | Place_items_stretch -> place_items_stretch
    (* Place self *)
    | Place_self_auto -> place_self_auto
    | Place_self_start -> place_self_start
    | Place_self_end -> place_self_end
    | Place_self_center -> place_self_center
    | Place_self_stretch -> place_self_stretch

  let of_string = function
    (* Justify content *)
    | [ "justify"; "start" ] -> Ok Justify_start
    | [ "justify"; "end" ] -> Ok Justify_end
    | [ "justify"; "center" ] -> Ok Justify_center
    | [ "justify"; "between" ] -> Ok Justify_between
    | [ "justify"; "around" ] -> Ok Justify_around
    | [ "justify"; "evenly" ] -> Ok Justify_evenly
    (* Align items *)
    | [ "items"; "start" ] -> Ok Items_start
    | [ "items"; "end" ] -> Ok Items_end
    | [ "items"; "center" ] -> Ok Items_center
    | [ "items"; "baseline" ] -> Ok Items_baseline
    | [ "items"; "stretch" ] -> Ok Items_stretch
    (* Align content *)
    | [ "content"; "start" ] -> Ok Content_start
    | [ "content"; "end" ] -> Ok Content_end
    | [ "content"; "center" ] -> Ok Content_center
    | [ "content"; "between" ] -> Ok Content_between
    | [ "content"; "around" ] -> Ok Content_around
    | [ "content"; "evenly" ] -> Ok Content_evenly
    | [ "content"; "stretch" ] -> Ok Content_stretch
    (* Align self *)
    | [ "self"; "auto" ] -> Ok Self_auto
    | [ "self"; "start" ] -> Ok Self_start
    | [ "self"; "end" ] -> Ok Self_end
    | [ "self"; "center" ] -> Ok Self_center
    | [ "self"; "baseline" ] -> Ok Self_baseline
    | [ "self"; "stretch" ] -> Ok Self_stretch
    (* Justify items *)
    | [ "justify"; "items"; "start" ] -> Ok Justify_items_start
    | [ "justify"; "items"; "end" ] -> Ok Justify_items_end
    | [ "justify"; "items"; "center" ] -> Ok Justify_items_center
    | [ "justify"; "items"; "stretch" ] -> Ok Justify_items_stretch
    (* Justify self *)
    | [ "justify"; "self"; "auto" ] -> Ok Justify_self_auto
    | [ "justify"; "self"; "start" ] -> Ok Justify_self_start
    | [ "justify"; "self"; "end" ] -> Ok Justify_self_end
    | [ "justify"; "self"; "center" ] -> Ok Justify_self_center
    | [ "justify"; "self"; "stretch" ] -> Ok Justify_self_stretch
    (* Place content *)
    | [ "place"; "content"; "start" ] -> Ok Place_content_start
    | [ "place"; "content"; "end" ] -> Ok Place_content_end
    | [ "place"; "content"; "center" ] -> Ok Place_content_center
    | [ "place"; "content"; "between" ] -> Ok Place_content_between
    | [ "place"; "content"; "around" ] -> Ok Place_content_around
    | [ "place"; "content"; "evenly" ] -> Ok Place_content_evenly
    | [ "place"; "content"; "stretch" ] -> Ok Place_content_stretch
    (* Place items *)
    | [ "place"; "items"; "start" ] -> Ok Place_items_start
    | [ "place"; "items"; "end" ] -> Ok Place_items_end
    | [ "place"; "items"; "center" ] -> Ok Place_items_center
    | [ "place"; "items"; "stretch" ] -> Ok Place_items_stretch
    (* Place self *)
    | [ "place"; "self"; "auto" ] -> Ok Place_self_auto
    | [ "place"; "self"; "start" ] -> Ok Place_self_start
    | [ "place"; "self"; "end" ] -> Ok Place_self_end
    | [ "place"; "self"; "center" ] -> Ok Place_self_center
    | [ "place"; "self"; "stretch" ] -> Ok Place_self_stretch
    | _ -> Error (`Msg "Not an alignment utility")

  let suborder = function
    (* Align content (1000-1099) - comes first *)
    | Content_start -> 1000
    | Content_end -> 1001
    | Content_center -> 1002
    | Content_between -> 1003
    | Content_around -> 1004
    | Content_evenly -> 1005
    | Content_stretch -> 1006
    (* Align items (2000-2099) *)
    | Items_start -> 2000
    | Items_end -> 2001
    | Items_center -> 2002
    | Items_baseline -> 2003
    | Items_stretch -> 2004
    (* Justify content (3000-3099) *)
    | Justify_start -> 3000
    | Justify_end -> 3001
    | Justify_center -> 3002
    | Justify_between -> 3003
    | Justify_around -> 3004
    | Justify_evenly -> 3005
    (* Align self (100000-100099) - comes after Gap priority group *)
    | Self_auto -> 100000
    | Self_start -> 100001
    | Self_end -> 100002
    | Self_center -> 100003
    | Self_baseline -> 100004
    | Self_stretch -> 100005
    (* Justify items/self (20000-20199) *)
    | Justify_items_start -> 20000
    | Justify_items_end -> 20001
    | Justify_items_center -> 20002
    | Justify_items_stretch -> 20003
    | Justify_self_auto -> 20100
    | Justify_self_start -> 20101
    | Justify_self_end -> 20102
    | Justify_self_center -> 20103
    | Justify_self_stretch -> 20104
    (* Place content/items/self (30000-30299) *)
    | Place_content_start -> 30000
    | Place_content_end -> 30001
    | Place_content_center -> 30002
    | Place_content_between -> 30003
    | Place_content_around -> 30004
    | Place_content_evenly -> 30005
    | Place_content_stretch -> 30006
    | Place_items_start -> 30100
    | Place_items_end -> 30101
    | Place_items_center -> 30102
    | Place_items_stretch -> 30103
    | Place_self_auto -> 30200
    | Place_self_start -> 30201
    | Place_self_end -> 30202
    | Place_self_center -> 30203
    | Place_self_stretch -> 30204
end

open Handler

(** Register alignment handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

(** {1 Justify Content Utilities} *)
let justify_start = utility Justify_start

let justify_end = utility Justify_end
let justify_center = utility Justify_center
let justify_between = utility Justify_between
let justify_around = utility Justify_around
let justify_evenly = utility Justify_evenly

(** {1 Align Items Utilities} *)
let items_start = utility Items_start

let items_end = utility Items_end
let items_center = utility Items_center
let items_baseline = utility Items_baseline
let items_stretch = utility Items_stretch

(** {1 Align Content Utilities} *)
let content_start = utility Content_start

let content_end = utility Content_end
let content_center = utility Content_center
let content_between = utility Content_between
let content_around = utility Content_around
let content_evenly = utility Content_evenly
let content_stretch = utility Content_stretch

(** {1 Align Self Utilities} *)
let self_auto = utility Self_auto

let self_start = utility Self_start
let self_end = utility Self_end
let self_center = utility Self_center
let self_baseline = utility Self_baseline
let self_stretch = utility Self_stretch

(** {1 Justify Items Utilities} *)
let justify_items_start = utility Justify_items_start

let justify_items_end = utility Justify_items_end
let justify_items_center = utility Justify_items_center
let justify_items_stretch = utility Justify_items_stretch

(** {1 Justify Self Utilities} *)
let justify_self_auto = utility Justify_self_auto

let justify_self_start = utility Justify_self_start
let justify_self_end = utility Justify_self_end
let justify_self_center = utility Justify_self_center
let justify_self_stretch = utility Justify_self_stretch

(** {1 Place Content Utilities} *)
let place_content_start = utility Place_content_start

let place_content_end = utility Place_content_end
let place_content_center = utility Place_content_center
let place_content_between = utility Place_content_between
let place_content_around = utility Place_content_around
let place_content_evenly = utility Place_content_evenly
let place_content_stretch = utility Place_content_stretch

(** {1 Place Items Utilities} *)
let place_items_start = utility Place_items_start

let place_items_end = utility Place_items_end
let place_items_center = utility Place_items_center
let place_items_stretch = utility Place_items_stretch

(** {1 Place Self Utilities} *)
let place_self_auto = utility Place_self_auto

let place_self_start = utility Place_self_start
let place_self_end = utility Place_self_end
let place_self_center = utility Place_self_center
let place_self_stretch = utility Place_self_stretch
