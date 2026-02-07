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
    | Self_baseline_last
    | Self_stretch
    | Self_center_safe
    | Self_end_safe
    | Self_start_safe
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
    | Justify_self_center_safe
    | Justify_self_end_safe
    | Justify_self_start_safe
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

  let name = "alignment"

  (* Same priority as gap (15), differentiated by suborder: - Container
     alignment: suborder 0-999 (before gap's 25000+) - Self alignment: suborder
     50000+ (after gap) *)
  (* Same priority as gap; comes after flex_props *)
  let priority = 17
  let justify_start = style [ justify_content Flex_start ]
  let justify_end = style [ justify_content Flex_end ]
  let justify_center = style [ justify_content Center ]
  let justify_between = style [ justify_content Space_between ]
  let justify_around = style [ justify_content Space_around ]
  let justify_evenly = style [ justify_content Space_evenly ]
  let items_start = style [ align_items Flex_start ]
  let items_end = style [ align_items Flex_end ]
  let items_center = style [ align_items Center ]
  let items_baseline = style [ align_items Baseline ]
  let items_stretch = style [ align_items Stretch ]
  let content_start = style [ align_content Start ]
  let content_end = style [ align_content End ]
  let content_center = style [ align_content Center ]
  let content_between = style [ align_content Space_between ]
  let content_around = style [ align_content Space_around ]
  let content_evenly = style [ align_content Space_evenly ]
  let content_stretch = style [ align_content Stretch ]
  let self_auto = style [ align_self Auto ]
  let self_start = style [ align_self Flex_start ]
  let self_end = style [ align_self Flex_end ]
  let self_center = style [ align_self Center ]
  let self_baseline = style [ align_self Baseline ]
  let self_baseline_last = style [ align_self Last_baseline ]
  let self_stretch = style [ align_self Stretch ]

  (* Safe variants - safe is the default behavior in CSS *)
  let self_center_safe = style [ align_self Center ]
  let self_end_safe = style [ align_self Flex_end ]
  let self_start_safe = style [ align_self Flex_start ]
  let justify_items_start = style [ justify_items Start ]
  let justify_items_end = style [ justify_items End ]
  let justify_items_center = style [ justify_items Center ]
  let justify_items_stretch = style [ justify_items Stretch ]
  let justify_self_auto = style [ justify_self Auto ]
  let justify_self_start = style [ justify_self Flex_start ]
  let justify_self_end = style [ justify_self Flex_end ]
  let justify_self_center = style [ justify_self Center ]
  let justify_self_stretch = style [ justify_self Stretch ]
  let justify_self_center_safe = style [ justify_self Safe_center ]
  let justify_self_end_safe = style [ justify_self Safe_flex_end ]
  let justify_self_start_safe = style [ justify_self Safe_flex_start ]
  let place_content_start = style [ place_content Start ]
  let place_content_end = style [ place_content End ]
  let place_content_center = style [ place_content Center ]
  let place_content_between = style [ place_content Space_between ]
  let place_content_around = style [ place_content Space_around ]
  let place_content_evenly = style [ place_content Space_evenly ]
  let place_content_stretch = style [ place_content Stretch ]
  let place_items_start = style [ place_items Start ]
  let place_items_end = style [ place_items End ]
  let place_items_center = style [ place_items Center ]
  let place_items_stretch = style [ place_items Stretch ]
  let place_self_auto = style [ place_self (Auto, Auto) ]
  let place_self_start = style [ place_self (Start, Start) ]
  let place_self_end = style [ place_self (End, End) ]
  let place_self_center = style [ place_self (Center, Center) ]
  let place_self_stretch = style [ place_self (Stretch, Stretch) ]

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
    | Self_baseline_last -> self_baseline_last
    | Self_stretch -> self_stretch
    | Self_center_safe -> self_center_safe
    | Self_end_safe -> self_end_safe
    | Self_start_safe -> self_start_safe
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
    | Justify_self_center_safe -> justify_self_center_safe
    | Justify_self_end_safe -> justify_self_end_safe
    | Justify_self_start_safe -> justify_self_start_safe
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

  let suborder = function
    (* Container alignments: 0-999 (before gap's 25000+) *)
    (* Place content (0-9) - alphabetical *)
    | Place_content_around -> 0
    | Place_content_between -> 1
    | Place_content_center -> 2
    | Place_content_end -> 3
    | Place_content_evenly -> 4
    | Place_content_start -> 5
    | Place_content_stretch -> 6
    (* Place items (10-19) - alphabetical *)
    | Place_items_center -> 10
    | Place_items_end -> 11
    | Place_items_start -> 12
    | Place_items_stretch -> 13
    (* Align content (20-29) - alphabetical *)
    | Content_around -> 20
    | Content_between -> 21
    | Content_center -> 22
    | Content_end -> 23
    | Content_evenly -> 24
    | Content_start -> 25
    | Content_stretch -> 26
    (* Align items (30-39) - alphabetical *)
    | Items_baseline -> 30
    | Items_center -> 31
    | Items_end -> 32
    | Items_start -> 33
    | Items_stretch -> 34
    (* Justify content (40-49) - alphabetical *)
    | Justify_around -> 40
    | Justify_between -> 41
    | Justify_center -> 42
    | Justify_end -> 43
    | Justify_evenly -> 44
    | Justify_start -> 45
    (* Justify items (50-59) - alphabetical *)
    | Justify_items_center -> 50
    | Justify_items_end -> 51
    | Justify_items_start -> 52
    | Justify_items_stretch -> 53
    (* Self alignments: 76000+ (after gap's max of 75000 for gap-y-full) *)
    (* Place self (76000-76009) - alphabetical *)
    | Place_self_auto -> 76000
    | Place_self_center -> 76001
    | Place_self_end -> 76002
    | Place_self_start -> 76003
    | Place_self_stretch -> 76004
    (* Align self (76010-76029) - alphabetical *)
    | Self_auto -> 76010
    | Self_baseline -> 76011
    | Self_baseline_last -> 76012
    | Self_center -> 76013
    | Self_center_safe -> 76014
    | Self_end -> 76015
    | Self_end_safe -> 76016
    | Self_start -> 76017
    | Self_start_safe -> 76018
    | Self_stretch -> 76019
    (* Justify self (76030-76049) - alphabetical *)
    | Justify_self_auto -> 76030
    | Justify_self_center -> 76031
    | Justify_self_center_safe -> 76032
    | Justify_self_end -> 76033
    | Justify_self_end_safe -> 76034
    | Justify_self_start -> 76035
    | Justify_self_start_safe -> 76036
    | Justify_self_stretch -> 76037

  let of_class = function
    (* Justify content *)
    | "justify-start" -> Ok Justify_start
    | "justify-end" -> Ok Justify_end
    | "justify-center" -> Ok Justify_center
    | "justify-between" -> Ok Justify_between
    | "justify-around" -> Ok Justify_around
    | "justify-evenly" -> Ok Justify_evenly
    (* Align items *)
    | "items-start" -> Ok Items_start
    | "items-end" -> Ok Items_end
    | "items-center" -> Ok Items_center
    | "items-baseline" -> Ok Items_baseline
    | "items-stretch" -> Ok Items_stretch
    (* Align content *)
    | "content-start" -> Ok Content_start
    | "content-end" -> Ok Content_end
    | "content-center" -> Ok Content_center
    | "content-between" -> Ok Content_between
    | "content-around" -> Ok Content_around
    | "content-evenly" -> Ok Content_evenly
    | "content-stretch" -> Ok Content_stretch
    (* Align self *)
    | "self-auto" -> Ok Self_auto
    | "self-start" -> Ok Self_start
    | "self-end" -> Ok Self_end
    | "self-center" -> Ok Self_center
    | "self-baseline" -> Ok Self_baseline
    | "self-baseline-last" -> Ok Self_baseline_last
    | "self-stretch" -> Ok Self_stretch
    | "self-center-safe" -> Ok Self_center_safe
    | "self-end-safe" -> Ok Self_end_safe
    | "self-start-safe" -> Ok Self_start_safe
    (* Justify items *)
    | "justify-items-start" -> Ok Justify_items_start
    | "justify-items-end" -> Ok Justify_items_end
    | "justify-items-center" -> Ok Justify_items_center
    | "justify-items-stretch" -> Ok Justify_items_stretch
    (* Justify self *)
    | "justify-self-auto" -> Ok Justify_self_auto
    | "justify-self-start" -> Ok Justify_self_start
    | "justify-self-end" -> Ok Justify_self_end
    | "justify-self-center" -> Ok Justify_self_center
    | "justify-self-stretch" -> Ok Justify_self_stretch
    | "justify-self-center-safe" -> Ok Justify_self_center_safe
    | "justify-self-end-safe" -> Ok Justify_self_end_safe
    | "justify-self-start-safe" -> Ok Justify_self_start_safe
    (* Place content *)
    | "place-content-start" -> Ok Place_content_start
    | "place-content-end" -> Ok Place_content_end
    | "place-content-center" -> Ok Place_content_center
    | "place-content-between" -> Ok Place_content_between
    | "place-content-around" -> Ok Place_content_around
    | "place-content-evenly" -> Ok Place_content_evenly
    | "place-content-stretch" -> Ok Place_content_stretch
    (* Place items *)
    | "place-items-start" -> Ok Place_items_start
    | "place-items-end" -> Ok Place_items_end
    | "place-items-center" -> Ok Place_items_center
    | "place-items-stretch" -> Ok Place_items_stretch
    (* Place self *)
    | "place-self-auto" -> Ok Place_self_auto
    | "place-self-start" -> Ok Place_self_start
    | "place-self-end" -> Ok Place_self_end
    | "place-self-center" -> Ok Place_self_center
    | "place-self-stretch" -> Ok Place_self_stretch
    | _ -> Error (`Msg "Not an alignment utility")

  let to_class = function
    (* Justify content *)
    | Justify_start -> "justify-start"
    | Justify_end -> "justify-end"
    | Justify_center -> "justify-center"
    | Justify_between -> "justify-between"
    | Justify_around -> "justify-around"
    | Justify_evenly -> "justify-evenly"
    (* Align items *)
    | Items_start -> "items-start"
    | Items_end -> "items-end"
    | Items_center -> "items-center"
    | Items_baseline -> "items-baseline"
    | Items_stretch -> "items-stretch"
    (* Align content *)
    | Content_start -> "content-start"
    | Content_end -> "content-end"
    | Content_center -> "content-center"
    | Content_between -> "content-between"
    | Content_around -> "content-around"
    | Content_evenly -> "content-evenly"
    | Content_stretch -> "content-stretch"
    (* Align self *)
    | Self_auto -> "self-auto"
    | Self_start -> "self-start"
    | Self_end -> "self-end"
    | Self_center -> "self-center"
    | Self_baseline -> "self-baseline"
    | Self_baseline_last -> "self-baseline-last"
    | Self_stretch -> "self-stretch"
    | Self_center_safe -> "self-center-safe"
    | Self_end_safe -> "self-end-safe"
    | Self_start_safe -> "self-start-safe"
    (* Justify items *)
    | Justify_items_start -> "justify-items-start"
    | Justify_items_end -> "justify-items-end"
    | Justify_items_center -> "justify-items-center"
    | Justify_items_stretch -> "justify-items-stretch"
    (* Justify self *)
    | Justify_self_auto -> "justify-self-auto"
    | Justify_self_start -> "justify-self-start"
    | Justify_self_end -> "justify-self-end"
    | Justify_self_center -> "justify-self-center"
    | Justify_self_stretch -> "justify-self-stretch"
    | Justify_self_center_safe -> "justify-self-center-safe"
    | Justify_self_end_safe -> "justify-self-end-safe"
    | Justify_self_start_safe -> "justify-self-start-safe"
    (* Place content *)
    | Place_content_start -> "place-content-start"
    | Place_content_end -> "place-content-end"
    | Place_content_center -> "place-content-center"
    | Place_content_between -> "place-content-between"
    | Place_content_around -> "place-content-around"
    | Place_content_evenly -> "place-content-evenly"
    | Place_content_stretch -> "place-content-stretch"
    (* Place items *)
    | Place_items_start -> "place-items-start"
    | Place_items_end -> "place-items-end"
    | Place_items_center -> "place-items-center"
    | Place_items_stretch -> "place-items-stretch"
    (* Place self *)
    | Place_self_auto -> "place-self-auto"
    | Place_self_start -> "place-self-start"
    | Place_self_end -> "place-self-end"
    | Place_self_center -> "place-self-center"
    | Place_self_stretch -> "place-self-stretch"
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
