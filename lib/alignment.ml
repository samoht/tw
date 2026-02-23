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
    | Justify_normal
    | Justify_stretch
    | Justify_center_safe
    | Justify_end_safe
    | (* Align items *)
      Items_start
    | Items_end
    | Items_center
    | Items_baseline
    | Items_stretch
    | Items_baseline_last
    | Items_center_safe
    | Items_end_safe
    | Items_start_safe
    | (* Align content *)
      Content_start
    | Content_end
    | Content_center
    | Content_between
    | Content_around
    | Content_evenly
    | Content_stretch
    | Content_baseline
    | Content_normal
    | Content_center_safe
    | Content_end_safe
    | Content_start_safe
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
    | Justify_items_center_safe
    | Justify_items_end_safe
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
    | Place_content_baseline
    | Place_content_center_safe
    | Place_content_end_safe
    | Place_content_start_safe
    | (* Place items *)
      Place_items_start
    | Place_items_end
    | Place_items_center
    | Place_items_stretch
    | Place_items_baseline
    | Place_items_center_safe
    | Place_items_end_safe
    | Place_items_start_safe
    | (* Place self *)
      Place_self_auto
    | Place_self_start
    | Place_self_end
    | Place_self_center
    | Place_self_stretch
    | Place_self_center_safe
    | Place_self_end_safe

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
  let justify_normal = style [ justify_content Normal ]
  let justify_stretch = style [ justify_content Stretch ]

  (* Safe variants - same output as non-safe in Tailwind v4 *)
  let justify_center_safe = style [ justify_content Center ]
  let justify_end_safe = style [ justify_content Flex_end ]
  let items_start = style [ align_items Flex_start ]
  let items_end = style [ align_items Flex_end ]
  let items_center = style [ align_items Center ]
  let items_baseline = style [ align_items Baseline ]
  let items_stretch = style [ align_items Stretch ]
  let items_baseline_last = style [ align_items Last_baseline ]

  (* Safe variants - same output as non-safe in Tailwind v4 *)
  let items_center_safe = style [ align_items Center ]
  let items_end_safe = style [ align_items Flex_end ]
  let items_start_safe = style [ align_items Flex_start ]
  let content_start = style [ align_content Flex_start ]
  let content_end = style [ align_content Flex_end ]
  let content_center = style [ align_content Center ]
  let content_between = style [ align_content Space_between ]
  let content_around = style [ align_content Space_around ]
  let content_evenly = style [ align_content Space_evenly ]
  let content_stretch = style [ align_content Stretch ]
  let content_baseline = style [ align_content Baseline ]
  let content_normal = style [ align_content Normal ]
  let content_center_safe = style [ align_content Center ]
  let content_end_safe = style [ align_content Flex_end ]
  let content_start_safe = style [ align_content Flex_start ]
  let self_auto = style [ align_self Auto ]
  let self_start = style [ align_self Flex_start ]
  let self_end = style [ align_self Flex_end ]
  let self_center = style [ align_self Center ]
  let self_baseline = style [ align_self Baseline ]
  let self_baseline_last = style [ align_self Last_baseline ]
  let self_stretch = style [ align_self Stretch ]

  (* Safe variants - same output as non-safe in Tailwind v4 *)
  let self_center_safe = style [ align_self Center ]
  let self_end_safe = style [ align_self Flex_end ]
  let self_start_safe = style [ align_self Flex_start ]
  let justify_items_start = style [ justify_items Start ]
  let justify_items_end = style [ justify_items End ]
  let justify_items_center = style [ justify_items Center ]
  let justify_items_stretch = style [ justify_items Stretch ]
  let justify_items_center_safe = style [ justify_items Center ]
  let justify_items_end_safe = style [ justify_items End ]
  let justify_self_auto = style [ justify_self Auto ]
  let justify_self_start = style [ justify_self Flex_start ]
  let justify_self_end = style [ justify_self Flex_end ]
  let justify_self_center = style [ justify_self Center ]
  let justify_self_stretch = style [ justify_self Stretch ]
  let justify_self_center_safe = style [ justify_self Center ]
  let justify_self_end_safe = style [ justify_self Flex_end ]
  let justify_self_start_safe = style [ justify_self Flex_start ]
  let place_content_start = style [ place_content Start ]
  let place_content_end = style [ place_content End ]
  let place_content_center = style [ place_content Center ]
  let place_content_between = style [ place_content Space_between ]
  let place_content_around = style [ place_content Space_around ]
  let place_content_evenly = style [ place_content Space_evenly ]
  let place_content_stretch = style [ place_content Stretch ]

  let place_content_baseline =
    style [ place_content (Align_justify (Baseline, Start)) ]

  let place_content_center_safe = style [ place_content Center ]
  let place_content_end_safe = style [ place_content End ]
  let place_content_start_safe = style [ place_content Start ]
  let place_items_start = style [ place_items Start ]
  let place_items_end = style [ place_items End ]
  let place_items_center = style [ place_items Center ]
  let place_items_stretch = style [ place_items Stretch_stretch ]
  let place_items_baseline = style [ place_items Baseline ]
  let place_items_center_safe = style [ place_items Center ]
  let place_items_end_safe = style [ place_items End ]
  let place_items_start_safe = style [ place_items Start ]
  let place_self_auto = style [ place_self (Auto, Auto) ]
  let place_self_start = style [ place_self (Start, Start) ]
  let place_self_end = style [ place_self (End, End) ]
  let place_self_center = style [ place_self (Center, Center) ]
  let place_self_stretch = style [ place_self (Stretch, Stretch) ]
  let place_self_center_safe = style [ place_self (Center, Center) ]
  let place_self_end_safe = style [ place_self (End, End) ]

  (* Single source of truth: (handler, class_name, style, suborder) *)
  let alignment_data =
    [
      (* Justify content *)
      (Justify_start, "justify-start", justify_start, 58);
      (Justify_end, "justify-end", justify_end, 54);
      (Justify_center, "justify-center", justify_center, 52);
      (Justify_between, "justify-between", justify_between, 51);
      (Justify_around, "justify-around", justify_around, 50);
      (Justify_evenly, "justify-evenly", justify_evenly, 56);
      (Justify_normal, "justify-normal", justify_normal, 57);
      (Justify_stretch, "justify-stretch", justify_stretch, 59);
      (Justify_center_safe, "justify-center-safe", justify_center_safe, 53);
      (Justify_end_safe, "justify-end-safe", justify_end_safe, 55);
      (* Align items *)
      (Items_start, "items-start", items_start, 36);
      (Items_end, "items-end", items_end, 34);
      (Items_center, "items-center", items_center, 32);
      (Items_baseline, "items-baseline", items_baseline, 30);
      (Items_stretch, "items-stretch", items_stretch, 38);
      (Items_baseline_last, "items-baseline-last", items_baseline_last, 31);
      (Items_center_safe, "items-center-safe", items_center_safe, 33);
      (Items_end_safe, "items-end-safe", items_end_safe, 35);
      (Items_start_safe, "items-start-safe", items_start_safe, 37);
      (* Align content *)
      (Content_start, "content-start", content_start, 29);
      (Content_end, "content-end", content_end, 25);
      (Content_center, "content-center", content_center, 23);
      (Content_between, "content-between", content_between, 22);
      (Content_around, "content-around", content_around, 20);
      (Content_evenly, "content-evenly", content_evenly, 27);
      (Content_stretch, "content-stretch", content_stretch, 31);
      (Content_baseline, "content-baseline", content_baseline, 21);
      (Content_normal, "content-normal", content_normal, 28);
      (Content_center_safe, "content-center-safe", content_center_safe, 24);
      (Content_end_safe, "content-end-safe", content_end_safe, 26);
      (Content_start_safe, "content-start-safe", content_start_safe, 30);
      (* Align self *)
      (Self_auto, "self-auto", self_auto, 76010);
      (Self_start, "self-start", self_start, 76017);
      (Self_end, "self-end", self_end, 76015);
      (Self_center, "self-center", self_center, 76013);
      (Self_baseline, "self-baseline", self_baseline, 76011);
      (Self_baseline_last, "self-baseline-last", self_baseline_last, 76012);
      (Self_stretch, "self-stretch", self_stretch, 76019);
      (Self_center_safe, "self-center-safe", self_center_safe, 76014);
      (Self_end_safe, "self-end-safe", self_end_safe, 76016);
      (Self_start_safe, "self-start-safe", self_start_safe, 76018);
      (* Justify items *)
      (Justify_items_start, "justify-items-start", justify_items_start, 74);
      (Justify_items_end, "justify-items-end", justify_items_end, 72);
      (Justify_items_center, "justify-items-center", justify_items_center, 70);
      (Justify_items_stretch, "justify-items-stretch", justify_items_stretch, 75);
      ( Justify_items_center_safe,
        "justify-items-center-safe",
        justify_items_center_safe,
        71 );
      ( Justify_items_end_safe,
        "justify-items-end-safe",
        justify_items_end_safe,
        73 );
      (* Justify self *)
      (Justify_self_auto, "justify-self-auto", justify_self_auto, 76030);
      (Justify_self_start, "justify-self-start", justify_self_start, 76035);
      (Justify_self_end, "justify-self-end", justify_self_end, 76033);
      (Justify_self_center, "justify-self-center", justify_self_center, 76031);
      (Justify_self_stretch, "justify-self-stretch", justify_self_stretch, 76037);
      ( Justify_self_center_safe,
        "justify-self-center-safe",
        justify_self_center_safe,
        76032 );
      ( Justify_self_end_safe,
        "justify-self-end-safe",
        justify_self_end_safe,
        76034 );
      ( Justify_self_start_safe,
        "justify-self-start-safe",
        justify_self_start_safe,
        76036 );
      (* Place content *)
      (Place_content_start, "place-content-start", place_content_start, 8);
      (Place_content_end, "place-content-end", place_content_end, 5);
      (Place_content_center, "place-content-center", place_content_center, 3);
      (Place_content_between, "place-content-between", place_content_between, 2);
      (Place_content_around, "place-content-around", place_content_around, 0);
      (Place_content_evenly, "place-content-evenly", place_content_evenly, 7);
      (Place_content_stretch, "place-content-stretch", place_content_stretch, 10);
      ( Place_content_baseline,
        "place-content-baseline",
        place_content_baseline,
        1 );
      ( Place_content_center_safe,
        "place-content-center-safe",
        place_content_center_safe,
        4 );
      ( Place_content_end_safe,
        "place-content-end-safe",
        place_content_end_safe,
        6 );
      ( Place_content_start_safe,
        "place-content-start-safe",
        place_content_start_safe,
        9 );
      (* Place items *)
      (Place_items_start, "place-items-start", place_items_start, 15);
      (Place_items_end, "place-items-end", place_items_end, 13);
      (Place_items_center, "place-items-center", place_items_center, 11);
      (Place_items_stretch, "place-items-stretch", place_items_stretch, 17);
      (Place_items_baseline, "place-items-baseline", place_items_baseline, 10);
      ( Place_items_center_safe,
        "place-items-center-safe",
        place_items_center_safe,
        12 );
      (Place_items_end_safe, "place-items-end-safe", place_items_end_safe, 14);
      ( Place_items_start_safe,
        "place-items-start-safe",
        place_items_start_safe,
        16 );
      (* Place self *)
      (Place_self_auto, "place-self-auto", place_self_auto, 76000);
      (Place_self_start, "place-self-start", place_self_start, 76005);
      (Place_self_end, "place-self-end", place_self_end, 76003);
      (Place_self_center, "place-self-center", place_self_center, 76001);
      (Place_self_stretch, "place-self-stretch", place_self_stretch, 76006);
      ( Place_self_center_safe,
        "place-self-center-safe",
        place_self_center_safe,
        76002 );
      (Place_self_end_safe, "place-self-end-safe", place_self_end_safe, 76004);
    ]

  (* Derived lookup tables *)
  let to_style_map =
    List.map (fun (t, _, style, _) -> (t, style)) alignment_data

  let to_class_map = List.map (fun (t, cls, _, _) -> (t, cls)) alignment_data
  let suborder_map = List.map (fun (t, _, _, ord) -> (t, ord)) alignment_data
  let of_class_map = List.map (fun (t, cls, _, _) -> (cls, t)) alignment_data

  (* Handler functions derived from maps *)
  let to_style t = List.assoc t to_style_map
  let to_class t = List.assoc t to_class_map
  let suborder t = List.assoc t suborder_map

  let of_class cls =
    match List.assoc_opt cls of_class_map with
    | Some t -> Ok t
    | None -> Error (`Msg "Not an alignment utility")
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
let content_baseline = utility Content_baseline
let content_normal = utility Content_normal
let content_center_safe = utility Content_center_safe
let content_end_safe = utility Content_end_safe
let content_start_safe = utility Content_start_safe

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
let justify_items_center_safe = utility Justify_items_center_safe
let justify_items_end_safe = utility Justify_items_end_safe

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
let place_items_baseline = utility Place_items_baseline
let place_items_center_safe = utility Place_items_center_safe
let place_items_end_safe = utility Place_items_end_safe
let place_items_start_safe = utility Place_items_start_safe

(** {1 Place Self Utilities} *)
let place_self_auto = utility Place_self_auto

let place_self_start = utility Place_self_start
let place_self_end = utility Place_self_end
let place_self_center = utility Place_self_center
let place_self_stretch = utility Place_self_stretch
let place_self_center_safe = utility Place_self_center_safe
let place_self_end_safe = utility Place_self_end_safe
