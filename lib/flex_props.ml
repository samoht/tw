(** Flexbox property utilities (grow, shrink, basis, order, flex shortcuts).

    These utilities control flexbox item behavior and come after sizing
    utilities in the cascade order. For flex display utilities (flex,
    inline-flex), see Flex module. For direction/wrap utilities, see Flex_layout
    module. *)

module Handler = struct
  open Style
  open Css

  type t =
    (* Flex shortcuts *)
    | Flex_1
    | Flex_auto
    | Flex_initial
    | Flex_none
    | Flex_n of int (* flex-N where N is any integer *)
    | Flex_fraction of int * int (* flex-N/M where N/M is a fraction *)
    | Flex_arbitrary of int (* flex-[123] *)
    (* Grow *)
    | Flex_grow
    | Flex_grow_0
    (* Shrink *)
    | Flex_shrink
    | Flex_shrink_0
    (* Basis *)
    | Basis_0
    | Basis_1
    | Basis_auto
    | Basis_full
    | Basis_fraction of int * int
    | Basis_named of string
    (* Order *)
    | Order of int
    | Neg_order of int (* -order-4 = calc(4 * -1) *)
    | Neg_order_arbitrary of string (* -order-[var(--value)] *)
    | Order_arbitrary of string (* order-[123] *)
    | Order_first
    | Order_last
    | Order_none

  type Utility.base += Self of t

  let name = "flex_props"

  (** Priority 7 - after sizing (6), before transforms (9) *)
  let priority = 7

  (* Flex shortcuts *)
  let flex_1 = style [ flex (Grow 1.0) ]
  let flex_auto = style [ flex Auto ]
  let flex_initial = style [ flex (Full (0., 1., Auto)) ]
  let flex_none = style [ flex None ]

  (* flex-N: flex: N *)
  let flex_n_style n = style [ flex (Grow (float_of_int n)) ]

  (* flex-N/M: flex: (N/M * 100)% - evaluates the fraction *)
  let flex_fraction_style n m =
    let pct_value = float_of_int n /. float_of_int m *. 100.0 in
    style [ flex (Basis (Pct pct_value)) ]

  (* Grow *)
  let flex_grow_utility = style [ flex_grow 1.0 ]
  let flex_grow_0_utility = style [ flex_grow 0.0 ]

  (* Shrink *)
  let flex_shrink_utility = style [ flex_shrink 1.0 ]
  let flex_shrink_0_utility = style [ flex_shrink 0.0 ]

  (* Basis *)
  let basis_0 = style [ flex_basis Zero ]
  let basis_1 = style [ flex_basis (Pct 100.0) ]
  let basis_auto = style [ flex_basis Auto ]
  let basis_full = style [ flex_basis (Pct 100.0) ]

  let basis_fraction_style n m =
    let raw = float_of_int n /. float_of_int m *. 100.0 in
    let pct_value = Float.round (raw *. 10000.0) /. 10000.0 in
    style [ flex_basis (Pct pct_value) ]

  let basis_named_style name =
    let ref : Css.length Css.var =
      Css.var_ref ~layer:"theme" ("container-" ^ name)
    in
    style [ flex_basis (Var ref) ]

  (* Order *)
  let order_style n = style [ order (Order_int n) ]

  let order_first =
    style
      [
        order
          (Var
             (Var.theme_ref "order-first" ~default:(Order_int (-9999))
                ~default_css:"-9999"));
      ]

  let order_last =
    style
      [
        order
          (Var
             (Var.theme_ref "order-last" ~default:(Order_int 9999)
                ~default_css:"9999"));
      ]

  let order_none = style [ order (Order_int 0) ]

  let to_style = function
    | Flex_1 -> flex_1
    | Flex_auto -> flex_auto
    | Flex_initial -> flex_initial
    | Flex_none -> flex_none
    | Flex_n n -> flex_n_style n
    | Flex_fraction (n, m) -> flex_fraction_style n m
    | Flex_arbitrary n -> flex_n_style n
    | Flex_grow -> flex_grow_utility
    | Flex_grow_0 -> flex_grow_0_utility
    | Flex_shrink -> flex_shrink_utility
    | Flex_shrink_0 -> flex_shrink_0_utility
    | Basis_0 -> basis_0
    | Basis_1 -> basis_1
    | Basis_auto -> basis_auto
    | Basis_full -> basis_full
    | Basis_fraction (n, m) -> basis_fraction_style n m
    | Basis_named name -> basis_named_style name
    | Order n -> order_style n
    | Neg_order n -> style [ order (Order_int (-n)) ]
    | Neg_order_arbitrary s -> (
        match int_of_string_opt s with
        | Some n -> style [ order (Order_int (-n)) ]
        | None -> style [ order (Order_calc ("calc(" ^ s ^ " * -1)")) ])
    | Order_arbitrary s -> (
        match int_of_string_opt s with
        | Some n -> style [ order (Order_int n) ]
        | None -> style [ order (Order_calc s) ])
    | Order_first -> order_first
    | Order_last -> order_last
    | Order_none -> order_none

  let suborder : t -> int = function
    (* Order - comes first in Tailwind's output. Ordering: negative first, then
       positive, then first/last/none, then arbitrary *)
    | Neg_order n -> n (* negative comes first *)
    | Neg_order_arbitrary _ -> 50 (* negative arbitrary after negative ints *)
    | Order n -> 100 + n
    | Order_arbitrary _ -> 150
    | Order_first -> 200
    | Order_last -> 201
    | Order_none -> 202
    (* Tailwind flex order: flex-1 < fractions < numbers < auto < initial <
       none. Note: flex-* utilities come AFTER order-* utilities *)
    | Flex_1 -> 1000
    (* Fractions: sorted by numerator then denominator (1/2 < 1/3 < 1/4 <
       2/3...) *)
    | Flex_fraction (n, m) -> 1020 + (n * 100) + m
    (* flex-N values: after fractions, ordered by value *)
    | Flex_n n -> 5000 + n
    (* Arbitrary flex values - after regular numbers *)
    | Flex_arbitrary n -> 6000 + n
    (* Named shortcuts come last *)
    | Flex_auto -> 10000
    | Flex_initial -> 10001
    | Flex_none -> 10002
    (* Shrink *)
    | Flex_shrink -> 20000
    | Flex_shrink_0 -> 20001
    (* Grow *)
    | Flex_grow -> 30000
    | Flex_grow_0 -> 30001
    (* Basis *)
    | Basis_0 -> 40000
    | Basis_1 -> 40001
    | Basis_auto -> 40002
    | Basis_full -> 40003
    | Basis_fraction (n, m) -> 40100 + (n * 100) + m
    | Basis_named _ -> 40500

  let err_not_utility = Error (`Msg "Not a flex property utility")

  let parse_fraction s =
    (* Parse "N/M" into (N, M) *)
    match String.split_on_char '/' s with
    | [ n_str; m_str ] -> (
        match (int_of_string_opt n_str, int_of_string_opt m_str) with
        | Some n, Some m when n > 0 && m > 0 -> Some (n, m)
        | _ -> None)
    | _ -> None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "flex"; "1" ] -> Ok Flex_1
    | [ "flex"; "auto" ] -> Ok Flex_auto
    | [ "flex"; "initial" ] -> Ok Flex_initial
    | [ "flex"; "none" ] -> Ok Flex_none
    | [ "flex"; "grow" ] | [ "grow" ] -> Ok Flex_grow
    | [ "flex"; "grow"; "0" ] | [ "grow"; "0" ] -> Ok Flex_grow_0
    | [ "flex"; "shrink" ] | [ "shrink" ] -> Ok Flex_shrink
    | [ "flex"; "shrink"; "0" ] | [ "shrink"; "0" ] -> Ok Flex_shrink_0
    | [ "basis"; "0" ] -> Ok Basis_0
    | [ "basis"; "1" ] -> Ok Basis_1
    | [ "basis"; "auto" ] -> Ok Basis_auto
    | [ "basis"; "full" ] -> Ok Basis_full
    | [ "basis"; value ] -> (
        match parse_fraction value with
        | Some (n, m) -> Ok (Basis_fraction (n, m))
        | None ->
            if Spacing.is_named_spacing value then Ok (Basis_named value)
            else err_not_utility)
    | [ "order"; "first" ] -> Ok Order_first
    | [ "order"; "last" ] -> Ok Order_last
    | [ "order"; "none" ] -> Ok Order_none
    | "order" :: rest when rest <> [] -> (
        let value = String.concat "-" rest in
        if
          String.length value > 2
          && value.[0] = '['
          && value.[String.length value - 1] = ']'
        then
          let inner = String.sub value 1 (String.length value - 2) in
          Ok (Order_arbitrary inner)
        else
          match int_of_string_opt value with
          | Some n when n >= 1 -> Ok (Order n)
          | _ -> err_not_utility)
    | "" :: "order" :: rest when rest <> [] -> (
        (* Negative order: -order-4, -order-[var(--value)] *)
        let value = String.concat "-" rest in
        if
          String.length value > 2
          && value.[0] = '['
          && value.[String.length value - 1] = ']'
        then
          let inner = String.sub value 1 (String.length value - 2) in
          Ok (Neg_order_arbitrary inner)
        else
          match int_of_string_opt value with
          | Some n when n >= 1 -> Ok (Neg_order n)
          | _ -> err_not_utility)
    | [ "flex"; value ] when String.length value > 0 && value.[0] = '[' ->
        (* Arbitrary flex: flex-[123] *)
        let len = String.length value in
        if len > 2 && value.[len - 1] = ']' then
          let inner = String.sub value 1 (len - 2) in
          match int_of_string_opt inner with
          | Some n -> Ok (Flex_arbitrary n)
          | None -> err_not_utility
        else err_not_utility
    | [ "flex"; value ] -> (
        (* Try fraction first (e.g., "1/2") *)
        match parse_fraction value with
        | Some (n, m) -> Ok (Flex_fraction (n, m))
        | None -> (
            (* Try numeric value (e.g., "99") *)
            match int_of_string_opt value with
            | Some n when n > 1 -> Ok (Flex_n n)
            | _ -> err_not_utility))
    | _ -> err_not_utility

  let to_class = function
    (* Flex shortcuts *)
    | Flex_1 -> "flex-1"
    | Flex_auto -> "flex-auto"
    | Flex_initial -> "flex-initial"
    | Flex_none -> "flex-none"
    | Flex_n n -> "flex-" ^ string_of_int n
    | Flex_fraction (n, m) -> "flex-" ^ string_of_int n ^ "/" ^ string_of_int m
    | Flex_arbitrary n -> "flex-[" ^ string_of_int n ^ "]"
    (* Grow - Tailwind v4 uses shorter names *)
    | Flex_grow -> "grow"
    | Flex_grow_0 -> "grow-0"
    (* Shrink - Tailwind v4 uses shorter names *)
    | Flex_shrink -> "shrink"
    | Flex_shrink_0 -> "shrink-0"
    (* Basis *)
    | Basis_0 -> "basis-0"
    | Basis_1 -> "basis-1"
    | Basis_auto -> "basis-auto"
    | Basis_full -> "basis-full"
    | Basis_fraction (n, m) ->
        "basis-" ^ string_of_int n ^ "/" ^ string_of_int m
    | Basis_named s -> "basis-" ^ s
    (* Order *)
    | Order n -> "order-" ^ string_of_int n
    | Neg_order n -> "-order-" ^ string_of_int n
    | Neg_order_arbitrary s -> "-order-[" ^ s ^ "]"
    | Order_arbitrary s -> "order-[" ^ s ^ "]"
    | Order_first -> "order-first"
    | Order_last -> "order-last"
    | Order_none -> "order-none"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let flex_1 = utility Flex_1
let flex_auto = utility Flex_auto
let flex_initial = utility Flex_initial
let flex_none = utility Flex_none
let flex_grow = utility Flex_grow
let flex_grow_0 = utility Flex_grow_0
let flex_shrink = utility Flex_shrink
let flex_shrink_0 = utility Flex_shrink_0
let basis_0 = utility Basis_0
let basis_1 = utility Basis_1
let basis_auto = utility Basis_auto
let basis_full = utility Basis_full
let order n = utility (Order n)
let order_first = utility Order_first
let order_last = utility Order_last
let order_none = utility Order_none
