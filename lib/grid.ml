(** Grid utilities. *)

open Style
open Css

(** Local grid utility type *)
type t =
  (* Display *)
  | Grid
  | Inline_grid
  (* Column *)
  | Col_auto
  | Col_span of int
  | Col_span_full
  | Col_start of int
  | Col_start_auto
  | Col_end of int
  | Col_end_auto
  (* Row *)
  | Row_auto
  | Row_span of int
  | Row_span_full
  | Row_start of int
  | Row_start_auto
  | Row_end of int
  | Row_end_auto

(** Extensible variant for grid utilities *)
type Utility.base += Grid_util of t

let wrap x = Grid_util x
let unwrap = function Grid_util x -> Some x | _ -> None
let base x = Utility.base (wrap x)
let grid = base Grid
let inline_grid = base Inline_grid
let col_auto = base Col_auto
let col_span n = base (Col_span n)
let col_span_full = base Col_span_full
let col_start n = base (Col_start n)
let col_start_auto = base Col_start_auto
let col_end n = base (Col_end n)
let col_end_auto = base Col_end_auto
let row_auto = base Row_auto
let row_span n = base (Row_span n)
let row_span_full = base Row_span_full
let row_start n = base (Row_start n)
let row_start_auto = base Row_start_auto
let row_end n = base (Row_end n)
let row_end_auto = base Row_end_auto
let grid' = style "grid" [ display Grid ]
let inline_grid' = style "inline-grid" [ display Inline_grid ]
let col_auto' = style "col-auto" [ grid_column_start Auto ]

let col_span' n =
  style
    (String.concat "-" [ "col"; "span"; string_of_int n ])
    [ grid_column (Span n, Auto) ]

let col_span_full' = style "col-span-full" [ grid_column (Num 1, Num (-1)) ]

let col_start' n =
  style
    (String.concat "-" [ "col"; "start"; string_of_int n ])
    [ grid_column_start (Num n) ]

let col_start_auto' = style "col-start-auto" [ grid_column_start Auto ]

let col_end' n =
  style
    (String.concat "-" [ "col"; "end"; string_of_int n ])
    [ grid_column_end (Num n) ]

let col_end_auto' = style "col-end-auto" [ grid_column_end Auto ]
let row_auto' = style "row-auto" [ grid_row_start Auto ]

let row_span' n =
  style
    (String.concat "-" [ "row"; "span"; string_of_int n ])
    [ grid_row (Span n, Auto) ]

let row_span_full' = style "row-span-full" [ grid_row (Num 1, Num (-1)) ]

let row_start' n =
  style
    (String.concat "-" [ "row"; "start"; string_of_int n ])
    [ grid_row_start (Num n) ]

let row_start_auto' = style "row-start-auto" [ grid_row_start Auto ]

let row_end' n =
  style
    (String.concat "-" [ "row"; "end"; string_of_int n ])
    [ grid_row_end (Num n) ]

let row_end_auto' = style "row-end-auto" [ grid_row_end Auto ]

let to_style = function
  | Grid -> grid'
  | Inline_grid -> inline_grid'
  | Col_auto -> col_auto'
  | Col_span n -> col_span' n
  | Col_span_full -> col_span_full'
  | Col_start n -> col_start' n
  | Col_start_auto -> col_start_auto'
  | Col_end n -> col_end' n
  | Col_end_auto -> col_end_auto'
  | Row_auto -> row_auto'
  | Row_span n -> row_span' n
  | Row_span_full -> row_span_full'
  | Row_start n -> row_start' n
  | Row_start_auto -> row_start_auto'
  | Row_end n -> row_end' n
  | Row_end_auto -> row_end_auto'

let suborder = function
  | Grid -> 0
  | Inline_grid -> 1
  | Col_auto -> 10
  | Col_span n -> 20 + n
  | Col_span_full -> 33
  | Col_start n -> 40 + n
  | Col_start_auto -> 53
  | Col_end n -> 60 + n
  | Col_end_auto -> 73
  | Row_auto -> 80
  | Row_span n -> 90 + n
  | Row_span_full -> 103
  | Row_start n -> 110 + n
  | Row_start_auto -> 123
  | Row_end n -> 130 + n
  | Row_end_auto -> 143

let err_not_utility = Error (`Msg "Not a grid utility")

let of_string = function
  | [ "grid" ] -> Ok Grid
  | [ "inline"; "grid" ] -> Ok Inline_grid
  | [ "col"; "auto" ] -> Ok Col_auto
  | [ "col"; "span"; "full" ] -> Ok Col_span_full
  | [ "col"; "span"; n ] -> (
      match Parse.int_bounded ~name:"col-span" ~min:1 ~max:12 n with
      | Ok i -> Ok (Col_span i)
      | Error _ -> err_not_utility)
  | [ "col"; "start"; "auto" ] -> Ok Col_start_auto
  | [ "col"; "start"; n ] -> (
      match Parse.int_pos ~name:"col-start" n with
      | Ok i -> Ok (Col_start i)
      | Error _ -> err_not_utility)
  | [ "col"; "end"; "auto" ] -> Ok Col_end_auto
  | [ "col"; "end"; n ] -> (
      match Parse.int_pos ~name:"col-end" n with
      | Ok i -> Ok (Col_end i)
      | Error _ -> err_not_utility)
  | [ "row"; "auto" ] -> Ok Row_auto
  | [ "row"; "span"; "full" ] -> Ok Row_span_full
  | [ "row"; "span"; n ] -> (
      match Parse.int_bounded ~name:"row-span" ~min:1 ~max:12 n with
      | Ok i -> Ok (Row_span i)
      | Error _ -> err_not_utility)
  | [ "row"; "start"; "auto" ] -> Ok Row_start_auto
  | [ "row"; "start"; n ] -> (
      match Parse.int_pos ~name:"row-start" n with
      | Ok i -> Ok (Row_start i)
      | Error _ -> err_not_utility)
  | [ "row"; "end"; "auto" ] -> Ok Row_end_auto
  | [ "row"; "end"; n ] -> (
      match Parse.int_pos ~name:"row-end" n with
      | Ok i -> Ok (Row_end i)
      | Error _ -> err_not_utility)
  | _ -> err_not_utility

(** Priority for grid utilities *)
let priority = 1

(** Register handler with Utility system *)
let () =
  Utility.register ~wrap ~unwrap { to_style; priority; suborder; of_string }

module Handler = struct
  type nonrec t = t

  let of_string = of_string
  let suborder = suborder
  let to_style = to_style
  let order x = (priority, suborder x)
end
