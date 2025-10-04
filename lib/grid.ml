(** Grid utilities. *)

module Handler = struct
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
  type Utility.base += Self of t

  (** Priority for grid utilities. Set to 4 to match other display utilities
      (block, inline, flex, etc.) since grid and inline-grid set the display
      property. *)
  let name = "grid"

  let priority = 4
  let grid = style [ display Grid ]
  let inline_grid = style [ display Inline_grid ]
  let col_auto = style [ grid_column_start Auto ]
  let col_span n = style [ grid_column (Span n, Auto) ]
  let col_span_full = style [ grid_column (Num 1, Num (-1)) ]
  let col_start n = style [ grid_column_start (Num n) ]
  let col_start_auto = style [ grid_column_start Auto ]
  let col_end n = style [ grid_column_end (Num n) ]
  let col_end_auto = style [ grid_column_end Auto ]
  let row_auto = style [ grid_row_start Auto ]
  let row_span n = style [ grid_row (Span n, Auto) ]
  let row_span_full = style [ grid_row (Num 1, Num (-1)) ]
  let row_start n = style [ grid_row_start (Num n) ]
  let row_start_auto = style [ grid_row_start Auto ]
  let row_end n = style [ grid_row_end (Num n) ]
  let row_end_auto = style [ grid_row_end Auto ]

  let to_style = function
    | Grid -> grid
    | Inline_grid -> inline_grid
    | Col_auto -> col_auto
    | Col_span n -> col_span n
    | Col_span_full -> col_span_full
    | Col_start n -> col_start n
    | Col_start_auto -> col_start_auto
    | Col_end n -> col_end n
    | Col_end_auto -> col_end_auto
    | Row_auto -> row_auto
    | Row_span n -> row_span n
    | Row_span_full -> row_span_full
    | Row_start n -> row_start n
    | Row_start_auto -> row_start_auto
    | Row_end n -> row_end n
    | Row_end_auto -> row_end_auto

  let suborder = function
    (* Display - suborder matches alphabetical position across all display
       utilities *)
    | Grid -> 3
    | Inline_grid -> 8
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

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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

  let to_class = function
    (* Display *)
    | Grid -> "grid"
    | Inline_grid -> "inline-grid"
    (* Column *)
    | Col_auto -> "col-auto"
    | Col_span n -> "col-span-" ^ string_of_int n
    | Col_span_full -> "col-span-full"
    | Col_start n -> "col-start-" ^ string_of_int n
    | Col_start_auto -> "col-start-auto"
    | Col_end n -> "col-end-" ^ string_of_int n
    | Col_end_auto -> "col-end-auto"
    (* Row *)
    | Row_auto -> "row-auto"
    | Row_span n -> "row-span-" ^ string_of_int n
    | Row_span_full -> "row-span-full"
    | Row_start n -> "row-start-" ^ string_of_int n
    | Row_start_auto -> "row-start-auto"
    | Row_end n -> "row-end-" ^ string_of_int n
    | Row_end_auto -> "row-end-auto"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let grid = utility Grid
let inline_grid = utility Inline_grid
let col_auto = utility Col_auto
let col_span n = utility (Col_span n)
let col_span_full = utility Col_span_full
let col_start n = utility (Col_start n)
let col_start_auto = utility Col_start_auto
let col_end n = utility (Col_end n)
let col_end_auto = utility Col_end_auto
let row_auto = utility Row_auto
let row_span n = utility (Row_span n)
let row_span_full = utility Row_span_full
let row_start n = utility (Row_start n)
let row_start_auto = utility Row_start_auto
let row_end n = utility (Row_end n)
let row_end_auto = utility Row_end_auto
