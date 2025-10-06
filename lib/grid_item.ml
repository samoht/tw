(** Grid item placement utilities (col-{i n}, row-{i n}).

    These utilities control how grid items are placed within a grid container.
    They come before display utilities in the cascade order. *)

module Handler = struct
  open Style
  open Css

  type t =
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

  type Utility.base += Self of t

  let name = "grid_item"

  (** Priority 1 - before margin (priority 2) *)
  let priority = 1

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
    (* Column utilities *)
    | Col_auto -> 0
    | Col_span n -> 10 + n
    | Col_span_full -> 23
    | Col_start n -> 30 + n
    | Col_start_auto -> 43
    | Col_end n -> 50 + n
    | Col_end_auto -> 63
    (* Row utilities *)
    | Row_auto -> 70
    | Row_span n -> 80 + n
    | Row_span_full -> 93
    | Row_start n -> 100 + n
    | Row_start_auto -> 113
    | Row_end n -> 120 + n
    | Row_end_auto -> 133

  let err_not_utility = Error (`Msg "Not a grid item utility")

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
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
