(** Grid item placement utilities (col-{i n}, row-{i n}).

    These utilities control how grid items are placed within a grid container.
    They come before display utilities in the cascade order. *)

(* Generate themed grid_column style: custom declaration + var reference when
   theme value is set, otherwise bare theme_ref fallback *)
let themed_decl name value_str =
  match int_of_string_opt value_str with
  | Some n -> Css.custom_declaration ~layer:"theme" ("--" ^ name) Css.Int n
  | None ->
      Css.custom_declaration ~layer:"theme" ("--" ^ name) Css.String value_str

let grid_col_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_column (Var ref, Auto) ]
  | None ->
      Style.style
        [
          Css.grid_column
            ( Var
                (Var.theme_ref name
                   ~default:(Css.Auto : Css.grid_line)
                   ~default_css:"auto"),
              Auto );
        ]

let grid_col_start_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_column_start (Var ref) ]
  | None ->
      Style.style
        [
          Css.grid_column_start
            (Var
               (Var.theme_ref name
                  ~default:(Css.Auto : Css.grid_line)
                  ~default_css:"auto"));
        ]

let grid_col_end_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_column_end (Var ref) ]
  | None ->
      Style.style
        [
          Css.grid_column_end
            (Var
               (Var.theme_ref name
                  ~default:(Css.Auto : Css.grid_line)
                  ~default_css:"auto"));
        ]

let grid_row_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_row (Var ref, Auto) ]
  | None ->
      Style.style
        [
          Css.grid_row
            ( Var
                (Var.theme_ref name
                   ~default:(Css.Auto : Css.grid_line)
                   ~default_css:"auto"),
              Auto );
        ]

let grid_row_start_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_row_start (Var ref) ]
  | None ->
      Style.style
        [
          Css.grid_row_start
            (Var
               (Var.theme_ref name
                  ~default:(Css.Auto : Css.grid_line)
                  ~default_css:"auto"));
        ]

let grid_row_end_themed_style name () =
  match Var.get_theme_value name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      let ref : Css.grid_line Css.var = Css.var_ref ~layer:"theme" name in
      Style.style [ decl; Css.grid_row_end (Var ref) ]
  | None ->
      Style.style
        [
          Css.grid_row_end
            (Var
               (Var.theme_ref name
                  ~default:(Css.Auto : Css.grid_line)
                  ~default_css:"auto"));
        ]

module Handler = struct
  open Style
  open Css

  type t =
    (* Column *)
    | Col of int
    | Neg_col of int (* -col-12 = calc(12 * -1) *)
    | Col_arbitrary of string (* col-[span_123/span_123] *)
    | Col_auto
    | Col_span of int
    | Col_span_arbitrary of string (* col-span-[var(--my-variable)] *)
    | Col_span_full
    | Col_start of int
    | Neg_col_start of int
    | Col_start_auto
    | Col_start_arbitrary of string
    | Col_start_named of string (* col-start-custom *)
    | Col_end of int
    | Neg_col_end of int
    | Col_end_auto
    | Col_end_arbitrary of string
    | Col_end_named of string (* col-end-custom *)
    (* Row *)
    | Row of int
    | Neg_row of int
    | Row_arbitrary of string
    | Row_auto
    | Row_span of int
    | Row_span_arbitrary of string (* row-span-[var(--my-variable)] *)
    | Row_span_full
    | Row_start of int
    | Neg_row_start of int
    | Row_start_auto
    | Row_start_arbitrary of string
    | Row_start_named of string (* row-start-custom *)
    | Row_end of int
    | Neg_row_end of int
    | Row_end_auto
    | Row_end_arbitrary of string
    | Row_end_named of string (* row-end-custom *)

  type Utility.base += Self of t

  let name = "grid_item"

  (** Priority 1 - before margin (priority 2) *)
  let priority = 1

  let col n = style [ grid_column (Num n, Auto) ]
  let neg_col n = style [ grid_column (Num (-n), Auto) ]
  let col_arbitrary s = style [ grid_column (Arbitrary s, Auto) ]
  let col_auto () = grid_col_themed_style "grid-column-auto" ()
  let col_span n = style [ grid_column (Span n, Span n) ]

  let col_span_arbitrary s =
    style [ grid_column (Arbitrary ("span " ^ s), Arbitrary ("span " ^ s)) ]

  let col_span_full = style [ grid_column (Num 1, Num (-1)) ]
  let col_start n = style [ grid_column_start (Num n) ]
  let neg_col_start n = style [ grid_column_start (Num (-n)) ]
  let col_start_arbitrary s = style [ grid_column_start (Arbitrary s) ]

  let col_start_auto () =
    grid_col_start_themed_style "grid-column-start-auto" ()

  let col_start_named s =
    grid_col_start_themed_style ("grid-column-start-" ^ s) ()

  let col_end n = style [ grid_column_end (Num n) ]
  let neg_col_end n = style [ grid_column_end (Num (-n)) ]
  let col_end_arbitrary s = style [ grid_column_end (Arbitrary s) ]
  let col_end_auto () = grid_col_end_themed_style "grid-column-end-auto" ()
  let col_end_named s = grid_col_end_themed_style ("grid-column-end-" ^ s) ()
  let row n = style [ grid_row (Num n, Auto) ]
  let neg_row n = style [ grid_row (Num (-n), Auto) ]
  let row_arbitrary s = style [ grid_row (Arbitrary s, Auto) ]
  let row_auto () = grid_row_themed_style "grid-row-auto" ()
  let row_span n = style [ grid_row (Span n, Span n) ]

  let row_span_arbitrary s =
    style [ grid_row (Arbitrary ("span " ^ s), Arbitrary ("span " ^ s)) ]

  let row_span_full = style [ grid_row (Num 1, Num (-1)) ]
  let row_start n = style [ grid_row_start (Num n) ]
  let neg_row_start n = style [ grid_row_start (Num (-n)) ]
  let row_start_arbitrary s = style [ grid_row_start (Arbitrary s) ]
  let row_start_auto () = grid_row_start_themed_style "grid-row-start-auto" ()
  let row_start_named s = grid_row_start_themed_style ("grid-row-start-" ^ s) ()
  let row_end n = style [ grid_row_end (Num n) ]
  let neg_row_end n = style [ grid_row_end (Num (-n)) ]
  let row_end_arbitrary s = style [ grid_row_end (Arbitrary s) ]
  let row_end_auto () = grid_row_end_themed_style "grid-row-end-auto" ()
  let row_end_named s = grid_row_end_themed_style ("grid-row-end-" ^ s) ()

  let to_style = function
    | Col n -> col n
    | Neg_col n -> neg_col n
    | Col_arbitrary s -> col_arbitrary s
    | Col_auto -> col_auto ()
    | Col_span n -> col_span n
    | Col_span_arbitrary s -> col_span_arbitrary s
    | Col_span_full -> col_span_full
    | Col_start n -> col_start n
    | Neg_col_start n -> neg_col_start n
    | Col_start_arbitrary s -> col_start_arbitrary s
    | Col_start_auto -> col_start_auto ()
    | Col_start_named s -> col_start_named s
    | Col_end n -> col_end n
    | Neg_col_end n -> neg_col_end n
    | Col_end_arbitrary s -> col_end_arbitrary s
    | Col_end_auto -> col_end_auto ()
    | Col_end_named s -> col_end_named s
    | Row n -> row n
    | Neg_row n -> neg_row n
    | Row_arbitrary s -> row_arbitrary s
    | Row_auto -> row_auto ()
    | Row_span n -> row_span n
    | Row_span_arbitrary s -> row_span_arbitrary s
    | Row_span_full -> row_span_full
    | Row_start n -> row_start n
    | Neg_row_start n -> neg_row_start n
    | Row_start_arbitrary s -> row_start_arbitrary s
    | Row_start_auto -> row_start_auto ()
    | Row_start_named s -> row_start_named s
    | Row_end n -> row_end n
    | Neg_row_end n -> neg_row_end n
    | Row_end_arbitrary s -> row_end_arbitrary s
    | Row_end_auto -> row_end_auto ()
    | Row_end_named s -> row_end_named s

  let suborder = function
    (* Column utilities - negative, positive, arbitrary, auto, span, span-arb,
       span-full *)
    | Neg_col n -> n
    | Col n -> 100 + n
    | Col_arbitrary _ -> 200
    | Col_auto -> 300
    | Col_span n -> 400 + n
    | Col_span_arbitrary _ -> 500
    | Col_span_full -> 600
    | Neg_col_start n -> 700 + n
    | Col_start n -> 800 + n
    | Col_start_arbitrary _ -> 900
    | Col_start_auto -> 950
    | Col_start_named _ -> 960
    | Neg_col_end n -> 1000 + n
    | Col_end n -> 1100 + n
    | Col_end_arbitrary _ -> 1200
    | Col_end_auto -> 1250
    | Col_end_named _ -> 1260
    (* Row utilities *)
    | Neg_row n -> 1300 + n
    | Row n -> 1400 + n
    | Row_arbitrary _ -> 1500
    | Row_auto -> 1600
    | Row_span n -> 1700 + n
    | Row_span_arbitrary _ -> 1800
    | Row_span_full -> 1900
    | Neg_row_start n -> 2000 + n
    | Row_start n -> 2100 + n
    | Row_start_arbitrary _ -> 2200
    | Row_start_auto -> 2250
    | Row_start_named _ -> 2260
    | Neg_row_end n -> 2300 + n
    | Row_end n -> 2400 + n
    | Row_end_arbitrary _ -> 2500
    | Row_end_auto -> 2550
    | Row_end_named _ -> 2560

  let err_not_utility = Error (`Msg "Not a grid item utility")

  let parse_arbitrary s =
    (* Parse [value] to value, replacing underscores with spaces *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      Some (String.map (fun c -> if c = '_' then ' ' else c) inner)
    else None

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "col"; "auto" ] -> Ok Col_auto
    | [ "col"; "span"; "full" ] -> Ok Col_span_full
    | [ "col"; "span"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Col_span_arbitrary v)
        | None -> err_not_utility)
    | [ "col"; "span"; n ] -> (
        match Parse.int_pos ~name:"col-span" n with
        | Ok i -> Ok (Col_span i)
        | Error _ -> err_not_utility)
    | [ "col"; "start"; "auto" ] -> Ok Col_start_auto
    | [ "col"; "start"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Col_start_arbitrary v)
        | None -> err_not_utility)
    | [ "col"; "start"; n ] -> (
        match Parse.int_pos ~name:"col-start" n with
        | Ok i -> Ok (Col_start i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Var.get_theme_value ("grid-column-start-" ^ n) <> None
            then Ok (Col_start_named n)
            else err_not_utility)
    | [ ""; "col"; "start"; n ] -> (
        (* Negative col-start: -col-start-12 *)
        match Parse.int_pos ~name:"-col-start" n with
        | Ok i -> Ok (Neg_col_start i)
        | Error _ -> err_not_utility)
    | [ "col"; "end"; "auto" ] -> Ok Col_end_auto
    | [ "col"; "end"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Col_end_arbitrary v)
        | None -> err_not_utility)
    | [ "col"; "end"; n ] -> (
        match Parse.int_pos ~name:"col-end" n with
        | Ok i -> Ok (Col_end i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Var.get_theme_value ("grid-column-end-" ^ n) <> None
            then Ok (Col_end_named n)
            else err_not_utility)
    | [ ""; "col"; "end"; n ] -> (
        (* Negative col-end: -col-end-12 *)
        match Parse.int_pos ~name:"-col-end" n with
        | Ok i -> Ok (Neg_col_end i)
        | Error _ -> err_not_utility)
    | [ "col"; n ] when String.length n > 0 && n.[0] = '[' -> (
        (* Arbitrary col: col-[span_123/span_123] *)
        match parse_arbitrary n with
        | Some v -> Ok (Col_arbitrary v)
        | None -> err_not_utility)
    | [ "col"; n ] -> (
        match Parse.int_pos ~name:"col" n with
        | Ok i -> Ok (Col i)
        | Error _ -> err_not_utility)
    | [ ""; "col"; n ] -> (
        (* Negative col: -col-12 *)
        match Parse.int_pos ~name:"-col" n with
        | Ok i -> Ok (Neg_col i)
        | Error _ -> err_not_utility)
    | [ "row"; "auto" ] -> Ok Row_auto
    | [ "row"; "span"; "full" ] -> Ok Row_span_full
    | [ "row"; "span"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Row_span_arbitrary v)
        | None -> err_not_utility)
    | [ "row"; "span"; n ] -> (
        match Parse.int_pos ~name:"row-span" n with
        | Ok i -> Ok (Row_span i)
        | Error _ -> err_not_utility)
    | [ "row"; n ] when String.length n > 0 && n.[0] = '[' -> (
        (* Arbitrary row: row-[span_123/span_123] *)
        match parse_arbitrary n with
        | Some v -> Ok (Row_arbitrary v)
        | None -> err_not_utility)
    | [ "row"; n ] -> (
        match Parse.int_pos ~name:"row" n with
        | Ok i -> Ok (Row i)
        | Error _ -> err_not_utility)
    | [ ""; "row"; n ] -> (
        (* Negative row: -row-12 *)
        match Parse.int_pos ~name:"-row" n with
        | Ok i -> Ok (Neg_row i)
        | Error _ -> err_not_utility)
    | [ "row"; "start"; "auto" ] -> Ok Row_start_auto
    | [ "row"; "start"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Row_start_arbitrary v)
        | None -> err_not_utility)
    | [ "row"; "start"; n ] -> (
        match Parse.int_pos ~name:"row-start" n with
        | Ok i -> Ok (Row_start i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Var.get_theme_value ("grid-row-start-" ^ n) <> None
            then Ok (Row_start_named n)
            else err_not_utility)
    | [ ""; "row"; "start"; n ] -> (
        (* Negative row-start: -row-start-12 *)
        match Parse.int_pos ~name:"-row-start" n with
        | Ok i -> Ok (Neg_row_start i)
        | Error _ -> err_not_utility)
    | [ "row"; "end"; "auto" ] -> Ok Row_end_auto
    | [ "row"; "end"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Row_end_arbitrary v)
        | None -> err_not_utility)
    | [ "row"; "end"; n ] -> (
        match Parse.int_pos ~name:"row-end" n with
        | Ok i -> Ok (Row_end i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Var.get_theme_value ("grid-row-end-" ^ n) <> None
            then Ok (Row_end_named n)
            else err_not_utility)
    | [ ""; "row"; "end"; n ] -> (
        (* Negative row-end: -row-end-12 *)
        match Parse.int_pos ~name:"-row-end" n with
        | Ok i -> Ok (Neg_row_end i)
        | Error _ -> err_not_utility)
    | _ -> err_not_utility

  let to_class_arbitrary s =
    (* Convert value to arbitrary format: replace spaces with underscores *)
    "[" ^ String.map (fun c -> if c = ' ' then '_' else c) s ^ "]"

  let to_class = function
    (* Column *)
    | Col n -> "col-" ^ string_of_int n
    | Neg_col n -> "-col-" ^ string_of_int n
    | Col_arbitrary s -> "col-" ^ to_class_arbitrary s
    | Col_auto -> "col-auto"
    | Col_span n -> "col-span-" ^ string_of_int n
    | Col_span_arbitrary s -> "col-span-" ^ to_class_arbitrary s
    | Col_span_full -> "col-span-full"
    | Col_start n -> "col-start-" ^ string_of_int n
    | Neg_col_start n -> "-col-start-" ^ string_of_int n
    | Col_start_arbitrary s -> "col-start-" ^ to_class_arbitrary s
    | Col_start_auto -> "col-start-auto"
    | Col_start_named s -> "col-start-" ^ s
    | Col_end n -> "col-end-" ^ string_of_int n
    | Neg_col_end n -> "-col-end-" ^ string_of_int n
    | Col_end_arbitrary s -> "col-end-" ^ to_class_arbitrary s
    | Col_end_auto -> "col-end-auto"
    | Col_end_named s -> "col-end-" ^ s
    (* Row *)
    | Row n -> "row-" ^ string_of_int n
    | Neg_row n -> "-row-" ^ string_of_int n
    | Row_arbitrary s -> "row-" ^ to_class_arbitrary s
    | Row_auto -> "row-auto"
    | Row_span n -> "row-span-" ^ string_of_int n
    | Row_span_arbitrary s -> "row-span-" ^ to_class_arbitrary s
    | Row_span_full -> "row-span-full"
    | Row_start n -> "row-start-" ^ string_of_int n
    | Neg_row_start n -> "-row-start-" ^ string_of_int n
    | Row_start_arbitrary s -> "row-start-" ^ to_class_arbitrary s
    | Row_start_auto -> "row-start-auto"
    | Row_start_named s -> "row-start-" ^ s
    | Row_end n -> "row-end-" ^ string_of_int n
    | Neg_row_end n -> "-row-end-" ^ string_of_int n
    | Row_end_arbitrary s -> "row-end-" ^ to_class_arbitrary s
    | Row_end_auto -> "row-end-auto"
    | Row_end_named s -> "row-end-" ^ s
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

let utility x = Utility.base (Self x)
let col n = utility (Col n)
let col_auto = utility Col_auto
let col_span n = utility (Col_span n)
let col_span_full = utility Col_span_full
let col_start n = utility (Col_start n)
let col_start_auto = utility Col_start_auto
let col_end n = utility (Col_end n)
let col_end_auto = utility Col_end_auto
let row n = utility (Row n)
let row_auto = utility Row_auto
let row_span n = utility (Row_span n)
let row_span_full = utility Row_span_full
let row_start n = utility (Row_start n)
let row_start_auto = utility Row_start_auto
let row_end n = utility (Row_end n)
let row_end_auto = utility Row_end_auto
