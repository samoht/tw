module Css = Cascade.Css

(* A custom declaration and a reference to it when the theme sets the value,
   else the bare theme reference. *)
let themed_decl name value_str =
  match int_of_string_opt value_str with
  | Some n -> Css.custom_property ~layer:"theme" ("--" ^ name) (string_of_int n)
  | None -> Css.custom_property ~layer:"theme" ("--" ^ name) value_str

let themed_grid_line ?theme name
    (declaration : Css.grid_line -> Css.declaration) =
  let ref : Css.grid_line Css.var =
    Var.theme_ref name ~default:(Css.Auto : Css.grid_line) ~default_css:"auto"
  in
  let grid_line : Css.grid_line = Css.Var ref in
  let grid_decl = declaration grid_line in
  match Scheme.theme_value theme name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      Style.style [ decl; grid_decl ]
  | None -> Style.style [ grid_decl ]

let grid_col_start_themed_style ?theme name () =
  themed_grid_line ?theme name Css.grid_column_start

let grid_col_end_themed_style ?theme name () =
  themed_grid_line ?theme name Css.grid_column_end

let grid_row_start_themed_style ?theme name () =
  themed_grid_line ?theme name Css.grid_row_start

let grid_row_end_themed_style ?theme name () =
  themed_grid_line ?theme name Css.grid_row_end

let themed_grid_shorthand ?theme name property =
  let ref : Css.grid_line_pair Css.var = Var.bracket name in
  let value : Css.grid_line_pair = Css.Var ref in
  let grid_decl = Css.Declaration.v property value in
  match Scheme.theme_value theme name with
  | Some value_str ->
      let decl = themed_decl name value_str in
      Style.style [ decl; grid_decl ]
  | None -> Style.style [ grid_decl ]

module Handler = struct
  open Style
  open Css

  type 'a bracket = [ `Typed of 'a | `Raw of string ]

  type t =
    | Col of int
    | Neg_col of int (* -col-12 = calc(12 * -1) *)
    (* The author's bracket text travels with the grid line it denotes, so the
       class name is spelled exactly as it was written. A bracket the grid-line
       grammar declines is a [`Raw] token stream. *)
    | Col_arbitrary of string * (Css.grid_line * Css.grid_line) bracket
      (* col-[span_123/span_123] *)
    | Col_auto
    | Col_span of int
    | Col_span_arbitrary of string (* col-span-[var(--my-variable)] *)
    | Col_span_full
    | Col_start of int
    | Neg_col_start of int
    | Col_start_auto
    | Col_start_arbitrary of string * Css.grid_line bracket
    | Col_start_named of string (* col-start-custom *)
    | Col_end of int
    | Neg_col_end of int
    | Col_end_auto
    | Col_end_arbitrary of string * Css.grid_line bracket
    | Col_end_named of string (* col-end-custom *)
    | Row of int
    | Neg_row of int
    | Row_arbitrary of string * (Css.grid_line * Css.grid_line) bracket
    | Row_auto
    | Row_span of int
    | Row_span_arbitrary of string (* row-span-[var(--my-variable)] *)
    | Row_span_full
    | Row_start of int
    | Neg_row_start of int
    | Row_start_auto
    | Row_start_arbitrary of string * Css.grid_line bracket
    | Row_start_named of string (* row-start-custom *)
    | Row_end of int
    | Neg_row_end of int
    | Row_end_auto
    | Row_end_arbitrary of string * Css.grid_line bracket
    | Row_end_named of string (* row-end-custom *)
    | Neg_line_arbitrary of line * string * string
  (* -col-[...]: the bracket as written, and its negated token stream *)

  and line = [ `Col | `Col_start | `Col_end | `Row | `Row_start | `Row_end ]

  let name = "grid_item"

  let line_name : line -> string = function
    | `Col -> "col"
    | `Col_start -> "col-start"
    | `Col_end -> "col-end"
    | `Row -> "row"
    | `Row_start -> "row-start"
    | `Row_end -> "row-end"

  let line_property : line -> string = function
    | `Col -> "grid-column"
    | `Col_start -> "grid-column-start"
    | `Col_end -> "grid-column-end"
    | `Row -> "grid-row"
    | `Row_start -> "grid-row-start"
    | `Row_end -> "grid-row-end"

  (* Before margin (priority 2). *)
  let priority _ = 1
  let col n = style [ grid_column (Num n, Auto) ]
  let neg_col n = style [ grid_column (Num (-n), Auto) ]

  (* The grid line a bracket denotes. [None] is a bracket the grid-line grammar
     cannot read. *)
  let read_gl s : Css.grid_line option =
    let cursor = Cascade.Cursor.of_string s in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_grid_line cursor
    with
    | Ok gl -> Some gl
    | Error _ -> None

  (* [col-[a/b]] takes both lines of the shorthand; a bracket that is one line
     leaves the end line [auto]. *)
  let read_grid_line_pair s : (Css.grid_line * Css.grid_line) option =
    let cursor = Cascade.Cursor.of_string s in
    match
      Cascade.Cursor.try_parse_full_err Css.Properties.read_grid_line_pair
        cursor
    with
    | Ok (Lines (start, end_)) -> Some (start, end_)
    | Ok (Var _) | Error _ ->
        Option.map (fun gl -> (gl, (Auto : Css.grid_line))) (read_gl s)

  (* A token stream on a grid line, negated or not, goes into the line's
     longhand as it stands. *)
  let line_token_stream line v =
    style (Option.to_list (Parse.opaque_declaration (line_property line) v))

  let arbitrary line declaration : _ bracket -> _ = function
    | `Typed v -> style [ declaration v ]
    | `Raw v -> line_token_stream line v

  let col_arbitrary = arbitrary `Col grid_column

  let col_auto ?theme () =
    match Scheme.theme_value theme "grid-column-auto" with
    | Some _ ->
        themed_grid_shorthand ?theme "grid-column-auto"
          Css.Properties.Grid_column
    | None -> style [ grid_column (Auto, Auto) ]

  let col_span n = style [ grid_column (Span n, Span n) ]

  (* [span <v> / span <v>], the shorthand Tailwind writes for a span. The value
     is embedded twice, so it is wrapped twice: a comment left open by the end
     of [v] would otherwise swallow the tokens generated after it. *)
  let span_pair v =
    Option.bind
      (Parse.wrap_declaration_value ~before:"span " ~after:" / span " v)
      (fun before -> Parse.wrap_declaration_value ~before ~after:"" v)

  (* var() substitutes before the <grid-line> grammar applies, so the text in
     [span var(--x)] is not a <custom-ident> and a printer that escapes it as
     one corrupts the parentheses. The declaration parser keeps it raw. A count
     is a plain decimal and a name a CSS identifier; anything else the bracket
     holds - the docs' [<value>] placeholder included - is passed through as
     written, which is what Tailwind does with it. *)
  let span_arbitrary property_name property raw =
    (* The stored text is the class name's, hint included; the value is what
       follows the hint, through the arbitrary-value pipeline. *)
    let s =
      Parse.decode_arbitrary_value
        (Option.value (Parse.value_after_hint raw) ~default:raw)
    in
    match Parse.decimal_int s with
    | Some n -> style [ property (Span n, Span n) ]
    | None ->
        let opaque () =
          match
            Option.bind (span_pair s) (Parse.opaque_declaration property_name)
          with
          | Some declaration -> style [ declaration ]
          | None -> style []
        in
        if Parse.is_var s then
          match
            Option.bind (span_pair s) (Css.parse_declaration property_name)
          with
          | Some declaration -> style [ declaration ]
          | None -> opaque ()
        else if Parse.is_ident s then
          style [ property (Span_name s, Span_name s) ]
        else opaque ()

  let col_span_arbitrary s = span_arbitrary "grid-column" grid_column s
  let col_span_full = style [ grid_column (Num 1, Num (-1)) ]
  let col_start n = style [ grid_column_start (Num n) ]
  let neg_col_start n = style [ grid_column_start (Num (-n)) ]
  let col_start_arbitrary = arbitrary `Col_start grid_column_start

  let col_start_auto ?theme () =
    match Scheme.theme_value theme "grid-column-start-auto" with
    | Some _ -> grid_col_start_themed_style ?theme "grid-column-start-auto" ()
    | None -> style [ grid_column_start Auto ]

  let col_start_named ?theme s =
    grid_col_start_themed_style ?theme ("grid-column-start-" ^ s) ()

  let col_end n = style [ grid_column_end (Num n) ]
  let neg_col_end n = style [ grid_column_end (Num (-n)) ]
  let col_end_arbitrary = arbitrary `Col_end grid_column_end

  let col_end_auto ?theme () =
    match Scheme.theme_value theme "grid-column-end-auto" with
    | Some _ -> grid_col_end_themed_style ?theme "grid-column-end-auto" ()
    | None -> style [ grid_column_end Auto ]

  let col_end_named ?theme s =
    grid_col_end_themed_style ?theme ("grid-column-end-" ^ s) ()

  let row n = style [ grid_row (Num n, Auto) ]
  let neg_row n = style [ grid_row (Num (-n), Auto) ]
  let row_arbitrary = arbitrary `Row grid_row

  let row_auto ?theme () =
    match Scheme.theme_value theme "grid-row-auto" with
    | Some _ ->
        themed_grid_shorthand ?theme "grid-row-auto" Css.Properties.Grid_row
    | None -> style [ grid_row (Auto, Auto) ]

  let row_span n = style [ grid_row (Span n, Span n) ]
  let row_span_arbitrary s = span_arbitrary "grid-row" grid_row s
  let row_span_full = style [ grid_row (Num 1, Num (-1)) ]
  let row_start n = style [ grid_row_start (Num n) ]
  let neg_row_start n = style [ grid_row_start (Num (-n)) ]
  let row_start_arbitrary = arbitrary `Row_start grid_row_start

  let row_start_auto ?theme () =
    match Scheme.theme_value theme "grid-row-start-auto" with
    | Some _ -> grid_row_start_themed_style ?theme "grid-row-start-auto" ()
    | None -> style [ grid_row_start Auto ]

  let row_start_named ?theme s =
    grid_row_start_themed_style ?theme ("grid-row-start-" ^ s) ()

  let row_end n = style [ grid_row_end (Num n) ]
  let neg_row_end n = style [ grid_row_end (Num (-n)) ]
  let row_end_arbitrary = arbitrary `Row_end grid_row_end

  let row_end_auto ?theme () =
    match Scheme.theme_value theme "grid-row-end-auto" with
    | Some _ -> grid_row_end_themed_style ?theme "grid-row-end-auto" ()
    | None -> style [ grid_row_end Auto ]

  let row_end_named ?theme s =
    grid_row_end_themed_style ?theme ("grid-row-end-" ^ s) ()

  let to_style theme =
    let col_auto () = col_auto ~theme () in
    let col_start_auto () = col_start_auto ~theme () in
    let col_start_named s = col_start_named ~theme s in
    let col_end_auto () = col_end_auto ~theme () in
    let col_end_named s = col_end_named ~theme s in
    let row_auto () = row_auto ~theme () in
    let row_start_auto () = row_start_auto ~theme () in
    let row_start_named s = row_start_named ~theme s in
    let row_end_auto () = row_end_auto ~theme () in
    let row_end_named s = row_end_named ~theme s in
    function
    | Col n -> col n
    | Neg_col n -> neg_col n
    | Col_arbitrary (_, pair) -> col_arbitrary pair
    | Col_auto -> col_auto ()
    | Col_span n -> col_span n
    | Col_span_arbitrary s -> col_span_arbitrary s
    | Col_span_full -> col_span_full
    | Col_start n -> col_start n
    | Neg_col_start n -> neg_col_start n
    | Col_start_arbitrary (_, gl) -> col_start_arbitrary gl
    | Col_start_auto -> col_start_auto ()
    | Col_start_named s -> col_start_named s
    | Col_end n -> col_end n
    | Neg_col_end n -> neg_col_end n
    | Col_end_arbitrary (_, gl) -> col_end_arbitrary gl
    | Col_end_auto -> col_end_auto ()
    | Col_end_named s -> col_end_named s
    | Row n -> row n
    | Neg_row n -> neg_row n
    | Row_arbitrary (_, pair) -> row_arbitrary pair
    | Row_auto -> row_auto ()
    | Row_span n -> row_span n
    | Row_span_arbitrary s -> row_span_arbitrary s
    | Row_span_full -> row_span_full
    | Row_start n -> row_start n
    | Neg_row_start n -> neg_row_start n
    | Row_start_arbitrary (_, gl) -> row_start_arbitrary gl
    | Row_start_auto -> row_start_auto ()
    | Row_start_named s -> row_start_named s
    | Row_end n -> row_end n
    | Neg_row_end n -> neg_row_end n
    | Row_end_arbitrary (_, gl) -> row_end_arbitrary gl
    | Row_end_auto -> row_end_auto ()
    | Row_end_named s -> row_end_named s
    | Neg_line_arbitrary (line, _, value) -> line_token_stream line value

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
    | Neg_line_arbitrary (`Col, _, _) -> 99
    | Neg_line_arbitrary (`Col_start, _, _) -> 799
    | Neg_line_arbitrary (`Col_end, _, _) -> 1099
    | Neg_line_arbitrary (`Row, _, _) -> 1399
    | Neg_line_arbitrary (`Row_start, _, _) -> 2099
    | Neg_line_arbitrary (`Row_end, _, _) -> 2399

  (* The bracket text as written, which is what the class name carries,
     data-type hint included. *)
  let parse_arbitrary s =
    if Parse.is_bracket_value s then Some (Parse.bracket_inner s) else None

  (* The same text with a data-type hint taken off, which is what a value reader
     wants: the hint chooses which longhand a bracket lands in and says nothing
     about the value, and every family here writes one longhand. [None] is a
     bracket whose hint is empty, which names no utility. Without this,
     [col-span-[foo:2]] wrote the hint into the declaration. *)
  let parse_arbitrary_value s =
    if Parse.is_bracket_value s then
      Option.map Parse.decode_arbitrary_value
        (Parse.value_after_hint (Parse.bracket_inner s))
    else None

  (* The pair a family stores: the class name's text, and the value read off the
     peeled one, or the token stream Tailwind writes when [read] declines it. *)
  let parse_arbitrary_pair read s =
    match (parse_arbitrary s, parse_arbitrary_value s) with
    | Some raw, Some value -> (
        match read value with
        | Some x -> Some (raw, `Typed x)
        | None ->
            Option.map
              (fun v -> (raw, `Raw v))
              (Parse.arbitrary_declaration_value raw))
    | _ -> None

  (* Tailwind negates any bracket on a grid line as [calc(<value> * -1)]. *)
  let neg_line_arbitrary line n =
    let inner = Parse.bracket_inner n in
    match
      Option.bind
        (Parse.arbitrary_declaration_value inner)
        Parse.negated_declaration_value
    with
    | Some value -> Ok (Neg_line_arbitrary (line, inner, value))
    | None -> Utility.unclaimed

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ ""; "col"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Col n
    | [ ""; "col"; "start"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Col_start n
    | [ ""; "col"; "end"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Col_end n
    | [ ""; "row"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Row n
    | [ ""; "row"; "start"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Row_start n
    | [ ""; "row"; "end"; n ] when Parse.is_bracket_value n ->
        neg_line_arbitrary `Row_end n
    | [ "col"; "auto" ] -> Ok Col_auto
    | [ "col"; "span"; "full" ] -> Ok Col_span_full
    | [ "col"; "span"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Col_span_arbitrary v)
        | None -> Utility.unclaimed)
    | [ "col"; "span"; n ] -> (
        match Parse.int_pos ~name:"col-span" n with
        | Ok i -> Ok (Col_span i)
        | Error _ -> Utility.unclaimed)
    | [ "col"; "start"; "auto" ] -> Ok Col_start_auto
    | [ "col"; "start"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_gl n with
        | Some (v, x) -> Ok (Col_start_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "col"; "start"; n ] -> (
        match Parse.int_pos ~name:"col-start" n with
        | Ok i -> Ok (Col_start i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Scheme.theme_value (Some theme) ("grid-column-start-" ^ n)
                 <> None
            then Ok (Col_start_named n)
            else Utility.unclaimed)
    | [ ""; "col"; "start"; n ] -> (
        match Parse.int_pos ~name:"-col-start" n with
        | Ok i -> Ok (Neg_col_start i)
        | Error _ -> Utility.unclaimed)
    | [ "col"; "end"; "auto" ] -> Ok Col_end_auto
    | [ "col"; "end"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_gl n with
        | Some (v, x) -> Ok (Col_end_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "col"; "end"; n ] -> (
        match Parse.int_pos ~name:"col-end" n with
        | Ok i -> Ok (Col_end i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Scheme.theme_value (Some theme) ("grid-column-end-" ^ n)
                 <> None
            then Ok (Col_end_named n)
            else Utility.unclaimed)
    | [ ""; "col"; "end"; n ] -> (
        match Parse.int_pos ~name:"-col-end" n with
        | Ok i -> Ok (Neg_col_end i)
        | Error _ -> Utility.unclaimed)
    | [ "col"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_grid_line_pair n with
        | Some (v, x) -> Ok (Col_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "col"; n ] -> (
        match Parse.int_pos ~name:"col" n with
        | Ok i -> Ok (Col i)
        | Error _ -> Utility.unclaimed)
    | [ ""; "col"; n ] -> (
        match Parse.int_pos ~name:"-col" n with
        | Ok i -> Ok (Neg_col i)
        | Error _ -> Utility.unclaimed)
    | [ "row"; "auto" ] -> Ok Row_auto
    | [ "row"; "span"; "full" ] -> Ok Row_span_full
    | [ "row"; "span"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary n with
        | Some v -> Ok (Row_span_arbitrary v)
        | None -> Utility.unclaimed)
    | [ "row"; "span"; n ] -> (
        match Parse.int_pos ~name:"row-span" n with
        | Ok i -> Ok (Row_span i)
        | Error _ -> Utility.unclaimed)
    | [ "row"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_grid_line_pair n with
        | Some (v, x) -> Ok (Row_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "row"; n ] -> (
        match Parse.int_pos ~name:"row" n with
        | Ok i -> Ok (Row i)
        | Error _ -> Utility.unclaimed)
    | [ ""; "row"; n ] -> (
        match Parse.int_pos ~name:"-row" n with
        | Ok i -> Ok (Neg_row i)
        | Error _ -> Utility.unclaimed)
    | [ "row"; "start"; "auto" ] -> Ok Row_start_auto
    | [ "row"; "start"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_gl n with
        | Some (v, x) -> Ok (Row_start_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "row"; "start"; n ] -> (
        match Parse.int_pos ~name:"row-start" n with
        | Ok i -> Ok (Row_start i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Scheme.theme_value (Some theme) ("grid-row-start-" ^ n) <> None
            then Ok (Row_start_named n)
            else Utility.unclaimed)
    | [ ""; "row"; "start"; n ] -> (
        match Parse.int_pos ~name:"-row-start" n with
        | Ok i -> Ok (Neg_row_start i)
        | Error _ -> Utility.unclaimed)
    | [ "row"; "end"; "auto" ] -> Ok Row_end_auto
    | [ "row"; "end"; n ] when String.length n > 0 && n.[0] = '[' -> (
        match parse_arbitrary_pair read_gl n with
        | Some (v, x) -> Ok (Row_end_arbitrary (v, x))
        | None -> Utility.unclaimed)
    | [ "row"; "end"; n ] -> (
        match Parse.int_pos ~name:"row-end" n with
        | Ok i -> Ok (Row_end i)
        | Error _ ->
            if
              (not (String.contains n '/'))
              && Scheme.theme_value (Some theme) ("grid-row-end-" ^ n) <> None
            then Ok (Row_end_named n)
            else Utility.unclaimed)
    | [ ""; "row"; "end"; n ] -> (
        match Parse.int_pos ~name:"-row-end" n with
        | Ok i -> Ok (Neg_row_end i)
        | Error _ -> Utility.unclaimed)
    | _ -> Utility.unclaimed

  let to_class_arbitrary s = "[" ^ s ^ "]"

  let to_class = function
    | Col n -> "col-" ^ string_of_int n
    | Neg_col n -> "-col-" ^ string_of_int n
    | Col_arbitrary (s, _) -> "col-" ^ to_class_arbitrary s
    | Col_auto -> "col-auto"
    | Col_span n -> "col-span-" ^ string_of_int n
    | Col_span_arbitrary s -> "col-span-" ^ to_class_arbitrary s
    | Col_span_full -> "col-span-full"
    | Col_start n -> "col-start-" ^ string_of_int n
    | Neg_col_start n -> "-col-start-" ^ string_of_int n
    | Col_start_arbitrary (s, _) -> "col-start-" ^ to_class_arbitrary s
    | Col_start_auto -> "col-start-auto"
    | Col_start_named s -> "col-start-" ^ s
    | Col_end n -> "col-end-" ^ string_of_int n
    | Neg_col_end n -> "-col-end-" ^ string_of_int n
    | Col_end_arbitrary (s, _) -> "col-end-" ^ to_class_arbitrary s
    | Col_end_auto -> "col-end-auto"
    | Col_end_named s -> "col-end-" ^ s
    | Row n -> "row-" ^ string_of_int n
    | Neg_row n -> "-row-" ^ string_of_int n
    | Row_arbitrary (s, _) -> "row-" ^ to_class_arbitrary s
    | Row_auto -> "row-auto"
    | Row_span n -> "row-span-" ^ string_of_int n
    | Row_span_arbitrary s -> "row-span-" ^ to_class_arbitrary s
    | Row_span_full -> "row-span-full"
    | Row_start n -> "row-start-" ^ string_of_int n
    | Neg_row_start n -> "-row-start-" ^ string_of_int n
    | Row_start_arbitrary (s, _) -> "row-start-" ^ to_class_arbitrary s
    | Row_start_auto -> "row-start-auto"
    | Row_start_named s -> "row-start-" ^ s
    | Row_end n -> "row-end-" ^ string_of_int n
    | Neg_row_end n -> "-row-end-" ^ string_of_int n
    | Row_end_arbitrary (s, _) -> "row-end-" ^ to_class_arbitrary s
    | Row_end_auto -> "row-end-auto"
    | Row_end_named s -> "row-end-" ^ s
    | Neg_line_arbitrary (line, s, _) -> "-" ^ line_name line ^ "-[" ^ s ^ "]"

  let examples =
    [
      Col_auto;
      Col_start_auto;
      Col_end_auto;
      Row_auto;
      Row_start_auto;
      Row_end_auto;
    ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
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
