(** Table-related utilities

    What's included:
    - Border collapse: `border-collapse`, `border-separate`.
    - Border spacing: `border-spacing-*`, `border-spacing-x-*`,
      `border-spacing-y-*` with numeric values using CSS variables.
    - Table layout: `table-auto`, `table-fixed`.

    What's not:
    - Other table-specific properties not exposed in the typed `Css` API.

    Parsing contract (`of_string`):
    - Accepts tokens like ["border"; "collapse"], ["border"; "separate"],
      ["border"; "spacing"; n], ["border"; "spacing"; "x"; n],
      ["border"; "spacing"; "y"; n], ["table"; "auto"], ["table"; "fixed"].
    - Unknown tokens yield `Error (`Msg "Not a table utility")`. *)

module Handler = struct
  open Style
  open Css

  (** Local table utility type *)
  type t =
    | Border_collapse
    | Border_separate
    | Border_spacing of int
    | Border_spacing_x of int
    | Border_spacing_y of int
    | Border_spacing_arb of Css.length
    | Border_spacing_x_arb of Css.length
    | Border_spacing_y_arb of Css.length
    | Table_auto
    | Table_fixed
    | Caption_top
    | Caption_bottom

  (** Extensible variant for table utilities *)
  type Utility.base += Self of t

  (** Priority for table utilities - comes before layout utilities *)
  let name = "tables"

  let priority = 4
  let err_not_utility = Error (`Msg "Not a table utility")

  (** CSS variables for border-spacing - initialized to 0 in properties layer *)
  let border_spacing_x_var =
    Var.property_default Css.Length ~initial:Zero ~property_order:20
      "tw-border-spacing-x"

  let border_spacing_y_var =
    Var.property_default Css.Length ~initial:Zero ~property_order:21
      "tw-border-spacing-y"

  (** Get spacing value - uses theme variable var(--spacing-N) *)
  let spacing_value n =
    let decl, len = Theme.spacing_calc n in
    (decl, len)

  (** border-spacing: sets both x and y variables and outputs two-value
      border-spacing *)
  let border_spacing_style n =
    let spacing_decl, spacing_len = spacing_value n in
    (* Set --tw-border-spacing-x and --tw-border-spacing-y to the spacing
       value *)
    let decl_x, x_ref = Var.binding border_spacing_x_var spacing_len in
    let decl_y, y_ref = Var.binding border_spacing_y_var spacing_len in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [
        spacing_decl;
        decl_x;
        decl_y;
        Css.border_spacing [ Var x_ref; Var y_ref ];
      ]

  (** border-spacing-x: sets only x variable *)
  let border_spacing_x_style n =
    let spacing_decl, spacing_len = spacing_value n in
    let decl_x, x_ref = Var.binding border_spacing_x_var spacing_len in
    let _, y_ref = Var.binding border_spacing_y_var Zero in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ spacing_decl; decl_x; Css.border_spacing [ Var x_ref; Var y_ref ] ]

  (** border-spacing-y: sets only y variable *)
  let border_spacing_y_style n =
    let spacing_decl, spacing_len = spacing_value n in
    let _, x_ref = Var.binding border_spacing_x_var Zero in
    let decl_y, y_ref = Var.binding border_spacing_y_var spacing_len in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ spacing_decl; decl_y; Css.border_spacing [ Var x_ref; Var y_ref ] ]

  let border_spacing_arb_style (len : Css.length) =
    let decl_x, x_ref = Var.binding border_spacing_x_var len in
    let decl_y, y_ref = Var.binding border_spacing_y_var len in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ decl_x; decl_y; Css.border_spacing [ Var x_ref; Var y_ref ] ]

  let border_spacing_x_arb_style (len : Css.length) =
    let decl_x, x_ref = Var.binding border_spacing_x_var len in
    let _, y_ref = Var.binding border_spacing_y_var Zero in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ decl_x; Css.border_spacing [ Var x_ref; Var y_ref ] ]

  let border_spacing_y_arb_style (len : Css.length) =
    let _, x_ref = Var.binding border_spacing_x_var Zero in
    let decl_y, y_ref = Var.binding border_spacing_y_var len in
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id
    in
    style
      ~property_rules:(Css.concat property_rules)
      [ decl_y; Css.border_spacing [ Var x_ref; Var y_ref ] ]

  let to_style = function
    | Border_collapse -> style [ Css.border_collapse Collapse ]
    | Border_separate -> style [ Css.border_collapse Separate ]
    | Border_spacing n -> border_spacing_style n
    | Border_spacing_x n -> border_spacing_x_style n
    | Border_spacing_y n -> border_spacing_y_style n
    | Border_spacing_arb len -> border_spacing_arb_style len
    | Border_spacing_x_arb len -> border_spacing_x_arb_style len
    | Border_spacing_y_arb len -> border_spacing_y_arb_style len
    | Table_auto -> style [ Css.table_layout Auto ]
    | Table_fixed -> style [ Css.table_layout Fixed ]
    | Caption_top -> style [ Css.caption_side Top ]
    | Caption_bottom -> style [ Css.caption_side Bottom ]

  let suborder = function
    (* Alphabetical among display utilities (shared priority 4). table=13,
       table-auto=14, table-caption=15, ..., table-fixed=19,
       table-footer-group=20, ... *)
    | Caption_bottom -> 1
    | Caption_top -> 2
    | Table_auto -> 14
    | Table_fixed -> 19
    | Border_collapse -> 30
    | Border_separate -> 31
    | Border_spacing n -> 32 + n
    | Border_spacing_arb _ -> 1000
    | Border_spacing_x n -> 1032 + n
    | Border_spacing_x_arb _ -> 2000
    | Border_spacing_y n -> 2032 + n
    | Border_spacing_y_arb _ -> 3000

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "border"; "collapse" ] -> Ok Border_collapse
    | [ "border"; "separate" ] -> Ok Border_separate
    | [ "border"; "spacing"; n ] when Parse.is_bracket_value n ->
        let inner = Parse.bracket_inner n in
        if String.ends_with ~suffix:"px" inner then
          let s = String.sub inner 0 (String.length inner - 2) in
          match float_of_string_opt s with
          | Some f -> Ok (Border_spacing_arb (Css.Px f))
          | None -> err_not_utility
        else err_not_utility
    | [ "border"; "spacing"; n ] -> (
        match int_of_string_opt n with
        | Some i when i >= 0 -> Ok (Border_spacing i)
        | _ -> err_not_utility)
    | [ "border"; "spacing"; "x"; n ] when Parse.is_bracket_value n ->
        let inner = Parse.bracket_inner n in
        if String.ends_with ~suffix:"px" inner then
          let s = String.sub inner 0 (String.length inner - 2) in
          match float_of_string_opt s with
          | Some f -> Ok (Border_spacing_x_arb (Css.Px f))
          | None -> err_not_utility
        else err_not_utility
    | [ "border"; "spacing"; "x"; n ] -> (
        match int_of_string_opt n with
        | Some i when i >= 0 -> Ok (Border_spacing_x i)
        | _ -> err_not_utility)
    | [ "border"; "spacing"; "y"; n ] when Parse.is_bracket_value n ->
        let inner = Parse.bracket_inner n in
        if String.ends_with ~suffix:"px" inner then
          let s = String.sub inner 0 (String.length inner - 2) in
          match float_of_string_opt s with
          | Some f -> Ok (Border_spacing_y_arb (Css.Px f))
          | None -> err_not_utility
        else err_not_utility
    | [ "border"; "spacing"; "y"; n ] -> (
        match int_of_string_opt n with
        | Some i when i >= 0 -> Ok (Border_spacing_y i)
        | _ -> err_not_utility)
    | [ "table"; "auto" ] -> Ok Table_auto
    | [ "table"; "fixed" ] -> Ok Table_fixed
    | [ "caption"; "top" ] -> Ok Caption_top
    | [ "caption"; "bottom" ] -> Ok Caption_bottom
    | _ -> err_not_utility

  let to_class = function
    | Border_collapse -> "border-collapse"
    | Border_separate -> "border-separate"
    | Border_spacing n -> "border-spacing-" ^ string_of_int n
    | Border_spacing_arb (Px n) ->
        let s = string_of_float n in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "border-spacing-[" ^ s ^ "px]"
    | Border_spacing_arb _ -> "border-spacing-[<length>]"
    | Border_spacing_x n -> "border-spacing-x-" ^ string_of_int n
    | Border_spacing_x_arb (Px n) ->
        let s = string_of_float n in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "border-spacing-x-[" ^ s ^ "px]"
    | Border_spacing_x_arb _ -> "border-spacing-x-[<length>]"
    | Border_spacing_y n -> "border-spacing-y-" ^ string_of_int n
    | Border_spacing_y_arb (Px n) ->
        let s = string_of_float n in
        let s =
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        "border-spacing-y-[" ^ s ^ "px]"
    | Border_spacing_y_arb _ -> "border-spacing-y-[<length>]"
    | Table_auto -> "table-auto"
    | Table_fixed -> "table-fixed"
    | Caption_top -> "caption-top"
    | Caption_bottom -> "caption-bottom"
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API *)
let utility x = Utility.base (Self x)

let border_collapse = utility Border_collapse
let border_separate = utility Border_separate
let border_spacing n = utility (Border_spacing n)
let border_spacing_x n = utility (Border_spacing_x n)
let border_spacing_y n = utility (Border_spacing_y n)
let table_auto = utility Table_auto
let table_fixed = utility Table_fixed
let caption_top = utility Caption_top
let caption_bottom = utility Caption_bottom
