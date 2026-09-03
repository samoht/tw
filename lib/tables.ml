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

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  (** Local table utility type *)
  type t =
    | Border_collapse
    | Border_separate
    | Border_spacing of float
    | Border_spacing_x of float
    | Border_spacing_y of float
    (* The author's bracket text travels with the length it denotes, so the
       class name is spelled exactly as it was written. *)
    | Border_spacing_arb of string * [ `Length of Css.length | `Raw of string ]
    | Border_spacing_x_arb of string * [ `Length of Css.length | `Raw of string ]
    | Border_spacing_y_arb of string * [ `Length of Css.length | `Raw of string ]
    | Table_auto
    | Table_fixed
    | Caption_top
    | Caption_bottom

  (** Extensible variant for table utilities *)

  (** Priority for table utilities - comes before layout utilities *)
  let name = "tables"

  let priority _ = 8
  let err_not_utility = Error (`Msg "Not a table utility")

  (** CSS variables for border-spacing - initialized to 0 in the properties
      layer. Tailwind emits these first; a negative property_order with no
      family sorts them ahead of every other composition variable. *)
  let border_spacing_x_var =
    Var.property_default Css.Length ~initial:Zero ~property_order:(-2)
      "tw-border-spacing-x"

  let border_spacing_y_var =
    Var.property_default Css.Length ~initial:Zero ~property_order:(-1)
      "tw-border-spacing-y"

  (** Get spacing value - uses theme variable var(--spacing-N) *)
  let spacing_value ?theme n =
    let decl, len = Theme.spacing_calc_float ?theme n in
    (decl, len)

  (** border-spacing: sets both x and y variables and outputs two-value
      border-spacing *)
  let border_spacing_style ?theme n =
    let spacing_decl, spacing_len = spacing_value ?theme n in
    (* Set --tw-border-spacing-x and --tw-border-spacing-y to the spacing
       value *)
    let decl_x, x_ref = Var.binding border_spacing_x_var spacing_len in
    let decl_y, y_ref = Var.binding border_spacing_y_var spacing_len in
    style
      [
        spacing_decl;
        decl_x;
        decl_y;
        Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]);
      ]

  (** border-spacing-x: sets only x variable *)
  let border_spacing_x_style ?theme n =
    let spacing_decl, spacing_len = spacing_value ?theme n in
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
      [
        spacing_decl;
        decl_x;
        Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]);
      ]

  (** border-spacing-y: sets only y variable *)
  let border_spacing_y_style ?theme n =
    let spacing_decl, spacing_len = spacing_value ?theme n in
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
      [
        spacing_decl;
        decl_y;
        Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]);
      ]

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
      [ decl_x; decl_y; Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]) ]

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
      [ decl_x; Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]) ]

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
      [ decl_y; Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]) ]

  let raw_border_spacing_style axis value =
    let property_rules =
      [
        Var.property_rule border_spacing_x_var;
        Var.property_rule border_spacing_y_var;
      ]
      |> List.filter_map Fun.id |> Css.concat
    in
    let x_ref = Var.reference border_spacing_x_var in
    let y_ref = Var.reference border_spacing_y_var in
    let raw var =
      Css.custom_property ~layer:"utilities" (Var.css_name var) value
    in
    let bindings =
      match axis with
      | `Both -> [ raw border_spacing_x_var; raw border_spacing_y_var ]
      | `X -> [ raw border_spacing_x_var ]
      | `Y -> [ raw border_spacing_y_var ]
    in
    style ~property_rules
      ~metadata:
        [ Var.metadata border_spacing_x_var; Var.metadata border_spacing_y_var ]
      (bindings @ [ Css.border_spacing (Lengths [ Var x_ref; Var y_ref ]) ])

  let to_style theme =
    let border_spacing_style n = border_spacing_style ~theme n in
    let border_spacing_x_style n = border_spacing_x_style ~theme n in
    let border_spacing_y_style n = border_spacing_y_style ~theme n in
    function
    | Border_collapse -> style [ Css.border_collapse Collapse ]
    | Border_separate -> style [ Css.border_collapse Separate ]
    | Border_spacing n -> border_spacing_style n
    | Border_spacing_x n -> border_spacing_x_style n
    | Border_spacing_y n -> border_spacing_y_style n
    | Border_spacing_arb (_, `Length len) -> border_spacing_arb_style len
    | Border_spacing_x_arb (_, `Length len) -> border_spacing_x_arb_style len
    | Border_spacing_y_arb (_, `Length len) -> border_spacing_y_arb_style len
    | Border_spacing_arb (_, `Raw value) -> raw_border_spacing_style `Both value
    | Border_spacing_x_arb (_, `Raw value) -> raw_border_spacing_style `X value
    | Border_spacing_y_arb (_, `Raw value) -> raw_border_spacing_style `Y value
    | Table_auto -> style [ Css.table_layout Auto ]
    | Table_fixed -> style [ Css.table_layout Fixed ]
    | Caption_top -> style [ Css.caption_side Top ]
    | Caption_bottom -> style [ Css.caption_side Bottom ]

  let suborder = function
    | Table_auto | Table_fixed -> 0
    | Caption_bottom | Caption_top -> 1
    | Border_collapse -> 30
    | Border_separate -> 31
    (* Values share their property's slot and use the candidate-name tiebreaker.
       Encoding a spacing magnitude in the suborder lets a large theme step
       escape its family and cross transform-origin. *)
    | Border_spacing _ | Border_spacing_arb _ -> 32
    | Border_spacing_x _ | Border_spacing_x_arb _ -> 33
    | Border_spacing_y _ | Border_spacing_y_arb _ -> 34

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "border"; "collapse" ] -> Ok Border_collapse
    | [ "border"; "separate" ] -> Ok Border_separate
    | [ "border"; "spacing"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match Parse.arbitrary_length inner with
        | Some len -> Ok (Border_spacing_arb (inner, `Length len))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some value -> Ok (Border_spacing_arb (inner, `Raw value))
            | None -> err_not_utility))
    | [ "border"; "spacing"; n ] -> (
        match Parse.spacing_value ~name:"border-spacing" n with
        | Ok f when Theme.has_spacing_step ~theme f -> Ok (Border_spacing f)
        | Ok _ | Error _ -> err_not_utility)
    | [ "border"; "spacing"; "x"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match Parse.arbitrary_length inner with
        | Some len -> Ok (Border_spacing_x_arb (inner, `Length len))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some value -> Ok (Border_spacing_x_arb (inner, `Raw value))
            | None -> err_not_utility))
    | [ "border"; "spacing"; "x"; n ] -> (
        match Parse.spacing_value ~name:"border-spacing-x" n with
        | Ok f when Theme.has_spacing_step ~theme f -> Ok (Border_spacing_x f)
        | Ok _ | Error _ -> err_not_utility)
    | [ "border"; "spacing"; "y"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match Parse.arbitrary_length inner with
        | Some len -> Ok (Border_spacing_y_arb (inner, `Length len))
        | None -> (
            match Parse.arbitrary_declaration_value inner with
            | Some value -> Ok (Border_spacing_y_arb (inner, `Raw value))
            | None -> err_not_utility))
    | [ "border"; "spacing"; "y"; n ] -> (
        match Parse.spacing_value ~name:"border-spacing-y" n with
        | Ok f when Theme.has_spacing_step ~theme f -> Ok (Border_spacing_y f)
        | Ok _ | Error _ -> err_not_utility)
    | [ "table"; "auto" ] -> Ok Table_auto
    | [ "table"; "fixed" ] -> Ok Table_fixed
    | [ "caption"; "top" ] -> Ok Caption_top
    | [ "caption"; "bottom" ] -> Ok Caption_bottom
    | _ -> err_not_utility

  let to_class = function
    | Border_collapse -> "border-collapse"
    | Border_separate -> "border-separate"
    | Border_spacing n ->
        "border-spacing-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25))
    | Border_spacing_arb (spelling, _) -> "border-spacing-[" ^ spelling ^ "]"
    | Border_spacing_x n ->
        "border-spacing-x-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25))
    | Border_spacing_x_arb (spelling, _) ->
        "border-spacing-x-[" ^ spelling ^ "]"
    | Border_spacing_y n ->
        "border-spacing-y-" ^ Spacing.pp_spacing_suffix (`Rem (n *. 0.25))
    | Border_spacing_y_arb (spelling, _) ->
        "border-spacing-y-[" ^ spelling ^ "]"
    | Table_auto -> "table-auto"
    | Table_fixed -> "table-fixed"
    | Caption_top -> "caption-top"
    | Caption_bottom -> "caption-bottom"

  let examples =
    [
      Border_collapse;
      Border_spacing 1.;
      Border_spacing_x 1.;
      Border_spacing_y 1.;
      Table_auto;
      Caption_top;
    ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register handler with Utility system *)

(** Public API *)
let utility = Utility_factory.v

let border_collapse = utility Border_collapse
let border_separate = utility Border_separate
let border_spacing' n = utility (Border_spacing n)
let border_spacing n = border_spacing' (float_of_int n)
let border_spacing_x n = utility (Border_spacing_x n)
let border_spacing_y n = utility (Border_spacing_y n)

(* Note: the axis variants' [n] is still a float multiplier, e.g., 4.0 for
   border-spacing-x-4; only the combined [border_spacing] is exposed through
   [Tw], so only it follows the int/prime-float convention. *)
let table_auto = utility Table_auto
let table_fixed = utility Table_fixed
let caption_top = utility Caption_top
let caption_bottom = utility Caption_bottom
