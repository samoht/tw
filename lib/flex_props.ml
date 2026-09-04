(** Flexbox property utilities (grow, shrink, basis, order, flex shortcuts).

    These utilities control flexbox item behavior and come after sizing
    utilities in the cascade order. For flex display utilities (flex,
    inline-flex), see Flex module. For direction/wrap utilities, see Flex_layout
    module. *)

module Css = Cascade.Css

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
    | Flex_arbitrary of string * [ `Flex of Css.flex | `Raw of string ]
    (* flex-[123] *)
    (* Grow *)
    | Flex_grow
    | Flex_grow_0
    | Flex_grow_n of int (* grow-N where N is any integer *)
    | Flex_grow_arbitrary of string * [ `Number of float | `Raw of string ]
      (* grow-[123] *)
    | Flex_grow_legacy (* flex-grow (deprecated alias, keeps its class name) *)
    | Flex_grow_0_legacy (* flex-grow-0 *)
    (* Shrink *)
    | Flex_shrink
    | Flex_shrink_0
    | Flex_shrink_arbitrary of string * [ `Number of float | `Raw of string ]
      (* shrink-[123] *)
    | Flex_shrink_legacy (* flex-shrink *)
    | Flex_shrink_0_legacy (* flex-shrink-0 *)
    (* Basis *)
    | Basis_0
    | Basis_1
    | Basis_spacing of int
    | Basis_auto
    | Basis_full
    | Basis_fraction of int * int
    | Basis_named of string
    | Basis_arbitrary of string * Css.flex_basis (* basis-[123px] *)
    (* Order *)
    | Order of int
    | Neg_order of int (* -order-4 = calc(4 * -1) *)
    (* The author's bracket text travels with the order it denotes, so the class
       name is spelled exactly as it was written. *)
    | Neg_order_arbitrary of string * Css.order (* -order-[var(--value)] *)
    | Order_arbitrary of string * Css.order (* order-[123] *)
    | Order_first
    | Order_last
    | Order_none

  let name = "flex_props"

  (* flex/grow/shrink/basis are the flexbox family (priority 7 - after sizing,
     before transforms). order- occupies an early canonical slot (rank 13, right
     after z-index), so it returns priority 0 with a suborder above z-index's
     ~20M band and below container (priority 1). *)
  let priority = function
    | Order _ | Neg_order _ | Neg_order_arbitrary _ | Order_arbitrary _
    | Order_first | Order_last | Order_none ->
        0
    | _ -> 7

  (* Flex shortcuts *)
  let flex_1 = style [ flex (Grow (Number 1.0)) ]
  let flex_auto = style [ flex Auto ]
  let flex_initial = style [ flex (Full (Number 0., Number 1., Auto)) ]
  let flex_none = style [ flex None ]

  (* flex-N: flex: N *)
  let flex_n_style n = style [ flex (Grow (Number (float_of_int n))) ]

  (* flex-N/M: flex: (N/M * 100)%, folded the way Tailwind folds it. *)
  let flex_fraction_style n m =
    style
      [
        flex
          (Basis (Pct (Option.value ~default:0. (Parse.fraction_percent n m))));
      ]

  (* A bracket no property grammar reads is still a value Tailwind writes out,
     so it reaches the sheet as the token stream it is. *)
  let opaque property raw =
    style (Option.to_list (Parse.opaque_declaration property raw))

  (* Grow *)
  let flex_grow_utility = style [ flex_grow 1.0 ]
  let flex_grow_0_utility = style [ flex_grow 0.0 ]

  (* Shrink *)
  let flex_shrink_utility = style [ flex_shrink 1.0 ]
  let flex_shrink_0_utility = style [ flex_shrink 0.0 ]

  (* Basis. Tailwind v4.3 emits [var(--spacing)] for [basis-1] and
     [calc(var(--spacing) * <n>)] otherwise; [basis-full] / [basis-1/1] emit
     literal [100%]. *)
  let basis_spacing n =
    let spacing_decl, _ = Var.binding Theme.spacing_var Theme.spacing_base in
    let spacing = Var.name Theme.spacing_var in
    let value : Css.flex_basis =
      if n = 1 then Var (Var.theme_ref spacing)
      else Calc Css.Calc.(mul (var spacing) (float (float_of_int n)))
    in
    style [ spacing_decl; flex_basis value ]

  let basis_0 = basis_spacing 0
  let basis_1 = basis_spacing 1
  let basis_auto = style [ flex_basis Auto ]
  let basis_full = style [ flex_basis (Pct 100.0) ]

  let basis_fraction_style n m =
    style
      [
        flex_basis (Pct (Option.value ~default:0. (Parse.fraction_percent n m)));
      ]

  (* [basis] lists --flex-basis, then --spacing, then --container, so a named
     size reads the first of those the theme defines. *)
  let basis_named_style ?theme name =
    let var_name = "container-" ^ name in
    let overridden namespace =
      let token = namespace ^ "-" ^ name in
      Option.map
        (fun value_str ->
          let decl =
            Css.custom_property ~layer:"theme" ("--" ^ token) value_str
          in
          let ref : Css.flex_basis Css.var =
            Var.theme_ref token
              ~default:(Css.Zero : Css.flex_basis)
              ~default_css:"0px"
          in
          style [ decl; flex_basis (Var ref) ])
        (Scheme.theme_value theme token)
    in
    match List.find_map overridden [ "flex-basis"; "spacing"; "container" ] with
    | Some styled -> styled
    (* Without an override the container scale still declares its own default,
       so the token the utility reads is in the sheet. *)
    | None -> (
        match Sizing.container_binding name with
        | Some (v, d) ->
            let decl, _ = Var.binding v d in
            let ref : Css.flex_basis Css.var =
              Var.theme_ref var_name
                ~default:(Css.Zero : Css.flex_basis)
                ~default_css:"0px"
            in
            style [ decl; flex_basis (Var ref) ]
        | None ->
            let ref : Css.flex_basis Css.var =
              Var.theme_ref var_name
                ~default:(Css.Zero : Css.flex_basis)
                ~default_css:"0px"
            in
            style [ flex_basis (Var ref) ])

  (* Order *)
  let order_style n = style [ order (Int n) ]

  let themed_order ?theme name default =
    match Scheme.theme_value theme name with
    | None -> style [ order (Int default) ]
    | Some value ->
        let decl = Css.custom_property ~layer:"theme" ("--" ^ name) value in
        let var_ref = Css.var_ref ~layer:"theme" name in
        style [ decl; order (Var var_ref) ]

  let order_first ?theme () = themed_order ?theme "order-first" (-9999)
  let order_last ?theme () = themed_order ?theme "order-last" 9999
  let order_none = style [ order (Int 0) ]

  let to_style theme =
    let basis_named_style name = basis_named_style ~theme name in
    let order_first () = order_first ~theme () in
    let order_last () = order_last ~theme () in
    function
    | Flex_1 -> flex_1
    | Flex_auto -> flex_auto
    | Flex_initial -> flex_initial
    | Flex_none -> flex_none
    | Flex_n n -> flex_n_style n
    | Flex_fraction (n, m) -> flex_fraction_style n m
    | Flex_arbitrary (_, `Flex v) -> style [ flex v ]
    | Flex_arbitrary (_, `Raw raw) -> opaque "flex" raw
    | Flex_grow -> flex_grow_utility
    | Flex_grow_0 -> flex_grow_0_utility
    | Flex_grow_n n -> style [ flex_grow (float_of_int n) ]
    | Flex_grow_arbitrary (_, `Number f) -> style [ flex_grow f ]
    | Flex_grow_arbitrary (_, `Raw raw) -> opaque "flex-grow" raw
    | Flex_grow_legacy -> flex_grow_utility
    | Flex_grow_0_legacy -> flex_grow_0_utility
    | Flex_shrink -> flex_shrink_utility
    | Flex_shrink_0 -> flex_shrink_0_utility
    | Flex_shrink_arbitrary (_, `Number f) -> style [ flex_shrink f ]
    | Flex_shrink_arbitrary (_, `Raw raw) -> opaque "flex-shrink" raw
    | Flex_shrink_legacy -> flex_shrink_utility
    | Flex_shrink_0_legacy -> flex_shrink_0_utility
    | Basis_0 -> basis_0
    | Basis_1 -> basis_1
    | Basis_spacing n -> basis_spacing n
    | Basis_auto -> basis_auto
    | Basis_full -> basis_full
    | Basis_fraction (n, m) -> basis_fraction_style n m
    | Basis_named name -> basis_named_style name
    | Basis_arbitrary (_, len) -> style [ flex_basis len ]
    | Order n -> order_style n
    | Neg_order n -> style [ order (Int (-n)) ]
    | Neg_order_arbitrary (_, o) ->
        style [ order (Calc (Css.Calc.mul (Val o) (Css.Calc.float (-1.)))) ]
    | Order_arbitrary (_, o) -> style [ order o ]
    | Order_first -> order_first ()
    | Order_last -> order_last ()
    | Order_none -> order_none

  let suborder : t -> int = function
    (* Order (priority 0) - after z-index (~20M band in layout.ml), before
       container (priority 1). Ordering: negative first, then positive, then
       first/last/none, then arbitrary. *)
    | Neg_order n -> 21_000_000 + n (* negative comes first *)
    | Neg_order_arbitrary _ -> 21_000_000 + 50
    | Order n -> 21_000_000 + 100 + n
    | Order_arbitrary _ -> 21_000_000 + 150
    | Order_first -> 21_000_000 + 200
    | Order_last -> 21_000_000 + 201
    | Order_none -> 21_000_000 + 202
    (* Tailwind flex order: flex-1 < fractions < numbers < auto < initial <
       none. Note: flex-* utilities come AFTER order-* utilities *)
    | Flex_1 -> 1000
    (* Fractions: sorted by numerator then denominator (1/2 < 1/3 < 1/4 <
       2/3...) *)
    | Flex_fraction (n, m) -> 1020 + (n * 100) + m
    (* flex-N values: after fractions, ordered by value *)
    | Flex_n n -> 5000 + n
    (* Arbitrary flex values - after the numbered ones, ordered by the grow
       factor the bracket denotes, and a bracket denoting none last: Tailwind
       sorts flex-[2] before flex-[10] before flex-[<value>]. *)
    | Flex_arbitrary (_, `Flex (Grow (Number f))) -> 6000 + int_of_float f
    | Flex_arbitrary _ -> 9000
    (* Named shortcuts come last *)
    | Flex_auto -> 10000
    | Flex_initial -> 10001
    | Flex_none -> 10002
    (* Shrink - legacy flex-shrink* sorts before the canonical shrink* *)
    | Flex_shrink_legacy -> 19998
    | Flex_shrink_0_legacy -> 19999
    | Flex_shrink -> 20000
    | Flex_shrink_0 -> 20001
    | Flex_shrink_arbitrary (_, `Number f) -> 20002 + int_of_float f
    | Flex_shrink_arbitrary _ -> 25000
    (* Grow - legacy flex-grow* sorts before the canonical grow* *)
    | Flex_grow_legacy -> 29998
    | Flex_grow_0_legacy -> 29999
    | Flex_grow -> 30000
    | Flex_grow_0 -> 30001
    | Flex_grow_n n -> 30000 + n
    (* Arbitrary grow values sort after every numbered one, the way Tailwind
       lists grow, grow-0, grow-3, grow-7, grow-[2], grow-[<value>]. *)
    | Flex_grow_arbitrary (_, `Number f) -> 35000 + int_of_float f
    | Flex_grow_arbitrary _ -> 39000
    (* Basis: fractions → arbitrary → keywords alphabetical → named *)
    | Basis_fraction (n, m) -> 40000 + (n * 10) + m
    | Basis_arbitrary _ -> 42000
    | Basis_0 -> 43000
    | Basis_1 -> 43001
    | Basis_spacing _ -> 43001
    | Basis_auto -> 43002
    | Basis_full -> 43003
    | Basis_named _ -> 44000

  let err_not_utility = Error (`Msg "Not a flex property utility")

  (* [basis-1/2] and [flex-1/2] fold the fraction to a percentage, which a zero
     denominator has none of. Every other numerator and denominator reads. *)
  let parse_fraction s =
    match Parse.fraction s with
    | Some (n, m) when m > 0 -> Some (n, m)
    | Some _ | None -> None

  (* The container scale, whose digit-led names ([2xl], [3xs])
     [is_named_spacing] rejects. *)
  let is_container_size = function
    | "3xs" | "2xs" | "xs" | "sm" | "md" | "lg" | "xl" | "2xl" | "3xl" | "4xl"
    | "5xl" | "6xl" | "7xl" ->
        true
    | _ -> false

  (* A bracket read with the grammar cascade has for the property, off the
     arbitrary-value pipeline every other family reads: [_] stands for a space,
     [--spacing(n)] expands, and a binary [+] gets the spaces CSS math wants, so
     [calc(1+2)] is a value rather than a parse error. *)
  let arbitrary_typed read inner =
    let cursor =
      Cascade.Cursor.of_string (Parse.decode_arbitrary_value inner)
    in
    match Cascade.Cursor.try_parse_full_err read cursor with
    | Ok v -> Some v
    | Error _ -> None

  (* The order an [order-[...]] bracket denotes. [None] is a bracket the order
     grammar cannot read, and [of_class] refuses the utility rather than leaving
     [to_style] to raise. *)
  let arbitrary_order inner : Css.order option =
    arbitrary_typed Css.Properties.read_order inner

  (* [flex-], [grow-] and [shrink-] take the same two readings [duration-] does:
     a value the property's own grammar accepts keeps its typed form, and
     anything else Tailwind writes out passes through as a declaration-safe
     token stream. OCaml's number reader serves neither - it reads [0x4] as 4,
     so the class named itself after a value nobody wrote, and it refuses
     [calc(1+2)], which is a value once the pipeline has spaced it out. *)
  let arbitrary_raw inner =
    Option.map (fun v -> `Raw v) (Parse.arbitrary_declaration_value inner)

  let arbitrary_flex inner =
    match arbitrary_typed Css.Properties.read_flex inner with
    | Some v -> Some (`Flex v)
    | None -> arbitrary_raw inner

  let arbitrary_factor inner =
    let typed : Css.flex_factor option =
      arbitrary_typed Css.Properties.read_flex_factor inner
    in
    match typed with
    | Some (Number f) -> Some (`Number f)
    | Some _ | None -> arbitrary_raw inner

  let of_class _theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "flex"; "1" ] -> Ok Flex_1
    | [ "flex"; "auto" ] -> Ok Flex_auto
    | [ "flex"; "initial" ] -> Ok Flex_initial
    | [ "flex"; "none" ] -> Ok Flex_none
    | [ "flex"; "grow" ] -> Ok Flex_grow_legacy
    | [ "grow" ] -> Ok Flex_grow
    | [ "flex"; "grow"; "0" ] -> Ok Flex_grow_0_legacy
    | [ "grow"; "0" ] -> Ok Flex_grow_0
    | [ "grow"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match arbitrary_factor inner with
        | Some v -> Ok (Flex_grow_arbitrary (inner, v))
        | None -> err_not_utility)
    | [ "grow"; n ] -> (
        match Parse.decimal_int n with
        | Some i when i > 0 -> Ok (Flex_grow_n i)
        | _ -> err_not_utility)
    | [ "flex"; "shrink" ] -> Ok Flex_shrink_legacy
    | [ "shrink" ] -> Ok Flex_shrink
    | [ "flex"; "shrink"; "0" ] -> Ok Flex_shrink_0_legacy
    | [ "shrink"; "0" ] -> Ok Flex_shrink_0
    | [ "shrink"; n ] when Parse.is_bracket_value n -> (
        let inner = Parse.bracket_inner n in
        match arbitrary_factor inner with
        | Some v -> Ok (Flex_shrink_arbitrary (inner, v))
        | None -> err_not_utility)
    | [ "basis"; "0" ] -> Ok Basis_0
    | [ "basis"; "1" ] -> Ok Basis_1
    | [ "basis"; "auto" ] -> Ok Basis_auto
    | [ "basis"; "full" ] -> Ok Basis_full
    | [ "basis"; value ] when Parse.is_bracket_value value ->
        let inner = Parse.bracket_inner value in
        let cursor =
          Cascade.Cursor.of_string (Parse.decode_arbitrary_value inner)
        in
        (match
           let value = Css.Properties.read_flex_basis cursor in
           Cascade.Cursor.ws cursor;
           Cascade.Cursor.expect_eof cursor;
           Some value
         with
          | value -> value
          | exception Cascade.Cursor.Parse_error _ -> None)
        |> Option.fold ~none:err_not_utility ~some:(fun value ->
            Ok (Basis_arbitrary (inner, value)))
    | [ "basis"; value ] -> (
        match Parse.decimal_int value with
        | Some n when n >= 0 -> Ok (Basis_spacing n)
        | _ -> (
            match parse_fraction value with
            | Some (n, m) -> Ok (Basis_fraction (n, m))
            | None ->
                (* A container-scale name may lead with a digit ([2xl], [3xs]),
                   which [is_named_spacing] rejects; [basis] emits
                   [var(--container-<name>)] for the whole scale. *)
                if Spacing.is_named_spacing value || is_container_size value
                then Ok (Basis_named value)
                else err_not_utility))
    | [ "order"; "first" ] -> Ok Order_first
    | [ "order"; "last" ] -> Ok Order_last
    | [ "order"; "none" ] -> Ok Order_none
    | "order" :: rest when rest <> [] -> (
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then
          let inner = Parse.bracket_inner value in
          match arbitrary_order inner with
          | Some o -> Ok (Order_arbitrary (inner, o))
          | None -> err_not_utility
        else
          match Parse.decimal_int value with
          | Some n when n >= 0 -> Ok (Order n)
          | _ -> err_not_utility)
    | "" :: "order" :: rest when rest <> [] -> (
        (* Negative order: -order-4, -order-[var(--value)] *)
        let value = String.concat "-" rest in
        if Parse.is_bracket_value value then
          let inner = Parse.bracket_inner value in
          match arbitrary_order inner with
          | Some o -> Ok (Neg_order_arbitrary (inner, o))
          | None -> err_not_utility
        else
          match Parse.decimal_int value with
          | Some n when n >= 1 -> Ok (Neg_order n)
          | _ -> err_not_utility)
    | [ "flex"; value ] when Parse.is_bracket_value value -> (
        (* Arbitrary flex: flex-[123] *)
        let inner = Parse.bracket_inner value in
        match arbitrary_flex inner with
        | Some v -> Ok (Flex_arbitrary (inner, v))
        | None -> err_not_utility)
    | [ "flex"; value ] -> (
        (* Try fraction first (e.g., "1/2") *)
        match parse_fraction value with
        | Some (n, m) -> Ok (Flex_fraction (n, m))
        | None -> (
            (* Try numeric value (e.g., "99") *)
            match Parse.decimal_int value with
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
    | Flex_arbitrary (raw, _) -> "flex-[" ^ raw ^ "]"
    (* Grow - Tailwind v4 uses shorter names; the flex-* spellings are kept as
       deprecated aliases that preserve their class name *)
    | Flex_grow -> "grow"
    | Flex_grow_0 -> "grow-0"
    | Flex_grow_n n -> "grow-" ^ string_of_int n
    | Flex_grow_arbitrary (raw, _) -> "grow-[" ^ raw ^ "]"
    | Flex_grow_legacy -> "flex-grow"
    | Flex_grow_0_legacy -> "flex-grow-0"
    (* Shrink - Tailwind v4 uses shorter names *)
    | Flex_shrink -> "shrink"
    | Flex_shrink_0 -> "shrink-0"
    | Flex_shrink_arbitrary (raw, _) -> "shrink-[" ^ raw ^ "]"
    | Flex_shrink_legacy -> "flex-shrink"
    | Flex_shrink_0_legacy -> "flex-shrink-0"
    (* Basis *)
    | Basis_0 -> "basis-0"
    | Basis_1 -> "basis-1"
    | Basis_spacing n -> "basis-" ^ string_of_int n
    | Basis_auto -> "basis-auto"
    | Basis_full -> "basis-full"
    | Basis_fraction (n, m) ->
        "basis-" ^ string_of_int n ^ "/" ^ string_of_int m
    | Basis_named s -> "basis-" ^ s
    | Basis_arbitrary (raw, _) -> "basis-[" ^ raw ^ "]"
    (* Order *)
    | Order n -> "order-" ^ string_of_int n
    | Neg_order n -> "-order-" ^ string_of_int n
    | Neg_order_arbitrary (s, _) -> "-order-[" ^ s ^ "]"
    | Order_arbitrary (s, _) -> "order-[" ^ s ^ "]"
    | Order_first -> "order-first"
    | Order_last -> "order-last"
    | Order_none -> "order-none"

  let examples = [ Flex_1; Flex_grow; Flex_shrink; Basis_0; Order 1 ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register handler with Utility system *)

let utility = Utility_factory.v
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
