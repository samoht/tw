(** Positioning utilities for controlling element placement *)

module Css = Cascade.Css

(** {1 Helper Functions} *)

(* Negate an inset length: simple units flip sign directly; var()/calc() and
   parenthesized expressions wrap in [calc(... * -1)] to match Tailwind. *)
let negate_length (l : Css.length) : Css.length =
  let open Css in
  match l with
  | Px n -> Px (-.n)
  | Rem n -> Rem (-.n)
  | Em n -> Em (-.n)
  | Pct n -> Pct (-.n)
  | Vw n -> Vw (-.n)
  | Vh n -> Vh (-.n)
  | other -> Calc (Calc.mul (Calc.length other) (Calc.float (-1.)))

(* A bare parenthesised group is a calc body Tailwind writes without the [calc]
   function, as in [left-[(var(--a)+var(--b))]]. Wrap it before normalising so
   [+] and [-] read as math operators inside the group. *)
let read_paren_calc inner : Css.length Css.calc option =
  let len = String.length inner in
  if len < 2 || inner.[0] <> '(' || inner.[len - 1] <> ')' then None
  else
    let calc = Parse.normalize_css_math_operators ("calc(" ^ inner ^ ")") in
    match
      Cascade.Cursor.try_parse_full_err
        (Css.Values.read_calc Css.Values.read_length)
        (Cascade.Cursor.of_string calc)
    with
    | Ok c -> Some c
    | Error _ -> None

(* The inside of an arbitrary inset value, as a length: one reader for every
   side and both signs.

   A bare parenthesised group is a length only under [negate]. Tailwind writes
   the negative as [calc((a + b) * -1)], where the group is a calc
   sub-expression; the positive it writes as [top: (a + b)], which is no
   declaration a browser accepts, so there is nothing to agree with. *)
let parse_bracket_length ?(negate = false) s : Css.length option =
  if not (Parse.is_bracket_value s) then None
  else
    let signed l = if negate then negate_length l else l in
    (* A data-type hint chooses the longhand and says nothing about the value.
       An inset side writes one longhand, so every hint lands here and the
       readers below are handed what follows it; the class name keeps the whole
       bracket, which is what the caller stores. *)
    match Parse.value_after_hint (Parse.bracket_inner s) with
    | None -> None
    | Some inner -> (
        if Parse.is_var inner then
          Some (signed (Css.Var (Var.bracket (Parse.extract_var_name inner))))
        else
          match Parse.arbitrary_length inner with
          | Some l -> Some (signed l)
          | None when not negate -> None
          | None -> (
              match read_paren_calc (Parse.decode_arbitrary_value inner) with
              | None -> None
              | Some c ->
                  Some (Css.Calc (Css.Calc.mul c (Css.Calc.float (-1.))))))

(* The theme token a named inset (top-header) reads: Tailwind resolves the name
   against the [--inset-*] namespace and falls back to [--spacing-*]. The value
   is the theme's, so the utility only references the token and the theme layer
   emits the binding; setting one here writes a length the theme never declared
   over whichever namespace the name actually came from. *)
let named_inset_value theme name : Css.length =
  let token =
    if Scheme.theme_value (Some theme) ("inset-" ^ name) <> None then
      "inset-" ^ name
    else "spacing-" ^ name
  in
  Css.Var (Var.theme_ref token)

(* Create a spacing value using calc(var(--spacing) * n). Returns the theme
   declaration and a length that references the variable. *)
let spacing_value ?theme n : Css.declaration * Css.length =
  Theme.spacing_calc ?theme n

(* The inset sides that take the spacing scale, so a fractional step (top-2.5)
   or the px step (left-px) can share one constructor. *)
module Side = struct
  type t =
    | Top
    | Right
    | Bottom
    | Left
    | Inset
    | Inset_x
    | Inset_y
    | Inset_s
    | Inset_e
    | Inset_bs
    | Inset_be
    | Start
    | End

  let name = function
    | Top -> "top"
    | Right -> "right"
    | Bottom -> "bottom"
    | Left -> "left"
    | Inset -> "inset"
    | Inset_x -> "inset-x"
    | Inset_y -> "inset-y"
    | Inset_s -> "inset-s"
    | Inset_e -> "inset-e"
    | Inset_bs -> "inset-bs"
    | Inset_be -> "inset-be"
    | Start -> "start"
    | End -> "end"

  (* What the side writes. [start] and [inset-s] name one property under two
     spellings, as [end] and [inset-e] do. *)
  let declarations side (len : Css.length) =
    match side with
    | Top -> [ Css.top len ]
    | Right -> [ Css.right len ]
    | Bottom -> [ Css.bottom len ]
    | Left -> [ Css.left len ]
    | Inset -> [ Css.inset [ len ] ]
    | Inset_x -> [ Css.inset_inline [ len ] ]
    | Inset_y -> [ Css.inset_block [ len ] ]
    | Start | Inset_s -> [ Css.inset_inline_start len ]
    | End | Inset_e -> [ Css.inset_inline_end len ]
    | Inset_bs -> [ Css.inset_block_start len ]
    | Inset_be -> [ Css.inset_block_end len ]

  (* Tailwind folds the zero and unit multipliers of the spacing scale to [0px]
     and [var(--spacing)], but its [start]/[end] handler resolves the step
     itself and keeps [calc(var(--spacing) * n)] for every step. *)
  let folds_spacing_scale = function Start | End -> false | _ -> true
end

(* A spacing step is a non-negative multiple of 0.25 ([isValidSpacingMultiplier]
   upstream), so [top-1.25] is a utility and [top-1.7] is not. *)
let is_quarter_multiple f = f >= 0. && Float.rem f 0.25 = 0.

(* [top-2.5] / [right-0.5] / [left-px]: a spacing token that is not a plain
   integer (those keep their existing [Top of int] path). *)
let parse_pos_spacing s : Style.spacing option =
  if s = "px" then Some `Px
  else
    match Parse.decimal_float s with
    | Some f when is_quarter_multiple f && not (Float.is_integer f) ->
        Some (`Rem (f *. 0.25))
    | _ -> None

(* Resolve a spacing token to (optional --spacing binding, length), mirroring
   the padding family so the px and fractional steps render identically. *)
let len_of_pos_spacing ?theme side (s : Style.spacing) :
    Css.declaration option * Css.length =
  match s with
  | `Rem f ->
      let step = f /. 0.25 in
      let decl, len =
        if Side.folds_spacing_scale side then
          Theme.spacing_calc_float ?theme step
        else Theme.spacing_product ?theme step
      in
      (Some decl, len)
  | `Px -> (None, Css.Px 1.)
  | `Full -> (None, Css.Pct 100.)
  | `Named name -> (None, Css.Var (Var.theme_ref ("spacing-" ^ name)))

let pos_spacing_style ?theme side (s : Style.spacing) =
  let decl_opt, len = len_of_pos_spacing ?theme side s in
  Style.style (Option.to_list decl_opt @ Side.declarations side len)

(* The same step, negated: [-inset-x-0.5]. The px step flips its sign directly,
   the scale steps through the negated multiplier so the calc reads
   [calc(var(--spacing) * -.5)] as Tailwind writes it. *)
let neg_pos_spacing_style ?theme side (s : Style.spacing) =
  let negated : Style.spacing =
    match s with `Rem f -> `Rem (-.f) | other -> other
  in
  let decl_opt, len = len_of_pos_spacing ?theme side negated in
  let len = match s with `Px -> negate_length len | _ -> len in
  Style.style (Option.to_list decl_opt @ Side.declarations side len)

(* A position fraction [n/m] resolves to [n/m * 100%], the same reading the
   sizing families give it: any numerator over any positive denominator, and an
   improper fraction (6/5 -> 120%) is a position like any other. *)
let frac_valid frac = Parse.fraction_pct frac <> None
let frac_pct frac = Option.value ~default:0. (Parse.fraction_pct frac)

module Handler = struct
  open Style
  open Css

  (** Local position utility type *)
  type t =
    | Position_static
    | Position_relative
    | Position_absolute
    | Position_fixed
    | Position_sticky
    | Inset_0
    | Inset_x_0
    | Inset_y_0
    | Inset_auto
    | Inset_full
    | Neg_inset_full
    | Inset_x_auto
    | Inset_x_full
    | Neg_inset_x_full
    | Inset_y_auto
    | Inset_y_full
    | Neg_inset_y_full
    | Pos_spacing of Side.t * Style.spacing
    | Neg_pos_spacing of Side.t * Style.spacing
      (* fractional or px spacing step on any inset side *)
    | Pos_fraction of Side.t * string
    | Neg_pos_fraction of Side.t * string
      (* raw fraction suffix kept for the class name *)
    | Pos_arbitrary of Side.t * string * Css.length
      (* raw bracket suffix kept for the class name, value already signed *)
    | Neg_pos_arbitrary of Side.t * string * Css.length
    | Pos_named of Side.t * string
    | Neg_pos_named of Side.t * string
      (* theme token reference like inset-shadowned *)
    | Inset of int
    | Inset_x of int
    | Inset_y of int
    (* Logical position utilities: inset-s, inset-e, inset-bs, inset-be *)
    | Inset_s of int
    | Inset_s_auto
    | Inset_s_full
    | Neg_inset_s_full
    | Inset_e of int
    | Inset_e_auto
    | Inset_e_full
    | Neg_inset_e_full
    | Inset_bs of int
    | Inset_bs_auto
    | Inset_bs_full
    | Neg_inset_bs_full
    | Inset_be of int
    | Inset_be_auto
    | Inset_be_full
    | Neg_inset_be_full
    | Top of int
    | Top_auto
    | Top_full
    | Neg_top_full
    | Right of int
    | Right_auto
    | Right_full
    | Neg_right_full
    | Bottom of int
    | Bottom_auto
    | Bottom_full
    | Neg_bottom_full
    | Left of int
    | Left_auto
    | Left_full
    | Neg_left_full
    | Start_auto
    | Start_full
    | Neg_start_full
    | End_auto
    | End_full
    | Neg_end_full

  (** Extensible variant for position utilities *)

  let name = "position"

  (** Priority for position utilities *)
  let priority _ = 0

  (** {1 Utility Conversion Functions} *)

  let to_style theme =
    let spacing_value n = spacing_value ~theme n in
    function
    | Position_static -> style [ position Static ]
    | Position_relative -> style [ position Relative ]
    | Position_absolute -> style [ position Absolute ]
    | Position_fixed -> style [ position Fixed ]
    | Position_sticky -> style [ position Sticky ]
    | Inset_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.inset [ zero_value ] ])
    | Inset_x_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.inset_inline [ zero_value ] ])
    | Inset_y_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.inset_block [ zero_value ] ])
    | Inset_auto -> style [ Css.inset [ Auto ] ]
    | Inset_full -> style [ Css.inset [ Pct 100.0 ] ]
    | Neg_inset_full -> style [ Css.inset [ Pct (-100.0) ] ]
    | Inset_x_auto -> style [ Css.inset_inline [ Auto ] ]
    | Inset_x_full -> style [ Css.inset_inline [ Pct 100.0 ] ]
    | Neg_inset_x_full -> style [ Css.inset_inline [ Pct (-100.0) ] ]
    | Inset_y_auto -> style [ Css.inset_block [ Auto ] ]
    | Inset_y_full -> style [ Css.inset_block [ Pct 100.0 ] ]
    | Neg_inset_y_full -> style [ Css.inset_block [ Pct (-100.0) ] ]
    | Pos_spacing (side, sp) -> pos_spacing_style ~theme side sp
    | Neg_pos_spacing (side, sp) -> neg_pos_spacing_style ~theme side sp
    | Pos_fraction (side, f) ->
        style (Side.declarations side (Pct (frac_pct f)))
    | Neg_pos_fraction (side, f) ->
        style (Side.declarations side (Pct (-.frac_pct f)))
    | Pos_arbitrary (side, _, len) | Neg_pos_arbitrary (side, _, len) ->
        style (Side.declarations side len)
    | Pos_named (side, name) ->
        style (Side.declarations side (named_inset_value theme name))
    | Neg_pos_named (side, name) ->
        style
          (Side.declarations side
             (negate_length (named_inset_value theme name)))
    | Inset n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset [ value ] ])
    | Inset_x n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline [ value ] ])
    | Inset_y n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block [ value ] ])
    (* inset-s = inset-inline-start *)
    | Inset_s n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_start value ])
    | Inset_s_auto -> style [ Css.inset_inline_start Auto ]
    | Inset_s_full -> style [ Css.inset_inline_start (Pct 100.0) ]
    | Neg_inset_s_full -> style [ Css.inset_inline_start (Pct (-100.0)) ]
    (* inset-e = inset-inline-end *)
    | Inset_e n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_end value ])
    | Inset_e_auto -> style [ Css.inset_inline_end Auto ]
    | Inset_e_full -> style [ Css.inset_inline_end (Pct 100.0) ]
    | Neg_inset_e_full -> style [ Css.inset_inline_end (Pct (-100.0)) ]
    (* inset-bs = inset-block-start *)
    | Inset_bs n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block_start value ])
    | Inset_bs_auto -> style [ Css.inset_block_start Auto ]
    | Inset_bs_full -> style [ Css.inset_block_start (Pct 100.0) ]
    | Neg_inset_bs_full -> style [ Css.inset_block_start (Pct (-100.0)) ]
    (* inset-be = inset-block-end *)
    | Inset_be n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block_end value ])
    | Inset_be_auto -> style [ Css.inset_block_end Auto ]
    | Inset_be_full -> style [ Css.inset_block_end (Pct 100.0) ]
    | Neg_inset_be_full -> style [ Css.inset_block_end (Pct (-100.0)) ]
    | Top n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.top value ])
    | Top_auto -> style [ Css.top Auto ]
    | Top_full -> style [ Css.top (Pct 100.0) ]
    | Neg_top_full -> style [ Css.top (Pct (-100.0)) ]
    | Right n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.right value ])
    | Right_auto -> style [ Css.right Auto ]
    | Right_full -> style [ Css.right (Pct 100.0) ]
    | Neg_right_full -> style [ Css.right (Pct (-100.0)) ]
    | Bottom n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.bottom value ])
    | Bottom_auto -> style [ Css.bottom Auto ]
    | Bottom_full -> style [ Css.bottom (Pct 100.0) ]
    | Neg_bottom_full -> style [ Css.bottom (Pct (-100.0)) ]
    | Left n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.left value ])
    | Left_auto -> style [ Css.left Auto ]
    | Left_full -> style [ Css.left (Pct 100.0) ]
    | Neg_left_full -> style [ Css.left (Pct (-100.0)) ]
    | Start_auto -> style [ Css.inset_inline_start Auto ]
    | Start_full -> style [ Css.inset_inline_start (Pct 100.0) ]
    | Neg_start_full -> style [ Css.inset_inline_start (Pct (-100.0)) ]
    | End_auto -> style [ Css.inset_inline_end Auto ]
    | End_full -> style [ Css.inset_inline_end (Pct 100.0) ]
    | Neg_end_full -> style [ Css.inset_inline_end (Pct (-100.0)) ]

  let int_of_string_with_sign = Parse.int_any

  (* Tailwind assigns one order slot to every candidate that writes the same
     position property, so [start] and [inset-s] share theirs. The shared
     candidate-name tiebreak then handles negatives, arbitrary values,
     fractions, keywords and scale values. *)
  let side_slot = function
    | Side.Inset -> 1_000_000
    | Side.Inset_x -> 2_000_000
    | Side.Inset_y -> 3_000_000
    | Side.Inset_s | Side.Start -> 4_000_000
    | Side.Inset_e | Side.End -> 5_000_000
    | Side.Inset_bs -> 6_000_000
    | Side.Inset_be -> 7_000_000
    | Side.Top -> 8_000_000
    | Side.Right -> 9_000_000
    | Side.Bottom -> 10_000_000
    | Side.Left -> 11_000_000

  let suborder = function
    | Position_absolute -> 0
    | Position_fixed -> 1
    | Position_relative -> 2
    | Position_static -> 3
    | Position_sticky -> 4
    | Pos_spacing (side, _)
    | Neg_pos_spacing (side, _)
    | Pos_fraction (side, _)
    | Neg_pos_fraction (side, _)
    | Pos_arbitrary (side, _, _)
    | Neg_pos_arbitrary (side, _, _)
    | Pos_named (side, _)
    | Neg_pos_named (side, _) ->
        side_slot side
    | Inset_0 | Inset_auto | Inset_full | Neg_inset_full | Inset _ ->
        side_slot Side.Inset
    | Inset_x_0 | Inset_x_auto | Inset_x_full | Neg_inset_x_full | Inset_x _ ->
        side_slot Side.Inset_x
    | Inset_y_0 | Inset_y_auto | Inset_y_full | Neg_inset_y_full | Inset_y _ ->
        side_slot Side.Inset_y
    | Inset_s _ | Inset_s_auto | Inset_s_full | Neg_inset_s_full | Start_auto
    | Start_full | Neg_start_full ->
        side_slot Side.Inset_s
    | Inset_e _ | Inset_e_auto | Inset_e_full | Neg_inset_e_full | End_auto
    | End_full | Neg_end_full ->
        side_slot Side.Inset_e
    | Inset_bs _ | Inset_bs_auto | Inset_bs_full | Neg_inset_bs_full ->
        side_slot Side.Inset_bs
    | Inset_be _ | Inset_be_auto | Inset_be_full | Neg_inset_be_full ->
        side_slot Side.Inset_be
    | Top _ | Top_auto | Top_full | Neg_top_full -> side_slot Side.Top
    | Right _ | Right_auto | Right_full | Neg_right_full -> side_slot Side.Right
    | Bottom _ | Bottom_auto | Bottom_full | Neg_bottom_full ->
        side_slot Side.Bottom
    | Left _ | Left_auto | Left_full | Neg_left_full -> side_slot Side.Left

  (* A named inset (top-header) is valid only when the theme defines the token.
     Tailwind resolves the name against the [--inset-*] namespace, then falls
     back to [--spacing-*]; without this gate a stray source token like
     [top-level] would parse as a utility and emit a bogus value. *)
  let is_named_inset theme n =
    Scheme.theme_value (Some theme) ("inset-" ^ n) <> None
    || Scheme.theme_value (Some theme) ("spacing-" ^ n) <> None

  let of_class theme class_name =
    (* Every numeric inset step reads the spacing scale, so both readers below
       answer for the whole family once the theme removes it. *)
    let int_of_string_with_sign n =
      match int_of_string_with_sign n with
      | Ok x when Theme.has_spacing_step ~theme (Float.abs (float_of_int x)) ->
          Ok x
      | Ok _ -> Error (`Msg "the spacing scale has no such step")
      | Error _ as error -> error
    in
    let parse_pos_spacing n : Style.spacing option =
      match parse_pos_spacing n with
      | Some (`Rem f) when not (Theme.has_spacing_step ~theme (f /. 0.25)) ->
          None
      | parsed -> parsed
    in
    (* [start-4] and [start-0.5] share one constructor: the logical inline sides
       have no separate integer step, so their reader spans the scale. *)
    let parse_scale_step n : Style.spacing option =
      match int_of_string_with_sign n with
      | Ok x when x >= 0 -> Some (`Rem (float_of_int x *. 0.25))
      | Ok _ -> None
      | Error _ -> parse_pos_spacing n
    in
    (* The tail every inset side shares once its own scale reader has declined:
       an arbitrary bracket, then a name the theme binds. Both signs read the
       same tail; the negative writes the name out as [calc(var(--x) * -1)]. *)
    let arbitrary_or_named side n =
      match parse_bracket_length n with
      | Some len -> Ok (Pos_arbitrary (side, n, len))
      | None when Parse.is_valid_theme_name n && is_named_inset theme n ->
          Ok (Pos_named (side, n))
      | None -> Error (`Msg "invalid")
    in
    let neg_arbitrary_or_named side n =
      match parse_bracket_length ~negate:true n with
      | Some len -> Ok (Neg_pos_arbitrary (side, n, len))
      | None when Parse.is_valid_theme_name n && is_named_inset theme n ->
          Ok (Neg_pos_named (side, n))
      | None -> Error (`Msg "invalid")
    in
    (* A fraction, then the side's own scale, then the shared tail. Every inset
       side reads all three, so the suffix vocabulary is one function of the
       reader the side uses for the scale. *)
    let value ~neg scale side n =
      if frac_valid n then
        Ok (if neg then Neg_pos_fraction (side, n) else Pos_fraction (side, n))
      else
        match scale n with
        | Some sp ->
            Ok
              (if neg then Neg_pos_spacing (side, sp) else Pos_spacing (side, sp))
        | None when neg -> neg_arbitrary_or_named side n
        | None -> arbitrary_or_named side n
    in
    let scale_or_arbitrary side n = value ~neg:false parse_pos_spacing side n in
    let neg_scale_or_arbitrary side n =
      value ~neg:true parse_pos_spacing side n
    in
    let step_or_arbitrary side n = value ~neg:false parse_scale_step side n in
    let neg_step_or_arbitrary side n =
      value ~neg:true parse_scale_step side n
    in
    let parts = Parse.split_class class_name in
    match parts with
    | [ "static" ] -> Ok Position_static
    | [ "relative" ] -> Ok Position_relative
    | [ "absolute" ] -> Ok Position_absolute
    | [ "fixed" ] -> Ok Position_fixed
    | [ "sticky" ] -> Ok Position_sticky
    | [ "inset"; "0" ] -> Ok Inset_0
    | [ "inset"; "x"; "0" ] -> Ok Inset_x_0
    | [ "inset"; "y"; "0" ] -> Ok Inset_y_0
    | [ "inset"; "auto" ] -> Ok Inset_auto
    | [ "inset"; "full" ] -> Ok Inset_full
    | [ ""; "inset"; "full" ] -> Ok Neg_inset_full
    | [ "inset"; "x"; "auto" ] -> Ok Inset_x_auto
    | [ "inset"; "x"; "full" ] -> Ok Inset_x_full
    | [ ""; "inset"; "x"; "full" ] -> Ok Neg_inset_x_full
    | [ "inset"; "y"; "auto" ] -> Ok Inset_y_auto
    | [ "inset"; "y"; "full" ] -> Ok Inset_y_full
    | [ ""; "inset"; "y"; "full" ] -> Ok Neg_inset_y_full
    | [ "inset"; "x"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_x x)
        | Error _ -> scale_or_arbitrary Side.Inset_x n)
    | [ ""; "inset"; "x"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_x (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_x n)
    | [ "inset"; "y"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_y x)
        | Error _ -> scale_or_arbitrary Side.Inset_y n)
    | [ ""; "inset"; "y"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_y (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_y n)
    (* inset-s = inset-inline-start *)
    | [ "inset"; "s"; "auto" ] -> Ok Inset_s_auto
    | [ "inset"; "s"; "full" ] -> Ok Inset_s_full
    | [ ""; "inset"; "s"; "full" ] -> Ok Neg_inset_s_full
    | [ "inset"; "s"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_s x)
        | Error _ -> scale_or_arbitrary Side.Inset_s n)
    | [ ""; "inset"; "s"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_s (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_s n)
    (* inset-e = inset-inline-end *)
    | [ "inset"; "e"; "auto" ] -> Ok Inset_e_auto
    | [ "inset"; "e"; "full" ] -> Ok Inset_e_full
    | [ ""; "inset"; "e"; "full" ] -> Ok Neg_inset_e_full
    | [ "inset"; "e"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_e x)
        | Error _ -> scale_or_arbitrary Side.Inset_e n)
    | [ ""; "inset"; "e"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_e (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_e n)
    (* inset-bs = inset-block-start *)
    | [ "inset"; "bs"; "auto" ] -> Ok Inset_bs_auto
    | [ "inset"; "bs"; "full" ] -> Ok Inset_bs_full
    | [ ""; "inset"; "bs"; "full" ] -> Ok Neg_inset_bs_full
    | [ "inset"; "bs"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_bs x)
        | Error _ -> scale_or_arbitrary Side.Inset_bs n)
    | [ ""; "inset"; "bs"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_bs (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_bs n)
    (* inset-be = inset-block-end *)
    | [ "inset"; "be"; "auto" ] -> Ok Inset_be_auto
    | [ "inset"; "be"; "full" ] -> Ok Inset_be_full
    | [ ""; "inset"; "be"; "full" ] -> Ok Neg_inset_be_full
    | [ "inset"; "be"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_be x)
        | Error _ -> scale_or_arbitrary Side.Inset_be n)
    | [ ""; "inset"; "be"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_be (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset_be n)
    | [ "inset"; n ]
      when n <> "shadow" && n <> "ring" && n <> "x" && n <> "y" && n <> "s"
           && n <> "e" && n <> "bs" && n <> "be" -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset x)
        | Error _ -> scale_or_arbitrary Side.Inset n)
    | [ ""; "inset"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Inset n)
    | [ "top"; "auto" ] -> Ok Top_auto
    | [ "top"; "full" ] -> Ok Top_full
    | [ ""; "top"; "full" ] -> Ok Neg_top_full
    | [ "top"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Top x)
        | Error _ -> scale_or_arbitrary Side.Top n)
    | [ ""; "top"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Top (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Top n)
    | [ "right"; "auto" ] -> Ok Right_auto
    | [ "right"; "full" ] -> Ok Right_full
    | [ ""; "right"; "full" ] -> Ok Neg_right_full
    | [ "right"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Right x)
        | Error _ -> scale_or_arbitrary Side.Right n)
    | [ ""; "right"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Right (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Right n)
    | [ "bottom"; "auto" ] -> Ok Bottom_auto
    | [ "bottom"; "full" ] -> Ok Bottom_full
    | [ ""; "bottom"; "full" ] -> Ok Neg_bottom_full
    | [ "bottom"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Bottom x)
        | Error _ -> scale_or_arbitrary Side.Bottom n)
    | [ ""; "bottom"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Bottom (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Bottom n)
    | [ "left"; "auto" ] -> Ok Left_auto
    | [ "left"; "full" ] -> Ok Left_full
    | [ ""; "left"; "full" ] -> Ok Neg_left_full
    | [ "left"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Left x)
        | Error _ -> scale_or_arbitrary Side.Left n)
    | [ ""; "left"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Left (-x))
        | Error _ -> neg_scale_or_arbitrary Side.Left n)
    | [ "start"; "auto" ] -> Ok Start_auto
    | [ "start"; "full" ] -> Ok Start_full
    | [ ""; "start"; "full" ] -> Ok Neg_start_full
    | [ "start"; n ] -> step_or_arbitrary Side.Start n
    | [ ""; "start"; n ] -> neg_step_or_arbitrary Side.Start n
    | [ "end"; "auto" ] -> Ok End_auto
    | [ "end"; "full" ] -> Ok End_full
    | [ ""; "end"; "full" ] -> Ok Neg_end_full
    | [ "end"; n ] -> step_or_arbitrary Side.End n
    | [ ""; "end"; n ] -> neg_step_or_arbitrary Side.End n
    | _ -> Error (`Msg "Not a position utility")

  let to_class = function
    | Position_static -> "static"
    | Position_relative -> "relative"
    | Position_absolute -> "absolute"
    | Position_fixed -> "fixed"
    | Position_sticky -> "sticky"
    | Inset_0 -> "inset-0"
    | Inset_x_0 -> "inset-x-0"
    | Inset_y_0 -> "inset-y-0"
    | Inset_auto -> "inset-auto"
    | Inset_full -> "inset-full"
    | Neg_inset_full -> "-inset-full"
    | Inset_x_auto -> "inset-x-auto"
    | Inset_x_full -> "inset-x-full"
    | Neg_inset_x_full -> "-inset-x-full"
    | Inset_y_auto -> "inset-y-auto"
    | Inset_y_full -> "inset-y-full"
    | Neg_inset_y_full -> "-inset-y-full"
    | Pos_spacing (side, sp) ->
        Side.name side ^ "-" ^ Spacing.pp_spacing_suffix sp
    | Neg_pos_spacing (side, sp) ->
        "-" ^ Side.name side ^ "-" ^ Spacing.pp_spacing_suffix sp
    | Pos_arbitrary (side, raw, _) -> Side.name side ^ "-" ^ raw
    | Neg_pos_arbitrary (side, raw, _) -> "-" ^ Side.name side ^ "-" ^ raw
    | Pos_fraction (side, f) -> Side.name side ^ "-" ^ f
    | Neg_pos_fraction (side, f) -> "-" ^ Side.name side ^ "-" ^ f
    | Pos_named (side, name) -> Side.name side ^ "-" ^ name
    | Neg_pos_named (side, name) -> "-" ^ Side.name side ^ "-" ^ name
    | Inset n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-" ^ string_of_int (abs n)
    | Inset_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-x-" ^ string_of_int (abs n)
    | Inset_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-y-" ^ string_of_int (abs n)
    (* inset-s = inset-inline-start *)
    | Inset_s n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-s-" ^ string_of_int (abs n)
    | Inset_s_auto -> "inset-s-auto"
    | Inset_s_full -> "inset-s-full"
    | Neg_inset_s_full -> "-inset-s-full"
    (* inset-e = inset-inline-end *)
    | Inset_e n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-e-" ^ string_of_int (abs n)
    | Inset_e_auto -> "inset-e-auto"
    | Inset_e_full -> "inset-e-full"
    | Neg_inset_e_full -> "-inset-e-full"
    (* inset-bs = inset-block-start *)
    | Inset_bs n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-bs-" ^ string_of_int (abs n)
    | Inset_bs_auto -> "inset-bs-auto"
    | Inset_bs_full -> "inset-bs-full"
    | Neg_inset_bs_full -> "-inset-bs-full"
    (* inset-be = inset-block-end *)
    | Inset_be n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-be-" ^ string_of_int (abs n)
    | Inset_be_auto -> "inset-be-auto"
    | Inset_be_full -> "inset-be-full"
    | Neg_inset_be_full -> "-inset-be-full"
    | Top_auto -> "top-auto"
    | Top_full -> "top-full"
    | Neg_top_full -> "-top-full"
    | Top n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "top-" ^ string_of_int (abs n)
    | Right_auto -> "right-auto"
    | Right_full -> "right-full"
    | Neg_right_full -> "-right-full"
    | Right n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "right-" ^ string_of_int (abs n)
    | Bottom_auto -> "bottom-auto"
    | Bottom_full -> "bottom-full"
    | Neg_bottom_full -> "-bottom-full"
    | Bottom n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "bottom-" ^ string_of_int (abs n)
    | Left_auto -> "left-auto"
    | Left_full -> "left-full"
    | Neg_left_full -> "-left-full"
    | Left n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "left-" ^ string_of_int (abs n)
    | Start_auto -> "start-auto"
    | Start_full -> "start-full"
    | Neg_start_full -> "-start-full"
    | End_auto -> "end-auto"
    | End_full -> "end-full"
    | Neg_end_full -> "-end-full"

  let examples =
    [
      Position_static;
      Inset_0;
      Inset_x_0;
      Inset_y_0;
      Top_auto;
      Right_auto;
      Bottom_auto;
      Left_auto;
      Start_auto;
      End_auto;
    ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register handler with Utility system *)

(** Public API combinators *)
let utility = Utility_factory.v

let static = utility Position_static
let relative = utility Position_relative
let absolute = utility Position_absolute
let fixed = utility Position_fixed
let sticky = utility Position_sticky
let inset n = utility (Inset n)
let inset_0 = utility Inset_0
let inset_x n = utility (Inset_x n)
let inset_x_0 = utility Inset_x_0
let inset_y n = utility (Inset_y n)
let inset_y_0 = utility Inset_y_0
let top n = utility (Top n)
let right n = utility (Right n)
let bottom n = utility (Bottom n)
let left n = utility (Left n)
let top_1_2 = utility (Pos_fraction (Side.Top, "1/2"))
let left_1_2 = utility (Pos_fraction (Side.Left, "1/2"))

(* A half-step (or any float) spacing value on a physical/axis inset side:
   [Pos_spacing]/[Neg_pos_spacing] already carry the scale factor as a
   [Style.spacing], the same representation [top-0.5] parses to from a class
   string. *)
let pos_prime side f =
  if f < 0.0 then utility (Neg_pos_spacing (side, `Rem (Float.abs f *. 0.25)))
  else utility (Pos_spacing (side, `Rem (f *. 0.25)))

let inset' f = pos_prime Side.Inset f
let inset_x' f = pos_prime Side.Inset_x f
let inset_y' f = pos_prime Side.Inset_y f
let top' f = pos_prime Side.Top f
let right' f = pos_prime Side.Right f
let bottom' f = pos_prime Side.Bottom f
let left' f = pos_prime Side.Left f
