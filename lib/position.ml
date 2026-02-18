(** Positioning utilities for controlling element placement *)

(** {1 Helper Functions} *)

(* Parse a bracket value like [4px] and return a Css.length *)
let parse_bracket_length s : (Css.length, _) result =
  if String.length s >= 3 && s.[0] = '[' && s.[String.length s - 1] = ']' then (
    let inner = String.sub s 1 (String.length s - 2) in
    let slen = String.length inner in
    let i = ref 0 in
    while
      !i < slen
      && (inner.[!i] = '-'
         || inner.[!i] = '.'
         || (inner.[!i] >= '0' && inner.[!i] <= '9'))
    do
      incr i
    done;
    let num_s = String.sub inner 0 !i in
    let unit_s = String.sub inner !i (slen - !i) in
    match Float.of_string_opt num_s with
    | None -> Error (`Msg ("Invalid number: " ^ num_s))
    | Some n -> (
        let open Css in
        match unit_s with
        | "px" -> Ok (Px n)
        | "rem" -> Ok (Rem n)
        | "em" -> Ok (Em n)
        | "%" -> Ok (Pct n)
        | "vw" -> Ok (Vw n)
        | "vh" -> Ok (Vh n)
        | _ -> Error (`Msg ("Invalid length unit: " ^ unit_s))))
  else Error (`Msg ("Not a bracket value: " ^ s))

(* Memoization cache for inset-named theme variables *)
let inset_named_cache : (string, Css.length Var.theme) Hashtbl.t =
  Hashtbl.create 16

(* Get or create a theme variable for a named inset value like
   --inset-shadowned. Uses order (3, 200) to place in theme layer after numbered
   spacing variables (which are at 3, 100+n). *)
let get_inset_named_var name =
  match Hashtbl.find_opt inset_named_cache name with
  | Some var -> var
  | None ->
      let var_name = "inset-" ^ name in
      (* Order (3, 200) places named inset vars after numbered spacing (3,
         100+n) *)
      let var = Var.theme Css.Length var_name ~order:(3, 200) in
      Hashtbl.add inset_named_cache name var;
      var

(* Create a named inset value using a theme variable like --inset-shadowned.
   Returns the theme declaration and a length that references the variable. *)
let named_inset_value name : Css.declaration * Css.length =
  let var = get_inset_named_var name in
  (* Use 1940px as placeholder value - this comes from Tailwind test config *)
  let concrete_value : Css.length = Px 1940. in
  let decl, ref = Var.binding var concrete_value in
  (decl, Css.Var ref)

(* Create a spacing value using calc(var(--spacing) * n). Returns the theme
   declaration and a length that references the variable. *)
let spacing_value n : Css.declaration * Css.length = Theme.spacing_calc n

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
    | Inset_3_4
    | Inset_x_auto
    | Inset_x_full
    | Neg_inset_x_full
    | Inset_x_3_4
    | Inset_y_auto
    | Inset_y_full
    | Neg_inset_y_full
    | Inset_y_3_4
    | Inset of int
    | Inset_arbitrary of Css.length
    | Inset_named of string (* Custom property reference like inset-shadowned *)
    | Inset_x of int
    | Inset_x_arbitrary of Css.length
    | Inset_x_named of string
    | Inset_y of int
    | Inset_y_arbitrary of Css.length
    | Inset_y_named of string
    (* Logical position utilities: inset-s, inset-e, inset-bs, inset-be *)
    | Inset_s of int
    | Inset_s_arbitrary of Css.length
    | Inset_s_named of string
    | Inset_s_auto
    | Inset_s_full
    | Neg_inset_s_full
    | Inset_s_3_4
    | Inset_e of int
    | Inset_e_arbitrary of Css.length
    | Inset_e_named of string
    | Inset_e_auto
    | Inset_e_full
    | Neg_inset_e_full
    | Inset_e_3_4
    | Inset_bs of int
    | Inset_bs_arbitrary of Css.length
    | Inset_bs_named of string
    | Inset_bs_auto
    | Inset_bs_full
    | Neg_inset_bs_full
    | Inset_bs_3_4
    | Inset_be of int
    | Inset_be_arbitrary of Css.length
    | Inset_be_named of string
    | Inset_be_auto
    | Inset_be_full
    | Neg_inset_be_full
    | Inset_be_3_4
    | Top of int
    | Top_1_2
    | Top_auto
    | Top_full
    | Neg_top_full
    | Top_3_4
    | Top_arbitrary of Css.length
    | Top_named of string
    | Right of int
    | Right_auto
    | Right_full
    | Neg_right_full
    | Right_3_4
    | Right_arbitrary of Css.length
    | Right_named of string
    | Bottom of int
    | Bottom_auto
    | Bottom_full
    | Neg_bottom_full
    | Bottom_3_4
    | Bottom_arbitrary of Css.length
    | Bottom_named of string
    | Left of int
    | Left_1_2
    | Left_auto
    | Left_full
    | Neg_left_full
    | Left_3_4
    | Left_arbitrary of Css.length
    | Left_named of string
    | Start of int
    | Start_auto
    | Start_full
    | Start_3_4
    | End of int
    | End_auto
    | End_full
    | End_3_4
    | Z of int

  (** Extensible variant for position utilities *)
  type Utility.base += Self of t

  let name = "position"

  (** Priority for position utilities *)
  let priority = 0

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Position_static -> style [ position Static ]
    | Position_relative -> style [ position Relative ]
    | Position_absolute -> style [ position Absolute ]
    | Position_fixed -> style [ position Fixed ]
    | Position_sticky -> style [ position Sticky ]
    | Inset_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.inset zero_value ])
    | Inset_x_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.left zero_value; Css.right zero_value ])
    | Inset_y_0 ->
        let decl, zero_value = spacing_value 0 in
        style (decl :: [ Css.bottom zero_value; Css.top zero_value ])
    | Inset_auto -> style [ Css.inset Auto ]
    | Inset_full -> style [ Css.inset (Pct 100.0) ]
    | Neg_inset_full -> style [ Css.inset (Pct (-100.0)) ]
    | Inset_3_4 -> style [ Css.inset (Pct 75.0) ]
    | Inset_x_auto -> style [ Css.inset_inline Auto ]
    | Inset_x_full -> style [ Css.inset_inline (Pct 100.0) ]
    | Neg_inset_x_full -> style [ Css.inset_inline (Pct (-100.0)) ]
    | Inset_x_3_4 -> style [ Css.inset_inline (Pct 75.0) ]
    | Inset_y_auto -> style [ Css.inset_block Auto ]
    | Inset_y_full -> style [ Css.inset_block (Pct 100.0) ]
    | Neg_inset_y_full -> style [ Css.inset_block (Pct (-100.0)) ]
    | Inset_y_3_4 -> style [ Css.inset_block (Pct 75.0) ]
    | Inset n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset value ])
    | Inset_arbitrary len -> style [ Css.inset len ]
    | Inset_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset value ])
    | Inset_x n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline value ])
    | Inset_x_arbitrary len -> style [ Css.inset_inline len ]
    | Inset_x_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_inline value ])
    | Inset_y n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block value ])
    | Inset_y_arbitrary len -> style [ Css.inset_block len ]
    | Inset_y_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_block value ])
    (* inset-s = inset-inline-start *)
    | Inset_s n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_start value ])
    | Inset_s_arbitrary len -> style [ Css.inset_inline_start len ]
    | Inset_s_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_inline_start value ])
    | Inset_s_auto -> style [ Css.inset_inline_start Auto ]
    | Inset_s_full -> style [ Css.inset_inline_start (Pct 100.0) ]
    | Neg_inset_s_full -> style [ Css.inset_inline_start (Pct (-100.0)) ]
    | Inset_s_3_4 -> style [ Css.inset_inline_start (Pct 75.0) ]
    (* inset-e = inset-inline-end *)
    | Inset_e n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_end value ])
    | Inset_e_arbitrary len -> style [ Css.inset_inline_end len ]
    | Inset_e_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_inline_end value ])
    | Inset_e_auto -> style [ Css.inset_inline_end Auto ]
    | Inset_e_full -> style [ Css.inset_inline_end (Pct 100.0) ]
    | Neg_inset_e_full -> style [ Css.inset_inline_end (Pct (-100.0)) ]
    | Inset_e_3_4 -> style [ Css.inset_inline_end (Pct 75.0) ]
    (* inset-bs = inset-block-start *)
    | Inset_bs n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block_start value ])
    | Inset_bs_arbitrary len -> style [ Css.inset_block_start len ]
    | Inset_bs_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_block_start value ])
    | Inset_bs_auto -> style [ Css.inset_block_start Auto ]
    | Inset_bs_full -> style [ Css.inset_block_start (Pct 100.0) ]
    | Neg_inset_bs_full -> style [ Css.inset_block_start (Pct (-100.0)) ]
    | Inset_bs_3_4 -> style [ Css.inset_block_start (Pct 75.0) ]
    (* inset-be = inset-block-end *)
    | Inset_be n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_block_end value ])
    | Inset_be_arbitrary len -> style [ Css.inset_block_end len ]
    | Inset_be_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.inset_block_end value ])
    | Inset_be_auto -> style [ Css.inset_block_end Auto ]
    | Inset_be_full -> style [ Css.inset_block_end (Pct 100.0) ]
    | Neg_inset_be_full -> style [ Css.inset_block_end (Pct (-100.0)) ]
    | Inset_be_3_4 -> style [ Css.inset_block_end (Pct 75.0) ]
    | Top n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.top value ])
    | Top_1_2 -> style [ Css.top (Pct 50.0) ]
    | Top_auto -> style [ Css.top Auto ]
    | Top_full -> style [ Css.top (Pct 100.0) ]
    | Neg_top_full -> style [ Css.top (Pct (-100.0)) ]
    | Top_3_4 -> style [ Css.top (Pct 75.0) ]
    | Top_arbitrary len -> style [ Css.top len ]
    | Top_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.top value ])
    | Right n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.right value ])
    | Right_auto -> style [ Css.right Auto ]
    | Right_full -> style [ Css.right (Pct 100.0) ]
    | Neg_right_full -> style [ Css.right (Pct (-100.0)) ]
    | Right_3_4 -> style [ Css.right (Pct 75.0) ]
    | Right_arbitrary len -> style [ Css.right len ]
    | Right_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.right value ])
    | Bottom n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.bottom value ])
    | Bottom_auto -> style [ Css.bottom Auto ]
    | Bottom_full -> style [ Css.bottom (Pct 100.0) ]
    | Neg_bottom_full -> style [ Css.bottom (Pct (-100.0)) ]
    | Bottom_3_4 -> style [ Css.bottom (Pct 75.0) ]
    | Bottom_arbitrary len -> style [ Css.bottom len ]
    | Bottom_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.bottom value ])
    | Left n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.left value ])
    | Left_1_2 -> style [ Css.left (Pct 50.0) ]
    | Left_auto -> style [ Css.left Auto ]
    | Left_full -> style [ Css.left (Pct 100.0) ]
    | Neg_left_full -> style [ Css.left (Pct (-100.0)) ]
    | Left_3_4 -> style [ Css.left (Pct 75.0) ]
    | Left_arbitrary len -> style [ Css.left len ]
    | Left_named name ->
        let decl, value = named_inset_value name in
        style (decl :: [ Css.left value ])
    | Start n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_start value ])
    | Start_auto -> style [ Css.inset_inline_start Auto ]
    | Start_full -> style [ Css.inset_inline_start (Pct 100.0) ]
    | Start_3_4 -> style [ Css.inset_inline_start (Pct 75.0) ]
    | End n ->
        let decl, value = spacing_value n in
        style (decl :: [ Css.inset_inline_end value ])
    | End_auto -> style [ Css.inset_inline_end Auto ]
    | End_full -> style [ Css.inset_inline_end (Pct 100.0) ]
    | End_3_4 -> style [ Css.inset_inline_end (Pct 75.0) ]
    | Z n -> style [ Css.z_index (Css.Index n) ]

  let int_of_string_with_sign = Parse.int_any

  (* Suborder follows Tailwind's ordering: 1. Negative numeric values (-inset-4)
     - small suborder 2. Negative percentage (-inset-full) 3. Fractions
     (inset-3/4) 4. Positive numeric (inset-4) 5. Arbitrary (inset-[4px]) 6.
     auto (inset-auto) 7. full (inset-full) 8. named (inset-shadowned) *)
  let suborder = function
    | Position_absolute -> 0
    | Position_fixed -> 1
    | Position_relative -> 2
    | Position_static -> 3
    | Position_sticky -> 4
    | Inset_0 -> 100
    | Inset_x_0 -> 101
    | Inset_y_0 -> 102
    | Inset n when n < 0 ->
        50 + abs n (* negative numeric first: -inset-4 = 54 *)
    | Neg_inset_full -> 60 (* negative full after negative numeric *)
    | Inset_3_4 -> 103 (* fractions *)
    | Inset n -> 104 + n (* positive numeric: inset-4 = 108 *)
    | Inset_arbitrary _ -> 120 (* arbitrary after numeric *)
    | Inset_auto -> 125
    | Inset_full -> 130
    | Inset_named _ -> 135
    | Neg_inset_x_full -> 145
    | Inset_x_3_4 -> 150
    | Inset_x n when n < 0 -> 140 + abs n
    | Inset_x n -> 155 + n
    | Inset_x_arbitrary _ -> 160
    | Inset_x_auto -> 165
    | Inset_x_full -> 170
    | Inset_x_named _ -> 175
    | Neg_inset_y_full -> 185
    | Inset_y_3_4 -> 190
    | Inset_y n when n < 0 -> 180 + abs n
    | Inset_y n -> 195 + n
    | Inset_y_arbitrary _ -> 200
    | Inset_y_auto -> 205
    | Inset_y_full -> 210
    | Inset_y_named _ -> 215
    (* inset-s suborders - follows same pattern as inset: negative numeric,
       neg_full, 3/4, positive, arbitrary, auto, full, named *)
    | Inset_s n when n < 0 -> 220 (* all negative numeric together *)
    | Neg_inset_s_full -> 221
    | Inset_s_3_4 -> 222
    | Inset_s n -> 223 + n
    | Inset_s_arbitrary _ -> 240
    | Inset_s_auto -> 241
    | Inset_s_full -> 242
    | Inset_s_named _ -> 243
    (* inset-e suborders *)
    | Inset_e n when n < 0 -> 250
    | Neg_inset_e_full -> 251
    | Inset_e_3_4 -> 252
    | Inset_e n -> 253 + n
    | Inset_e_arbitrary _ -> 270
    | Inset_e_auto -> 271
    | Inset_e_full -> 272
    | Inset_e_named _ -> 273
    (* inset-bs suborders *)
    | Inset_bs n when n < 0 -> 280
    | Neg_inset_bs_full -> 281
    | Inset_bs_3_4 -> 282
    | Inset_bs n -> 283 + n
    | Inset_bs_arbitrary _ -> 300
    | Inset_bs_auto -> 301
    | Inset_bs_full -> 302
    | Inset_bs_named _ -> 303
    (* inset-be suborders *)
    | Inset_be n when n < 0 -> 310
    | Neg_inset_be_full -> 311
    | Inset_be_3_4 -> 312
    | Inset_be n -> 313 + n
    | Inset_be_arbitrary _ -> 330
    | Inset_be_auto -> 331
    | Inset_be_full -> 332
    | Inset_be_named _ -> 333
    (* top suborders - negative, neg_full, 3/4, positive, arbitrary, auto, full,
       named *)
    | Top n when n < 0 -> 400
    | Neg_top_full -> 401
    | Top_1_2 -> 402
    | Top_3_4 -> 403
    | Top n -> 404 + n
    | Top_arbitrary _ -> 420
    | Top_auto -> 421
    | Top_full -> 422
    | Top_named _ -> 423
    (* right suborders *)
    | Right n when n < 0 -> 500
    | Neg_right_full -> 501
    | Right_3_4 -> 502
    | Right n -> 504 + n
    | Right_arbitrary _ -> 520
    | Right_auto -> 521
    | Right_full -> 522
    | Right_named _ -> 523
    (* bottom suborders *)
    | Bottom n when n < 0 -> 600
    | Neg_bottom_full -> 601
    | Bottom_3_4 -> 602
    | Bottom n -> 604 + n
    | Bottom_arbitrary _ -> 620
    | Bottom_auto -> 621
    | Bottom_full -> 622
    | Bottom_named _ -> 623
    (* left suborders *)
    | Left n when n < 0 -> 700
    | Neg_left_full -> 701
    | Left_1_2 -> 702
    | Left_3_4 -> 703
    | Left n -> 704 + n
    | Left_arbitrary _ -> 720
    | Left_auto -> 721
    | Left_full -> 722
    | Left_named _ -> 723
    | Start_3_4 -> 850
    | Start_auto -> 851
    | Start_full -> 852
    | Start n -> 950 + n
    | End_3_4 -> 950
    | End_auto -> 951
    | End_full -> 952
    | End n -> 1000 + n
    | Z n -> 1100 + n

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
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
    | [ "inset"; "3/4" ] -> Ok Inset_3_4
    | [ "inset"; "x"; "auto" ] -> Ok Inset_x_auto
    | [ "inset"; "x"; "full" ] -> Ok Inset_x_full
    | [ ""; "inset"; "x"; "full" ] -> Ok Neg_inset_x_full
    | [ "inset"; "x"; "3/4" ] -> Ok Inset_x_3_4
    | [ "inset"; "y"; "auto" ] -> Ok Inset_y_auto
    | [ "inset"; "y"; "full" ] -> Ok Inset_y_full
    | [ ""; "inset"; "y"; "full" ] -> Ok Neg_inset_y_full
    | [ "inset"; "y"; "3/4" ] -> Ok Inset_y_3_4
    | [ "inset"; "x"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_x x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_x_arbitrary len)
            | Error _ -> Ok (Inset_x_named n)))
    | [ ""; "inset"; "x"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_x (-x))
    | [ "inset"; "y"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_y x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_y_arbitrary len)
            | Error _ -> Ok (Inset_y_named n)))
    | [ ""; "inset"; "y"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_y (-x))
    (* inset-s = inset-inline-start *)
    | [ "inset"; "s"; "auto" ] -> Ok Inset_s_auto
    | [ "inset"; "s"; "full" ] -> Ok Inset_s_full
    | [ ""; "inset"; "s"; "full" ] -> Ok Neg_inset_s_full
    | [ "inset"; "s"; "3/4" ] -> Ok Inset_s_3_4
    | [ "inset"; "s"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_s x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_s_arbitrary len)
            | Error _ -> Ok (Inset_s_named n)))
    | [ ""; "inset"; "s"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_s (-x))
    (* inset-e = inset-inline-end *)
    | [ "inset"; "e"; "auto" ] -> Ok Inset_e_auto
    | [ "inset"; "e"; "full" ] -> Ok Inset_e_full
    | [ ""; "inset"; "e"; "full" ] -> Ok Neg_inset_e_full
    | [ "inset"; "e"; "3/4" ] -> Ok Inset_e_3_4
    | [ "inset"; "e"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_e x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_e_arbitrary len)
            | Error _ -> Ok (Inset_e_named n)))
    | [ ""; "inset"; "e"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_e (-x))
    (* inset-bs = inset-block-start *)
    | [ "inset"; "bs"; "auto" ] -> Ok Inset_bs_auto
    | [ "inset"; "bs"; "full" ] -> Ok Inset_bs_full
    | [ ""; "inset"; "bs"; "full" ] -> Ok Neg_inset_bs_full
    | [ "inset"; "bs"; "3/4" ] -> Ok Inset_bs_3_4
    | [ "inset"; "bs"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_bs x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_bs_arbitrary len)
            | Error _ -> Ok (Inset_bs_named n)))
    | [ ""; "inset"; "bs"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_bs (-x))
    (* inset-be = inset-block-end *)
    | [ "inset"; "be"; "auto" ] -> Ok Inset_be_auto
    | [ "inset"; "be"; "full" ] -> Ok Inset_be_full
    | [ ""; "inset"; "be"; "full" ] -> Ok Neg_inset_be_full
    | [ "inset"; "be"; "3/4" ] -> Ok Inset_be_3_4
    | [ "inset"; "be"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset_be x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_be_arbitrary len)
            | Error _ -> Ok (Inset_be_named n)))
    | [ ""; "inset"; "be"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset_be (-x))
    | [ "inset"; n ] when n <> "shadow" && n <> "ring" -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Inset x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Inset_arbitrary len)
            | Error _ -> Ok (Inset_named n)))
    | [ ""; "inset"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Inset (-x))
    | [ "top"; "1/2" ] -> Ok Top_1_2
    | [ "top"; "3/4" ] -> Ok Top_3_4
    | [ "top"; "auto" ] -> Ok Top_auto
    | [ "top"; "full" ] -> Ok Top_full
    | [ ""; "top"; "full" ] -> Ok Neg_top_full
    | [ "top"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Top x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Top_arbitrary len)
            | Error _ -> Ok (Top_named n)))
    | [ ""; "top"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Top (-x))
    | [ "right"; "3/4" ] -> Ok Right_3_4
    | [ "right"; "auto" ] -> Ok Right_auto
    | [ "right"; "full" ] -> Ok Right_full
    | [ ""; "right"; "full" ] -> Ok Neg_right_full
    | [ "right"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Right x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Right_arbitrary len)
            | Error _ -> Ok (Right_named n)))
    | [ ""; "right"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Right (-x))
    | [ "bottom"; "3/4" ] -> Ok Bottom_3_4
    | [ "bottom"; "auto" ] -> Ok Bottom_auto
    | [ "bottom"; "full" ] -> Ok Bottom_full
    | [ ""; "bottom"; "full" ] -> Ok Neg_bottom_full
    | [ "bottom"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Bottom x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Bottom_arbitrary len)
            | Error _ -> Ok (Bottom_named n)))
    | [ ""; "bottom"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Bottom (-x))
    | [ "left"; "1/2" ] -> Ok Left_1_2
    | [ "left"; "3/4" ] -> Ok Left_3_4
    | [ "left"; "auto" ] -> Ok Left_auto
    | [ "left"; "full" ] -> Ok Left_full
    | [ ""; "left"; "full" ] -> Ok Neg_left_full
    | [ "left"; n ] -> (
        match int_of_string_with_sign n with
        | Ok x -> Ok (Left x)
        | Error _ -> (
            match parse_bracket_length n with
            | Ok len -> Ok (Left_arbitrary len)
            | Error _ -> Ok (Left_named n)))
    | [ ""; "left"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Left (-x))
    | [ "start"; "3/4" ] -> Ok Start_3_4
    | [ "start"; "auto" ] -> Ok Start_auto
    | [ "start"; "full" ] -> Ok Start_full
    | [ "start"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Start x)
    | [ ""; "start"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> Start (-x))
    | [ "end"; "3/4" ] -> Ok End_3_4
    | [ "end"; "auto" ] -> Ok End_auto
    | [ "end"; "full" ] -> Ok End_full
    | [ "end"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> End x)
    | [ ""; "end"; n ] ->
        int_of_string_with_sign n |> Result.map (fun x -> End (-x))
    | [ "z"; n ] -> int_of_string_with_sign n |> Result.map (fun x -> Z x)
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
    | Inset_3_4 -> "inset-3/4"
    | Inset_x_auto -> "inset-x-auto"
    | Inset_x_full -> "inset-x-full"
    | Neg_inset_x_full -> "-inset-x-full"
    | Inset_x_3_4 -> "inset-x-3/4"
    | Inset_y_auto -> "inset-y-auto"
    | Inset_y_full -> "inset-y-full"
    | Neg_inset_y_full -> "-inset-y-full"
    | Inset_y_3_4 -> "inset-y-3/4"
    | Inset n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-" ^ string_of_int (abs n)
    | Inset_arbitrary len ->
        "inset-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_named name -> "inset-" ^ name
    | Inset_x n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-x-" ^ string_of_int (abs n)
    | Inset_x_arbitrary len ->
        "inset-x-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_x_named name -> "inset-x-" ^ name
    | Inset_y n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-y-" ^ string_of_int (abs n)
    | Inset_y_arbitrary len ->
        "inset-y-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_y_named name -> "inset-y-" ^ name
    (* inset-s = inset-inline-start *)
    | Inset_s n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-s-" ^ string_of_int (abs n)
    | Inset_s_arbitrary len ->
        "inset-s-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_s_named name -> "inset-s-" ^ name
    | Inset_s_auto -> "inset-s-auto"
    | Inset_s_full -> "inset-s-full"
    | Neg_inset_s_full -> "-inset-s-full"
    | Inset_s_3_4 -> "inset-s-3/4"
    (* inset-e = inset-inline-end *)
    | Inset_e n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-e-" ^ string_of_int (abs n)
    | Inset_e_arbitrary len ->
        "inset-e-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_e_named name -> "inset-e-" ^ name
    | Inset_e_auto -> "inset-e-auto"
    | Inset_e_full -> "inset-e-full"
    | Neg_inset_e_full -> "-inset-e-full"
    | Inset_e_3_4 -> "inset-e-3/4"
    (* inset-bs = inset-block-start *)
    | Inset_bs n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-bs-" ^ string_of_int (abs n)
    | Inset_bs_arbitrary len ->
        "inset-bs-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_bs_named name -> "inset-bs-" ^ name
    | Inset_bs_auto -> "inset-bs-auto"
    | Inset_bs_full -> "inset-bs-full"
    | Neg_inset_bs_full -> "-inset-bs-full"
    | Inset_bs_3_4 -> "inset-bs-3/4"
    (* inset-be = inset-block-end *)
    | Inset_be n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "inset-be-" ^ string_of_int (abs n)
    | Inset_be_arbitrary len ->
        "inset-be-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Inset_be_named name -> "inset-be-" ^ name
    | Inset_be_auto -> "inset-be-auto"
    | Inset_be_full -> "inset-be-full"
    | Neg_inset_be_full -> "-inset-be-full"
    | Inset_be_3_4 -> "inset-be-3/4"
    | Top_1_2 -> "top-1/2"
    | Top_3_4 -> "top-3/4"
    | Top_auto -> "top-auto"
    | Top_full -> "top-full"
    | Neg_top_full -> "-top-full"
    | Top n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "top-" ^ string_of_int (abs n)
    | Top_arbitrary len ->
        "top-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Top_named name -> "top-" ^ name
    | Right_3_4 -> "right-3/4"
    | Right_auto -> "right-auto"
    | Right_full -> "right-full"
    | Neg_right_full -> "-right-full"
    | Right n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "right-" ^ string_of_int (abs n)
    | Right_arbitrary len ->
        "right-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Right_named name -> "right-" ^ name
    | Bottom_3_4 -> "bottom-3/4"
    | Bottom_auto -> "bottom-auto"
    | Bottom_full -> "bottom-full"
    | Neg_bottom_full -> "-bottom-full"
    | Bottom n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "bottom-" ^ string_of_int (abs n)
    | Bottom_arbitrary len ->
        "bottom-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Bottom_named name -> "bottom-" ^ name
    | Left_1_2 -> "left-1/2"
    | Left_3_4 -> "left-3/4"
    | Left_auto -> "left-auto"
    | Left_full -> "left-full"
    | Neg_left_full -> "-left-full"
    | Left n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "left-" ^ string_of_int (abs n)
    | Left_arbitrary len ->
        "left-[" ^ Css.Pp.to_string (Css.pp_length ~always:true) len ^ "]"
    | Left_named name -> "left-" ^ name
    | Start_3_4 -> "start-3/4"
    | Start_auto -> "start-auto"
    | Start_full -> "start-full"
    | Start n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "start-" ^ string_of_int (abs n)
    | End_3_4 -> "end-3/4"
    | End_auto -> "end-auto"
    | End_full -> "end-full"
    | End n ->
        let prefix = if n < 0 then "-" else "" in
        prefix ^ "end-" ^ string_of_int (abs n)
    | Z n -> "z-" ^ string_of_int n
end

open Handler

(** Register handler with Utility system *)
let () = Utility.register (module Handler)

(** Public API combinators *)
let utility x = Utility.base (Self x)

let static = utility Position_static
let relative = utility Position_relative
let absolute = utility Position_absolute
let fixed = utility Position_fixed
let sticky = utility Position_sticky
let inset n = utility (Inset n)
let inset_0 = utility Inset_0
let inset_x_0 = utility Inset_x_0
let inset_y_0 = utility Inset_y_0
let top n = utility (Top n)
let right n = utility (Right n)
let bottom n = utility (Bottom n)
let left n = utility (Left n)
let top_1_2 = utility Top_1_2
let left_1_2 = utility Left_1_2
let z n = utility (Z n)
