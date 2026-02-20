(** Text shadow utilities for CSS text-shadow property. *)

module Handler = struct
  open Style
  open Css

  type t =
    | Text_shadow_none
    | Text_shadow_2xs
    | Text_shadow_xs
    | Text_shadow_sm
    | Text_shadow_md
    | Text_shadow_lg
    | Text_shadow_arbitrary of string

  type Utility.base += Self of t

  let name = "text_shadow"
  let priority = 35 (* After effects/shadows *)

  (* Define the text-shadow-color variable *)
  let text_shadow_color_var =
    Var.channel ~needs_property:true ~property_order:35 ~family:`Text_shadow
      Css.Color "tw-text-shadow-color"

  (* Parse arbitrary shadow value like "12px_12px_#0088cc" *)
  let parse_arbitrary_shadow (s : string) :
      (length * length * length option * string option) option =
    let normalized = String.map (fun c -> if c = '_' then ' ' else c) s in
    let parts = String.split_on_char ' ' normalized in
    let parse_length str : length option =
      let len = String.length str in
      if len >= 1 then (
        let num_end = ref 0 in
        while
          !num_end < len
          && (str.[!num_end] = '-'
             || str.[!num_end] = '.'
             || (str.[!num_end] >= '0' && str.[!num_end] <= '9'))
        do
          incr num_end
        done;
        let num_str = String.sub str 0 !num_end in
        let unit_str = String.sub str !num_end (len - !num_end) in
        match float_of_string_opt num_str with
        | Some n -> (
            match unit_str with
            | "px" -> Some (Px n)
            | "rem" -> Some (Rem n)
            | "em" -> Some (Em n)
            | "" when n = 0.0 -> Some Zero
            | _ -> Option.none)
        | Option.None -> Option.none)
      else Option.none
    in
    (* Extract color if present (starts with #) *)
    let rec find_color_and_lengths acc (parts : string list) :
        string list * string option =
      match parts with
      | [] -> (List.rev acc, Option.none)
      | x :: _rest when String.length x > 0 && x.[0] = '#' ->
          (List.rev acc, Option.some x)
      | x :: rest -> find_color_and_lengths (x :: acc) rest
    in
    let length_strs, color_opt = find_color_and_lengths [] parts in
    let lengths = List.filter_map parse_length length_strs in
    match lengths with
    | [ h; v ] -> Option.some (h, v, Option.none, color_opt)
    | [ h; v; blur ] -> Option.some (h, v, Option.some blur, color_opt)
    | _ -> Option.none

  (* Shorten hex color if possible: #0088cc -> #08c *)
  let shorten_hex c =
    let len = String.length c in
    if len = 7 then
      let r1, r2 = (c.[1], c.[2]) in
      let g1, g2 = (c.[3], c.[4]) in
      let b1, b2 = (c.[5], c.[6]) in
      if r1 = r2 && g1 = g2 && b1 = b2 then Printf.sprintf "#%c%c%c" r1 g1 b1
      else c
    else c

  let text_shadow_none = style [ Css.text_shadow Css.None ]

  let text_shadow_2xs =
    let color_ref =
      Var.reference_with_fallback text_shadow_color_var (Css.hex "#0000001a")
    in
    style
      [
        Css.text_shadow
          (Css.Text_shadow
             {
               h_offset = Px 0.;
               v_offset = Px 1.;
               blur = Some (Px 0.);
               color = Some (Var color_ref);
             });
      ]

  let text_shadow_xs =
    let color_ref =
      Var.reference_with_fallback text_shadow_color_var (Css.hex "#0000001a")
    in
    style
      [
        Css.text_shadow
          (Css.Text_shadow
             {
               h_offset = Px 0.;
               v_offset = Px 1.;
               blur = Some (Px 1.);
               color = Some (Var color_ref);
             });
      ]

  let text_shadow_sm =
    (* text-shadow-sm has two shadows *)
    let color_ref =
      Var.reference_with_fallback text_shadow_color_var (Css.hex "#0000000f")
    in
    let shadow1 =
      Css.Text_shadow
        {
          h_offset = Px 0.;
          v_offset = Px 1.;
          blur = Some (Px 2.);
          color = Some (Var color_ref);
        }
    in
    let shadow2 =
      Css.Text_shadow
        {
          h_offset = Px 0.;
          v_offset = Px 2.;
          blur = Some (Px 2.);
          color = Some (Var color_ref);
        }
    in
    (* Use text_shadows to emit multiple shadows *)
    style [ Css.text_shadows [ shadow1; shadow2 ] ]

  let text_shadow_md =
    let color_ref =
      Var.reference_with_fallback text_shadow_color_var (Css.hex "#0000001a")
    in
    style
      [
        Css.text_shadow
          (Css.Text_shadow
             {
               h_offset = Px 0.;
               v_offset = Px 2.;
               blur = Some (Px 4.);
               color = Some (Var color_ref);
             });
      ]

  let text_shadow_lg =
    let color_ref =
      Var.reference_with_fallback text_shadow_color_var (Css.hex "#0000001a")
    in
    style
      [
        Css.text_shadow
          (Css.Text_shadow
             {
               h_offset = Px 0.;
               v_offset = Px 4.;
               blur = Some (Px 8.);
               color = Some (Var color_ref);
             });
      ]

  let text_shadow_arbitrary (arb : string) =
    match parse_arbitrary_shadow arb with
    | Option.Some (h_offset, v_offset, blur, color_opt) ->
        let fallback_color =
          match color_opt with
          | Option.Some c -> shorten_hex c
          | Option.None -> "currentcolor"
        in
        let color_ref =
          Var.reference_with_fallback text_shadow_color_var
            (Css.hex fallback_color)
        in
        style
          [
            Css.text_shadow
              (Css.Text_shadow
                 { h_offset; v_offset; blur; color = Some (Var color_ref) });
          ]
    | Option.None -> text_shadow_none

  let to_style = function
    | Text_shadow_none -> text_shadow_none
    | Text_shadow_2xs -> text_shadow_2xs
    | Text_shadow_xs -> text_shadow_xs
    | Text_shadow_sm -> text_shadow_sm
    | Text_shadow_md -> text_shadow_md
    | Text_shadow_lg -> text_shadow_lg
    | Text_shadow_arbitrary arb -> text_shadow_arbitrary arb

  let err_not_utility = Error (`Msg "Not a text shadow utility")

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    | [ "text"; "shadow"; "none" ] -> Ok Text_shadow_none
    | [ "text"; "shadow"; "2xs" ] -> Ok Text_shadow_2xs
    | [ "text"; "shadow"; "xs" ] -> Ok Text_shadow_xs
    | [ "text"; "shadow"; "sm" ] -> Ok Text_shadow_sm
    | [ "text"; "shadow"; "md" ] -> Ok Text_shadow_md
    | [ "text"; "shadow" ] -> Ok Text_shadow_md
    | [ "text"; "shadow"; "lg" ] -> Ok Text_shadow_lg
    | [ "text"; "shadow"; arb ] when String.length arb > 2 && arb.[0] = '[' ->
        let len = String.length arb in
        if arb.[len - 1] = ']' then
          let inner = String.sub arb 1 (len - 2) in
          Ok (Text_shadow_arbitrary inner)
        else err_not_utility
    | _ -> err_not_utility

  let to_class = function
    | Text_shadow_none -> "text-shadow-none"
    | Text_shadow_2xs -> "text-shadow-2xs"
    | Text_shadow_xs -> "text-shadow-xs"
    | Text_shadow_sm -> "text-shadow-sm"
    | Text_shadow_md -> "text-shadow"
    | Text_shadow_lg -> "text-shadow-lg"
    | Text_shadow_arbitrary arb -> "text-shadow-[" ^ arb ^ "]"

  let suborder = function
    | Text_shadow_none -> 0
    | Text_shadow_2xs -> 1
    | Text_shadow_xs -> 2
    | Text_shadow_sm -> 3
    | Text_shadow_md -> 4
    | Text_shadow_lg -> 5
    | Text_shadow_arbitrary _ -> 100
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)
let text_shadow_none = utility Text_shadow_none
let text_shadow_2xs = utility Text_shadow_2xs
let text_shadow_xs = utility Text_shadow_xs
let text_shadow_sm = utility Text_shadow_sm
let text_shadow = utility Text_shadow_md
let text_shadow_md = utility Text_shadow_md
let text_shadow_lg = utility Text_shadow_lg
let text_shadow_arbitrary arb = utility (Text_shadow_arbitrary arb)
