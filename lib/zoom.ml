(** zoom utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t = Zoom_pct of float | Zoom_arbitrary of string * Css.zoom
  type Utility.base += Self of t

  let name = "zoom"
  let priority = 2
  let suborder = function Zoom_pct _ -> 0 | Zoom_arbitrary _ -> 1

  let num_to_string n =
    if Float.is_integer n then string_of_int (int_of_float n) else Pp.float n

  let to_class = function
    | Zoom_pct n -> "zoom-" ^ num_to_string n
    | Zoom_arbitrary (raw, _) -> "zoom-" ^ raw

  let to_style _theme = function
    | Zoom_pct n -> style [ Css.zoom (Pct n) ]
    | Zoom_arbitrary (_, v) -> style [ Css.zoom v ]

  (* [zoom-[var(--zoom)]] references a var; other bracket values parse as a
     number or percentage. *)
  let parse_arbitrary raw : Css.zoom option =
    if
      String.length raw > 2
      && raw.[0] = '['
      && raw.[String.length raw - 1] = ']'
    then
      let inner = String.sub raw 1 (String.length raw - 2) in
      if Parse.is_var inner then
        Some (Var (Var.bracket (Parse.extract_var_name inner)) : Css.zoom)
      else
        match float_of_string_opt inner with
        | Some n -> Some (Num n : Css.zoom)
        | None -> None
    else None

  let of_class class_name =
    match Parse.split_class class_name with
    | [ "zoom"; n ] -> (
        match int_of_string_opt n with
        | Some i -> Ok (Zoom_pct (float_of_int i))
        | None -> (
            match parse_arbitrary n with
            | Some v -> Ok (Zoom_arbitrary (n, v))
            | None -> Error (`Msg "Not a zoom utility")))
    | _ -> Error (`Msg "Not a zoom utility")
end

let () = Utility.register (module Handler)
