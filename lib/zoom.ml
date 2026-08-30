(** zoom utilities *)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t = Percent of float | Arbitrary of string * Css.zoom
  type Utility.base += Self of t

  let name = "zoom"

  (* Tailwind emits [zoom-*] between the transforms and the animations. It
     shares the transforms' priority rather than taking one of its own, there
     being no integer left between the two, and starts past every suborder
     transforms.ml assigns (2004 at the time of writing). *)
  let priority _ = 9
  let suborder = function Percent _ -> 3000 | Arbitrary _ -> 3001

  let num_to_string n =
    if Float.is_integer n then string_of_int (int_of_float n) else Pp.float n

  let to_class = function
    | Percent n -> "zoom-" ^ num_to_string n
    | Arbitrary (raw, _) -> "zoom-" ^ raw

  let to_style _theme = function
    | Percent n -> style [ Css.zoom (Pct n) ]
    | Arbitrary (_, v) -> style [ Css.zoom v ]

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

  let of_class _theme class_name =
    match Parse.split_class class_name with
    | [ "zoom"; n ] -> (
        match int_of_string_opt n with
        | Some i -> Ok (Percent (float_of_int i))
        | None -> (
            match parse_arbitrary n with
            | Some v -> Ok (Arbitrary (n, v))
            | None -> Error (`Msg "Not a zoom utility")))
    | _ -> Error (`Msg "Not a zoom utility")

  let examples = [ Percent 100. ]
end

let () = Utility.register (module Handler)
