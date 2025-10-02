(** Utility module for common utility types and functions *)

type base = ..
type t = Base of base | Modified of Style.modifier * t | Group of t list

let base x = Base x

module type Handler = sig
  type t
  type base += Self of t

  val to_style : t -> Style.t
  val priority : int
  val suborder : t -> int
  val of_string : string list -> (t, [ `Msg of string ]) result
end

type handler = H : (module Handler with type t = 'a) -> handler

let handlers : handler list ref = ref []

let register (type a) (module M : Handler with type t = a) =
  let internal_h = H (module M : Handler with type t = a) in
  handlers := internal_h :: !handlers

let base_of_string parts =
  let rec try_handlers = function
    | [] -> Error (`Msg "Unknown utility")
    | H (module M) :: rest -> (
        match M.of_string parts with
        | Ok x -> Ok (M.Self x)
        | Error _ -> try_handlers rest)
  in
  try_handlers !handlers

let base_to_style u =
  let rec try_handlers = function
    | [] ->
        prerr_endline
          ("Total handlers registered: " ^ string_of_int (List.length !handlers));
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H (module M) :: rest -> (
        match u with M.Self x -> M.to_style x | _ -> try_handlers rest)
  in
  try_handlers !handlers

let rec to_style = function
  | Base u -> base_to_style u
  | Modified (m, u) -> Style.Modified (m, to_style u)
  | Group us -> Style.Group (List.map to_style us)

let order (u : base) : int * int =
  let rec try_handlers = function
    | [] ->
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H (module M) :: rest -> (
        match u with
        | M.Self x -> (M.priority, M.suborder x)
        | _ -> try_handlers rest)
  in
  try_handlers !handlers

let deduplicate utilities =
  let rec go seen acc = function
    | [] -> List.rev acc
    | u :: rest ->
        if List.mem u seen then go seen acc rest
        else go (u :: seen) (u :: acc) rest
  in
  go [] [] (List.rev utilities)

let css_of_string = Css.of_string
