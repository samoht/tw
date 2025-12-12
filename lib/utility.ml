(** Utility module for common utility types and functions *)

type base = ..
type t = Base of base | Modified of Style.modifier * t | Group of t list

let base x = Base x

module type Handler = sig
  type t
  type base += Self of t

  val name : string
  val to_style : t -> Style.t
  val priority : int
  val suborder : t -> int
  val of_class : string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
end

type handler = H : (module Handler with type t = 'a) -> handler

let handlers : handler list ref = ref []

let register (type a) (module M : Handler with type t = a) =
  let internal_h = H (module M : Handler with type t = a) in
  handlers := internal_h :: !handlers

let name_of_base u =
  let rec try_handlers = function
    | [] -> failwith "name_of_base"
    | H (module M) :: rest -> (
        match u with M.Self _ -> M.name | _ -> try_handlers rest)
  in
  try_handlers !handlers

let class_of_base u =
  let rec try_handlers = function
    | [] -> failwith "name_of_base"
    | H (module M) :: rest -> (
        match u with M.Self x -> M.to_class x | _ -> try_handlers rest)
  in
  try_handlers !handlers

let base_of_class class_name =
  let rec try_handlers = function
    | [] -> Error (`Msg "Unknown utility")
    | H (module M) :: rest -> (
        match M.of_class class_name with
        | Ok x -> Ok (M.Self x)
        | Error _ -> try_handlers rest)
  in
  try_handlers !handlers

(* Keep for backward compatibility with tests *)
let base_of_strings parts =
  let class_name = String.concat "-" parts in
  base_of_class class_name

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

let rec to_class = function
  | Base u -> class_of_base u
  | Modified (m, u) -> (
      match u with
      | Group us ->
          (* When a modifier wraps a group, apply it to each item in the
             group *)
          String.concat " "
            (List.map (fun item -> to_class (Modified (m, item))) us)
      | _ -> Style.pp_modifier m ^ ":" ^ to_class u)
  | Group us -> String.concat " " (List.map to_class us)

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
