(** Utility module for common utility types and functions *)

type base = ..
(** Base utility type without modifiers - extensible variant *)

(** Utility with optional modifiers *)
type t = Base of base | Modified of Style.modifier * t | Group of t list

(** Wrap a base utility into a Utility.t *)
let base x = Base x

type 'a handler = {
  to_style : 'a -> Style.t;
  priority : int;
  suborder : 'a -> int;
  of_string : string list -> ('a, [ `Msg of string ]) result;
}
(** Generic handler for a specific utility type 'a *)

(** Internal handler wrapper using existential types *)
type internal_handler =
  | H : {
      handler : 'a handler;
      wrap : 'a -> base;
      unwrap : base -> 'a option;
    }
      -> internal_handler

(** Registry of all utility handlers *)
let handlers : internal_handler list ref = ref []

(** Register a typed utility handler with wrapping functions *)
let register (type a) ~(wrap : a -> base) ~(unwrap : base -> a option)
    (h : a handler) =
  let internal_h = H { handler = h; wrap; unwrap } in
  handlers := internal_h :: !handlers

(** Parse CSS string into AST *)
let css_of_string = Css.of_string

(** Parse a class string into a base utility (without modifiers) *)
let base_of_string parts =
  let rec try_handlers = function
    | [] -> Error (`Msg "Unknown utility")
    | H { handler; wrap; _ } :: rest -> (
        match handler.of_string parts with
        | Ok x -> Ok (wrap x)
        | Error _ -> try_handlers rest)
  in
  try_handlers !handlers

(** Convert base utility to Style.t *)
let base_to_style u =
  let rec try_handlers = function
    | [] ->
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H { handler; unwrap; _ } :: rest -> (
        match unwrap u with
        | Some x -> handler.to_style x
        | None -> try_handlers rest)
  in
  try_handlers !handlers

(** Convert Utility.t (with modifiers) to Style.t *)
let rec to_style = function
  | Base u -> base_to_style u
  | Modified (m, u) -> Style.Modified (m, to_style u)
  | Group us -> Style.Group (List.map to_style us)

(** Get ordering information (priority, suborder) for a base utility *)
let order (u : base) : int * int =
  let rec try_handlers = function
    | [] ->
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H { handler; unwrap; _ } :: rest -> (
        match unwrap u with
        | Some x -> (handler.priority, handler.suborder x)
        | None -> try_handlers rest)
  in
  try_handlers !handlers

(** Deduplicate utilities while preserving order (last occurrence wins) *)
let deduplicate utilities =
  let rec go seen acc = function
    | [] -> List.rev acc
    | u :: rest ->
        if List.mem u seen then go seen acc rest
        else go (u :: seen) (u :: acc) rest
  in
  go [] [] (List.rev utilities)
