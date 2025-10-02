(** Container query utilities for responsive design based on container size

    What's included:
    - `container-type-size`, `container-type-inline-size`,
      `container-type-normal` and named containers via `container-<name>`
      helpers.
    - Container query modifiers (`container_*`, `container ?name min_width`).

    What's not:
    - Arbitrary complex container conditions. Use `container ~name width` for
      the common `min-width` pattern; extend with a custom `Modified (Container
      ...)` if you need more.

    Parsing contract (`of_string`):
    - Accepts ["container"; "type"; ..] and ["container"; name]. Unknown tokens
      yield `Error (`Msg "Not a container utility")`. *)

open Css

(** Local container utility type *)
type t =
  | Container_type_size
  | Container_type_inline_size
  | Container_type_normal
  | Container_name of string

type Utility.base += Containers of t

let wrap x = Containers x
let unwrap = function Containers x -> Some x | _ -> None
let base x = Utility.base (Containers x)
let err_not_utility = Error (`Msg "Not a container utility")

(** Container Query Modifiers *)
let container_sm styles =
  Utility.Modified (Style.Container Style.Container_sm, Utility.Group styles)

let container_md styles =
  Utility.Modified (Style.Container Style.Container_md, Utility.Group styles)

let container_lg styles =
  Utility.Modified (Style.Container Style.Container_lg, Utility.Group styles)

let container_xl styles =
  Utility.Modified (Style.Container Style.Container_xl, Utility.Group styles)

let container_2xl styles =
  Utility.Modified (Style.Container Style.Container_2xl, Utility.Group styles)

let container ?name min_width styles =
  let query =
    match name with
    | None -> Style.Container_named ("", min_width)
    | Some n -> Style.Container_named (n, min_width)
  in
  Utility.Group
    (List.map (fun t -> Utility.Modified (Style.Container query, t)) styles)

open Style

let container_type_size' = style "container-type-size" [ container_type Size ]

let container_type_inline_size' =
  style "container-type-inline-size" [ container_type Inline_size ]

let container_type_normal' =
  style "container-type-normal" [ container_type Normal ]

(** Container name utility for setting container-name *)
let container_name' name =
  style ("container-name-" ^ name) [ container_name name ]

let container_type_size = base Container_type_size
let container_type_inline_size = base Container_type_inline_size
let container_type_normal = base Container_type_normal
let container_name_util name = base (Container_name name)
let container_name = container_name_util

(** Helper Functions *)
let container_query_to_css_prefix = function
  | Container_sm -> "@container (min-width:24rem)"
  | Container_md -> "@container (min-width:28rem)"
  | Container_lg -> "@container (min-width:32rem)"
  | Container_xl -> "@container (min-width:36rem)"
  | Container_2xl -> "@container (min-width:42rem)"
  | Container_named ("", width) ->
      "@container (min-width:" ^ string_of_int width ^ "px)"
  | Container_named (name, width) ->
      "@container " ^ name ^ " (min-width:" ^ string_of_int width ^ "px)"

let container_query_to_class_prefix = function
  | Container_sm -> "@sm"
  | Container_md -> "@md"
  | Container_lg -> "@lg"
  | Container_xl -> "@xl"
  | Container_2xl -> "@2xl"
  | Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Container_named (name, width) -> "@" ^ name ^ "/" ^ string_of_int width

let to_style = function
  | Container_type_size -> container_type_size'
  | Container_type_inline_size -> container_type_inline_size'
  | Container_type_normal -> container_type_normal'
  | Container_name name -> container_name' name

let suborder = function
  | Container_type_size -> 0
  | Container_type_inline_size -> 1
  | Container_type_normal -> 2
  | Container_name _ -> 100

let of_string = function
  | [ "container"; "type"; "size" ] -> Ok Container_type_size
  | [ "container"; "type"; "inline"; "size" ] -> Ok Container_type_inline_size
  | [ "container"; "type"; "normal" ] -> Ok Container_type_normal
  | [ "container"; name ] -> Ok (Container_name name)
  | _ -> err_not_utility

let priority = 14
let handler = { Utility.of_string; priority; suborder; to_style }
let () = Utility.register ~wrap ~unwrap handler

module Handler = struct
  type t =
    | Container_type_size
    | Container_type_inline_size
    | Container_type_normal
    | Container_name of string

  let of_string : string list -> (t, [ `Msg of string ]) result = function
    | [ "container"; "type"; "size" ] -> Ok Container_type_size
    | [ "container"; "type"; "inline"; "size" ] -> Ok Container_type_inline_size
    | [ "container"; "type"; "normal" ] -> Ok Container_type_normal
    | [ "container"; name ] -> Ok (Container_name name)
    | _ -> err_not_utility

  let suborder = function
    | Container_type_size -> 0
    | Container_type_inline_size -> 1
    | Container_type_normal -> 2
    | Container_name _ -> 100

  let to_style = function
    | Container_type_size -> container_type_size'
    | Container_type_inline_size -> container_type_inline_size'
    | Container_type_normal -> container_type_normal'
    | Container_name name -> container_name' name

  let order x = (priority, suborder x)
end
