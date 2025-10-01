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

open Style
open Css

(** {1 Utility Types} *)

type utility =
  | Container_type_size
  | Container_type_inline_size
  | Container_type_normal
  | Container_name of string

(** {1 Container Type Utilities} *)

let container_type_size = style "container-type-size" [ container_type Size ]

let container_type_inline_size =
  style "container-type-inline-size" [ container_type Inline_size ]

let container_type_normal =
  style "container-type-normal" [ container_type Normal ]

let container_name name = style ("container-" ^ name) [ container_name name ]

(** {1 Container Query Modifiers} *)

let container_sm styles = Modified (Container Container_sm, Group styles)
let container_md styles = Modified (Container Container_md, Group styles)
let container_lg styles = Modified (Container Container_lg, Group styles)
let container_xl styles = Modified (Container Container_xl, Group styles)
let container_2xl styles = Modified (Container Container_2xl, Group styles)

let container ?name min_width styles =
  let query =
    match name with
    | None -> Container_named ("", min_width)
    | Some n -> Container_named (n, min_width)
  in
  Group (List.map (fun t -> Modified (Container query, t)) styles)

(** {1 Helper Functions} *)

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

(** {1 Conversion Functions} *)

let to_style = function
  | Container_type_size -> container_type_size
  | Container_type_inline_size -> container_type_inline_size
  | Container_type_normal -> container_type_normal
  | Container_name name -> container_name name

(** {1 Parsing Functions} *)

let of_string = function
  | [ "container"; "type"; "size" ] -> Ok Container_type_size
  | [ "container"; "type"; "inline"; "size" ] -> Ok Container_type_inline_size
  | [ "container"; "type"; "normal" ] -> Ok Container_type_normal
  | [ "container"; name ] -> Ok (Container_name name)
  | _ -> Error (`Msg "Not a container utility")

(** {1 Ordering Support} *)

let suborder = function
  | Container_type_size -> 0
  | Container_type_inline_size -> 1
  | Container_type_normal -> 2
  | Container_name _ -> 100
