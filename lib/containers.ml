(** Container query utilities for responsive design based on container size *)

open Core
open Css

(** {1 Container Type Utilities} *)

let container_type_size = style "container-type-size" [ container_type Size ]

let container_type_inline_size =
  style "container-type-inline-size" [ container_type Inline_size ]

let container_type_normal =
  style "container-type-normal" [ container_type Normal ]

let container_name name = style ("container-" ^ name) [ container_name name ]

(** {1 Container Query Modifiers} *)

let on_container_sm styles = Modified (Container Container_sm, Group styles)
let on_container_md styles = Modified (Container Container_md, Group styles)
let on_container_lg styles = Modified (Container Container_lg, Group styles)
let on_container_xl styles = Modified (Container Container_xl, Group styles)
let on_container_2xl styles = Modified (Container Container_2xl, Group styles)

let on_container ?name min_width styles =
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
      Pp.str [ "@container (min-width:"; string_of_int width; "px)" ]
  | Container_named (name, width) ->
      Pp.str [ "@container "; name; " (min-width:"; string_of_int width; "px)" ]

let container_query_to_class_prefix = function
  | Container_sm -> "@sm"
  | Container_md -> "@md"
  | Container_lg -> "@lg"
  | Container_xl -> "@xl"
  | Container_2xl -> "@2xl"
  | Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Container_named (name, width) -> "@" ^ name ^ "/" ^ string_of_int width

(** {1 Parsing Functions} *)

let of_string = function
  | [ "container"; "type"; "size" ] -> Ok container_type_size
  | [ "container"; "type"; "inline"; "size" ] -> Ok container_type_inline_size
  | [ "container"; "type"; "normal" ] -> Ok container_type_normal
  | [ "container"; name ] -> Ok (container_name name)
  | _ -> Error (`Msg "Not a container utility")
