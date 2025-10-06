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

module Handler = struct
  open Style
  open Css

  (** Local container utility type *)
  type t =
    | Container (* @container - sets container-type: inline-size *)
    | Container_normal (* @container-normal - sets container-type: normal *)
    | Container_named of string (* @container/name *)

  type Utility.base += Self of t

  let name = "containers"
  let priority = 14

  let to_class = function
    | Container -> "@container"
    | Container_normal -> "@container-normal"
    | Container_named name -> "@container/" ^ name

  let container = style [ container_type Inline_size ]
  let container_normal = style [ container_type Normal ]

  let container_named name =
    style [ container_type Inline_size; container_name name ]

  let to_style = function
    | Container -> container
    | Container_normal -> container_normal
    | Container_named name -> container_named name

  let suborder = function
    | Container -> 1
    | Container_named _ -> 0
    | Container_normal -> 100

  let of_class class_name =
    (* Handle @container syntax used in Tailwind v4 *)
    if class_name = "@container" then Ok Container
    else if class_name = "@container-normal" then Ok Container_normal
    else if String.starts_with ~prefix:"@container/" class_name then
      let name = String.sub class_name 11 (String.length class_name - 11) in
      Ok (Container_named name)
    else Error (`Msg "Not a container utility")
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)

(** Container Query Modifiers *)
let container_sm styles =
  Utility.Modified (Container Container_sm, Utility.Group styles)

let container_md styles =
  Utility.Modified (Container Container_md, Utility.Group styles)

let container_lg styles =
  Utility.Modified (Container Container_lg, Utility.Group styles)

let container_xl styles =
  Utility.Modified (Container Container_xl, Utility.Group styles)

let container_2xl styles =
  Utility.Modified (Container Container_2xl, Utility.Group styles)

let container_query ?name min_width styles =
  let query =
    match name with
    | None -> Style.Container_named ("", min_width)
    | Some n -> Style.Container_named (n, min_width)
  in
  Utility.Group
    (List.map (fun t -> Utility.Modified (Container query, t)) styles)

let container = utility Container
let container_normal = utility Container_normal
let container_named name = utility (Container_named name)

(** Helper Functions *)
let container_query_to_css_prefix = function
  | Style.Container_sm -> "@container (min-width:24rem)"
  | Style.Container_md -> "@container (min-width:28rem)"
  | Style.Container_lg -> "@container (min-width:32rem)"
  | Style.Container_xl -> "@container (min-width:36rem)"
  | Style.Container_2xl -> "@container (min-width:42rem)"
  | Style.Container_named ("", width) ->
      "@container (min-width:" ^ string_of_int width ^ "px)"
  | Style.Container_named (name, width) ->
      "@container " ^ name ^ " (min-width:" ^ string_of_int width ^ "px)"

let container_query_to_class_prefix = function
  | Style.Container_sm -> "@sm"
  | Style.Container_md -> "@md"
  | Style.Container_lg -> "@lg"
  | Style.Container_xl -> "@xl"
  | Style.Container_2xl -> "@2xl"
  | Style.Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Style.Container_named (name, width) ->
      "@" ^ name ^ "/" ^ string_of_int width
