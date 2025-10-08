(** Container query utilities for responsive design based on container size. *)

module Handler = struct
  open Style
  open Css

  (** Local container utility type *)
  type t =
    | Layout_container (* .container - layout container with width:100% *)
    | Container (* @container - sets container-type: inline-size *)
    | Container_normal (* @container-normal - sets container-type: normal *)
    | Container_named of string (* @container/name *)

  type Utility.base += Self of t

  let name = "containers"
  let priority = 0 (* Container appears first in utilities layer *)

  let to_class = function
    | Layout_container -> "container"
    | Container -> "@container"
    | Container_normal -> "@container-normal"
    | Container_named name -> "@container/" ^ name

  let layout_container_style =
    let open Css in
    let open Css.Selector in
    let base_sel = class_ "container" in
    let media_rules =
      [
        media ~condition:"(min-width:40rem)"
          [ rule ~selector:base_sel [ max_width (Rem 40.) ] ];
        media ~condition:"(min-width:48rem)"
          [ rule ~selector:base_sel [ max_width (Rem 48.) ] ];
        media ~condition:"(min-width:64rem)"
          [ rule ~selector:base_sel [ max_width (Rem 64.) ] ];
        media ~condition:"(min-width:80rem)"
          [ rule ~selector:base_sel [ max_width (Rem 80.) ] ];
        media ~condition:"(min-width:96rem)"
          [ rule ~selector:base_sel [ max_width (Rem 96.) ] ];
      ]
    in
    style ~rules:(Some media_rules) [ width (Pct 100.) ]

  let container_query = style [ container_type Inline_size ]
  let container_normal_style = style [ container_type Normal ]

  let container_named_style name =
    style [ container_type Inline_size; container_name name ]

  let to_style = function
    | Layout_container -> layout_container_style
    | Container -> container_query
    | Container_normal -> container_normal_style
    | Container_named name -> container_named_style name

  let suborder = function
    | Container_named _ -> 0
    | Container -> 1
    | Container_normal -> 2
    | Layout_container -> 100

  let of_class = function
    | "container" -> Ok Layout_container
    | "@container" -> Ok Container
    | "@container-normal" -> Ok Container_normal
    | n when String.starts_with ~prefix:"@container/" n ->
        let name = String.sub n 11 (String.length n - 11) in
        Ok (Container_named name)
    | _ -> Error (`Msg "Not a container utility")
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

let container = utility Layout_container
let at_container = utility Container
let at_container_normal = utility Container_normal
let at_container_named name = utility (Container_named name)

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
