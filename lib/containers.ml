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
  let priority = -1 (* Container appears first in utilities layer *)

  let to_class = function
    | Layout_container -> "container"
    | Container -> "@container"
    | Container_normal -> "@container-normal"
    | Container_named name -> "@container/" ^ name

  let layout_container_style =
    let open Css in
    (* Use top-level media queries to match Tailwind's minified output.
       Container media queries should come AFTER the base .container rule.
       rules.ml handles this ordering since container has only media rules and
       base props. *)
    let container_selector = Selector.class_ "container" in
    let media_rules =
      [
        media ~condition:(Media.Min_width 40.)
          [ rule ~selector:container_selector [ max_width (Rem 40.) ] ];
        media ~condition:(Media.Min_width 48.)
          [ rule ~selector:container_selector [ max_width (Rem 48.) ] ];
        media ~condition:(Media.Min_width 64.)
          [ rule ~selector:container_selector [ max_width (Rem 64.) ] ];
        media ~condition:(Media.Min_width 80.)
          [ rule ~selector:container_selector [ max_width (Rem 80.) ] ];
        media ~condition:(Media.Min_width 96.)
          [ rule ~selector:container_selector [ max_width (Rem 96.) ] ];
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
    | Layout_container -> 0 (* Very low to appear early with media queries *)

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

(** Convert a container query modifier to a structured Container.t condition *)
let container_query_to_condition = function
  | Style.Container_sm -> Css.Container.Min_width_rem 24.
  | Style.Container_md -> Css.Container.Min_width_rem 28.
  | Style.Container_lg -> Css.Container.Min_width_rem 32.
  | Style.Container_xl -> Css.Container.Min_width_rem 36.
  | Style.Container_2xl -> Css.Container.Min_width_rem 42.
  | Style.Container_named ("", width) -> Css.Container.Min_width_px width
  | Style.Container_named (name, width) ->
      Css.Container.Named (name, Min_width_px width)

let container_query_to_class_prefix = function
  | Style.Container_sm -> "@sm"
  | Style.Container_md -> "@md"
  | Style.Container_lg -> "@lg"
  | Style.Container_xl -> "@xl"
  | Style.Container_2xl -> "@2xl"
  | Style.Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Style.Container_named (name, width) ->
      "@" ^ name ^ "/" ^ string_of_int width
