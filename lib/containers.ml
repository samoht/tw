(** Container query utilities for responsive design based on container size. *)

module Css = Cascade.Css

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
  let priority = 1
  (* Tailwind orders .container by its width property: after the position group
     (inset, z-index) and before margin. *)

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
    let min_width_rem rem = media_min_width_length (Rem rem) in
    let media_rules =
      [
        media ~condition:(min_width_rem 40.)
          [ rule ~selector:container_selector [ max_width (Rem 40.) ] ];
        media ~condition:(min_width_rem 48.)
          [ rule ~selector:container_selector [ max_width (Rem 48.) ] ];
        media ~condition:(min_width_rem 64.)
          [ rule ~selector:container_selector [ max_width (Rem 64.) ] ];
        media ~condition:(min_width_rem 80.)
          [ rule ~selector:container_selector [ max_width (Rem 80.) ] ];
        media ~condition:(min_width_rem 96.)
          [ rule ~selector:container_selector [ max_width (Rem 96.) ] ];
      ]
    in
    style ~rules:(Some media_rules) [ width (Pct 100.) ]

  let container_query = style [ container_type Inline_size ]
  let container_normal_style = style [ container_type Normal ]

  (* Tailwind v4 emits the [container] shorthand ([container: <name> /
     inline-size]) rather than the longhand pair. *)
  let container_named_style name =
    style [ Css.Declaration.container ~type_:Inline_size name ]

  let to_style _theme = function
    | Layout_container -> layout_container_style
    | Container -> container_query
    | Container_normal -> container_normal_style
    | Container_named name -> container_named_style name

  let suborder = function
    | Container_named _ -> 0
    | Container -> 1
    | Container_normal -> 2
    | Layout_container -> 3 (* After @container utilities *)

  let of_class _theme = function
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

(* Tailwind v4 container-query thresholds (rem) for the named size scale. *)
let container_size_rem = function
  | Style.Container_3xs -> Some 16.
  | Style.Container_2xs -> Some 18.
  | Style.Container_xs -> Some 20.
  | Style.Container_sm -> Some 24.
  | Style.Container_md -> Some 28.
  | Style.Container_lg -> Some 32.
  | Style.Container_xl -> Some 36.
  | Style.Container_2xl -> Some 42.
  | Style.Container_3xl -> Some 48.
  | Style.Container_4xl -> Some 56.
  | Style.Container_5xl -> Some 64.
  | Style.Container_6xl -> Some 72.
  | Style.Container_7xl -> Some 80.
  | Style.Container_named _ | Style.Container_size _ | Style.Container_len _
  | Style.Container_len_cmp _ ->
      None

(* A [(width <op> len)] container feature query, matching Tailwind v4's range
   syntax: [(width >= 24rem)] for min, [(width < 28rem)] for max. *)
let width_range op len =
  Css.Container.Feature_query
    (Css.Media.Cond
       (Css.Media.Feature
          (Css.Media.Range (Css.Media.Width, op, Css.Media.Length len))))

(* Tailwind v4's max container query is the negated min: [@max-md] is [not
   (width >= 28rem)], which Lightning CSS lowers to [not (min-width:28rem)].
   Emitting the negated form keeps parity with Tailwind's optimized output. *)
let width_cond cmp len =
  match cmp with
  | Style.Cq_min -> width_range Css.Media.Ge len
  | Style.Cq_max -> Css.Container.Not (width_range Css.Media.Ge len)

(** Convert a container query modifier to a structured Container.t condition *)
let container_query_to_condition q =
  let geq len = width_range Css.Media.Ge len in
  let rem r : Css.length = Css.Values.Rem r in
  match q with
  | Style.Container_3xs -> geq (rem 16.)
  | Style.Container_2xs -> geq (rem 18.)
  | Style.Container_xs -> geq (rem 20.)
  | Style.Container_sm -> geq (rem 24.)
  | Style.Container_md -> geq (rem 28.)
  | Style.Container_lg -> geq (rem 32.)
  | Style.Container_xl -> geq (rem 36.)
  | Style.Container_2xl -> geq (rem 42.)
  | Style.Container_3xl -> geq (rem 48.)
  | Style.Container_4xl -> geq (rem 56.)
  | Style.Container_5xl -> geq (rem 64.)
  | Style.Container_6xl -> geq (rem 72.)
  | Style.Container_7xl -> geq (rem 80.)
  | Style.Container_named ("", width) ->
      geq (Css.Values.Px (float_of_int width))
  | Style.Container_named (name, width) ->
      Css.Container.Named (name, geq (Css.Values.Px (float_of_int width)))
  | Style.Container_size (cmp, inner) ->
      width_cond cmp (rem (Option.value ~default:0. (container_size_rem inner)))
  | Style.Container_len len -> geq len
  | Style.Container_len_cmp (cmp, len) -> width_cond cmp len

let container_query_to_class_prefix = function
  | Style.Container_3xs -> "@3xs"
  | Style.Container_2xs -> "@2xs"
  | Style.Container_xs -> "@xs"
  | Style.Container_sm -> "@sm"
  | Style.Container_md -> "@md"
  | Style.Container_lg -> "@lg"
  | Style.Container_xl -> "@xl"
  | Style.Container_2xl -> "@2xl"
  | Style.Container_3xl -> "@3xl"
  | Style.Container_4xl -> "@4xl"
  | Style.Container_5xl -> "@5xl"
  | Style.Container_6xl -> "@6xl"
  | Style.Container_7xl -> "@7xl"
  | Style.Container_named ("", width) -> "@" ^ string_of_int width ^ "px"
  | Style.Container_named (name, width) ->
      "@" ^ name ^ "/" ^ string_of_int width
  | (Style.Container_size _ | Style.Container_len _ | Style.Container_len_cmp _)
    as q ->
      "@" ^ Style.container_size_name q
