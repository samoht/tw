(** Mask gradient utilities for creating gradient masks.

    Provides utilities for mask-t-from-*, mask-t-to-*, mask-r-from-*,
    mask-r-to-*, mask-b-from-*, mask-b-to-*, mask-l-from-*, mask-l-to-*,
    mask-x-from-*, mask-x-to-*, mask-y-from-*, mask-y-to-*, mask-linear-from-*,
    mask-linear-to-*, mask-radial-*, mask-radial-from-*, mask-radial-to-*,
    mask-conic-from-*, mask-conic-to-*. *)

module Handler = struct
  open Style
  open Css

  type direction =
    | Top
    | Right
    | Bottom
    | Left
    | X
    | Y
    | Linear
    | Radial
    | Conic

  type position_end = From | To

  type value =
    | Spacing of float (* calc(var(--spacing) * N) *)
    | Percent of float (* N% *)
    | Arbitrary of string (* [value] *)

  type radial_shape = Circle | Ellipse

  type radial_size =
    | Closest_corner
    | Closest_side
    | Farthest_corner
    | Farthest_side
    | Arbitrary_size of string

  type radial_at_position =
    | At_keyword of string (* keywords like "bottom", "top left" *)
    | At_arbitrary of
        string (* arbitrary values like "25%" - stored without brackets *)

  type t =
    | Mask_position of direction * position_end * value
    | Mask_radial (* just mask-radial with no position *)
    | Mask_radial_at of radial_at_position (* mask-radial-at-* *)
    | Mask_radial_shape of radial_shape (* mask-circle, mask-ellipse *)
    | Mask_radial_size of radial_size (* mask-radial-closest-corner etc. *)

  type Utility.base += Self of t

  let name = "mask_gradient"
  let priority = 21

  let direction_name = function
    | Top -> "top"
    | Right -> "right"
    | Bottom -> "bottom"
    | Left -> "left"
    | X -> "x"
    | Y -> "y"
    | Linear -> "linear"
    | Radial -> "radial"
    | Conic -> "conic"

  let direction_short = function
    | Top -> "t"
    | Right -> "r"
    | Bottom -> "b"
    | Left -> "l"
    | X -> "x"
    | Y -> "y"
    | Linear -> "linear"
    | Radial -> "radial"
    | Conic -> "conic"

  let position_end_name = function From -> "from" | To -> "to"

  (* Format the position value as CSS *)
  let format_position_value = function
    | Spacing n ->
        if n = 0.0 then "calc(var(--spacing) * 0)"
        else if Float.is_integer n then
          Printf.sprintf "calc(var(--spacing) * %d)" (int_of_float n)
        else Printf.sprintf "calc(var(--spacing) * %g)" n
    | Percent p ->
        if Float.is_integer p then Printf.sprintf "%d%%" (int_of_float p)
        else Printf.sprintf "%g%%" p
    | Arbitrary v -> v

  (* Common mask image using List of three var references *)
  let mask_image_list : background_image =
    List
      [
        background_image_var_none "tw-mask-linear";
        background_image_var_none "tw-mask-radial";
        background_image_var_none "tw-mask-conic";
      ]

  (* For directional masks, the linear composition *)
  let mask_linear_decl =
    "var(--tw-mask-left), var(--tw-mask-right), var(--tw-mask-bottom), \
     var(--tw-mask-top)"

  (* Generate the gradient value for a direction *)
  let gradient_for_direction dir =
    let dir_name = direction_name dir in
    Printf.sprintf
      "linear-gradient(to %s, var(--tw-mask-%s-from-color) \
       var(--tw-mask-%s-from-position), var(--tw-mask-%s-to-color) \
       var(--tw-mask-%s-to-position))"
      dir_name dir_name dir_name dir_name dir_name

  (* Radial mask uses a stops variable for composability *)
  let radial_stops_decl =
    "var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at \
     var(--tw-mask-radial-position), var(--tw-mask-radial-from-color) \
     var(--tw-mask-radial-from-position), var(--tw-mask-radial-to-color) \
     var(--tw-mask-radial-to-position)"

  let radial_gradient_decl = "radial-gradient(var(--tw-mask-radial-stops))"

  (* Conic mask uses a stops variable for composability *)
  let conic_stops_decl =
    "from var(--tw-mask-conic-position), var(--tw-mask-conic-from-color) \
     var(--tw-mask-conic-from-position), var(--tw-mask-conic-to-color) \
     var(--tw-mask-conic-to-position)"

  let conic_gradient_decl = "conic-gradient(var(--tw-mask-conic-stops))"

  (* Linear mask uses a stops variable for composability *)
  let linear_stops_decl =
    "var(--tw-mask-linear-position), var(--tw-mask-linear-from-color) \
     var(--tw-mask-linear-from-position), var(--tw-mask-linear-to-color) \
     var(--tw-mask-linear-to-position)"

  let linear_gradient_decl = "linear-gradient(var(--tw-mask-linear-stops))"

  (* Common composite declarations *)
  let composite_decls =
    [
      webkit_mask_composite Source_in;
      webkit_mask_composite Source_in;
      mask_composite Intersect;
    ]

  (* Helper to create mask-image declarations using typed functions *)
  let mask_image_decls =
    [
      webkit_mask_image mask_image_list;
      webkit_mask_image mask_image_list;
      mask_image mask_image_list;
    ]

  (* Build the style for a directional mask position *)
  let build_directional_style dir pos_end value =
    let dir_name = direction_name dir in
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    (* The variable being set *)
    let var_name =
      Printf.sprintf "--tw-mask-%s-%s-position" dir_name pos_name
    in

    (* Common declarations for all directional masks *)
    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-%s" dir_name)
            (gradient_for_direction dir);
          custom_property ~layer:"utilities" var_name pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  (* Build the style for mask-x (both left and right) *)
  let build_x_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          custom_property ~layer:"utilities" "--tw-mask-left"
            (gradient_for_direction Left);
          custom_property ~layer:"utilities" "--tw-mask-right"
            (gradient_for_direction Right);
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-left-%s-position" pos_name)
            pos_value;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-right-%s-position" pos_name)
            pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  (* Build the style for mask-y (both top and bottom) *)
  let build_y_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          custom_property ~layer:"utilities" "--tw-mask-top"
            (gradient_for_direction Top);
          custom_property ~layer:"utilities" "--tw-mask-bottom"
            (gradient_for_direction Bottom);
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-top-%s-position" pos_name)
            pos_value;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-bottom-%s-position" pos_name)
            pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  (* Build the style for mask-linear (generic linear gradient) *)
  let build_linear_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear-stops"
            linear_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-linear"
            linear_gradient_decl;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-linear-%s-position" pos_name)
            pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  (* Build the style for mask-radial position *)
  let build_radial_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-radial-stops"
            radial_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-radial"
            radial_gradient_decl;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-radial-%s-position" pos_name)
            pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  (* Build the style for mask-radial (no position) - produces no output in
     Tailwind *)
  let build_radial_base_style = style []

  (* Build the style for mask-radial-at-* - only sets the position variable *)
  let build_radial_at_style pos =
    let position_str =
      match pos with At_keyword s -> s | At_arbitrary s -> s
    in
    let decls =
      [
        custom_property ~layer:"utilities" "--tw-mask-radial-position"
          position_str;
      ]
    in
    style decls

  (* Build the style for mask-circle/mask-ellipse *)
  let build_radial_shape_style shape =
    let shape_str =
      match shape with Circle -> "circle" | Ellipse -> "ellipse"
    in
    style
      [ custom_property ~layer:"utilities" "--tw-mask-radial-shape" shape_str ]

  (* Build the style for mask-radial size keywords and arbitrary sizes *)
  let build_radial_size_style size =
    let size_str =
      match size with
      | Closest_corner -> "closest-corner"
      | Closest_side -> "closest-side"
      | Farthest_corner -> "farthest-corner"
      | Farthest_side -> "farthest-side"
      | Arbitrary_size s -> s
    in
    (* Arbitrary sizes get full mask setup, keywords just set the variable *)
    match size with
    | Arbitrary_size _ ->
        let common_decls =
          mask_image_decls
          @ [
              custom_property ~layer:"utilities" "--tw-mask-radial"
                "radial-gradient(var(--tw-mask-radial-stops, \
                 var(--tw-mask-radial-size)))";
              custom_property ~layer:"utilities" "--tw-mask-radial-size"
                size_str;
            ]
        in
        style (common_decls @ composite_decls)
    | _ ->
        style
          [
            custom_property ~layer:"utilities" "--tw-mask-radial-size" size_str;
          ]

  (* Build the style for mask-conic position *)
  let build_conic_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-conic-stops"
            conic_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-conic"
            conic_gradient_decl;
          custom_property ~layer:"utilities"
            (Printf.sprintf "--tw-mask-conic-%s-position" pos_name)
            pos_value;
        ]
    in
    style (common_decls @ composite_decls)

  let to_style = function
    | Mask_position (Top, pos_end, value) ->
        build_directional_style Top pos_end value
    | Mask_position (Right, pos_end, value) ->
        build_directional_style Right pos_end value
    | Mask_position (Bottom, pos_end, value) ->
        build_directional_style Bottom pos_end value
    | Mask_position (Left, pos_end, value) ->
        build_directional_style Left pos_end value
    | Mask_position (X, pos_end, value) -> build_x_style pos_end value
    | Mask_position (Y, pos_end, value) -> build_y_style pos_end value
    | Mask_position (Linear, pos_end, value) -> build_linear_style pos_end value
    | Mask_position (Radial, pos_end, value) -> build_radial_style pos_end value
    | Mask_position (Conic, pos_end, value) -> build_conic_style pos_end value
    | Mask_radial -> build_radial_base_style
    | Mask_radial_at pos -> build_radial_at_style pos
    | Mask_radial_shape shape -> build_radial_shape_style shape
    | Mask_radial_size size -> build_radial_size_style size

  let suborder = function
    | Mask_position (dir, pos_end, _) ->
        let dir_offset =
          match dir with
          | Top -> 0
          | Right -> 100
          | Bottom -> 200
          | Left -> 300
          | X -> 400
          | Y -> 500
          | Linear -> 600
          | Radial -> 700
          | Conic -> 800
        in
        let pos_offset = match pos_end with From -> 0 | To -> 50 in
        dir_offset + pos_offset
    | Mask_radial -> 750
    | Mask_radial_at _ -> 760
    | Mask_radial_shape Circle -> 770
    | Mask_radial_shape Ellipse -> 771
    | Mask_radial_size Closest_corner -> 780
    | Mask_radial_size Closest_side -> 781
    | Mask_radial_size Farthest_corner -> 782
    | Mask_radial_size Farthest_side -> 783
    | Mask_radial_size (Arbitrary_size _) -> 785

  (* Parse a value from the class suffix *)
  let parse_value suffix =
    if String.length suffix > 0 && suffix.[0] = '[' then
      (* Arbitrary value *)
      let len = String.length suffix in
      if len > 2 && suffix.[len - 1] = ']' then
        let inner = String.sub suffix 1 (len - 2) in
        Option.some (Arbitrary inner)
      else Option.none
    else if String.length suffix > 0 && suffix.[String.length suffix - 1] = '%'
    then
      (* Percentage *)
      let num_str = String.sub suffix 0 (String.length suffix - 1) in
      match float_of_string_opt num_str with
      | Some n -> Option.some (Percent n)
      | None -> Option.none
    else
      (* Spacing multiplier *)
      match float_of_string_opt suffix with
      | Some n -> Option.some (Spacing n)
      | None -> Option.none

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    (* mask-t-from-*, mask-t-to-* *)
    | "mask" :: "t" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Top, From, value))
        | None -> Error (`Msg "Invalid mask-t-from value"))
    | "mask" :: "t" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Top, To, value))
        | None -> Error (`Msg "Invalid mask-t-to value"))
    (* mask-r-from-*, mask-r-to-* *)
    | "mask" :: "r" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Right, From, value))
        | None -> Error (`Msg "Invalid mask-r-from value"))
    | "mask" :: "r" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Right, To, value))
        | None -> Error (`Msg "Invalid mask-r-to value"))
    (* mask-b-from-*, mask-b-to-* *)
    | "mask" :: "b" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Bottom, From, value))
        | None -> Error (`Msg "Invalid mask-b-from value"))
    | "mask" :: "b" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Bottom, To, value))
        | None -> Error (`Msg "Invalid mask-b-to value"))
    (* mask-l-from-*, mask-l-to-* *)
    | "mask" :: "l" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Left, From, value))
        | None -> Error (`Msg "Invalid mask-l-from value"))
    | "mask" :: "l" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Left, To, value))
        | None -> Error (`Msg "Invalid mask-l-to value"))
    (* mask-x-from-*, mask-x-to-* *)
    | "mask" :: "x" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (X, From, value))
        | None -> Error (`Msg "Invalid mask-x-from value"))
    | "mask" :: "x" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (X, To, value))
        | None -> Error (`Msg "Invalid mask-x-to value"))
    (* mask-y-from-*, mask-y-to-* *)
    | "mask" :: "y" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Y, From, value))
        | None -> Error (`Msg "Invalid mask-y-from value"))
    | "mask" :: "y" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Y, To, value))
        | None -> Error (`Msg "Invalid mask-y-to value"))
    (* mask-linear-from-*, mask-linear-to-* *)
    | "mask" :: "linear" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Linear, From, value))
        | None -> Error (`Msg "Invalid mask-linear-from value"))
    | "mask" :: "linear" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Linear, To, value))
        | None -> Error (`Msg "Invalid mask-linear-to value"))
    (* mask-radial *)
    | [ "mask"; "radial" ] -> Ok Mask_radial
    (* mask-radial-at-* *)
    | "mask" :: "radial" :: "at" :: rest when rest <> [] ->
        let position = String.concat " " rest in
        (* Handle arbitrary values - strip brackets *)
        let pos =
          if
            String.length position > 2
            && position.[0] = '['
            && position.[String.length position - 1] = ']'
          then At_arbitrary (String.sub position 1 (String.length position - 2))
          else At_keyword position
        in
        Ok (Mask_radial_at pos)
    (* mask-radial-from-*, mask-radial-to-* *)
    | "mask" :: "radial" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Radial, From, value))
        | None -> Error (`Msg "Invalid mask-radial-from value"))
    | "mask" :: "radial" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Radial, To, value))
        | None -> Error (`Msg "Invalid mask-radial-to value"))
    (* mask-conic-from-*, mask-conic-to-* *)
    | "mask" :: "conic" :: "from" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Conic, From, value))
        | None -> Error (`Msg "Invalid mask-conic-from value"))
    | "mask" :: "conic" :: "to" :: rest when rest <> [] -> (
        let suffix = String.concat "-" rest in
        match parse_value suffix with
        | Some value -> Ok (Mask_position (Conic, To, value))
        | None -> Error (`Msg "Invalid mask-conic-to value"))
    (* mask-circle, mask-ellipse *)
    | [ "mask"; "circle" ] -> Ok (Mask_radial_shape Circle)
    | [ "mask"; "ellipse" ] -> Ok (Mask_radial_shape Ellipse)
    (* mask-radial size keywords *)
    | [ "mask"; "radial"; "closest"; "corner" ] ->
        Ok (Mask_radial_size Closest_corner)
    | [ "mask"; "radial"; "closest"; "side" ] ->
        Ok (Mask_radial_size Closest_side)
    | [ "mask"; "radial"; "farthest"; "corner" ] ->
        Ok (Mask_radial_size Farthest_corner)
    | [ "mask"; "radial"; "farthest"; "side" ] ->
        Ok (Mask_radial_size Farthest_side)
    (* mask-radial-[size] - arbitrary size *)
    | [ "mask"; "radial"; arb ]
      when String.length arb > 2
           && arb.[0] = '['
           && arb.[String.length arb - 1] = ']' ->
        let size_value = String.sub arb 1 (String.length arb - 2) in
        (* Replace underscores with spaces *)
        let size_value =
          String.map (fun c -> if c = '_' then ' ' else c) size_value
        in
        Ok (Mask_radial_size (Arbitrary_size size_value))
    | _ -> Error (`Msg "Not a mask gradient utility")

  let format_value = function
    | Spacing n ->
        if Float.is_integer n then string_of_int (int_of_float n)
        else string_of_float n
    | Percent p ->
        if Float.is_integer p then Printf.sprintf "%d%%" (int_of_float p)
        else Printf.sprintf "%g%%" p
    | Arbitrary v -> Printf.sprintf "[%s]" v

  let to_class = function
    | Mask_position (dir, pos_end, value) ->
        Printf.sprintf "mask-%s-%s-%s" (direction_short dir)
          (position_end_name pos_end)
          (format_value value)
    | Mask_radial -> "mask-radial"
    | Mask_radial_at (At_keyword pos) ->
        Printf.sprintf "mask-radial-at-%s"
          (String.concat "-" (String.split_on_char ' ' pos))
    | Mask_radial_at (At_arbitrary pos) ->
        Printf.sprintf "mask-radial-at-[%s]" pos
    | Mask_radial_shape Circle -> "mask-circle"
    | Mask_radial_shape Ellipse -> "mask-ellipse"
    | Mask_radial_size Closest_corner -> "mask-radial-closest-corner"
    | Mask_radial_size Closest_side -> "mask-radial-closest-side"
    | Mask_radial_size Farthest_corner -> "mask-radial-farthest-corner"
    | Mask_radial_size Farthest_side -> "mask-radial-farthest-side"
    | Mask_radial_size (Arbitrary_size s) ->
        let escaped = String.map (fun c -> if c = ' ' then '_' else c) s in
        Printf.sprintf "mask-radial-[%s]" escaped
end

open Handler

let () = Utility.register (module Handler)
let utility x = Utility.base (Self x)

(* Convenience functions for creating mask gradient utilities *)
let mask_t_from value = utility (Mask_position (Top, From, value))
let mask_t_to value = utility (Mask_position (Top, To, value))
let mask_r_from value = utility (Mask_position (Right, From, value))
let mask_r_to value = utility (Mask_position (Right, To, value))
let mask_b_from value = utility (Mask_position (Bottom, From, value))
let mask_b_to value = utility (Mask_position (Bottom, To, value))
let mask_l_from value = utility (Mask_position (Left, From, value))
let mask_l_to value = utility (Mask_position (Left, To, value))
let mask_x_from value = utility (Mask_position (X, From, value))
let mask_x_to value = utility (Mask_position (X, To, value))
let mask_y_from value = utility (Mask_position (Y, From, value))
let mask_y_to value = utility (Mask_position (Y, To, value))
let mask_linear_from value = utility (Mask_position (Linear, From, value))
let mask_linear_to value = utility (Mask_position (Linear, To, value))
let mask_radial = utility Mask_radial
let mask_radial_at pos = utility (Mask_radial_at pos)
let mask_radial_from value = utility (Mask_position (Radial, From, value))
let mask_radial_to value = utility (Mask_position (Radial, To, value))
let mask_conic_from value = utility (Mask_position (Conic, From, value))
let mask_conic_to value = utility (Mask_position (Conic, To, value))
