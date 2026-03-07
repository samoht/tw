(** Mask gradient utilities for creating gradient masks.

    Provides utilities for mask-t-from-*, mask-t-to-*, mask-r-from-*,
    mask-r-to-*, mask-b-from-*, mask-b-to-*, mask-l-from-*, mask-l-to-*,
    mask-x-from-*, mask-x-to-*, mask-y-from-*, mask-y-to-*, mask-linear-from-*,
    mask-linear-to-*, mask-radial-*, mask-radial-from-*, mask-radial-to-*,
    mask-conic-from-*, mask-conic-to-*. *)

module Handler = struct
  open Style

  let pp_int = Pp.int
  let pp_float = Pp.float

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

  type mask_angle =
    | Angle_int of int (* mask-linear-45 → calc(1deg * 45) *)
    | Angle_arb of string
  (* mask-linear-[3rad] → original value, converted at style time *)

  type var_ref_kind = Plain_var | Length_var

  type t =
    | Mask_position of direction * position_end * value
    | Mask_var_ref of direction * position_end * var_ref_kind * string
      (* (--var) or (length:--var) → sets position to var(--var) *)
    | Mask_color_ref of direction * position_end * string
      (* (color:--var) → sets color to var(--var) *)
    | Mask_linear_angle of mask_angle
    | Mask_conic_angle of mask_angle
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
          "calc(var(--spacing) * " ^ pp_int (int_of_float n) ^ ")"
        else "calc(var(--spacing) * " ^ pp_float n ^ ")"
    | Percent p ->
        if Float.is_integer p then pp_int (int_of_float p) ^ "%"
        else pp_float p ^ "%"
    | Arbitrary v -> v

  (* When using Spacing values, we need to emit the theme declaration for
     --spacing so that :root, :host { --spacing: .25rem } appears *)
  let spacing_theme_decl value =
    match value with
    | Spacing _ ->
        let decl, _ = Var.binding Theme.spacing_var Theme.spacing_base in
        [ decl ]
    | Percent _ | Arbitrary _ -> []

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
    "linear-gradient(to " ^ dir_name ^ ", var(--tw-mask-" ^ dir_name
    ^ "-from-color) var(--tw-mask-" ^ dir_name
    ^ "-from-position), var(--tw-mask-" ^ dir_name ^ "-to-color) var(--tw-mask-"
    ^ dir_name ^ "-to-position))"

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

  (* @property rule helpers - creates rule AND registers property_order *)
  let prop ?(order = 100) name initial =
    Var.register_property_order ~name:("tw-mask-" ^ name) ~order;
    property ~name:("--tw-mask-" ^ name) Universal ~initial_value:initial
      ~inherits:false ()

  (* Common @property rules for mask-image vars *)
  let common_property_rules =
    concat
      [
        prop ~order:55 "linear" "linear-gradient(#fff, #fff)";
        prop ~order:56 "radial" "linear-gradient(#fff, #fff)";
        prop ~order:57 "conic" "linear-gradient(#fff, #fff)";
      ]

  (* @property rules for directional gradient vars (left, right, bottom, top) *)
  let directional_gradient_property_rules =
    concat
      [
        prop ~order:58 "left" "linear-gradient(#fff, #fff)";
        prop ~order:59 "right" "linear-gradient(#fff, #fff)";
        prop ~order:60 "bottom" "linear-gradient(#fff, #fff)";
        prop ~order:61 "top" "linear-gradient(#fff, #fff)";
      ]

  (* @property rules for a specific direction's from/to vars. Order offset
     varies by direction so that e.g. left endpoints sort before right endpoints
     in the properties layer. *)
  let direction_endpoint_rules ?(order_base = 62) dir_name =
    concat
      [
        prop ~order:order_base (dir_name ^ "-from-position") "0%";
        prop ~order:(order_base + 1) (dir_name ^ "-to-position") "100%";
        prop ~order:(order_base + 2) (dir_name ^ "-from-color") "black";
        prop ~order:(order_base + 3) (dir_name ^ "-to-color") "transparent";
      ]

  (* Pre-compute property rules for all directions at module load time to avoid
     re-registering property orders dynamically during tests. X/Y use different
     order_bases so their sub-directions sort correctly in properties layer. *)
  let top_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules "top";
      ]

  let right_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules "right";
      ]

  let bottom_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules "bottom";
      ]

  let left_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules "left";
      ]

  let x_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules ~order_base:62 "right";
        direction_endpoint_rules ~order_base:66 "left";
      ]

  let y_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules ~order_base:62 "top";
        direction_endpoint_rules ~order_base:66 "bottom";
      ]

  (* Linear/radial/conic property rules always include the position @property
     (used by both angle and from/to modes) *)
  let linear_property_rules =
    concat
      [
        common_property_rules;
        concat [ prop ~order:61 "linear-position" "0deg" ];
        direction_endpoint_rules "linear";
      ]

  let radial_property_rules =
    concat
      [
        common_property_rules;
        direction_endpoint_rules "radial";
        concat
          [
            prop ~order:70 "radial-shape" "ellipse";
            prop ~order:71 "radial-size" "farthest-corner";
            prop ~order:72 "radial-position" "center";
          ];
      ]

  let conic_property_rules =
    concat
      [
        common_property_rules;
        concat [ prop ~order:61 "conic-position" "0deg" ];
        direction_endpoint_rules "conic";
      ]

  let property_rules_for_direction = function
    | Top -> top_property_rules
    | Right -> right_property_rules
    | Bottom -> bottom_property_rules
    | Left -> left_property_rules
    | X -> x_property_rules
    | Y -> y_property_rules
    | Linear -> linear_property_rules
    | Radial -> radial_property_rules
    | Conic -> conic_property_rules

  (* Build the style for a directional mask position *)
  let build_directional_style dir pos_end value =
    let dir_name = direction_name dir in
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    (* The variable being set *)
    let var_name = "--tw-mask-" ^ dir_name ^ "-" ^ pos_name ^ "-position" in

    let property_rules = property_rules_for_direction dir in

    (* Common declarations for all directional masks *)
    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          custom_property ~layer:"utilities" ("--tw-mask-" ^ dir_name)
            (gradient_for_direction dir);
          custom_property ~layer:"utilities" var_name pos_value;
        ]
    in
    style ~property_rules (common_decls @ composite_decls)

  (* Build the style for mask-x (both left and right) *)
  let build_x_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          (* Right group first, then left group — interleaved order *)
          custom_property ~layer:"utilities" "--tw-mask-right"
            (gradient_for_direction Right);
          custom_property ~layer:"utilities"
            ("--tw-mask-right-" ^ pos_name ^ "-position")
            pos_value;
          custom_property ~layer:"utilities" "--tw-mask-left"
            (gradient_for_direction Left);
          custom_property ~layer:"utilities"
            ("--tw-mask-left-" ^ pos_name ^ "-position")
            pos_value;
        ]
    in
    style ~property_rules:x_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-y (both top and bottom) *)
  let build_y_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear" mask_linear_decl;
          (* Top group first, then bottom group — interleaved order *)
          custom_property ~layer:"utilities" "--tw-mask-top"
            (gradient_for_direction Top);
          custom_property ~layer:"utilities"
            ("--tw-mask-top-" ^ pos_name ^ "-position")
            pos_value;
          custom_property ~layer:"utilities" "--tw-mask-bottom"
            (gradient_for_direction Bottom);
          custom_property ~layer:"utilities"
            ("--tw-mask-bottom-" ^ pos_name ^ "-position")
            pos_value;
        ]
    in
    style ~property_rules:y_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-linear (generic linear gradient) *)
  let build_linear_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear-stops"
            linear_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-linear"
            linear_gradient_decl;
          custom_property ~layer:"utilities"
            ("--tw-mask-linear-" ^ pos_name ^ "-position")
            pos_value;
        ]
    in
    style ~property_rules:linear_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-radial position *)
  let build_radial_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-radial-stops"
            radial_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-radial"
            radial_gradient_decl;
          custom_property ~layer:"utilities"
            ("--tw-mask-radial-" ^ pos_name ^ "-position")
            pos_value;
        ]
    in
    style ~property_rules:radial_property_rules (common_decls @ composite_decls)

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
    style ~property_rules:radial_property_rules decls

  (* Build the style for mask-circle/mask-ellipse *)
  let build_radial_shape_style shape =
    let shape_str =
      match shape with Circle -> "circle" | Ellipse -> "ellipse"
    in
    style ~property_rules:radial_property_rules
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
        style ~property_rules:radial_property_rules
          (common_decls @ composite_decls)
    | _ ->
        style ~property_rules:radial_property_rules
          [
            custom_property ~layer:"utilities" "--tw-mask-radial-size" size_str;
          ]

  (* Build the style for mask-conic position *)
  let build_conic_style pos_end value =
    let pos_name = position_end_name pos_end in
    let pos_value = format_position_value value in

    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-conic-stops"
            conic_stops_decl;
          custom_property ~layer:"utilities" "--tw-mask-conic"
            conic_gradient_decl;
          custom_property ~layer:"utilities"
            ("--tw-mask-conic-" ^ pos_name ^ "-position")
            pos_value;
        ]
    in
    style ~property_rules:conic_property_rules (common_decls @ composite_decls)

  (* Convert arbitrary angle values (e.g. "3rad") to degrees *)
  let convert_angle_to_css s =
    let to_deg n =
      let rounded = Float.round (n *. 1000.0) /. 1000.0 in
      Pp.float_to_string ~max_decimals:3 rounded ^ "deg"
    in
    let len = String.length s in
    if len > 3 && String.sub s (len - 3) 3 = "rad" then
      let num_str = String.sub s 0 (len - 3) in
      match float_of_string_opt num_str with
      | Some n -> to_deg (n *. 180.0 /. Float.pi)
      | None -> s
    else if len > 4 && String.sub s (len - 4) 4 = "turn" then
      let num_str = String.sub s 0 (len - 4) in
      match float_of_string_opt num_str with
      | Some n -> to_deg (n *. 360.0)
      | None -> s
    else if len > 4 && String.sub s (len - 4) 4 = "grad" then
      let num_str = String.sub s 0 (len - 4) in
      match float_of_string_opt num_str with
      | Some n -> to_deg (n *. 0.9)
      | None -> s
    else s

  let format_angle_position = function
    | Angle_int n -> "calc(1deg * " ^ string_of_int n ^ ")"
    | Angle_arb s -> convert_angle_to_css s

  (* Build the style for mask-linear-N (angle shorthand) *)
  let build_linear_angle_style angle =
    let pos_value = format_angle_position angle in
    let decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-linear"
            "linear-gradient(var(--tw-mask-linear-stops, \
             var(--tw-mask-linear-position)))";
          custom_property ~layer:"utilities" "--tw-mask-linear-position"
            pos_value;
        ]
    in
    style ~property_rules:linear_property_rules (decls @ composite_decls)

  (* Build the style for mask-conic-N (angle shorthand) *)
  let build_conic_angle_style angle =
    let pos_value = format_angle_position angle in
    let decls =
      mask_image_decls
      @ [
          custom_property ~layer:"utilities" "--tw-mask-conic"
            "conic-gradient(var(--tw-mask-conic-stops, \
             var(--tw-mask-conic-position)))";
          custom_property ~layer:"utilities" "--tw-mask-conic-position"
            pos_value;
        ]
    in
    style ~property_rules:conic_property_rules (decls @ composite_decls)

  (* Build directional var ref/color ref declarations for a single direction *)
  let single_dir_var_decls dir_name pos_name prop var_name =
    [
      custom_property ~layer:"utilities" ("--tw-mask-" ^ dir_name)
        (gradient_for_direction
           (match dir_name with
           | "left" -> Left
           | "right" -> Right
           | "top" -> Top
           | "bottom" -> Bottom
           | d -> failwith ("unexpected direction " ^ d)));
      custom_property ~layer:"utilities"
        ("--tw-mask-" ^ dir_name ^ "-" ^ pos_name ^ "-" ^ prop)
        ("var(" ^ var_name ^ ")");
    ]

  (* Helper to get the stops + gradient decls for a gradient-type direction *)
  let stops_based_decls dir_name stops_decl gradient_decl =
    [
      custom_property ~layer:"utilities"
        ("--tw-mask-" ^ dir_name ^ "-stops")
        stops_decl;
      custom_property ~layer:"utilities" ("--tw-mask-" ^ dir_name) gradient_decl;
    ]

  (* Build the style for parenthesized var reference setting position *)
  let build_var_ref_style dir pos_end var_name =
    let pos_name = position_end_name pos_end in
    let property_rules = property_rules_for_direction dir in
    let merge_key =
      "mask-" ^ direction_short dir ^ "-" ^ pos_name ^ "-var-position"
    in
    let dir_decls =
      match dir with
      | X ->
          single_dir_var_decls "right" pos_name "position" var_name
          @ single_dir_var_decls "left" pos_name "position" var_name
      | Y ->
          single_dir_var_decls "top" pos_name "position" var_name
          @ single_dir_var_decls "bottom" pos_name "position" var_name
      | Linear ->
          stops_based_decls "linear" linear_stops_decl linear_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-linear-" ^ pos_name ^ "-position")
                ("var(" ^ var_name ^ ")");
            ]
      | Radial ->
          stops_based_decls "radial" radial_stops_decl radial_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-radial-" ^ pos_name ^ "-position")
                ("var(" ^ var_name ^ ")");
            ]
      | Conic ->
          stops_based_decls "conic" conic_stops_decl conic_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-conic-" ^ pos_name ^ "-position")
                ("var(" ^ var_name ^ ")");
            ]
      | _ ->
          let dir_name = direction_name dir in
          [
            custom_property ~layer:"utilities" ("--tw-mask-" ^ dir_name)
              (gradient_for_direction dir);
            custom_property ~layer:"utilities"
              ("--tw-mask-" ^ dir_name ^ "-" ^ pos_name ^ "-position")
              ("var(" ^ var_name ^ ")");
          ]
    in
    (* For Linear/Radial/Conic, dir_decls already sets --tw-mask-linear via
       stops_based_decls. For others, we need the mask_linear_decl. *)
    let linear_decl =
      match dir with
      | Linear | Radial | Conic -> []
      | _ ->
          [
            custom_property ~layer:"utilities" "--tw-mask-linear"
              mask_linear_decl;
          ]
    in
    let common_decls = mask_image_decls @ linear_decl @ dir_decls in
    style ~merge_key ~property_rules (common_decls @ composite_decls)

  (* Build the style for parenthesized var reference setting color *)
  let build_color_ref_style dir pos_end var_name =
    let pos_name = position_end_name pos_end in
    let property_rules = property_rules_for_direction dir in
    let dir_decls =
      match dir with
      | X ->
          single_dir_var_decls "right" pos_name "color" var_name
          @ single_dir_var_decls "left" pos_name "color" var_name
      | Y ->
          single_dir_var_decls "top" pos_name "color" var_name
          @ single_dir_var_decls "bottom" pos_name "color" var_name
      | Linear ->
          stops_based_decls "linear" linear_stops_decl linear_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-linear-" ^ pos_name ^ "-color")
                ("var(" ^ var_name ^ ")");
            ]
      | Radial ->
          stops_based_decls "radial" radial_stops_decl radial_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-radial-" ^ pos_name ^ "-color")
                ("var(" ^ var_name ^ ")");
            ]
      | Conic ->
          stops_based_decls "conic" conic_stops_decl conic_gradient_decl
          @ [
              custom_property ~layer:"utilities"
                ("--tw-mask-conic-" ^ pos_name ^ "-color")
                ("var(" ^ var_name ^ ")");
            ]
      | _ ->
          let dir_name = direction_name dir in
          [
            custom_property ~layer:"utilities" ("--tw-mask-" ^ dir_name)
              (gradient_for_direction dir);
            custom_property ~layer:"utilities"
              ("--tw-mask-" ^ dir_name ^ "-" ^ pos_name ^ "-color")
              ("var(" ^ var_name ^ ")");
          ]
    in
    let linear_decl =
      match dir with
      | Linear | Radial | Conic -> []
      | _ ->
          [
            custom_property ~layer:"utilities" "--tw-mask-linear"
              mask_linear_decl;
          ]
    in
    let common_decls = mask_image_decls @ linear_decl @ dir_decls in
    style ~property_rules (common_decls @ composite_decls)

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
    | Mask_linear_angle angle -> build_linear_angle_style angle
    | Mask_conic_angle angle -> build_conic_angle_style angle
    | Mask_radial -> build_radial_base_style
    | Mask_radial_at pos -> build_radial_at_style pos
    | Mask_radial_shape shape -> build_radial_shape_style shape
    | Mask_radial_size size -> build_radial_size_style size
    | Mask_var_ref (dir, pos_end, _, var_name) ->
        build_var_ref_style dir pos_end var_name
    | Mask_color_ref (dir, pos_end, var_name) ->
        build_color_ref_style dir pos_end var_name

  let suborder = function
    | Mask_color_ref (dir, pos_end, _) ->
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
    | Mask_var_ref (dir, pos_end, _, _) ->
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
        dir_offset + pos_offset + 1
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
        dir_offset + pos_offset + 10
    | Mask_linear_angle _ -> 650
    | Mask_conic_angle _ -> 850
    | Mask_radial -> 750
    | Mask_radial_size (Arbitrary_size _) -> 755
    | Mask_radial_at _ -> 760
    | Mask_radial_shape Circle -> 770
    | Mask_radial_shape Ellipse -> 771
    | Mask_radial_size Closest_corner -> 780
    | Mask_radial_size Closest_side -> 781
    | Mask_radial_size Farthest_corner -> 782
    | Mask_radial_size Farthest_side -> 783

  (* Check if a float is a valid Tailwind spacing multiplier: non-negative,
     either an integer or ending in .5 *)
  let is_valid_spacing n =
    n >= 0.0 && (Float.is_integer n || Float.is_integer (n *. 2.0))

  (* Parse a value from the class suffix *)
  let parse_value suffix =
    if String.length suffix > 0 && suffix.[0] = '[' then
      (* Arbitrary value - reject negative values *)
      let len = String.length suffix in
      if len > 2 && suffix.[len - 1] = ']' then
        let inner = String.sub suffix 1 (len - 2) in
        if String.length inner > 0 && inner.[0] = '-' then Option.none
        else Option.some (Arbitrary inner)
      else Option.none
    else if String.length suffix > 0 && suffix.[String.length suffix - 1] = '%'
    then
      (* Percentage - must be non-negative integer *)
      let num_str = String.sub suffix 0 (String.length suffix - 1) in
      match int_of_string_opt num_str with
      | Some n when n >= 0 -> Option.some (Percent (Float.of_int n))
      | _ -> Option.none
    else
      (* Spacing multiplier - must be non-negative, integer or half *)
      match float_of_string_opt suffix with
      | Some n when is_valid_spacing n -> Option.some (Spacing n)
      | _ -> Option.none

  (* Parse a parenthesized var reference like "(--var)", "(length:--var)",
     "(color:--var)". Returns `Some (is_color, var_name)` or None. *)
  let parse_paren_var suffix =
    let len = String.length suffix in
    if len > 2 && suffix.[0] = '(' && suffix.[len - 1] = ')' then
      let inner = String.sub suffix 1 (len - 2) in
      if String.length inner > 7 && String.sub inner 0 6 = "color:" then
        (* (color:--var-name) → color ref *)
        let var_name = String.sub inner 6 (String.length inner - 6) in
        Some (`Color, var_name)
      else if String.length inner > 9 && String.sub inner 0 7 = "length:" then
        (* (length:--var-name) → position ref with length prefix *)
        let var_name = String.sub inner 7 (String.length inner - 7) in
        Some (`Length, var_name)
      else if String.length inner > 2 && inner.[0] = '-' && inner.[1] = '-' then
        (* (--var-name) → position ref *)
        Some (`Position, inner)
      else None
    else None

  (* Parse directional from/to with support for values and paren refs *)
  let parse_directional dir pos_end rest =
    let suffix = String.concat "-" rest in
    match parse_paren_var suffix with
    | Some (`Color, var_name) -> Ok (Mask_color_ref (dir, pos_end, var_name))
    | Some (`Length, var_name) ->
        Ok (Mask_var_ref (dir, pos_end, Length_var, var_name))
    | Some (`Position, var_name) ->
        Ok (Mask_var_ref (dir, pos_end, Plain_var, var_name))
    | None -> (
        match parse_value suffix with
        | Some value -> Ok (Mask_position (dir, pos_end, value))
        | None ->
            Error
              (`Msg
                 ("Invalid mask-" ^ direction_short dir ^ "-"
                ^ position_end_name pos_end ^ " value")))

  let of_class class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* mask-t-from-*, mask-t-to-* *)
    | "mask" :: "t" :: "from" :: rest when rest <> [] ->
        parse_directional Top From rest
    | "mask" :: "t" :: "to" :: rest when rest <> [] ->
        parse_directional Top To rest
    (* mask-r-from-*, mask-r-to-* *)
    | "mask" :: "r" :: "from" :: rest when rest <> [] ->
        parse_directional Right From rest
    | "mask" :: "r" :: "to" :: rest when rest <> [] ->
        parse_directional Right To rest
    (* mask-b-from-*, mask-b-to-* *)
    | "mask" :: "b" :: "from" :: rest when rest <> [] ->
        parse_directional Bottom From rest
    | "mask" :: "b" :: "to" :: rest when rest <> [] ->
        parse_directional Bottom To rest
    (* mask-l-from-*, mask-l-to-* *)
    | "mask" :: "l" :: "from" :: rest when rest <> [] ->
        parse_directional Left From rest
    | "mask" :: "l" :: "to" :: rest when rest <> [] ->
        parse_directional Left To rest
    (* mask-x-from-*, mask-x-to-* *)
    | "mask" :: "x" :: "from" :: rest when rest <> [] ->
        parse_directional X From rest
    | "mask" :: "x" :: "to" :: rest when rest <> [] ->
        parse_directional X To rest
    (* mask-y-from-*, mask-y-to-* *)
    | "mask" :: "y" :: "from" :: rest when rest <> [] ->
        parse_directional Y From rest
    | "mask" :: "y" :: "to" :: rest when rest <> [] ->
        parse_directional Y To rest
    (* mask-linear-from-*, mask-linear-to-* *)
    | "mask" :: "linear" :: "from" :: rest when rest <> [] ->
        parse_directional Linear From rest
    | "mask" :: "linear" :: "to" :: rest when rest <> [] ->
        parse_directional Linear To rest
    (* mask-linear-N (angle), mask-linear-[arb] *)
    | [ "mask"; "linear"; n ] -> (
        if String.length n > 2 && n.[0] = '[' && n.[String.length n - 1] = ']'
        then
          let inner = String.sub n 1 (String.length n - 2) in
          Ok (Mask_linear_angle (Angle_arb inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Mask_linear_angle (Angle_int i))
          | None -> Error (`Msg "Invalid mask-linear angle value"))
    (* -mask-linear-N (negative angle) *)
    | [ ""; "mask"; "linear"; n ] -> (
        match int_of_string_opt n with
        | Some i -> Ok (Mask_linear_angle (Angle_int (-i)))
        | None -> Error (`Msg "Invalid negative mask-linear angle value"))
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
    | "mask" :: "radial" :: "from" :: rest when rest <> [] ->
        parse_directional Radial From rest
    | "mask" :: "radial" :: "to" :: rest when rest <> [] ->
        parse_directional Radial To rest
    (* mask-conic-from-*, mask-conic-to-* *)
    | "mask" :: "conic" :: "from" :: rest when rest <> [] ->
        parse_directional Conic From rest
    | "mask" :: "conic" :: "to" :: rest when rest <> [] ->
        parse_directional Conic To rest
    (* mask-conic-N (angle), mask-conic-[arb] *)
    | [ "mask"; "conic"; n ] -> (
        if String.length n > 2 && n.[0] = '[' && n.[String.length n - 1] = ']'
        then
          let inner = String.sub n 1 (String.length n - 2) in
          Ok (Mask_conic_angle (Angle_arb inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Mask_conic_angle (Angle_int i))
          | None -> Error (`Msg "Invalid mask-conic angle value"))
    (* -mask-conic-N (negative angle) *)
    | [ ""; "mask"; "conic"; n ] -> (
        match int_of_string_opt n with
        | Some i -> Ok (Mask_conic_angle (Angle_int (-i)))
        | None -> Error (`Msg "Invalid negative mask-conic angle value"))
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
        if Float.is_integer p then pp_int (int_of_float p) ^ "%"
        else pp_float p ^ "%"
    | Arbitrary v -> "[" ^ v ^ "]"

  let to_class = function
    | Mask_position (dir, pos_end, value) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ format_value value
    | Mask_var_ref (dir, pos_end, Plain_var, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-("
        ^ var_name ^ ")"
    | Mask_var_ref (dir, pos_end, Length_var, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
        ^ "-(length:" ^ var_name ^ ")"
    | Mask_color_ref (dir, pos_end, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
        ^ "-(color:" ^ var_name ^ ")"
    | Mask_linear_angle (Angle_int n) ->
        if n < 0 then "-mask-linear-" ^ string_of_int (-n)
        else "mask-linear-" ^ string_of_int n
    | Mask_linear_angle (Angle_arb s) -> "mask-linear-[" ^ s ^ "]"
    | Mask_conic_angle (Angle_int n) ->
        if n < 0 then "-mask-conic-" ^ string_of_int (-n)
        else "mask-conic-" ^ string_of_int n
    | Mask_conic_angle (Angle_arb s) -> "mask-conic-[" ^ s ^ "]"
    | Mask_radial -> "mask-radial"
    | Mask_radial_at (At_keyword pos) ->
        "mask-radial-at-" ^ String.concat "-" (String.split_on_char ' ' pos)
    | Mask_radial_at (At_arbitrary pos) -> "mask-radial-at-[" ^ pos ^ "]"
    | Mask_radial_shape Circle -> "mask-circle"
    | Mask_radial_shape Ellipse -> "mask-ellipse"
    | Mask_radial_size Closest_corner -> "mask-radial-closest-corner"
    | Mask_radial_size Closest_side -> "mask-radial-closest-side"
    | Mask_radial_size Farthest_corner -> "mask-radial-farthest-corner"
    | Mask_radial_size Farthest_side -> "mask-radial-farthest-side"
    | Mask_radial_size (Arbitrary_size s) ->
        let escaped = String.map (fun c -> if c = ' ' then '_' else c) s in
        "mask-radial-[" ^ escaped ^ "]"
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
