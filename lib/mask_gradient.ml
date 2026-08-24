(** Mask gradient utilities for creating gradient masks.

    Provides utilities for mask-t-from-*, mask-t-to-*, mask-r-from-*,
    mask-r-to-*, mask-b-from-*, mask-b-to-*, mask-l-from-*, mask-l-to-*,
    mask-x-from-*, mask-x-to-*, mask-y-from-*, mask-y-to-*, mask-linear-from-*,
    mask-linear-to-*, mask-radial-*, mask-radial-from-*, mask-radial-to-*,
    mask-conic-from-*, mask-conic-to-*. *)

module Css = Cascade.Css

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
    | Angle_int of
        int (* mask-linear-45 → calc(1deg * 45); mask-linear-1 → 1deg *)
    | Angle_arb of string
      (* mask-linear-[3rad] → 171.887deg, read and converted at style time *)
    | Angle_arb_neg of string
  (* -mask-linear-[3rad] → calc(171.887deg * -1) *)

  type var_ref_kind = Plain_var | Length_var

  type t =
    | Mask_position of direction * position_end * value
    | Mask_var_ref of direction * position_end * var_ref_kind * string
      (* (--var) or (length:--var) → sets position to var(--var) *)
    | Mask_color_ref of direction * position_end * string
      (* (color:--var) → sets color to var(--var) *)
    | Mask_stop_color of direction * position_end * Color.color * int
      (* a palette entry as the stop colour *)
    | Mask_stop_keyword of direction * position_end * Css.color * string
      (* transparent / current as the stop colour *)
    | Mask_linear_angle of mask_angle
    | Mask_conic_angle of mask_angle
    | Mask_radial (* just mask-radial with no position *)
    | Mask_radial_at of radial_at_position (* mask-radial-at-* *)
    | Mask_radial_shape of radial_shape (* mask-circle, mask-ellipse *)
    | Mask_radial_size of radial_size (* mask-radial-closest-corner etc. *)

  type Utility.base += Self of t

  let name = "mask_gradient"

  (* Tailwind emits the mask-gradient utilities after the backgrounds and before
     the other masks, which come before fill/stroke and padding. *)
  let priority _ = 21

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

  (* A mask direction owns five variables: the slot holding the gradient it
     contributes to [mask-image], and the colour and position of each of that
     gradient's two stops. The names are written out rather than assembled, so
     every one of them is a value the compiler checks. *)
  type stop_vars = {
    slot : Css.background_image Var.channel;
    from_position : Css.length_percentage Var.channel;
    to_position : Css.length_percentage Var.channel;
    from_color : Css.color Var.channel;
    to_color : Css.color Var.channel;
  }

  let stop_vars ~slot ~from_position ~to_position ~from_color ~to_color =
    {
      slot = Var.channel Background_image slot;
      from_position = Var.channel Length_percentage from_position;
      to_position = Var.channel Length_percentage to_position;
      from_color = Var.channel Color from_color;
      to_color = Var.channel Color to_color;
    }

  let top_vars =
    stop_vars ~slot:"tw-mask-top" ~from_position:"tw-mask-top-from-position"
      ~to_position:"tw-mask-top-to-position"
      ~from_color:"tw-mask-top-from-color" ~to_color:"tw-mask-top-to-color"

  let right_vars =
    stop_vars ~slot:"tw-mask-right" ~from_position:"tw-mask-right-from-position"
      ~to_position:"tw-mask-right-to-position"
      ~from_color:"tw-mask-right-from-color" ~to_color:"tw-mask-right-to-color"

  let bottom_vars =
    stop_vars ~slot:"tw-mask-bottom"
      ~from_position:"tw-mask-bottom-from-position"
      ~to_position:"tw-mask-bottom-to-position"
      ~from_color:"tw-mask-bottom-from-color"
      ~to_color:"tw-mask-bottom-to-color"

  let left_vars =
    stop_vars ~slot:"tw-mask-left" ~from_position:"tw-mask-left-from-position"
      ~to_position:"tw-mask-left-to-position"
      ~from_color:"tw-mask-left-from-color" ~to_color:"tw-mask-left-to-color"

  let linear_vars =
    stop_vars ~slot:"tw-mask-linear"
      ~from_position:"tw-mask-linear-from-position"
      ~to_position:"tw-mask-linear-to-position"
      ~from_color:"tw-mask-linear-from-color"
      ~to_color:"tw-mask-linear-to-color"

  let radial_vars =
    stop_vars ~slot:"tw-mask-radial"
      ~from_position:"tw-mask-radial-from-position"
      ~to_position:"tw-mask-radial-to-position"
      ~from_color:"tw-mask-radial-from-color"
      ~to_color:"tw-mask-radial-to-color"

  let conic_vars =
    stop_vars ~slot:"tw-mask-conic" ~from_position:"tw-mask-conic-from-position"
      ~to_position:"tw-mask-conic-to-position"
      ~from_color:"tw-mask-conic-from-color" ~to_color:"tw-mask-conic-to-color"

  (* [mask-x] and [mask-y] write both edges of their axis, so they own no
     variables themselves. *)
  let vars_for = function
    | Top -> top_vars
    | Right -> right_vars
    | Bottom -> bottom_vars
    | Left -> left_vars
    | Linear -> linear_vars
    | Radial -> radial_vars
    | Conic -> conic_vars
    | (X | Y) as d ->
        invalid_arg ("mask-" ^ direction_short d ^ " has no variables")

  (* Everything between a gradient's parentheses, so a stop utility can replace
     one stop without restating the others. *)
  let linear_stops_var = Var.channel Gradient_stop "tw-mask-linear-stops"
  let conic_stops_var = Var.channel Gradient_stop "tw-mask-conic-stops"

  (* The angle [mask-linear-45] and [mask-conic-45] turn their gradient by. *)
  let linear_position_var = Var.channel Angle "tw-mask-linear-position"
  let conic_position_var = Var.channel Angle "tw-mask-conic-position"

  (* The [@property] initial values. A utility reads variables it does not set
     itself, so the initial is both what the browser starts from and what the
     inline renderer puts in the reference's place. *)
  let white_gradient : background_image =
    Linear_gradient
      ( Default_direction,
        [
          Color_percentage (hex "#fff", None, None);
          Color_percentage (hex "#fff", None, None);
        ] )

  let start_position : length_percentage = Pct 0.
  let end_position : length_percentage = Pct 100.
  let start_color : Css.color = Named Black
  let end_color : Css.color = Transparent
  let no_rotation : angle = Deg 0.

  (* Reading a mask variable. The [@property] rule gives it an initial value, so
     the reference needs no fallback of its own. *)
  let read v initial = snd (Var.binding v initial)

  (* Writing one. *)
  let set v value = fst (Var.binding v value)

  (* The two stops a mask gradient runs between. *)
  let stops v : gradient_stop list =
    [
      Color_percentage
        ( Var (read v.from_color start_color),
          Some (Var (read v.from_position start_position)),
          None );
      Color_percentage
        ( Var (read v.to_color end_color),
          Some (Var (read v.to_position end_position)),
          None );
    ]

  let edge_of : direction -> gradient_direction * stop_vars = function
    | Top -> (To_top, top_vars)
    | Right -> (To_right, right_vars)
    | Bottom -> (To_bottom, bottom_vars)
    | Left -> (To_left, left_vars)
    | (X | Y | Linear | Radial | Conic) as d ->
        invalid_arg ("mask-" ^ direction_short d ^ " is not an edge")

  (* [--tw-mask-<edge>] runs its two stops towards that edge. *)
  let edge_gradient dir =
    let side, v = edge_of dir in
    set v.slot (Linear_gradient (side, stops v))

  (* A directional utility layers the four edges into [--tw-mask-linear]. The
     edges it did not touch still contribute their initial white gradient, which
     masks nothing. *)
  let edge_composition =
    set linear_vars.slot
      (List
         [
           Var (read left_vars.slot white_gradient);
           Var (read right_vars.slot white_gradient);
           Var (read bottom_vars.slot white_gradient);
           Var (read top_vars.slot white_gradient);
         ])

  let linear_stops : gradient_stop =
    List
      (Direction (Angle (Var (read linear_position_var no_rotation)))
      :: stops linear_vars)

  let conic_stops : gradient_stop =
    List
      (Position
         (Conic_position
            (conic_gradient_config
               ~angle:(Var (read conic_position_var no_rotation))
               ()))
      :: stops conic_vars)

  let linear_stops_decls =
    let decl, stops_ref = Var.binding linear_stops_var linear_stops in
    [ decl; set linear_vars.slot (Linear_gradient_var stops_ref) ]

  let conic_stops_decls =
    let decl, stops_ref = Var.binding conic_stops_var conic_stops in
    [ decl; set conic_vars.slot (Conic_gradient_var stops_ref) ]

  (* [--tw-mask-radial-shape], [--tw-mask-radial-size] and
     [--tw-mask-radial-position] have no {!Css.kind}, so cascade types neither a
     declaration that sets one nor a reference that reads one. The radial stops
     read all three, so this family stays a token stream. *)
  let radial_stops_decl =
    "var(--tw-mask-radial-shape) var(--tw-mask-radial-size) at \
     var(--tw-mask-radial-position), var(--tw-mask-radial-from-color) \
     var(--tw-mask-radial-from-position), var(--tw-mask-radial-to-color) \
     var(--tw-mask-radial-to-position)"

  let radial_gradient_decl = "radial-gradient(var(--tw-mask-radial-stops))"

  let radial_stops_decls =
    [
      custom_property ~layer:"utilities" "--tw-mask-radial-stops"
        radial_stops_decl;
      custom_property ~layer:"utilities" "--tw-mask-radial" radial_gradient_decl;
    ]

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

  (* A stop is a <length-percentage>. A bracket value cascade cannot read goes
     into the sheet as the author wrote it. *)
  let position_var v = function From -> v.from_position | To -> v.to_position
  let color_var v = function From -> v.from_color | To -> v.to_color

  let arbitrary_length_percentage s : length_percentage option =
    match
      Cascade.Cursor.try_parse_full_err Css.Values.read_length_percentage
        (Cascade.Cursor.of_string s)
    with
    | Ok lp -> Some lp
    | Error _ -> None

  let position_decl ?theme v pos_end value =
    let var = position_var v pos_end in
    match value with
    | Spacing 0. ->
        (* The zero spacing step keeps its unit ([0px]) here, where a bare [0]
           would be the folded length. *)
        set var (Length (Px 0.))
    | Spacing n ->
        let _, len = Theme.spacing_calc_float ?theme n in
        set var (Length len)
    | Percent p -> set var (Pct p)
    | Arbitrary raw -> (
        match arbitrary_length_percentage raw with
        | Some lp -> set var lp
        | None -> custom_property ~layer:"utilities" (Var.css_name var) raw)

  (* Build the style for a directional mask position *)
  let build_directional_style ?theme dir pos_end value =
    let property_rules = property_rules_for_direction dir in

    (* Common declarations for all directional masks *)
    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          edge_composition;
          edge_gradient dir;
          position_decl ?theme (vars_for dir) pos_end value;
        ]
    in
    style ~property_rules (common_decls @ composite_decls)

  (* Build the style for mask-x (both left and right) *)
  let build_x_style ?theme pos_end value =
    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          edge_composition;
          (* Right group first, then left group — interleaved order *)
          edge_gradient Right;
          position_decl ?theme right_vars pos_end value;
          edge_gradient Left;
          position_decl ?theme left_vars pos_end value;
        ]
    in
    style ~property_rules:x_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-y (both top and bottom) *)
  let build_y_style ?theme pos_end value =
    let common_decls =
      spacing_theme_decl value @ mask_image_decls
      @ [
          edge_composition;
          (* Top group first, then bottom group — interleaved order *)
          edge_gradient Top;
          position_decl ?theme top_vars pos_end value;
          edge_gradient Bottom;
          position_decl ?theme bottom_vars pos_end value;
        ]
    in
    style ~property_rules:y_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-linear (generic linear gradient) *)
  let build_linear_style ?theme pos_end value =
    let common_decls =
      spacing_theme_decl value @ mask_image_decls @ linear_stops_decls
      @ [ position_decl ?theme linear_vars pos_end value ]
    in
    style ~property_rules:linear_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-radial position *)
  let build_radial_style ?theme pos_end value =
    let common_decls =
      spacing_theme_decl value @ mask_image_decls @ radial_stops_decls
      @ [ position_decl ?theme radial_vars pos_end value ]
    in
    style ~property_rules:radial_property_rules (common_decls @ composite_decls)

  (* Build the style for mask-radial (no position) - produces no output in
     Tailwind *)
  let build_radial_base_style = style []

  (* An arbitrary [mask-radial-at-[30%_30%]] is a position value, so it goes
     through the value printer rather than reaching the sheet as written. *)
  let radial_at_position raw : Css.position_value option =
    match
      String.split_on_char ' ' (Parse.decode_arbitrary_value raw)
      |> List.filter (fun s -> s <> "")
    with
    | [ x; y ] -> (
        match (Css.parse_length x, Css.parse_length y) with
        | Some xv, Some yv -> Some (XY (xv, yv))
        | _ -> None)
    | [ v ] ->
        Option.map
          (fun (l : Css.length) : Css.position_value -> Single l)
          (Css.parse_length v)
    | _ -> None

  (* Build the style for mask-radial-at-* - only sets the position variable *)
  let build_radial_at_style pos =
    let position_str =
      match pos with
      | At_keyword s -> s
      | At_arbitrary s -> (
          match radial_at_position s with
          | Some p -> Cascade.Pp.to_string Css.Properties.pp_position_value p
          | None -> s)
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
    (* Arbitrary sizes get full mask setup with property_rules, keywords just
       set the variable without property rules *)
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
        style
          [
            custom_property ~layer:"utilities" "--tw-mask-radial-size" size_str;
          ]

  (* Build the style for mask-conic position *)
  let build_conic_style ?theme pos_end value =
    let common_decls =
      spacing_theme_decl value @ mask_image_decls @ conic_stops_decls
      @ [ position_decl ?theme conic_vars pos_end value ]
    in
    style ~property_rules:conic_property_rules (common_decls @ composite_decls)

  (* Tailwind writes an arbitrary radian or turn angle into the position
     variable as degrees; every other unit goes in as the author spelled it. *)
  let to_degrees : angle -> angle =
    let deg n : angle = Deg (Float.round (n *. 1000.0) /. 1000.0) in
    function
    | Rad n -> deg (n *. 180.0 /. Float.pi)
    | Turn n -> deg (n *. 360.0)
    | a -> a

  let arbitrary_angle s : angle option =
    match
      Cascade.Cursor.try_parse_full_err Css.Values.read_angle
        (Cascade.Cursor.of_string s)
    with
    | Ok a -> Some (to_degrees a)
    | Error _ -> None

  (* [--tw-mask-*-position] is an opaque [syntax: "*"] custom property, so the
     value reproduces Tailwind's own output: a bare degree for the trivial
     multipliers -1/0/1, and [calc(1deg * n)] otherwise. *)
  let angle_position_decl var angle =
    let negated a : angle = Calc (Expr (Val a, Mul, Num (-1.0))) in
    match angle with
    | Angle_int n when n >= -1 && n <= 1 -> set var (Deg (float_of_int n))
    | Angle_int n ->
        set var (Calc (Expr (Val (Deg 1.0), Mul, Num (float_of_int n))))
    (* A bracket value that is not an angle still reaches the sheet, negation
       and all, because the variable takes any token stream and Tailwind writes
       the text through. *)
    | Angle_arb s -> (
        match arbitrary_angle s with
        | Some a -> set var a
        | None -> custom_property ~layer:"utilities" (Var.css_name var) s)
    | Angle_arb_neg s -> (
        match arbitrary_angle s with
        | Some a -> set var (negated a)
        | None ->
            custom_property ~layer:"utilities" (Var.css_name var)
              ("calc(" ^ s ^ " * -1)"))

  (* The angle shorthands take the whole gradient from the position variable
     unless a stop utility has filled the stops variable in. *)
  let build_linear_angle_style angle =
    let stops_ref =
      Var.reference_with_fallback linear_stops_var
        (Direction (Angle (Var (read linear_position_var no_rotation))))
    in
    let decls =
      mask_image_decls
      @ [
          set linear_vars.slot (Linear_gradient_var stops_ref);
          angle_position_decl linear_position_var angle;
        ]
    in
    style ~property_rules:linear_property_rules (decls @ composite_decls)

  let build_conic_angle_style angle =
    let stops_ref =
      Var.reference_with_fallback conic_stops_var
        (Direction (Angle (Var (read conic_position_var no_rotation))))
    in
    let decls =
      mask_image_decls
      @ [
          set conic_vars.slot (Conic_gradient_var stops_ref);
          angle_position_decl conic_position_var angle;
        ]
    in
    style ~property_rules:conic_property_rules (decls @ composite_decls)

  (* [(--x)] keeps the leading dashes the class was written with; the reference
     takes the bare name. *)
  let bare_var_name name =
    if String.length name > 2 && name.[0] = '-' && name.[1] = '-' then
      String.sub name 2 (String.length name - 2)
    else name

  (* A stop utility restates the gradient it belongs to, then writes its own end
     of it. *)
  let stop_decls dir stop =
    let dir_decls =
      match dir with
      | X ->
          [ edge_gradient Right; stop right_vars ]
          @ [ edge_gradient Left; stop left_vars ]
      | Y ->
          [ edge_gradient Top; stop top_vars ]
          @ [ edge_gradient Bottom; stop bottom_vars ]
      | Linear -> linear_stops_decls @ [ stop linear_vars ]
      | Radial -> radial_stops_decls @ [ stop radial_vars ]
      | Conic -> conic_stops_decls @ [ stop conic_vars ]
      | Top | Right | Bottom | Left ->
          [ edge_gradient dir; stop (vars_for dir) ]
    in
    (* Linear, radial and conic own the whole mask-image slot, so only the four
       edges have to layer themselves into [--tw-mask-linear]. *)
    let linear_decl =
      match dir with
      | Linear | Radial | Conic -> []
      | Top | Right | Bottom | Left | X | Y -> [ edge_composition ]
    in
    mask_image_decls @ linear_decl @ dir_decls

  (* Build the style for parenthesized var reference setting position *)
  let build_var_ref_style dir pos_end var_name =
    let property_rules = property_rules_for_direction dir in
    let merge_key =
      "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
      ^ "-var-position"
    in
    let position : length_percentage =
      Var (Var.bracket (bare_var_name var_name))
    in
    let stop v = set (position_var v pos_end) position in
    style ~merge_key ~property_rules (stop_decls dir stop @ composite_decls)

  (* The stop colour: [var(--color-black)] for a palette entry, the keyword
     itself for [transparent] / [current]. *)
  let build_color_value_style dir pos_end value =
    let property_rules = property_rules_for_direction dir in
    let stop v = set (color_var v pos_end) value in
    style ~property_rules (stop_decls dir stop @ composite_decls)

  (* A custom property spells the keyword [currentcolor], which is what cascade
     folds a token stream to and what Tailwind writes; the typed colour prints
     as the [currentColor] a colour property takes, so this one keyword goes in
     as text. *)
  let build_current_color_style dir pos_end =
    let property_rules = property_rules_for_direction dir in
    let stop v =
      custom_property ~layer:"utilities"
        (Var.css_name (color_var v pos_end))
        "currentcolor"
    in
    style ~property_rules (stop_decls dir stop @ composite_decls)

  (* A named stop colour points at its palette token, and carries the token's
     own theme declaration along so the sheet defines what it references. *)
  let build_stop_color_style ?theme dir pos_end c shade =
    let decl, token = Color.Handler.color_binding ?theme c shade in
    match build_color_value_style dir pos_end (Var token : Css.color) with
    | Style.Style st -> Style.Style { st with props = decl :: st.props }
    | other -> other

  let build_color_ref_style dir pos_end var_name =
    build_color_value_style dir pos_end
      (Var (Var.bracket (bare_var_name var_name)) : Css.color)

  let to_style theme =
    let build_directional_style dir pos_end value =
      build_directional_style ~theme dir pos_end value
    in
    let build_x_style pos_end value = build_x_style ~theme pos_end value in
    let build_y_style pos_end value = build_y_style ~theme pos_end value in
    let build_linear_style pos_end value =
      build_linear_style ~theme pos_end value
    in
    let build_radial_style pos_end value =
      build_radial_style ~theme pos_end value
    in
    let build_conic_style pos_end value =
      build_conic_style ~theme pos_end value
    in
    function
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
    | Mask_stop_color (dir, pos_end, c, shade) ->
        build_stop_color_style ~theme dir pos_end c shade
    | Mask_stop_keyword (dir, pos_end, Current, _) ->
        build_current_color_style dir pos_end
    | Mask_stop_keyword (dir, pos_end, value, _) ->
        build_color_value_style dir pos_end value

  (* Tailwind sorts a layer's rules by the properties they set, walking the
     fixed table its property-order list gives: mask-image, then a block per
     side (--tw-mask-top, then its from-color, from-position, to-color and
     to-position stops, then the same for right, bottom and left), then the
     --tw-mask-linear, --tw-mask-radial and --tw-mask-conic blocks, then
     mask-composite. Two rules sort on the first table index that separates
     them, and one that runs out of properties sorts after one that keeps going.
     These suborders are those indices, spread four apart to leave room for the
     two tie-breaks below. *)
  let property_suborder index = 4 * index

  (* mask-x-* and mask-y-* write the opposite side too, so they lead the
     mask-r-* and mask-t-* they share a first property with. Tailwind breaks the
     remaining ties on the class name, where the (--var) spellings lead the
     lengths and percentages. *)
  let stop_suborder dir pos_end ~color ~var =
    let block =
      match dir with
      | Top | Y -> 210
      | Right | X -> 215
      | Bottom -> 220
      | Left -> 225
      | Linear -> 231
      | Radial -> 239
      | Conic -> 245
    in
    let stop =
      match (pos_end, color) with
      | From, true -> 1
      | From, false -> 2
      | To, true -> 3
      | To, false -> 4
    in
    let sides = match dir with X | Y -> 0 | _ -> 2 in
    property_suborder (block + stop) + sides + if var then 0 else 1

  let suborder = function
    | Mask_stop_keyword (dir, pos_end, _, _)
    | Mask_stop_color (dir, pos_end, _, _) ->
        stop_suborder dir pos_end ~color:true ~var:false
    | Mask_color_ref (dir, pos_end, _) ->
        stop_suborder dir pos_end ~color:true ~var:true
    | Mask_var_ref (dir, pos_end, _, _) ->
        stop_suborder dir pos_end ~color:false ~var:true
    | Mask_position (dir, pos_end, _) ->
        stop_suborder dir pos_end ~color:false ~var:false
    | Mask_linear_angle _ -> property_suborder 231
    | Mask_radial -> property_suborder 236
    | Mask_radial_size (Arbitrary_size _) -> property_suborder 238
    | Mask_conic_angle _ -> property_suborder 245
    (* mask-circle, the radial size keywords and mask-radial-at-* set a
       --tw-mask-radial-* variable and no mask-image, so they trail every
       mask-image utility, [Masks]'s mask-none and mask-[<image>] forms at 10100
       included, and still lead its mask-composite band at 10200. *)
    | Mask_radial_shape Circle -> 10150
    | Mask_radial_shape Ellipse -> 10151
    | Mask_radial_size Closest_corner -> 10160
    | Mask_radial_size Closest_side -> 10161
    | Mask_radial_size Farthest_corner -> 10162
    | Mask_radial_size Farthest_side -> 10163
    | Mask_radial_at _ -> 10170

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
        | None -> (
            (* A stop is either a position or a colour, and the two share the
               syntax slot, so a colour name is the last thing tried. *)
            let keyword k =
              Some (Mask_stop_keyword (dir, pos_end, k, String.concat "-" rest))
            in
            let stop =
              match rest with
              | [ "transparent" ] -> keyword Css.Transparent
              | [ "current" ] -> keyword Css.current_color
              | _ -> (
                  match Color.shade_of_strings rest with
                  | Ok (c, shade) ->
                      Some (Mask_stop_color (dir, pos_end, c, shade))
                  | Error _ -> None)
            in
            match stop with
            | Some stop -> Ok stop
            | None ->
                Error
                  (`Msg
                     ("Invalid mask-" ^ direction_short dir ^ "-"
                    ^ position_end_name pos_end ^ " value"))))

  let of_class _theme class_name =
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
    (* -mask-linear-N (negative angle), -mask-linear-[arb] *)
    | [ ""; "mask"; "linear"; n ] -> (
        if String.length n > 2 && n.[0] = '[' && n.[String.length n - 1] = ']'
        then
          let inner = String.sub n 1 (String.length n - 2) in
          Ok (Mask_linear_angle (Angle_arb_neg inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Mask_linear_angle (Angle_int (-i)))
          | None -> Error (`Msg "Invalid negative mask-linear angle value"))
    (* mask-radial *)
    | [ "mask"; "radial" ] -> Ok Mask_radial
    (* mask-radial-at-* *)
    | "mask" :: "radial" :: "at" :: rest when rest <> [] ->
        let position = String.concat " " rest in
        (* Handle arbitrary values - strip brackets *)
        if
          String.length position > 2
          && position.[0] = '['
          && position.[String.length position - 1] = ']'
        then
          let inner = String.sub position 1 (String.length position - 2) in
          if radial_at_position inner = None then
            Error (`Msg ("Invalid mask-radial-at position: " ^ position))
          else Ok (Mask_radial_at (At_arbitrary inner))
        else
          (* Validate keyword positions: top, bottom, left, right, center and
             combinations *)
          let is_valid_keyword =
            List.for_all
              (fun w ->
                List.mem w [ "top"; "bottom"; "left"; "right"; "center" ])
              rest
          in
          if is_valid_keyword then Ok (Mask_radial_at (At_keyword position))
          else Error (`Msg ("Invalid mask-radial-at position: " ^ position))
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
    (* -mask-conic-N (negative angle), -mask-conic-[arb] *)
    | [ ""; "mask"; "conic"; n ] -> (
        if String.length n > 2 && n.[0] = '[' && n.[String.length n - 1] = ']'
        then
          let inner = String.sub n 1 (String.length n - 2) in
          Ok (Mask_conic_angle (Angle_arb_neg inner))
        else
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
    | Mask_stop_keyword (dir, pos_end, _, name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ name
    | Mask_stop_color (dir, pos_end, c, shade) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ Color.color_to_string c
        ^ if Color.is_shadeless c then "" else "-" ^ string_of_int shade
    | Mask_color_ref (dir, pos_end, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
        ^ "-(color:" ^ var_name ^ ")"
    | Mask_linear_angle (Angle_int n) ->
        if n < 0 then "-mask-linear-" ^ string_of_int (-n)
        else "mask-linear-" ^ string_of_int n
    | Mask_linear_angle (Angle_arb s) -> "mask-linear-[" ^ s ^ "]"
    | Mask_linear_angle (Angle_arb_neg s) -> "-mask-linear-[" ^ s ^ "]"
    | Mask_conic_angle (Angle_int n) ->
        if n < 0 then "-mask-conic-" ^ string_of_int (-n)
        else "mask-conic-" ^ string_of_int n
    | Mask_conic_angle (Angle_arb s) -> "mask-conic-[" ^ s ^ "]"
    | Mask_conic_angle (Angle_arb_neg s) -> "-mask-conic-[" ^ s ^ "]"
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

  let examples = [ Mask_linear_angle (Angle_int 0) ]
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
