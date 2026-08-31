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
    | Keyword of string (* keywords like "bottom", "top left" *)
    | Bracket of
        string (* arbitrary values like "25%" - stored without brackets *)

  type mask_angle =
    | Int of int (* mask-linear-45 → calc(1deg * 45); mask-linear-1 → 1deg *)
    | Arb of string
      (* mask-linear-[3rad] → 171.887deg, read and converted at style time *)
    | Arb_neg of string
  (* -mask-linear-[3rad] → calc(171.887deg * -1) *)

  type var_ref_kind = Plain_var | Length_var

  type t =
    | Position of direction * position_end * value
    | Var_ref of direction * position_end * var_ref_kind * string
      (* (--var) or (length:--var) → sets position to var(--var) *)
    | Color_ref of direction * position_end * string
      (* (color:--var) → sets color to var(--var) *)
    | Stop_color of direction * position_end * Color.color * int
      (* a palette entry as the stop colour *)
    | Stop_keyword of direction * position_end * Css.color * string
      (* transparent / current as the stop colour *)
    | Linear_angle of mask_angle
    | Conic_angle of mask_angle
    | Radial (* just mask-radial with no position *)
    | Radial_at of radial_at_position (* mask-radial-at-* *)
    | Radial_shape of radial_shape (* mask-circle, mask-ellipse *)
    | Radial_size of radial_size (* mask-radial-closest-corner etc. *)

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
  let radial_stops_var = Var.channel Gradient_stop "tw-mask-radial-stops"

  (* The radial gradient's own geometry, which its stops read alongside the two
     colour stops every direction owns. *)
  let radial_shape_var = Var.channel Radial_shape "tw-mask-radial-shape"
  let radial_size_var = Var.channel Radial_size "tw-mask-radial-size"
  let radial_position_var = Var.channel Position_value "tw-mask-radial-position"

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
  let default_shape : Css.radial_shape = Ellipse
  let default_size : Css.radial_size = Farthest_corner
  let default_position : position_value = Center

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

  (* The radial gradient reads its shape, size and position out of variables, so
     a shape or size utility can move one of them without restating the stops.
     [radial_geometry] is that [<ending-shape> <size> at <position>] prelude. *)
  let radial_geometry : gradient_position =
    Radial_position
      (radial_gradient_config
         ~shape:(Var (read radial_shape_var default_shape))
         ~size:(Var (read radial_size_var default_size))
         ~position:(Var (read radial_position_var default_position))
         ())

  let radial_stops : gradient_stop =
    List (Position radial_geometry :: stops radial_vars)

  let radial_stops_decls =
    let decl, stops_ref = Var.binding radial_stops_var radial_stops in
    [ decl; set radial_vars.slot (Radial_gradient_var stops_ref) ]

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

  (* @property rules for a specific direction's from/to vars. The order offset
     varies by axis so that e.g. right endpoints sort before left endpoints in
     the properties layer. A direction keeps one slot whichever utility emits
     it: [mask-l-*] and the left half of [mask-x-*] name the same four custom
     properties, so both ask for 66. *)
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
        direction_endpoint_rules ~order_base:66 "bottom";
      ]

  let left_property_rules =
    concat
      [
        common_property_rules;
        directional_gradient_property_rules;
        direction_endpoint_rules ~order_base:66 "left";
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

  (* A bracket [mask-radial-at-*] is a position value, and of_class has already
     read it as one. The keyword form is the class's own words: it admits runs
     no position value carries ([center center], [top top]), so it goes into the
     sheet as the author spelled it. *)
  let build_radial_at_style pos =
    match pos with
    | Bracket s -> (
        match radial_at_position s with
        | Some p -> style [ set radial_position_var p ]
        | None ->
            style
              [
                custom_property ~layer:"utilities"
                  (Var.css_name radial_position_var)
                  s;
              ])
    | Keyword s ->
        style
          [
            custom_property ~layer:"utilities"
              (Var.css_name radial_position_var)
              s;
          ]

  (* Build the style for mask-circle/mask-ellipse *)
  let build_radial_shape_style (shape : radial_shape) =
    let shape : Css.radial_shape =
      match shape with Circle -> Circle | Ellipse -> Ellipse
    in
    style [ set radial_shape_var shape ]

  (* [mask-radial-[...]] takes whatever the author wrote, so a size cascade can
     read becomes a typed radius and anything else goes into the sheet as
     written. *)
  let radial_size_decl s =
    let typed : Css.radial_size option =
      match String.split_on_char ' ' s with
      | [ one ] ->
          Option.map
            (fun l : Css.radial_size -> Circle_radius l)
            (Css.parse_length one)
      | [ x; y ] -> (
          match
            (arbitrary_length_percentage x, arbitrary_length_percentage y)
          with
          | Some x, Some y -> Some (Ellipse_radii (x, y))
          | _ -> None)
      | _ -> None
    in
    match typed with
    | Some size -> set radial_size_var size
    | None ->
        custom_property ~layer:"utilities" (Var.css_name radial_size_var) s

  (* Build the style for mask-radial size keywords and arbitrary sizes *)
  let build_radial_size_style (size : radial_size) =
    let size_decl =
      match size with
      | Closest_corner -> set radial_size_var Closest_corner
      | Closest_side -> set radial_size_var Closest_side
      | Farthest_corner -> set radial_size_var Farthest_corner
      | Farthest_side -> set radial_size_var Farthest_side
      | Arbitrary_size s -> radial_size_decl s
    in
    (* Arbitrary sizes get full mask setup with property_rules, keywords just
       set the variable without property rules *)
    match size with
    | Arbitrary_size _ ->
        (* With no stops of its own the gradient falls back to the bare size,
           which is the whole argument list a size utility leaves. *)
        let stops_ref =
          Var.reference_with_fallback radial_stops_var
            (Position
               (Radial_position
                  (radial_gradient_config
                     ~size:(Var (read radial_size_var default_size))
                     ())))
        in
        let common_decls =
          mask_image_decls
          @ [ set radial_vars.slot (Radial_gradient_var stops_ref); size_decl ]
        in
        style ~property_rules:radial_property_rules
          (common_decls @ composite_decls)
    | _ -> style [ size_decl ]

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

  (* A bracket angle cascade cannot read is kept as the tokens the author wrote,
     so it still reaches the sheet through a typed angle. *)
  let invalid_angle s : angle =
    Invalid (Css.Values.read_invalid_value (Cascade.Cursor.of_string s))

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
    | Int n when n >= -1 && n <= 1 -> set var (Deg (float_of_int n))
    | Int n -> set var (Calc (Expr (Val (Deg 1.0), Mul, Num (float_of_int n))))
    (* A bracket value that is not an angle still reaches the sheet, negation
       and all, because the variable takes any token stream and Tailwind writes
       the text through. On its own it is carried as the spec-invalid angle it
       is; negated it is spelled out, because a typed [calc] over an invalid
       operand loses the spaces around the operator that both tools write. *)
    | Arb s ->
        set var (Option.value (arbitrary_angle s) ~default:(invalid_angle s))
    | Arb_neg s -> (
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

  (* The keyword goes in as text because this custom property is a token stream:
     a typed [Current] folds through it to a hex, where Tailwind writes
     [currentcolor] for a gradient stop to resolve against the element. Not a
     workaround for cascade's printer, which spells the keyword correctly and
     positionally - [currentColor] bare, [currentcolor] inside a function. *)
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
    | Position (Top, pos_end, value) ->
        build_directional_style Top pos_end value
    | Position (Right, pos_end, value) ->
        build_directional_style Right pos_end value
    | Position (Bottom, pos_end, value) ->
        build_directional_style Bottom pos_end value
    | Position (Left, pos_end, value) ->
        build_directional_style Left pos_end value
    | Position (X, pos_end, value) -> build_x_style pos_end value
    | Position (Y, pos_end, value) -> build_y_style pos_end value
    | Position (Linear, pos_end, value) -> build_linear_style pos_end value
    | Position (Radial, pos_end, value) -> build_radial_style pos_end value
    | Position (Conic, pos_end, value) -> build_conic_style pos_end value
    | Linear_angle angle -> build_linear_angle_style angle
    | Conic_angle angle -> build_conic_angle_style angle
    | Radial -> build_radial_base_style
    | Radial_at pos -> build_radial_at_style pos
    | Radial_shape shape -> build_radial_shape_style shape
    | Radial_size size -> build_radial_size_style size
    | Var_ref (dir, pos_end, _, var_name) ->
        build_var_ref_style dir pos_end var_name
    | Color_ref (dir, pos_end, var_name) ->
        build_color_ref_style dir pos_end var_name
    | Stop_color (dir, pos_end, c, shade) ->
        build_stop_color_style ~theme dir pos_end c shade
    | Stop_keyword (dir, pos_end, Current, _) ->
        build_current_color_style dir pos_end
    | Stop_keyword (dir, pos_end, value, _) ->
        build_color_value_style dir pos_end value

  (* Tailwind sorts a layer's rules by the properties they set, walking the
     fixed table its property-order list gives: mask-image, then a block per
     side (--tw-mask-top, then its from-color, from-position, to-color and
     to-position stops, then the same for right, bottom and left), then the
     --tw-mask-linear, --tw-mask-radial and --tw-mask-conic blocks, then
     mask-composite. Two rules sort on the first table index that separates
     them, and one that runs out of properties sorts after one that keeps going.
     Every utility here writes mask-image and carries on, so they share its
     slot; inside it they sort by the index that separates them, spread four
     apart to leave room for the two tie-breaks below. *)
  let mask_image_rank = 209
  let first_stop_rank = 210

  let property_suborder index =
    Utility.Property_order.slot mask_image_rank + (4 * (index - first_stop_rank))

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
    | Stop_keyword (dir, pos_end, _, _) | Stop_color (dir, pos_end, _, _) ->
        stop_suborder dir pos_end ~color:true ~var:false
    | Color_ref (dir, pos_end, _) ->
        stop_suborder dir pos_end ~color:true ~var:true
    | Var_ref (dir, pos_end, _, _) ->
        stop_suborder dir pos_end ~color:false ~var:true
    | Position (dir, pos_end, _) ->
        stop_suborder dir pos_end ~color:false ~var:false
    | Linear_angle _ -> property_suborder 231
    | Radial -> property_suborder 236
    | Radial_size (Arbitrary_size _) -> property_suborder 238
    | Conic_angle _ -> property_suborder 245
    (* mask-circle, the radial size keywords and mask-radial-at-* write a
       --tw-mask-radial-* variable and no mask-image, so each closes the slot of
       the variable it names: --tw-mask-radial-shape (237),
       --tw-mask-radial-size (238) and --tw-mask-radial-position (239). The
       class name orders the ones sharing a slot. *)
    | Radial_shape _ -> Utility.Property_order.last 237
    | Radial_size _ -> Utility.Property_order.last 238
    | Radial_at _ -> Utility.Property_order.last 239

  (* Check if a float is a valid Tailwind spacing multiplier: non-negative,
     either an integer or ending in .5 *)
  let is_valid_spacing n =
    n >= 0.0 && (Float.is_integer n || Float.is_integer (n *. 2.0))

  (* Parse a value from the class suffix *)
  let parse_value suffix =
    if String.length suffix > 0 && suffix.[0] = '[' then
      (* Arbitrary value - reject negative values, and any text that would not
         stay inside the declaration it is written into. *)
      if Parse.is_bracket_value suffix then
        let inner = Parse.bracket_inner suffix in
        if String.length inner > 0 && inner.[0] = '-' then Option.none
        else if not (Parse.is_declaration_value inner) then Option.none
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
  let parse_directional ~theme dir pos_end rest =
    let suffix = String.concat "-" rest in
    match parse_paren_var suffix with
    | Some (`Color, var_name) -> Ok (Color_ref (dir, pos_end, var_name))
    | Some (`Length, var_name) ->
        Ok (Var_ref (dir, pos_end, Length_var, var_name))
    | Some (`Position, var_name) ->
        Ok (Var_ref (dir, pos_end, Plain_var, var_name))
    | None -> (
        match parse_value suffix with
        | Some value -> Ok (Position (dir, pos_end, value))
        | None -> (
            (* A stop is either a position or a colour, and the two share the
               syntax slot, so a colour name is the last thing tried. *)
            let keyword k =
              Some (Stop_keyword (dir, pos_end, k, String.concat "-" rest))
            in
            let stop =
              match rest with
              | [ "transparent" ] -> keyword Css.Transparent
              | [ "current" ] -> keyword Css.current_color
              | _ -> (
                  match Color.shade_of_strings ~theme rest with
                  | Ok (c, shade) -> Some (Stop_color (dir, pos_end, c, shade))
                  | Error _ -> None)
            in
            match stop with
            | Some stop -> Ok stop
            | None ->
                Error
                  (`Msg
                     ("Invalid mask-" ^ direction_short dir ^ "-"
                    ^ position_end_name pos_end ^ " value"))))

  let of_class theme class_name =
    let parts = Parse.split_class class_name in
    match parts with
    (* mask-t-from-*, mask-t-to-* *)
    | "mask" :: "t" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Top From rest
    | "mask" :: "t" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Top To rest
    (* mask-r-from-*, mask-r-to-* *)
    | "mask" :: "r" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Right From rest
    | "mask" :: "r" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Right To rest
    (* mask-b-from-*, mask-b-to-* *)
    | "mask" :: "b" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Bottom From rest
    | "mask" :: "b" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Bottom To rest
    (* mask-l-from-*, mask-l-to-* *)
    | "mask" :: "l" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Left From rest
    | "mask" :: "l" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Left To rest
    (* mask-x-from-*, mask-x-to-* *)
    | "mask" :: "x" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme X From rest
    | "mask" :: "x" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme X To rest
    (* mask-y-from-*, mask-y-to-* *)
    | "mask" :: "y" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Y From rest
    | "mask" :: "y" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Y To rest
    (* mask-linear-from-*, mask-linear-to-* *)
    | "mask" :: "linear" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Linear From rest
    | "mask" :: "linear" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Linear To rest
    (* mask-linear-N (angle), mask-linear-[arb] *)
    | [ "mask"; "linear"; n ] -> (
        if Parse.is_bracket_value n then
          let inner = Parse.bracket_inner n in
          Ok (Linear_angle (Arb inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Linear_angle (Int i))
          | None -> Error (`Msg "Invalid mask-linear angle value"))
    (* -mask-linear-N (negative angle), -mask-linear-[arb] *)
    | [ ""; "mask"; "linear"; n ] -> (
        if Parse.is_bracket_value n then
          let inner = Parse.bracket_inner n in
          Ok (Linear_angle (Arb_neg inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Linear_angle (Int (-i)))
          | None -> Error (`Msg "Invalid negative mask-linear angle value"))
    (* mask-radial *)
    | [ "mask"; "radial" ] -> Ok Radial
    (* mask-radial-at-* *)
    | "mask" :: "radial" :: "at" :: rest when rest <> [] ->
        let position = String.concat " " rest in
        (* Handle arbitrary values - strip brackets *)
        if Parse.is_bracket_value position then
          let inner = Parse.bracket_inner position in
          if radial_at_position inner = None then
            Error (`Msg ("Invalid mask-radial-at position: " ^ position))
          else Ok (Radial_at (Bracket inner))
        else
          (* Validate keyword positions: top, bottom, left, right, center and
             combinations *)
          let is_valid_keyword =
            List.for_all
              (fun w ->
                List.mem w [ "top"; "bottom"; "left"; "right"; "center" ])
              rest
          in
          if is_valid_keyword then Ok (Radial_at (Keyword position))
          else Error (`Msg ("Invalid mask-radial-at position: " ^ position))
    (* mask-radial-from-*, mask-radial-to-* *)
    | "mask" :: "radial" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Radial From rest
    | "mask" :: "radial" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Radial To rest
    (* mask-conic-from-*, mask-conic-to-* *)
    | "mask" :: "conic" :: "from" :: rest when rest <> [] ->
        parse_directional ~theme Conic From rest
    | "mask" :: "conic" :: "to" :: rest when rest <> [] ->
        parse_directional ~theme Conic To rest
    (* mask-conic-N (angle), mask-conic-[arb] *)
    | [ "mask"; "conic"; n ] -> (
        if Parse.is_bracket_value n then
          let inner = Parse.bracket_inner n in
          Ok (Conic_angle (Arb inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Conic_angle (Int i))
          | None -> Error (`Msg "Invalid mask-conic angle value"))
    (* -mask-conic-N (negative angle), -mask-conic-[arb] *)
    | [ ""; "mask"; "conic"; n ] -> (
        if Parse.is_bracket_value n then
          let inner = Parse.bracket_inner n in
          Ok (Conic_angle (Arb_neg inner))
        else
          match int_of_string_opt n with
          | Some i -> Ok (Conic_angle (Int (-i)))
          | None -> Error (`Msg "Invalid negative mask-conic angle value"))
    (* mask-circle, mask-ellipse *)
    | [ "mask"; "circle" ] -> Ok (Radial_shape Circle)
    | [ "mask"; "ellipse" ] -> Ok (Radial_shape Ellipse)
    (* mask-radial size keywords *)
    | [ "mask"; "radial"; "closest"; "corner" ] ->
        Ok (Radial_size Closest_corner)
    | [ "mask"; "radial"; "closest"; "side" ] -> Ok (Radial_size Closest_side)
    | [ "mask"; "radial"; "farthest"; "corner" ] ->
        Ok (Radial_size Farthest_corner)
    | [ "mask"; "radial"; "farthest"; "side" ] -> Ok (Radial_size Farthest_side)
    (* mask-radial-[size] - arbitrary size *)
    | [ "mask"; "radial"; arb ] when Parse.is_bracket_value arb ->
        let size_value = Parse.bracket_inner arb in
        (* Replace underscores with spaces *)
        let size_value =
          String.map (fun c -> if c = '_' then ' ' else c) size_value
        in
        if not (Parse.is_declaration_value size_value) then
          Error (`Msg ("Invalid mask-radial size: " ^ size_value))
        else Ok (Radial_size (Arbitrary_size size_value))
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
    | Position (dir, pos_end, value) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ format_value value
    | Var_ref (dir, pos_end, Plain_var, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-("
        ^ var_name ^ ")"
    | Var_ref (dir, pos_end, Length_var, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
        ^ "-(length:" ^ var_name ^ ")"
    | Stop_keyword (dir, pos_end, _, name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ name
    | Stop_color (dir, pos_end, c, shade) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end ^ "-"
        ^ Color.color_to_string c
        ^ if Color.is_shadeless c then "" else "-" ^ string_of_int shade
    | Color_ref (dir, pos_end, var_name) ->
        "mask-" ^ direction_short dir ^ "-" ^ position_end_name pos_end
        ^ "-(color:" ^ var_name ^ ")"
    | Linear_angle (Int n) ->
        if n < 0 then "-mask-linear-" ^ string_of_int (-n)
        else "mask-linear-" ^ string_of_int n
    | Linear_angle (Arb s) -> "mask-linear-[" ^ s ^ "]"
    | Linear_angle (Arb_neg s) -> "-mask-linear-[" ^ s ^ "]"
    | Conic_angle (Int n) ->
        if n < 0 then "-mask-conic-" ^ string_of_int (-n)
        else "mask-conic-" ^ string_of_int n
    | Conic_angle (Arb s) -> "mask-conic-[" ^ s ^ "]"
    | Conic_angle (Arb_neg s) -> "-mask-conic-[" ^ s ^ "]"
    | Radial -> "mask-radial"
    | Radial_at (Keyword pos) ->
        "mask-radial-at-" ^ String.concat "-" (String.split_on_char ' ' pos)
    | Radial_at (Bracket pos) -> "mask-radial-at-[" ^ pos ^ "]"
    | Radial_shape Circle -> "mask-circle"
    | Radial_shape Ellipse -> "mask-ellipse"
    | Radial_size Closest_corner -> "mask-radial-closest-corner"
    | Radial_size Closest_side -> "mask-radial-closest-side"
    | Radial_size Farthest_corner -> "mask-radial-farthest-corner"
    | Radial_size Farthest_side -> "mask-radial-farthest-side"
    | Radial_size (Arbitrary_size s) ->
        let escaped = String.map (fun c -> if c = ' ' then '_' else c) s in
        "mask-radial-[" ^ escaped ^ "]"

  let examples = [ Linear_angle (Int 0) ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v

(* Convenience functions for creating mask gradient utilities *)
let mask_t_from value = utility (Position (Top, From, value))
let mask_t_to value = utility (Position (Top, To, value))
let mask_r_from value = utility (Position (Right, From, value))
let mask_r_to value = utility (Position (Right, To, value))
let mask_b_from value = utility (Position (Bottom, From, value))
let mask_b_to value = utility (Position (Bottom, To, value))
let mask_l_from value = utility (Position (Left, From, value))
let mask_l_to value = utility (Position (Left, To, value))
let mask_x_from value = utility (Position (X, From, value))
let mask_x_to value = utility (Position (X, To, value))
let mask_y_from value = utility (Position (Y, From, value))
let mask_y_to value = utility (Position (Y, To, value))
let mask_linear_from value = utility (Position (Linear, From, value))
let mask_linear_to value = utility (Position (Linear, To, value))
let mask_radial = utility Radial
let mask_radial_at pos = utility (Radial_at pos)
let mask_radial_from value = utility (Position (Radial, From, value))
let mask_radial_to value = utility (Position (Radial, To, value))
let mask_conic_from value = utility (Position (Conic, From, value))
let mask_conic_to value = utility (Position (Conic, To, value))
