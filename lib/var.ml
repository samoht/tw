module Css = Cascade.Css
(* Typed CSS custom properties (variables) - Simplified API

   This module provides the core extensible variable system for CSS custom
   properties following the simplified design documented in var.mli *)

(* Layer classification for CSS variables *)
type layer = Theme | Utility

(* Registry for custom properties-layer initial CSS values. Some variables need
   a different string in the properties layer than what pp_length produces
   (e.g., ring-offset-width needs "0px" in properties but "0" in @property). *)
(* The properties layer and the [@property] rule want different spellings of one
   registered zero, and Tailwind emits both: [--tw-ring-offset-width: 0px] in
   the layer, [initial-value: 0] in the rule. The layer's spelling is stated
   here rather than carried on the variable, because the layer is built from
   parsed [@property] statements whose only handle is the name. A total
   function, so nothing depends on definition order. *)
let properties_layer_override = function
  | "--tw-ring-offset-width" -> Some "0px"
  | _ -> None

(* Variable definition - the main currency *)
type 'a property_info = {
  initial : 'a option;
  inherits : bool;
  universal : bool;
}

let property_info ?initial ?(inherits = false) ?(universal = false) () =
  { initial; inherits; universal }

type _ role =
  | Theme : [ `Theme ] role
  | Property_default : [ `Property_default ] role
  | Channel : [ `Channel ] role
  | Ref_only : [ `Ref_only ] role

type ('a, 'r) t = {
  kind : 'a Css.kind; (* CSS type witness *)
  name : string; (* Variable name without -- prefix *)
  role : 'r role; (* The GADT role/pattern of this variable *)
  binding : ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var;
      (* Function to create declaration and var ref *)
  property : 'a property_info option; (* For @property registration *)
  fallback : 'a option; (* Built-in fallback for ref_only variables *)
}

(* Type shortcuts for common patterns *)
type 'a theme = ('a, [ `Theme ]) t
type 'a property_default = ('a, [ `Property_default ]) t
type 'a channel = ('a, [ `Channel ]) t
type 'a ref_only = ('a, [ `Ref_only ]) t

(* Global registry of the metadata the layer builders look up by variable name,
   filled by [v] as each variable definition is evaluated. *)
module Registry = struct
  (* Table mapping variable_name -> (priority, suborder) *)
  let name_registry : (string, int * int) Hashtbl.t = Hashtbl.create 128

  (* Table mapping variable_name -> property_order for @supports block
     ordering *)
  let property_order_registry : (string, int) Hashtbl.t = Hashtbl.create 128

  (* Families for ordering/grouping without string prefixes *)
  type family =
    [ `Border
    | `Rotate
    | `Skew
    | `Scale
    | `Translate
    | `Gradient
    | `Shadow
    | `Inset_shadow
    | `Ring
    | `Inset_ring
    | `Leading
    | `Font_weight
    | `Duration
    | `Tracking
    | `Content
    | `Text_shadow
    | `Filter
    | `Drop_shadow
    | `Backdrop_filter ]

  let family_registry : (string, family) Hashtbl.t = Hashtbl.create 128
  let needs_property_registry : (string, bool) Hashtbl.t = Hashtbl.create 128

  (* A slot is shared, not owned: each token family numbers its members from its
     own base, so several families sit at the same (priority, suborder) — e.g.
     --radius-4xl, --drop-shadow-md and --ease-out are all (7, 10). The families
     a project [@theme] names (--font-<name>, --animate-<name>, --inset-<name>)
     funnel every member into one slot by design. The theme layer breaks a tie
     on the variable name, so a shared slot is still a total order.

     A name, on the other hand, owns its slot: it is one custom property and one
     position in the theme layer. The definition that first claims a name fixes
     that position, and a definition reaching a name that is already placed is
     answered with the established slot. That is how a project naming one of the
     built-in tokens lands: [--font-sans] from a project [@theme] arrives
     through the [--font-<name>] family's shared slot, and keeps (1, 0). *)
  let register_variable ~name ~order =
    match Hashtbl.find_opt name_registry name with
    | Some established -> established
    | None ->
        Hashtbl.replace name_registry name order;
        order

  (* A name owns its family and its [@property] need: each is a fact about one
     custom property, not a slot several families share. A second registration
     that agrees restates the fact; one that disagrees is a constant copied by
     hand that has drifted, so it is refused rather than letting module
     initialisation order decide the properties layer. *)
  let register_once tbl ~what ~pp ~name ~value =
    match Hashtbl.find_opt tbl name with
    | Some established when established <> value ->
        invalid_arg
          (Pp.str
             [
               "--";
               name;
               ": ";
               what;
               " is ";
               pp established;
               ", registered again as ";
               pp value;
             ])
    | Some _ -> ()
    | None -> Hashtbl.replace tbl name value

  let register_property_order ~name ~order =
    register_once property_order_registry ~what:"property order" ~pp:Pp.int
      ~name ~value:order

  let property_order name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt property_order_registry name

  let order name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt name_registry name

  let family_name : family -> string = function
    | `Border -> "Border"
    | `Rotate -> "Rotate"
    | `Skew -> "Skew"
    | `Scale -> "Scale"
    | `Translate -> "Translate"
    | `Gradient -> "Gradient"
    | `Shadow -> "Shadow"
    | `Inset_shadow -> "Inset_shadow"
    | `Ring -> "Ring"
    | `Inset_ring -> "Inset_ring"
    | `Leading -> "Leading"
    | `Font_weight -> "Font_weight"
    | `Duration -> "Duration"
    | `Tracking -> "Tracking"
    | `Content -> "Content"
    | `Text_shadow -> "Text_shadow"
    | `Filter -> "Filter"
    | `Drop_shadow -> "Drop_shadow"
    | `Backdrop_filter -> "Backdrop_filter"

  let register_family ~name ~family =
    register_once family_registry ~what:"family" ~pp:family_name ~name
      ~value:family

  let family name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt family_registry name

  let register_needs_property ~name ~needs =
    register_once needs_property_registry ~what:"@property need" ~pp:Pp.bool
      ~name ~value:needs

  let needs_property name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    match Hashtbl.find_opt needs_property_registry name with
    | Some b -> b
    | None -> false
end

(* Get property order for a variable name (for external use in build.ml) *)
let property_order = Registry.property_order
let register_property_order = Registry.register_property_order
let order = Registry.order
let family = Registry.family
let needs_property = Registry.needs_property

type family = Registry.family

type info =
  | Info : {
      name : string;
      kind : 'a Css.kind;
      need_property : bool;
      order : (int * int) option;
    }
      -> info

let (meta_of_info : info -> Css.meta), (info_of_meta : Css.meta -> info option)
    =
  Css.meta ()

let layer_name = function (Theme : layer) -> "theme" | Utility -> "utilities"

(* Convert a [Css.kind] witness to the matching [Css.Properties.kind]. *)
let properties_kind_of_kind : type a. a Css.kind -> a Css.Properties.kind =
  let open Css in
  function
  | Length -> Css.Properties.Length
  | Color -> Css.Properties.Color
  | Rgb -> Css.Properties.Rgb
  | Int -> Css.Properties.Int
  | Number -> Css.Properties.Number
  | Float -> Css.Properties.Float
  | Percentage -> Css.Properties.Percentage
  | Length_percentage -> Css.Properties.Length_percentage
  | Number_percentage -> Css.Properties.Number_percentage
  | Opacity -> Css.Properties.Opacity
  | Value -> Css.Properties.Value
  | Duration -> Css.Properties.Duration
  | Aspect_ratio -> Css.Properties.Aspect_ratio
  | Border_style -> Css.Properties.Border_style
  | Outline_style -> Css.Properties.Outline_style
  | Border -> Css.Properties.Border
  | Font_weight -> Css.Properties.Font_weight
  | Font_size -> Css.Properties.Font_size
  | Line_height -> Css.Properties.Line_height
  | Font_family -> Css.Properties.Font_family
  | Font_feature_settings -> Css.Properties.Font_feature_settings
  | Font_variation_settings -> Css.Properties.Font_variation_settings
  | Numeric -> Css.Properties.Numeric
  | Font_variant_numeric_token -> Css.Properties.Font_variant_numeric_token
  | Blend_mode -> Css.Properties.Blend_mode
  | Scroll_snap_strictness -> Css.Properties.Scroll_snap_strictness
  | Angle -> Css.Properties.Angle
  | Rotate -> Css.Properties.Rotate
  | Scale -> Css.Properties.Scale
  | Shadow -> Css.Properties.Shadow
  | Content -> Css.Properties.Content
  | Gradient_stop -> Css.Properties.Gradient_stop
  | Gradient_direction -> Css.Properties.Gradient_direction
  | Gradient_position -> Css.Properties.Gradient_position
  | Radial_shape -> Css.Properties.Radial_shape
  | Radial_size -> Css.Properties.Radial_size
  | Position_value -> Css.Properties.Position_value
  | Animation -> Css.Properties.Animation
  | Timing_function -> Css.Properties.Timing_function
  | Transform -> Css.Properties.Transform
  | Touch_action -> Css.Properties.Touch_action
  | Transition_property_value -> Css.Properties.Transition_property_value
  | Background_image -> Css.Properties.Background_image
  | Z_index -> Css.Properties.Z_index
  | Filter -> Css.Properties.Filter
  | Font_src -> Css.Properties.Font_src

(* Create a variable template *)
let v : type a r.
    a Css.kind ->
    ?property:a property_info ->
    ?order:int * int ->
    ?property_order:int ->
    ?family:family ->
    ?fallback:a ->
    ?runtime:bool ->
    role:r role ->
    string ->
    layer:layer ->
    (a, r) t =
 fun kind ?property ?order ?property_order ?family ?fallback ?runtime ~role name
     ~layer ->
  (* Ensure theme variables have an order *)
  (match (layer, order) with
  | Theme, None ->
      failwith ("Variable '" ^ name ^ "' in theme layer must have an order")
  | _ -> ());

  (* Claim the name's theme slot, and carry the slot the name settled on: a
     declaration this variable binds sorts where the name sorts. *)
  let order =
    match order with
    | Some ord -> Some (Registry.register_variable ~name ~order:ord)
    | None -> None
  in

  (* Register property order if provided *)
  (match property_order with
  | Some ord -> Registry.register_property_order ~name ~order:ord
  | None -> ());

  (match family with
  | Some fam -> Registry.register_family ~name ~family:fam
  | None -> ());

  (* Register needs_property so we can look it up by name *)
  let need_property = property <> None in
  if need_property then Registry.register_needs_property ~name ~needs:true;

  let info = Info { kind; name; need_property; order } in
  let binding ?fallback:fb value =
    let meta = meta_of_info info in
    let layer_name = Some (layer_name layer) in
    (* For ref_only, always use built-in fallback *)
    let actual_fallback =
      match ((role : r role), fallback, fb) with
      | Ref_only, Some f, _ ->
          Css.Fallback f (* Always use built-in fallback for ref_only *)
      | _, _, Some f -> f (* Use provided fallback *)
      | _ -> Css.None (* No fallback *)
    in
    Css.var ~default:value ~fallback:actual_fallback ?layer:layer_name ~meta
      ?runtime name kind value
  in
  { kind; name; role; binding; property; fallback }

(* Convenience constructors to encode patterns safely *)

let theme kind ?runtime name ~order =
  v kind ?property:None ?order:(Some order) ?runtime ~role:Theme name
    ~layer:Theme

let property_default kind ~initial ?(inherits = false) ?(universal = false)
    ?property_order ?family name =
  let property = property_info ~initial ~inherits ~universal () in
  v kind ~property ?property_order ?family ~role:Property_default name
    ~layer:Utility

let channel ?(needs_property = false) ?property_order ?family kind name =
  if needs_property then
    (* Channels that need @property for animations/transitions *)
    let property =
      property_info ?initial:None ~inherits:false ~universal:true ()
    in
    v kind ~property ?property_order ?family ~role:Channel name ~layer:Utility
  else v kind ?property_order ?family ~role:Channel name ~layer:Utility

let string_of_kind_value : type a. a Css.kind -> a -> string =
 fun kind value ->
  match kind with
  | Css.Color -> Css.Pp.to_string ~minify:true Css.Values.pp_color value
  | Css.Gradient_position ->
      Css.Pp.to_string ~minify:true Css.Properties.pp_gradient_position value
  | _ ->
      Css.Properties.string_of_kind_value (properties_kind_of_kind kind) value

(* Helper to create @property with correct syntax based on kind *)
let property_universal : type a.
    name:string -> a Css.kind -> a option -> inherits:bool -> Css.t =
 fun ~name kind initial ~inherits ->
  let open Css in
  match initial with
  | None -> property ~name Universal ~inherits ()
  | Some v -> (
      match (kind, v) with
      | Gradient_stop, (List [] : Css.gradient_stop) ->
          property ~name Universal ~inherits ()
      | Percentage, (Pct 0. : Css.percentage) ->
          property ~name Universal ~inherits ()
      | Gradient_direction, (To_bottom : Css.gradient_direction) ->
          property ~name Universal ~inherits ()
      | Gradient_position, (Linear_position To_bottom : Css.gradient_position)
        ->
          property ~name Universal ~inherits ()
      | _ ->
          let initial_str = string_of_kind_value kind v in
          property ~name Universal ~initial_value:initial_str ~inherits ())

let property_typed : type a.
    name:string -> a Css.kind -> a option -> inherits:bool -> Css.t =
 fun ~name kind initial ~inherits ->
  let open Css in
  match (kind, initial) with
  | Length, None -> property ~name Universal ~inherits ()
  | Length, Some v -> property ~name Length ~initial_value:v ~inherits ()
  | Float, None -> property ~name Percentage ~inherits ()
  | Float, Some v ->
      property ~name Percentage ~initial_value:(Pct v) ~inherits ()
  | Color, None -> property ~name Universal ~inherits ()
  | Color, Some v -> property ~name Color ~initial_value:v ~inherits ()
  | Percentage, None -> property ~name Percentage ~inherits ()
  | Percentage, Some v ->
      property ~name Percentage ~initial_value:v ~inherits ()
  | Length_percentage, None -> property ~name Length_percentage ~inherits ()
  | Length_percentage, Some v ->
      property ~name Length_percentage ~initial_value:v ~inherits ()
  | Number_percentage, None -> property ~name Universal ~inherits ()
  | Number_percentage, Some v ->
      let initial_str = string_of_kind_value kind v in
      property ~name Universal ~initial_value:initial_str ~inherits ()
  | Gradient_stop, None -> property ~name Universal ~inherits ()
  | Gradient_stop, Some (List [] : Css.gradient_stop) ->
      property ~name Universal ~inherits ()
  | Gradient_stop, Some v ->
      property ~name Universal
        ~initial_value:(string_of_kind_value kind v)
        ~inherits ()
  | _, None -> property ~name Universal ~inherits ()
  | _, Some v ->
      property ~name Universal
        ~initial_value:(string_of_kind_value kind v)
        ~inherits ()

let property ~name kind initial ~inherits ~universal =
  if universal then property_universal ~name kind initial ~inherits
  else property_typed ~name kind initial ~inherits

(* Get @property rule if metadata present *)
let property_rule : type a r. (a, r) t -> Css.t option =
 fun var ->
  match var.role with
  | Property_default | Channel -> (
      match var.property with
      | None -> None
      | Some { initial; inherits; universal; _ } ->
          let name = "--" ^ var.name in
          Some (property ~name var.kind initial ~inherits ~universal))
  | _ -> None (* Other roles don't generate @property rules *)

(* Convenience function for property_default variables to get property rules or
   empty *)
let property_rules : type a. (a, [< `Property_default ]) t -> Css.t =
 fun var ->
  match property_rule var with
  | None ->
      (* This should never happen for property_default variables *)
      failwith
        ("property_default variable '" ^ var.name
       ^ "' is missing property metadata. This is a bug in the variable \
          definition - property_default variables must always have property \
          metadata with an initial value.")
  | Some r -> r

(* Create a binding: returns both declaration and a context-aware var
   reference *)
let binding var ?fallback value = var.binding ?fallback value

(* Reset a channel to the CSS-wide [initial] keyword ([--tw-x: initial]). Used
   by the [*-initial] / [via-none] utilities, which clear a channel var rather
   than set it to a typed value. *)
let binding_initial var =
  Css.custom_property ~layer:"utilities" ("--" ^ var.name) "initial"

(* Create a variable reference for variables with @property defaults OR
   fallback *)
let reference : type a b. (a, b) t -> a Css.var =
 fun var ->
  match var.role with
  | Ref_only -> (
      (* ref_only variables must have a built-in fallback *)
      match var.fallback with
      | None -> failwith ("ref_only variable " ^ var.name ^ " missing fallback")
      | Some fb_value ->
          let _, var_ref = var.binding fb_value in
          var_ref)
  | Property_default -> (
      (* property_default variables should have initial value in property *)
      match var.property with
      | None ->
          failwith
            ("property_default variable " ^ var.name ^ " missing property")
      | Some { initial; _ } -> (
          match initial with
          | None ->
              failwith
                ("property_default variable " ^ var.name
               ^ " missing initial value")
          | Some initial_value ->
              let _, var_ref = var.binding initial_value in
              var_ref))
  | Theme | Channel -> assert false

let reference_with_fallback : type a b. (a, b) t -> a -> a Css.var =
 fun var fallback_value ->
  match var.role with
  | Theme | Channel ->
      let _, var_ref =
        var.binding ~fallback:(Css.Fallback fallback_value) fallback_value
      in
      var_ref
  | Property_default | Ref_only -> assert false

(* Reference a channel variable with an empty fallback. Produces: var(--name,) -
   empty fallback means unset variables contribute nothing. Used for optional
   transform components. *)
let reference_with_empty_fallback : type a. (a, [< `Channel ]) t -> a Css.var =
 fun var -> Css.var_ref ~fallback:Css.Empty var.name

(* Reference a channel variable with a var fallback to a theme variable
   Produces: var(--channel, var(--theme-fallback)) IMPORTANT: This creates a
   reference WITHOUT property metadata to avoid generating @property rules when
   the variable is only referenced, not set. *)
let reference_with_var_fallback : type a.
    (a, [< `Channel ]) t -> (a, [< `Theme ]) t -> a -> a Css.var =
 fun channel_var theme_var _dummy_value ->
  let fallback_name = theme_var.name in
  (* Create a var reference directly without going through binding, to avoid
     adding property metadata that would trigger @property rules *)
  Css.var_ref ~fallback:(Css.Var_fallback fallback_name) channel_var.name

let ref_only kind name ~fallback =
  (* Create a utility variable that's only referenced, never set *)
  v kind ~fallback ~role:Ref_only name ~layer:Utility

(* Registry for theme_ref variables: maps var name -> default CSS string *)
let theme_ref_registry : (string, string) Hashtbl.t = Hashtbl.create 64

let theme_ref : type a. ?default:a -> ?default_css:string -> string -> a Css.var
    =
 fun ?default ?default_css name ->
  (match default_css with
  | Some css -> Hashtbl.replace theme_ref_registry name css
  | None -> ());
  Css.var_ref ~layer:"theme" ?default name

let resolve_theme_refs name = Hashtbl.find_opt theme_ref_registry name

(* Convert Property_info to the string value for properties layer This extracts
   the initial value and converts it to the appropriate CSS string *)
let property_info_to_declaration_value (Css.Property_info info) =
  match properties_layer_override info.name with
  | Some css -> css
  | None -> (
      match info.initial_value with
      | None -> "initial"
      | Some v -> (
          let open Css.Variables in
          match info.syntax with
          | Universal -> v (* Universal syntax already stores strings *)
          | _ -> (
              let (* For non-Universal syntax, we shouldn't be in the properties
                     layer but handle it gracefully using the existing pp
                     functions *)
                open
                Css.Values
              in
              match info.syntax with
              | Length -> (
                  match v with
                  | Zero -> "0"
                  | _ -> Css.Pp.to_string (pp_length ~always:true) v)
              (* A [<number>] initial value is a number: appending [%] made it a
                 percentage, which is a different type. Unreachable today
                 because [property_typed] emits no Number syntax, so this is the
                 arm a new one would land on. *)
              | Number -> Pp.float v
              | syntax -> Css.Pp.to_string (pp_value syntax) v)))

let name var = var.name
let css_name var = "--" ^ var.name

let needs_property_rule v =
  match Css.var_meta v with
  | None ->
      (* Variables without metadata (e.g., raw theme variable references like
         --animate-pulse) don't need @property rules *)
      false
  | Some meta -> (
      match info_of_meta meta with
      | Some (Info i) -> i.need_property
      | None -> assert false)

let order_of_declaration decl =
  match Css.meta_of_declaration decl with
  | None -> None
  | Some meta -> (
      match info_of_meta meta with Some (Info t) -> t.order | None -> None)

let property_initial_string = property_info_to_declaration_value
let pp v = Pp.str [ "Var(--"; v.name; ")" ]

let bracket ?fallback name =
  let parsed_reference =
    match fallback with
    | Some _ -> None
    | None ->
        let expression =
          if String.starts_with ~prefix:"var(" name then Some name
          else if String.contains name ',' then Some ("var(--" ^ name ^ ")")
          else None
        in
        Option.bind expression (fun expression ->
            try
              Some
                (Css.Variables.read_reference
                   (Cascade.Cursor.of_string expression))
            with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> None)
  in
  match parsed_reference with
  | Some (name, Some fallback) ->
      Css.var_ref ~runtime:true
        ~fallback:(Css.Values.syntax_fallback fallback)
        name
  | Some (name, None) -> Css.var_ref ~runtime:true name
  | None -> Css.var_ref ?fallback name
