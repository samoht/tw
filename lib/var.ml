(* Typed CSS custom properties (variables) - Simplified API

   This module provides the core extensible variable system for CSS custom
   properties following the simplified design from todo/vars.md *)

(* Layer classification for CSS variables *)
type layer = Theme | Utility

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
  layer : layer; (* Theme or Utility *)
  role : 'r role; (* The GADT role/pattern of this variable *)
  binding : ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var;
      (* Function to create declaration and var ref *)
  property : 'a property_info option; (* For @property registration *)
  order : (int * int) option;
      (* Explicit ordering for theme layer (utility_priority, suborder) *)
  property_order : int option;
      (* Explicit ordering for properties layer @supports block *)
  fallback : 'a option; (* Built-in fallback for ref_only variables *)
}

(* Type shortcuts for common patterns *)
type 'a theme = ('a, [ `Theme ]) t
type 'a property_default = ('a, [ `Property_default ]) t
type 'a channel = ('a, [ `Channel ]) t
type 'a ref_only = ('a, [ `Ref_only ]) t

(* Global registry to prevent order and name conflicts *)
module Registry = struct
  (* Table mapping (priority, suborder) -> variable_name *)
  let order_registry : (int * int, string) Hashtbl.t = Hashtbl.create 128

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
    | `Tracking ]

  let family_registry : (string, family) Hashtbl.t = Hashtbl.create 128
  let needs_property_registry : (string, bool) Hashtbl.t = Hashtbl.create 128

  let register_variable ~name ~order =
    (* Check for order conflicts *)
    (match Hashtbl.find_opt order_registry order with
    | Some existing_name when existing_name <> name ->
        let order_str =
          match order with
          | p, s -> "(" ^ string_of_int p ^ ", " ^ string_of_int s ^ ")"
        in
        failwith
          ("Variable order conflict: order " ^ order_str
         ^ " is already used by variable '" ^ existing_name
         ^ "', cannot assign to '" ^ name ^ "'")
    | _ -> ());

    (* Check for name conflicts - fail if name already exists regardless of
       order *)
    (match Hashtbl.find_opt name_registry name with
    | Some existing_order ->
        let order_str =
          match existing_order with
          | p, s -> "(" ^ string_of_int p ^ ", " ^ string_of_int s ^ ")"
        in
        failwith
          ("Variable name conflict: variable '" ^ name
         ^ "' is already registered with order " ^ order_str
         ^ ", cannot create duplicate")
    | None -> ());

    (* Register the variable *)
    Hashtbl.replace order_registry order name;
    Hashtbl.replace name_registry name order

  let register_property_order ~name ~order =
    Hashtbl.replace property_order_registry name order

  let get_property_order name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt property_order_registry name

  let get_order name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt name_registry name

  let register_family ~name ~family =
    Hashtbl.replace family_registry name family

  let get_family name =
    (* Strip leading -- if present *)
    let name =
      if String.starts_with ~prefix:"--" name then
        String.sub name 2 (String.length name - 2)
      else name
    in
    Hashtbl.find_opt family_registry name

  let register_needs_property ~name ~needs =
    Hashtbl.replace needs_property_registry name needs

  let get_needs_property name =
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

(* Get property order for a variable name (for external use in rules.ml) *)
let get_property_order = Registry.get_property_order
let get_order = Registry.get_order
let get_family = Registry.get_family
let get_needs_property = Registry.get_needs_property

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

(* Helper to serialize number_percentage values for universal syntax *)
let number_percentage_to_string (np : Css.number_percentage) =
  match np with Num f | Pct f -> Pp.float f | _ -> "initial"

let channel_to_string : Css.channel -> string = function
  | Css.Int i -> string_of_int i
  | Css.Num f -> Pp.float f
  | Css.Pct p -> Pp.float p ^ "%"
  | Css.Var _ -> "0"

(* Single source of truth for converting typed values to CSS strings This
   function converts a typed value to its CSS string representation for use in
   both @property initial-value and properties layer *)
let value_to_css_string : type a. a Css.kind -> a -> string =
 fun kind value ->
  match kind with
  | Css.Length -> Css.Pp.to_string (Css.pp_length ~always:false) value
  | Css.Color -> Css.Pp.to_string Css.pp_color value
  | Css.Angle -> Css.Pp.to_string Css.pp_angle value
  | Css.Duration -> Css.Pp.to_string Css.pp_duration value
  | Css.Float -> Pp.float value
  | Css.Percentage -> ( match value with Pct f -> Pp.float f | _ -> "initial")
  | Css.Number_percentage -> number_percentage_to_string value
  | Css.Int -> string_of_int value
  | Css.String -> if value = "" then "\"\"" else value
  | Css.Content -> (
      match value with
      | Css.String "" -> "\"\""
      | Css.String s -> "\"" ^ s ^ "\""
      | Css.None -> "none"
      | Css.Normal -> "normal"
      | Css.Open_quote -> "open-quote"
      | Css.Close_quote -> "close-quote"
      | Css.Var _ -> "initial")
  | Css.Font_weight -> Css.Pp.to_string Css.pp_font_weight value
  | Css.Shadow -> "0 0 #0000"
  | Css.Border_style -> Css.Pp.to_string Css.pp_border_style value
  | Css.Scroll_snap_strictness ->
      Css.Pp.to_string Css.pp_scroll_snap_strictness value
  | Css.Rgb -> (
      match value with
      | Css.Channels { r; g; b } ->
          channel_to_string r ^ " " ^ channel_to_string g ^ " "
          ^ channel_to_string b
      | Css.Var _ -> "initial")
  | Css.Animation -> Css.Pp.to_string Css.pp_animation value
  | Css.Gradient_direction -> Css.Pp.to_string Css.pp_gradient_direction value
  | _ -> "initial"

(* Alias for backward compatibility *)
let initial_to_universal = value_to_css_string

(* Create a variable template *)
let create : type a r.
    a Css.kind ->
    ?property:a property_info ->
    ?order:int * int ->
    ?property_order:int ->
    ?family:family ->
    ?fallback:a ->
    role:r role ->
    string ->
    layer:layer ->
    (a, r) t =
 fun kind ?property ?order ?property_order ?family ?fallback ~role name
     ~layer ->
  (* Ensure theme variables have an order *)
  (match (layer, order) with
  | Theme, None ->
      failwith ("Variable '" ^ name ^ "' in theme layer must have an order")
  | _ -> ());

  (* Register the variable in the global registry to prevent conflicts *)
  (match order with
  | Some ord -> Registry.register_variable ~name ~order:ord
  | None -> ());

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
      name kind value
  in
  {
    kind;
    name;
    layer;
    role;
    binding;
    property;
    order;
    property_order;
    fallback;
  }

(* Convenience constructors to encode patterns safely *)

let theme kind name ~order =
  create kind ?property:None ?order:(Some order) ~role:Theme name ~layer:Theme

let property_default kind ~initial ?(inherits = false) ?(universal = false)
    ?property_order ?family name =
  let property = property_info ~initial ~inherits ~universal () in
  create kind ~property ?property_order ?family ~role:Property_default name
    ~layer:Utility

let channel ?(needs_property = false) ?property_order ?family kind name =
  if needs_property then
    (* Channels that need @property for animations/transitions *)
    let property =
      property_info ?initial:None ~inherits:false ~universal:true ()
    in
    create kind ~property ?property_order ?family ~role:Channel name
      ~layer:Utility
  else create kind ?property_order ?family ~role:Channel name ~layer:Utility

(* Place after [reference] to avoid forward reference issues *)

(* Helper to create @property with correct syntax based on kind *)
let create_property : type a.
    name:string ->
    a Css.kind ->
    a option ->
    inherits:bool ->
    universal:bool ->
    Css.t =
 fun ~name kind initial ~inherits ~universal ->
  let open Css in
  if universal then
    match initial with
    | None -> property ~name Universal ~inherits ()
    | Some v -> (
        match (kind, v) with
        | Gradient_stop, List [] -> property ~name Universal ~inherits ()
        | Percentage, Pct 0. -> property ~name Universal ~inherits ()
        | Gradient_direction, To_bottom -> property ~name Universal ~inherits ()
        | _ ->
            let initial_str = value_to_css_string kind v in
            property ~name Universal ~initial_value:initial_str ~inherits ())
  else
    match (kind, initial) with
    (* Length - use length-percentage syntax when there's a value *)
    | Length, None -> property ~name Universal ~inherits ()
    | Length, Some v -> property ~name Length ~initial_value:v ~inherits ()
    (* Float as Percentage *)
    | Float, None -> property ~name Percentage ~inherits ()
    | Float, Some v ->
        property ~name Percentage ~initial_value:(Pct v) ~inherits ()
    (* Color - use universal syntax when no initial value, color syntax
       otherwise *)
    | Color, None -> property ~name Universal ~inherits ()
    | Color, Some v -> property ~name Color ~initial_value:v ~inherits ()
    (* Percentage - use length-percentage syntax *)
    | Percentage, None -> property ~name Length_percentage ~inherits ()
    | Percentage, Some v ->
        property ~name Length_percentage
          ~initial_value:(Pct (match v with Pct f -> f | _ -> 0.0))
          ~inherits ()
    (* Number_percentage - use universal syntax *)
    | Number_percentage, None -> property ~name Universal ~inherits ()
    | Number_percentage, Some v ->
        let initial_str = number_percentage_to_string v in
        property ~name Universal ~initial_value:initial_str ~inherits ()
    (* Gradient_stops - special handling *)
    | Gradient_stop, None -> property ~name Universal ~inherits ()
    | Gradient_stop, Some (List []) ->
        (* Empty list means no initial-value, just like None *)
        property ~name Universal ~inherits ()
    | Gradient_stop, Some v ->
        let initial_str = initial_to_universal kind v in
        property ~name Universal ~initial_value:initial_str ~inherits ()
    (* Everything else - use Universal syntax *)
    | _, None -> property ~name Universal ~inherits ()
    | _, Some v ->
        let initial_str = initial_to_universal kind v in
        property ~name Universal ~initial_value:initial_str ~inherits ()

(* Get @property rule if metadata present *)
let property_rule : type a r. (a, r) t -> Css.t option =
 fun var ->
  match var.role with
  | Property_default | Channel -> (
      match var.property with
      | None -> None
      | Some { initial; inherits; universal } ->
          let name = "--" ^ var.name in
          Some (create_property ~name var.kind initial ~inherits ~universal))
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
  create kind ~fallback ~role:Ref_only name ~layer:Utility

(* Convert Property_info to the string value for properties layer This extracts
   the initial value and converts it to the appropriate CSS string *)
let property_info_to_declaration_value (Css.Property_info info) =
  match info.initial_value with
  | None -> "initial"
  | Some v -> (
      let open Css.Variables in
      match info.syntax with
      | Universal -> v (* Universal syntax already stores strings *)
      | _ -> (
          let
          (* For non-Universal syntax, we shouldn't be in the properties layer
             but handle it gracefully using the existing pp functions *)
          open
            Css.Values in
          match info.syntax with
          | Length -> (
              match v with
              | Zero -> "0px"
              | Px f when f = 0. -> "0px"
              | _ -> Css.Pp.to_string (pp_length ~always:true) v)
          | Number -> Pp.float v ^ "%"
          | syntax -> Css.Pp.to_string (pp_value syntax) v))

let css_name var = "--" ^ var.name

let var_needs_property v =
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

(* Heterogeneous collections *)
type any_var = Any : ('a, 'r) t -> any_var

let properties vars =
  (* Collect statements from property rules and deduplicate by name *)
  let stmts =
    vars
    |> List.filter_map (fun (Any v) -> property_rule v)
    |> List.concat_map Css.statements
  in
  let seen = Hashtbl.create 32 in
  let filtered =
    List.filter
      (fun stmt ->
        match Css.as_property stmt with
        | Some (Css.Property_info info) ->
            if Hashtbl.mem seen info.name then false
            else (
              Hashtbl.add seen info.name ();
              true)
        | None -> true)
      stmts
  in
  Css.v filtered

let pp ppf v = Format.fprintf ppf "Var(--%s)" v.name
