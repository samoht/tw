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
  order : int option; (* Explicit ordering for theme layer *)
  fallback : 'a option; (* Built-in fallback for ref_only variables *)
}

(* Type shortcuts for common patterns *)
type 'a theme = ('a, [ `Theme ]) t
type 'a property_default = ('a, [ `Property_default ]) t
type 'a channel = ('a, [ `Channel ]) t
type 'a ref_only = ('a, [ `Ref_only ]) t

type info =
  | Info : {
      name : string;
      kind : 'a Css.kind;
      need_property : bool;
      order : int option;
    }
      -> info

let (meta_of_info : info -> Css.meta), (info_of_meta : Css.meta -> info option)
    =
  Css.meta ()

let layer_name = function (Theme : layer) -> "theme" | Utility -> "utilities"

(* Convert initial value to Universal syntax for @property *)
let initial_to_universal : type a. a Css.kind -> a -> string =
 fun kind initial ->
  match kind with
  | Css.Length -> Css.Pp.to_string (Css.pp_length ~always:false) initial
  | Css.Color -> Css.Pp.to_string Css.pp_color initial
  | Css.Angle -> Css.Pp.to_string Css.pp_angle initial
  | Css.Duration -> Css.Pp.to_string Css.pp_duration initial
  | Css.Float -> Pp.float initial ^ "%"
  | Css.Int -> string_of_int initial
  | Css.String -> initial
  | Css.Font_weight -> Css.Pp.to_string Css.pp_font_weight initial
  | Css.Shadow -> "0 0 #0000"
  | Css.Border_style -> Css.Pp.to_string Css.pp_border_style initial
  | _ -> "initial" (* Fallback *)

(* Create a variable template *)
let create : type a r.
    a Css.kind ->
    ?property:a property_info ->
    ?order:int ->
    ?fallback:a ->
    role:r role ->
    string ->
    layer:layer ->
    (a, r) t =
 fun kind ?property ?order ?fallback ~role name ~layer ->
  (* Ensure theme variables have an order *)
  (match (layer, order) with
  | Theme, None ->
      failwith ("Variable '" ^ name ^ "' in theme layer must have an order")
  | _ -> ());

  let info = Info { kind; name; need_property = property <> None; order } in
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
  { kind; name; layer; role; binding; property; order; fallback }

(* Convenience constructors to encode patterns safely *)

let theme kind name ~order =
  create kind ?property:None ?order:(Some order) ~role:Theme name ~layer:Theme

let property_default kind ~initial ?(inherits = false) ?(universal = false) name
    =
  let property = property_info ~initial ~inherits ~universal () in
  create kind ~property ~role:Property_default name ~layer:Utility

let channel ?(needs_property = false) kind name =
  if needs_property then
    (* Channels that need @property for animations/transitions *)
    let property =
      property_info ?initial:None ~inherits:false ~universal:true ()
    in
    create kind ~property ~role:Channel name ~layer:Utility
  else create kind ~role:Channel name ~layer:Utility

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
  (* Force universal syntax if requested *)
  if universal then
    match initial with
    | None -> property ~name Universal ~inherits ()
    | Some v -> (
        (* Special case: empty lists for Gradient_stops should not have
           initial-value *)
        match (kind, v) with
        | Gradient_stop, List [] -> property ~name Universal ~inherits ()
        | _ ->
            let initial_str = initial_to_universal kind v in
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
        property ~name Length_percentage ~initial_value:(Percentage v) ~inherits
          ()
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

let ref_only kind name ~fallback =
  (* Create a utility variable that's only referenced, never set *)
  create kind ~fallback ~role:Ref_only name ~layer:Utility

(* Property info to declaration value conversion *)
let property_info_to_declaration_value (Css.Property_info info) =
  match info.initial_value with
  | None -> "initial"
  | Some v -> (
      let open Css.Variables in
      let open Css.Values in
      match info.syntax with
      | Length -> (
          match v with
          | Zero -> "0px"
          | Px f when f = 0. -> "0px"
          | _ -> Css.Pp.to_string (pp_length ~always:true) v)
      | Number -> Pp.float v ^ "%"
      | syntax -> Css.Pp.to_string (pp_value syntax) v)

let var_needs_property v =
  match Css.var_meta v with
  | None -> failwith ("raw CSS variable: " ^ Css.var_name v)
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
  Css.of_statements filtered
