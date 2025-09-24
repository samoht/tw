(* Typed CSS custom properties (variables) - Simplified API

   This module provides the core extensible variable system for CSS custom
   properties following the simplified design from todo/vars.md *)

(* Layer classification for CSS variables *)
type layer = Theme | Utility

(* Variable definition - the main currency *)
type 'a property_info = { initial : 'a; inherits : bool }

type 'a t = {
  kind : 'a Css.kind; (* CSS type witness *)
  name : string; (* Variable name without -- prefix *)
  layer : layer; (* Theme or Utility *)
  binding : ?fallback:'a -> 'a -> Css.declaration * 'a Css.var;
      (* Function to create declaration and var ref *)
  property : 'a property_info option; (* For @property registration *)
  order : int option; (* Explicit ordering for theme layer *)
}

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

let layer_name = function Theme -> "theme" | Utility -> "utilities"

(* Convert initial value to Universal syntax for @property *)
let initial_to_universal : type a. a Css.kind -> a -> string =
 fun kind initial ->
  match kind with
  | Css.Length -> (
      let open Css in
      match initial with
      | Zero -> "0px"
      | Px f when f = 0. -> "0px"
      | _ -> Css.Pp.to_string (pp_length ~always:true) initial)
  | Css.Color -> Css.Pp.to_string Css.pp_color initial
  | Css.Angle -> Css.Pp.to_string Css.pp_angle initial
  | Css.Duration -> Css.Pp.to_string Css.pp_duration initial
  | Css.Float -> Pp.float initial ^ "%"
  | Css.Int -> string_of_int initial
  | Css.String -> initial
  | Css.Shadow -> "0 0 #0000"
  | _ -> "initial" (* Fallback *)

(* Create a variable template *)
let create : type a.
    a Css.kind ->
    ?property:a * bool ->
    ?order:int ->
    string ->
    layer:layer ->
    a t =
 fun kind ?property ?order name ~layer ->
  (* Ensure theme variables have an order *)
  (match (layer, order) with
  | Theme, None ->
      failwith ("Variable '" ^ name ^ "' in theme layer must have an order")
  | _ -> ());

  let prop_info_opt =
    match property with
    | None -> None
    | Some (initial, inherits) -> Some { initial; inherits }
  in
  let info = Info { kind; name; need_property = property <> None; order } in
  let binding ?fallback value =
    let meta = meta_of_info info in
    let layer_name = Some (layer_name layer) in
    let fallback = Option.map (fun f -> Css.Fallback f) fallback in
    Css.var ~default:value ?fallback ?layer:layer_name ~meta name kind value
  in
  { kind; name; layer; binding; property = prop_info_opt; order }

(* Get @property rule if metadata present *)
let property_rule : type a. a t -> Css.t option =
 fun var ->
  match var.property with
  | None -> None
  | Some { initial; inherits } ->
      let name = "--" ^ var.name in
      let initial_str = initial_to_universal var.kind initial in
      Some
        (Css.property ~name Css.Universal ~inherits ~initial_value:initial_str
           ())

(* Create a binding: returns both declaration and a context-aware var
   reference *)
let binding var ?fallback value = var.binding ?fallback value

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
