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

type 'a t = {
  kind : 'a Css.kind; (* CSS type witness *)
  name : string; (* Variable name without -- prefix *)
  layer : layer; (* Theme or Utility *)
  binding : ?fallback:'a Css.fallback -> 'a -> Css.declaration * 'a Css.var;
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
      | Zero -> "0"
      | Px f when f = 0. -> "0"
      | _ -> Css.Pp.to_string (pp_length ~always:true) initial)
  | Css.Color -> Css.Pp.to_string Css.pp_color initial
  | Css.Angle -> Css.Pp.to_string Css.pp_angle initial
  | Css.Duration -> Css.Pp.to_string Css.pp_duration initial
  | Css.Float -> Pp.float initial ^ "%"
  | Css.Int -> string_of_int initial
  | Css.String -> initial
  | Css.Shadow -> "0 0 #0000"
  | Css.Border_style -> Css.Pp.to_string Css.pp_border_style initial
  | _ -> "initial" (* Fallback *)

(* Create a variable template *)
let create : type a.
    a Css.kind ->
    ?property:a property_info ->
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

  let info = Info { kind; name; need_property = property <> None; order } in
  let binding ?fallback value =
    let meta = meta_of_info info in
    let layer_name = Some (layer_name layer) in
    (* Accept fallback as 'a Css.fallback directly *)
    Css.var ~default:value ?fallback ?layer:layer_name ~meta name kind value
  in
  { kind; name; layer; binding; property; order }

(* Convenience constructors to encode patterns safely *)

let theme kind name ~order =
  create kind ?property:None ?order:(Some order) name ~layer:Theme

let property_default kind ?initial ?(inherits = false) ?(universal = false) name
    =
  let property = property_info ?initial ~inherits ~universal () in
  create kind ~property name ~layer:Utility

let channel kind name = create kind name ~layer:Utility

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
        | Gradient_stops, [] -> property ~name Universal ~inherits ()
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
    | Gradient_stops, None -> property ~name Universal ~inherits ()
    | Gradient_stops, Some [] ->
        (* Empty list means no initial-value, just like None *)
        property ~name Universal ~inherits ()
    | Gradient_stops, Some v ->
        let initial_str = initial_to_universal kind v in
        property ~name Universal ~initial_value:initial_str ~inherits ()
    (* Everything else - use Universal syntax *)
    | _, None -> property ~name Universal ~inherits ()
    | _, Some v ->
        let initial_str = initial_to_universal kind v in
        property ~name Universal ~initial_value:initial_str ~inherits ()

(* Get @property rule if metadata present *)
let property_rule : type a. a t -> Css.t option =
 fun var ->
  match var.property with
  | None -> None
  | Some { initial; inherits; universal } ->
      let name = "--" ^ var.name in
      Some (create_property ~name var.kind initial ~inherits ~universal)

(* Create a binding: returns both declaration and a context-aware var
   reference *)
let binding var ?fallback value = var.binding ?fallback value

(* Create a variable reference for variables with @property defaults OR
   fallback *)
let reference : type a. ?fallback:a Css.fallback -> a t -> a Css.var =
 fun ?fallback var ->
  match (var.property, fallback) with
  | None, None ->
      failwith
        ("Var.reference requires either ~property metadata or a fallback: "
       ^ var.name)
  | None, Some (Css.Fallback fallback_value) ->
      (* No property metadata but have fallback - use fallback as the value *)
      let _, var_ref =
        var.binding ~fallback:(Css.Fallback fallback_value) fallback_value
      in
      var_ref
  | None, Some _ ->
      (* Empty or None fallback without property metadata *)
      failwith
        ("Var.reference without property metadata requires a Fallback value: "
       ^ var.name)
  | Some { initial; _ }, _ -> (
      match (initial, fallback) with
      | None, None ->
          failwith
            ("Var.reference requires either an initial value or a fallback: "
           ^ var.name)
      | None, Some (Css.Fallback fallback_value) ->
          (* No initial but have fallback - use fallback as the value *)
          let _, var_ref =
            var.binding ~fallback:(Css.Fallback fallback_value) fallback_value
          in
          var_ref
      | None, Some Css.None ->
          (* No initial value but explicit None fallback - create bare var
             reference We need a dummy value to create the binding, but the
             fallback None will ensure only var(--name) is generated without a
             fallback *)
          failwith
            ("Cannot create bare variable reference without initial value: "
           ^ var.name ^ ". Use a concrete fallback or add initial value.")
      | None, Some _ ->
          (* Other fallback types without initial value *)
          failwith
            ("Var.reference with no initial value requires a Fallback value: "
           ^ var.name)
      | Some initial_value, _ ->
          (* Have initial value - use it with optional fallback *)
          let _, var_ref = var.binding ?fallback initial_value in
          var_ref)

let ref_only kind name ~fallback =
  let v = channel kind name in
  reference ~fallback:(Css.Fallback fallback) v

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
