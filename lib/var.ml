module Css = Cascade.Css
(* Typed CSS custom properties (variables) - Simplified API

   This module provides the core extensible variable system for CSS custom
   properties following the simplified design documented in var.mli *)

(* Layer classification for CSS variables *)
type layer = Theme | Utility

(* Tailwind spells one custom-property zero two ways: [--tw-ring-offset-width:
   0px] in the properties layer's bulk declaration, but [initial-value: 0] in
   its own [@property] rule. Other zero-initialised [Length] variables (e.g.
   border-spacing-x/y) get "0" in both places, so this isn't a general
   Length-zero rule cascade's printer could apply - it's a one-name quirk of
   Tailwind's own output that has to be special-cased here. A total function, so
   nothing depends on definition order. *)
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
  meta : Css.meta;
}

(* Type shortcuts for common patterns *)
type 'a theme = ('a, [ `Theme ]) t
type 'a property_default = ('a, [ `Property_default ]) t
type 'a channel = ('a, [ `Channel ]) t
type 'a ref_only = ('a, [ `Ref_only ]) t

(* Families for ordering/grouping without string prefixes. *)
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

type info =
  | Info : {
      name : string;
      kind : 'a Css.kind option;
      property : 'a property_info option;
      order : (int * int) option;
      property_order : int option;
      family : family option;
      runtime : bool;
      default_css : string option;
    }
      -> info

let (meta_of_info : info -> Css.meta), (info_of_meta : Css.meta -> info option)
    =
  Css.meta ()

type metadata = info

let metadata_name (Info info) = info.name
let metadata_order (Info info) = info.order
let metadata_property_order (Info info) = info.property_order
let metadata_family (Info info) = info.family
let metadata_needs_property (Info info) = info.property <> None
let metadata_default_css (Info info) = info.default_css
let metadata_of_var v = Option.bind (Css.var_meta v) info_of_meta

let metadata_of_declaration declaration =
  Option.bind (Css.meta_of_declaration declaration) info_of_meta

let metadata var =
  match info_of_meta var.meta with
  | Some metadata -> metadata
  | None -> assert false

let layer_name = function (Theme : layer) -> "theme" | Utility -> "utilities"

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

  let info =
    Info
      {
        kind = Some kind;
        name;
        property;
        order;
        property_order;
        family;
        runtime = Option.value runtime ~default:false;
        default_css = None;
      }
  in
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
  { kind; name; role; binding; property; fallback; meta = meta_of_info info }

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
  | _ -> Css.Properties.string_of_kind_value kind value

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

let property_rule_of_metadata (Info info) =
  match (info.kind, info.property) with
  | Some kind, Some { initial; inherits; universal } ->
      Some (property ~name:("--" ^ info.name) kind initial ~inherits ~universal)
  | _ -> None

let property_rule_of_var var =
  Option.bind (metadata_of_var var) property_rule_of_metadata

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

let theme_ref : type a. ?default:a -> ?default_css:string -> string -> a Css.var
    =
 fun ?default ?default_css name ->
  let info =
    Info
      {
        kind = None;
        name;
        property = None;
        order = None;
        property_order = None;
        family = None;
        runtime = false;
        default_css;
      }
  in
  Css.var_ref ~layer:"theme" ~meta:(meta_of_info info) ?default name

(* Turn a parsed [@property] statement's typed initial value into the
   declaration that sets it in the properties layer. [typed_custom_property]
   prints the value with cascade's own [pp_value] for [info.syntax], so this
   never hand-dispatches on the syntax or reprints a value itself. *)
let declaration_of_property_info (Css.Property_info info) =
  match properties_layer_override info.name with
  | Some css -> Css.custom_property info.name css
  | None -> (
      match info.initial_value with
      | None -> Css.custom_property info.name "initial"
      | Some v -> Css.Variables.typed_custom_property info.name info.syntax v)

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
      | Some (Info i) -> i.property <> None
      | None -> assert false)

let order_of_declaration decl =
  Option.bind (metadata_of_declaration decl) metadata_order

let is_runtime_declaration decl =
  match metadata_of_declaration decl with
  | Some (Info t) -> t.runtime
  | None -> false

let property_initial_declaration = declaration_of_property_info
let pp v = Pp.str [ "Var(--"; v.name; ")" ]

(* CSS Syntax 3 (ED), the [<declaration-value>] production: any token sequence
   with no unmatched [)], []] or [}] in it. A [var()] read whole gets that for
   free - its function token ends at the first unmatched closer, and text after
   it is text the reference does not cover - but a cursor over the arguments
   alone has no closer to end at, so the loose one is looked for here. *)
let closes_nothing = function
  | Cascade.Component.Preserved { kind = Cascade.Token.Close _; _ } -> true
  | _ -> false

let bracket ?fallback name =
  (* Both readers hand back the name without its [--] and the fallback as
     [<declaration-value>] text, which is what [Css.Values.syntax_fallback]
     (below) consumes. *)
  let read_whole source read =
    try
      let cursor = Cascade.Cursor.of_string source in
      if List.exists closes_nothing (Cascade.Cursor.remaining cursor) then None
      else
        let name, fallback = read cursor in
        (* Text neither reader consumed is text the reference does not cover. *)
        if Cascade.Cursor.is_done cursor then Some (name, fallback) else None
    with Cascade.Cursor.Parse_error _ | Invalid_argument _ -> None
  in
  let parsed_reference =
    match fallback with
    | Some _ -> None
    | None ->
        if String.starts_with ~prefix:"var(" name then
          read_whole name Css.Variables.read_reference
        else if String.contains name ',' then
          (* [name] is a [var()] body: the custom property and the fallback
             written after the comma, the spelling {!Parse.extract_var_name}
             retains. [--] is the property's own prefix, the one [css_name]
             writes back. *)
          read_whole ("--" ^ name) Css.Variables.read_reference_body_as_string
        else None
  in
  (* [Parse.extract_var_name] leaves malformed references alone. Arbitrary
     values historically also use [var(--...)] as a wrapper around a raw
     custom-property name, where a loose closer belongs to that name. Retain
     that interpretation only after the CSS reference reader has rejected the
     whole string. *)
  let raw_name =
    let prefix = "var(--" in
    let len = String.length name in
    let prefix_len = String.length prefix in
    if
      parsed_reference = None
      && String.starts_with ~prefix name
      && len > prefix_len
      && name.[len - 1] = ')'
    then String.trim (String.sub name prefix_len (len - prefix_len - 1))
    else name
  in
  match parsed_reference with
  | Some (name, Some fallback) ->
      Css.var_ref ~runtime:true
        ~fallback:(Css.Values.syntax_fallback fallback)
        name
  | Some (name, None) -> Css.var_ref ~runtime:true name
  | None -> Css.var_ref ?fallback raw_name
