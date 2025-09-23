(* Typed CSS custom properties (variables) - Simplified API

   This module provides the core extensible variable system for CSS custom
   properties following the simplified design from todo/vars.md *)

(* Layer classification for CSS variables *)
type layer = Theme | Utility

(* CSS variable kinds as extensible GADT - modules add their own *)
type _ kind = ..

(* Core CSS variable kinds *)
type _ kind +=
  | Color : string * int option -> Css.color kind
  | Spacing : Css.length kind
  | Font_family_list : Css.font_family kind
  | Scroll_snap_strictness : Css.scroll_snap_strictness kind
  | Duration : Css.duration kind

(* Variable definition - the main currency *)
type 'a property_info = Info : 'b Css.syntax * 'b * bool -> 'a property_info

type 'a t = {
  kind : 'a kind; (* Type witness for safety *)
  name : string; (* Variable name without -- prefix *)
  layer : layer; (* Theme or Utility *)
  fallback : 'a option; (* Default for var() references *)
  property : 'a property_info option; (* For @property registration *)
}

(* Existential wrapper *)
type any = Any : _ kind -> any

(* Metadata storage *)
type meta_info = { var : any; needs_property : bool }

let ( (meta_of_info : meta_info -> Css.meta),
      (info_of_meta : Css.meta -> meta_info option) ) =
  Css.meta ()

let of_meta meta =
  match info_of_meta meta with None -> None | Some { var; _ } -> Some var

let needs_property_of_meta meta =
  match info_of_meta meta with
  | None -> None
  | Some { needs_property; _ } -> Some needs_property

(* Canonical color ordering *)
let canonical_color_order = function
  | "red" -> 0
  | "orange" -> 1
  | "amber" -> 2
  | "yellow" -> 3
  | "lime" -> 4
  | "green" -> 5
  | "emerald" -> 6
  | "teal" -> 7
  | "cyan" -> 8
  | "sky" -> 9
  | "blue" -> 10
  | "indigo" -> 11
  | "violet" -> 12
  | "purple" -> 13
  | "fuchsia" -> 14
  | "pink" -> 15
  | "rose" -> 16
  | "slate" -> 17
  | "gray" -> 18
  | "zinc" -> 19
  | "neutral" -> 20
  | "stone" -> 21
  | "black" -> 100
  | "white" -> 101
  | _ -> 200

(* Variable ordering for theme layer *)
let order : type a. a kind -> int = function
  | Color (_, _) -> 3
  | Spacing -> 4
  | Font_family_list -> 5
  | Scroll_snap_strictness -> 1501
  | Duration -> 1502
  | _ -> 9999 (* Extensions go last *)

let compare_color : type a b. a kind -> b kind -> int =
 fun a b ->
  match (a, b) with
  | Color (name_a, shade_a), Color (name_b, shade_b) ->
      let name_cmp =
        Int.compare
          (canonical_color_order name_a)
          (canonical_color_order name_b)
      in
      if name_cmp <> 0 then name_cmp
      else Option.compare Int.compare shade_a shade_b
  | _ -> 0

let compare (Any a) (Any b) =
  let order_a = order a in
  let order_b = order b in
  let cmp = Int.compare order_a order_b in
  if cmp <> 0 then cmp else compare_color a b

let layer_name = function Theme -> "theme" | Utility -> "utilities"

(* Create a variable template *)
let create : type a. a kind -> ?fallback:a -> string -> layer:layer -> a t =
 fun kind ?fallback name ~layer ->
  { kind; name; layer; fallback; property = None }

(* Add @property metadata *)
let with_property : type a b.
    syntax:b Css.syntax -> initial:b -> ?inherits:bool -> a t -> a t =
 fun ~syntax ~initial ?(inherits = false) var ->
  { var with property = Some (Info (syntax, initial, inherits)) }

(* Get declaration with a specific value *)
let declaration : type a. a t -> a -> Css.declaration =
 fun var value ->
  let meta =
    meta_of_info { var = Any var.kind; needs_property = var.property <> None }
  in
  let var_name = var.name in
  let layer = Some (layer_name var.layer) in
  (* Create typed CSS variable *)
  match var.kind with
  | Spacing ->
      let d, _ = Css.var ?layer ~meta var_name Length value in
      d
  | Color (_, _) ->
      let d, _ = Css.var ?layer ~meta var_name Color value in
      d
  | Font_family_list ->
      let d, _ = Css.var ?layer ~meta var_name Font_family value in
      d
  | Scroll_snap_strictness ->
      let d, _ = Css.var ?layer ~meta var_name Scroll_snap_strictness value in
      d
  | Duration ->
      let d, _ = Css.var ?layer ~meta var_name Duration value in
      d
  | _ -> (
      (* For extension kinds defined in other modules (like Typography), we need
         to handle them specially. Since we can't match on constructors defined
         later, we use Obj.magic to convert the value to the appropriate CSS
         kind. This is safe because the type system ensures the value matches
         the kind's return type.

         We try each CSS kind in turn - the CSS module will accept the right
         one. *)
      try
        (* Try as Length - used by text size variables *)
        let d, _ = Css.var ?layer ~meta var_name Length (Obj.magic value) in
        d
      with _ -> (
        try
          (* Try as Line_height - used by text line height variables *)
          let d, _ =
            Css.var ?layer ~meta var_name Line_height (Obj.magic value)
          in
          d
        with _ -> (
          try
            (* Try as Font_family - used by font family variables *)
            let d, _ =
              Css.var ?layer ~meta var_name Font_family (Obj.magic value)
            in
            d
          with _ -> (
            try
              (* Try as Font_weight - used by font weight variables *)
              let d, _ =
                Css.var ?layer ~meta var_name Font_weight (Obj.magic value)
              in
              d
            with _ -> (
              try
                (* Try as Font_variant_numeric_token *)
                let d, _ =
                  Css.var ?layer ~meta var_name Font_variant_numeric_token
                    (Obj.magic value)
                in
                d
              with _ -> (
                try
                  (* Try as Content *)
                  let d, _ =
                    Css.var ?layer ~meta var_name Content (Obj.magic value)
                  in
                  d
                with _ ->
                  (* Final fallback - this shouldn't happen for Typography
                     kinds *)
                  failwith
                    (Printf.sprintf
                       "Var.declaration: Could not determine CSS kind for \
                        variable '%s'"
                       var_name)))))))

(* Check if variable needs @property rule *)
let needs_property : type a. a t -> bool = fun var -> var.property <> None

(* Get layer of a variable *)
let layer : type a. a t -> layer = fun var -> var.layer

(* Get var() reference *)
let use : type a. a t -> a Css.var =
 fun var ->
  let meta = meta_of_info { var = Any var.kind; needs_property = false } in
  let fallback = Option.map (fun f -> Css.Fallback f) var.fallback in
  (* Pass fallback as default too for inline mode resolution *)
  let default = var.fallback in
  Css.var_ref ?fallback ?default ~meta var.name

(* Get @property rule if metadata present *)
let property_rule : type a. a t -> Css.t option =
 fun var ->
  match var.property with
  | None -> None
  | Some (Info (syntax, initial, inherits)) ->
      let name = "--" ^ var.name in
      Some (Css.property ~name syntax ~inherits ~initial_value:initial ())

(* === Var binding for new style function === *)
type binding = Binding : 'a t * 'a -> binding

(* Extract declaration from binding *)
let declaration_of_binding (Binding (var, value)) = declaration var value

(* Extract property rule from binding *)
let property_rule_of_binding (Binding (var, _)) = property_rule var

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
      | syntax -> Css.Pp.to_string (pp_value syntax) v)

(* Compare declarations *)
let compare_declarations _layer d1 d2 =
  match (Css.meta_of_declaration d1, Css.meta_of_declaration d2) with
  | Some m1, Some m2 -> (
      match (of_meta m1, of_meta m2) with
      | Some v1, Some v2 -> compare v1 v2
      | _ -> 0)
  | _ -> 0

(* Map and Set modules *)
module Map = Map.Make (struct
  type t = any

  let compare = compare
end)

module Set = Set.Make (struct
  type t = any

  let compare = compare
end)
