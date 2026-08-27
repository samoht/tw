(** Utility module for common utility types and functions *)

type base = ..

type t =
  | Base of base
  | Modified of Style.modifier * t
  | Group of t list
  | Important of bool * t  (** [bool] is [true] for the v4 trailing [!] form. *)
  | Aliased of string * t
      (** [Aliased (class_name, u)] renders as [u] but reports [class_name] from
          {!to_class}, so the emitted selector matches the source spelling. Used
          for the [prop-(--x)] shorthand, which is [prop-[var(--x)]] in value
          but keeps its own class name. *)

let base x = Base x
let important ?(suffix = false) x = Important (suffix, x)
let alias class_name u = Aliased (class_name, u)

(* See the .mli. *)
module Property_order = struct
  let width = 1000
  let slot rank = width * rank
  let last rank = slot rank + (width - 1)
end

module type Handler = sig
  type t
  type base += Self of t

  val name : string
  val to_style : Scheme.t -> t -> Style.t
  val priority : t -> int
  val suborder : t -> int
  val of_class : Scheme.t -> string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val examples : t list
end

type handler = H : (module Handler with type t = 'a) -> handler

let handlers : handler list ref = ref []

(* A project's [@utility] sorts at the slot of the property it declares, so that
   slot has to be readable from a property. Each handler offers a few of its own
   utilities as {!Handler.examples}; running them through [to_style] says which
   properties they set, and the lowest order among the ones setting a property
   is that property's slot. The ordering itself stays declared once, on the
   utilities - nothing here restates it. *)
(* [true] on an entry means it came from a property the utility writes on the
   element itself, which outranks one it writes only inside a nested rule. *)
let property_slots :
    (Cascade.Css.Declaration.prop_key, bool * (int * int)) Hashtbl.t option ref
    =
  ref None

let register (type a) (module M : Handler with type t = a) =
  let internal_h = H (module M : Handler with type t = a) in
  property_slots := None;
  handlers := internal_h :: !handlers

(* The declarations a style writes on the element itself: a pseudo-element
   suffix or a rule of its own moves them off it. *)
let rec style_own_declarations (s : Style.t) =
  match s with
  | Style.Style { pseudo_suffix = Some _; _ } -> []
  | Style.Style { props; _ } -> props
  | Style.Modified (_, inner) -> style_own_declarations inner
  | Style.Group inner -> List.concat_map style_own_declarations inner

(* The declarations a style writes, its own and those of the rules it
   carries. *)
let rec style_declarations (s : Style.t) =
  match s with
  | Style.Style { props; rules; _ } ->
      props
      @ List.concat_map
          (fun rule ->
            match Cascade.Css.as_rule rule with
            | Some (_, decls, _) -> decls
            | None -> [])
          (Stdlib.Option.value ~default:[] rules)
  | Style.Modified (_, inner) -> style_declarations inner
  | Style.Group inner -> List.concat_map style_declarations inner

(* A width utility writes the style property as a bare reference to the style
   utilities' channel, so the declaration says nothing about which family the
   utility belongs to. No cascade API tells a bare var() carrier from a real
   value, so the two carriers are named here. *)
let is_ordering_carrier decl =
  match Cascade.Css.Declaration.property_key decl with
  | Key Border_style ->
      Cascade.Css.declaration_value ~minify:true decl = "var(--tw-border-style)"
  | Key Outline_style ->
      Cascade.Css.declaration_value ~minify:true decl
      = "var(--tw-outline-style)"
  | _ -> false

(* The property name a vendor-prefixed one stands in for: [-webkit-user-select]
   is [user-select]. *)
let unprefixed_name name =
  if String.length name > 1 && name.[0] = '-' then
    Option.map
      (fun i -> String.sub name (i + 1) (String.length name - i - 1))
      (String.index_from_opt name 1 '-')
  else None

(* A prefixed declaration a later one repeats unprefixed is the same property
   written twice for reach, and the slot belongs to the standard spelling. One
   with no unprefixed twin ([-webkit-line-clamp]) is the utility's own. *)
let is_prefixed_duplicate decl rest =
  match unprefixed_name (Cascade.Css.Declaration.property_name decl) with
  | None -> false
  | Some plain ->
      List.exists
        (fun d -> String.equal (Cascade.Css.Declaration.property_name d) plain)
        rest

let ordering_property declarations =
  let rec scan = function
    | [] -> None
    | decl :: rest -> (
        if is_ordering_carrier decl || is_prefixed_duplicate decl rest then
          scan rest
        else
          match Cascade.Css.Declaration.property_key decl with
          | Key (Custom_property _) | Key (Unknown_property _) -> scan rest
          | key -> Some key)
  in
  scan declarations

let build_property_slots () =
  let tbl = Hashtbl.create 512 in
  (* A property a utility writes on the element itself decides that utility's
     family; one it writes only inside a rule it carries does not, unless
     nothing else claims the property. [placeholder-transparent] writes [color]
     in a [::placeholder] rule, and the [color] slot belongs to the text
     colours. *)
  let record ~own key order =
    match Hashtbl.find_opt tbl key with
    | Some (true, _) when not own -> ()
    | Some (had_own, existing) when had_own = own && existing <= order -> ()
    | _ -> Hashtbl.replace tbl key (own, order)
  in
  List.iter
    (fun (H (module M)) ->
      List.iter
        (fun example ->
          let order = (M.priority example, M.suborder example) in
          let style = M.to_style Scheme.default example in
          (* The property a utility claims is the first named property it
             writes: the one it is named for. Theme-token declarations that make
             that value available are not slots of their own. A later named
             declaration is incidental - line-clamp ends with [display:
             -webkit-box] but the display slot belongs to the display utilities,
             which sort elsewhere. *)
          match ordering_property (style_own_declarations style) with
          | Some key -> record ~own:true key order
          | None ->
              ordering_property (style_declarations style)
              |> Option.iter (fun key -> record ~own:false key order))
        M.examples)
    !handlers;
  tbl

let order_of_property key =
  let tbl =
    match !property_slots with
    | Some tbl -> tbl
    | None ->
        let tbl = build_property_slots () in
        property_slots := Some tbl;
        tbl
  in
  Option.map snd (Hashtbl.find_opt tbl key)

let name_of_base u =
  let rec try_handlers = function
    | [] -> failwith "name_of_base"
    | H (module M) :: rest -> (
        match u with M.Self _ -> M.name | _ -> try_handlers rest)
  in
  try_handlers !handlers

let class_of_base u =
  let visit (H (module M)) =
    match u with M.Self x -> Some (M.to_class x) | _ -> None
  in
  match List.find_map visit !handlers with
  | Some class_name -> class_name
  | None -> failwith "name_of_base"

let base_of_class theme class_name =
  let rec try_handlers = function
    | [] -> Error (`Msg "Unknown utility")
    | H (module M) :: rest -> (
        match M.of_class theme class_name with
        | Ok x -> Ok (M.Self x)
        | Error _ -> try_handlers rest)
  in
  try_handlers !handlers

(* Keep for backward compatibility with tests *)
let base_of_strings theme parts =
  let class_name = String.concat "-" parts in
  base_of_class theme class_name

let base_to_style theme u =
  let rec try_handlers = function
    | [] ->
        prerr_endline
          ("Total handlers registered: " ^ string_of_int (List.length !handlers));
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H (module M) :: rest -> (
        match u with M.Self x -> M.to_style theme x | _ -> try_handlers rest)
  in
  try_handlers !handlers

let rec to_style theme = function
  | Base u -> base_to_style theme u
  | Modified (m, u) -> Style.Modified (m, to_style theme u)
  | Group us -> Style.Group (List.map (to_style theme) us)
  | Important (_, u) -> Style.map_important (to_style theme u)
  | Aliased (_, u) -> to_style theme u

let rec to_class = function
  | Base u -> class_of_base u
  | Modified (m, u) -> (
      match u with
      | Group us ->
          (* When a modifier wraps a group, apply it to each item in the
             group *)
          String.concat " "
            (List.map (fun item -> to_class (Modified (m, item))) us)
      | _ -> Style.pp_modifier m ^ ":" ^ to_class u)
  | Group us -> String.concat " " (List.map to_class us)
  | Important (suffix, u) ->
      if suffix then to_class u ^ "!" else "!" ^ to_class u
  | Aliased (class_name, _) -> class_name

let rec pp = function
  | Base u -> "Base " ^ class_of_base u
  | Modified (m, u) -> "Modified (" ^ Style.pp_modifier m ^ ", " ^ pp u ^ ")"
  | Group us -> "Group [" ^ String.concat "; " (List.map pp us) ^ "]"
  | Important (_, u) -> "Important (" ^ pp u ^ ")"
  | Aliased (class_name, u) -> "Aliased (" ^ class_name ^ ", " ^ pp u ^ ")"

let order (u : base) : int * int =
  let rec try_handlers = function
    | [] ->
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H (module M) :: rest -> (
        match u with
        | M.Self x -> (M.priority x, M.suborder x)
        | _ -> try_handlers rest)
  in
  try_handlers !handlers

let deduplicate utilities =
  let rec go seen acc = function
    | [] -> List.rev acc
    | u :: rest ->
        if List.mem u seen then go seen acc rest
        else go (u :: seen) (u :: acc) rest
  in
  go [] [] (List.rev utilities)
