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

  val name : string
  val to_style : Scheme.t -> t -> Style.t
  val priority : t -> int
  val suborder : t -> int
  val of_class : Scheme.t -> string -> (t, [ `Msg of string ]) result
  val to_class : t -> string
  val examples : t list
end

module type Registered = sig
  include Handler

  val inject : t -> base
  val project : base -> t option
end

type handler = H : (module Registered with type t = 'a) -> handler

(* A project's [@utility] sorts at the slot of the property it declares, so that
   slot has to be readable from a property. Each handler offers a few of its own
   utilities as {!Handler.examples}; running them through [to_style] says which
   properties they set, and the lowest order among the ones setting a property
   is that property's slot. The ordering itself stays declared once, on the
   utilities - nothing here restates it. *)
(* [true] on an entry means it came from a property the utility writes on the
   element itself, which outranks one it writes only inside a nested rule. *)
type registry = {
  handlers : handler list;
  property_slots :
    (Cascade.Css.Declaration.prop_key, bool * (int * int)) Hashtbl.t option;
}

let registry = Atomic.make { handlers = []; property_slots = None }
let handlers_snapshot () = (Atomic.get registry).handlers

let register (type a) (module M : Registered with type t = a) =
  let internal_h = H (module M : Registered with type t = a) in
  let rec add () =
    let current = Atomic.get registry in
    let next =
      { handlers = internal_h :: current.handlers; property_slots = None }
    in
    if not (Atomic.compare_and_set registry current next) then add ()
  in
  add ()

module Make (M : Handler) = struct
  type base += Self of M.t

  module Registered = struct
    include M

    let inject value = Self value
    let project = function Self value -> Some value | _ -> None
  end

  let () = register (module Registered)
  let v value = Base (Self value)
end

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
   utility belongs to. The two carriers are named by the channel they read,
   matched on the typed value rather than on the printed declaration, so a
   declared [border-style] naming any other variable stays a style utility. *)
(* A carrier reads its channel and nothing else. There is no public accessor for
   a declaration's typed value - [Properties_intf] is one of cascade's private
   modules, so destructuring the record only compiles when cascade is built from
   source in the same workspace - and the printed form is the surface cascade
   does expose. Comparing it to the one spelling a carrier can have keeps a
   declared [border-style] naming any other variable a style utility. *)
let carries_var decl name =
  String.equal
    (Cascade.Css.declaration_value ~minify:true decl)
    ("var(--" ^ name ^ ")")

let is_ordering_carrier decl =
  match Cascade.Css.Declaration.property_key decl with
  | Key Border_style -> carries_var decl "tw-border-style"
  | Key Outline_style -> carries_var decl "tw-outline-style"
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

let build_property_slots handlers =
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
    handlers;
  tbl

let order_of_property key =
  let rec slots () =
    let current = Atomic.get registry in
    match current.property_slots with
    | Some table -> table
    | None ->
        let table = build_property_slots current.handlers in
        let next = { current with property_slots = Some table } in
        if Atomic.compare_and_set registry current next then table else slots ()
  in
  Option.map snd (Hashtbl.find_opt (slots ()) key)

let name_of_base u =
  let rec try_handlers = function
    | [] -> failwith "name_of_base"
    | H (module M) :: rest -> (
        match M.project u with Some _ -> M.name | None -> try_handlers rest)
  in
  try_handlers (handlers_snapshot ())

let class_of_base u =
  let visit (H (module M)) = Option.map M.to_class (M.project u) in
  match List.find_map visit (handlers_snapshot ()) with
  | Some class_name -> class_name
  | None -> failwith "class_of_base"

let base_of_class theme class_name =
  let rec try_handlers = function
    | [] -> Error (`Msg "Unknown utility")
    | H (module M) :: rest -> (
        match M.of_class theme class_name with
        | Ok x -> Ok (M.inject x)
        | Error _ -> try_handlers rest)
  in
  try_handlers (handlers_snapshot ())

(* Every utility each handler offers as its own example, as class names. *)
let examples_classes () =
  List.concat_map
    (fun (H (module M)) -> List.map M.to_class M.examples)
    (handlers_snapshot ())

(* Every handler that would claim [class_name], by name. [base_of_class] takes
   the first, and the order is the dune link order, so a class two handlers both
   accept resolves on an unrelated build detail rather than on anything
   declared. This is what lets a test assert there is no such class. *)
let claiming_handlers theme class_name =
  List.filter_map
    (fun (H (module M)) ->
      match M.of_class theme class_name with
      | Ok _ -> Some M.name
      | Error _ -> None)
    (handlers_snapshot ())

(* Keep for backward compatibility with tests *)
let base_of_strings theme parts =
  let class_name = String.concat "-" parts in
  base_of_class theme class_name

let base_to_style theme u =
  let handlers = handlers_snapshot () in
  let rec try_handlers = function
    | [] ->
        prerr_endline
          ("Total handlers registered: " ^ string_of_int (List.length handlers));
        failwith
          "Unknown utility type - handler not registered. This is a bug in the \
           utility system."
    | H (module M) :: rest -> (
        match M.project u with
        | Some x -> M.to_style theme x
        | None -> try_handlers rest)
  in
  try_handlers handlers

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
        match M.project u with
        | Some x -> (M.priority x, M.suborder x)
        | None -> try_handlers rest)
  in
  try_handlers (handlers_snapshot ())

let deduplicate utilities =
  let rec go seen acc = function
    | [] -> List.rev acc
    | u :: rest ->
        if List.mem u seen then go seen acc rest
        else go (u :: seen) (u :: acc) rest
  in
  go [] [] (List.rev utilities)
