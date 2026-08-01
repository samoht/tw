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
let property_slots :
    (Cascade.Css.Declaration.prop_key, int * int) Hashtbl.t option ref =
  ref None

let register (type a) (module M : Handler with type t = a) =
  let internal_h = H (module M : Handler with type t = a) in
  property_slots := None;
  handlers := internal_h :: !handlers

(* The declarations a style writes, its own and those of the rules it carries. A
   style with a pseudo-element suffix is skipped: [placeholder-transparent]
   writes [color], but the colour slot belongs to [text-*], which writes it on
   the element itself. *)
let rec style_declarations (s : Style.t) =
  match s with
  | Style.Style { pseudo_suffix = Some _; _ } -> []
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

(* The property key a declaration can be a slot for. An author's own variable is
   not one: only the properties Tailwind orders utilities by are. *)
let plain_key d =
  match Cascade.Css.Declaration.property_key d with
  | Cascade.Css.Declaration.Key (Custom_property _)
  | Cascade.Css.Declaration.Key (Unknown_property _) ->
      None
  | key -> Some key

(* A vendor-prefixed name without its prefix: [-webkit-mask-image] is
   [mask-image]. [None] when the name carries no prefix. *)
let unprefixed name =
  if String.length name = 0 || name.[0] <> '-' then None
  else
    match String.index_from_opt name 1 '-' with
    | Some i -> Some (String.sub name (i + 1) (String.length name - i - 1))
    | None -> None

(* The properties an example claims: the one it is named for, and the ones it
   writes alongside.

   A vendor prefix names the same slot as the plain property, so it is passed
   over when the style writes both - [mask-none] writes [-webkit-mask-image]
   ahead of [mask-image], and the slot is [mask-image]'s. When only the prefixed
   form is there the family is named for a property with no plain spelling, and
   it claims nothing rather than the next declaration along: [line-clamp-none]
   leads with [-webkit-line-clamp] and would otherwise claim [display], which
   belongs to the display utilities. *)
let claimed_keys decls =
  let names = List.map Cascade.Css.Declaration.property_name decls in
  let rec go named alongside = function
    | [] -> (named, List.rev alongside)
    | d :: rest -> (
        match plain_key d with
        | None -> go named alongside rest
        | Some key -> (
            match unprefixed (Cascade.Css.Declaration.property_name d) with
            | Some plain when List.mem plain names -> go named alongside rest
            | Some _ -> (named, List.rev alongside)
            | None ->
                if named = None then go (Some key) alongside rest
                else go named (key :: alongside) rest))
  in
  go None [] decls

let build_property_slots () =
  let tbl = Hashtbl.create 512 in
  let alongside = Hashtbl.create 512 in
  let record tbl key order =
    match Hashtbl.find_opt tbl key with
    | Some existing when existing <= order -> ()
    | _ -> Hashtbl.replace tbl key order
  in
  (* A family claims the property it is named for before any property it merely
     writes alongside, so the two are collected apart and the named ones win.
     [border-2] writes [border-style: var(--tw-border-style)] ahead of
     [border-width], and border-width is still claimed, from the second pass. *)
  List.iter
    (fun (H (module M)) ->
      List.iter
        (fun example ->
          let order = (M.priority example, M.suborder example) in
          let style = M.to_style Scheme.default example in
          let named, rest = claimed_keys (style_declarations style) in
          Stdlib.Option.iter (fun key -> record tbl key order) named;
          List.iter (fun key -> record alongside key order) rest)
        M.examples)
    !handlers;
  Hashtbl.iter
    (fun key order ->
      if not (Hashtbl.mem tbl key) then Hashtbl.add tbl key order)
    alongside;
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
  Hashtbl.find_opt tbl key

let name_of_base u =
  let rec try_handlers = function
    | [] -> failwith "name_of_base"
    | H (module M) :: rest -> (
        match u with M.Self _ -> M.name | _ -> try_handlers rest)
  in
  try_handlers !handlers

let class_of_base u =
  let found = ref None in
  let visit (H (module M)) =
    match (!found, u) with
    | None, M.Self x -> found := Some (M.to_class x)
    | _ -> ()
  in
  List.iter visit !handlers;
  match !found with
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
