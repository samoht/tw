module Css = Cascade.Css

let equal_layer = Css.Stylesheet.equal_layer_name

(* An empty [@layer name] block says only that the name has a slot, and with
   nothing in it the fold below reads it as no occurrence at all and leaves it
   standing. The generated sheet writes an empty utilities layer whenever every
   utility in the sheet is a declared one, and [hoist_layer_blocks] fills a slot
   from the first block of its name, so an empty block in front of the real one
   hides the rules. Write what it means, a slot, and both passes then see the
   block that has them. Tailwind emits the same [@layer name;] for it. *)
let declare_empty_layers stmts =
  let has_content name =
    List.exists
      (fun st ->
        match Css.as_layer st with
        | Some (Some n, _ :: _) -> equal_layer n name
        | _ -> false)
      stmts
  in
  List.map
    (fun st ->
      match Css.as_layer st with
      | Some (Some n, []) when has_content n -> Css.layer_decl [ n ]
      | _ -> st)
    stmts

(* A named layer appears once in Tailwind's output, so fold every repeat of a
   name into the first. The generated sheet and the [@layer properties] block
   each applied utility hoists say the same thing, and the fold takes no hook to
   re-optimize the body it joins the way the [merge_consecutive_*] passes do, so
   drop what the joined body now holds twice. *)
let merge_named_layers stmts =
  let stmts = declare_empty_layers stmts in
  let merged = Css.Optimize.merge_named_layers_by_name stmts in
  if List.compare_lengths merged stmts = 0 then stmts
  else
    List.map
      (fun stmt ->
        match Css.as_layer stmt with
        | Some (Some name, inner) -> Css.layer ~name (Statements.dedup inner)
        | _ -> stmt)
      merged

(* Every [@apply] and every declared utility hoists a [@layer properties] block
   of its own, holding the initial values of the variables its utilities set,
   and the generated sheet hoists one too. Folded into the single layer the
   sheet declares, they line up as a run of [@supports] blocks over one
   condition and one universal selector, each repeating variables the others
   already declare. Tailwind writes one block. Joining the run leaves every
   variable on the value the last block in it gave it, which is the value it
   held before. *)
let join_fallback_rules stmts =
  Statements.merge_same_selector stmts
  |> List.map (fun stmt ->
      match Css.as_rule stmt with
      | Some (selector, decls, []) ->
          Css.rule ~selector (Css.Optimize.deduplicate_declarations decls)
      | _ -> stmt)

let collapse_property_fallbacks stmts =
  List.map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when equal_layer name [ "properties" ] ->
          Css.layer ~name
            (Css.Optimize.merge_consecutive_supports
               ~optimize_merged_block:join_fallback_rules inner)
      | _ -> stmt)
    stmts

(* A token declared twice keeps the place its first declaration holds, so the
   theme's own order survives, and takes the value its last one gives, which is
   the value the cascade settled on. *)
let join_theme_declarations decls =
  let last = Hashtbl.create 16 in
  List.iter
    (fun d ->
      Option.iter
        (fun name -> Hashtbl.replace last name d)
        (Css.custom_declaration_name d))
    decls;
  let placed = Hashtbl.create 16 in
  List.filter_map
    (fun d ->
      match Css.custom_declaration_name d with
      | None -> Some d
      | Some name when Hashtbl.mem placed name -> None
      | Some name ->
          Hashtbl.add placed name ();
          Some (Hashtbl.find last name))
    decls

(* Every [@apply] hoists a [:root, :host] rule of its own, declaring the tokens
   its utilities read, and the generated sheet declares the theme too. Folded
   into the one [@layer theme], they line up as a run of rules Tailwind writes
   as one; under [theme(static)] every token in the later ones is already in the
   first. *)
let join_theme_rules stmts =
  List.map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when equal_layer name [ "theme" ] ->
          Css.layer ~name
            (Statements.merge_same_selector inner
            |> List.map (fun stmt ->
                match Css.as_rule stmt with
                | Some (selector, decls, []) ->
                    Css.rule ~selector (join_theme_declarations decls)
                | _ -> stmt))
      | _ -> stmt)
    stmts

let layer_block_name stmt =
  match Css.layer_block_name stmt with Some [] | None -> None | name -> name

(* A layer is declared by the first mention of its name, so a later declaration
   of the same slot says nothing. Expanding a layer list writes a slot where its
   name sat and the sheet carries a standalone declaration of that same slot
   further on, so track what has been declared and keep only the first. *)
let slot_registry () =
  let declared = Hashtbl.create 8 in
  let key n = Css.Stylesheet.string_of_layer_name n in
  let declare n = Hashtbl.replace declared (key n) () in
  let is_declared n = Hashtbl.mem declared (key n) in
  (declare, is_declared)

(* [@layer components;] on its own declares the layer's slot; the block that
   fills it can come much later, from an imported file. Tailwind emits the block
   in the slot, so move it there. The declared order already makes this
   cascade-neutral; it is the document shape that differs. *)
let movable_layer_slots stmts =
  let is_block_of n st =
    match layer_block_name st with Some m -> equal_layer m n | None -> false
  in
  (* Two layer names never share their printed text, so it keys them. *)
  let by_text a b =
    String.compare
      (Css.Stylesheet.string_of_layer_name a)
      (Css.Stylesheet.string_of_layer_name b)
  in
  List.filter_map Css.layer_statement_name_list stmts
  |> List.concat |> List.sort_uniq by_text
  |> List.filter (fun n -> List.exists (is_block_of n) stmts)

let expand_layer_list ~movable ~emitted ~declare ~is_declared ~block_for names =
  List.filter_map
    (fun n ->
      let repeat = is_declared n in
      declare n;
      if List.exists (equal_layer n) movable then
        if Hashtbl.mem emitted n then None
        else begin
          Hashtbl.add emitted n ();
          block_for n
        end
      else if repeat then None
      else Some (Css.layer_decl [ n ]))
    names

let fresh_layer_decl ~declare ~is_declared names =
  match List.filter (fun n -> not (is_declared n)) names with
  | [] -> []
  | fresh ->
      List.iter declare fresh;
      [ Css.layer_decl fresh ]

(* [@layer properties] carries the initial values of the variables the utilities
   set, so it has to be declared ahead of the layers that read them, which is
   where Tailwind emits it. The generated sheet places its own; one an [@apply]
   hoisted arrives at the end of the entrypoint text instead, and would order
   after every other layer. [Place_routed] does the same for the
   declared-utility path. *)
let lead_properties_layer stmts =
  let is_properties stmt =
    match Css.layer_block_name stmt with
    | Some name -> Css.Stylesheet.equal_layer_name name [ "properties" ]
    | None -> false
  in
  match List.partition is_properties stmts with
  | [], _ -> stmts
  | lead, rest -> lead @ rest

let hoist_layer_blocks stmts =
  let movable = movable_layer_slots stmts in
  if movable = [] then stmts
  else
    let is_block_of n st =
      match layer_block_name st with Some m -> equal_layer m n | None -> false
    in
    let block_for n = List.find_opt (is_block_of n) stmts in
    let emitted = Hashtbl.create 8 in
    let declare, is_declared = slot_registry () in
    List.concat_map
      (fun stmt ->
        match Css.layer_statement_name_list stmt with
        | Some names
          when List.exists (fun n -> List.exists (equal_layer n) movable) names
          ->
            expand_layer_list ~movable ~emitted ~declare ~is_declared ~block_for
              names
        | Some names -> fresh_layer_decl ~declare ~is_declared names
        | None -> (
            match layer_block_name stmt with
            | Some n when List.exists (equal_layer n) movable -> []
            | Some n ->
                declare n;
                [ stmt ]
            | None -> [ stmt ]))
      stmts

(* A token the project declared in an [@theme inline] block has no declaration
   of its own — the value goes into the utility instead. Two exceptions: one
   that refers to itself, where inlining would leave the reference dangling, and
   one some other rule still reads. That has to be judged over the whole
   document: the typography plugin's [.prose code] reads [--font-mono] from the
   components layer. *)
let drop_unread_inline_tokens ~theme stmts =
  if Tw.Scheme.(theme.inline_tokens) = [] then stmts
  else
    let reads =
      Css.vars_of_stylesheet (Css.v stmts) |> List.map Css.any_var_name
    in
    let keep decl =
      match Css.custom_declaration_name decl with
      | Some n when String.length n > 2 ->
          let bare = String.sub n 2 (String.length n - 2) in
          (not (Tw.Scheme.is_inline_token theme bare)) || List.mem n reads
      | _ -> true
    in
    let rec go stmts =
      List.map
        (fun stmt ->
          match Css.as_layer stmt with
          | Some (name, inner) -> Css.layer ?name (go inner)
          | None -> (
              match Css.as_rule stmt with
              | Some (sel, decls, nested) ->
                  Css.rule ~selector:sel ~nested (List.filter keep decls)
              | None -> stmt))
        stmts
    in
    go stmts

(* An imported file under [layer(...)] is expanded before its layer wraps it, so
   what its [@apply]s hoisted - a [@layer theme] or [@layer properties] block,
   an [@property] registration, an [@keyframes] - arrives inside that layer.
   Tailwind keeps them at the top of the sheet, where [merge_named_layers] folds
   the blocks into the generated sheet's own. A class-less rule the author wrote
   in the import stays where it is: the expansion never leaves one bare, it
   wraps its theme rule in [@layer theme]. *)
let lift_hoisted_out_of_layers stmts =
  let hoisted stmt =
    (match Css.as_layer stmt with
      | Some (Some name, _) ->
          equal_layer name [ "theme" ] || equal_layer name [ "properties" ]
      | _ -> false)
    || Css.as_property stmt <> None
    || Css.as_keyframes stmt <> None
  in
  List.concat_map
    (fun stmt ->
      match Css.as_layer stmt with
      | Some (Some name, inner) when List.exists hoisted inner ->
          let lifted, kept = List.partition hoisted inner in
          lifted @ [ Css.layer ~name kept ]
      | _ -> [ stmt ])
    stmts

(* A [@property] the author's [@apply] hoisted and one the generated sheet sets
   name the same custom property, and a second [@property] for a name is
   redundant. Keep the first, and put them all at the end of the document, where
   Tailwind emits them: spliced at the [@import] instead, they sit ahead of the
   author's own rules and shift every one of them. *)
let collect_properties_at_end stmts =
  let seen = Hashtbl.create 64 in
  let keep, props =
    List.partition_map
      (fun stmt ->
        match Css.as_property stmt with
        | None ->
            if Css.as_keyframes stmt <> None then Right stmt
            else Left (Some stmt)
        | Some (Css.Property_info { name; _ }) ->
            if Hashtbl.mem seen name then Left None
            else begin
              Hashtbl.add seen name ();
              Right stmt
            end)
      stmts
  in
  let at_end, keyframes =
    List.partition (fun st -> Css.as_keyframes st = None) props
  in
  List.filter_map Fun.id keep @ at_end @ keyframes

let tidy ~theme stmts =
  stmts |> lift_hoisted_out_of_layers |> merge_named_layers
  |> collapse_property_fallbacks |> join_theme_rules |> hoist_layer_blocks
  |> lead_properties_layer
  |> drop_unread_inline_tokens ~theme
  |> collect_properties_at_end
