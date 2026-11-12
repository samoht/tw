module Css = Cascade.Css
module Index = Cascade.Source_index

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Preload every transitively-referenced stylesheet, keyed by the URL resolved
   against its importer, which is what the inliner looks up. Mirrors cascade's
   own filesystem loader. A package import has no file and stays unresolved on
   purpose, so the splice below can find it. *)
let preload_imports ~transform ~base_url stylesheet =
  let imports = Hashtbl.create 16 in
  let rec scan_under base sheet =
    let loader = Css.Context.loader ~base_url:base () in
    Css.fold (scan_stmt loader) () sheet
  and scan_stmt loader () stmt =
    match Css.as_import stmt with
    | Some ir when not (Tailwind_import.is_package_url ir.url) ->
        handle loader (Css.decode_import_url ir.url)
    | _ -> ()
  and handle loader url =
    match Css.Context.resolve_url loader url with
    | Error _ -> ()
    | Ok resolved -> (
        if not (Hashtbl.mem imports resolved) then
          match read_file resolved with
          | exception Sys_error _ -> ()
          | content -> (
              let content = transform content in
              Hashtbl.add imports resolved content;
              match Css.of_string content with
              | Ok inner -> scan_under resolved inner.Css.stylesheet
              | Error _ -> ()))
  in
  scan_under base_url stylesheet;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) imports []

(* Compile the project's CSS entrypoint instead of only reading its [@theme].
   Tailwind treats that file as the stylesheet: its own rules and its relative
   [@import]s are part of the output, and [@import "tailwindcss"] is where the
   generated sheet goes. Reading it for tokens alone silently dropped every rule
   the project wrote. *)
(* A [@property] the author's [@apply] hoisted and one the generated sheet sets
   name the same custom property, and a second [@property] for a name is
   redundant. Keep the first, and put them all at the end of the document, where
   Tailwind emits them: spliced at the [@import] instead, they sit ahead of the
   author's own rules and shift every one of them. *)
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

(* Tailwind runs authored CSS through Lightning CSS, which writes a legacy value
   before a dynamic [color-mix()] and keeps the authored value behind a feature
   query. Utility output already carries that pair; this pass is only applied to
   the parsed entrypoint before the generated sheet is spliced in.

   [flatten_nesting] has made every element rule flat by the time this runs, so
   splitting one declaration into a rule/query pair preserves its exact place
   among the author's declarations. *)
let fallback_declaration ~theme declaration =
  let value = Css.declaration_value ~minify:true declaration in
  match Css.parse_color value with
  | None -> None
  | Some color -> (
      match Tw.Color.pre_color_mix_fallback theme color with
      | None -> None
      | Some fallback ->
          let fallback_value =
            Css.color fallback |> Css.Declaration.normalize
            |> Css.declaration_value ~minify:true
          in
          let layer = Css.Declaration.custom_declaration_layer declaration in
          Css.Declaration.parse_declaration ?layer
            (Css.declaration_name declaration)
            fallback_value
          |> Option.map (fun fallback ->
              if Css.declaration_is_important declaration then
                Css.important fallback
              else fallback))

let lower_color_mix_rule ~theme selector declarations =
  let rec loop pending emitted = function
    | [] ->
        let emitted =
          match pending with
          | [] -> emitted
          | _ -> Css.rule ~selector (List.rev pending) :: emitted
        in
        List.rev emitted
    | declaration :: rest -> (
        match fallback_declaration ~theme declaration with
        | None -> loop (declaration :: pending) emitted rest
        | Some fallback ->
            let base_declarations =
              match pending with
              | previous :: _
                when Css.Declaration.equal_declaration previous fallback ->
                  List.rev pending
              | _ -> List.rev (fallback :: pending)
            in
            let base = Css.rule ~selector base_declarations in
            let enhanced = Css.rule ~selector [ declaration ] in
            let supports =
              Css.supports ~condition:Tw.Color.color_mix_supports_condition
                [ enhanced ]
            in
            loop [] (supports :: base :: emitted) rest)
  in
  loop [] [] declarations

let authored_color_mix_fallbacks ~theme stmts =
  let rec lower_block stmts =
    List.concat_map
      (fun statement ->
        match Css.as_supports statement with
        | Some (condition, _)
          when Css.Supports.equal condition
                 Tw.Color.color_mix_supports_condition ->
            [ statement ]
        | _ -> (
            let statement =
              Css.Stylesheet.map_statement_children lower_block statement
            in
            match Css.as_rule statement with
            | Some (selector, declarations, []) ->
                lower_color_mix_rule ~theme selector declarations
            | _ -> [ statement ]))
      stmts
  in
  lower_block stmts

(* A theme token the author's own CSS reads has to be declared, the way one a
   utility reads is. [padding: --spacing(4)] expands to [calc(var(--spacing) *
   4)], and a sheet declaring no [--spacing] resolves it to nothing - the rule
   is right and the page renders unstyled, which is the shape every author-CSS
   defect here has taken.

   Only the author's own statements are scanned. The generated sheet already
   declares what it reads, and re-deriving it from the whole result is what
   over-emits: a [--default-*] token nothing set would arrive in the theme layer
   and move a fallback the utilities layer spells. *)
let author_theme_tokens ~theme stmts =
  Css.vars_of_rules stmts
  |> List.filter_map (fun (Css.V var) ->
      (* [var_name] is the bare name, without the [--] a declaration carries. *)
      let bare = Cascade.Css.var_name var in
      (* A reference token is declared somewhere else, by definition. *)
      if Tw.Scheme.is_reference_token theme bare then None
      else
        Option.map
          (fun value -> ("--" ^ bare, value))
          (Value_fn.theme_token_value theme bare))
  |> List.sort_uniq compare

(* Declared anywhere in [stmts], at any depth. *)
let declared_custom_properties stmts =
  Cascade.Css.Stylesheet.fold_declarations
    (fun names decls ->
      List.filter_map Cascade.Css.custom_declaration_name decls @ names)
    [] stmts

let declare_author_theme_tokens tokens stmts =
  let declared = declared_custom_properties stmts in
  match List.filter (fun (name, _) -> not (List.mem name declared)) tokens with
  | [] -> stmts
  | missing ->
      (* An [@layer theme] block, so [merge_named_layers] folds it into the
         generated sheet's own rather than leaving a second one behind. *)
      let decls =
        List.map
          (fun (name, value) -> Css.custom_property ~layer:"theme" name value)
          missing
      in
      let selector =
        Css.Selector.List [ Css.Selector.Root; Css.Selector.host () ]
      in
      Css.layer ~name:[ "theme" ] [ Css.rule ~selector decls ] :: stmts

(* The author's own top-level [@property] rules get the fallback block the
   generated sheet writes for its own. Appended, so [merge_named_layers] folds
   it into the sheet's [@layer properties] after the utilities' variables. *)
let add_author_property_fallbacks author stmts =
  match List.filter (fun s -> Option.is_some (Css.as_property s)) author with
  | [] -> stmts
  | rules -> stmts @ Tw.property_fallbacks rules

(* The statements of [generated] one import asks for, in the layer its [layer()]
   names or unlayered without one. The utilities bring what their rules need
   beside them: the [@layer properties] fallbacks and the [@property] and
   [@keyframes] rules. No part brings the components layer, which the generated
   sheet only ever declares empty. *)
let generated_part ?(with_base = false) part ~layer generated =
  let stmts = Css.statements generated in
  let layer_named name stmt =
    match Css.as_layer stmt with
    | Some (Some n, inner) when equal_layer n [ name ] -> Some inner
    | _ -> None
  in
  let contents name = List.concat (List.filter_map (layer_named name) stmts) in
  let place = function
    | [] -> []
    | inner -> (
        match layer with
        | None -> inner
        | Some name -> [ Css.layer ~name inner ])
  in
  let is_layer stmt =
    Option.is_some (Css.as_layer stmt)
    || Option.is_some (Css.layer_statement_name_list stmt)
  in
  match part with
  | Tailwind_import.Whole -> stmts
  | Tailwind_import.Theme_part -> place (contents "theme")
  | Tailwind_import.Preflight_part -> place (contents "base")
  | Tailwind_import.Utilities_part ->
      (* Without a preflight import the generated base layer holds only what a
         plugin adds, the forms reset, and Tailwind writes that in [@layer base]
         whichever part the entrypoint imports. *)
      let base =
        match contents "base" with
        | [] -> []
        | inner ->
            if with_base then [ Css.layer ~name:[ "base" ] inner ] else []
      in
      List.filter (fun s -> Option.is_some (layer_named "properties" s)) stmts
      @ base
      @ place (contents "utilities")
      @ List.filter (fun s -> not (is_layer s)) stmts

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

let splice_into_entrypoint ~theme ~path generated =
  match read_file path with
  | exception Sys_error _ -> generated
  | raw -> (
      let raw = Tailwind_import.utilities_as_import raw in
      let with_base = not (Tailwind_import.imports_preflight raw) in
      let css =
        Author_css.expand ~theme
          (Entry_theme.hoist_keyframes (Tailwind_import.strip_options raw))
      in
      match Css.of_string css with
      | Error _ -> generated
      | Ok p ->
          (* An imported file uses the same Tailwind syntax, and its [@variant]s
             may be declared in the entrypoint, so it gets the same treatment
             with those declarations in scope. *)
          let _, entry_defs =
            Custom_variant.take (Tailwind_import.strip_options raw)
          in
          let transform body =
            Author_css.expand ~extra_defs:entry_defs ~theme
              (Tailwind_import.strip_options
                 (Tailwind_import.utilities_as_import body))
          in
          let imports =
            preload_imports ~transform ~base_url:path p.Css.stylesheet
          in
          let loader = Css.Context.loader ~base_url:path ~imports () in
          (* Tailwind flattens the author's nesting, including what the expanded
             variants introduce, so match that shape. *)
          let inlined =
            Css.flatten_nesting (Css.inline_imports loader p.Css.stylesheet)
          in
          Css.statements inlined
          |> authored_color_mix_fallbacks ~theme
          |> List.concat_map (fun stmt ->
              match stmt with
              | Cascade.Stylesheet.Import { url; layer; _ } as s -> (
                  match Tailwind_import.part_of_url url with
                  | Some part -> generated_part ~with_base part ~layer generated
                  | None -> [ s ])
              | s -> [ s ])
          (* A stylesheet that only references the package declares no token,
             its own [@theme]'s included. *)
          |> declare_author_theme_tokens
               (if theme.Tw.Scheme.reference_theme then []
                else author_theme_tokens ~theme (Css.statements inlined))
          |> add_author_property_fallbacks (Css.statements inlined)
          |> lift_hoisted_out_of_layers |> merge_named_layers
          |> collapse_property_fallbacks |> join_theme_rules
          |> hoist_layer_blocks |> lead_properties_layer
          |> drop_unread_inline_tokens ~theme
          |> collect_properties_at_end |> Css.v)

(* Read the entrypoint's [@custom-variant] and [@utility] declarations. A
   project can redefine a built-in variant here (e.g. class-based [dark]) or
   declare a utility of its own; both govern the whole utility set, not only the
   author's own CSS. *)
let entry_defs take = function
  | None -> []
  | Some path -> (
      match read_file path with
      | exception Sys_error _ -> []
      | raw -> snd (take (Tailwind_import.strip_options raw)))

let entry_variant_defs = entry_defs Custom_variant.take
let entry_utility_defs = entry_defs Custom_utility.take
