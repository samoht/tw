module Css = Cascade.Css

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
    | Some (Some n, inner) when Css.Stylesheet.equal_layer_name n [ name ] ->
        Some inner
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

(* Compile the project's CSS entrypoint instead of only reading its [@theme].
   Tailwind treats that file as the stylesheet: its own rules and its relative
   [@import]s are part of the output, and [@import "tailwindcss"] is where the
   generated sheet goes. Reading it for tokens alone silently dropped every rule
   the project wrote. *)
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
          |> Sheet_shape.tidy ~theme |> Css.v)

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
