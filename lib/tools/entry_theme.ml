module Css = Cascade.Css
module Components = Text_pass.Components
module Index = Cascade.Source_index

(* [--<ns>-*: initial] takes a whole [@theme] namespace out of the theme, and
   [--<ns>-*] is not a custom-property name: the [*] ends the ident, so "consume
   a declaration" (CSS Syntax 3 sec. 5.5.15) finds no [:] where it wants one and
   the block loses the reset. The token stream still has it, so it is read from
   there, keyed by where it starts so it can go back among the declarations in
   the order the block wrote them. *)
let namespace_resets body =
  let components = Components.array body in
  let text from upto = String.trim (String.sub body from (upto - from)) in
  (* The reset an ident at [i] opens, and the index to carry on from. *)
  let reset i name acc =
    let star = Components.after_whitespace components (i + 1) in
    let colon = Components.after_whitespace components (star + 1) in
    if
      colon < Array.length components
      && (match components.(star) with
        | Cascade.Component.Preserved { kind = Cascade.Token.Delim "*"; _ } ->
            true
        | _ -> false)
      &&
      match components.(colon) with
      | Cascade.Component.Preserved { kind = Cascade.Token.Colon; _ } -> true
      | _ -> false
    then
      let last = Components.semicolon_from components (colon + 1) in
      let upto =
        if last < Array.length components then
          Components.start components.(last)
        else String.length body
      in
      let namespace = String.sub name 2 (String.length name - 2) ^ "*" in
      ( last + 1,
        ( Components.start components.(i),
          (namespace, text (Components.stop components.(colon)) upto) )
        :: acc )
    else (i + 1, acc)
  in
  (* Only the component values of the block itself are visited. Nested rules and
     bracket groups are each one component and therefore stepped past. *)
  let rec go i acc =
    if i >= Array.length components then List.rev acc
    else
      match components.(i) with
      | Cascade.Component.Preserved { kind = Cascade.Token.Ident name; _ }
        when String.length name >= 2 && String.equal (String.sub name 0 2) "--"
        ->
          let i, acc = reset i name acc in
          go i acc
      | _ -> go (i + 1) acc
  in
  go 0 []

(* Tailwind's [@theme] is not a CSS at-rule, so cascade keeps it whole rather
   than interpreting it (CSS Syntax 3 sec. 5.5.2, "consume an at-rule"): the
   prelude carries the modifiers, the block its declarations, both as the source
   text they were written as. *)
let block = function
  | Cascade.Stylesheet.Unknown_at_rule
      { name = "theme"; prelude; block = Some body } ->
      Some (prelude, body)
  | _ -> None

(* The [(bare-name, value)] pairs a [@theme] block declares. Its body is a
   declaration list, read here the way a rule's body is read, so reading carries
   on past a nested at-rule such as the [@keyframes] a project writes beside its
   [--animate-*] token. The namespace resets come off the token stream and are
   merged back in by where each one starts. *)
let tokens body =
  let declared =
    Cascade.Reader.of_string body
    |> Cascade.Parser.block_contents
    |> (fun out -> out.Cascade.Parser.value)
    |> List.concat_map (function `Decls decls -> decls | `Rule _ -> [])
    |> List.filter_map (fun (decl : Cascade.Component.declaration) ->
        let { Cascade.Component.name; value; _ } =
          decl.Cascade.Component.node
        in
        Option.map
          (fun bare ->
            ( decl.Cascade.Component.loc.Cascade.Loc.start_pos,
              (bare, Cascade.Parser.string_of_components value) ))
          (Tw.Parse.bare_name name))
  in
  List.stable_sort
    (fun (a, _) (b, _) -> Int.compare a b)
    (declared @ namespace_resets body)
  |> List.map snd

(* Extract @theme token overrides from a project CSS entrypoint, so tw renders
   with the same tokens Tailwind reads from it: the pairs every block declares,
   and the names among them that came from an [@theme inline] block. The
   resulting strings feed Scheme.with_overrides. *)
let blocks css =
  match Css.of_string css with
  | Error _ -> []
  | Ok parse ->
      Css.statements parse.Css.stylesheet
      |> List.filter_map block
      |> List.map (fun (prelude, body) ->
          (Tw.split_whitespace prelude, tokens body))

(* The names the blocks carrying the modifier [option] declare. *)
let tokens_with option blocks =
  List.concat_map
    (fun (options, tokens) ->
      if List.mem option options then List.map fst tokens else [])
    blocks

let overrides_of_css css =
  let blocks = blocks css in
  (List.concat_map snd blocks, tokens_with "inline" blocks)

(* A project can declare [@keyframes] inside its [@theme] block, beside the
   [--animate-*] token that names it. [@theme] is a build-time directive, so
   [drop_directives] takes the whole block out of the emitted CSS; lift actual
   keyframe at-rules to the top level first, where Tailwind emits them. *)
let hoist_keyframes css =
  let lifted = Buffer.create 0 in
  let css =
    Text_pass.rewrite ~names:[ "@theme" ] css (fun index buf i ->
        match Index.at_rule index ~name:"@theme" i with
        | Some { brace; block = { next = stop; _ }; _ } ->
            Buffer.add_string buf (String.sub css i (brace + 1 - i));
            let rec go_theme i =
              if i >= stop then ()
              else
                match Index.at_rule index ~name:"@keyframes" i with
                | Some { block = { next; _ }; _ } when next <= stop ->
                    Buffer.add_string lifted (String.sub css i (next - i));
                    go_theme next
                | Some _ | None ->
                    Buffer.add_char buf css.[i];
                    go_theme (i + 1)
            in
            go_theme (brace + 1);
            Some stop
        | None -> None)
  in
  css ^ Buffer.contents lifted

(* The theme a project's entrypoint asks for: its [\@theme] overrides, whether
   it imported [theme(static)], and the prefix it named. One function so the CLI
   and anything measuring the CLI build the same theme from the same file rather
   than two that drift. *)
let of_css css =
  let overrides, inline = overrides_of_css css in
  let base =
    if Tailwind_import.imports_static_theme css then
      { Tw.Scheme.default with static_theme = true }
    else Tw.Scheme.default
  in
  let base =
    {
      base with
      prefix = Tailwind_import.import_prefix css;
      important = Tailwind_import.imports_important css;
      reference_theme =
        Tailwind_import.references_tailwind css
        && Tailwind_import.parts css = [];
    }
  in
  let blocks = blocks css in
  let static = tokens_with "static" blocks in
  let reference = tokens_with "reference" blocks in
  Tw.Scheme.with_overrides ~inline ~reference ~static base overrides
