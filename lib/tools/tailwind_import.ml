module Css = Cascade.Css
module Index = Cascade.Source_index

let split_top_level = Tw.Parse.split_top_level

(* [@import "tailwindcss"] and its subpath forms are the package, not a file on
   disk: each marks where part of the generated sheet belongs. The package's own
   [index.css] imports [theme.css], [preflight.css] and [utilities.css] in turn,
   so the bare import is all three, each file is its own part, and any other
   subpath is read as the package. *)
type part = Whole | Theme_part | Preflight_part | Utilities_part

let part_of_url url =
  let u = Css.decode_import_url url in
  match Filename.remove_extension u with
  | "tailwindcss" -> Some Whole
  | "tailwindcss/theme" -> Some Theme_part
  | "tailwindcss/preflight" -> Some Preflight_part
  | "tailwindcss/utilities" -> Some Utilities_part
  | _ when String.starts_with ~prefix:"tailwindcss/" u -> Some Whole
  | _ -> None

let is_package_url url = Option.is_some (part_of_url url)

(* [@import "tailwindcss" theme(static)] asks for the whole theme, not only the
   variables a utility used. The option is not CSS, so it is read off the
   import's own [theme()] call rather than from a parsed stylesheet. *)
let imports_static_theme css =
  let index = Index.v css in
  let static =
    Index.calls index ~name:"theme"
    |> List.filter (fun (_, (block : Index.block)) ->
        String.trim block.body = "static")
  in
  Index.at_statements index ~name:"@import"
  |> List.exists (fun (at, (import : Index.statement)) ->
      List.exists (fun (i, _) -> at < i && i < import.next) static)

(* [@import "tailwindcss" prefix(tw)] asks for [tw:] in front of every candidate
   and [--tw-] in front of every theme token. Read the same way [theme(static)]
   is: the option function sits inside the import statement. *)
let import_prefix css =
  let index = Index.v css in
  let prefixes = Index.calls index ~name:"prefix" in
  Index.at_statements index ~name:"@import"
  |> List.filter_map (fun (at, (import : Index.statement)) ->
      List.find_map
        (fun (i, (block : Index.block)) ->
          if at < i && i < import.next then
            match String.trim block.body with "" -> None | name -> Some name
          else None)
        prefixes)
  |> function
  | name :: _ -> Some name
  | [] -> None

(* [@import "tailwindcss" important] marks every utility declaration
   [!important]. The option is a bare word in the import's prelude rather than a
   call, so it is read off the prelude's top-level words. *)
let imports_important css =
  let index = Index.v css in
  let blank = function '\n' | '\t' | '\r' | '\012' -> ' ' | c -> c in
  Index.at_statements index ~name:"@import"
  |> List.exists (fun (_, (import : Index.statement)) ->
      List.mem "important"
        (split_top_level ' ' (String.map blank import.prelude)))

(* [@reference "tailwindcss"] brings the theme into scope for [@apply] without
   emitting any of it. The prelude is the URL alone. *)
let references_tailwind css =
  let index = Index.v css in
  Index.at_statements index ~name:"@reference"
  |> List.exists (fun (_, (statement : Index.statement)) ->
      is_package_url (String.trim statement.prelude))

(* [@plugin "@tailwindcss/forms"] resets native form controls in the base layer,
   which is the plugin's default strategy. An options block naming [strategy:
   "class"] leaves the controls alone and styles only the [form-*] classes. *)
let forms_base css =
  let index = Index.v css in
  let unquote s = Text_pass.unquote (String.trim s) in
  let names_forms prelude =
    String.equal (unquote prelude) "@tailwindcss/forms"
  in
  let class_strategy body =
    split_top_level ';' body
    |> List.exists (fun option ->
        match String.index_opt option ':' with
        | None -> false
        | Some i ->
            let value =
              String.sub option (i + 1) (String.length option - i - 1)
            in
            String.equal (String.trim (String.sub option 0 i)) "strategy"
            && String.equal (unquote value) "class")
  in
  let len = String.length css in
  let rec block i =
    if i >= len then false
    else
      match Index.at_rule index ~name:"@plugin" i with
      | Some { prelude; block = { body; next }; _ } when names_forms prelude ->
          (not (class_strategy body)) || block next
      | _ -> block (i + 1)
  in
  Index.at_statements index ~name:"@plugin"
  |> List.exists (fun (_, (statement : Index.statement)) ->
      names_forms statement.prelude)
  || block 0

(* The JavaScript configs the entrypoint's [@config] directives name, as written
   and in source order. *)
let config_directives css =
  let index = Index.v css in
  Index.at_statements index ~name:"@config"
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map (fun (_, (statement : Index.statement)) ->
      String.trim statement.prelude)

(* Tailwind extends [@import] with options CSS has no grammar for
   ([theme(static)], [source(none)], [prefix(tw)]). Strip actual option function
   tokens from actual import statements so quoted parentheses and comments do
   not alter their boundaries. *)
let strip_options css =
  Text_pass.rewrite ~names:[ "@import" ] css (fun index buf i ->
      let option_at i =
        List.find_map
          (fun name -> Index.call index ~name i)
          [ "theme"; "source"; "prefix" ]
      in
      let rec copy_import i stop =
        if i >= stop then ()
        else
          match option_at i with
          | Some { next; _ } when next <= stop -> copy_import next stop
          | Some _ | None ->
              Buffer.add_char buf css.[i];
              copy_import (i + 1) stop
      in
      match Index.at_statement index ~name:"@import" i with
      | Some { next; _ } ->
          copy_import i next;
          Some next
      | None -> None)

(* [@tailwind utilities] is all [tailwindcss/utilities.css] holds, so it is read
   as that import, where the directive stands. *)
let utilities_as_import css =
  let index = Index.v css in
  let len = String.length css in
  let buf = Buffer.create len in
  let asks_for_utilities (statement : Index.statement) =
    match split_top_level ' ' (String.trim statement.prelude) with
    | "utilities" :: _ -> true
    | _ -> false
  in
  let rec go i =
    if i >= len then ()
    else
      match Index.at_statement index ~name:"@tailwind" i with
      | Some statement when asks_for_utilities statement ->
          Buffer.add_string buf "@import \"tailwindcss/utilities\";";
          go statement.next
      | _ ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  Buffer.contents buf

let parts css =
  match Css.of_string (strip_options (utilities_as_import css)) with
  | Error _ -> []
  | Ok parse ->
      List.filter_map
        (fun stmt ->
          Option.bind (Css.as_import stmt)
            (fun (ir : Css.Stylesheet.import_rule) -> part_of_url ir.url))
        (Css.statements parse.Css.stylesheet)

let imports_preflight css =
  match parts css with
  | [] -> true
  | parts ->
      List.exists
        (function
          | Whole | Preflight_part -> true
          | Theme_part | Utilities_part -> false)
        parts
