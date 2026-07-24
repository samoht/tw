module Css = Cascade.Css
open Cascade_diff
open Cmdliner

(* Parse a whitespace-separated string of classes *)
let parse_classes ?(warn = true) ?(theme = Tw.Scheme.default) classes_str =
  let class_names = Tw_tools.Source_scan.split_whitespace classes_str in
  List.filter_map
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok style -> Some style
      | Error _ ->
          if warn then Fmt.epr "Warning: Unknown class '%s'@." cls;
          None)
    class_names

(* Recursively get files in directories *)
let rec files path patterns =
  if Sys.is_directory path then
    let entries = Sys.readdir path |> Array.to_list in
    List.concat_map
      (fun entry -> files (Filename.concat path entry) patterns)
      entries
  else if
    List.exists (fun pattern -> Filename.check_suffix path pattern) patterns
  then [ path ]
  else []

(* Generation backend - determines which tool to use *)
type backend =
  | Native (* Use our tw implementation *)
  | Tailwind (* Use real tailwindcss tool *)
  | Diff (* Compare tw vs tailwindcss *)

(* Main command implementation *)
type gen_opts = {
  minify : bool;
  optimize : bool;
  quiet : bool;
  css_mode : Tw.Css.mode;
  backend : backend;
  theme : Tw.Scheme.t;
      (** Theme used by tw's renderer, built from the project's --input-css so a
          --diff over a real repo compares against the same [@theme] Tailwind
          uses. Defaults to {!Tw.Scheme.default}. *)
  input_css : string option;
      (** Path to the project's CSS entrypoint, fed verbatim to the real
          Tailwind backend so both sides share the project config. *)
  input_css_path : string option;
      (** The entrypoint's own path, so tw can compile it (its rules and its
          relative [@import]s), not just read its [@theme]. *)
  diff_mode : Cascade_diff.Css_compare.mode;
      (** Comparison mode for --diff. [`Canonical] (default) ignores selector
          regrouping/reordering and is right for real-world parity sweeps;
          [`Auto]/[`Tree] (structural) reports regrouping, for tests that target
          it. *)
}

let read_file path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Rewrite each Tailwind [@theme [modifiers] {] header to a [:root {] rule.
   Cascade's parser accepts [@theme] but drops its body (it is not a standard
   at-rule), so this swaps only the at-rule keyword and its modifiers up to the
   opening brace; the declarations themselves are left for the real parser. *)
let theme_blocks_as_root css =
  let len = String.length css in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if !i + 6 <= len && String.sub css !i 6 = "@theme" then begin
      (* Skip the header up to the block's opening brace. *)
      let j = ref (!i + 6) in
      while !j < len && css.[!j] <> '{' && css.[!j] <> ';' do
        incr j
      done;
      if !j < len && css.[!j] = '{' then begin
        Buffer.add_string buf ":root ";
        i := !j (* keep the '{' and the body verbatim *)
      end
      else begin
        Buffer.add_char buf css.[!i];
        incr i
      end
    end
    else begin
      Buffer.add_char buf css.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* Extract @theme token overrides ([(bare-name, value)]) from a project CSS
   entrypoint, so tw renders with the same tokens Tailwind reads from it. The
   declarations are parsed by cascade (after the @theme -> :root header swap);
   the resulting name/value strings feed Scheme.with_overrides. *)
let theme_overrides_of_css css =
  match Css.of_string (theme_blocks_as_root css) with
  | Error _ -> []
  | Ok parse ->
      Css.rules_of_statements (Css.statements parse.Css.stylesheet)
      |> List.concat_map (fun (_sel, decls) ->
          List.filter_map
            (fun d ->
              match Css.custom_declaration_name d with
              | Some n when String.length n > 2 && String.sub n 0 2 = "--" ->
                  Some
                    ( String.sub n 2 (String.length n - 2),
                      Css.declaration_value d )
              | _ -> None)
            decls)

(* [@import "tailwindcss"] (and its subpath forms) is the package entry, not a
   file on disk: it marks where the generated theme/base/utilities belong. *)
let is_tailwind_import url =
  let u = Css.decode_import_url url in
  u = "tailwindcss" || String.starts_with ~prefix:"tailwindcss/" u

(* Tailwind extends [@import] with options CSS has no grammar for
   ([theme(static)], [source(none)], [prefix(tw)]). A CSS parser reads the
   trailing prelude as a media-query list, fails on it, and drops the whole
   statement, taking the import marker with it. Strip those options so the
   import survives as a plain one. *)
let strip_tailwind_import_options css =
  let len = String.length css in
  let buf = Buffer.create len in
  let opt_at i =
    List.find_opt
      (fun k ->
        i + String.length k <= len && String.sub css i (String.length k) = k)
      [ "theme("; "source("; "prefix(" ]
  in
  let rec skip_group i depth =
    if i >= len then i
    else if css.[i] = '(' then skip_group (i + 1) (depth + 1)
    else if css.[i] = ')' then
      if depth = 1 then i + 1 else skip_group (i + 1) (depth - 1)
    else skip_group (i + 1) depth
  in
  let rec go i in_import =
    if i >= len then ()
    else if (not in_import) && i + 7 <= len && String.sub css i 7 = "@import"
    then begin
      Buffer.add_string buf "@import";
      go (i + 7) true
    end
    else if in_import && css.[i] = ';' then begin
      Buffer.add_char buf ';';
      go (i + 1) false
    end
    else
      match if in_import then opt_at i else None with
      | Some k -> go (skip_group (i + String.length k) 1) in_import
      | None ->
          Buffer.add_char buf css.[i];
          go (i + 1) in_import
  in
  go 0 false;
  Buffer.contents buf

(* Tailwind's [@custom-variant NAME { ... @slot; ... }] declares a variant, and
   [@variant NAME { decls }] applies it inside author CSS. Both are Tailwind
   syntax, so a CSS parser drops them and the declarations they guard vanish.
   Expanding here keeps the [&] nesting for cascade to flatten.

   Only the built-in [dark] is known without a declaration; other names need one
   in the entrypoint. *)

let builtin_variants =
  [ ("dark", "@media (prefers-color-scheme: dark) { @slot; }") ]

(* Body of the [{ ... }] starting at [i] (the brace), and the index after it. *)
let block_at css i =
  let len = String.length css in
  let rec scan j depth =
    if j >= len then (String.sub css (i + 1) (len - i - 1), len)
    else
      match css.[j] with
      | '{' -> scan (j + 1) (depth + 1)
      | '}' ->
          if depth = 1 then (String.sub css (i + 1) (j - i - 1), j + 1)
          else scan (j + 1) (depth - 1)
      | _ -> scan (j + 1) depth
  in
  scan i 0

(* Body of the [( ... )] starting at [i] (the paren), and the index after it. *)
let block_paren_at css i =
  let len = String.length css in
  let rec scan j depth =
    if j >= len then (String.sub css (i + 1) (len - i - 1), len)
    else
      match css.[j] with
      | '(' -> scan (j + 1) (depth + 1)
      | ')' ->
          if depth = 1 then (String.sub css (i + 1) (j - i - 1), j + 1)
          else scan (j + 1) (depth - 1)
      | _ -> scan (j + 1) depth
  in
  scan i 0

(* [@at-keyword NAME {] header starting at [i]: the name and the brace index. *)
let at_rule_header css i keyword =
  let len = String.length css in
  let klen = String.length keyword in
  if i + klen > len || String.sub css i klen <> keyword then None
  else
    let rec brace j =
      if j >= len then None
      else if css.[j] = '{' then
        Some (String.trim (String.sub css (i + klen) (j - i - klen)), j)
      else if css.[j] = ';' then None
      else brace (j + 1)
    in
    brace (i + klen)

(* Pull out the [@custom-variant] declarations, dropping them from the CSS:
   Tailwind does not emit them either. *)
let take_custom_variants css =
  let len = String.length css in
  let buf = Buffer.create len in
  let defs = ref [] in
  let rec go i =
    if i >= len then ()
    else
      match at_rule_header css i "@custom-variant" with
      | Some (name, brace) when name <> "" ->
          let body, next = block_at css brace in
          defs := (name, body) :: !defs;
          go next
      | _ ->
          Buffer.add_char buf css.[i];
          go (i + 1)
  in
  go 0;
  (Buffer.contents buf, !defs)

(* Replace [@variant NAME { decls }] with the variant's body, substituting the
   declarations at each [@slot]. Nested [@variant]s expand outermost-first, so
   the recursion re-runs over the result. *)
let rec expand_variants ~depth defs css =
  if depth > 8 then css
  else
    let len = String.length css in
    let buf = Buffer.create len in
    let changed = ref false in
    let rec go i =
      if i >= len then ()
      else
        match at_rule_header css i "@variant" with
        | Some (name, brace) when List.mem_assoc name defs ->
            let body, next = block_at css brace in
            let template = List.assoc name defs in
            changed := true;
            Buffer.add_string buf (fill_slots template body);
            go next
        | _ ->
            Buffer.add_char buf css.[i];
            go (i + 1)
    in
    go 0;
    let out = Buffer.contents buf in
    if !changed then expand_variants ~depth:(depth + 1) defs out else out

and fill_slots template body =
  let len = String.length template in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else if i + 5 <= len && String.sub template i 5 = "@slot" then begin
      Buffer.add_string buf body;
      (* swallow the terminating [;] so the slot does not leave a stray one *)
      let j = ref (i + 5) in
      while !j < len && (template.[!j] = ' ' || template.[!j] = '\n') do
        incr j
      done;
      go (if !j < len && template.[!j] = ';' then !j + 1 else !j)
    end
    else begin
      Buffer.add_char buf template.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buf

(* Tailwind's [--spacing(N)] is shorthand for the spacing scale. It is not CSS,
   so a parser rejects the declaration and it drops out of the output. *)
let expand_spacing_fn css =
  let len = String.length css in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then ()
    else if i + 10 <= len && String.sub css i 10 = "--spacing(" then begin
      let body, next = block_paren_at css (i + 9) in
      Buffer.add_string buf
        (String.concat "" [ "calc(var(--spacing) * "; body; ")" ]);
      go next
    end
    else begin
      Buffer.add_char buf css.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buf

let apply_variants css =
  let css, defs = take_custom_variants css in
  (* A project declaration wins over the built-in of the same name. *)
  let defs = defs @ builtin_variants in
  expand_spacing_fn (expand_variants ~depth:0 defs css)

(* Preload every transitively-referenced stylesheet, keyed by the URL resolved
   against its importer, which is what the inliner looks up. Mirrors cascade's
   own filesystem loader. A package import has no file and stays unresolved on
   purpose, so the splice below can find it. *)
let preload_imports ~base_url stylesheet =
  let imports = Hashtbl.create 16 in
  let rec scan_under base sheet =
    let loader = Css.Context.loader ~base_url:base () in
    Css.fold (scan_stmt loader) () sheet
  and scan_stmt loader () stmt =
    match Css.as_import stmt with
    | Some ir when not (is_tailwind_import ir.url) ->
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
let splice_into_entrypoint ~path generated =
  match read_file path with
  | exception Sys_error _ -> generated
  | raw -> (
      let css = apply_variants (strip_tailwind_import_options raw) in
      match Css.of_string css with
      | Error _ -> generated
      | Ok p ->
          let imports = preload_imports ~base_url:path p.Css.stylesheet in
          let loader = Css.Context.loader ~base_url:path ~imports () in
          (* Tailwind flattens the author's nesting, including what the expanded
             variants introduce, so match that shape. *)
          let inlined =
            Css.flatten_nesting (Css.inline_imports loader p.Css.stylesheet)
          in
          Css.statements inlined
          |> List.concat_map (fun stmt ->
              match stmt with
              | Cascade.Stylesheet.Import { url; _ } when is_tailwind_import url
                ->
                  Css.statements generated
              | s -> [ s ])
          |> Css.v)

let eval_flag flag ~default =
  match flag with `Enable -> true | `Disable -> false | `Default -> default

let print_diff_result label diff =
  match diff.Css_compare.result with
  | Css_compare.No_diff _ -> Fmt.pr "✓ No differences found%s@." label
  | _ ->
      Fmt.pr "Differences found%s:@.@." label;
      let buf = Buffer.create 256 in
      Css_compare.pp ~expected:"Tailwind" ~actual:"tw" buf diff;
      print_string (Buffer.contents buf);
      Fmt.pr "@."

let render_css ~(opts : gen_opts) stylesheet =
  let stylesheet =
    match opts.css_mode with
    | Inline -> Tw.Css.inline_vars stylesheet
    | Variables -> stylesheet
  in
  let stylesheet =
    if opts.optimize then
      (* The generated sheet is a closed author stylesheet, so prune theme
         tokens nothing references -- e.g. --spacing once p-0 folds to 0, which
         Tailwind also drops. *)
      Tw.Css.optimize ~prune_unused_custom_props:true stylesheet
    else stylesheet
  in
  Tw.Css.to_string ~minify:opts.minify stylesheet

(* Surface of_string's specific message (e.g. the actionable arbitrary-property
   feedback) for a single unknown class; fall back to a generic message. *)
let unknown_class_error class_str =
  match Tw.of_string class_str with
  | Error (`Msg m) -> Fmt.str "Error: %s" m
  | Ok _ -> Fmt.str "Error: Unknown class: %s" class_str

let diff_single_class class_str ~(opts : gen_opts) =
  try
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true ?input_css:opts.input_css [ class_str ]
    in
    let tw_styles = parse_classes ~warn:false ~theme:opts.theme class_str in
    let styles = match tw_styles with [] -> [] | s -> s in
    let stylesheet = Tw.to_css ~theme:opts.theme ~base:true styles in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Css_compare.diff ~mode:opts.diff_mode ~prune_unused_custom_props:true
        legacy_css our_css
    in
    match tw_styles with
    | [] when class_str = "" ->
        print_diff_result " (empty/base only)" diff;
        `Ok ()
    | [] -> `Error (false, unknown_class_error class_str)
    | _ ->
        print_diff_result
          (Fmt.str " between Tailwind and tw for '%s'" class_str)
          diff;
        `Ok ()
  with e ->
    `Error (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e))

let process_single_class class_str flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_single_class class_str ~opts
  | Tailwind -> (
      try
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true [ class_str ]
        in
        print_string css;
        `Ok ()
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (Printexc.to_string e)
          ))
  | Native -> (
      let include_base = eval_flag flag ~default:false in
      let tw_styles = parse_classes ~warn:false class_str in
      let styles = match tw_styles with [] -> [] | s -> s in
      match tw_styles with
      | [] when class_str <> "" -> `Error (false, unknown_class_error class_str)
      | _ ->
          let stylesheet = Tw.to_css ~base:include_base styles in
          print_string (render_css ~opts stylesheet);
          `Ok ())

let collect_files paths =
  List.concat_map
    (fun path ->
      if Sys.file_exists path then
        if Sys.is_directory path then
          (* Classes live outside component sources too: a docs site keeps most
             of its markup in .md/.mdx, and plain .ts/.js hold class strings
             just as .tsx does. Skipping them emits a fraction of the utilities
             the project uses, with nothing to say so. *)
          files path
            [
              ".html";
              ".eml";
              ".ml";
              ".re";
              ".js";
              ".jsx";
              ".ts";
              ".tsx";
              ".vue";
              ".svelte";
              ".md";
              ".mdx";
            ]
        else [ path ]
      else [])
    paths

let print_stats ~quiet ~candidate_count ~known_count =
  if (not quiet) && known_count = 0 && candidate_count > 0 then (
    Fmt.epr "@.--- Statistics ---%@.";
    Fmt.epr "Candidate tokens scanned: %d@." candidate_count;
    Fmt.epr "Successfully parsed: %d@." known_count)

let parse_known_candidates ?(theme = Tw.Scheme.default) candidates =
  List.filter_map
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Ok style -> Some (cls, style)
      | Error _ -> None)
    candidates

let diff_files paths ~(opts : gen_opts) =
  try
    let all_files = collect_files paths in
    let all_classes =
      List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
      |> List.sort_uniq String.compare
    in
    let legacy_css =
      Tw_tools.Tailwind_gen.generate ~minify:opts.minify ~optimize:opts.optimize
        ~forms:true ?input_css:opts.input_css all_classes
    in
    let tw_styles =
      parse_known_candidates ~theme:opts.theme all_classes |> List.map snd
    in
    let stylesheet = Tw.to_css ~theme:opts.theme ~base:true tw_styles in
    let our_css = render_css ~opts stylesheet in
    let diff =
      Css_compare.diff ~mode:opts.diff_mode ~prune_unused_custom_props:true
        legacy_css our_css
    in
    print_diff_result "" diff;
    `Ok ()
  with e ->
    `Error (false, Fmt.str "Error during comparison: %s" (Printexc.to_string e))

let native_files paths flag ~(opts : gen_opts) =
  let include_base = eval_flag flag ~default:true in
  try
    let all_files = collect_files paths in
    let all_classes =
      List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
      |> List.sort_uniq String.compare
    in
    let known = parse_known_candidates all_classes in
    let tw_styles = List.map snd known in
    let stylesheet = Tw.to_css ~base:include_base tw_styles in
    let stylesheet =
      match opts.input_css_path with
      | Some path -> splice_into_entrypoint ~path stylesheet
      | None -> stylesheet
    in
    print_string (render_css ~opts stylesheet);
    print_stats ~quiet:opts.quiet ~candidate_count:(List.length all_classes)
      ~known_count:(List.length known);
    `Ok ()
  with e -> `Error (false, Fmt.str "Error: %s" (Printexc.to_string e))

let process_files paths flag ~(opts : gen_opts) =
  match opts.backend with
  | Diff -> diff_files paths ~opts
  | Tailwind -> (
      try
        let all_files = collect_files paths in
        let all_classes =
          List.concat_map Tw_tools.Source_scan.candidates_from_file all_files
          |> List.sort_uniq String.compare
        in
        let css =
          Tw_tools.Tailwind_gen.generate ~minify:opts.minify
            ~optimize:opts.optimize ~forms:true all_classes
        in
        print_string css;
        `Ok ()
      with e ->
        `Error
          ( false,
            Fmt.str "Error generating with Tailwind: %s" (Printexc.to_string e)
          ))
  | Native -> native_files paths flag ~opts

let tw_main single_class base_flag ~css_mode ~minify ~optimize ~quiet ~backend
    ~input_css ~diff_mode paths =
  (* Resolve default CSS mode based on operation kind when not provided *)
  let resolved_css_mode : Css.mode =
    match (single_class, backend, css_mode) with
    | _, Diff, _ -> Variables (* Diff always uses variables mode *)
    | _, _, `Inline -> Inline
    | _, _, `Variables -> Variables
    | Some _, _, `Default -> Inline (* single-class defaults to inline mode *)
    | None, _, `Default -> Variables (* files/scan default to variables *)
  in
  (* Diff mode forces minified output; Cascade handles semantic comparison. *)
  let resolved_minify = match backend with Diff -> true | _ -> minify in
  let resolved_optimize = optimize in
  (* Build the renderer theme from the project's CSS entrypoint (its @theme), so
     a --diff over a real repo compares against the same tokens Tailwind
     uses. *)
  let css_content = Option.map read_file input_css in
  let theme =
    match css_content with
    | None -> Tw.Scheme.default
    | Some css ->
        Tw.Scheme.with_overrides Tw.Scheme.default (theme_overrides_of_css css)
  in
  let opts : gen_opts =
    {
      minify = resolved_minify;
      optimize = resolved_optimize;
      quiet;
      css_mode = resolved_css_mode;
      backend;
      theme;
      input_css = css_content;
      input_css_path = input_css;
      diff_mode;
    }
  in
  match single_class with
  | Some class_str -> process_single_class class_str base_flag ~opts
  | None -> (
      match paths with
      | [] -> `Error (true, "Either provide -s <class> or file/directory paths")
      | paths -> process_files paths base_flag ~opts)

(* Command-line arguments *)
let single_flag =
  let doc = "Generate CSS for a single Tailwind class" in
  Arg.(
    value & opt (some string) None & info [ "s"; "single" ] ~docv:"CLASS" ~doc)

let base_flag =
  Arg.(
    value
    & vflag `Default
        [
          ( `Enable,
            info [ "base" ]
              ~doc:
                "Include the Base layer (Preflight CSS reset and semantic \
                 defaults)" );
          (`Disable, info [ "no-base" ] ~doc:"Exclude the Base layer");
        ])

let minify_flag =
  let doc = "Minify the generated CSS output" in
  Arg.(value & flag & info [ "minify" ] ~doc)

let optimize_flag =
  let doc =
    "Pass CSS optimization through to the Tailwind backend. tw output is not \
     pre-optimized."
  in
  Arg.(value & flag & info [ "optimize" ] ~doc)

let quiet_flag =
  let doc = "Suppress warnings about unknown classes" in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let input_css_arg =
  let doc =
    "Project CSS entrypoint to feed to Tailwind during --diff. @theme blocks \
     are also used to configure tw's renderer."
  in
  Arg.(value & opt (some file) None & info [ "input-css" ] ~docv:"CSS" ~doc)

let tailwind_flag =
  let doc_tailwind = "Use the real tailwindcss tool to generate CSS" in
  Arg.(value & flag & info [ "tailwind" ] ~doc:doc_tailwind)

let diff_flag =
  let doc = "Compare tw output with real Tailwind CSS." in
  Arg.(value & flag & info [ "diff" ] ~doc)

let diff_mode_arg =
  let doc =
    "CSS comparison mode for --diff: canonical (default, ignores selector \
     regrouping/reordering, right for real-world parity sweeps), auto, \
     tree/structural (reports regrouping), or string."
  in
  let mode_conv =
    Arg.enum
      [
        ("canonical", `Canonical);
        ("auto", `Auto);
        ("tree", `Tree);
        ("structural", `Tree);
        ("string", `String);
      ]
  in
  Arg.(
    value & opt mode_conv `Canonical & info [ "diff-mode" ] ~docv:"MODE" ~doc)

let css_mode_vflag =
  let doc_inline = "Inline mode: resolve values (no variables), no layers." in
  let doc_vars = "Variables mode: emit CSS variables and layered output." in
  Arg.(
    value
    & vflag `Default
        [
          (`Inline, info [ "inline" ] ~doc:doc_inline);
          (`Variables, info [ "variables" ] ~doc:doc_vars);
        ])

let paths_arg =
  let doc = "Files or directories to scan for Tailwind classes" in
  Arg.(value & pos_all file [] & info [] ~docv:"PATH" ~doc)

let man =
  [
    `S Manpage.s_description;
    `P "tw is a tool that generates CSS from Tailwind-like utility classes.";
    `P
      "It can generate CSS for a single class using -s (no base styles by \
       default), or scan files/directories and generate a complete stylesheet \
       (with base styles by default).";
    `S Manpage.s_examples;
    `P "Generate CSS for a single class (no Base layer by default):";
    `Pre "  tw -s bg-blue-500";
    `P "Generate CSS for a single class with the Base layer:";
    `Pre "  tw -s bg-blue-500 --base";
    `P "Scan files and generate CSS (with the Base layer by default):";
    `Pre "  tw index.html src/";
    `P "Scan files and generate CSS without the Base layer:";
    `Pre "  tw --no-base index.html src/";
    `P "Generate inline mode (no variables, no layers):";
    `Pre "  tw --inline index.html src/";
    `P "Generate minified CSS:";
    `Pre "  tw --minify index.html src/";
    `P "Generate optimized CSS (rule merging/deduplication):";
    `Pre "  tw --optimize index.html src/";
    `P "Generate both minified and optimized CSS:";
    `Pre "  tw --minify --optimize index.html src/";
    `P "Use real Tailwind CSS:";
    `Pre "  tw -s bg-blue-500 --tailwind";
    `P "Compare tw output with real Tailwind CSS:";
    `Pre "  tw -s prose-sm --diff --diff-mode=canonical";
    `P "Use structural diff output when regrouping/order is relevant:";
    `Pre "  tw -s prose-sm --diff --diff-mode=tree";
    `S Manpage.s_see_also;
    `P "https://tailwindcss.com";
  ]

let cmd =
  let doc = "A Tailwind CSS-like utility class generator for OCaml" in
  let info = Cmd.info "tw" ~version:Tw_info.version ~doc ~man in
  Cmd.v info
    Term.(
      ret
        (const (fun s b css_m m o q tailwind diff diff_mode input_css paths ->
             let backend, diff_mode =
               if diff then (Diff, diff_mode)
               else
                 let backend = if tailwind then Tailwind else Native in
                 (backend, `Canonical)
             in
             tw_main s b ~css_mode:css_m ~minify:m ~optimize:o ~quiet:q ~backend
               ~diff_mode ~input_css paths)
        $ single_flag $ base_flag $ css_mode_vflag $ minify_flag $ optimize_flag
        $ quiet_flag $ tailwind_flag $ diff_flag $ diff_mode_arg $ input_css_arg
        $ paths_arg))

let normalize_argv argv =
  argv |> Array.to_list
  |> List.concat_map (fun arg ->
      let prefix = "--diff=" in
      let prefix_len = String.length prefix in
      if String.length arg > prefix_len && String.sub arg 0 prefix_len = prefix
      then
        let mode = String.sub arg prefix_len (String.length arg - prefix_len) in
        [ "--diff"; "--diff-mode=" ^ mode ]
      else [ arg ])
  |> Array.of_list

let () = exit (Cmd.eval ~argv:(normalize_argv Sys.argv) cmd)
