module Index = Cascade.Source_index

(* [@source inline("...")] is the safelist and [@source not inline("...")] the
   blocklist. The argument is read off the [inline()] call inside the statement,
   and what stands between the at-keyword and that call says which of the two it
   is; anything else there is not the option. *)
let inline css =
  let index = Index.v css in
  let calls = Index.calls index ~name:"inline" in
  let keyword = String.length "@source" in
  let argument at (source : Index.statement) =
    List.find_map
      (fun (i, (call : Index.block)) ->
        if at + keyword <= i && call.next <= source.next then
          match
            String.trim (String.sub css (at + keyword) (i - at - keyword))
          with
          | "" -> Some (false, call.body)
          | "not" -> Some (true, call.body)
          | _ -> None
        else None)
      calls
  in
  Index.at_statements index ~name:"@source"
  |> List.fold_left
       (fun (safelist, blocklist) (at, source) ->
         match argument at source with
         | None -> (safelist, blocklist)
         | Some (negated, body) -> (
             match Text_pass.quoted_contents body with
             | None -> (safelist, blocklist)
             | Some patterns ->
                 let candidates =
                   Tw.Parse.split_top_level ' ' patterns
                   |> List.filter (fun p -> p <> "")
                   |> List.concat_map Brace.expand
                 in
                 if negated then (safelist, blocklist @ candidates)
                 else (safelist @ candidates, blocklist)))
       ([], [])

(* [@source "<path>"] names files to scan for candidates, relative to the
   stylesheet, and [@source not "<path>"] takes files back out. The [inline()]
   forms are the safelist above: their argument is a call, not a quoted path. *)
let paths css =
  let index = Index.v css in
  let blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  Index.at_statements index ~name:"@source"
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.fold_left
       (fun (included, excluded) (_, (source : Index.statement)) ->
         let prelude = String.trim source.prelude in
         let n = String.length prelude in
         let negated, argument =
           if
             n > 3
             && String.starts_with ~prefix:"not" prelude
             && blank prelude.[3]
           then (true, String.sub prelude 3 (n - 3))
           else (false, prelude)
         in
         match Text_pass.quoted_contents argument with
         | None -> (included, excluded)
         | Some path when negated -> (included, excluded @ [ path ])
         | Some path -> (included @ [ path ], excluded))
       ([], [])

(* The directives whose argument is a path the CLI resolves against the
   stylesheet. [@source not "../x"] names one too; [@source inline("...")] names
   candidates, not a path. *)
let path_directives =
  [ "@import"; "@source"; "@plugin"; "@config"; "@reference" ]

let is_relative_path path =
  String.starts_with ~prefix:"./" path || String.starts_with ~prefix:"../" path

(* Where the quoted path of the directive at [at] opens and closes, when what
   stands before the quote is blank or, for [@source], the [not] keyword. *)
let quoted_path_span css ~name ~at ~next =
  let len = String.length css in
  let blank c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let rec skip_blank i =
    if i < next && blank css.[i] then skip_blank (i + 1) else i
  in
  let i = skip_blank (at + String.length name) in
  let i =
    if
      String.equal name "@source"
      && i + 3 < len
      && String.sub css i 3 = "not"
      && blank css.[i + 3]
    then skip_blank (i + 3)
    else i
  in
  if i < next && (css.[i] = '"' || css.[i] = '\'') then
    match String.index_from_opt css (i + 1) css.[i] with
    | Some close when close < next -> Some (i + 1, close)
    | _ -> None
  else None

let rooted ~dir css =
  let index = Index.v css in
  let spans =
    List.concat_map
      (fun name ->
        Index.at_statements index ~name
        |> List.filter_map (fun (at, (statement : Index.statement)) ->
            match quoted_path_span css ~name ~at ~next:statement.next with
            | Some (start, stop) ->
                let path = String.sub css start (stop - start) in
                if is_relative_path path then Some (start, stop, path) else None
            | None -> None))
      path_directives
    |> List.sort (fun (a, _, _) (b, _, _) -> Int.compare a b)
  in
  let buf = Buffer.create (String.length css) in
  let last =
    List.fold_left
      (fun pos (start, stop, path) ->
        Buffer.add_string buf (String.sub css pos (start - pos));
        Buffer.add_string buf (Filename.concat dir path);
        stop)
      0 spans
  in
  Buffer.add_string buf (String.sub css last (String.length css - last));
  Buffer.contents buf

(* [@import "tailwindcss" source(none)] turns automatic source detection off,
   and [source("../src")] moves where it starts. The option sits inside the
   import statement, or inside [@tailwind utilities] for a sheet importing the
   parts. *)
let root css =
  let index = Index.v css in
  let sources = Index.calls index ~name:"source" in
  let option (at, (statement : Index.statement)) =
    List.find_map
      (fun (i, (block : Index.block)) ->
        if at < i && i < statement.next then
          match String.trim block.body with
          | "none" -> Some `None
          | body ->
              Option.map (fun dir -> `Dir dir) (Text_pass.quoted_contents body)
        else None)
      sources
  in
  Index.at_statements index ~name:"@import"
  @ Index.at_statements index ~name:"@tailwind"
  |> List.find_map option
  |> Option.value ~default:`Detect
