module Index = Cascade.Source_index

(* Tailwind's [@custom-variant NAME { ... @slot; ... }] and selector shorthand
   [@custom-variant NAME (...);] declare variants, and [@variant NAME { decls }]
   applies one inside author CSS. These are Tailwind syntax, so a CSS parser
   drops them and the declarations they guard vanish. Expanding here keeps the
   [&] nesting for cascade to flatten.

   Only the built-in [dark] is known without a declaration; other names need one
   in the entrypoint. *)

let builtins = [ ("dark", "@media (prefers-color-scheme: dark) { @slot; }") ]

let shorthand_variant prelude =
  let prelude = String.trim prelude in
  let len = String.length prelude in
  let whitespace = function
    | ' ' | '\t' | '\n' | '\r' | '\012' -> true
    | _ -> false
  in
  let rec name_end i =
    if i >= len || prelude.[i] = '(' || whitespace prelude.[i] then i
    else name_end (i + 1)
  in
  let i = name_end 0 in
  let name = String.sub prelude 0 i in
  let selector = String.trim (String.sub prelude i (len - i)) in
  if
    name = ""
    || String.length selector < 2
    || selector.[0] <> '('
    || selector.[String.length selector - 1] <> ')'
  then None
  else
    let selector = String.sub selector 1 (String.length selector - 2) in
    Some (name, selector ^ " { @slot; }")

let take css =
  let defs = ref [] in
  let css =
    Text_pass.rewrite ~names:[ "@custom-variant" ] css (fun index _ i ->
        match Index.at_rule index ~name:"@custom-variant" i with
        | Some { prelude; block = { body; next }; _ } when prelude <> "" ->
            defs := (prelude, body) :: !defs;
            Some next
        | _ -> (
            match Index.at_statement index ~name:"@custom-variant" i with
            | Some { prelude; next } -> (
                match shorthand_variant prelude with
                | Some def ->
                    defs := def :: !defs;
                    Some next
                | None -> None)
            | None -> None))
  in
  (css, !defs)

let fill_slots template body =
  Text_pass.rewrite ~names:[ "@slot" ] template (fun index buf i ->
      match Index.at_statement index ~name:"@slot" i with
      | Some { next; _ } ->
          Buffer.add_string buf body;
          Some next
      | None -> None)

(* Replace [@variant NAME { decls }] with the variant's body, substituting the
   declarations at each [@slot]. Nested [@variant]s expand outermost-first, so
   the recursion re-runs over the result. *)
let rec expand_from ~depth defs css =
  if depth > 8 then css
  else
    let changed = ref false in
    let out =
      Text_pass.rewrite ~names:[ "@variant" ] css (fun index buf i ->
          match Index.at_rule index ~name:"@variant" i with
          | Some { prelude; block = { body; next }; _ }
            when List.mem_assoc prelude defs ->
              let template = List.assoc prelude defs in
              changed := true;
              Buffer.add_string buf (fill_slots template body);
              Some next
          | _ -> None)
    in
    if !changed then expand_from ~depth:(depth + 1) defs out else out

(* Split a class name on its variant separators. A [:] inside [[&>*]] or [(--x)]
   is part of the segment, not a separator. *)
let segments name =
  let len = String.length name in
  let rec seg_end i depth =
    if i >= len then i
    else
      match name.[i] with
      | '[' | '(' -> seg_end (i + 1) (depth + 1)
      | ']' | ')' -> seg_end (i + 1) (depth - 1)
      | ':' when depth = 0 -> i
      | _ -> seg_end (i + 1) depth
  in
  let rec go i acc =
    let stop = seg_end i 0 in
    let seg = String.sub name i (stop - i) in
    if stop >= len then List.rev (seg :: acc) else go (stop + 1) (seg :: acc)
  in
  go 0 []

(* Separate the variants a project declared with [@custom-variant] from the rest
   of the class. They cannot go through [Tw.of_string], which only knows the
   built-in variants, so they are re-emitted as [@variant] blocks for [expand]
   to expand. A declared variant is picked out wherever it sits in the chain —
   [lg:dark:flex] as much as [dark:lg:flex] — and the built-in prefixes stay
   attached to the utility, which keeps their media queries wrapped around the
   declared variant's selector. *)
let split_declared defs name =
  match segments name with
  | [] | [ _ ] -> ([], name)
  | segs ->
      let bare = List.nth segs (List.length segs - 1) in
      let prefix = List.filteri (fun i _ -> i < List.length segs - 1) segs in
      let declared, builtin =
        List.partition (fun s -> List.mem_assoc s defs) prefix
      in
      (declared, String.concat ":" (builtin @ [ bare ]))

let expand defs css = expand_from ~depth:0 defs css
