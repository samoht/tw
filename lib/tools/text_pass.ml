(* Where the entrypoint's blocks, at-rule headers and function calls start and
   end, keyed by the offset each one starts at.

   The Tailwind at-rules are not CSS, so a stylesheet parser drops them and they
   have to be located in the text first -- but counting braces over the raw
   bytes counts the ones inside a string, a comment or an escape too. The block
   then ends in the wrong place and everything after it is silently dropped.
   cascade's component values come out of CSS Syntax 3 sec. 5.4 already matched,
   and each carries the range of source it was parsed from, so the offsets are
   read off those. cascade owns the index; the passes over it are the
   Tailwind-specific part. *)
module Index = Cascade.Source_index

module Components = struct
  open Cascade

  let array css =
    Parser.list_of_component_values (Reader.of_string css)
    |> (fun out -> out.Parser.value)
    |> Array.of_list

  let start component = (Component.source_loc component).Loc.start_pos
  let stop component = (Component.source_loc component).Loc.end_pos

  let rec after_whitespace components i =
    if i >= Array.length components then i
    else
      match components.(i) with
      | Component.Preserved { kind = Token.Whitespace _; _ } ->
          after_whitespace components (i + 1)
      | _ -> i

  (* Component values already group every function and bracket block, so only a
     top-level semicolon can close this declaration. Mismatched closers stay
     inside the group whose matching closer is still outstanding. *)
  let rec semicolon_from components i =
    if i >= Array.length components then i
    else
      match components.(i) with
      | Component.Preserved { kind = Token.Semicolon; _ } -> i
      | _ -> semicolon_from components (i + 1)

  (* Source ranges of punctuation that separates declarations. Curly blocks are
     walked because a nested rule may itself contain functional utility
     declarations; parentheses, square blocks and functions remain atomic. *)
  let punctuation components =
    let rec add acc = function
      | [] -> acc
      | Component.Preserved
          { kind = Token.Semicolon | Token.Close Token.Curly; loc; _ }
        :: rest ->
          add ((loc.Loc.start_pos, loc.Loc.end_pos) :: acc) rest
      | Component.Block { node = { opening = Token.Curly; value; closed }; loc }
        :: rest ->
          let acc = (loc.Loc.start_pos, loc.Loc.start_pos + 1) :: acc in
          let acc = add acc value in
          let acc =
            if closed then (loc.Loc.end_pos - 1, loc.Loc.end_pos) :: acc
            else acc
          in
          add acc rest
      | _ :: rest -> add acc rest
    in
    List.rev (add [] components)
end

(* Whether [css] spells [name] anywhere. *)
let mentions name css = Re.execp (Re.compile (Re.str name)) css

(* [css] copied with [step] rewriting it. [step index buf i] runs at each offset
   nothing has been copied from yet: it writes to [buf] what stands for the
   source at [i] and answers where to resume, or [None] to have the byte at [i]
   copied. [names] is what the pass matches; the index finds a name only as the
   source spells it, so a source spelling none of them is handed back before the
   parse the index costs. *)
let rewrite ~names css step =
  if not (List.exists (fun name -> mentions name css) names) then css
  else
    let index = Index.v css in
    let len = String.length css in
    let buf = Buffer.create len in
    let step = step index buf in
    let rec go i =
      if i >= len then ()
      else
        match step i with
        | Some next -> go next
        | None ->
            Buffer.add_char buf css.[i];
            go (i + 1)
    in
    go 0;
    Buffer.contents buf

(* [s] without the matching quotes around it, when it has them. *)
let strip_quotes s =
  let n = String.length s in
  if n >= 2 && (s.[0] = '"' || s.[0] = '\'') && s.[n - 1] = s.[0] then
    Some (String.sub s 1 (n - 2))
  else None

(* [s] unquoted when it is quoted, else as it is. *)
let unquote s = Option.value ~default:s (strip_quotes s)

(* The text of a quoted argument. The bundle refuses an unquoted one. *)
let quoted_contents body = strip_quotes (String.trim body)
