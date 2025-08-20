(** CSS debugging utilities for testing *)

(** Write CSS to a temporary file for inspection *)
let write_temp_css prefix css =
  let file = Filename.temp_file prefix ".css" in
  let oc = open_out file in
  output_string oc css;
  close_out oc;
  file

(** Format minified CSS for readability *)
let format_css css =
  let buf = Buffer.create (String.length css * 2) in
  let len = String.length css in
  let rec loop i indent =
    if i >= len then ()
    else
      match css.[i] with
      | '{' ->
          Buffer.add_char buf '{';
          Buffer.add_char buf '\n';
          let new_indent = indent + 2 in
          Buffer.add_string buf (String.make new_indent ' ');
          loop (i + 1) new_indent
      | '}' ->
          let new_indent = max 0 (indent - 2) in
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make new_indent ' ');
          Buffer.add_char buf '}';
          if i + 1 < len && css.[i + 1] <> '}' then Buffer.add_char buf '\n';
          if i + 1 < len && css.[i + 1] <> '}' then
            Buffer.add_string buf (String.make new_indent ' ');
          loop (i + 1) new_indent
      | ';' ->
          Buffer.add_char buf ';';
          Buffer.add_char buf '\n';
          if i + 1 < len && css.[i + 1] <> '}' then
            Buffer.add_string buf (String.make indent ' ');
          loop (i + 1) indent
      | ',' when i + 1 < len && (css.[i + 1] = ':' || css.[i + 1] = '*') ->
          (* Selector separator *)
          Buffer.add_char buf ',';
          Buffer.add_char buf '\n';
          Buffer.add_string buf (String.make indent ' ');
          loop (i + 1) indent
      | c ->
          Buffer.add_char buf c;
          loop (i + 1) indent
  in
  loop 0 0;
  Buffer.contents buf

(** Extract a specific CSS rule by selector *)
let extract_rule css selector =
  (* Escape special regex characters in selector *)
  let escaped_selector = Re.str selector in
  let pattern =
    Re.seq
      [
        escaped_selector;
        Re.rep Re.space;
        Re.char '{';
        Re.group (Re.rep (Re.compl [ Re.char '}' ]));
        Re.char '}';
      ]
  in
  let regex = Re.compile pattern in
  match Re.exec_opt regex css with
  | Some groups -> (
      try Some (String.trim (Re.Group.get groups 1)) with Not_found -> None)
  | None -> None

(** Find the first difference between two CSS strings *)
let find_first_diff css1 css2 =
  let len1 = String.length css1 in
  let len2 = String.length css2 in
  let min_len = min len1 len2 in

  (* Extract version info from headers *)
  let get_version css =
    if String.starts_with ~prefix:"/* tw v" css then
      let end_idx = try String.index_from css 7 ' ' with Not_found -> 20 in
      "tw v" ^ String.sub css 7 (min (end_idx - 7) 20)
    else if String.starts_with ~prefix:"/* ≈ tailwindcss" css then "tailwind"
    else "unknown"
  in

  let tw_label =
    let v = get_version css1 in
    if v = "unknown" then "tw" else v
  in
  let tailwind_label =
    let v = get_version css2 in
    if v = "unknown" then "tailwind" else v
  in

  let get_context css len pos =
    let context_size = 50 in
    let start = max 0 (pos - context_size) in
    let end_pos = min len (pos + context_size) in
    String.sub css start (end_pos - start)
  in

  (* Try to find which layer/selector and property we're in at position pos *)
  let get_location css pos =
    (* Find all @layer declarations before this position *)
    let rec find_layers acc i =
      if i >= pos then acc
      else if i + 6 < String.length css && String.sub css i 6 = "@layer" then
        let space_idx =
          try String.index_from css (i + 7) ' ' with Not_found -> i + 20
        in
        let brace_idx =
          try String.index_from css (i + 7) '{' with Not_found -> i + 20
        in
        let end_idx = min space_idx brace_idx in
        let layer_name = String.sub css (i + 7) (end_idx - i - 7) in
        find_layers ((i, "@layer " ^ layer_name) :: acc) (i + 1)
      else find_layers acc (i + 1)
    in
    let layers = List.rev (find_layers [] 0) in
    (* Find the most recent layer *)
    let layer =
      match
        List.find_opt (fun (layer_pos, _) -> layer_pos < pos) (List.rev layers)
      with
      | Some (_, layer) -> layer
      | None -> ""
    in

    (* Find the current CSS selector (rule) *)
    let rec find_selector i =
      if i <= 0 then ""
      else
        match css.[i] with
        | '{' ->
            (* Found opening brace, look backwards for selector *)
            let rec find_sel_start j =
              if j <= 0 then 0
              else
                match css.[j] with
                | ('}' | ';' | '>') when j > 0 && css.[j - 1] <> '\\' -> j + 1
                | _ when j > 6 && String.sub css (j - 6) 7 = "@layer " -> j
                | _ -> find_sel_start (j - 1)
            in
            let sel_start = find_sel_start (i - 1) in
            let selector =
              String.sub css sel_start (i - sel_start) |> String.trim
            in
            if selector <> "" && selector.[0] <> '@' then selector else ""
        | _ -> find_selector (i - 1)
    in

    (* Find the current property or selector *)
    let rec find_property i =
      if i <= 0 then ""
      else
        match css.[i] with
        | ';' | '{' ->
            (* Found boundary, look for property name after it *)
            let rec find_prop_start j =
              if j >= pos then ""
              else if
                j + 1 < String.length css && css.[j] = '-' && css.[j + 1] = '-'
              then
                (* Found start of CSS variable *)
                let rec find_prop_end k =
                  if k >= String.length css then k
                  else
                    match css.[k] with
                    | ':' | ';' | '}' -> k
                    | _ -> find_prop_end (k + 1)
                in
                let prop_end = min (find_prop_end j) pos in
                String.sub css j (prop_end - j)
              else if css.[j] <> ' ' && css.[j] <> '\n' && css.[j] <> '\t' then
                (* Found start of regular property *)
                let rec find_prop_end k =
                  if k >= String.length css then k
                  else match css.[k] with ':' -> k | _ -> find_prop_end (k + 1)
                in
                let prop_end = min (find_prop_end j) pos in
                String.trim (String.sub css j (prop_end - j))
              else find_prop_start (j + 1)
            in
            find_prop_start (i + 1)
        | _ -> find_property (i - 1)
    in

    let selector = find_selector pos in
    let property = find_property pos in

    (* Build full context string *)
    let context_parts =
      [
        (if layer <> "" then Some layer else None);
        (if selector <> "" then Some selector else None);
        (if property <> "" then Some property else None);
      ]
      |> List.filter_map (fun x -> x)
    in
    String.concat " > " context_parts
  in

  let check_char_at i =
    if css1.[i] <> css2.[i] then
      let context1 = get_context css1 len1 i in
      let context2 = get_context css2 len2 i in
      let location = get_location css1 i in

      (* Highlight the difference in the context *)
      let highlight_diff ctx pos =
        let rel_pos = min pos 50 in
        (* Position within context *)
        if rel_pos >= 0 && rel_pos < String.length ctx then
          let before = String.sub ctx 0 rel_pos in
          let diff_char = String.make 1 ctx.[rel_pos] in
          let after =
            String.sub ctx (rel_pos + 1) (String.length ctx - rel_pos - 1)
          in
          Fmt.str "%s%a%s" before
            Fmt.(styled `Bold @@ styled `Red string)
            diff_char after
        else ctx
      in

      (* Format location as concise tree *)
      let format_location loc =
        if loc = "" then ""
        else
          let parts = String.split_on_char '>' loc |> List.map String.trim in
          match parts with
          | [] -> ""
          | parts ->
              "\n"
              ^ String.concat " > "
                  (List.map
                     (fun p -> Fmt.str "%a" Fmt.(styled `Cyan string) p)
                     parts)
      in

      Some
        ( i,
          format_location location,
          let tw_padding =
            String.make
              (max 0 (String.length tailwind_label - String.length tw_label))
              ' '
          in
          Fmt.str "%a:%s %s\n%a: %s"
            Fmt.(styled `Green string)
            tw_label tw_padding
            (highlight_diff context1 50)
            Fmt.(styled `Blue string)
            tailwind_label
            (highlight_diff context2 50) )
    else None
  in

  let rec loop i =
    if i >= min_len then
      if len1 = len2 then None
      else
        let _shorter, longer, _shorter_name, longer_name =
          if len1 < len2 then (css1, css2, tw_label, tailwind_label)
          else (css2, css1, tailwind_label, tw_label)
        in
        let extra =
          String.sub longer min_len (String.length longer - min_len)
        in
        let extra_preview =
          if String.length extra > 200 then String.sub extra 0 200 ^ "..."
          else extra
        in
        Some
          ( i,
            "Length mismatch",
            Fmt.str
              "CSS1 length: %d, CSS2 length: %d\n\n\
               %a has %d extra characters:\n\
               %a"
              len1 len2
              Fmt.(styled `Blue string)
              longer_name
              (String.length longer - min_len)
              Fmt.(styled `Yellow string)
              extra_preview )
    else
      match check_char_at i with
      | Some result -> Some result
      | None -> loop (i + 1)
  in
  loop 0
