let bound_re = Re.seq [ Re.opt (Re.char '-'); Re.rep1 Re.digit ]

let range_re =
  Re.compile
    (Re.whole_string
       (Re.seq
          [
            Re.group bound_re;
            Re.str "..";
            Re.group bound_re;
            Re.opt (Re.seq [ Re.str ".."; Re.group bound_re ]);
          ]))

(* [{from..to..step}] counts from [from] to [to] inclusive. The step defaults to
   one, and a step pointing away from [to] is turned round rather than never
   arriving, as the bundle does. A zero step is an error there, so the pattern
   names nothing here. A step too large for an int outruns the range, as the
   bundle's does, and a step that would carry past [max_int] or [min_int] ends
   the count rather than wrapping round. *)
let range inner =
  let bound g k = Option.bind (Re.Group.get_opt g k) int_of_string_opt in
  let magnitude = function
    | None -> 1
    | Some s -> (
        match int_of_string_opt s with
        | Some n when n <> min_int -> abs n
        | Some _ | None -> max_int)
  in
  match Re.exec_opt range_re inner with
  | None -> None
  | Some g -> (
      match (bound g 1, bound g 2) with
      | Some first, Some last ->
          let step = magnitude (Re.Group.get_opt g 3) in
          let step = if first <= last then step else -step in
          let rec count n acc =
            let acc = string_of_int n :: acc in
            let next = n + step in
            let ended =
              if step > 0 then next < n || next > last
              else next > n || next < last
            in
            if ended then List.rev acc else count next acc
          in
          Some (if step = 0 then [] else count first [])
      | _ -> None)

(* Tailwind's brace expansion: the first [{...}] is a comma list or a range,
   each alternative is expanded in turn, and so is what follows the brace. An
   unbalanced brace is refused by the bundle, so it names nothing. *)
let rec expand pattern =
  match String.index_opt pattern '{' with
  | None -> [ pattern ]
  | Some opening -> (
      let len = String.length pattern in
      let rec closing depth i =
        if i >= len then None
        else
          match pattern.[i] with
          | '{' -> closing (depth + 1) (i + 1)
          | '}' when depth = 1 -> Some i
          | '}' -> closing (depth - 1) (i + 1)
          | _ -> closing depth (i + 1)
      in
      match closing 0 opening with
      | None -> []
      | Some close ->
          let prefix = String.sub pattern 0 opening in
          let inner = String.sub pattern (opening + 1) (close - opening - 1) in
          let suffix = String.sub pattern (close + 1) (len - close - 1) in
          let alternatives =
            match range inner with
            | Some values -> values
            | None ->
                List.concat_map expand (Tw.Parse.split_top_level ',' inner)
          in
          List.concat_map
            (fun rest ->
              List.map
                (fun alt -> String.concat "" [ prefix; alt; rest ])
                alternatives)
            (expand suffix))
