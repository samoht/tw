let int_any s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None -> Error (`Msg ("Invalid number: " ^ s))

let int_pos ~name s =
  match int_of_string_opt s with
  | Some n when n >= 0 -> Ok n
  | Some _ -> Error (`Msg (name ^ " must be non-negative: " ^ s))
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

let int_bounded ~name ~min ~max s =
  match int_of_string_opt s with
  | Some n when n >= min && n <= max -> Ok n
  | Some _ ->
      Error
        (`Msg
           ("" ^ name ^ " must be between " ^ string_of_int min ^ " and "
          ^ string_of_int max ^ ": " ^ s))
  | None -> Error (`Msg ("Invalid " ^ name ^ " value: " ^ s))

let ( >|= ) r f = Result.map f r
