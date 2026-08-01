type t = { base : bool; forms : bool option; layers : bool }

let default = { base = true; forms = None; layers = true }

let v ?(base = default.base) ?forms ?(layers = default.layers) () =
  { base; forms; layers }

let base t = t.base
let forms t = t.forms
let layers t = t.layers

let to_string t =
  Pp.str
    [
      "{base=";
      Pp.bool t.base;
      "; forms=";
      (match t.forms with None -> "auto" | Some b -> Pp.bool b);
      "; layers=";
      Pp.bool t.layers;
      "}";
    ]
