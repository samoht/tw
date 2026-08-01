type t = { base : bool; forms : bool option; layers : bool }

let default = { base = true; forms = None; layers = true }

let v ?(base = default.base) ?forms ?(layers = default.layers) () =
  { base; forms; layers }
