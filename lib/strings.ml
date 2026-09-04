let index ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go 0

let contains ~sub s = Option.is_some (index ~sub s)
let is_digit c = c >= '0' && c <= '9'
