let contains ~sub s =
  let n = String.length s and m = String.length sub in
  let rec loop i = i + m <= n && (String.sub s i m = sub || loop (i + 1)) in
  loop 0

let is_digit c = c >= '0' && c <= '9'
