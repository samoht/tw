#use "topfind"

#require "tw"

open Tw.Css

let test_simple = "font-family:var(--font,Arial,sans-serif)";;

match Declaration.of_string test_simple with
| Ok _ -> print_endline "Simple test: OK"
| Error e -> Printf.printf "Simple test: ERROR - %s\n" (pp_parse_error e)

let test_complex =
  "font-family:var(--default-font-family,ui-sans-serif,system-ui,sans-serif,\"Apple \
   Color Emoji\")"
;;

match Declaration.of_string test_complex with
| Ok _ -> print_endline "Complex test: OK"
| Error e -> Printf.printf "Complex test: ERROR - %s\n" (pp_parse_error e)
