open Tw

let () =
  let classes = [ bg slate 500; bg amber 500; bg rose 500 ] in

  let css_stylesheet = to_css classes in
  let css_output = Css.to_string ~minify:false css_stylesheet in

  (* Find utility class lines *)
  let lines = String.split_on_char '\n' css_output in
  List.iter
    (fun line ->
      if String.contains line ".bg-" && String.contains line "500{" then
        let start = String.index line '.' in
        let end_ = String.index line '{' in
        let class_name = String.sub line start (end_ - start) in
        Printf.printf "Found: %s\n" class_name)
    lines
