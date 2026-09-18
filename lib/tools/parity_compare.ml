let diff ?mode expected actual =
  Cascade_diff.Css_compare.diff ?mode expected actual

let uncovered ~html classes =
  let on_page = Hashtbl.create 256 in
  List.iter
    (fun candidate -> Hashtbl.replace on_page candidate ())
    (Source_scan.candidates html);
  List.filter (fun cls -> not (Hashtbl.mem on_page cls)) classes

let browser ~html ~classes ~tailwind ~tw =
  match uncovered ~html classes with
  | _ :: _ as missing ->
      Error
        (String.concat ""
           [
             "no element of the document carries ";
             String.concat ", " missing;
             ", so the browser would compare nothing for it";
           ])
  | [] -> Browser_compare.run ~html [ ("Tailwind", tailwind); ("tw", tw) ]
