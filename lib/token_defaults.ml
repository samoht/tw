let table : (string, string) Hashtbl.t = Hashtbl.create 64
let register name css = Hashtbl.replace table name css
let find name = Hashtbl.find_opt table name
let all () = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table []
