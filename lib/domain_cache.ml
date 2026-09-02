type ('key, 'value) t = ('key, 'value) Hashtbl.t Domain.DLS.key

let v size = Domain.DLS.new_key (fun () -> Hashtbl.create size)

let or_add cache key make =
  let table = Domain.DLS.get cache in
  match Hashtbl.find_opt table key with
  | Some value -> value
  | None ->
      let value = make () in
      Hashtbl.add table key value;
      value
