module Css = Cascade.Css

(* Whether [table] already holds [stmt], recording it when it does not.
   [Css.hash_statement] buckets a statement and [Css.equal_statement] settles
   the bucket, so identity is decided on the statement rather than on the CSS
   text it renders to. *)
let seen table stmt =
  let bucket = Css.hash_statement stmt in
  if List.exists (Css.equal_statement stmt) (Hashtbl.find_all table bucket) then
    true
  else begin
    Hashtbl.add table bucket stmt;
    false
  end

let dedup stmts =
  let table = Hashtbl.create 8 in
  List.filter (fun stmt -> not (seen table stmt)) stmts

(* A utility's selector names a class, the theme block's [:root, :host] does
   not. Asked of the selector itself rather than of its text, where a '.' also
   comes from an attribute value or a decimal inside a pseudo argument. *)
let rec merge_same_selector = function
  | a :: b :: rest -> (
      match (Css.as_rule a, Css.as_rule b) with
      | Some (sa, da, []), Some (sb, db, []) when Css.Selector.equal sa sb ->
          merge_same_selector (Css.rule ~selector:sa (da @ db) :: rest)
      | _ -> a :: merge_same_selector (b :: rest))
  | stmts -> stmts
