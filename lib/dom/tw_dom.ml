(* Client-side CSS injection.

   Maintains a <style> element in <head> containing CSS for all utilities
   registered via [use]. Each utility's class name is tracked to avoid
   re-injection. The <style> content is rebuilt when new utilities appear. *)

open Brr

let registered : (string, unit) Hashtbl.t = Hashtbl.create 128
let all_styles : Tw.t list ref = ref []
let style_el : El.t option ref = ref None
let include_base : bool ref = ref true

let ensure_style_el () =
  match !style_el with
  | Some el -> el
  | None ->
      let head = Document.head G.document in
      let el = El.v El.Name.style [] in
      El.set_at (Jstr.v "data-tw") (Some (Jstr.v "runtime")) el;
      El.append_children head [ el ];
      style_el := Some el;
      el

let set_text_content el s = El.set_children el [ El.txt (Jstr.v s) ]

let rebuild_css () =
  let el = ensure_style_el () in
  let styles = List.rev !all_styles in
  let css = Tw.to_css ~base:!include_base ~optimize:true styles in
  let css_str = Css.to_string ~minify:true css in
  set_text_content el css_str

let init ?(base = true) () =
  include_base := base;
  ignore (ensure_style_el ())

let use styles =
  let new_found = ref false in
  List.iter
    (fun s ->
      let cls = Tw.pp s in
      if not (Hashtbl.mem registered cls) then (
        Hashtbl.add registered cls ();
        all_styles := s :: !all_styles;
        new_found := true))
    styles;
  if !new_found then rebuild_css ();
  Tw.to_classes styles

let use_str s =
  let styles = Tw.str s in
  ignore (use styles);
  s

let css () =
  let styles = List.rev !all_styles in
  let css = Tw.to_css ~base:!include_base ~optimize:true styles in
  Css.to_string ~minify:true css
