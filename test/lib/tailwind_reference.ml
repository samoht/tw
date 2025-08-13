(** Module to get reference CSS from actual Tailwind CLI *)

let create_temp_html classes =
  let html = Printf.sprintf {|<div class="%s">Test</div>|} classes in
  let tmp = Filename.temp_file "tw_test_" ".html" in
  let oc = open_out tmp in
  output_string oc html;
  close_out oc;
  tmp

let get_tailwind_css classes =
  let html_file = create_temp_html classes in
  let cmd = Printf.sprintf "tailwindcss --content '%s' 2>/dev/null" html_file in
  let ic = Unix.open_process_in cmd in
  let css = ref "" in
  (try
     while true do
       css := !css ^ input_line ic ^ "\n"
     done
   with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Sys.remove html_file;
  !css

let extract_property css selector property_name =
  (* Find the selector in the CSS *)
  let selector_pattern =
    Re.Perl.compile_pat
      (Printf.sprintf "%s\\s*\\{([^}]+)\\}" (Re.Perl.quote selector))
  in
  match Re.exec_opt selector_pattern css with
  | Some m -> (
      let block = Re.Group.get m 1 in
      (* Extract the property value *)
      let prop_pattern =
        Re.Perl.compile_pat (Printf.sprintf "%s:\\s*([^;]+)" property_name)
      in
      match Re.exec_opt prop_pattern block with
      | Some pm -> Some (String.trim (Re.Group.get pm 1))
      | None -> None)
  | None -> None

let get_property_value class_name property_name =
  let css = get_tailwind_css class_name in
  let selector =
    "."
    ^ Re.Perl.substitute_string (Re.Perl.compile_pat ":") ~by:"\\\\:" class_name
  in
  extract_property css selector property_name

(* Cache results to avoid calling tailwind CLI too many times *)
let cache = Hashtbl.create 100

let get_cached class_name property_name =
  let key = class_name ^ ":" ^ property_name in
  match Hashtbl.find_opt cache key with
  | Some v -> v
  | None ->
      let value = get_property_value class_name property_name in
      Hashtbl.add cache key value;
      value
