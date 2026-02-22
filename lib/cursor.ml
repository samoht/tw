(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

module Handler = struct
  open Style
  open Css

  type t =
    | Cursor_alias
    | Cursor_all_scroll
    | Cursor_auto
    | Cursor_cell
    | Cursor_col_resize
    | Cursor_context_menu
    | Cursor_copy
    | Cursor_crosshair
    | Cursor_default
    | Cursor_e_resize
    | Cursor_ew_resize
    | Cursor_grab
    | Cursor_grabbing
    | Cursor_help
    | Cursor_move
    | Cursor_n_resize
    | Cursor_ne_resize
    | Cursor_nesw_resize
    | Cursor_no_drop
    | Cursor_none
    | Cursor_not_allowed
    | Cursor_ns_resize
    | Cursor_nw_resize
    | Cursor_nwse_resize
    | Cursor_pointer
    | Cursor_progress
    | Cursor_row_resize
    | Cursor_s_resize
    | Cursor_se_resize
    | Cursor_sw_resize
    | Cursor_text
    | Cursor_vertical_text
    | Cursor_w_resize
    | Cursor_wait
    | Cursor_zoom_in
    | Cursor_zoom_out
    | Cursor_bracket_var of string
    | Cursor_theme of string

  type Utility.base += Self of t

  let name = "cursor"
  let priority = 11

  (* Single source of truth: (handler, class_suffix, css_value) *)
  (* Alphabetically ordered - suborder derived from position *)
  let cursor_data =
    [
      (Cursor_alias, "alias", Alias);
      (Cursor_all_scroll, "all-scroll", All_scroll);
      (Cursor_auto, "auto", Auto);
      (Cursor_cell, "cell", Cell);
      (Cursor_col_resize, "col-resize", Col_resize);
      (Cursor_context_menu, "context-menu", Context_menu);
      (Cursor_copy, "copy", Copy);
      (Cursor_crosshair, "crosshair", Crosshair);
      (Cursor_default, "default", Default);
      (Cursor_e_resize, "e-resize", E_resize);
      (Cursor_ew_resize, "ew-resize", Ew_resize);
      (Cursor_grab, "grab", Grab);
      (Cursor_grabbing, "grabbing", Grabbing);
      (Cursor_help, "help", Help);
      (Cursor_move, "move", Move);
      (Cursor_n_resize, "n-resize", N_resize);
      (Cursor_ne_resize, "ne-resize", Ne_resize);
      (Cursor_nesw_resize, "nesw-resize", Nesw_resize);
      (Cursor_no_drop, "no-drop", No_drop);
      (Cursor_none, "none", None);
      (Cursor_not_allowed, "not-allowed", Not_allowed);
      (Cursor_ns_resize, "ns-resize", Ns_resize);
      (Cursor_nw_resize, "nw-resize", Nw_resize);
      (Cursor_nwse_resize, "nwse-resize", Nwse_resize);
      (Cursor_pointer, "pointer", Pointer);
      (Cursor_progress, "progress", Progress);
      (Cursor_row_resize, "row-resize", Row_resize);
      (Cursor_s_resize, "s-resize", S_resize);
      (Cursor_se_resize, "se-resize", Se_resize);
      (Cursor_sw_resize, "sw-resize", Sw_resize);
      (Cursor_text, "text", Text);
      (Cursor_vertical_text, "vertical-text", Vertical_text);
      (Cursor_w_resize, "w-resize", W_resize);
      (Cursor_wait, "wait", Wait);
      (Cursor_zoom_in, "zoom-in", Zoom_in);
      (Cursor_zoom_out, "zoom-out", Zoom_out);
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (t, suffix, _) -> (t, "cursor-" ^ suffix)) cursor_data

  let to_style_map =
    List.map (fun (t, _, css_val) -> (t, style [ cursor css_val ])) cursor_data

  let suborder_map =
    List.mapi (fun i (t, _, _) -> (t, (i + 1) * 10)) cursor_data

  let of_class_map =
    List.map (fun (t, suffix, _) -> ("cursor-" ^ suffix, t)) cursor_data

  (* Handler functions derived from maps *)
  let to_class = function
    | Cursor_bracket_var s -> "cursor-[" ^ s ^ "]"
    | Cursor_theme name -> "cursor-" ^ name
    | t -> List.assoc t to_class_map

  let to_style = function
    | Cursor_bracket_var s ->
        let inner = Parse.extract_var_name s in
        let ref : Css.cursor Css.var = Css.var_ref inner in
        style [ cursor (Var ref) ]
    | Cursor_theme name -> (
        let var_name = "cursor-" ^ name in
        let ref : Css.cursor Css.var =
          Var.theme_ref var_name
            ~default:(Auto : Css.cursor)
            ~default_css:"auto"
        in
        match Var.get_theme_value var_name with
        | Some value ->
            let theme_decl =
              Css.custom_declaration ~layer:"theme" ("--" ^ var_name) String
                value
            in
            style [ theme_decl; cursor (Var ref) ]
        | None -> style [ cursor (Var ref) ])
    | t -> List.assoc t to_style_map

  (* Sorted suffixes from cursor_data for computing theme suborder *)
  let sorted_suffixes =
    List.mapi (fun i (_, suffix, _) -> (suffix, (i + 1) * 10)) cursor_data

  let theme_suborder name =
    (* Find alphabetical position among known cursors *)
    let rec find = function
      | [] -> 999
      | (suffix, order) :: _ when String.compare name suffix < 0 -> order - 5
      | _ :: rest -> find rest
    in
    find sorted_suffixes

  let suborder = function
    | Cursor_bracket_var _ -> -1
    | Cursor_theme name -> theme_suborder name
    | t -> List.assoc t suborder_map

  let of_class cls =
    let parts = Parse.split_class cls in
    match parts with
    | [ "cursor"; value ] when Parse.is_bracket_var value ->
        Ok (Cursor_bracket_var (Parse.bracket_inner value))
    | [ "cursor"; name ] when not (List.mem_assoc cls of_class_map) ->
        if Parse.is_valid_theme_name name then Ok (Cursor_theme name)
        else Error (`Msg "Not a cursor utility")
    | _ -> (
        match List.assoc_opt cls of_class_map with
        | Some t -> Ok t
        | None -> Error (`Msg "Not a cursor utility"))
end

open Handler

(** Register the cursor utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let cursor_alias = utility Cursor_alias
let cursor_all_scroll = utility Cursor_all_scroll
let cursor_auto = utility Cursor_auto
let cursor_cell = utility Cursor_cell
let cursor_col_resize = utility Cursor_col_resize
let cursor_context_menu = utility Cursor_context_menu
let cursor_copy = utility Cursor_copy
let cursor_crosshair = utility Cursor_crosshair
let cursor_default = utility Cursor_default
let cursor_e_resize = utility Cursor_e_resize
let cursor_ew_resize = utility Cursor_ew_resize
let cursor_grab = utility Cursor_grab
let cursor_grabbing = utility Cursor_grabbing
let cursor_help = utility Cursor_help
let cursor_move = utility Cursor_move
let cursor_n_resize = utility Cursor_n_resize
let cursor_ne_resize = utility Cursor_ne_resize
let cursor_nesw_resize = utility Cursor_nesw_resize
let cursor_no_drop = utility Cursor_no_drop
let cursor_none = utility Cursor_none
let cursor_not_allowed = utility Cursor_not_allowed
let cursor_ns_resize = utility Cursor_ns_resize
let cursor_nw_resize = utility Cursor_nw_resize
let cursor_nwse_resize = utility Cursor_nwse_resize
let cursor_pointer = utility Cursor_pointer
let cursor_progress = utility Cursor_progress
let cursor_row_resize = utility Cursor_row_resize
let cursor_s_resize = utility Cursor_s_resize
let cursor_se_resize = utility Cursor_se_resize
let cursor_sw_resize = utility Cursor_sw_resize
let cursor_text = utility Cursor_text
let cursor_vertical_text = utility Cursor_vertical_text
let cursor_w_resize = utility Cursor_w_resize
let cursor_wait = utility Cursor_wait
let cursor_zoom_in = utility Cursor_zoom_in
let cursor_zoom_out = utility Cursor_zoom_out
