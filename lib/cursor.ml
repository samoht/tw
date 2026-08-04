(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

module Css = Cascade.Css

module Handler = struct
  open Style

  type t = Keyword of Css.cursor | Bracket_var of string | Theme of string
  type Utility.base += Self of t

  let name = "cursor"
  let priority _ = 11

  (* Single source of truth: (css value, class_suffix) *)
  (* Alphabetically ordered - suborder derived from position *)
  let cursor_data : (Css.cursor * string) list =
    [
      (Css.Alias, "alias");
      (Css.All_scroll, "all-scroll");
      (Css.Auto, "auto");
      (Css.Cell, "cell");
      (Css.Col_resize, "col-resize");
      (Css.Context_menu, "context-menu");
      (Css.Copy, "copy");
      (Css.Crosshair, "crosshair");
      (Css.Default, "default");
      (Css.E_resize, "e-resize");
      (Css.Ew_resize, "ew-resize");
      (Css.Grab, "grab");
      (Css.Grabbing, "grabbing");
      (Css.Help, "help");
      (Css.Move, "move");
      (Css.N_resize, "n-resize");
      (Css.Ne_resize, "ne-resize");
      (Css.Nesw_resize, "nesw-resize");
      (Css.No_drop, "no-drop");
      (Css.None, "none");
      (Css.Not_allowed, "not-allowed");
      (Css.Ns_resize, "ns-resize");
      (Css.Nw_resize, "nw-resize");
      (Css.Nwse_resize, "nwse-resize");
      (Css.Pointer, "pointer");
      (Css.Progress, "progress");
      (Css.Row_resize, "row-resize");
      (Css.S_resize, "s-resize");
      (Css.Se_resize, "se-resize");
      (Css.Sw_resize, "sw-resize");
      (Css.Text, "text");
      (Css.Vertical_text, "vertical-text");
      (Css.W_resize, "w-resize");
      (Css.Wait, "wait");
      (Css.Zoom_in, "zoom-in");
      (Css.Zoom_out, "zoom-out");
    ]

  (* Derived lookup tables *)
  let to_class_map =
    List.map (fun (v, suffix) -> (v, "cursor-" ^ suffix)) cursor_data

  let suborder_map = List.mapi (fun i (v, _) -> (v, (i + 1) * 10)) cursor_data

  let of_class_map =
    List.map (fun (v, suffix) -> ("cursor-" ^ suffix, Keyword v)) cursor_data

  (* Handler functions derived from maps *)
  let to_class = function
    | Bracket_var s -> "cursor-[" ^ s ^ "]"
    | Theme name -> "cursor-" ^ name
    | Keyword v -> List.assoc v to_class_map

  let to_style theme = function
    | Bracket_var s ->
        let inner = Parse.extract_var_name s in
        let ref : Css.cursor Css.var = Var.bracket inner in
        style [ Css.cursor (Css.Var ref) ]
    | Theme name -> (
        let var_name = "cursor-" ^ name in
        let ref : Css.cursor Css.var =
          Var.theme_ref var_name
            ~default:(Css.Auto : Css.cursor)
            ~default_css:"auto"
        in
        match Scheme.theme_value (Some theme) var_name with
        | Some value ->
            let theme_decl =
              Css.custom_property ~layer:"theme" ("--" ^ var_name) value
            in
            style [ theme_decl; Css.cursor (Css.Var ref) ]
        | None -> style [ Css.cursor (Css.Var ref) ])
    | Keyword v -> style [ Css.cursor v ]

  (* Sorted suffixes from cursor_data for computing theme suborder *)
  let sorted_suffixes =
    List.mapi (fun i (_, suffix) -> (suffix, (i + 1) * 10)) cursor_data

  let theme_suborder name =
    (* Find alphabetical position among known cursors *)
    let rec find = function
      | [] -> 999
      | (suffix, order) :: _ when String.compare name suffix < 0 -> order - 5
      | _ :: rest -> find rest
    in
    find sorted_suffixes

  let suborder = function
    | Bracket_var _ -> -1
    | Theme name -> theme_suborder name
    | Keyword v -> List.assoc v suborder_map

  let of_class _theme cls =
    let parts = Parse.split_class cls in
    match parts with
    | [ "cursor"; value ] when Parse.is_bracket_var value ->
        Ok (Bracket_var (Parse.bracket_inner value))
    | [ "cursor"; name ] when not (List.mem_assoc cls of_class_map) ->
        (* A theme token name is an identifier: [cursor-[<value>]] is not
           one. *)
        if Parse.is_valid_theme_name name && Parse.is_ident name then
          Ok (Theme name)
        else Error (`Msg "Not a cursor utility")
    | _ -> (
        match List.assoc_opt cls of_class_map with
        | Some t -> Ok t
        | None -> Error (`Msg "Not a cursor utility"))

  let examples = [ Keyword Css.Auto ]
end

open Handler

(** Register the cursor utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let cursor_alias = utility (Keyword Css.Alias)
let cursor_all_scroll = utility (Keyword Css.All_scroll)
let cursor_auto = utility (Keyword Css.Auto)
let cursor_cell = utility (Keyword Css.Cell)
let cursor_col_resize = utility (Keyword Css.Col_resize)
let cursor_context_menu = utility (Keyword Css.Context_menu)
let cursor_copy = utility (Keyword Css.Copy)
let cursor_crosshair = utility (Keyword Css.Crosshair)
let cursor_default = utility (Keyword Css.Default)
let cursor_e_resize = utility (Keyword Css.E_resize)
let cursor_ew_resize = utility (Keyword Css.Ew_resize)
let cursor_grab = utility (Keyword Css.Grab)
let cursor_grabbing = utility (Keyword Css.Grabbing)
let cursor_help = utility (Keyword Css.Help)
let cursor_move = utility (Keyword Css.Move)
let cursor_n_resize = utility (Keyword Css.N_resize)
let cursor_ne_resize = utility (Keyword Css.Ne_resize)
let cursor_nesw_resize = utility (Keyword Css.Nesw_resize)
let cursor_no_drop = utility (Keyword Css.No_drop)
let cursor_none = utility (Keyword Css.None)
let cursor_not_allowed = utility (Keyword Css.Not_allowed)
let cursor_ns_resize = utility (Keyword Css.Ns_resize)
let cursor_nw_resize = utility (Keyword Css.Nw_resize)
let cursor_nwse_resize = utility (Keyword Css.Nwse_resize)
let cursor_pointer = utility (Keyword Css.Pointer)
let cursor_progress = utility (Keyword Css.Progress)
let cursor_row_resize = utility (Keyword Css.Row_resize)
let cursor_s_resize = utility (Keyword Css.S_resize)
let cursor_se_resize = utility (Keyword Css.Se_resize)
let cursor_sw_resize = utility (Keyword Css.Sw_resize)
let cursor_text = utility (Keyword Css.Text)
let cursor_vertical_text = utility (Keyword Css.Vertical_text)
let cursor_w_resize = utility (Keyword Css.W_resize)
let cursor_wait = utility (Keyword Css.Wait)
let cursor_zoom_in = utility (Keyword Css.Zoom_in)
let cursor_zoom_out = utility (Keyword Css.Zoom_out)
