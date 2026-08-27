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

  (* Class suffix and cascade suborder of one cursor keyword, alphabetical by
     suffix. Written as a match rather than a lookup table: a constructor added
     to [Css.cursor] without an entry here is a compile error, not a [Not_found]
     raised out of [to_class] partway through rendering a sheet. The [url()]
     form, the CSS-wide keywords and [var()] are spelled out for the same
     reason. *)
  let data : Css.cursor -> string * int = function
    | Css.Alias -> ("alias", 10)
    | Css.All_scroll -> ("all-scroll", 20)
    | Css.Auto -> ("auto", 30)
    | Css.Cell -> ("cell", 40)
    | Css.Col_resize -> ("col-resize", 50)
    | Css.Context_menu -> ("context-menu", 60)
    | Css.Copy -> ("copy", 70)
    | Css.Crosshair -> ("crosshair", 80)
    | Css.Default -> ("default", 90)
    | Css.E_resize -> ("e-resize", 100)
    | Css.Ew_resize -> ("ew-resize", 110)
    | Css.Grab -> ("grab", 120)
    | Css.Grabbing -> ("grabbing", 130)
    | Css.Help -> ("help", 140)
    | Css.Move -> ("move", 150)
    | Css.N_resize -> ("n-resize", 160)
    | Css.Ne_resize -> ("ne-resize", 170)
    | Css.Nesw_resize -> ("nesw-resize", 180)
    | Css.No_drop -> ("no-drop", 190)
    | Css.None -> ("none", 200)
    | Css.Not_allowed -> ("not-allowed", 210)
    | Css.Ns_resize -> ("ns-resize", 220)
    | Css.Nw_resize -> ("nw-resize", 230)
    | Css.Nwse_resize -> ("nwse-resize", 240)
    | Css.Pointer -> ("pointer", 250)
    | Css.Progress -> ("progress", 260)
    | Css.Row_resize -> ("row-resize", 270)
    | Css.S_resize -> ("s-resize", 280)
    | Css.Se_resize -> ("se-resize", 290)
    | Css.Sw_resize -> ("sw-resize", 300)
    | Css.Text -> ("text", 310)
    | Css.Vertical_text -> ("vertical-text", 320)
    | Css.W_resize -> ("w-resize", 330)
    | Css.Wait -> ("wait", 340)
    | Css.Zoom_in -> ("zoom-in", 350)
    | Css.Zoom_out -> ("zoom-out", 360)
    | Css.Url _ | Css.Inherit | Css.Initial | Css.Unset | Css.Revert
    | Css.Revert_layer | Css.Var _ ->
        (* No cursor class names a url(), a CSS-wide keyword or a var(), so
           [of_class] never builds one: the bracket and theme forms have their
           own constructors. *)
        invalid_arg "cursor: value has no class name"

  (* Every keyword a class names, in suffix order, for the class-name lookup and
     for placing a theme cursor among them. *)
  let all : Css.cursor list =
    [
      Css.Alias;
      Css.All_scroll;
      Css.Auto;
      Css.Cell;
      Css.Col_resize;
      Css.Context_menu;
      Css.Copy;
      Css.Crosshair;
      Css.Default;
      Css.E_resize;
      Css.Ew_resize;
      Css.Grab;
      Css.Grabbing;
      Css.Help;
      Css.Move;
      Css.N_resize;
      Css.Ne_resize;
      Css.Nesw_resize;
      Css.No_drop;
      Css.None;
      Css.Not_allowed;
      Css.Ns_resize;
      Css.Nw_resize;
      Css.Nwse_resize;
      Css.Pointer;
      Css.Progress;
      Css.Row_resize;
      Css.S_resize;
      Css.Se_resize;
      Css.Sw_resize;
      Css.Text;
      Css.Vertical_text;
      Css.W_resize;
      Css.Wait;
      Css.Zoom_in;
      Css.Zoom_out;
    ]

  let of_class_map =
    List.map (fun v -> ("cursor-" ^ fst (data v), Keyword v)) all

  let to_class = function
    | Bracket_var s -> "cursor-[" ^ s ^ "]"
    | Theme name -> "cursor-" ^ name
    | Keyword v -> "cursor-" ^ fst (data v)

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

  (* The keyword suffixes with their suborders, for placing a theme cursor among
     them. *)
  let sorted_suffixes = List.map data all

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
    | Keyword v -> snd (data v)

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
