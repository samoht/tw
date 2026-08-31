(** Cursor utilities

    @see <https://tailwindcss.com/docs/cursor> Tailwind CSS Cursor documentation
*)

module Css = Cascade.Css

module Handler = struct
  open Style

  (* The keywords a [cursor-*] class names, spelled as this module's own type
     rather than as a [Css.cursor]. A [url()], a CSS-wide keyword and a [var()]
     are all cursor values and none of them is a utility, so carrying the CSS
     type here would leave [data] partial and [to_class] raising on a value it
     was handed. *)
  type keyword =
    | Alias
    | All_scroll
    | Auto
    | Cell
    | Col_resize
    | Context_menu
    | Copy
    | Crosshair
    | Default
    | E_resize
    | Ew_resize
    | Grab
    | Grabbing
    | Help
    | Move
    | N_resize
    | Ne_resize
    | Nesw_resize
    | No_drop
    | No_cursor
    | Not_allowed
    | Ns_resize
    | Nw_resize
    | Nwse_resize
    | Pointer
    | Progress
    | Row_resize
    | S_resize
    | Se_resize
    | Sw_resize
    | Text
    | Vertical_text
    | W_resize
    | Wait
    | Zoom_in
    | Zoom_out

  type t = Keyword of keyword | Bracket_var of string | Theme of string

  let name = "cursor"
  let priority _ = 11

  (* Class suffix, cursor value and cascade suborder of one keyword,
     alphabetical by suffix. One match covers all three, so a constructor added
     to [keyword] without an entry here is a compile error. *)
  let data : keyword -> string * Css.cursor * int = function
    | Alias -> ("alias", Css.Alias, 10)
    | All_scroll -> ("all-scroll", Css.All_scroll, 20)
    | Auto -> ("auto", Css.Auto, 30)
    | Cell -> ("cell", Css.Cell, 40)
    | Col_resize -> ("col-resize", Css.Col_resize, 50)
    | Context_menu -> ("context-menu", Css.Context_menu, 60)
    | Copy -> ("copy", Css.Copy, 70)
    | Crosshair -> ("crosshair", Css.Crosshair, 80)
    | Default -> ("default", Css.Default, 90)
    | E_resize -> ("e-resize", Css.E_resize, 100)
    | Ew_resize -> ("ew-resize", Css.Ew_resize, 110)
    | Grab -> ("grab", Css.Grab, 120)
    | Grabbing -> ("grabbing", Css.Grabbing, 130)
    | Help -> ("help", Css.Help, 140)
    | Move -> ("move", Css.Move, 150)
    | N_resize -> ("n-resize", Css.N_resize, 160)
    | Ne_resize -> ("ne-resize", Css.Ne_resize, 170)
    | Nesw_resize -> ("nesw-resize", Css.Nesw_resize, 180)
    | No_drop -> ("no-drop", Css.No_drop, 190)
    | No_cursor -> ("none", Css.None, 200)
    | Not_allowed -> ("not-allowed", Css.Not_allowed, 210)
    | Ns_resize -> ("ns-resize", Css.Ns_resize, 220)
    | Nw_resize -> ("nw-resize", Css.Nw_resize, 230)
    | Nwse_resize -> ("nwse-resize", Css.Nwse_resize, 240)
    | Pointer -> ("pointer", Css.Pointer, 250)
    | Progress -> ("progress", Css.Progress, 260)
    | Row_resize -> ("row-resize", Css.Row_resize, 270)
    | S_resize -> ("s-resize", Css.S_resize, 280)
    | Se_resize -> ("se-resize", Css.Se_resize, 290)
    | Sw_resize -> ("sw-resize", Css.Sw_resize, 300)
    | Text -> ("text", Css.Text, 310)
    | Vertical_text -> ("vertical-text", Css.Vertical_text, 320)
    | W_resize -> ("w-resize", Css.W_resize, 330)
    | Wait -> ("wait", Css.Wait, 340)
    | Zoom_in -> ("zoom-in", Css.Zoom_in, 350)
    | Zoom_out -> ("zoom-out", Css.Zoom_out, 360)

  (* Every keyword a class names, in suffix order, for the class-name lookup and
     for placing a theme cursor among them. *)
  let all =
    [
      Alias;
      All_scroll;
      Auto;
      Cell;
      Col_resize;
      Context_menu;
      Copy;
      Crosshair;
      Default;
      E_resize;
      Ew_resize;
      Grab;
      Grabbing;
      Help;
      Move;
      N_resize;
      Ne_resize;
      Nesw_resize;
      No_drop;
      No_cursor;
      Not_allowed;
      Ns_resize;
      Nw_resize;
      Nwse_resize;
      Pointer;
      Progress;
      Row_resize;
      S_resize;
      Se_resize;
      Sw_resize;
      Text;
      Vertical_text;
      W_resize;
      Wait;
      Zoom_in;
      Zoom_out;
    ]

  let keyword_suffix v =
    let suffix, _, _ = data v in
    suffix

  let of_class_map =
    List.map (fun v -> ("cursor-" ^ keyword_suffix v, Keyword v)) all

  let to_class = function
    | Bracket_var s -> "cursor-[" ^ s ^ "]"
    | Theme name -> "cursor-" ^ name
    | Keyword v -> "cursor-" ^ keyword_suffix v

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
    | Keyword v ->
        let _, cursor, _ = data v in
        style [ Css.cursor cursor ]

  (* The keyword suffixes with their suborders, for placing a theme cursor among
     them. *)
  let sorted_suffixes =
    List.map
      (fun v ->
        let suffix, _, order = data v in
        (suffix, order))
      all

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
    | Keyword v ->
        let _, _, order = data v in
        order

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

  let examples = [ Keyword Auto ]
end

open Handler

module Utility_factory = Utility.Make (Handler)
(** Register the cursor utility handlers *)

(** Public API returning Utility.t *)
let utility = Utility_factory.v

let cursor_alias = utility (Keyword Alias)
let cursor_all_scroll = utility (Keyword All_scroll)
let cursor_auto = utility (Keyword Auto)
let cursor_cell = utility (Keyword Cell)
let cursor_col_resize = utility (Keyword Col_resize)
let cursor_context_menu = utility (Keyword Context_menu)
let cursor_copy = utility (Keyword Copy)
let cursor_crosshair = utility (Keyword Crosshair)
let cursor_default = utility (Keyword Default)
let cursor_e_resize = utility (Keyword E_resize)
let cursor_ew_resize = utility (Keyword Ew_resize)
let cursor_grab = utility (Keyword Grab)
let cursor_grabbing = utility (Keyword Grabbing)
let cursor_help = utility (Keyword Help)
let cursor_move = utility (Keyword Move)
let cursor_n_resize = utility (Keyword N_resize)
let cursor_ne_resize = utility (Keyword Ne_resize)
let cursor_nesw_resize = utility (Keyword Nesw_resize)
let cursor_no_drop = utility (Keyword No_drop)
let cursor_none = utility (Keyword No_cursor)
let cursor_not_allowed = utility (Keyword Not_allowed)
let cursor_ns_resize = utility (Keyword Ns_resize)
let cursor_nw_resize = utility (Keyword Nw_resize)
let cursor_nwse_resize = utility (Keyword Nwse_resize)
let cursor_pointer = utility (Keyword Pointer)
let cursor_progress = utility (Keyword Progress)
let cursor_row_resize = utility (Keyword Row_resize)
let cursor_s_resize = utility (Keyword S_resize)
let cursor_se_resize = utility (Keyword Se_resize)
let cursor_sw_resize = utility (Keyword Sw_resize)
let cursor_text = utility (Keyword Text)
let cursor_vertical_text = utility (Keyword Vertical_text)
let cursor_w_resize = utility (Keyword W_resize)
let cursor_wait = utility (Keyword Wait)
let cursor_zoom_in = utility (Keyword Zoom_in)
let cursor_zoom_out = utility (Keyword Zoom_out)
