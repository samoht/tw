(** Scrollbar utilities: scrollbar-width, scrollbar-gutter, scrollbar-thumb,
    scrollbar-track. *)

module Css = Cascade.Css

let thumb_var =
  Var.property_default Css.Color ~initial:(Css.hex "#0000") ~property_order:30
    "tw-scrollbar-thumb"

let track_var =
  Var.property_default Css.Color ~initial:(Css.hex "#0000") ~property_order:31
    "tw-scrollbar-track"

module Handler = struct
  open Style

  (* A scrollbar-thumb / -track colour source. *)
  type color_spec =
    | Theme of Color.color * int * Color.opacity_modifier
    | Bracket of string * Css.color * Color.opacity_modifier
    | Current
    | Inherit
    | Transparent

  type t =
    | Width_auto
    | Width_none
    | Width_thin
    | Gutter_auto
    | Gutter_stable
    | Gutter_both
    | Thumb of color_spec
    | Track of color_spec

  let name = "scrollbar"

  (* Scrollbar properties follow scroll padding and precede list style. *)
  let priority _ = 11

  let suborder = function
    | Width_auto | Width_none | Width_thin -> 1_450_000
    | Thumb _ | Track _ -> 1_460_000
    | Gutter_auto | Gutter_both | Gutter_stable -> 1_470_000

  let spec_class = function
    | Theme (color, shade, op) ->
        let base =
          if Color.is_shadeless color then Color.color_to_string color
          else Color.color_to_string color ^ "-" ^ string_of_int shade
        in
        base ^ Color.opacity_suffix op
    | Bracket (raw, _, op) -> raw ^ Color.opacity_suffix op
    | Current -> "current"
    | Inherit -> "inherit"
    | Transparent -> "transparent"

  let to_class = function
    | Width_auto -> "scrollbar-auto"
    | Width_none -> "scrollbar-none"
    | Width_thin -> "scrollbar-thin"
    | Gutter_auto -> "scrollbar-gutter-auto"
    | Gutter_stable -> "scrollbar-gutter-stable"
    | Gutter_both -> "scrollbar-gutter-both"
    | Thumb s -> "scrollbar-thumb-" ^ spec_class s
    | Track s -> "scrollbar-track-" ^ spec_class s

  let hex_string_of (c : Css.color) : string =
    let fmt r g b a =
      let h2 = Pp.hex_byte in
      "#" ^ h2 r ^ h2 g ^ h2 b ^ if a = 255 then "" else h2 a
    in
    match c with
    | Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ } -> fmt r g b a
    | _ -> (
        match Color.css_color_to_hex c with
        | Some (Css.Hex { r; g; b; a } | Css.Authored_hex { r; g; b; a; _ }) ->
            fmt r g b a
        | _ -> "#000000")

  (* Return (declarations placed on the rule, optional @supports rules) that set
     [set_var] to the resolved colour. *)
  let value_decls ?theme ~set_var spec :
      Css.declaration list * Css.statement list =
    let keyword k =
      Css.custom_property ~layer:"utilities" (Var.css_name set_var) k
    in
    match spec with
    (* The two keywords need opposite treatment, and neither is a workaround for
       cascade.

       [currentcolor] as text: this custom property is a token stream, so a
       typed [Current] folds through it to [#00000000] where Tailwind writes the
       keyword. The optimizer is right to fold a colour it can resolve; what it
       cannot know is that this stream is read by a property that resolves
       [currentcolor] against the element. (cascade's own capital-C spelling is
       correct too, and positional: a bare [accent-color] takes [currentColor],
       the same keyword inside a [color-mix()] takes [currentcolor], which is
       what the upstream fixtures hold.)

       [transparent] typed: a token stream is exactly what folds to [#0000], so
       spelling it as text produced the divergence the text was meant to avoid.

       Neither shows up under [--diff] on one class - nothing reads the property
       in a one-class sheet and canonical mode prunes it - so the check is
       against [--tailwind] on the emitted property, not the diff. *)
    | Transparent ->
        ([ fst (Var.binding set_var (Css.Transparent : Css.color)) ], [])
    | Current -> ([ keyword "currentcolor" ], [])
    | Inherit -> ([ fst (Var.binding set_var (Css.Inherit : Css.color)) ], [])
    | Theme (color, shade, Color.No_opacity) ->
        let color_decl, color_ref =
          Var.binding (Color.color_var color shade) (Color.to_css color shade)
        in
        let set_decl, _ = Var.binding set_var (Css.Var color_ref) in
        ([ color_decl; set_decl ], [])
    | Theme (color, shade, op) ->
        let percent = Color.opacity_to_percent op in
        let fallback_hex =
          match Color.hex_alpha_color ?theme color shade op with
          | Some h -> h
          | None -> "#000000"
        in
        let fallback, _ = Var.binding set_var (Css.hex fallback_hex) in
        let color_decl, color_ref =
          Var.binding (Color.color_var color shade) (Color.to_css color shade)
        in
        let oklab =
          Css.color_mix ~in_space:Oklab (Css.Var color_ref) Css.Transparent
            ~percent1:percent
        in
        let supports_decl, _ = Var.binding set_var oklab in
        let supports =
          Css.supports ~condition:Color.color_mix_supports_condition
            [
              Css.rule ~selector:(Css.Selector.class_ "_")
                [ color_decl; supports_decl ];
            ]
        in
        ([ fallback ], [ supports ])
    | Bracket (_, css_color, Color.No_opacity) ->
        let folded =
          match Color.css_color_to_hex css_color with
          | Some h -> h
          | None -> css_color
        in
        ([ fst (Var.binding set_var folded) ], [])
    | Bracket (_, css_color, op) ->
        let percent = Color.opacity_to_percent op in
        let oklab =
          Color.hex_to_oklab_alpha (hex_string_of css_color) (percent /. 100.)
        in
        ([ fst (Var.binding set_var oklab) ], [])

  let compose ?theme ~set_var spec =
    let main_decls, supports_rules = value_decls ?theme ~set_var spec in
    let scrollbar_color_decl =
      Css.scrollbar_color
        (Colors
           (Css.Var (Var.reference thumb_var), Css.Var (Var.reference track_var)))
    in
    let property_rules =
      [ Var.property_rule thumb_var; Var.property_rule track_var ]
      |> List.filter_map (fun x -> x)
      |> Css.concat
    in
    match supports_rules with
    | [] -> style ~property_rules (main_decls @ [ scrollbar_color_decl ])
    | _ ->
        (* Tailwind sets the fallback custom property, then upgrades it inside
           @supports, then applies scrollbar-color in a trailing rule so the
           var() reference sits after the override. Emit all three through
           [rules] (empty main props) to keep that order. *)
        let self decls = Css.rule ~selector:(Css.Selector.class_ "_") decls in
        let ordered =
          (self main_decls :: supports_rules)
          @ [ self [ scrollbar_color_decl ] ]
        in
        style ~property_rules ~rules:(Some ordered) []

  let to_style theme =
    let compose ~set_var s = compose ~theme ~set_var s in
    function
    | Width_auto -> style [ Css.scrollbar_width Auto ]
    | Width_none -> style [ Css.scrollbar_width None ]
    | Width_thin -> style [ Css.scrollbar_width Thin ]
    | Gutter_auto -> style [ Css.scrollbar_gutter Auto ]
    | Gutter_stable -> style [ Css.scrollbar_gutter Stable ]
    | Gutter_both -> style [ Css.scrollbar_gutter Stable_both_edges ]
    | Thumb s -> compose ~set_var:thumb_var s
    | Track s -> compose ~set_var:track_var s

  let has_opacity s = String.contains s '/'

  let parse_color ?theme mk rest =
    match rest with
    | [ "inherit" ] -> Ok (mk Inherit)
    | [ "transparent" ] -> Ok (mk Transparent)
    | [ current_str ] when String.starts_with ~prefix:"current" current_str -> (
        let _, op = Color.parse_opacity_modifier ?theme current_str in
        match op with
        | Color.No_opacity -> Ok (mk Current)
        | _ -> Error (`Msg "Not a scrollbar utility"))
    | [ v ]
      when Parse.is_bracket_value (fst (Color.parse_opacity_modifier ?theme v))
      -> (
        let base, op = Color.parse_opacity_modifier ?theme v in
        let inner = Parse.bracket_inner base in
        match Color.parse_bracket_color inner with
        | Some c -> Ok (mk (Bracket (base, c, op)))
        | None -> Error (`Msg "Not a scrollbar utility"))
    | parts when List.exists has_opacity parts -> (
        match Color.shade_and_opacity_of_strings ?theme parts with
        | Ok (c, shade, op) -> Ok (mk (Theme (c, shade, op)))
        | Error e -> Error e)
    | parts -> (
        match Color.shade_of_strings ?theme parts with
        | Ok (c, shade) -> Ok (mk (Theme (c, shade, Color.No_opacity)))
        | Error e -> Error e)

  let of_class theme class_name =
    match Parse.split_class class_name with
    | [ "scrollbar"; "auto" ] -> Ok Width_auto
    | [ "scrollbar"; "none" ] -> Ok Width_none
    | [ "scrollbar"; "thin" ] -> Ok Width_thin
    | [ "scrollbar"; "gutter"; "auto" ] -> Ok Gutter_auto
    | [ "scrollbar"; "gutter"; "stable" ] -> Ok Gutter_stable
    | [ "scrollbar"; "gutter"; "both" ] -> Ok Gutter_both
    | "scrollbar" :: "thumb" :: rest ->
        parse_color ~theme (fun s -> Thumb s) rest
    | "scrollbar" :: "track" :: rest ->
        parse_color ~theme (fun s -> Track s) rest
    | _ -> Error (`Msg "Not a scrollbar utility")

  let examples = [ Width_auto; Gutter_auto; Thumb Current; Track Current ]
end

module Utility_factory = Utility.Make (Handler)
