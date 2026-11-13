(** Tests for the colours a class names and the theme tokens they read. *)

module Css = Cascade.Css
module Srgb = Tw__.Srgb (* Access internal module *)
module Color_value = Tw__.Color_value
open Color_value

(* A custom property is a dashed ident, so every byte of its name outside the
   CSS name set has to carry a backslash for the whole thing to lex as one
   token. Walking the name is the check: a bare [#], [(] or [,] ends the ident
   and turns the rest into stray tokens. *)
let check_escaped_name name =
  let n = String.length name in
  let is_name_code_point c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' -> true
    | c -> Char.code c >= 0x80
  in
  let rec walk i =
    if i >= n then ()
    else if name.[i] = '\\' then walk (i + 2)
    else if is_name_code_point name.[i] then walk (i + 1)
    else Alcotest.failf "--%s: %C at offset %d is unescaped" name name.[i] i
  in
  walk 0

(* [color_var] is public, so it has to name a variable that a browser can read
   whatever colour it is handed - the utility API happens to branch on
   [is_custom_color] before it gets here, but the signature makes no such
   promise. *)
let test_color_var_name_is_one_ident () =
  List.iter
    (fun color -> check_escaped_name (Tw.Var.name (color_var color 500)))
    [
      Red;
      Black;
      Theme_named "brand";
      Hex "0088cc";
      Rgb { red = 1; green = 2; blue = 3 };
      Oklch Srgb.{ l = 50.; c = 0.1; h = 20. };
      Css (Css.hex "#0088cc");
    ]

(* The theme layer and the utilities layer rank the same colour names in
   different orders, and a name missing from either map silently takes that
   map's unknown-colour slot. Both are derived from one list, so the pairing
   holds by construction; this is what fails if that stops being true. *)
let ranked_color_names =
  [
    "transparent";
    "black";
    "white";
    "red";
    "orange";
    "amber";
    "yellow";
    "lime";
    "green";
    "emerald";
    "teal";
    "cyan";
    "sky";
    "blue";
    "indigo";
    "violet";
    "purple";
    "fuchsia";
    "pink";
    "rose";
    "slate";
    "gray";
    "zinc";
    "neutral";
    "stone";
    "mauve";
    "olive";
    "mist";
    "taupe";
  ]

let test_color_orders_cover_the_same_names () =
  let unknown_theme = Color_value.theme_order "definitely-not-a-colour" in
  let unknown_utilities =
    Color_value.utilities_order "definitely-not-a-colour"
  in
  Alcotest.(check (pair int int))
    "unknown theme colour falls back" (2, 100000) unknown_theme;
  Alcotest.(check (pair int int))
    "unknown utility colour falls back" (2, 100) unknown_utilities;
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " has a theme slot")
        false
        (Color_value.theme_order name = unknown_theme);
      Alcotest.(check bool)
        (name ^ " has a utilities slot")
        false
        (Color_value.utilities_order name = unknown_utilities))
    ranked_color_names

(* The utilities layer leads with transparent and black and is alphabetical from
   there. *)
let test_utilities_color_order_is_alphabetical () =
  let order name = snd (Color_value.utilities_order name) in
  Alcotest.(check int) "transparent leads" 0 (order "transparent");
  Alcotest.(check int) "black follows" 1 (order "black");
  Alcotest.(check int) "amber opens the alphabetical run" 2 (order "amber");
  Alcotest.(check int) "white sits between violet and yellow" 26 (order "white");
  Alcotest.(check int) "zinc closes it" 28 (order "zinc");
  (* The v4.3.3 families take their alphabetical places rather than the
     unknown-colour slot. *)
  Alcotest.(check int) "mauve follows lime" 11 (order "mauve");
  Alcotest.(check int) "mist follows mauve" 12 (order "mist");
  Alcotest.(check int) "olive follows neutral" 14 (order "olive");
  Alcotest.(check int) "taupe follows stone" 23 (order "taupe")

(* Tailwind 4.3.3 declares mauve, olive, mist and taupe in its [@theme] after
   stone and before black, in that order. Read off [@import "tailwindcss"
   theme(static)]. *)
let test_v433_families_rank_in_theme_order () =
  let order name = snd (Color_value.theme_order name) in
  let names =
    [ "stone"; "mauve"; "olive"; "mist"; "taupe"; "black"; "white" ]
  in
  let orders = List.map order names in
  Alcotest.(check (list int))
    "the four families sit between stone and black, each in its own slot"
    (List.sort_uniq compare orders)
    orders

(* A palette colour reads its [--color-*] token, shaded or not; a colour that
   carries its own value, or names a project token, reads none. *)
let test_palette_token () =
  let token = Alcotest.(check (option string)) in
  token "shaded" (Some "color-red-500") (palette_token Red 500);
  token "shadeless" (Some "color-black") (palette_token Black 500);
  token "shadeless ignores the shade" (Some "color-white")
    (palette_token White 900);
  token "hex" None (palette_token (Hex "0088cc") 500);
  token "project token" None (palette_token (Theme_named "brand") 500)

(* A [color-*] token name maps back to the palette colour it names; a name the
   palette does not know, or one with no [color-] prefix, maps to nothing. *)
let test_theme_color_of_name () =
  let name n =
    Option.map
      (fun (c, shade) -> color_to_string c ^ "/" ^ string_of_int shade)
      (theme_color_of_name n)
  in
  let check = Alcotest.(check (option string)) in
  check "shaded" (Some "red/500") (name "color-red-500");
  check "shadeless" (Some "black/500") (name "color-black");
  check "no prefix" None (name "red-500");
  check "prefix alone" None (name "color-");
  check "unknown shade" None (name "color-red-550");
  check "unknown family" None (name "color-brand")

(* [shade_of_strings] takes a palette family with one of its shades, or a
   shadeless colour on its own; a family alone reads [--color-red], a token the
   default theme never declares. *)
let test_shade_of_strings () =
  let parse parts =
    match shade_of_strings parts with
    | Ok (c, shade) -> Some (color_to_string c ^ "/" ^ string_of_int shade)
    | Error _ -> None
  in
  let check = Alcotest.(check (option string)) in
  check "family and shade" (Some "blue/500") (parse [ "blue"; "500" ]);
  check "black" (Some "black/500") (parse [ "black" ]);
  check "family alone" None (parse [ "blue" ]);
  check "shade off the scale" None (parse [ "blue"; "550" ]);
  check "shade on a shadeless colour" None (parse [ "black"; "500" ]);
  check "nothing" None (parse [])

let tests =
  [
    ( "Colour variable name is one ident",
      `Quick,
      test_color_var_name_is_one_ident );
    ( "Colour orders cover the same names",
      `Quick,
      test_color_orders_cover_the_same_names );
    ( "Utilities colour order is alphabetical",
      `Quick,
      test_utilities_color_order_is_alphabetical );
    ( "v4.3.3 families rank in theme order",
      `Quick,
      test_v433_families_rank_in_theme_order );
    ("Palette token", `Quick, test_palette_token);
    ("Theme colour of name", `Quick, test_theme_color_of_name);
    ("Shade of strings", `Quick, test_shade_of_strings);
  ]

let suite = ("color_value", tests)
