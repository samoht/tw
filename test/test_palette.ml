(** Tests for Tailwind's default colour palette. *)

module Palette = Tw__.Palette (* Access internal module *)
module Srgb = Tw__.Srgb

let families =
  List.filter (fun n -> not (List.mem n [ "black"; "white" ])) Palette.names

(* Every family Tailwind's [@theme] declares carries all eleven shades, and no
   other. *)
let test_every_family_has_every_shade () =
  Alcotest.(check int) "26 families" 26 (List.length families);
  List.iter
    (fun name ->
      List.iter
        (fun shade ->
          Alcotest.(check bool)
            (name ^ "-" ^ string_of_int shade)
            true
            (Option.is_some (Palette.oklch name shade)))
        Palette.shades;
      List.iter
        (fun shade ->
          Alcotest.(check bool)
            (name ^ "-" ^ string_of_int shade ^ " is no shade")
            true
            (Option.is_none (Palette.oklch name shade)))
        [ 0; 25; 150; 450; 501; 1000; -500 ])
    families

(* The values Tailwind v4's [@theme] declares, read off the shipped stylesheet:
   [--color-blue-500: oklch(62.3% .214 259.815)] and its neighbours. *)
let test_oklch_values () =
  let check name shade (l, c, h) =
    match Palette.oklch name shade with
    | None -> Alcotest.failf "%s-%d is missing" name shade
    | Some o ->
        let label = name ^ "-" ^ string_of_int shade in
        Alcotest.(check (float 0.)) (label ^ " lightness") l o.Srgb.l;
        Alcotest.(check (float 0.)) (label ^ " chroma") c o.Srgb.c;
        Alcotest.(check (float 0.)) (label ^ " hue") h o.Srgb.h
  in
  check "blue" 500 (62.3, 0.214, 259.815);
  check "red" 500 (63.7, 0.237, 25.331);
  check "gray" 50 (98.5, 0.002, 247.839);
  check "neutral" 500 (55.6, 0.0, 0.0);
  check "taupe" 950 (14.7, 0.004, 49.3)

(* A family name is read case-insensitively; anything else is no family. *)
let test_lookup_names () =
  let hue name = Option.map (fun o -> o.Srgb.h) (Palette.oklch name 500) in
  Alcotest.(check (option (float 0.))) "upper case" (hue "blue") (hue "BLUE");
  Alcotest.(check (option (float 0.))) "mixed case" (hue "blue") (hue "Blue");
  List.iter
    (fun name ->
      Alcotest.(check bool)
        (name ^ " is no family") true
        (Option.is_none (Palette.oklch name 500)))
    [ "black"; "white"; "transparent"; ""; "blue-500"; " blue"; "blu" ]

let test_oklch_css () =
  Alcotest.(check (option string))
    "blue-500" (Some "oklch(62.3% 0.214 259.815)")
    (Palette.oklch_css "blue" 500);
  Alcotest.(check (option string))
    "unknown shade" None
    (Palette.oklch_css "blue" 550)

(* [names] is the order Tailwind's [@theme] declares the colours in, which the
   theme layer sorts by: each name once, the four v4.3.3 families after stone,
   and the two shadeless colours last. *)
let test_names_order () =
  Alcotest.(check int)
    "each name once"
    (List.length Palette.names)
    (List.length (List.sort_uniq String.compare Palette.names));
  Alcotest.(check (list string))
    "the tail"
    [ "stone"; "mauve"; "olive"; "mist"; "taupe"; "black"; "white" ]
    (List.filteri (fun i _ -> i >= List.length Palette.names - 7) Palette.names);
  Alcotest.(check string) "red leads" "red" (List.hd Palette.names)

let tests =
  [
    ("Every family has every shade", `Quick, test_every_family_has_every_shade);
    ("OKLCh values", `Quick, test_oklch_values);
    ("Lookup names", `Quick, test_lookup_names);
    ("OKLCh CSS", `Quick, test_oklch_css);
    ("Names order", `Quick, test_names_order);
  ]

let suite = ("palette", tests)
