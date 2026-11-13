(** Tests for colours under an opacity modifier. *)

module Color_opacity = Tw__.Color_opacity (* Access internal module *)
open Color_opacity

(* A class name is a selector, so it has to repeat the text the author wrote
   rather than a re-print of the number it parsed to. [/[25]] and [/[25.0]]
   denote one alpha and are two distinct classes; only the author's spelling
   matches the markup. *)
let test_opacity_modifier_class_roundtrip () =
  let roundtrip cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.(check string) "class" cls (Tw.pp u)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter roundtrip
    [
      "bg-red-500/50";
      "bg-red-500/2.5";
      "bg-red-500/[25]";
      "bg-red-500/[.5]";
      "bg-red-500/[25%]";
      "bg-red-500/[25.50%]";
      "bg-red-500/[var(--o)]";
      "bg-red-500/(--o)";
      "text-red-500/[25]";
      "border-red-500/[25]";
      "shadow-lg/[25]";
      "shadow-red-500/[25]";
      "inset-shadow-sm/[25]";
      "ring-red-500/[25]";
      "ring-offset-red-500/[25]";
      "inset-ring-red-500/[25]";
      "drop-shadow-lg/[25]";
      "drop-shadow/[25]";
      "text-shadow-lg/[25]";
      "decoration-red-500/[25]";
      "divide-red-500/[25]";
      "fill-red-500/[25]";
      "accent-red-500/[25]";
      "outline-red-500/[25]";
      "placeholder-red-500/[25]";
      "from-red-500/[25]";
      "[color:red]/[25]";
    ]

(* The bracket modifier holds a number, so a non-numeric spelling is not a
   utility at all rather than one with a surprising class name. *)
let test_opacity_modifier_rejects_non_numeric () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [
      "bg-red-500/[abc]";
      "bg-red-500/[]";
      "bg-red-500/[25px]";
      "bg-red-500/[%]";
      "shadow-lg/[25px]";
    ]

(* The modifier's [/] is the last one outside a bracket, as Tailwind reads a
   candidate, so the [/] a bracket value spells stays inside it:
   [shadow-[0_1px_2px_rgb(0_0_0_/_0.1)]/50] was an unknown class, the first [/]
   having split the bracket in two. *)
let test_modifier_slash_outside_bracket () =
  List.iter
    (fun cls ->
      Alcotest.(check bool)
        (cls ^ " parses") true
        (Result.is_ok (Tw.of_string cls)))
    [
      "shadow-[0_1px_2px_rgb(0_0_0_/_0.1)]/50";
      "bg-[rgb(0_0_0_/_0.5)]/50";
      "text-[color:rgb(0_0_0_/_0.5)]/50";
    ];
  Test_helpers.check_declarations "bg-[rgb(0_0_0_/_0.5)]/50"
    [ "background-color:color-mix(in oklab,#00000080 50%,transparent)" ]

(* The modifier is what follows the last [/] outside a bracket or a paren, and
   prints back the way the author spelled it. A spelling that is no modifier
   leaves the whole string as the base. *)
let test_parse_opacity_modifier () =
  let check s (base, modifier) =
    let b, m = parse_opacity_modifier s in
    Alcotest.(check string) (s ^ " base") base b;
    Alcotest.(check string) (s ^ " modifier") modifier (opacity_suffix m)
  in
  check "500/50" ("500", "/50");
  check "500/2.5" ("500", "/2.5");
  check "500/[0.5]" ("500", "/[0.5]");
  check "500/[25.0]" ("500", "/[25.0]");
  check "500/[50%]" ("500", "/[50%]");
  check "500/[var(--o)]" ("500", "/[var(--o)]");
  check "500/(--o)" ("500", "/(--o)");
  check "[rgb(0_0_0_/_0.5)]/50" ("[rgb(0_0_0_/_0.5)]", "/50");
  check "[rgb(0_0_0_/_0.5)]" ("[rgb(0_0_0_/_0.5)]", "");
  check "500" ("500", "");
  check "500/abc" ("500/abc", "");
  check "500/-5" ("500/-5", "");
  check "500/[abc]" ("500/[abc]", "");
  check "500/(o)" ("500/(o)", "");
  check "500/" ("500/", "")

(* A number is a percentage, a bare bracket number a fraction of one, and an
   alpha read from a variable has no value here, so it counts as 100. *)
let test_opacity_to_percent () =
  let percent s =
    match opacity_of_string s with
    | Some m -> opacity_to_percent m
    | None -> Alcotest.failf "%s is no modifier" s
  in
  let check s expected = Alcotest.(check (float 1e-9)) s expected (percent s) in
  check "50" 50.;
  check "2.5" 2.5;
  check "[0.5]" 50.;
  check "[25%]" 25.;
  check "[var(--o)]" 100.;
  check "(--o)" 100.;
  Alcotest.(check (float 0.)) "none" 100. (opacity_to_percent No_opacity)

(* Only a number at 100% or more is a no-op: a variable or a theme name has a
   value only the browser knows. *)
let test_is_fully_opaque () =
  let opaque s =
    match opacity_of_string s with
    | Some m -> is_fully_opaque m
    | None -> Alcotest.failf "%s is no modifier" s
  in
  Alcotest.(check bool) "100" true (opaque "100");
  Alcotest.(check bool) "[1]" true (opaque "[1]");
  Alcotest.(check bool) "99.5" false (opaque "99.5");
  Alcotest.(check bool) "(--o)" false (opaque "(--o)");
  Alcotest.(check bool) "no modifier" true (is_fully_opaque No_opacity)

(* The custom property a modifier reads, in either spelling, without its
   [--]. *)
let test_opacity_var_bare_of () =
  let var s = Option.bind (opacity_of_string s) opacity_var_bare_of in
  let check = Alcotest.(check (option string)) in
  check "shorthand" (Some "o") (var "(--o)");
  check "bracket" (Some "o") (var "[var(--o)]");
  check "number" None (var "50")

let tests =
  [
    ( "Opacity modifier class roundtrip",
      `Quick,
      test_opacity_modifier_class_roundtrip );
    ( "Opacity modifier rejects non-numeric",
      `Quick,
      test_opacity_modifier_rejects_non_numeric );
    ( "Modifier slash outside a bracket",
      `Quick,
      test_modifier_slash_outside_bracket );
    ("Parse opacity modifier", `Quick, test_parse_opacity_modifier);
    ("Opacity to percent", `Quick, test_opacity_to_percent);
    ("Fully opaque", `Quick, test_is_fully_opaque);
    ("Opacity variable name", `Quick, test_opacity_var_bare_of);
  ]

let suite = ("color_opacity", tests)
