module Css = Cascade.Css

let test_default () =
  let s = Tw.Scheme.default in
  Alcotest.(check int) "default ring width" 1 s.default_ring_width;
  Alcotest.(check int) "default border width" 1 s.default_border_width;
  Alcotest.(check int) "default outline width" 1 s.default_outline_width

let test_find_color () =
  let s : Tw.Scheme.t =
    { Tw.Scheme.default with colors = [ ("red-500", Hex "#ef4444") ] }
  in
  Alcotest.(check bool)
    "finds defined color" true
    (Tw.Scheme.hex_color s "red-500" <> None);
  Alcotest.(check bool)
    "missing color returns none" true
    (Tw.Scheme.hex_color s "blue-500" = None)

(* The value [sheet]'s theme layer declares [name] with, once per declaration,
   so a token declared twice with two values shows both. *)
let theme_declarations name sheet =
  match Css.layer_block [ "theme" ] sheet with
  | None -> []
  | Some stmts ->
      List.concat_map
        (fun stmt ->
          match Css.as_rule stmt with
          | Some (_, decls, _) ->
              List.filter_map
                (fun d ->
                  if Css.custom_declaration_name d = Some name then
                    Some (Css.declaration_value ~minify:false d)
                  else None)
                decls
          | None -> [])
        stmts

let pink_override : Tw.Scheme.t =
  {
    Tw.Scheme.default with
    colors =
      [
        ("pink-500", Hex "#ff3c8e");
        ("black", Hex "#010203");
        ("purple-950", Oklch { l = 50.; c = 0.1; h = 200. });
      ];
  }

let check_declares ~theme ~token ~value utilities =
  let sheet = Tw.to_css ~theme ~base:false utilities in
  Alcotest.(check (list string))
    (String.concat " " (List.map Tw.pp utilities) ^ " declares " ^ token)
    [ value ]
    (theme_declarations token sheet)

(* Tailwind reads a palette colour through its [--color-*] token, and an
   [@theme] block that re-values the token re-colours every utility naming it:
   [@theme { --color-pink-500: #ff3c8e }] makes the theme layer declare
   [--color-pink-500: #ff3c8e] whether [bg-pink-500], [text-pink-500] or any
   other colour utility asked for it. A scheme colour is that token. *)
let test_color_override_every_family () =
  let theme = pink_override in
  let declares token value cls =
    match Tw.of_string ~theme cls with
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
    | Ok u -> check_declares ~theme ~token ~value [ u ]
  in
  List.iter
    (fun family -> declares "--color-pink-500" "#ff3c8e" (family ^ "-pink-500"))
    [
      "bg";
      "text";
      "border";
      "border-t";
      "outline";
      "ring";
      "ring-offset";
      "inset-ring";
      "divide";
      "decoration";
      "fill";
      "stroke";
      "accent";
      "caret";
      "placeholder";
      "from";
      "via";
      "to";
      "shadow";
      "inset-shadow";
      "text-shadow";
      "drop-shadow";
      "mask-linear-from";
      "scrollbar-thumb";
      "scrollbar-track";
    ];
  List.iter
    (fun family -> declares "--color-black" "#010203" (family ^ "-black"))
    [ "bg"; "text"; "border"; "from"; "fill"; "shadow" ];
  List.iter
    (fun family ->
      declares "--color-purple-950" "oklch(50% .1 200)" (family ^ "-purple-950"))
    [ "bg"; "text"; "border"; "from" ]

(* Two utilities naming one colour declare one token. Whichever comes first, the
   value is the override, not the palette default. *)
let test_color_override_any_order () =
  let theme = pink_override in
  let bg = Tw.bg ~shade:500 Tw.pink and text = Tw.text ~shade:500 Tw.pink in
  check_declares ~theme ~token:"--color-pink-500" ~value:"#ff3c8e" [ bg; text ];
  check_declares ~theme ~token:"--color-pink-500" ~value:"#ff3c8e" [ text; bg ]

(* [@theme { --color-neon-plum: #aa00aa }] declares a colour the palette lacks,
   and Tailwind then accepts [bg-neon-plum] and every other colour utility on
   it. A scheme colour is that token, so its name parses wherever a colour
   does. *)
let test_scheme_color_name_parses () =
  let theme =
    { Tw.Scheme.default with colors = [ ("neon-plum", Hex "#aa00aa") ] }
  in
  List.iter
    (fun cls ->
      match Tw.of_string ~theme cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok u ->
          Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp u);
          check_declares ~theme ~token:"--color-neon-plum" ~value:"#aa00aa"
            [ u ])
    [
      "bg-neon-plum";
      "text-neon-plum";
      "border-neon-plum";
      "divide-neon-plum";
      "fill-neon-plum";
      "from-neon-plum";
      "bg-neon-plum/50";
      "divide-neon-plum/50";
    ]

let test_breakpoint_override () =
  let s =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("breakpoint-10xl", "1600px") ]
  in
  Alcotest.(check (option (float 0.)))
    "breakpoint token populates the typed theme" (Some 1600.)
    (Tw.Scheme.breakpoint s "10xl")

(* A namespace reset spares the scales Tailwind lists as separate even though
   they share the prefix, so [--text-*: initial] drops the font sizes and leaves
   [--text-shadow-*] standing. *)
let test_namespace_reset_spares_nested_scales () =
  let s =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("text-*", "initial") ]
  in
  Alcotest.(check bool)
    "the font sizes go" true
    (Tw.Scheme.token s "text-sm" = None);
  Alcotest.(check bool)
    "the text shadows stay" true
    (Tw.Scheme.token s "text-shadow-2xs" <> None)

(* Tailwind reads [--*: initial] as a reset of the whole theme, not just of one
   namespace: every registered token goes, and only what the block declares in
   its own right stands. *)
let test_whole_theme_reset () =
  let s =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("*", "initial"); ("spacing", "2px") ]
  in
  Alcotest.(check bool)
    "the breakpoints go" true
    (Tw.Scheme.token s "breakpoint-md" = None);
  Alcotest.(check bool)
    "the font sizes go" true
    (Tw.Scheme.token s "text-sm" = None);
  Alcotest.(check (option string))
    "the block's own token stays" (Some "2px")
    (Tw.Scheme.token s "spacing")

let tests =
  Alcotest.
    [
      test_case "default scheme" `Quick test_default;
      test_case "namespace reset spares nested scales" `Quick
        test_namespace_reset_spares_nested_scales;
      test_case "whole theme reset" `Quick test_whole_theme_reset;
      test_case "find color" `Quick test_find_color;
      test_case "color override reaches every family" `Quick
        test_color_override_every_family;
      test_case "color override whatever the order" `Quick
        test_color_override_any_order;
      test_case "scheme colour name parses" `Quick test_scheme_color_name_parses;
      test_case "breakpoint override" `Quick test_breakpoint_override;
    ]

let suite = ("scheme", tests)
