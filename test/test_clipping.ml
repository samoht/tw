open Alcotest

let test_clip_polygon () =
  let open Tw in
  let tri = Tw.clip_polygon [ (50., 0.); (0., 100.); (100., 100.) ] in
  check string "clip class" "clip-[polygon(50% 0%, 0% 100%, 100% 100%)]"
    (Tw.pp tri);
  let css = to_css [ tri ] |> Css.pp ~minify:false in
  check bool "has clip-path property" true
    (Astring.String.is_infix ~affix:"clip-path:" css);
  (* Note: 0% is output as 0 in CSS, so check for that pattern *)
  check bool "has polygon value" true
    (Astring.String.is_infix ~affix:"polygon(50% 0, 0 100%, 100% 100%)" css)

(* Test clip-path: inset() parsing with 1-4 length values (CSS shorthand) *)
let test_clip_inset_shorthand () =
  let check_roundtrip input =
    let r = Css.Reader.of_string input in
    match Css.Declaration.read_declaration r with
    | None -> Alcotest.fail "Failed to parse declaration"
    | Some decl ->
        let output =
          Css.Pp.to_string ~minify:true Css.Declaration.pp_declaration decl
        in
        check string (Fmt.str "parse %s" input) input output
  in
  let check_parse input expected =
    let r = Css.Reader.of_string input in
    match Css.Declaration.read_declaration r with
    | None -> Alcotest.fail "Failed to parse declaration"
    | Some decl ->
        let output =
          Css.Pp.to_string ~minify:true Css.Declaration.pp_declaration decl
        in
        check string (Fmt.str "parse %s" input) expected output
  in
  (* 1 value: all four sides get the same value *)
  check_roundtrip "clip-path:inset(50%)";
  check_roundtrip "clip-path:inset(10px)";
  (* 2 values: top/bottom, left/right *)
  check_roundtrip "clip-path:inset(10% 20%)";
  (* 3 values: top, left/right, bottom *)
  check_roundtrip "clip-path:inset(10% 20% 30%)";
  (* 4 values: top, right, bottom, left *)
  check_parse "clip-path:inset(0px 10px 20px 30px)"
    "clip-path:inset(0 10px 20px 30px)"

let tests =
  [
    test_case "clip polygon" `Quick test_clip_polygon;
    test_case "clip inset shorthand" `Quick test_clip_inset_shorthand;
  ]

let suite = ("clipping", tests)
