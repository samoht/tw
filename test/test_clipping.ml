module Css = Cascade.Css
open Alcotest

let test_clip_polygon () =
  let open Tw in
  let tri = Tw.clip_polygon [ (50., 0.); (0., 100.); (100., 100.) ] in
  check string "clip class" "clip-[polygon(50% 0%, 0% 100%, 100% 100%)]"
    (Tw.pp tri);
  let css = to_css [ tri ] |> Css.pp ~minify:false in
  check bool "has clip-path property" true
    (Astring.String.is_infix ~affix:"clip-path:" css);
  (* Polygon points preserve percentage units and the API keeps compact
     commas. *)
  check bool "has polygon value" true
    (Astring.String.is_infix ~affix:"polygon(50% 0%,0% 100%,100% 100%)" css)

(* Test clip-path: inset() parsing with 1-4 length values (CSS shorthand).
   Canonicalisation (e.g. dropping the unit on zero lengths inside basic-shape
   functions) lives in cascade's normalize/optimize pass, not in pp - so the
   helpers wrap the declaration in a tiny rule, run the optimize pipeline, and
   extract the declaration body for comparison. *)
let test_clip_inset_shorthand () =
  let normalize input =
    let wrapped = ".x{" ^ input ^ "}" in
    match Css.of_string wrapped with
    | Error _ -> Alcotest.fail (Fmt.str "Failed to parse: %s" input)
    | Ok { stylesheet; _ } ->
        let out =
          stylesheet
          |> Css.optimize ~scope:`Stylesheet
          |> Css.to_string ~minify:true
        in
        let n = String.length out in
        if n >= 4 && String.sub out 0 3 = ".x{" && out.[n - 1] = '}' then
          String.sub out 3 (n - 4)
        else out
  in
  let check_roundtrip input =
    check string (Fmt.str "parse %s" input) input (normalize input)
  in
  let check_parse input expected =
    check string (Fmt.str "parse %s" input) expected (normalize input)
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
