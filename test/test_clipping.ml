open Alcotest

let test_clip_polygon () =
  let open Tw in
  let tri = Tw.clip_polygon [ (50., 0.); (0., 100.); (100., 100.) ] in
  check string "clip class" "clip-[polygon(50% 0%, 0% 100%, 100% 100%)]"
    (Tw.pp tri);
  let css = to_css [ tri ] |> Css.pp ~minify:false in
  check bool "has clip property" true
    (Astring.String.is_infix ~affix:"clip:" css);
  check bool "has polygon value" true
    (Astring.String.is_infix ~affix:"polygon(50% 0%, 0% 100%, 100% 100%)" css)

let tests = [ test_case "clip polygon" `Quick test_clip_polygon ]
