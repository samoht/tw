open Alcotest

let test_aspect_classes () =
  let open Tw in
  check string "square class" "aspect-square" (Tw.pp aspect_square);
  check string "video class" "aspect-video" (Tw.pp aspect_video);
  check string "ratio class" "aspect-[16/9]" (Tw.pp (aspect_ratio 16 9))

let test_aspect_css () =
  let open Tw in
  let css = to_css [ aspect_ratio 16 9 ] |> Css.pp ~minify:false in
  check bool "has aspect-ratio" true
    (Astring.String.is_infix ~affix:"aspect-ratio" css);
  check bool "has 16/9" true (Astring.String.is_infix ~affix:"16/9" css)

let tests =
  [
    test_case "aspect classes" `Quick test_aspect_classes;
    test_case "aspect css" `Quick test_aspect_css;
  ]

let suite = ("aspect", tests)
