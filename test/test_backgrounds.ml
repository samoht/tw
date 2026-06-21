open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Backgrounds.Handler)

let test_bg_colors () =
  check "bg-red-500";
  check "bg-blue-600";
  check "bg-green-700"

let test_gradient_direction () =
  let u = Tw.Backgrounds.bg_gradient_to Tw.Backgrounds.Bottom in
  Alcotest.check string "bg-gradient-to-b" "bg-gradient-to-b"
    (Tw.Utility.to_class u)

let test_gradient_colors () =
  let open Tw in
  let from = Backgrounds.from_color Color.red in
  let via = Backgrounds.via_color Color.blue ~shade:600 in
  let to_ = Backgrounds.to_color Color.green in
  Alcotest.check string "from-red-500" "from-red-500" (Utility.to_class from);
  Alcotest.check string "via-blue-600" "via-blue-600" (Utility.to_class via);
  Alcotest.check string "to-green-500" "to-green-500" (Utility.to_class to_)

let test_of_string_invalid () =
  (* Invalid background utilities *)
  let test_invalid =
    Test_helpers.check_invalid_parts (module Tw.Backgrounds.Handler)
  in

  (* Invalid gradient direction *)
  test_invalid [ "bg"; "gradient"; "to" ];
  (* Missing direction *)
  test_invalid [ "bg"; "gradient"; "to"; "invalid" ];
  (* Invalid direction *)
  test_invalid [ "bg"; "gradient"; "to"; "x" ];

  (* Invalid direction *)

  (* Invalid from/via/to colors *)
  test_invalid [ "from" ];
  (* Missing color *)
  test_invalid [ "from"; "invalid" ];
  (* Invalid color *)
  test_invalid [ "via" ];
  (* Missing color *)
  test_invalid [ "via"; "notacolor" ];
  (* Invalid color *)
  test_invalid [ "to" ];
  (* Missing color *)
  test_invalid [ "to"; "xyz" ];

  (* Invalid color *)

  (* Invalid prefixes *)
  test_invalid [ "bg" ];
  (* Incomplete *)
  test_invalid [ "bg"; "gradient" ];
  (* Incomplete *)
  test_invalid [ "unknown"; "red" ]
(* Unknown prefix *)

let suborder_matches_tailwind () =
  let open Tw in
  let colors = [ red; blue; green; yellow; purple; pink ] in
  let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ] in
  let utilities =
    List.concat_map
      (fun color -> List.map (fun shade -> bg ~shade color) shades)
      colors
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"backgrounds suborder matches Tailwind" shuffled

(* An arbitrary url() with its own quotes must not be double-wrapped: tw used to
   emit the broken url("'/img/x.png'"); it now canonicalises to a valid
   url(). *)
let test_bg_arbitrary_url () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  List.iter
    (fun cls ->
      let css = css_of cls in
      Alcotest.(check bool)
        (cls ^ " emits a valid url()")
        true
        (Astring.String.is_infix ~affix:"url(/img/x.png)" css);
      Alcotest.(check bool)
        (cls ^ " does not double-quote")
        false
        (Astring.String.is_infix ~affix:"url(\"'" css))
    [
      "bg-[url('/img/x.png')]";
      "bg-[url(\"/img/x.png\")]";
      "bg-[url(/img/x.png)]";
    ]

(* Arbitrary rgb()/rgba() gradient stops set the gradient colour rather than
   being silently dropped as a position (they used to produce no
   --tw-gradient-from). *)
let test_gradient_rgba_stop () =
  let css_of cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.failf "could not parse %S" cls
  in
  Alcotest.(check bool)
    "from-[rgba(...)] sets --tw-gradient-from" true
    (Astring.String.is_infix ~affix:"--tw-gradient-from"
       (css_of "from-[rgba(5,74,218,0.60)]"));
  Alcotest.(check bool)
    "to-[rgb(...)] sets --tw-gradient-to" true
    (Astring.String.is_infix ~affix:"--tw-gradient-to"
       (css_of "to-[rgb(16,26,50,0.60)]"))

(* A gradient stop-position utility (from-10%) registers the whole
   --tw-gradient-* @property family, like the colour utilities, matching the
   CLI. It used to register only the three *-position properties. *)
let test_gradient_stop_position_properties () =
  let css =
    match Tw.of_string "from-10%" with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error _ -> Alcotest.fail "could not parse from-10%"
  in
  Alcotest.(check bool)
    "from-10% sets --tw-gradient-from-position" true
    (Astring.String.is_infix ~affix:"--tw-gradient-from-position: 10%" css);
  Alcotest.(check bool)
    "from-10% registers @property --tw-gradient-from" true
    (Astring.String.is_infix ~affix:"@property --tw-gradient-from" css);
  Alcotest.(check bool)
    "from-10% registers @property --tw-gradient-stops" true
    (Astring.String.is_infix ~affix:"@property --tw-gradient-stops" css)

let tests =
  [
    test_case "bg colors" `Quick test_bg_colors;
    test_case "bg arbitrary url quoting" `Quick test_bg_arbitrary_url;
    test_case "arbitrary rgba gradient stop" `Quick test_gradient_rgba_stop;
    test_case "gradient stop-position @property family" `Quick
      test_gradient_stop_position_properties;
    test_case "gradient direction" `Quick test_gradient_direction;
    test_case "gradient colors" `Quick test_gradient_colors;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "backgrounds suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("backgrounds", tests)
