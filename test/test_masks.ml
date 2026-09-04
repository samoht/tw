let check = Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler)

let test_roundtrip () =
  check "mask-none";
  check "mask-add";
  check "mask-exclude";
  check "mask-intersect";
  check "mask-subtract";
  check "mask-alpha";
  check "mask-luminance";
  check "mask-match";
  check "mask-type-alpha";
  check "mask-type-luminance";
  check "mask-auto";
  check "mask-clip-border";
  check "mask-clip-padding";
  check "mask-clip-content";
  check "mask-clip-fill";
  check "mask-clip-stroke";
  check "mask-clip-view";
  check "mask-no-clip";
  check "mask-origin-border";
  check "mask-origin-padding";
  check "mask-origin-content";
  check "mask-origin-fill";
  check "mask-origin-stroke";
  check "mask-origin-view"

let test_invalid () =
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask-foo";
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask"

(* The typed mask constructors are newly exposed in tw.mli; check a sample
   across the families agrees with the parser on class names. *)
let test_typed () =
  Test_helpers.check_typed_class "mask-clip-border" Tw.mask_clip_border;
  Test_helpers.check_typed_class "mask-no-clip" Tw.mask_no_clip;
  Test_helpers.check_typed_class "mask-add" Tw.mask_add;
  Test_helpers.check_typed_class "mask-none" Tw.mask_none;
  Test_helpers.check_typed_class "mask-alpha" Tw.mask_alpha;
  Test_helpers.check_typed_class "mask-origin-border" Tw.mask_origin_border;
  Test_helpers.check_typed_class "mask-center" Tw.mask_center;
  Test_helpers.check_typed_class "mask-top-right" Tw.mask_top_right;
  Test_helpers.check_typed_class "mask-repeat" Tw.mask_repeat;
  Test_helpers.check_typed_class "mask-cover" Tw.mask_cover;
  Test_helpers.check_typed_class "mask-type-alpha" Tw.mask_type_alpha

(* [mask-[<image>]] takes any background-image, not only a linear-gradient. *)
let test_bracket_image () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "radial-gradient reaches mask-image" true
    (Astring.String.is_infix ~affix:"mask-image:radial-gradient(white,black)"
       (css "mask-[radial-gradient(white,black)]"));
  check "mask-[radial-gradient(white,black)]";
  check "mask-[conic-gradient(white,black)]";
  check "mask-[linear-gradient(white,black)]"

(* A mask takes one image and one position per layer, comma-separated. The
   single-[url(...)] reading used to swallow the comma, and a position list was
   rejected outright. *)
let test_bracket_layer_list () =
  (* Read unminified. What is under test is that the list keeps both layers, and
     minified spelling is not that question: a [%] ends a percentage token, so
     [30% 50%] compacts to [30%50%] - still two components, still valid, and it
     round-trips - while a different minifier folds the pair to its [x] alone.
     Pinning either spelling pins the minifier instead of the reading. *)
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "two url layers stay two" true
    (Astring.String.is_infix ~affix:"mask-image: url(/a.png), url(/b.png)"
       (css "mask-[url(/a.png),url(/b.png)]"));
  Alcotest.(check bool)
    "two positions stay two" true
    (Astring.String.is_infix ~affix:"mask-position: 30% 50%, 70% 50%"
       (css "mask-position-[30%_50%,70%_50%]"))

(* A bracket that would end the declaration or swallow what follows it is the
   one thing no mask utility can hold, and Tailwind writes nothing for it
   either. Every other unreadable value is emitted verbatim; see
   {!test_bracket_falls_through_to_a_longhand}. *)
let test_invalid_bracket_value () =
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask-[a;b]";
  Test_helpers.check_invalid_input (module Tw.Masks.Handler) "mask-size-[a;b]";
  Test_helpers.check_invalid_input
    (module Tw.Masks.Handler)
    "mask-[position:a;b]"

(* A bracket no reader here takes is still a utility: Tailwind writes it into
   the longhand the class names, spelled as the author wrote it, and leaves the
   browser to decide whether it means anything. The value's own validity is not
   what selects the longhand, so [mask-[red]] is an image and not a refusal. *)
let test_bracket_falls_through_to_a_longhand () =
  let has cls decls = Test_helpers.check_declarations cls decls in
  let image cls value =
    has cls [ "-webkit-mask-image:" ^ value; "mask-image:" ^ value ]
  in
  let position cls value =
    has cls [ "-webkit-mask-position:" ^ value; "mask-position:" ^ value ]
  in
  let size cls value =
    has cls [ "-webkit-mask-size:" ^ value; "mask-size:" ^ value ]
  in
  (* an unhinted bracket no image and no position reader takes *)
  image "mask-[foo]" "foo";
  image "mask-[red]" "red";
  image "mask-[--c]" "--c";
  (* the [url()] that is not one whole token, so no position goes with it *)
  image "mask-[url(x.png)_center]" "url(x.png) center";
  (* Tailwind reads a math function as a length, and a length is a position
     component, so such a bracket goes to mask-position however the rest of it
     reads. Cascade will not read [calc(1 + 2)] back as a position, which is
     correct of it: a bare number is no length-percentage. *)
  position "mask-[calc(1+2)]" "calc(1 + 2)";
  position "mask-[foo_calc(1+2)]" "foo calc(1 + 2)";
  (* a sub-property bracket names its longhand outright *)
  position "mask-position-[foo]" "foo";
  size "mask-size-[foo]" "foo";
  (* a data-type hint names it too, and says nothing about the value *)
  image "mask-[image:notanimage]" "notanimage";
  image "mask-[url:notaurl]" "notaurl";
  size "mask-[length:nope]" "nope";
  size "mask-[size:nope]" "nope";
  position "mask-[position:nope]" "nope";
  (* A sub-property class names its longhand outright, so a hint in front of the
     value only says where the value starts - whatever the hint is. *)
  position "mask-position-[length:2em]" "2em";
  position "mask-position-[bogus:2em]" "2em";
  size "mask-size-[size:2em]" "2em";
  (* An unknown hint on the unhinted class settles the longhand all the same: it
     takes the last resort, and nothing is read from what follows. A bare [2em]
     with no hint would have been a position. *)
  image "mask-[bogus:2em]" "2em";
  image "mask-[bogus:url(a.png)]" "url(a.png)";
  (* The name has to be an identifier to be a hint, so this one is the value. *)
  image "mask-[10px:2em]" "10px:2em";
  (* and a [:] inside a function call belongs to the value *)
  position "mask-position-[url(http://x/a.png)]" "url(http://x/a.png)";
  (* every one of them prints back as the author wrote it: a hint dropped from
     the class name is a selector that matches no markup *)
  List.iter
    (fun cls ->
      Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler) cls)
    [
      "mask-[foo]";
      "mask-[url(x.png)_center]";
      "mask-[calc(1+2)]";
      "mask-position-[foo]";
      "mask-size-[foo]";
      "mask-[image:notanimage]";
      "mask-[length:nope]";
      "mask-[position:nope]";
      "mask-position-[length:2em]";
      "mask-size-[size:2em]";
      "mask-[bogus:2em]";
      "mask-[10px:2em]";
    ]

(* mask-size-[...], mask-[size:...] and mask-[length:...] only read px, %, and
   rem before; every CSS length unit does now, matching real Tailwind's
   mask-size-[2em]. *)
let test_bracket_size_units () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "mask-size-[2em]" "mask-size:2em";
  has "mask-[size:2em]" "mask-size:2em";
  has "mask-[length:2em]" "mask-size:2em"

(* A mask-position bracket takes the whole CSS grammar, the same as
   background-position: a single edge keyword and the edge/offset form. Both
   used to fall through the hand-rolled parser to a silent [center]. *)
let test_bracket_position_grammar () =
  (* Unminified, so the assertions read as the grammar they are about. Compact
     spelling belongs to the minifier: [10px 20px] keeps its space while [30%
     50%] loses it, because a [%] already ends the token. *)
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "mask-[position:top]" "mask-position: top";
  has "mask-position-[top]" "mask-position: top";
  has "mask-[top]" "mask-position: top";
  (* the lengths and layer-list forms are unchanged *)
  has "mask-[position:10px_20px]" "mask-position: 10px 20px";
  has "mask-position-[30%_50%,70%_50%]" "mask-position: 30% 50%, 70% 50%"

(* Masks sit between the backgrounds and fill/stroke, and the mask-gradient
   utilities lead them. Sharing padding's slot interleaved the two families with
   the padding rules, which breaks the adjacency the block merging relies on. *)
let order_matches_tailwind () =
  let classes =
    [
      "bg-red-500";
      "mask-x-from-20%";
      "mask-b-from-50%";
      "mask-none";
      "mask-top";
      "mask-cover";
      "mask-repeat-x";
      "fill-blue-500";
      "stroke-green-500";
      "p-4";
      "pt-2";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  Test_helpers.check_ordering_matches ~test_name:"mask order matches Tailwind"
    (Test_helpers.shuffle utilities)

(* A mask image is an arbitrary value, so [_] is a space and [\_] a literal
   underscore: a file name carrying one is written with the escape. *)
let test_bracket_image_underscore_escape () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  Alcotest.(check bool)
    "an escaped underscore stays in the file name" true
    (Astring.String.is_infix ~affix:"mask-image: url('a_b.png')"
       (css {|mask-[url('a\_b.png')]|}))

(* Tailwind leaves a bare [_] alone inside [url()], where it is part of the file
   name rather than an encoded space, so [mask-[url('a_b.png')]] names
   [a_b.png]. Only the [\_] escape is undone, the way the background family
   already reads it. The quotes are the minifier's to drop, on both sides. *)
let test_bracket_url_keeps_underscores () =
  Test_helpers.check_declarations "mask-[url('a_b.png')]"
    [ "-webkit-mask-image:url(a_b.png)"; "mask-image:url(a_b.png)" ];
  Test_helpers.check_declarations "mask-[url(a_b.png)]"
    [ "-webkit-mask-image:url(a_b.png)"; "mask-image:url(a_b.png)" ]

(* A [url()] argument is left verbatim, so a bare [_] in a file name is a file
   name too. Only the [_] outside the url is a space, which [image-set()] is the
   case that tells the two apart. *)
let test_bracket_url_underscore () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " emits " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has "mask-[url('a_b.png')]" "mask-image: url('a_b.png')";
  (* Tailwind writes the inner url quoted. The quoting is cascade's canonical
     spelling of the same URL; the underscore is the point. *)
  has "mask-[image-set(url('a_b.png')_1x)]"
    "mask-image: image-set(url(a_b.png) 1x)"

(* A [url()] carrying a position is no [url()] token, so the typed reading has
   no answer for it and the whole bracket goes out verbatim instead. What must
   not come back is the slice between the bracket's first [(] and its last [)]:
   it cuts the file name at the trailing word, and both halves of the utility
   carry the cut, so [mask-[url(x.png)_center]] named itself
   [.mask-\[url\(x\.png\)_cente\)\]] - a selector no markup carries - and held
   [url("x.png)_cente")]. The [bg-] family reads its bracket the same way. *)
let test_bracket_url_needs_a_whole_token () =
  (* Read unminified, so the assertions read as the values they are about: a [%]
     already ends its token, so the minifier writes [50% 50%] compactly. *)
  let whole cls value =
    Test_helpers.check_declarations ~minify:false cls
      [ "-webkit-mask-image: " ^ value; "mask-image: " ^ value ];
    Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler) cls
  in
  whole "mask-[url(x.png)_center]" "url(x.png) center";
  whole "mask-[url(x.png)_no-repeat]" "url(x.png) no-repeat";
  whole "mask-[url(x.png)_50%_50%]" "url(x.png) 50% 50%";
  (* The whole token reads as a token, so cascade prints it as one. *)
  whole "mask-[url(x.png)]" "url(x.png)"

(* An [image:]/[url:] hint says how to read the value written after it; it does
   not make that value the name of a custom property. [mask-[image:url(a.png)]]
   wrote [mask-image: var(--url\(a\.png\))]. *)
let test_bracket_data_type_hint_reads_the_value () =
  Test_helpers.check_declarations "mask-[image:url(a.png)]"
    [ "-webkit-mask-image:url(a.png)"; "mask-image:url(a.png)" ];
  Test_helpers.check_declarations "mask-[url:url(a.png)]"
    [ "-webkit-mask-image:url(a.png)"; "mask-image:url(a.png)" ];
  (* a var() reference after the hint still names a custom property *)
  Test_helpers.check_declarations "mask-[image:var(--some-var)]"
    [
      "-webkit-mask-image:var(--some-var)";
      "-webkit-mask-image:var(--some-var)";
      "mask-image:var(--some-var)";
    ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "mask-[url:url(a.png)] round-trips" "mask-[url:url(a.png)]"
    (Tw.pp (Result.get_ok (Tw.of_string "mask-[url:url(a.png)]")));
  (* A value no image reader takes is written out as it stands: the hint says
     which longhand takes the bracket and nothing about what may be in it. *)
  Test_helpers.check_declarations "mask-[image:notanimage]"
    [ "-webkit-mask-image:notanimage"; "mask-image:notanimage" ]

(* The hint's name is a run of [a-z] and [-] and nothing else, so an upper-case
   letter or a digit ends it and the whole bracket is the value. A bracket whose
   hint is empty, and one with nothing after the hint, name no utility. *)
let test_hint_name_is_a_lower_case_run () =
  let image cls value =
    Test_helpers.check_declarations cls
      [ "-webkit-mask-image:" ^ value; "mask-image:" ^ value ]
  in
  image "mask-[FOO:2em]" "FOO:2em";
  image "mask-[a1:2em]" "a1:2em";
  Test_helpers.check_declarations "mask-size-[FOO:2em]"
    [ "-webkit-mask-size:FOO:2em"; "mask-size:FOO:2em" ];
  (* only the leading run is the hint's, so the value keeps its own [:] *)
  image "mask-[bogus:foo:2em]" "foo:2em";
  List.iter
    (fun cls ->
      Test_helpers.check_handler_roundtrip (module Tw.Masks.Handler) cls)
    [ "mask-[FOO:2em]"; "mask-[a1:2em]"; "mask-size-[FOO:2em]" ];
  List.iter
    (Test_helpers.check_invalid_input (module Tw.Masks.Handler))
    [ "mask-[:2em]"; "mask-[foo:]"; "mask-size-[:2em]"; "mask-position-[:2em]" ]

let tests =
  Test_helpers.standard ~roundtrip:test_roundtrip ~invalid:test_invalid
  @ [
      Alcotest.test_case "bracket data-type hint reads the value" `Quick
        test_bracket_data_type_hint_reads_the_value;
      Alcotest.test_case "hint name is a lower-case run" `Quick
        test_hint_name_is_a_lower_case_run;
      Alcotest.test_case "mask image underscore escape" `Quick
        test_bracket_image_underscore_escape;
      Alcotest.test_case "bracket url keeps underscores" `Quick
        test_bracket_url_keeps_underscores;
      Alcotest.test_case "mask image url underscores" `Quick
        test_bracket_url_underscore;
      Alcotest.test_case "bracket url needs a whole token" `Quick
        test_bracket_url_needs_a_whole_token;
      Alcotest.test_case "typed constructors" `Quick test_typed;
      Alcotest.test_case "arbitrary mask image" `Quick test_bracket_image;
      Alcotest.test_case "arbitrary mask layer list" `Quick
        test_bracket_layer_list;
      Alcotest.test_case "invalid bracket value" `Quick
        test_invalid_bracket_value;
      Alcotest.test_case "bracket falls through to a longhand" `Quick
        test_bracket_falls_through_to_a_longhand;
      Alcotest.test_case "bracket position grammar" `Quick
        test_bracket_position_grammar;
      Alcotest.test_case "bracket size units" `Quick test_bracket_size_units;
      Alcotest.test_case "order matches Tailwind" `Slow order_matches_tailwind;
    ]

let suite = ("masks", tests)
