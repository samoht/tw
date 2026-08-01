open Alcotest

let check class_name =
  match Tw.Private.Containers.Handler.of_class Tw.Theme.default class_name with
  | Ok t ->
      check string "containers class" class_name
        (Tw.Private.Containers.Handler.to_class t)
  | Error (`Msg msg) -> fail msg

let test_container_types () =
  check "@container";
  check "@container-normal";
  check "@container-size"

let test_container_name () =
  check "@container/sidebar";
  check "@container/header";
  check "@container/main"

let test_multiple_named_containers () =
  (* Test multiple named containers together - should match Tailwind output *)
  Test_helpers.check_ordering_matches ~test_name:"multiple named containers"
    Tw.
      [
        at_container_named "sidebar";
        at_container_named "header";
        at_container_named "main";
      ]

let test_of_string_invalid () =
  (* Invalid container utilities *)
  let test_invalid input =
    let class_name = String.concat "-" input in
    match
      Tw.Private.Containers.Handler.of_class Tw.Theme.default class_name
    with
    | Ok _ -> fail ("Expected error for: " ^ class_name)
    | Error _ -> ()
  in

  (* Invalid type values - these don't match the patterns *)
  test_invalid [ "container"; "type"; "invalid" ];
  (* Invalid type value *)
  test_invalid [ "container"; "type"; "block" ];
  (* Invalid type *)
  test_invalid [ "container"; "type"; "inline" ];

  (* Incomplete inline-size *)

  (* Invalid container formats *)
  test_invalid [ "container"; "type"; "size"; "extra" ];
  (* Extra tokens *)
  test_invalid [ "not"; "container" ];
  (* Wrong prefix *)
  test_invalid [];
  (* Empty input *)
  test_invalid [ "type"; "size" ]
(* Missing container prefix *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      container;
      at_container;
      at_container_normal;
      at_container_named "sidebar";
      at_container_named "header";
      at_container_named "main";
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in
  Test_helpers.check_ordering_matches
    ~test_name:"containers suborder matches Tailwind" shuffled

(* A container-query variant keeps the raw bracket token in its class name, and
   an outer variant wraps it rather than replacing it: the composition used to
   fall through, so [sm:@max-md:X] lost its breakpoint entirely. *)
let test_container_variant_composition () =
  let css cls =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ]
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  (* [theme()] resolves in the condition but not in the class name *)
  has "@min-[theme(--breakpoint-lg)]:hidden"
    ".\\@min-\\[theme\\(--breakpoint-lg\\)\\]\\:hidden";
  has "@min-[theme(--breakpoint-lg)]:hidden" "@container(width>=64rem)";
  (* an outer breakpoint stays outside the container query *)
  has "sm:@max-[40rem]:inline-block"
    "@media(min-width:40rem){@container not \
     (width>=40rem){.sm\\:\\@max-\\[40rem\\]\\:inline-block";
  (* two container queries nest *)
  has "@sm:@max-md:flex-col"
    "@container(width>=24rem){@container not (width>=28rem)"

(* [@sm/main] aims the size query at the container named [main] rather than the
   nearest one. *)
let test_scoped_container_variant () =
  let css cls =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ]
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "@sm/main:flex-col" "@container main (width>=24rem)";
  has "@sm/main:flex-col" ".\\@sm\\/main\\:flex-col";
  has "@max-sm/main:hidden" "@container main not (width>=24rem)";
  has "@min-[400px]/sidebar:flex" "@container sidebar (width>=400px)"

(* Tailwind spells the theme lookup both [theme(--x)] and [--theme(--x)]. *)
let test_theme_fn_container_query () =
  let css cls =
    match Tw.of_string cls with
    | Ok u ->
        Tw.to_css ~config:(Tw.Config.v ~base:false ()) [ u ]
        |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let has cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  has "@min-[--theme(--breakpoint-sm)]:flex" "@container(width>=40rem)";
  has "@min-[theme(--breakpoint-sm)]:flex" "@container(width>=40rem)"

let test_variant_width_order () =
  (* Tailwind orders container variants like breakpoints: the @max-* negated
     lower bounds first and largest first, then the plain ones ascending, with a
     named container interleaved at its own width. The variant order alone gave
     every @-prefixed variant the same key, so they fell through to an
     alphabetical comparison of the class prefix. *)
  let classes =
    [
      "@md:flex";
      "@sm:flex";
      "@lg:flex";
      "@xs:flex";
      "@max-sm:flex";
      "@max-lg:flex";
      "@sm/main:flex";
    ]
  in
  let utilities = List.map (fun c -> Result.get_ok (Tw.of_string c)) classes in
  let css =
    Cascade.Css.to_string ~minify:true
      (Tw.to_css ~config:(Tw.Config.v ~base:false ()) utilities)
  in
  let expected =
    [
      "@container not (width>=32rem)";
      "@container not (width>=24rem)";
      "@container(width>=20rem)";
      "@container main (width>=24rem)";
      "@container(width>=24rem)";
      "@container(width>=28rem)";
      "@container(width>=32rem)";
    ]
  in
  let positions =
    List.map
      (fun needle ->
        let n = String.length needle and h = String.length css in
        let rec go i =
          if i + n > h then -1
          else if String.sub css i n = needle then i
          else go (i + 1)
        in
        go 0)
      expected
  in
  Alcotest.check Alcotest.bool "every container condition is emitted" true
    (List.for_all (fun p -> p >= 0) positions);
  Alcotest.check
    (Alcotest.list Alcotest.int)
    "container variants are emitted widest-negated first, then ascending"
    (List.sort Int.compare positions)
    positions

let tests =
  [
    test_case "types" `Quick test_container_types;
    test_case "variant width order" `Quick test_variant_width_order;
    test_case "variant composition" `Quick test_container_variant_composition;
    test_case "name" `Quick test_container_name;
    test_case "multiple named containers" `Quick test_multiple_named_containers;
    test_case "scoped container variant" `Quick test_scoped_container_variant;
    test_case "theme() in a container query" `Quick
      test_theme_fn_container_query;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "containers suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("containers", tests)
