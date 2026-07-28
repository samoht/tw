open Alcotest

let check class_name =
  match Tw.Containers.Handler.of_class Tw.Scheme.default class_name with
  | Ok t ->
      check string "containers class" class_name
        (Tw.Containers.Handler.to_class t)
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
    match Tw.Containers.Handler.of_class Tw.Scheme.default class_name with
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
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
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

let tests =
  [
    test_case "types" `Quick test_container_types;
    test_case "variant composition" `Quick test_container_variant_composition;
    test_case "name" `Quick test_container_name;
    test_case "multiple named containers" `Quick test_multiple_named_containers;
    test_case "of_string invalid cases" `Quick test_of_string_invalid;
    test_case "containers suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
  ]

let suite = ("containers", tests)
