module Css = Cascade.Css
open Alcotest

let check_animation =
  Test_helpers.check_handler_roundtrip (module Tw.Animations.Handler)

let check_transition =
  Test_helpers.check_handler_roundtrip (module Tw.Transitions.Handler)

(* Helper to check if animation property exists with expected name or var ref *)
let has_animation_name expected_name css =
  let open Tw in
  (* Check for var(--animate-NAME) or direct NAME *)
  let var_ref = "var(--animate-" ^ expected_name ^ ")" in
  Css.fold
    (fun found stmt ->
      if found then found
      else
        match Css.as_rule stmt with
        | Some (_, decls, _) ->
            List.exists
              (fun decl ->
                let name = Css.declaration_name decl in
                let value = Css.declaration_value decl in
                name = "animation"
                && String.length value > 0
                && (value = expected_name || value = var_ref
                   || String.length value > String.length expected_name
                      && String.sub value 0 (String.length expected_name)
                         = expected_name))
              decls
        | None -> false)
    false css

(* Helper to check if transition-property exists *)
let has_transition_property css =
  let open Tw in
  Css.fold
    (fun found stmt ->
      if found then found
      else
        match Css.as_rule stmt with
        | Some (_, decls, _) ->
            List.exists
              (fun decl -> Css.declaration_name decl = "transition-property")
              decls
        | None -> false)
    false css

let test_transitions () =
  check_transition "transition-none";
  check_transition "transition-opacity";
  check_transition "transition"

let animate_utilities () =
  check_animation "animate-none";
  check_animation "animate-spin";
  check_animation "animate-bounce"

let test_duration_delay () =
  check_transition "duration-300";
  check_transition "delay-150"

let test_animation_css () =
  (* Test that animate utilities generate CSS with correct animation
     properties *)
  let open Tw in
  Alcotest.check bool "animate-spin has animation:spin" true
    (has_animation_name "spin" (to_css [ animate_spin ]));
  Alcotest.check bool "animate-bounce has animation:bounce" true
    (has_animation_name "bounce" (to_css [ animate_bounce ]));
  Alcotest.check bool "animate-pulse has animation:pulse" true
    (has_animation_name "pulse" (to_css [ animate_pulse ]))

let test_transition_css () =
  (* Test that transition utilities generate CSS with correct transition
     properties - Tailwind v4 uses individual properties, not shorthand *)
  let open Tw in
  (* transition-all uses individual properties (transition-property, etc.) *)
  Alcotest.check bool "transition-all has transition-property" true
    (has_transition_property (to_css [ transition_all ]));
  (* transition-none should use transition-property: none *)
  Alcotest.check bool "transition-none has transition-property (not transition)"
    true
    (has_transition_property (to_css [ transition_none ]))

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      animate_spin;
      animate_ping;
      animate_pulse;
      animate_bounce;
      transition_all;
      transition_none;
      duration 150;
      delay 200;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"animations suborder matches Tailwind" shuffled

(* [animate-[...]] takes an animation shorthand. A bracket the animation grammar
   cannot read is accepted and then raises out of [to_css], a pure conversion,
   so the rejection belongs at parse time. *)
let test_invalid_arbitrary_animation () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  let renders cls =
    match Tw.of_string cls with
    | Ok u -> ignore (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string)
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  rejected "animate-[#fff]";
  rejected "animate-[50%]";
  rejected "animate-[1/2]";
  rejected "animate-[calc(1px_+_2px)]";
  renders "animate-[spin_1s_linear_infinite]";
  renders "animate-[bounce_1s]"

(* The animations the [@keyframes] rules of a sheet define, sorted so the check
   reads as a set. *)
let keyframes_names css =
  Css.fold
    (fun acc stmt ->
      match Css.as_keyframes stmt with
      | Some (name, _) -> name :: acc
      | None -> acc)
    [] css
  |> List.sort String.compare

let theme_token name value =
  { Tw.Scheme.default with token_overrides = [ (name, value) ] }

let animate_keyframes ?(theme = Tw.Scheme.default) cls =
  match Tw.of_string ~theme cls with
  | Ok u -> keyframes_names (Tw.to_css ~theme ~base:false [ u ])
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* Tailwind keys the built-in [@keyframes] off the animation the value names,
   not off whether the [--animate-*] token carries its default: a project
   [@theme] redefining [--animate-ping] keeps [@keyframes ping], one pointing
   the token at another built-in pulls that one in instead, and one naming an
   animation Tailwind has no keyframes for gets none invented. *)
let test_keyframes_follow_the_animation_name () =
  let check msg expected theme =
    Alcotest.(check (list string))
      msg expected
      (animate_keyframes ~theme "animate-ping")
  in
  check "default theme" [ "ping" ] Tw.Scheme.default;
  check "redefined token still names ping" [ "ping" ]
    (theme_token "animate-ping" "ping 2s linear infinite");
  check "token pointed at another built-in" [ "spin" ]
    (theme_token "animate-ping" "spin 1s linear infinite");
  check "both animations of a list" [ "ping"; "spin" ]
    (theme_token "animate-ping"
       "ping 1s linear infinite, spin 2s linear infinite");
  check "no keyframes invented for an unknown animation" []
    (theme_token "animate-ping" "wiggle 1s ease-in-out infinite");
  check "a longer ident is not the animation" []
    (theme_token "animate-ping" "pinger 1s linear infinite")

(* A theme-declared animation and an [animate-[...]] bracket name their
   animation the same way, so they pull in the same built-in keyframes. *)
let test_keyframes_for_theme_and_bracket_names () =
  Alcotest.(check (list string))
    "theme animation naming a built-in" [ "spin" ]
    (animate_keyframes
       ~theme:(theme_token "animate-slow-spin" "spin 3s linear infinite")
       "animate-slow-spin");
  Alcotest.(check (list string))
    "bracket naming a built-in" [ "ping" ]
    (animate_keyframes "animate-[ping_2s_linear_infinite]");
  Alcotest.(check (list string))
    "bracket naming an unknown animation" []
    (animate_keyframes "animate-[wiggle_1s_ease-in-out_infinite]")

(* Tailwind orders the [animate-*] rules alphabetically by class name and a
   theme-declared animation is just another name in that order. Measured with
   the pinned CLI over an [@theme] declaring [--animate-aaa], [--animate-mmm]
   and [--animate-zzz]: aaa, bounce, mmm, none, ping, pulse, spin, zzz. The
   ordering helpers drive the CLI without a theme, so the expectation is written
   out rather than compared against a live run. *)
let test_theme_animation_sorts_by_name () =
  let theme =
    {
      Tw.Scheme.default with
      token_overrides =
        [
          ("animate-aaa", "aaa 1s linear infinite");
          ("animate-mmm", "mmm 1s linear infinite");
          ("animate-zzz", "zzz 1s linear infinite");
        ];
    }
  in
  let utility cls =
    match Tw.of_string ~theme cls with
    | Ok u -> u
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let utilities =
    List.map utility
      [
        "animate-pulse";
        "animate-zzz";
        "animate-spin";
        "animate-bounce";
        "animate-aaa";
        "animate-mmm";
        "animate-none";
        "animate-ping";
      ]
  in
  let css = Tw.to_css ~theme ~base:false utilities in
  Alcotest.(check (list string))
    "alphabetical by class name"
    [
      ".animate-aaa";
      ".animate-bounce";
      ".animate-mmm";
      ".animate-none";
      ".animate-ping";
      ".animate-pulse";
      ".animate-spin";
      ".animate-zzz";
    ]
    (Test_helpers.selectors_in_layer "utilities" css)

let tests =
  [
    test_case "transitions" `Quick test_transitions;
    test_case "animations" `Quick animate_utilities;
    test_case "duration + delay" `Quick test_duration_delay;
    test_case "animation CSS output" `Quick test_animation_css;
    test_case "transition CSS output" `Quick test_transition_css;
    test_case "animations suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "invalid arbitrary animation" `Quick
      test_invalid_arbitrary_animation;
    test_case "keyframes follow the animation name" `Quick
      test_keyframes_follow_the_animation_name;
    test_case "keyframes for theme and bracket names" `Quick
      test_keyframes_for_theme_and_bracket_names;
    test_case "theme animation sorts by name" `Quick
      test_theme_animation_sorts_by_name;
  ]

let suite = ("animations", tests)
