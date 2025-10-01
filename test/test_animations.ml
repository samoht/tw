open Alcotest

let check parts =
  let expected = String.concat "-" parts in
  match Tw.Animations.of_string parts with
  | Ok t ->
      check string "animations class" expected
        (Tw.Style.pp (Tw.Animations.to_style t))
  | Error (`Msg msg) -> fail msg

let test_transitions () =
  check [ "transition"; "none" ];
  check [ "transition"; "opacity" ];
  check [ "transition" ]

let animate_utilities () =
  check [ "animate"; "none" ];
  check [ "animate"; "spin" ];
  check [ "animate"; "bounce" ]

let test_duration_delay () =
  check [ "duration"; "300" ];
  check [ "delay"; "150" ]

let test_animation_css () =
  (* Test that animate utilities generate CSS with correct animation
     properties *)
  let open Tw in
  (* Helper to check if animation property exists with expected name *)
  let has_animation_name expected_name css =
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
                  && (value = expected_name
                     || String.length value > String.length expected_name
                        && String.sub value 0 (String.length expected_name)
                           = expected_name))
                decls
          | None -> false)
      false css
  in

  Alcotest.check bool "animate-spin has animation:spin" true
    (has_animation_name "spin" (to_css [ animate_spin ]));
  Alcotest.check bool "animate-bounce has animation:bounce" true
    (has_animation_name "bounce" (to_css [ animate_bounce ]));
  Alcotest.check bool "animate-pulse has animation:pulse" true
    (has_animation_name "pulse" (to_css [ animate_pulse ]))

let test_transition_css () =
  (* Test that transition utilities generate CSS with correct transition
     properties *)
  let open Tw in
  (* Helper to check if transition property exists *)
  let has_transition css =
    Css.fold
      (fun found stmt ->
        if found then found
        else
          match Css.as_rule stmt with
          | Some (_, decls, _) ->
              List.exists
                (fun decl -> Css.declaration_name decl = "transition")
                decls
          | None -> false)
      false css
  in

  Alcotest.check bool "transition-all has transition property" true
    (has_transition (to_css [ transition_all ]));
  Alcotest.check bool "transition-none has transition property" true
    (has_transition (to_css [ transition_none ]))

let tests =
  [
    test_case "transitions" `Quick test_transitions;
    test_case "animations" `Quick animate_utilities;
    test_case "duration + delay" `Quick test_duration_delay;
    test_case "animation CSS output" `Quick test_animation_css;
    test_case "transition CSS output" `Quick test_transition_css;
  ]

let suite = ("animations", tests)
