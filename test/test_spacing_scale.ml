module Css = Cascade.Css
module Spacing_scale = Tw.Private.Spacing_scale

let length l = Css.Pp.to_string Css.pp_length l

(* Without an explicit step in the theme, a spacing value is computed from the
   single [--spacing] variable. *)
let test_computed_from_the_step_variable () =
  let _, len = Spacing_scale.spacing_calc 4 in
  Alcotest.(check string) "four steps" "calc(var(--spacing) * 4)" (length len)

(* One step is emitted bare rather than as [calc(var(--spacing) * 1)]. *)
let test_one_step_is_bare () =
  let _, len = Spacing_scale.spacing_calc 1 in
  Alcotest.(check string) "one step" "var(--spacing)" (length len)

let test_negative_is_a_negative_multiplier () =
  let _, len = Spacing_scale.spacing_calc (-4) in
  Alcotest.(check string) "minus four" "calc(var(--spacing) * -4)" (length len)

(* A theme that names the step reads its own variable instead. *)
let test_explicit_step_reads_its_own_variable () =
  let theme = Tw.Theme.with_spacing [ (4, Css.Rem 1.) ] Tw.Theme.default in
  let _, len = Spacing_scale.spacing_calc ~theme 4 in
  Alcotest.(check string) "explicit" "var(--spacing-4)" (length len);
  let _, neg = Spacing_scale.spacing_calc ~theme (-4) in
  Alcotest.(check string)
    "explicit negative" "calc(var(--spacing-4) * -1)" (length neg)

let test_half_steps () =
  let _, len = Spacing_scale.spacing_calc_float 2.5 in
  Alcotest.(check string)
    "two and a half steps" "calc(var(--spacing) * 2.5)" (length len)

let test_spacing_times () =
  Alcotest.(check (option string))
    "four steps resolved" (Some "1rem")
    (Spacing_scale.spacing_times 4.);
  Alcotest.(check (option string))
    "half a step" (Some ".125rem")
    (Spacing_scale.spacing_times 0.5)

let test_base_is_a_quarter_rem () =
  Alcotest.(check string) "step" ".25rem" (length Spacing_scale.spacing_base)

let tests =
  Alcotest.
    [
      test_case "computed from the step variable" `Quick
        test_computed_from_the_step_variable;
      test_case "one step is bare" `Quick test_one_step_is_bare;
      test_case "negative is a negative multiplier" `Quick
        test_negative_is_a_negative_multiplier;
      test_case "explicit step reads its own variable" `Quick
        test_explicit_step_reads_its_own_variable;
      test_case "half steps" `Quick test_half_steps;
      test_case "spacing_times" `Quick test_spacing_times;
      test_case "base is a quarter rem" `Quick test_base_is_a_quarter_rem;
    ]

let suite = ("spacing_scale", tests)
