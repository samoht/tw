open Alcotest

let check_class = Test_helpers.check_class

let basic_tables () =
  check_class "border-collapse" Tw.Tables.border_collapse;
  check_class "border-separate" Tw.Tables.border_separate;
  check_class "border-spacing-2" (Tw.Tables.border_spacing 2);
  check_class "table-auto" Tw.Tables.table_auto;
  check_class "table-fixed" Tw.Tables.table_fixed

(* [border_spacing'] is the half-step sibling of [border_spacing]: same
   underlying value, a fractional multiplier instead of an integer one. *)
let border_spacing_prime () =
  check_class "border-spacing-0.5" (Tw.Tables.border_spacing' 0.5)

(* An arbitrary border-spacing is read with the whole CSS length grammar and
   spelled in the class name the way the author wrote it. Reading only px left
   every other unit unparsed, and the class-name printer had a placeholder
   waiting to stand in for whatever it could not spell. *)
let arbitrary_border_spacing () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits cls affix =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "border-spacing-[123px]" "--tw-border-spacing-x:123px";
  emits "border-spacing-[1rem]" "--tw-border-spacing-y:1rem";
  emits "border-spacing-x-[2em]" "--tw-border-spacing-x:2em";
  emits "border-spacing-y-[1.5vw]" "--tw-border-spacing-y:1.5vw";
  emits "border-spacing-[calc(1rem_+_2px)]"
    "--tw-border-spacing-x:calc(1rem + 2px)";
  List.iter
    (fun cls ->
      Alcotest.(check string)
        (cls ^ " round-trips") cls
        (Tw.pp (Result.get_ok (Tw.of_string cls))))
    [
      "border-spacing-[1rem]";
      "border-spacing-x-[2em]";
      "border-spacing-y-[1.5vw]";
    ]

(* A bracket that is not a length is not a border-spacing. *)
let invalid_border_spacing () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "border-spacing-[1zz]";
  rejected "border-spacing-x-[12px3]";
  rejected "border-spacing-y-[<length>]"

let tests =
  [
    test_case "basic tables" `Quick basic_tables;
    test_case "border-spacing half-step" `Quick border_spacing_prime;
    test_case "arbitrary border-spacing" `Quick arbitrary_border_spacing;
    test_case "invalid border-spacing" `Quick invalid_border_spacing;
  ]

let suite = ("tables", tests)
