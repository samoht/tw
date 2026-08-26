module Css = Cascade.Css

let test_escape_in_selector () =
  let test_css = ".inset-3\\/4 { inset: 75%; }" in
  match Css.of_string test_css with
  | Ok _stylesheet -> ()
  | Error e ->
      Alcotest.failf "Failed to parse escaped selector: %s"
        (Cascade.Error.to_string e)

let unknown cls =
  match Tw.of_string cls with
  | Ok u -> Alcotest.failf "expected %s to be unknown, got %s" cls (Tw.pp u)
  | Error (`Msg _) -> ()

let round_trips cls =
  match Tw.of_string cls with
  | Ok u -> Alcotest.(check string) "round-trips" cls (Tw.pp u)
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* A class suffix is written in plain decimal. OCaml's literal grammar also
   admits [0x]/[0o]/[0b] bases, [_] digit separators and a leading [+]; reading
   a suffix as an OCaml literal would accept [p-0x4] and emit [.p-4], a rule
   nobody wrote. One class per integer reader: spacing_value, int_pos, int_any,
   int_bounded. *)
let test_int_rejects_non_decimal_spellings () =
  List.iter unknown
    [
      "p-0x4";
      "p-0X4";
      "p-0o17";
      "p-0b101";
      "p-1_0";
      "gap-1_0";
      "duration-0x4";
      "delay-1_0";
      "rotate-0x4";
      "z-0x10";
      "order-1_0";
      "columns-0x2";
      "grid-cols-0x2";
      "grid-rows-0x2";
      "basis-1_0";
      "flex-0x2";
      "w-0x4";
      "h-0x4";
      "border-0x2";
      "line-clamp-0x2";
      "opacity-0x10";
    ]

(* The sign is stripped before the suffix is read, so a negative utility takes
   the same grammar. *)
let test_negative_int_rejects_non_decimal_spellings () =
  List.iter unknown [ "-m-1_0"; "-rotate-0x4"; "-translate-x-0x4"; "-z-0x10" ]

(* [Float.of_string] reads the same non-decimal spellings plus hex-float
   exponents, which would make [p-0x1p4] mean [.p-16]. A fractional suffix needs
   digits on both sides of the point. *)
let test_decimal_rejects_non_decimal_spellings () =
  List.iter unknown
    [
      "p-0x1p4"; "p-1_5"; "p-1.2_5"; "p-1e2"; "p-+1.5"; "p-.5"; "p-1."; "m-1e1";
    ]

let test_decimal_class_suffixes_round_trip () =
  List.iter round_trips
    [
      "p-4";
      "p-0";
      "p-96";
      "p-0.5";
      "p-1.5";
      "p-2.5";
      "m-3.5";
      "gap-10";
      "opacity-25";
      "border-2";
      "z-10";
      "order-10";
      "line-clamp-2";
      "duration-150";
      "delay-100";
      "rotate-45";
      "-m-10";
      "-rotate-45";
    ]

(* A valid suffix must also be the canonical decimal spelling. Otherwise the
   parser would accept one class and emit a selector for a different class. *)
let test_redundant_zero_spellings_are_rejected () =
  List.iter unknown [ "p-04"; "p-4.0"; "p-1.50" ]

(* [Parse.split_class] remembers the split of the last class name it was given,
   because every handler in turn splits the same one. A parse must therefore not
   depend on what was parsed before it, nor on whether two equal class names are
   the same string. *)
let test_parse_is_independent_of_history () =
  let classes =
    [ "p-4"; "px-4"; "m-[calc(1rem-2px)]"; "grid-cols-2"; "bg-blue-500" ]
  in
  List.iter round_trips classes;
  List.iter round_trips (List.rev classes);
  List.iter round_trips classes;
  let built = String.concat "-" [ "grid"; "cols"; "2" ] in
  round_trips "grid-cols-3";
  round_trips built;
  unknown "grid-cols-"

let tests =
  Alcotest.
    [
      test_case "parse backslash escape in selector" `Quick
        test_escape_in_selector;
      test_case "int suffixes reject non-decimal spellings" `Quick
        test_int_rejects_non_decimal_spellings;
      test_case "negative int suffixes reject non-decimal spellings" `Quick
        test_negative_int_rejects_non_decimal_spellings;
      test_case "decimal suffixes reject non-decimal spellings" `Quick
        test_decimal_rejects_non_decimal_spellings;
      test_case "decimal class suffixes round-trip" `Quick
        test_decimal_class_suffixes_round_trip;
      test_case "redundant zero suffixes are rejected" `Quick
        test_redundant_zero_spellings_are_rejected;
      test_case "parsing is independent of parse history" `Quick
        test_parse_is_independent_of_history;
    ]

let suite = ("parse", tests)
