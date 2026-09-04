open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Borders.Handler)

(* A theme token's value and a variant's selector are neither of them a
   declaration, so the tests that turn on one compare the sheet whole.
   [check_declarations] reads only the rules whose selector holds a [.], which
   is what leaves both out. *)
let sheet cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

let check_sheet cls expected = Alcotest.(check string) cls expected (sheet cls)

let of_string_valid () =
  check "border";
  check "border-0";
  check "border-2";
  check "border-4";
  check "border-8";
  (* v4 accepts any bare integer width, not just the fixed scale *)
  check "border-1";
  check "border-3";
  check "outline-3";
  check "outline-5";
  (* outline-offset accepts any bare integer, positive and negative *)
  check "outline-offset-6";
  check "outline-offset-3";
  check "-outline-offset-6";
  check "-outline-offset-1";

  check "border-t";
  check "border-r";
  check "border-b";
  check "border-l";
  check "border-x";
  check "border-y";

  check "border-t-2";
  check "border-r-4";
  check "border-x-4";
  check "border-y-2";
  check "border-x-0";
  check "border-y-8";

  (* A side or axis takes any integer, as the bare border does: border-x-16 was
     rejected where Tailwind emits border-inline-width: 16px. *)
  check "border-x-16";
  check "border-y-16";
  check "border-t-16";
  check "border-x-3";
  check "border-l-5";
  check "border-b-12";
  check "border-bs-12";
  check "border-e-7";

  (* logical single-side borders: inline/block start and end *)
  check "border-s";
  check "border-e";
  check "border-bs";
  check "border-be";
  check "border-s-4";
  check "border-e-0";
  check "border-bs-2";
  check "border-be-8";

  check "border-solid";
  check "border-dashed";
  check "border-dotted";
  check "border-double";
  check "border-none";

  check "rounded";
  check "rounded-none";
  check "rounded-xs";
  check "rounded-sm";
  check "rounded-md";
  check "rounded-lg";
  check "rounded-xl";
  check "rounded-2xl";
  check "rounded-3xl";
  check "rounded-4xl";
  check "rounded-full";

  check "rounded-t";
  check "rounded-r";
  check "rounded-b";
  check "rounded-l";

  check "rounded-tl";
  check "rounded-tr";
  check "rounded-br";
  check "rounded-bl";

  check "rounded-t-lg";
  check "rounded-t-xs";
  check "rounded-tl-2xl";
  check "rounded-ee-4xl";
  check "rounded-tl-xs"

let of_string_invalid () =
  (* Invalid border values *)
  let fail_maybe =
    Test_helpers.check_invalid_parts (module Tw.Borders.Handler)
  in

  fail_maybe [ "border"; "invalid" ];
  (* Invalid style *)
  fail_maybe [ "border"; "z" ];
  (* Invalid side *)
  fail_maybe [ "rounded"; "z" ];
  (* Invalid corner *)
  fail_maybe [ "unknown" ]
(* Unknown border type *)

let suborder_matches_tailwind () =
  let open Tw in
  let utilities =
    [
      border_xs;
      border_sm;
      border_md;
      border_lg;
      border_solid;
      border_dashed;
      border_dotted;
      border_double;
      border_none;
      rounded_none;
      rounded_sm;
      rounded;
      rounded_md;
      rounded_lg;
      rounded_xl;
      rounded_2xl;
      rounded_3xl;
      rounded_full;
    ]
  in
  let shuffled = Test_helpers.shuffle utilities in

  Test_helpers.check_ordering_matches
    ~test_name:"borders suborder matches Tailwind" shuffled

(* A width, a side width and a style all write border-*-width or -style, so what
   an element is actually bordered with is a rendering question. *)
let rendering_matches_tailwind () =
  let classes =
    [
      "border";
      "border-2";
      "border-4";
      "border-x-2";
      "border-y-4";
      "border-t-4";
      "border-b-2";
      "border-solid";
      "border-dashed";
      "border-dotted";
      "border-none";
      "border-current";
      "rounded";
      "rounded-md";
      "rounded-lg";
      "rounded-full";
      "rounded-t-lg";
      "rounded-br-xl";
      "outline";
      "outline-2";
      "outline-dashed";
      "outline-offset-2";
    ]
  in
  Test_helpers.check_rendering_matches ~test_name:"borders render like Tailwind"
    (List.map (fun c -> Result.get_ok (Tw.of_string c)) classes)

(* rounded-sm's default radius is .25rem in v4.3.1, not the old .125rem. *)
let test_rounded_sm_default () =
  check_sheet "rounded-sm"
    "@layer theme,components,utilities;@layer \
     theme{:root,:host{--radius-sm:.25rem}}@layer components;@layer \
     utilities{.rounded-sm{border-radius:var(--radius-sm)}}"

(* rounded-xs is a v4.3.1 addition: references var(--radius-xs) and emits the
   .125rem default token. *)
let test_rounded_xs () =
  check_sheet "rounded-xs"
    "@layer theme,components,utilities;@layer \
     theme{:root,:host{--radius-xs:.125rem}}@layer components;@layer \
     utilities{.rounded-xs{border-radius:var(--radius-xs)}}"

(* rounded-4xl is a v4.3.1 addition: references var(--radius-4xl) and emits the
   2rem default token. *)
let test_rounded_4xl () =
  check_sheet "rounded-4xl"
    "@layer theme,components,utilities;@layer \
     theme{:root,:host{--radius-4xl:2rem}}@layer components;@layer \
     utilities{.rounded-4xl{border-radius:var(--radius-4xl)}}"

(* Per-side/corner full radius inlines the infinite value (matching the
   all-corners variant and Tailwind's calc(infinity*1px)), not a --radius-full
   token that defaulted to the wrong 9999px. The empty theme layer is what says
   no such token is emitted, which no assertion on the declarations alone
   reaches. *)
let test_rounded_side_full_inlined () =
  check_sheet "rounded-l-full"
    "@layer theme,components,utilities;@layer theme;@layer components;@layer \
     utilities{.rounded-l-full{border-top-left-radius:3.40282e38px;border-bottom-left-radius:3.40282e38px}}"

(* Numeric outline widths (outline-1/2/4/8) emit outline-width: Npx with the
   outline-style var; they used to be unknown classes. *)
let test_outline_widths () =
  check_declarations "outline-1"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:1px" ];
  check_declarations "outline-2"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:2px" ];
  (* v4 takes any bare integer, not just the fixed scale *)
  check_declarations "outline-3"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:3px" ]

(* Arbitrary outline-offset lengths (outline-offset-[3px]) emit the length,
   alongside the var() form (outline-offset-[var(--x)]). *)
let test_outline_offset_arbitrary () =
  check_declarations "outline-offset-[3px]" [ "outline-offset:3px" ]

(* outline-hidden's forced-colors reset is its own @media block; under a state
   modifier (focus, focus-within) the block must use the modified selector, not
   the bare .outline-hidden, and stay grouped with the regular rule. It used to
   keep the bare selector and reorder before the regular rule. *)
let test_outline_hidden_modifier_forced_colors () =
  check_sheet "focus:outline-hidden"
    "@layer theme,components,utilities;@layer theme;@layer components;@layer \
     utilities{.focus\\:outline-hidden:focus{--tw-outline-style:none;outline-style:none}@media(forced-colors:active){.focus\\:outline-hidden:focus{outline-offset:2px;outline:2px \
     solid #0000}}}"

(* The outline family sorts as one block: outline-hidden, the widths, the
   offsets, the colors, then the styles. Order matters beyond byte parity here:
   outline-hidden's forced-colors reset writes the outline shorthand, so a color
   rule ahead of it loses its outline-color. *)
let test_outline_ordering () =
  let parse cls =
    match Tw.of_string cls with
    | Ok u -> u
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let utilities =
    List.map parse
      [
        "outline";
        "outline-2";
        "outline-[3px]";
        "outline-hidden";
        "outline-offset-2";
        "-outline-offset-4";
        "outline-red-500";
        "outline-blue-500";
        "outline-current";
        "outline-transparent";
        "outline-dashed";
        "outline-solid";
        "outline-none";
      ]
  in
  Test_helpers.check_ordering_matches ~test_name:"outline family order"
    (Test_helpers.shuffle utilities)

(* Arbitrary per-side border widths (border-t-[1px], ...) emit the side width
   plus the side border-style var; they used to be unknown classes. *)
let test_border_side_arbitrary_width () =
  check_declarations "border-t-[1px]"
    [ "border-top-style:var(--tw-border-style)"; "border-top-width:1px" ];
  check_declarations "border-l-[0.5rem]"
    [ "border-left-style:var(--tw-border-style)"; "border-left-width:.5rem" ]

(* Logical single-side borders emit the inline/block start/end style var and
   width, like the physical per-side borders do. *)
let test_logical_side_borders () =
  check_declarations "border-s"
    [
      "border-inline-start-style:var(--tw-border-style)";
      "border-inline-start-width:1px";
    ];
  check_declarations "border-be-2"
    [
      "border-block-end-style:var(--tw-border-style)";
      "border-block-end-width:2px";
    ];
  check_declarations "border-e"
    [
      "border-inline-end-style:var(--tw-border-style)";
      "border-inline-end-width:1px";
    ]

(* Arbitrary outline and border widths take any CSS length unit, not just the
   px/rem/em/% the hand-rolled suffix parsers knew: Tailwind emits
   outline-width: 3rem for outline-[3rem] and border-width: 3vw for
   border-[3vw]. *)
let test_bracket_width_units () =
  let outline cls width =
    check_declarations cls
      [ "outline-style:var(--tw-outline-style)"; "outline-width:" ^ width ]
  in
  outline "outline-[3rem]" "3rem";
  outline "outline-[2em]" "2em";
  outline "outline-[3px]" "3px";
  outline "outline-[50%]" "50%";
  check_declarations "border-[3vw]"
    [ "border-style:var(--tw-border-style)"; "border-width:3vw" ];
  check_declarations "border-[2ch]"
    [ "border-style:var(--tw-border-style)"; "border-width:2ch" ];
  check_declarations "border-[0.5rem]"
    [ "border-style:var(--tw-border-style)"; "border-width:.5rem" ];
  check_declarations "border-t-[3vw]"
    [ "border-top-style:var(--tw-border-style)"; "border-top-width:3vw" ]

(* A math function in a width bracket stands for the width it computes, so it is
   a width and not a colour. The bracket was classified by its first character,
   which put [calc(...)] on the colour side and refused the class Tailwind
   renders. *)
let test_bracket_math_function_width () =
  check_declarations "border-[calc(1rem_+_2px)]"
    [ "border-style:var(--tw-border-style)"; "border-width:calc(1rem + 2px)" ];
  check_declarations "border-t-[calc(1rem_+_2px)]"
    [
      "border-top-style:var(--tw-border-style)";
      "border-top-width:calc(1rem + 2px)";
    ];
  check_declarations "border-[min(2px,1rem)]"
    [ "border-style:var(--tw-border-style)"; "border-width:min(2px,1rem)" ];
  check_declarations "border-[clamp(1px,2vw,3rem)]"
    [
      "border-style:var(--tw-border-style)"; "border-width:clamp(1px,2vw,3rem)";
    ];
  check_declarations "outline-[calc(1rem_+_2px)]"
    [
      "outline-style:var(--tw-outline-style)"; "outline-width:calc(1rem + 2px)";
    ];
  (* a bare var() is still a colour on both, so it writes the colour alone and
     none of the width's style var *)
  check_declarations "border-[var(--w)]" [ "border-color:var(--w)" ];
  check_declarations "outline-[var(--w)]" [ "outline-color:var(--w)" ]

(* The three CSS line-width keywords are border widths in their own right, so a
   bracket naming one is a width and not an unknown class: Tailwind emits
   border-width: thin for border-[thin], and the same for medium and thick on
   every side. *)
let test_bracket_line_width_keywords () =
  let all_sides cls keyword =
    check_declarations cls
      [ "border-style:var(--tw-border-style)"; "border-width:" ^ keyword ]
  in
  all_sides "border-[thin]" "thin";
  all_sides "border-[medium]" "medium";
  all_sides "border-[thick]" "thick";
  check_declarations "border-t-[thin]"
    [ "border-top-style:var(--tw-border-style)"; "border-top-width:thin" ];
  check_declarations "border-l-[thick]"
    [ "border-left-style:var(--tw-border-style)"; "border-left-width:thick" ]

(* A [--radius-*] token the project declared in its [@theme] names a corner
   radius the built-in scale has no slot for. Tailwind generates the utility
   from it, on the all-corners form and on every side and corner; tw rejected
   the class outright, so the project token emitted nothing. *)
let test_project_radius_token () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("radius-blob", "3rem") ]
  in
  let css cls =
    match Tw.of_string ~theme cls with
    | Ok u -> Tw.to_css ~theme ~base:false [ u ] |> Tw.Css.to_string
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  (* [check_declarations] and [check_sheet] both compile the class against the
     default theme, which has no [--radius-blob], so these stay on a substring
     of the sheet this theme renders. *)
  let emits affix cls =
    Alcotest.(check bool) cls true (Astring.String.is_infix ~affix (css cls))
  in
  emits "border-radius: var(--radius-blob)" "rounded-blob";
  emits "border-top-left-radius: var(--radius-blob)" "rounded-t-blob";
  emits "border-top-right-radius: var(--radius-blob)" "rounded-t-blob";
  emits "border-top-left-radius: var(--radius-blob)" "rounded-tl-blob";
  (* A name the project did not declare stays an unknown class. *)
  Alcotest.(check bool)
    "an undeclared radius name is rejected" true
    (Result.is_error (Tw.of_string ~theme "rounded-nope"))

(* Every [border-<side>-<n>] writes the same [border-top-width], so their
   relative order decides which one wins. The four nullary widths (0, 2, 4, 8)
   were sorted on an ordinal that packed them into four slots and left the
   bracket in the next one, so any other width sorted past the bracket:
   [border-t-6] landed after [border-t-\[3px\]] where Tailwind puts it between 4
   and 8. *)
let test_side_width_order () =
  Test_helpers.check_class_order ~test_name:"border side widths"
    [
      "border-t";
      "border-t-0";
      "border-t-2";
      "border-t-4";
      "border-t-6";
      "border-t-8";
      "border-t-40";
      "border-t-[3px]";
    ]

(* The axis and logical sides take an arbitrary width the same way the physical
   sides do. The bracket arm matched the four physical sides only, so an axis or
   logical side fell through to the colour path and failed there. *)
let test_axis_arbitrary_width () =
  let side cls family width =
    check_declarations cls
      [
        "border-" ^ family ^ "-style:var(--tw-border-style)";
        "border-" ^ family ^ "-width:" ^ width;
      ]
  in
  side "border-x-[3px]" "inline" "3px";
  side "border-y-[3px]" "block" "3px";
  side "border-s-[3px]" "inline-start" "3px";
  side "border-e-[3px]" "inline-end" "3px";
  side "border-bs-[3px]" "block-start" "3px";
  side "border-be-[3px]" "block-end" "3px";
  side "border-x-[0.5rem]" "inline" ".5rem";
  (* a bracket that is not a length is still not a width *)
  Alcotest.(check bool)
    "border-x-[1e] is rejected" true
    (Result.is_error (Tw.of_string "border-x-[1e]"))

(* Every axis and logical side writes a width some other side writes too, so
   their relative order decides which one wins. The families were packed into
   bands ten wide, so [border-x-25] landed on [border-s-5]'s slot and the two
   sorted by class name instead. *)
let test_axis_width_order () =
  Test_helpers.check_class_order ~test_name:"border axis widths"
    [
      "border-x";
      "border-x-25";
      "border-y";
      "border-y-25";
      "border-s";
      "border-s-5";
      "border-e";
      "border-e-5";
      "border-bs";
      "border-bs-5";
      "border-be";
      "border-be-5";
      "border-t-6";
      "border-l-2";
    ]

(* The all-sides widths sort by the number they name, and the bracket sorts
   after every one of them. The four the scale names (0, 2, 4, 8) took the first
   four slots of the band and the rest counted from the same base, so [border-3]
   landed on [border-8]'s slot and [border-40] sorted past the bracket. *)
let test_all_sides_width_order () =
  Test_helpers.check_class_order ~test_name:"border widths"
    [
      "border";
      "border-0";
      "border-2";
      "border-3";
      "border-4";
      "border-5";
      "border-8";
      "border-40";
      "border-[3px]";
    ]

(* An axis or logical bracket width sorts inside its own side's band, after the
   numeric widths, the way the physical sides already do. *)
let test_axis_arbitrary_width_order () =
  Test_helpers.check_class_order ~test_name:"border axis bracket widths"
    [
      "border-x-2";
      "border-x-[3px]";
      "border-y-2";
      "border-y-[3px]";
      "border-s-2";
      "border-s-[3px]";
      "border-e-2";
      "border-e-[3px]";
      "border-bs-2";
      "border-bs-[3px]";
      "border-be-2";
      "border-be-[3px]";
      "border-t-[3px]";
    ]

(* A bracket whose content is not a length is not an outline or border width:
   the parser rejects it, rather than accepting it and raising from the length
   conversion once the sheet is rendered. *)
let test_invalid_bracket_widths () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "outline-[.]";
  rejected "outline-[1e]";
  rejected "outline-[-]";
  rejected "border-[.]";
  rejected "border-[abc]";
  rejected "border-t-[1e]"

(* A data-type hint says how to read the value written after it; it does not
   make that value the name of a custom property. [outline-[length:3px]] wrote
   [outline-width: var(--3px)] where Tailwind writes [outline-width: 3px]. *)
let test_bracket_data_type_hint_reads_the_value () =
  check_declarations "outline-[length:3px]"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:3px" ];
  check_declarations "outline-[percentage:10%]"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:10%" ];
  (* a var() reference after the hint still names a custom property *)
  check_declarations "outline-[length:var(--my-width)]"
    [ "outline-style:var(--tw-outline-style)"; "outline-width:var(--my-width)" ];
  (* the class prints back with the hint the author wrote *)
  Alcotest.(check string)
    "outline-[length:3px] round-trips" "outline-[length:3px]"
    (Tw.pp (Result.get_ok (Tw.of_string "outline-[length:3px]")));
  (* A value the width reader refuses is held open, not settled: Tailwind writes
     the bracket out whatever it says, so refusing is an intermediate. *)
  check_invalid_input
    ~why:(Diverges "emitted verbatim; tw needs an opaque declaration to match")
    (module Tw.Borders.Handler)
    "outline-[length:notawidth]"

let tests =
  [
    test_case "bracket data-type hint reads the value" `Quick
      test_bracket_data_type_hint_reads_the_value;
    test_case "bracket width units" `Quick test_bracket_width_units;
    test_case "invalid bracket widths" `Quick test_invalid_bracket_widths;
    test_case "bracket line-width keywords" `Quick
      test_bracket_line_width_keywords;
    test_case "bracket math-function width" `Quick
      test_bracket_math_function_width;
    test_case "project radius token" `Quick test_project_radius_token;
    test_case "border side width order" `Slow test_side_width_order;
    test_case "border axis arbitrary widths" `Quick test_axis_arbitrary_width;
    test_case "border all-sides width order" `Slow test_all_sides_width_order;
    test_case "border axis width order" `Slow test_axis_width_order;
    test_case "border axis bracket width order" `Slow
      test_axis_arbitrary_width_order;
    test_case "rounded-sm default radius" `Quick test_rounded_sm_default;
    test_case "rounded-xs default radius" `Quick test_rounded_xs;
    test_case "rounded-4xl default radius" `Quick test_rounded_4xl;
    test_case "rounded-l-full inlined radius" `Quick
      test_rounded_side_full_inlined;
    test_case "outline numeric widths" `Quick test_outline_widths;
    test_case "outline-offset arbitrary length" `Quick
      test_outline_offset_arbitrary;
    test_case "outline-hidden modifier forced-colors" `Quick
      test_outline_hidden_modifier_forced_colors;
    test_case "outline family order matches Tailwind" `Quick
      test_outline_ordering;
    test_case "border side arbitrary widths" `Quick
      test_border_side_arbitrary_width;
    test_case "logical single-side borders" `Quick test_logical_side_borders;
    test_case "borders of_string - valid values" `Quick of_string_valid;
    test_case "borders of_string - invalid values" `Quick of_string_invalid;
    test_case "borders suborder matches Tailwind" `Quick
      suborder_matches_tailwind;
    test_case "borders render like Tailwind" `Slow rendering_matches_tailwind;
  ]

let suite = ("borders", tests)
