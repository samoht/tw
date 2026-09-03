open Alcotest
open Tw.Arbitrary.Handler

let check input =
  match of_class Tw.Scheme.default input with
  | Ok result ->
      Alcotest.check string "arbitrary class name" input (to_class result)
  | Error (`Msg msg) -> fail msg

let of_string_valid () =
  check "[color:red]/50";
  check "[background-color:blue]/[50%]";
  check "[border-color:#ff0000]/25";
  (* var-valued colours and custom properties with /opacity round-trip *)
  check "[color:var(--my-color)]/50";
  check "[color:rgb(255_0_0)]/50";
  check "[border-color:oklch(0.5_0.2_250)]/[var(--x)]";
  check "[--x:#ff0000]/50";
  check "[--gradient-bg:var(--color-black)]/15";
  (* Plain custom-property declarations and non-colour standard properties parse
     via the cascade declaration parser. *)
  check "[--foo:bar]";
  check "[mask-type:luminance]";
  check "[display:flex]";
  check "[color:red]"

let rejected cls =
  match of_class Tw.Scheme.default cls with
  | Ok result ->
      failf "expected %s to be rejected, got %s" cls (to_class result)
  | Error _ -> ()

let of_string_invalid () =
  rejected "";
  rejected "color:red";
  rejected "[invalid]";
  rejected "[]"

(* What follows the closing bracket is part of the class name, so a suffix that
   is not a [/opacity] modifier names a class Tailwind does not recognise. *)
let test_trailing_text () =
  rejected "[color:red]xyz";
  rejected "[display:flex]junk";
  rejected "[color:red]/";
  rejected "[color:red]/bogus";
  rejected "[color:red]/-5";
  rejected "[color:red]/50/50"

let css cls =
  match Tw.of_string cls with
  | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string
  | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m

(* An arbitrary [property:value] normalises omitted whitespace around calc
   operators, like the utility form: [margin:calc(100%-10px)] emits margin:
   calc(100% - 10px). *)
let test_property_calc_operators () =
  Alcotest.(check bool)
    "[margin:calc(100%-10px)] spaces the operator" true
    (Astring.String.is_infix ~affix:"margin: calc(100% - 10px)"
       (css "[margin:calc(100%-10px)]"));
  Alcotest.(check bool)
    "[width:calc(var(--a)-var(--b))] spaces the operator" true
    (Astring.String.is_infix ~affix:"width: calc(var(--a) - var(--b))"
       (css "[width:calc(var(--a)-var(--b))]"))

(* theme() dot-notation inside an arbitrary value resolves statically:
   theme(colors.red.500) to the red-500 oklch, the opacity form to that colour
   mixed with the alpha, and the class name round-trips via its alias. *)
let test_theme_dot_notation () =
  let g =
    "bg-[image:linear-gradient(to_right,theme(colors.red.500)_75%,theme(colors.red.500/25%))]"
  in
  Alcotest.(check bool)
    "theme(colors.red.500) resolves to the red-500 oklch" true
    (Astring.String.is_infix ~affix:"oklch(63.7%" (css g));
  Alcotest.(check bool)
    "theme(colors.red.500/25%) mixes the alpha in" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, oklch(63.7% .237 25.331) 25%, transparent)"
       (css g));
  Alcotest.(check string)
    "theme() class round-trips" g
    (Tw.pp (Result.get_ok (Tw.of_string g)))

(* A var-valued colour with /opacity used to raise invalid_arg; it now emits an
   oklab color-mix under @supports, with a fallback, type-safely. *)
let test_var_color_opacity () =
  let out = css "[color:var(--my-color)]/50" in
  Alcotest.(check bool)
    "oklab color-mix on the var" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, var(--my-color) 50%, transparent)" out)

(* Arbitrary-value underscores encode spaces before the colour is parsed. The
   plain declaration path already decoded them; the /opacity path did not. *)
let test_encoded_space_color_opacity () =
  Alcotest.(check bool)
    "rgb underscores are decoded" true
    (Astring.String.is_infix ~affix:"rgb(255 0 0)"
       (css "[color:rgb(255_0_0)]/50"));
  Alcotest.(check bool)
    "oklch underscores are decoded" true
    (Astring.String.is_infix ~affix:"oklch(.5 .2 250)"
       (css "[border-color:oklch(0.5_0.2_250)]/[var(--x)]"))

(* A custom property with /opacity sets the property to a color-mix via the
   typed [Css.var] form (no token stream). *)
let test_custom_prop_opacity () =
  let out = css "[--x:#ff0000]/50" in
  Alcotest.(check bool)
    "custom property gets a color-mix value" true
    (Astring.String.is_infix
       ~affix:"--x: color-mix(in oklab, #ff0000 50%, transparent)" out)

(* A class either compiles or is refused; an exception escaping [of_string] or
   [to_css] is neither. Swallowing [Error] here is the point - Tailwind refuses
   many of these too - but a raise is a failure, which is what
   {!Test_helpers.sweep_one} enforces. *)
let test_no_crash () =
  List.iter
    (fun cls -> ignore (Test_helpers.sweep_one cls))
    [
      "[--gradient-bg:var(--color-black)]/15";
      "[color:var(--color-red-500)]/40";
      "[--foo:bar]";
      "[mask-type:luminance]";
    ]

(* A class tw accepts must name a rule the class itself selects. Getting that
   wrong is silent: the sheet grows a rule no markup can ever match, and every
   oracle that compares declarations reads the two sheets as equal.

   Two shipped bugs had exactly this shape - a class name re-printed through a
   CSS printer that drops a leading zero, and one rebuilt from a parsed length
   that had stopped at a comment - so the sweep runs the payloads that produce
   it across every family taking a bracket value, every arbitrary property, and
   every variant that brackets a breakpoint or a selector.

   Refusal is a legitimate answer and is not counted. The one thing asserted is
   that nothing is accepted under a name it cannot be selected by. *)
let known_selector_gaps =
  (* [has-[<name>]] where <name> is one of the has- shorthands collapses onto
     the shorthand variant: [Style.Has], [Style.Group_has] and [Style.Peer_has]
     each carry the selector text with no record of whether the author bracketed
     it, so [has-[hover]] and [has-hover] are one value and the bracket is gone
     when the class is spelled back. Tailwind reads the bracket as a type
     selector instead ([:has(:is(hover))]), so closing this splits the
     constructor rather than fixing a printer. *)
  [ "has-[hover]:flex"; "group-has-[hover]:flex"; "peer-has-[hover]:flex" ]

let sweep_classes =
  List.concat
    [
      (* <family>-[<payload>] *)
      List.concat_map
        (fun f ->
          List.map
            (fun p -> String.concat "" [ f; "-["; p; "]" ])
            Test_helpers.adversarial_payloads)
        Test_helpers.arbitrary_families;
      (* the arbitrary-property form, on both sides of the colon *)
      List.concat_map
        (fun p ->
          [
            String.concat "" [ "[color:"; p; "]" ];
            String.concat "" [ "[--x:"; p; "]" ];
            String.concat "" [ "["; p; ":red]" ];
          ])
        Test_helpers.adversarial_payloads;
      (* variants that bracket a breakpoint, a condition or a selector *)
      List.concat_map
        (fun v ->
          List.map
            (fun p -> String.concat "" [ v; "-["; p; "]:flex" ])
            Test_helpers.adversarial_payloads)
        [
          "min";
          "max";
          "supports";
          "data";
          "aria";
          "has";
          "group-has";
          "peer-has";
          "not";
          "in";
          "nth";
          "nth-last";
          "group";
          "peer";
          "group-data";
          "peer-data";
        ];
    ]

let test_adversarial_value_sweep () =
  let classes = sweep_classes in
  let mismatches =
    List.filter_map
      (fun cls ->
        match Test_helpers.sweep_one cls with
        | Test_helpers.Mismatched why -> Some (cls, why)
        | Rejected | Emitted_nothing | Matched -> None)
      classes
  in
  let names = List.map fst mismatches in
  let unexpected =
    List.filter
      (fun (cls, _) -> not (List.mem cls known_selector_gaps))
      mismatches
  in
  (match unexpected with
  | [] -> ()
  | l ->
      Alcotest.failf "%d classes emit a rule they cannot select:\n%s"
        (List.length l)
        (String.concat "\n"
           (List.map (fun (c, w) -> String.concat "" [ "  "; c; ": "; w ]) l)));
  (* The gap list is exact, so it fails when it grows and again when it is
     closed, rather than quietly covering more each release. *)
  Alcotest.(check (slist string String.compare))
    "the known gaps are exactly the ones still open" known_selector_gaps names

(* Tailwind's [--spacing(N)] shorthand reads the spacing scale, so it has to be
   expanded here too: the value used to reach the sheet verbatim. *)
let test_property_spacing_fn () =
  Alcotest.(check bool)
    "[--gap:--spacing(10)] reads the spacing scale" true
    (Astring.String.is_infix ~affix:"--gap: calc(var(--spacing) * 10)"
       (css "[--gap:--spacing(10)]"))

(* Tailwind's [--alpha(C/P)] is the [/opacity] form written as a function, and a
   reference to a palette token renders from the palette, so the fallback is a
   colour rather than the bare reference. *)
let test_alpha_fn () =
  let out = css "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]" in
  Alcotest.(check bool)
    "fallback resolves the palette colour" true
    (Astring.String.is_infix ~affix:"--checkered-bg: #0307121a" out);
  Alcotest.(check bool)
    "@supports keeps the token reference" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, var(--color-gray-950) 10%, transparent)" out);
  Alcotest.(check string)
    "the --alpha() spelling round-trips"
    "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]"
    (Tw.pp
       (Result.get_ok
          (Tw.of_string "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]")))

(* The [/] modifier applies to the colour the value denotes, so a value written
   with [--alpha()] mixes twice, and both spellings survive the round-trip. *)
let test_alpha_fn_with_modifier () =
  check "[color:--alpha(red/50%)]/25";
  Alcotest.(check bool)
    "the modifier mixes the alpha'd colour" true
    (Astring.String.is_infix
       ~affix:
         "color-mix(in oklab, color-mix(in oklab, red 50%, transparent) 25%, \
          transparent)"
       (css "[color:--alpha(red/50%)]/25"))

(* An opacity read from a custom property keeps the spelling it was written
   with, in either the bracket or the parenthesised form. *)
let test_var_opacity_spelling () =
  check "[color:red]/[var(--x)]";
  check "[color:red]/(--x)";
  Alcotest.(check bool)
    "the var supplies the mix percentage" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, red var(--x), transparent)"
       (css "[color:red]/(--x)"))

(* Colour values go through the CSS reader, so every named colour is a colour,
   not a hand-picked subset of them. *)
let test_named_colour_value () =
  check "[color:rebeccapurple]/50";
  Alcotest.(check bool)
    "rebeccapurple mixes like any other named colour" true
    (Astring.String.is_infix
       ~affix:"color-mix(in oklab, rebeccapurple 50%, transparent)"
       (css "[color:rebeccapurple]/50"));
  (* A value that names no colour has nothing to mix. *)
  rejected "[color:notacolour]/50"

(* A [#...] value is only a colour when it is a hex spelling. A malformed one
   reaches the raising hex constructor from inside [of_class], so the exception
   escapes the parser itself. *)
let test_invalid_hex_value () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok _ -> Alcotest.failf "expected %s to be rejected" cls
    | Error _ -> ()
  in
  rejected "[color:#zz]";
  rejected "[color:#]";
  rejected "[color:#12345]";
  Alcotest.(check bool)
    "[color:#ff0000] still emits the colour" true
    (Astring.String.is_infix ~affix:"color: #ff0000" (css "[color:#ff0000]"))

(* An arbitrary property sorts where the property it declares sorts, which is
   the same rule a project's own [@utility] follows. They all shared one slot
   near the end of the layer instead, so [[order:3]] came after the margins it
   belongs in front of. No two of these write a common property, so no canonical
   comparison could see it; the positions in the sheet can. *)
let test_sorts_by_declared_property () =
  Test_helpers.check_class_order ~test_name:"arbitrary property slots"
    [ "[order:3]"; "m-4"; "[display:grid]"; "p-4"; "[color:red]" ]

(* A property name keeps the underscores it is written with, and [\_] is the
   escape that spells one where the value grammar would read a space. Both
   spellings name the same custom property; the backslash reached the sheet as a
   character of the name. *)
let test_property_underscore_escape () =
  let has cls affix =
    Alcotest.(check bool)
      (cls ^ " declares " ^ affix)
      true
      (Astring.String.is_infix ~affix (css cls))
  in
  has {|[--my\_var:red]|} "--my_var: red";
  has "[--my_var:red]" "--my_var: red"

(* A []] the value quotes belongs to the value, so the scan for the closing
   bracket reads strings the way the CSS tokeniser does. Tailwind emits
   [content: 'a]b'] for the first of these; a scan blind to quotes stopped at
   the []] inside the string and refused the class. *)
let test_quoted_closing_bracket () =
  check "[content:'a]b']";
  check "[--x:'a]b']";
  check {|[content:"a]b"]|};
  Alcotest.(check bool)
    "[content:'a]b'] keeps the bracket in the string" true
    (Astring.String.is_infix ~affix:"content: 'a]b'" (css "[content:'a]b']"));
  Alcotest.(check bool)
    "[--x:'a]b'] keeps the bracket in the string" true
    (Astring.String.is_infix ~affix:"--x: 'a]b'" (css "[--x:'a]b']"));
  Alcotest.(check bool)
    "[background-image:url('a]b')] keeps the bracket in the url" true
    (Astring.String.is_infix ~affix:"background-image: url('a]b')"
       (css "[background-image:url('a]b')]"))

(* A [url()] argument is left verbatim, so a [_] in a file name stays one while
   the [_] separating it from the next value becomes a space. *)
let test_url_underscore () =
  Alcotest.(check bool)
    "the url keeps its underscore" true
    (Astring.String.is_infix ~affix:"background-image: url('a_b.png')"
       (css "[background-image:url('a_b.png')]"));
  Alcotest.(check bool)
    "the underscore after the url is a space" true
    (Astring.String.is_infix ~affix:"background-image: url('a_b.png') center"
       (css "[background-image:url('a_b.png')_center]"));
  Alcotest.(check bool)
    "the underscore inside an image-set url stays" true
    (Astring.String.is_infix
       ~affix:"background-image: image-set(url('a_b.png') 1x)"
       (css "[background-image:image-set(url('a_b.png')_1x)]"))

let tests =
  [
    test_case "property name underscore escape" `Quick
      test_property_underscore_escape;
    test_case "sorts by the property it declares" `Quick
      test_sorts_by_declared_property;
    test_case "invalid hex value" `Quick test_invalid_hex_value;
    test_case "arbitrary of_string - valid values" `Quick of_string_valid;
    test_case "arbitrary of_string - invalid values" `Quick of_string_invalid;
    test_case "text after the closing bracket" `Quick test_trailing_text;
    test_case "quoted closing bracket" `Quick test_quoted_closing_bracket;
    test_case "url argument underscores" `Quick test_url_underscore;
    test_case "--alpha() value with a /opacity modifier" `Quick
      test_alpha_fn_with_modifier;
    test_case "var-valued opacity modifier spelling" `Quick
      test_var_opacity_spelling;
    test_case "named colour value" `Quick test_named_colour_value;
    test_case "property value calc operators" `Quick
      test_property_calc_operators;
    test_case "property value --spacing()" `Quick test_property_spacing_fn;
    test_case "property value --alpha()" `Quick test_alpha_fn;
    test_case "theme() dot-notation" `Quick test_theme_dot_notation;
    test_case "var-valued colour with opacity" `Quick test_var_color_opacity;
    test_case "encoded-space colour with opacity" `Quick
      test_encoded_space_color_opacity;
    test_case "custom property with opacity" `Quick test_custom_prop_opacity;
    test_case "deferred and var inputs never crash" `Quick test_no_crash;
    test_case "adversarial arbitrary values name their own rules" `Quick
      test_adversarial_value_sweep;
  ]

let suite = ("arbitrary", tests)
