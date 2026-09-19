open Alcotest
open Tw.Arbitrary.Handler

let check = Test_helpers.check_handler_roundtrip (module Tw.Arbitrary.Handler)

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

(* An arbitrary [property:value] normalises omitted whitespace around calc
   operators, like the utility form: [margin:calc(100%-10px)] emits margin:
   calc(100% - 10px). *)
let test_property_calc_operators () =
  Test_helpers.check_declarations ~minify:false "[margin:calc(100%-10px)]"
    [ "margin: calc(100% - 10px)" ];
  Test_helpers.check_declarations ~minify:false
    "[width:calc(var(--a)-var(--b))]"
    [ "width: calc(var(--a) - var(--b))" ]

(* theme() dot-notation inside an arbitrary value resolves statically:
   theme(colors.red.500) to the red-500 oklch, the opacity form to that colour
   mixed with the alpha, and the class name round-trips via its alias. *)
let test_theme_dot_notation () =
  let g =
    "bg-[image:linear-gradient(to_right,theme(colors.red.500)_75%,theme(colors.red.500/25%))]"
  in
  Test_helpers.check_declarations ~minify:false g
    [
      "background-image: linear-gradient(to right, oklch(63.7% .237 25.331) \
       75%, color-mix(in oklab, oklch(63.7% .237 25.331) 25%, transparent))";
    ];
  Alcotest.(check string)
    "theme() class round-trips" g
    (Tw.pp (Result.get_ok (Tw.of_string g)))

(* A var-valued colour with /opacity used to raise invalid_arg; it now emits an
   oklab color-mix under @supports, with a fallback, type-safely. *)
let test_var_color_opacity () =
  (* The fallback arm leaves the var() bare, the shape the CLI writes. *)
  Test_helpers.check_declarations ~minify:false "[color:var(--my-color)]/50"
    [
      "color: var(--my-color)";
      "color: color-mix(in oklab, var(--my-color) 50%, transparent)";
    ]

(* Arbitrary-value underscores encode spaces before the colour is parsed. The
   plain declaration path already decoded them; the /opacity path did not. *)
let test_encoded_space_color_opacity () =
  (* The whole list, which also says the modifier decodes the colour once: the
     mix names the decoded rgb(), not the undecoded spelling. A mix of a colour
     and a percentage is written once, as the CLI writes it; tw wrote an sRGB
     twin of its own before it. *)
  Test_helpers.check_declarations ~minify:false "[color:rgb(255_0_0)]/50"
    [ "color: color-mix(in oklab, rgb(255 0 0) 50%, transparent)" ];
  (* A var() alpha needs the polyfill pair, the bare colour in the open. The
     oklch value is the spelling already pinned here: cascade drops the leading
     zero the CLI writes. *)
  Test_helpers.check_declarations ~minify:false
    "[border-color:oklch(0.5_0.2_250)]/[var(--x)]"
    [
      "border-color: oklch(.5 .2 250)";
      "border-color: color-mix(in oklab, oklch(.5 .2 250) var(--x), \
       transparent)";
    ]

(* A custom property with /opacity sets the property to a color-mix via the
   typed [Css.var] form (no token stream). *)
let test_custom_prop_opacity () =
  Test_helpers.check_declarations ~minify:false "[--x:#ff0000]/50"
    [ "--x: color-mix(in oklab, #ff0000 50%, transparent)" ]

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
  Test_helpers.check_declarations ~minify:false "[--gap:--spacing(10)]"
    [ "--gap: calc(var(--spacing) * 10)" ]

(* Tailwind's [--alpha(C/P)] is the [/opacity] form written as a function, and a
   reference to a palette token renders from the palette, so the fallback is a
   colour rather than the bare reference. *)
let test_alpha_fn () =
  Test_helpers.check_declarations ~minify:false
    "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]"
    [
      "--checkered-bg: #0307121a";
      "--checkered-bg: color-mix(in oklab, var(--color-gray-950) 10%, \
       transparent)";
    ];
  Alcotest.(check string)
    "the --alpha() spelling round-trips"
    "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]"
    (Tw.pp
       (Result.get_ok
          (Tw.of_string "[--checkered-bg:--alpha(var(--color-gray-950)/10%)]")))

(* A bare number in [--alpha()] is the fraction Tailwind scales to a percentage,
   in an arbitrary property as in a colour utility; the reader took [0.2] for
   [0.2%]. *)
let test_alpha_fn_bare_number () =
  Test_helpers.check_declarations ~minify:false "[color:--alpha(red/0.2)]"
    [ "color: color-mix(in oklab, red 20%, transparent)" ]

(* An [--alpha()] whose alpha reads a custom property mixes that property, in an
   arbitrary property as in a colour utility. The reader took only a percentage
   alpha, so the class was refused. The pair is the one the [/(--o)] modifier
   writes: the bare colour in the open, the mix behind the guard. *)
let test_alpha_fn_var_alpha () =
  check "[color:--alpha(red/var(--o))]";
  Test_helpers.check_declarations ~minify:false "[color:--alpha(red/var(--o))]"
    [ "color: red"; "color: color-mix(in oklab, red var(--o), transparent)" ];
  Test_helpers.check_declarations ~minify:false
    "[--x:--alpha(var(--c)/var(--o))]"
    [
      "--x: var(--c)";
      "--x: color-mix(in oklab, var(--c) var(--o), transparent)";
    ]

(* Tailwind substitutes [--alpha()] wherever it stands in a value, as it does
   [--spacing()]: a shadow or a gradient spelling one reads on with the
   [color-mix()] in its place. The call was read only as the whole value, and
   the guard that refuses a surviving call ran over the undecoded text, where
   [1px_--alpha] is one token, so the call reached the sheet as written. *)
let test_alpha_fn_inside_a_value () =
  check "[box-shadow:0_0_0_1px_--alpha(red/50%)]";
  Test_helpers.check_declarations ~minify:false
    "[box-shadow:0_0_0_1px_--alpha(red/50%)]"
    [ "box-shadow: 0 0 0 1px color-mix(in oklab, red 50%, transparent)" ];
  Test_helpers.check_declarations ~minify:false
    "[background-image:linear-gradient(--alpha(red/0.5),blue)]"
    [
      "background-image: linear-gradient(color-mix(in oklab, red 50%, \
       transparent), blue)";
    ];
  (* a call missing its alpha is a lookup that failed, wherever it stands *)
  rejected "[box-shadow:0_0_0_1px_--alpha(red)]"

(* The [/] modifier applies to the colour the value denotes, so a value written
   with [--alpha()] mixes twice, and both spellings survive the round-trip. *)
let test_alpha_fn_with_modifier () =
  check "[color:--alpha(red/50%)]/25";
  (* Both mixes are nested. Neither reads a custom property or [currentcolor],
     so a browser resolves the value on its own and Tailwind writes it once; tw
     wrote an sRGB twin in the open that Tailwind's polyfill never writes. *)
  Test_helpers.check_declarations ~minify:false "[color:--alpha(red/50%)]/25"
    [
      "color: color-mix(in oklab, color-mix(in oklab, red 50%, transparent) \
       25%, transparent)";
    ]

(* An opacity read from a custom property keeps the spelling it was written
   with, in either the bracket or the parenthesised form. *)
let test_var_opacity_spelling () =
  check "[color:red]/[var(--x)]";
  check "[color:red]/(--x)";
  (* The mix reads a custom property, so the open form is its first colour, as
     it is for every other family; tw wrote the mix in sRGB there. *)
  Test_helpers.check_declarations ~minify:false "[color:red]/(--x)"
    [ "color: red"; "color: color-mix(in oklab, red var(--x), transparent)" ]

(* Tailwind's colour-mix polyfill applies to any declaration whose value holds a
   [color-mix()] reading a custom property or [currentcolor], wherever in the
   value the mix stands: the declaration is written with each such mix replaced
   by its first colour in the open, and as written behind the colour-mix guard.
   A mix reading only theme tokens has them inlined, in sRGB, in the open. A mix
   a browser resolves on its own is written once. tw wrote the mix alone
   wherever it stood inside a longer value, and the typed colour arm wrote an
   sRGB twin for every modifier, guarded or not. *)
let test_mix_polyfill () =
  let pair cls ~open_ ~guarded =
    Test_helpers.check_declarations ~minify:false cls [ open_; guarded ]
  in
  let mix colour alpha =
    "color-mix(in oklab, " ^ colour ^ " " ^ alpha ^ ", transparent)"
  in
  pair "[box-shadow:0_0_0_1px_--alpha(red/var(--o))]"
    ~open_:"box-shadow: 0 0 0 1px red"
    ~guarded:("box-shadow: 0 0 0 1px " ^ mix "red" "var(--o)");
  (* a mix the author spelled out is read the same way *)
  pair "[box-shadow:0_0_0_1px_color-mix(in_oklab,red_var(--o),transparent)]"
    ~open_:"box-shadow: 0 0 0 1px red"
    ~guarded:("box-shadow: 0 0 0 1px " ^ mix "red" "var(--o)");
  (* [currentColor] is cascade's spelling of the keyword outside a mix *)
  pair "[background-image:linear-gradient(--alpha(currentcolor/50%),blue)]"
    ~open_:"background-image: linear-gradient(currentColor, blue)"
    ~guarded:
      ("background-image: linear-gradient(" ^ mix "currentcolor" "50%"
     ^ ", blue)");
  pair "[--x:0_0_1px_--alpha(red/var(--o))]" ~open_:"--x: 0 0 1px red"
    ~guarded:("--x: 0 0 1px " ^ mix "red" "var(--o)");
  (* a theme token is inlined, and the mix narrowed to sRGB, in the open *)
  pair "[box-shadow:0_0_0_1px_--alpha(var(--color-red-500)/50%)]"
    ~open_:
      "box-shadow: 0 0 0 1px color-mix(in srgb, oklch(63.7% .237 25.331) 50%, \
       transparent)"
    ~guarded:("box-shadow: 0 0 0 1px " ^ mix "var(--color-red-500)" "50%");
  (* a mix standing for its first colour is read again when that colour is a mix
     itself *)
  pair
    "[color:color-mix(in_oklab,color-mix(in_oklab,red_var(--o),transparent)_50%,transparent)]"
    ~open_:"color: red"
    ~guarded:("color: " ^ mix (mix "red" "var(--o)") "50%");
  (* the typed colour arm follows the same rule: a percentage alone needs no
     twin, [currentcolor] and a custom property need the pair *)
  Test_helpers.check_declarations ~minify:false "[color:red]/50"
    [ "color: " ^ mix "red" "50%" ];
  pair "[color:currentcolor]/50" ~open_:"color: currentColor"
    ~guarded:("color: " ^ mix "currentcolor" "50%");
  pair "[--x:red]/(--o)" ~open_:"--x: red"
    ~guarded:("--x: " ^ mix "red" "var(--o)");
  (* a mix a browser resolves on its own is written once *)
  Test_helpers.check_declarations ~minify:false
    "[box-shadow:0_0_0_1px_--alpha(red/50%)]"
    [ "box-shadow: 0 0 0 1px " ^ mix "red" "50%" ]

(* A modifier reading a custom property mixes that property into the guarded
   value on a bracket [var()] colour as on a plain one. The var arm folded the
   modifier to a percentage, which a var() has none of, so the mix said [100%]
   and the modifier was dropped. The fallback keeps the var() bare, the shape
   the CLI writes. *)
let test_var_colour_opacity_var () =
  let mixed cls property =
    Test_helpers.check_declarations ~minify:false cls
      [
        property ^ ": var(--c)";
        property ^ ": color-mix(in oklab, var(--c) var(--o), transparent)";
      ]
  in
  mixed "[color:var(--c)]/(--o)" "color";
  mixed "[color:var(--c)]/[var(--o)]" "color";
  mixed "[background-color:var(--c)]/(--o)" "background-color";
  mixed "[--x:var(--c)]/(--o)" "--x"

(* A named [--opacity-*] token reads as [var(--opacity-half)] and nothing else,
   in the guarded mix, and as the percentage the theme binds it to in the srgb
   fallback. The token's value is a percentage, which the reader took for a
   float and could not read, so the fallback said [100%] and the reference grew
   a second fallback, [var(--half-opacity)], that Tailwind never writes. The var
   arm folded the token to [100%] outright. *)
let test_named_opacity () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("opacity-half", "50%") ]
  in
  let mixed cls property colour =
    Test_helpers.check_declarations ~theme ~minify:false cls
      [
        property ^ ": color-mix(in srgb, " ^ colour ^ " 50%, transparent)";
        property ^ ": color-mix(in oklab, " ^ colour
        ^ " var(--opacity-half), transparent)";
      ]
  in
  mixed "[color:red]/half" "color" "red";
  mixed "[background-color:#123456]/half" "background-color" "#123456";
  mixed "[--my-color:red]/half" "--my-color" "red";
  Test_helpers.check_declarations ~theme ~minify:false "[color:var(--c)]/half"
    [
      "color: var(--c)";
      "color: color-mix(in oklab, var(--c) var(--opacity-half), transparent)";
    ]

(* Colour values go through the CSS reader, so every named colour is a colour,
   not a hand-picked subset of them. *)
let test_named_colour_value () =
  check "[color:rebeccapurple]/50";
  Test_helpers.check_declarations ~minify:false "[color:rebeccapurple]/50"
    [ "color: color-mix(in oklab, rebeccapurple 50%, transparent)" ];
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
  Test_helpers.check_declarations ~minify:false "[color:#ff0000]"
    [ "color: #ff0000" ]

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
    Test_helpers.check_declarations ~minify:false cls [ affix ]
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
  Test_helpers.check_declarations ~minify:false "[content:'a]b']"
    [ "content: 'a]b'" ];
  (* Tailwind keeps the quote the class wrote; the quoting is cascade's
     canonical spelling of the same CSS string. *)
  Test_helpers.check_declarations ~minify:false "[--x:'a]b']" [ {|--x: "a]b"|} ];
  Test_helpers.check_declarations ~minify:false "[background-image:url('a]b')]"
    [ "background-image: url('a]b')" ]

(* A [url()] argument is left verbatim, so a [_] in a file name stays one while
   the [_] separating it from the next value becomes a space. *)
let test_url_underscore () =
  Test_helpers.check_declarations ~minify:false
    "[background-image:url('a_b.png')]"
    [ "background-image: url('a_b.png')" ];
  (* Outside the url the underscore is a space, in a shorthand that takes both.
     [background-image] does not take a position, and tw declines to write the
     invalid declaration Tailwind emits for it. *)
  Test_helpers.check_declarations ~minify:false
    "[background:url(a_b.png)_no-repeat]"
    [ "background: url(a_b.png) no-repeat" ];
  (* Tailwind keeps the inner url quoted; the quoting is cascade's canonical
     spelling of the same URL. *)
  Test_helpers.check_declarations ~minify:false
    "[background-image:image-set(url('a_b.png')_1x)]"
    [ "background-image: image-set(url(a_b.png) 1x)" ]

(* The same token-stream contract, read through the named utilities that carry a
   bracket: a candidate is generated when its bracket holds one safe CSS
   declaration value, even when the browser will reject that value for the
   utility's property. The generated documentation corpus uses [<value>] to
   exercise this boundary. *)
let generated_placeholders () =
  let cases =
    [
      ("border-spacing-[<value>]", "--tw-border-spacing-x:<value>");
      ("border-spacing-x-[<value>]", "--tw-border-spacing-x:<value>");
      ("border-spacing-y-[<value>]", "--tw-border-spacing-y:<value>");
      ("ease-[<value>]", "--tw-ease:<value>");
      ("from-[<value>]", "--tw-gradient-from:<value>");
      ("inset-ring-[<value>]", "--tw-inset-ring-color:<value>");
      ("ring-[<value>]", "--tw-ring-color:<value>");
      ("skew-[<value>]", "--tw-skew-x:skewX(<value>)");
      ("skew-x-[<value>]", "--tw-skew-x:skewX(<value>)");
      ("skew-y-[<value>]", "--tw-skew-y:skewY(<value>)");
      ("line-clamp-[<value>]", "-webkit-box-orient:vertical");
      ("translate-z-[<value>]", "--tw-translate-z:<value>");
      ("divide-x-[<value>]", "--tw-divide-x-reverse:0");
      ("divide-y-[<value>]", "--tw-divide-y-reverse:0");
      ("scrollbar-thumb-[<value>]", "--tw-scrollbar-thumb:<value>");
      ("scrollbar-track-[<value>]", "--tw-scrollbar-track:<value>");
      ("via-[<value>]", "--tw-gradient-via:<value>");
      ("backdrop-blur-[<value>]", "--tw-backdrop-blur:blur(<value>)");
      ( "backdrop-brightness-[<value>]",
        "--tw-backdrop-brightness:brightness(<value>)" );
      ("backdrop-contrast-[<value>]", "--tw-backdrop-contrast:contrast(<value>)");
      ( "backdrop-grayscale-[<value>]",
        "--tw-backdrop-grayscale:grayscale(<value>)" );
      ( "backdrop-hue-rotate-[<value>]",
        "--tw-backdrop-hue-rotate:hue-rotate(<value>)" );
      ("backdrop-invert-[<value>]", "--tw-backdrop-invert:invert(<value>)");
      ("backdrop-opacity-[<value>]", "--tw-backdrop-opacity:opacity(<value>)");
      ("backdrop-saturate-[<value>]", "--tw-backdrop-saturate:saturate(<value>)");
      ("backdrop-sepia-[<value>]", "--tw-backdrop-sepia:sepia(<value>)");
      ("bg-conic-[<value>]", "--tw-gradient-position:<value>");
      ("blur-[<value>]", "--tw-blur:blur(<value>)");
      ("brightness-[<value>]", "--tw-brightness:brightness(<value>)");
      ("content-[<value>]", "--tw-content:<value>");
      ("content-[attr(<name>)]", "--tw-content:attr(<name>)");
      ("contrast-[<value>]", "--tw-contrast:contrast(<value>)");
      ("drop-shadow-[<value>]", "--tw-drop-shadow-size:drop-shadow(<value>)");
      ("grayscale-[<value>]", "--tw-grayscale:grayscale(<value>)");
      ("hue-rotate-[<value>]", "--tw-hue-rotate:hue-rotate(<value>)");
      ("inset-shadow-[<value>]", "--tw-inset-shadow:inset <value>");
      ("invert-[<value>]", "--tw-invert:invert(<value>)");
      ("saturate-[<value>]", "--tw-saturate:saturate(<value>)");
      ("sepia-[<value>]", "--tw-sepia:sepia(<value>)");
      ("shadow-[<value>]", "--tw-shadow:<value>");
      ("leading-[<value>]", "--tw-leading:<value>");
      ("tracking-[<value>]", "--tw-tracking:<value>");
      ("transition-[<value>]", "transition-property:<value>");
      ("duration-[<value>]", "--tw-duration:<value>");
      ("to-[<value>]", "--tw-gradient-to:<value>");
      ("animate-[<value>]", "animation:<value>");
      ("aspect-[<value>]", "aspect-ratio:<value>");
    ]
  in
  List.iter
    (fun (cls, fragment) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok utility ->
          Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp utility);
          let css =
            Tw.to_css ~base:false [ utility ] |> Tw.Css.to_string ~minify:true
          in
          Alcotest.(check bool)
            (cls ^ " emits its arbitrary value")
            true
            (Astring.String.is_infix ~affix:fragment css))
    cases

(* Semantic nonsense is still one declaration value; syntax that can start a
   second declaration or close the current rule is not. *)
let unsafe_values_are_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok utility ->
          Alcotest.failf "expected %s to be rejected, got %s" cls
            (Tw.pp utility)
      | Error _ -> ())
    [
      "border-spacing-[x;y]";
      "ease-[linear;color:red]";
      "content-[attr(x);display:block]";
      "animate-[spin;display:block]";
      "rotate-[123deg]/foo";
      "scale-[123]/foo";
      "skew-[123deg]/foo";
      "skew-x-[123deg]/foo";
      "skew-y-[123deg]/foo";
    ]

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
    test_case "--alpha() bare number alpha" `Quick test_alpha_fn_bare_number;
    test_case "--alpha() value with a /opacity modifier" `Quick
      test_alpha_fn_with_modifier;
    test_case "colour-mix polyfill" `Quick test_mix_polyfill;
    test_case "--alpha() with a var alpha" `Quick test_alpha_fn_var_alpha;
    test_case "--alpha() inside a value" `Quick test_alpha_fn_inside_a_value;
    test_case "var-valued opacity modifier spelling" `Quick
      test_var_opacity_spelling;
    test_case "named colour value" `Quick test_named_colour_value;
    test_case "var-valued colour opacity from a var" `Quick
      test_var_colour_opacity_var;
    test_case "named opacity token" `Quick test_named_opacity;
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
    test_case "generated placeholders" `Quick generated_placeholders;
    test_case "unsafe values are rejected" `Quick unsafe_values_are_rejected;
  ]

let suite = ("arbitrary", tests)
