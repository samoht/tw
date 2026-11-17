module Css = Cascade.Css
open Alcotest

let check_utilities = Test_helpers.check_utilities

(* One token reads into the modifier it names, and a token naming none reads as
   nothing, so [Modifiers.apply] can refuse the whole class. A named group holds
   the state it names, not the unnamed [group-] modifier. *)
let test_modifier_reads_one_token () =
  let theme = Tw.Scheme.default in
  let reads token expected =
    check bool token true
      (Tw.Variant_parse.modifier ~theme token = Some expected)
  in
  reads "hover" Tw.Style.Hover;
  reads "md" (Tw.Style.Responsive `Md);
  reads "not-hover" (Tw.Style.Not Tw.Style.Hover);
  reads "group-hover/item" (Tw.Style.Named_group (Tw.Style.Hover, "item"));
  check bool "unknown" true (Tw.Variant_parse.modifier ~theme "unknown" = None);
  check bool "split name" true
    (Tw.Variant_parse.split_name "checked/name" = ("checked", Some "name"));
  reads "aria-sort" (Tw.Style.Aria "sort");
  reads "group-aria-[sort]" (Tw.Style.Group_aria ("[sort]", None))

(* The bracket holds a length, so a word is not a breakpoint at all. *)
let test_arbitrary_breakpoint_rejects_non_length () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [ "min-[abc]:flex"; "max-[abc]:flex"; "min-[]:flex" ]

(* The number in front of the unit is a CSS number, not an OCaml one. Reading it
   with [float_of_string_opt] turned [min-[0x600px]] into a live [(min-width:
   1536px)] breakpoint: a query the browser honours, built out of a value
   Tailwind passes through verbatim for the browser to drop. Nothing here is a
   length either, so tw declines the whole class. *)
let test_arbitrary_breakpoint_rejects_ocaml_literals () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "%s parsed as %s emitting %s" cls (Tw.pp u)
            (Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true)
      | Error (`Msg _) -> ())
    [
      "min-[0x600px]:p-4";
      "max-[0x600px]:p-4";
      "min-[0x600]:p-4";
      "min-[0o17px]:p-4";
      "min-[1_000px]:p-4";
    ]

(* A [@custom-variant] belongs to the [@theme] block that declared it. Two
   stylesheets built in one process read different themes, so a variant the
   first declared must be unknown to the second. *)
let test_custom_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      custom_variants =
        [
          ( "is-data",
            Tw.Scheme.{ values = [ ("", "&[data-x]") ]; template = "&:is({})" }
          );
        ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "is-data:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "is-data:flex"))

(* Same for a [@custom-variant] whose body is a container query: it is held in
   its own field, and belongs to its own theme just the same. *)
let test_container_variant_is_theme_local () =
  let declaring : Tw.Scheme.t =
    {
      Tw.Scheme.default with
      container_variants = [ ("has-a", Css.Container.of_string "style(--a)") ];
    }
  in
  check bool "the declaring theme resolves its own variant" true
    (Result.is_ok (Tw.of_string ~theme:declaring "has-a:flex"));
  check bool "a second theme does not see it" true
    (Result.is_error (Tw.of_string ~theme:Tw.Scheme.default "has-a:flex"))

(* A [@theme] block that clears [--breakpoint-*] takes the built-in responsive
   variants with it: [md:] then names a breakpoint the project no longer has,
   and the candidate stops resolving the way an unknown variant does. The
   [min-], [max-] and [not-] spellings read the same breakpoint, so they go too,
   while a breakpoint the block declared for itself still resolves. *)
let test_removed_breakpoint_drops_its_variants () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default
      [ ("breakpoint-*", "initial"); ("breakpoint-tablet", "40rem") ]
  in
  List.iter
    (fun cls ->
      check bool
        (cls ^ " names a breakpoint the theme removed")
        true
        (Result.is_error (Tw.of_string ~theme cls)))
    [ "md:flex"; "min-md:flex"; "max-md:flex"; "not-md:flex" ];
  check bool "the theme's own breakpoint still resolves" true
    (Result.is_ok (Tw.of_string ~theme "tablet:flex"))

(* [--breakpoint-md: initial] removes the one breakpoint, and Tailwind emits
   nothing for [md:flex]. The built-in table let it go, and the custom
   breakpoints then took it back: [initial] reads as a length, so the removed
   token was a custom breakpoint of that width, and the sheet carried [@media
   (min-width: initial)], a query no browser honours. *)
let test_removed_single_breakpoint_drops_its_variants () =
  let theme =
    Tw.Scheme.with_overrides Tw.Scheme.default [ ("breakpoint-md", "initial") ]
  in
  List.iter
    (fun cls ->
      check bool
        (cls ^ " names a breakpoint the theme removed")
        true
        (Result.is_error (Tw.of_string ~theme cls)))
    [ "md:flex"; "min-md:flex"; "max-md:flex"; "not-md:flex" ];
  check bool "the other breakpoints still resolve" true
    (Result.is_ok (Tw.of_string ~theme "lg:flex"))

(* The bracket is negated as a selector, so content the selector grammar cannot
   read is not a negation and the class is refused at parse time. The reader
   raises rather than answering, and it stops at the first thing it cannot use,
   so a trailing remainder is refused too rather than silently dropped. *)
let test_not_bracket_unreadable_selector_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u ->
          Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
      | Error (`Msg _) -> ())
    [
      "not-[a;b]:flex";
      "not-[0.5ch]:flex";
      "not-[1px}]:flex";
      "not-[a{b]:flex";
      "not-[*/]:flex";
      "not-[<value>]:flex";
      "not-[url(a;b)]:flex";
      "not-[()]:flex";
    ];
  (* the readable ones still parse *)
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok _ -> ()
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m)
    [
      "not-[.foo]:flex";
      "not-[.os-macos_&]:flex";
      "not-[:checked]:flex";
      "not-[@media_print]:flex";
    ]

(* [not-data-] negates the selector [data-] builds, and Tailwind 4.3.3 reads it
   so: [:not([data-state=open])] for [not-data-[state=open]]. A value no
   attribute selector can hold compiles to nothing there, so tw refuses the
   class rather than building the attribute name [data-[0.5ch]] and raising. *)
let test_not_data_negates_data () =
  check_utilities "not-data-active:flex"
    {|.not-data-active\:flex:not([data-active]){display:flex}|};
  check_utilities "not-data-[foo]:flex"
    {|.not-data-\[foo\]\:flex:not([data-foo]){display:flex}|};
  check_utilities "not-data-[state=open]:flex"
    {|.not-data-\[state\=open\]\:flex:not([data-state=open]){display:flex}|};
  List.iter
    (Test_helpers.check_invalid_input (module Tw.Flex.Handler))
    [
      "not-data-[0.5ch]:flex";
      "not-data-[a;b]:flex";
      "not-data-0.5ch:flex";
      "not-data-foo/bar:flex";
    ]

(* [not-nth-] negates the selector the [nth-] variant builds, whichever of the
   four it is: Tailwind writes [:not(:nth-of-type(3))] for [not-nth-of-type-3],
   and nothing for [not-nth-foo]. *)
let test_not_nth_negates_nth () =
  check_utilities "not-nth-3:flex"
    {|.not-nth-3\:flex:not(:nth-child(3)){display:flex}|};
  check_utilities "not-nth-last-3:flex"
    {|.not-nth-last-3\:flex:not(:nth-last-child(3)){display:flex}|};
  check_utilities "not-nth-of-type-3:flex"
    {|.not-nth-of-type-3\:flex:not(:nth-of-type(3)){display:flex}|};
  check_utilities "not-nth-last-of-type-[2n+1]:flex"
    {|.not-nth-last-of-type-\[2n\+1\]\:flex:not(:nth-last-of-type(odd)){display:flex}|};
  Test_helpers.check_invalid_input (module Tw.Flex.Handler) "not-nth-foo:flex";
  (* Tailwind writes [:not(:nth-child(0.5ch))], a selector browsers refuse, so
     refusing the class loses nothing, as for [nth-[abc]]. *)
  match Tw.of_string "not-nth-[0.5ch]:flex" with
  | Ok u -> Alcotest.failf "not-nth-[0.5ch]:flex parsed as %s" (Tw.pp u)
  | Error _ -> ()

(* A bare [aria-] name and a [group-not-] or [peer-not-] bracket are built into
   a selector at render time, so a name or a bracket the selector grammar
   refuses is refused where the class is parsed. Tailwind compiles nothing for
   [aria-0.5ch], [peer-not-[a;b]] and an at-rule bracket, and writes
   [:not(:is(()))] for [group-not-[()]], a selector browsers refuse. A bare
   [group-data-] name is taken as written, as Tailwind takes it, rather than
   underscore-decoded into a name no selector holds. *)
let test_unbuildable_scoped_selectors () =
  check_utilities "group-data-a_b:flex"
    {|.group-data-a_b\:flex:is(:where(.group)[data-a_b] *){display:flex}|};
  List.iter
    (Test_helpers.check_invalid_input (module Tw.Flex.Handler))
    [
      "aria-0.5ch:flex";
      "not-aria-0.5ch:flex";
      "not-in-aria-0.5ch:flex";
      "peer-not-[a;b]:flex";
      "group-not-[@media_print]:flex";
      "peer-not-[@supports(display:grid)]/n:flex";
    ];
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok u -> Alcotest.failf "%s parsed as %s" cls (Tw.pp u)
      | Error _ -> ())
    [ "group-not-[()]:flex"; "group-not-[0.5ch]/n:flex" ]

(* An nth-* bracket holds an An+B expression and a supports- bracket holds an
   @supports condition; both are taken verbatim, so content the CSS grammar has
   no production for escapes as a parse error out of the selector or condition
   reader. They are validated where data-[ and has-[ are. *)
let test_invalid_bracket_modifiers () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "nth-[]:flex";
  rejected "nth-[abc]:flex";
  rejected "nth-last-[]:flex";
  rejected "nth-of-type-[]:flex";
  rejected "nth-last-of-type-[]:flex";
  rejected "supports-[]:flex"

(* An aria- or data- variant names an attribute, so an empty argument would
   compile to [[aria-]] or [[data-]]: a selector browsers parse and keep, and
   nothing ever matches. An underscore stands for a space, so it is empty
   too. *)
let test_empty_attribute_brackets () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "aria-[]:flex";
  rejected "aria-[_]:flex";
  rejected "group-aria-[]:flex";
  rejected "peer-aria-[]:flex";
  rejected "data-[]:flex";
  rejected "data-[_]:flex";
  rejected "group-data-[]:flex";
  rejected "peer-data-[]:flex"

(* A named aria- or data- variant writes its name into the attribute as it
   stands: [_] is no space outside brackets, and any name reads, not only the
   ARIA states tw knows. A bare aria name matches [="true"]; in brackets it is
   an attribute expression, so [aria-[busy]] matches presence. A name no
   attribute selector can hold is refused, as Tailwind refuses it. *)
let test_named_attribute_variants () =
  check_utilities "aria-_:flex" {|.aria-_\:flex[aria-_=true]{display:flex}|};
  check_utilities "group-data-_:flex"
    {|.group-data-_\:flex:is(:where(.group)[data-_] *){display:flex}|};
  check_utilities "peer-data-_:flex"
    {|.peer-data-_\:flex:is(:where(.peer)[data-_]~*){display:flex}|};
  check_utilities "aria-foo:flex"
    {|.aria-foo\:flex[aria-foo=true]{display:flex}|};
  check_utilities "not-aria-a_b:flex"
    {|.not-aria-a_b\:flex:not([aria-a_b=true]){display:flex}|};
  check_utilities "in-aria-a_b:flex"
    {|:where([aria-a_b=true]) .in-aria-a_b\:flex{display:flex}|};
  check_utilities "aria-foo_bar:flex"
    {|.aria-foo_bar\:flex[aria-foo_bar=true]{display:flex}|};
  check_utilities "group-aria-foo/x:flex"
    {|.group-aria-foo\/x\:flex:is(:where(.group\/x)[aria-foo=true] *){display:flex}|};
  check_utilities "peer-aria-_:flex"
    {|.peer-aria-_\:flex:is(:where(.peer)[aria-_=true]~*){display:flex}|};
  check_utilities "in-aria-foo:flex"
    {|:where([aria-foo=true]) .in-aria-foo\:flex{display:flex}|};
  check_utilities "has-aria-foo:flex"
    {|.has-aria-foo\:flex:has([aria-foo=true]){display:flex}|};
  check_utilities "not-aria-foo:flex"
    {|.not-aria-foo\:flex:not([aria-foo=true]){display:flex}|};
  check_utilities "aria-[busy]:flex"
    {|.aria-\[busy\]\:flex[aria-busy]{display:flex}|};
  check_utilities "group-aria-[checked]:flex"
    {|.group-aria-\[checked\]\:flex:is(:where(.group)[aria-checked] *){display:flex}|};
  check_utilities "peer-aria-[busy]:flex"
    {|.peer-aria-\[busy\]\:flex:is(:where(.peer)[aria-busy]~*){display:flex}|};
  check_utilities "not-aria-[busy]:flex"
    {|.not-aria-\[busy\]\:flex:not([aria-busy]){display:flex}|};
  check_utilities "group-data-a_b:flex"
    {|.group-data-a_b\:flex:is(:where(.group)[data-a_b] *){display:flex}|};
  check_utilities "has-data-_:flex"
    {|.has-data-_\:flex:has([data-_]){display:flex}|};
  check_utilities "has-data-a_b:flex"
    {|.has-data-a_b\:flex:has([data-a_b]){display:flex}|};
  check_utilities "group-has-data-a_b:flex"
    {|.group-has-data-a_b\:flex:is(:where(.group):has([data-a_b]) *){display:flex}|};
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "aria-foo.bar:flex";
  rejected {|aria-a\b:flex|};
  rejected "aria-foo/x:flex";
  rejected "data-a.b:flex";
  rejected "group-data-a.b:flex";
  rejected "in-data-a.b:flex";
  rejected "has-data-a.b:flex";
  rejected "group-data-[a_b]:flex"

let test_padded_attribute_brackets () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "aria-[_modal_]:flex";
  rejected "data-[_state=open_]:flex"

(* An attribute bracket must name an attribute the selector constructor builds.
   Tailwind writes [[aria-0.5ch]], a selector browsers refuse, so refusing the
   class loses nothing. It writes [[data-a\b]] too, an escaped code point the
   constructor does not take yet: a gap, but a refused class rather than a crash
   at render time. *)
let test_unbuildable_attribute_names () =
  let rejected cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  rejected "aria-[0.5ch]:flex";
  rejected "aria-[a;b]:flex";
  rejected "group-aria-[()]:flex";
  rejected "peer-aria-[+1px]/n:flex";
  rejected "in-aria-[0.5ch]:flex";
  rejected "not-aria-[a}b]:flex";
  rejected {|data-[a\b]:flex|};
  rejected {|group-data-[a\b]:flex|}

(* The attribute spellings that do name something keep working, including the
   empty-name-with-value form Tailwind also accepts. *)
let test_attribute_brackets_still_parse () =
  check_utilities "aria-[modal]:flex"
    {|.aria-\[modal\]\:flex[aria-modal]{display:flex}|};
  check_utilities "aria-[sort=ascending]:flex"
    {|.aria-\[sort\=ascending\]\:flex[aria-sort=ascending]{display:flex}|};
  check_utilities "aria-[=true]:flex"
    {|.aria-\[\=true\]\:flex[aria-=true]{display:flex}|};
  check_utilities "data-[state=open]:flex"
    {|.data-\[state\=open\]\:flex[data-state=open]{display:flex}|};
  check_utilities "group-data-[dragging]:flex"
    {|.group-data-\[dragging\]\:flex:is(:where(.group)[data-dragging] *){display:flex}|};
  check_utilities "peer-data-[dragging]:flex"
    {|.peer-data-\[dragging\]\:flex:is(:where(.peer)[data-dragging]~*){display:flex}|};
  check_utilities "group-data-modified:flex"
    {|.group-data-modified\:flex:is(:where(.group)[data-modified] *){display:flex}|}

(* [parse_data_expr] reads the [data-[...]] bracket body into an attribute
   match: every operator ([$=], [^=], [*=], [~=], [|=], bare [=]), the
   presence-only bare form, a quoted value holding a decoded space, and a
   trailing case-sensitivity flag. This pins the selector each spelling produces
   today so a rewrite of the reader cannot silently change one. *)
let test_data_bracket_operators () =
  let css cls =
    match Tw.of_string cls with
    | Ok u -> Tw.to_css ~base:false [ u ] |> Tw.Css.to_string ~minify:true
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  let emits affix cls =
    check bool cls true (Astring.String.is_infix ~affix (css cls))
  in
  check_utilities "data-[size~=large]:flex"
    {|.data-\[size\~\=large\]\:flex[data-size~=large]{display:flex}|};
  check_utilities "data-[foo=bar]:flex"
    {|.data-\[foo\=bar\]\:flex[data-foo=bar]{display:flex}|};
  check_utilities "data-[foo^=bar]:flex"
    {|.data-\[foo\^\=bar\]\:flex[data-foo^=bar]{display:flex}|};
  check_utilities "data-[foo$=bar]:flex"
    {|.data-\[foo\$\=bar\]\:flex[data-foo$=bar]{display:flex}|};
  check_utilities "data-[foo*=bar]:flex"
    {|.data-\[foo\*\=bar\]\:flex[data-foo*=bar]{display:flex}|};
  check_utilities "data-[foo|=bar]:flex"
    {|.data-\[foo\|\=bar\]\:flex[data-foo|=bar]{display:flex}|};
  check_utilities "data-[open]:flex"
    {|.data-\[open\]\:flex[data-open]{display:flex}|};
  (* A decoded space in the value stays on a substring: tw quotes the attribute
     value where the pinned CLI escapes the space instead ([data-foo=a\ b]). *)
  emits {|[data-foo="a b"]|} "data-[foo='a_b']:flex";
  check_utilities "data-[foo=bar_i]:flex"
    {|.data-\[foo\=bar_i\]\:flex[data-foo=bar i]{display:flex}|};
  check_utilities "data-[foo=bar_s]:flex"
    {|.data-\[foo\=bar_s\]\:flex[data-foo=bar s]{display:flex}|};
  check_utilities "group-data-[size~=large]:flex"
    {|.group-data-\[size\~\=large\]\:flex:is(:where(.group)[data-size~=large] *){display:flex}|};
  check_utilities "peer-data-[size~=large]:flex"
    {|.peer-data-\[size\~\=large\]\:flex:is(:where(.peer)[data-size~=large]~*){display:flex}|}

(* A bare [[...]] variant compounds its selector onto the utility's own class,
   so what the brackets hold has to be a compound selector. [~] is both the
   sibling combinator and the [~=] whitespace-list attribute operator, and only
   reading the bracket as a selector tells them apart: a character scan that
   rejects every [~] rejects [[data-size~=large]] along with [p_~_span]. *)
let test_bare_selector_variant_attribute_operators () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  check_utilities "[[data-size~=large]]:underline"
    {|.\[\[data-size\~\=large\]\]\:underline[data-size~=large]{text-decoration-line:underline}|};
  check_utilities "group-[[data-size~=large]]:underline"
    {|.group-\[\[data-size\~\=large\]\]\:underline:is(:where(.group)[data-size~=large] *){text-decoration-line:underline}|};
  check_utilities "peer-[[data-size~=large]]:underline"
    {|.peer-\[\[data-size\~\=large\]\]\:underline:is(:where(.peer)[data-size~=large]~*){text-decoration-line:underline}|};
  (* the attribute operators that never collided with a combinator stay put *)
  check_utilities "[[lang|=en]]:underline"
    {|.\[\[lang\|\=en\]\]\:underline[lang|=en]{text-decoration-line:underline}|};
  check_utilities "[[href^=https]]:underline"
    {|.\[\[href\^\=https\]\]\:underline[href^=https]{text-decoration-line:underline}|};
  (* a leading combinator selects nothing on its own *)
  rejected "[>img]:underline"

(* A bracket with no [&] compounds onto the utility's class the way Tailwind
   writes [&:is(...)]: a class, an attribute or a pseudo-class attaches as it
   stands, and a type or universal selector, a complex selector or a list stays
   in [:is()], under [group-] and [peer-] as well. *)
let test_arbitrary_selector_variants () =
  check_utilities "[p_~_span]:underline"
    {|.\[p_\~_span\]\:underline:is(p~span){text-decoration-line:underline}|};
  check_utilities "[.a_.b]:underline"
    {|.\[\.a_\.b\]\:underline:is(.a .b){text-decoration-line:underline}|};
  check_utilities "[p>span]:underline"
    {|.\[p\>span\]\:underline:is(p>span){text-decoration-line:underline}|};
  check_utilities "[p_+_span]:underline"
    {|.\[p_\+_span\]\:underline:is(p+span){text-decoration-line:underline}|};
  check_utilities "[div_p_span]:underline"
    {|.\[div_p_span\]\:underline:is(div p span){text-decoration-line:underline}|};
  check_utilities "[:hover_p]:underline"
    {|.\[\:hover_p\]\:underline:is(:hover p){text-decoration-line:underline}|};
  check_utilities "[.a,.b]:underline"
    {|.\[\.a\,\.b\]\:underline:is(.a,.b){text-decoration-line:underline}|};
  check_utilities "[*]:underline"
    {|.\[\*\]\:underline:is(*){text-decoration-line:underline}|};
  check_utilities "[.a_.b]:hover:underline"
    {|@media(hover:hover){.\[\.a_\.b\]\:hover\:underline:is(.a .b):hover{text-decoration-line:underline}}|};
  check_utilities "group-[.a,.b]:underline"
    {|.group-\[\.a\,\.b\]\:underline:is(:where(.group):is(.a,.b) *){text-decoration-line:underline}|};
  check_utilities "group-[*]:underline"
    {|.group-\[\*\]\:underline:is(:where(.group):is(*) *){text-decoration-line:underline}|};
  check_utilities "peer-[p_~_span]:underline"
    {|.peer-\[p_\~_span\]\:underline:is(:where(.peer):is(p~span)~*){text-decoration-line:underline}|}

(* An [in-] or [not-] bracket is the arbitrary selector its own variant reads:
   with no [&] it goes through [:is()] where it is not a compound, as Tailwind
   writes it, and an [&] stands for the element itself. [in-[.a_.b]] read the
   bracket as one class name, [.a_\.b]. *)
let test_in_and_not_bracket_selectors () =
  check_utilities "in-[.a_.b]:flex"
    {|:where(:is(.a .b)) .in-\[\.a_\.b\]\:flex{display:flex}|};
  check_utilities "in-[.a,.b]:flex"
    {|:where(:is(.a,.b)) .in-\[\.a\,\.b\]\:flex{display:flex}|};
  check_utilities "in-[.a:hover]:flex"
    {|:where(.a:hover) .in-\[\.a\:hover\]\:flex{display:flex}|};
  check_utilities "in-[[data-x]]:flex"
    {|:where([data-x]) .in-\[\[data-x\]\]\:flex{display:flex}|};
  check_utilities "in-[:hover]:flex"
    {|:where(:hover) .in-\[\:hover\]\:flex{display:flex}|};
  check_utilities "in-[p]:flex" {|:where(:is(p)) .in-\[p\]\:flex{display:flex}|};
  check_utilities "in-[p.a]:flex"
    {|:where(:is(p.a)) .in-\[p\.a\]\:flex{display:flex}|};
  check_utilities "in-[&_p]:flex"
    {|:where(* p) .in-\[\&_p\]\:flex{display:flex}|};
  check_utilities "in-[.a_&]:flex"
    {|:where(.a *) .in-\[\.a_\&\]\:flex{display:flex}|};
  check_utilities "not-in-[.a_.b]:flex"
    {|.not-in-\[\.a_\.b\]\:flex:not(:where(:is(.a .b)) *){display:flex}|};
  check_utilities "not-[p_~_span]:flex"
    {|.not-\[p_\~_span\]\:flex:not(:is(p~span)){display:flex}|};
  check_utilities "not-[.a,.b]:flex"
    {|.not-\[\.a\,\.b\]\:flex:not(:is(.a,.b)){display:flex}|};
  check_utilities "not-[p]:flex" {|.not-\[p\]\:flex:not(:is(p)){display:flex}|};
  Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
    "in-[@media_print]:flex";
  (* Tailwind writes [:where( > p)] and [:where()], selectors browsers drop, so
     refusing these loses nothing. *)
  let refused cls =
    match Tw.of_string cls with
    | Ok u -> Alcotest.failf "expected %s to be rejected, got %s" cls (Tw.pp u)
    | Error _ -> ()
  in
  refused "in-[>p]:flex";
  refused "not-in-[>p]:flex"

(* A [group-not-[...]] or [peer-not-[...]] bracket is the selector the scope
   negates, and keeps that negation and its own class name when an outer [not-],
   [has-] or [in-] holds the whole variant. tw printed [group-not-not-[.foo]]
   and dropped the [:not()]. *)
let test_scoped_negated_bracket_nested () =
  check_utilities "not-group-not-[.foo]:flex"
    {|.not-group-not-\[\.foo\]\:flex:not(:is(:where(.group):not(.foo) *)){display:flex}|};
  check_utilities "not-group-not-[.foo]/x:flex"
    {|.not-group-not-\[\.foo\]\/x\:flex:not(:is(:where(.group\/x):not(.foo) *)){display:flex}|};
  check_utilities "not-group-not-[p_span]:flex"
    {|.not-group-not-\[p_span\]\:flex:not(:is(:where(.group):not(:is(p span)) *)){display:flex}|};
  check_utilities "not-peer-not-[.foo]:flex"
    {|.not-peer-not-\[\.foo\]\:flex:not(:is(:where(.peer):not(.foo)~*)){display:flex}|};
  check_utilities "has-group-not-[.foo]:flex"
    {|.has-group-not-\[\.foo\]\:flex:has(:is(:where(.group):not(.foo) *)){display:flex}|};
  check_utilities "in-group-not-[.foo]:flex"
    {|:where(:is(:where(.group):not(.foo) *)) .in-group-not-\[\.foo\]\:flex{display:flex}|}

(* The valid spellings the validation must keep accepting. *)
let test_valid_bracket_modifiers () =
  check_utilities "nth-[2n+1]:flex"
    {|.nth-\[2n\+1\]\:flex:nth-child(odd){display:flex}|};
  check_utilities "nth-[3]:flex" {|.nth-\[3\]\:flex:nth-child(3){display:flex}|};
  check_utilities "nth-last-[2n]:flex"
    {|.nth-last-\[2n\]\:flex:nth-last-child(2n){display:flex}|};
  check_utilities "nth-of-type-[odd]:flex"
    {|.nth-of-type-\[odd\]\:flex:nth-of-type(odd){display:flex}|};
  check_utilities "supports-[display:grid]:flex"
    {|@supports(display:grid){.supports-\[display\:grid\]\:flex{display:flex}}|}

(* A [supports-[...]] condition that is not a bare [prop:value] pair goes to the
   condition reader whole. [not(display:grid)] was split at its colon into the
   property [not(display] and the value [grid)], which cascade refused with a
   [Failure] out of a pure render, exit 125. Tailwind 4.3.3 writes [@supports
   not (display:grid)], spacing a keyword the function-token spelling would hide
   from the [@supports] grammar, and passes [selector(...)] through. A condition
   the grammar has no production for is refused, not raised. *)
let test_supports_condition_text () =
  check_utilities "supports-[not(display:grid)]:flex"
    {|@supports not (display:grid){.supports-\[not\(display\:grid\)\]\:flex{display:flex}}|};
  check_utilities "supports-[not_(display:grid)]:flex"
    {|@supports not (display:grid){.supports-\[not_\(display\:grid\)\]\:flex{display:flex}}|};
  check_utilities "supports-[selector(:has(a))]:flex"
    {|@supports selector(:has(a)){.supports-\[selector\(\:has\(a\)\)\]\:flex{display:flex}}|};
  Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
    "supports-[display:)]:flex"

(* A class that parses but renders no rule at all is worse than one that is
   refused: a typo in a variant name stays silent. Tailwind emits nothing for
   these four either, so rejecting them moves no rendered output - it only turns
   silence into a message. [group-not-hover] is the shape: [rule.ml] already
   knows a group negation cannot wrap a media query and answers with no rules,
   so the parser should not have accepted it. *)
let test_silent_empty_variants_rejected () =
  let rejected =
    Test_helpers.check_invalid_class ~why:Test_helpers.Not_a_utility
  in
  rejected "group-not-hover:flex";
  rejected "group-not-device-hocus:flex";
  rejected "peer-not-hover:flex";
  (* The negations that do have a selector form still parse. *)
  check bool "group-not-checked still parses" true
    (Result.is_ok (Tw.of_string "group-not-checked:flex"));
  check bool "not-hover still parses" true
    (Result.is_ok (Tw.of_string "not-hover:flex"))

let tests =
  [
    test_case "arbitrary breakpoint rejects non-length" `Quick
      test_arbitrary_breakpoint_rejects_non_length;
    test_case "arbitrary breakpoint rejects OCaml literals" `Quick
      test_arbitrary_breakpoint_rejects_ocaml_literals;
    test_case "custom variant is theme-local" `Quick
      test_custom_variant_is_theme_local;
    test_case "container variant is theme-local" `Quick
      test_container_variant_is_theme_local;
    test_case "removed breakpoint drops its variants" `Quick
      test_removed_breakpoint_drops_its_variants;
    test_case "a removed single breakpoint drops its variants" `Quick
      test_removed_single_breakpoint_drops_its_variants;
    test_case "invalid bracket modifiers" `Quick test_invalid_bracket_modifiers;
    test_case "empty attribute brackets" `Quick test_empty_attribute_brackets;
    test_case "named attribute variants" `Quick test_named_attribute_variants;
    test_case "padded attribute brackets" `Quick test_padded_attribute_brackets;
    test_case "unbuildable attribute names" `Quick
      test_unbuildable_attribute_names;
    test_case "attribute brackets still parse" `Quick
      test_attribute_brackets_still_parse;
    test_case "data bracket operators" `Quick test_data_bracket_operators;
    test_case "bare selector variant attribute operators" `Quick
      test_bare_selector_variant_attribute_operators;
    test_case "arbitrary selector variants" `Quick
      test_arbitrary_selector_variants;
    test_case "in and not bracket selectors" `Quick
      test_in_and_not_bracket_selectors;
    test_case "scoped negated bracket nested" `Quick
      test_scoped_negated_bracket_nested;
    test_case "valid bracket modifiers" `Quick test_valid_bracket_modifiers;
    test_case "supports condition text" `Quick test_supports_condition_text;
    test_case "not-[selector] unreadable content rejected" `Quick
      test_not_bracket_unreadable_selector_rejected;
    test_case "not-data negates data" `Quick test_not_data_negates_data;
    test_case "not-nth negates nth" `Quick test_not_nth_negates_nth;
    test_case "unbuildable scoped selectors" `Quick
      test_unbuildable_scoped_selectors;
    test_case "silent empty variants rejected" `Quick
      test_silent_empty_variants_rejected;
  ]

let suite =
  ( "variant_parse",
    test_case "modifier reads one token" `Quick test_modifier_reads_one_token
    :: tests )
