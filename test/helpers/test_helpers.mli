(** Test helper functions for CSS comparison and minimization *)

open Cascade

val check_class : string -> Tw.Utility.t -> unit
(** [check_class expected t] checks that the utility [t] produces the expected
    class name string. *)

val extract_utilities_layer_rules : Css.t -> Css.statement list
(** [extract_utilities_layer_rules css] extracts rules from the utilities layer.
*)

val extract_rule_selectors : Css.statement list -> string list
(** [extract_rule_selectors stmts] extracts selector strings from CSS rules. *)

val our_css : Tw.t list -> string
(** [our_css utilities] is tw's stylesheet for [utilities], base layer included
    and minified. *)

val require_tailwind_cli : unit -> unit
(** [require_tailwind_cli ()] returns when the pinned Tailwind CLI is usable.
    Otherwise it skips the calling test, or fails it when [TW_TAILWIND_TESTS=1]
    says the CLI is meant to be there: a parity test that stops asking the
    oracle reports an agreement it never checked. *)

val tailwind_css : ?forms:bool -> string list -> string
(** [tailwind_css classnames] is the pinned Tailwind CLI's stylesheet for the
    same classes. Goes through {!require_tailwind_cli}, so it skips the test
    when the CLI is unavailable and fails it under [TW_TAILWIND_TESTS=1]. *)

val properties_of_class : string -> Css.Declaration.prop_key list
(** [properties_of_class cls] is every property [cls] declares, custom
    properties included: two utilities can conflict on a [--tw-*] alone. *)

val interacting_pairs : string list -> (string * string) list
(** [interacting_pairs classes] pairs up the classes that write on each other.
    An element carrying such a pair is where an ordering difference becomes
    observable; a class on its own can only differ in value. *)

val ordering_diff : ?forms:bool -> Tw.t list -> Cascade_diff.Css_compare.t
(** [ordering_diff ?forms utilities] compares tw's sheet for [utilities] against
    the pinned Tailwind CLI's, canonically and with dead custom properties
    pruned. Both {!check_ordering_fails} and {!check_ordering_matches} go
    through it: the fuzzer minimises with the same predicate the suites assert
    on, so a case it reports as minimal is one the assertion also rejects.
    Pruning is what makes it blind to a utility whose only output is an
    unreferenced binding; {!check_rendering_matches} covers that class. *)

val dropped_declarations : Cascade_diff.Css_compare.t -> Error.t list
(** [dropped_declarations diff] is the parse warnings either side of [diff]
    carries, minus the one allowed exception. A warning means the reader
    rejected a declaration and dropped it from that side's AST before the
    comparison, so the diff read less than it appears to. *)

val check_no_dropped_declarations :
  test_name:string -> Cascade_diff.Css_compare.t -> unit
(** [check_no_dropped_declarations ~test_name diff] fails when either side of
    [diff] carries a parse warning, i.e. a declaration the reader rejected and
    dropped from that side's AST before the comparison. Such a drop makes the
    comparison read as a phantom addition on the side that parsed, or as no
    difference at all when both sides collapse to the same AST, so it is a
    finding rather than noise. Tailwind's bare-number [color-mix] mixing amount,
    which CSS Color 5 sec. 3.1 does not admit and a browser drops too, is the
    one allowed exception. *)

val check_ordering_fails : ?forms:bool -> Tw.t list -> bool
(** [check_ordering_fails ?forms utilities] is [true] when {!ordering_diff}
    finds a difference. The minimisation predicate. *)

val delta_debug : ('a list -> bool) -> 'a list -> 'a list
(** [delta_debug check_fails lst] uses delta debugging (ddmin algorithm) to
    minimize a failing test case. *)

val minimal_pair : ('a list -> bool) -> 'a list -> 'a list option
(** [minimal_pair check_fails lst] finds the first pair of elements that causes
    the check to fail. *)

val minimize_failing_case : ('a list -> bool) -> 'a list -> 'a list option
(** [minimize_failing_case check_fails initial] minimizes a failing test case to
    the smallest possible set using delta debugging. Returns [None] if the
    initial case doesn't fail. *)

val check_ordering_matches :
  ?forms:bool -> test_name:string -> Tw.t list -> unit
(** [check_ordering_matches ?forms ~test_name utilities] compares the ordering
    of utilities between our implementation and Tailwind CSS, failing the test
    if they differ. *)

val tree_diff_css :
  expected:string -> actual:string -> Cascade_diff.Css_compare.t
(** [tree_diff_css ~expected ~actual] compares two sheets structurally, in mode
    [`Tree] and with nothing pruned, after respelling both through cascade's
    printer.

    The respelling is what makes the mode usable against the real CLI. Cascade
    and Tailwind's minifier write the same value differently wherever CSS makes
    the difference insignificant - [oklch(63.7% .237 25.331)] against
    [oklch(63.7%.237 25.331)], a quoted font stack against an unquoted one - and
    a structural comparison reads a custom property's value as bytes, so those
    spellings read as differences. Parsing and re-printing puts both sheets in
    one spelling and changes nothing else: the printer merges no rules, moves
    none and drops no binding.

    Mode [`Canonical], which every other Tailwind oracle here uses, cannot
    report a rule written twice (its optimizer folds the copy away) nor a
    custom-property binding nothing reads (every caller prunes those off both
    sides). This is the mode that can. *)

val tree_diff : ?forms:bool -> Tw.t list -> Cascade_diff.Css_compare.t
(** [tree_diff ?forms utilities] is {!tree_diff_css} between the pinned Tailwind
    CLI's sheet for [utilities] and tw's, base layer included on both sides.
    Goes through {!require_tailwind_cli}. *)

val surplus : Cascade_diff.Css_compare.t -> string list
(** [surplus diff] is what tw's sheet carries that Tailwind's does not: a rule
    Tailwind has no counterpart for, and a declaration added to a rule both
    sheets write, each described by where it sits. A rule emitted twice appears
    as the second copy being added and a binding nothing references as the
    binding being added, so one list answers for both. Empty for any [diff] that
    is not a tree diff.

    A rule inside a container only one sheet has is not counted: an [@media] or
    [@container] block the two sheets spell differently comes and goes as a
    pair, and reading its rules as surplus would report the spelling rather than
    anything tw wrote twice. *)

val check_no_surplus : test_name:string -> Cascade_diff.Css_compare.t -> unit
(** [check_no_surplus ~test_name diff] fails when {!surplus} is non-empty,
    naming every rule and declaration tw writes and Tailwind does not. *)

val class_position : string -> string -> int option
(** [class_position sheet cls] is the byte offset in [sheet] where the rule for
    class [cls] starts, or [None] when the sheet declares no such class. The
    match ends where the class name ends, so [.bg-top] is not read off
    [.bg-top-left]; what follows it is not constrained, so a selector that
    continues past the class ([.divide-x>*], [.group:hover .x]) is found too.
    The oracle behind {!check_class_order}. *)

val check_class_order : ?forms:bool -> test_name:string -> string list -> unit
(** [check_class_order ?forms ~test_name classes] compares where each of
    [classes] lands in tw's sheet against where the pinned Tailwind CLI puts it.
    {!check_ordering_matches} pairs rules up by key, so a layer holding every
    expected rule in the wrong places still compares equal; reading the
    positions back is what catches a reorder. Skips when the CLI is unavailable.
*)

val layer_statement_keys : string -> layer:string -> string list
(** [layer_statement_keys sheet ~layer] is the sequence of top-level statements
    inside [sheet]'s [@layer layer] block, in the order the bytes carry, each
    keyed by its selector or its at-rule prelude. A selector list is expanded
    onto its branches, since a list is one statement but several rules as far as
    order goes. [sheet] is read as text, not parsed, so both a tw sheet and a
    Tailwind one go through the same reader; a sheet with no such layer gives
    the empty list. *)

type order_gap = {
  pairs : int;  (** structural identities occurring exactly once on each side *)
  moves : int;  (** the fewest of those that have to move *)
  moved : (string * int * int) list;
      (** each moved key with its rank among {!field-pairs} on Tailwind's side
          and on tw's *)
}
(** What separates two sheets' statement order, in one number. *)

val sheet_order_gap : layer:string -> tailwind:string -> tw:string -> order_gap
(** [sheet_order_gap ~layer ~tailwind ~tw] measures how far tw's statement order
    in [@layer layer] is from Tailwind's. At-rule identities include a
    fingerprint of their nested statement structure, so repeated media and
    supports preludes remain distinguishable. Only identities occurring exactly
    once on both sides are paired, so no pairing choice of the gate's own can
    move the number; over those, {!field-moves} is the count outside a longest
    subsequence, which is the fewest statements that have to move for the orders
    to agree.

    {!check_class_order} asks the same question of a handful of named classes
    and {!check_ordering_matches} cannot ask it at all, the canonical differ
    being blind to a cascade-neutral reorder by design. This is the whole-sheet
    form: it sees a family placed in the wrong band even when no two utilities
    it reorders share a property. *)

val sheets : ?forms:bool -> Tw.t list -> string * string
(** [sheets ?forms utilities] is the pinned Tailwind CLI's sheet for [utilities]
    and tw's, both minified. Generating the pair once is what lets {!sheet_diff}
    and {!inverted_pairs} read the same two sheets. *)

val sheet_diff : tailwind:string -> tw:string -> Cascade_diff.Css_compare.t
(** [sheet_diff ~tailwind ~tw] compares the two sheets canonically, with dead
    custom properties pruned, the way {!ordering_diff} does. Returning the
    comparison rather than a verdict is what lets a caller read its structure:
    the differ reports a reorder as a {!Cascade_diff.Tree_diff.Reordered} node
    whenever the two rules are cascade-significant, which a caller carrying a
    set of known misorderings has to be able to recognise. *)

val describe_diff : Cascade_diff.Css_compare.t -> string
(** [describe_diff diff] is [diff] rendered for a failure message. *)

val describe_dropped : Error.t list -> string
(** [describe_dropped dropped] says that the comparison read less than it
    appears to, and which declarations it could not read. *)

val inverted_pairs :
  tailwind:string -> tw:string -> string list -> (string * string) list
(** [inverted_pairs ~tailwind ~tw classes] is every pair of [classes] the two
    sheets put in opposite orders, each written [(a, b)] with [a] the one
    Tailwind emits first. Only a pair whose two rules sit under the same at-rule
    in the same layer on both sides is compared: the position of a [@media]
    block says where the block sits rather than where a utility inside it sorts,
    and tw emits one block per rule where Tailwind merges. A class naming
    several rules is taken at the first.

    A pair, not a count. Both sheets order their utilities by a key of their
    own, so two classes that disagree here disagree in every sheet holding both,
    whatever else was drawn - which is what lets a set of known disagreements be
    recorded and a new one still fail. {!sheet_order_gap} answers the
    whole-sheet question instead, and its count moves with what is in the sheet.
*)

val render_elements : string list -> string list
(** [render_elements classnames] is the element list {!check_rendering_matches}
    renders: each class on its own, then one element per {!interacting_pairs}
    pair, duplicates dropped and first occurrence kept. *)

val check_rendering_matches :
  ?forms:bool -> ?inner:string -> test_name:string -> Tw.t list -> unit
(** [check_rendering_matches ?forms ?inner ~test_name utilities] renders both
    sheets in headless Chromium and fails on any computed style that differs.
    Each class gets an element of its own, plus one per {!interacting_pairs}
    pair, which is where an ordering difference shows. Fails too when an element
    does not carry the classes it was given, since then the comparison is
    vacuous. Skips when node or Playwright is absent; [TW_BROWSER_TESTS=0] opts
    out where they are present.

    [inner] is markup put inside every element built, and every descendant of it
    is compared too. Without it the elements are bare, so a rule that only
    matches a child - which is most of what [@tailwindcss/typography] emits -
    has nothing to match and the comparison passes without reading it.

    Every node is read four times: itself, then its [::before], [::after] and
    [::marker]. A rule on a pseudo-element leaves the element's own computed
    style untouched, so prose's list bullets and everything the [before:] and
    [after:] variants write are invisible without it. *)

(** {1 CSS Test Helpers} *)

val selector_testable : Css.Selector.t Alcotest.testable
(** [selector_testable] is an Alcotest testable for CSS selectors, using
    structural equality and pretty-printing via [Css.Selector.to_string]. *)

val sort_selectors : Css.Selector.t list -> Css.Selector.t list
(** [sort_selectors sels] returns selectors sorted by their string
    representation. Useful for order-insensitive comparisons in tests. *)

val has_layer : string -> Css.t -> bool
(** [has_layer name css] checks if a layer with the given name exists in the
    stylesheet. *)

val vars_in_layer : string -> Css.t -> string list
(** [vars_in_layer layer_name css] gets all custom property names from a layer.
*)

val has_var_in_layer : string -> string -> Css.t -> bool
(** [has_var_in_layer var_name layer_name css] checks if a variable name exists
    in a layer. *)

val selectors_in_layer : string -> Css.t -> string list
(** [selectors_in_layer layer_name css] gets all selectors from a layer. *)

val has_selector_in_layer : string -> string -> Css.t -> bool
(** [has_selector_in_layer selector layer_name css] checks if a selector exists
    in a layer. *)

val media_conditions : Css.t -> string list
(** [media_conditions css] gets all media query conditions from stylesheet,
    recursively. *)

val has_media_condition : string -> Css.t -> bool
(** [has_media_condition condition css] checks if a specific media condition
    exists. *)

val selectors_in_media : condition:string -> Css.t -> string list
(** [selectors_in_media ~condition css] returns the selector strings contained
    within the media query that matches [condition]. Returns [[]] if not found
    or if the block has no rules. *)

val has_selector_in_media : condition:string -> selector:string -> Css.t -> bool
(** [has_selector_in_media ~condition ~selector css] checks whether [selector]
    appears inside the media query identified by [condition]. *)

val count_selector_in_media :
  condition:string -> selector:string -> Css.t -> int
(** [count_selector_in_media ~condition ~selector css] counts how many times
    [selector] appears inside the media query identified by [condition]. *)

val selectors_in_media_sel : condition:string -> Css.t -> Css.Selector.t list
(** [selectors_in_media_sel ~condition css] returns the raw selector ASTs inside
    the matching media query. *)

val has_selector_in_media_sel :
  condition:string -> selector:Css.Selector.t -> Css.t -> bool
(** [has_selector_in_media_sel ~condition ~selector css] checks for a selector
    using structural equality on the selector AST. *)

val count_selector_in_media_sel :
  condition:string -> selector:Css.Selector.t -> Css.t -> int
(** [count_selector_in_media_sel ~condition ~selector css] counts selectors by
    structural equality on the selector AST. *)

val inline_has_property : string -> string -> bool
(** [inline_has_property prop_name inline_style] checks if inline style contains
    a specific property. *)

val has_var_in_declarations : ?inline:bool -> Css.declaration list -> bool
(** [has_var_in_declarations ?inline decls] checks if declarations contain any
    var() references. *)

(** {1 Utility Generators} *)

val spacing_values : int list
(** [spacing_values] is the list of common spacing values used in Tailwind
    utilities. *)

val test_rng : Random.State.t
(** [test_rng] is the global RNG for randomized tests. Initialized with a random
    seed printed to stderr. Set [TEST_SEED] env var to replay a specific seed.
*)

val shuffle : 'a list -> 'a list
(** [shuffle lst] returns a shuffled copy of the list using Fisher-Yates
    algorithm with {!test_rng}. *)

(** {1 Generic Test Patterns} *)

module type Handler = sig
  type t

  val of_class : Tw.Scheme.t -> string -> (t, [ `Msg of string ]) result
  (** [of_class theme s] parses a class name. *)

  val to_class : t -> string
  (** [to_class v] converts to a class name. *)
end

val check_handler_roundtrip : (module Handler) -> string -> unit
(** [check_handler_roundtrip h class_name] tests that parsing with
    {!val-of_class} and converting back with {!val-to_class} round-trip
    correctly. *)

(** Why a class must not parse. Each constructor is a claim about Tailwind as
    well as about tw, so that {!check_negative_premises} can hold it to the
    pinned CLI. *)
type rejection =
  | Not_a_utility
      (** Tailwind emits nothing for the class either, so refusing it is the
          whole answer. *)
  | Another_handler
      (** A real utility a different tw handler owns: Tailwind emits for it and
          so does {!Tw.of_string}, and only the handler under test says no. *)
  | Diverges of string
      (** Tailwind emits for the class and {!Tw.of_string} does not, with the
          string saying why. Every one of these is a measured parity gap held
          open here rather than a rejection that reads as intended. *)

val check_invalid_input : ?why:rejection -> (module Handler) -> string -> unit
(** [check_invalid_input ?why h input] tests that parsing fails for invalid
    input as expected, and records [input] for {!check_negative_premises}. [why]
    defaults to {!constructor-Not_a_utility}; the half of it that needs no CLI,
    whether {!Tw.of_string} knows the class, is checked here. *)

val check_negative_premises : unit -> unit
(** [check_negative_premises ()] asks the pinned tailwindcss CLI whether every
    negative test that has run so far had its premise right, in one generation
    over the whole corpus. It skips without the CLI, and fails instead under
    [TW_TAILWIND_TESTS=1], the way every other parity check here does.

    It reads what the negative tests registered as they ran, so it belongs after
    them: see [test/test.ml]. *)

val standard :
  roundtrip:(unit -> unit) ->
  invalid:(unit -> unit) ->
  unit Alcotest.test_case list
(** [standard ~roundtrip ~invalid] creates the standard test case list with
    roundtrip and invalid test cases. *)

val check_parts : (module Handler) -> string list -> unit
(** [check_parts h parts] concatenates parts with "-" and tests roundtrip. *)

val check_invalid_parts :
  ?why:rejection -> (module Handler) -> string list -> unit
(** [check_invalid_parts ?why h parts] concatenates parts with "-" and tests
    that parsing fails, as {!check_invalid_input}. *)

val check_typed_class : string -> Tw.t -> unit
(** [check_typed_class cls value] checks that the typed constructor [value]
    pretty-prints to [cls] and that [cls] round-trips back through
    [Tw.of_string]. *)

val adversarial_payloads : string list
(** [adversarial_payloads] are bracket values chosen to break the two things a
    utility does with author text: respell it into the class name, and place it
    into a declaration. Numbers whose canonical spelling differs from the
    author's, text that ends the declaration or the rule, comments, quotes,
    placeholders no CSS grammar reads, and non-ASCII identifiers. *)

val arbitrary_families : string list
(** [arbitrary_families] is every class prefix that accepts [<prefix>-[value]],
    found by feeding a benign bracket value to each [val] exported by
    [lib/tw.mli] and the family modules it re-exports, and to each literal
    match-arm prefix in [lib/*.ml]. *)

type sweep_verdict =
  | Rejected  (** [of_string] refused the class: a legitimate outcome *)
  | Emitted_nothing  (** parsed, but contributed no rule *)
  | Matched  (** parsed, and every rule it emits is selected by the class *)
  | Mismatched of string
      (** parsed, and emitted a rule the class cannot match *)

val sweep_one : string -> sweep_verdict
(** [sweep_one cls] compiles [cls] and reports what came of it. It fails the
    test if [of_string] or [to_css] raises: an exception escaping is never a
    legitimate answer, whereas [Error] is. {!constructor-Mismatched} is the one
    verdict that is a bug - the class was accepted and named a rule it cannot
    select - and it covers both halves: no emitted selector carries the class,
    or {!Tw.pp} spells the class differently from the author. *)

val unescape_selector : string -> string
(** [unescape_selector s] undoes CSS Syntax 3 (ED) sec. 4.3.7 escaping, so a
    selector can be compared against the class text it was built from whichever
    spelling the printer chose. *)

val selectors_of_utility : Tw.t -> string list
(** [selectors_of_utility u] is every selector [u] emits, nested rules inside
    [\@media], [\@supports], [\@container] and [\@layer] included. *)
