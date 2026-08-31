(** Reader for the upstream fixtures: the generated [utilities.txt] and
    [variants.txt], both generated.

    The fixtures are one block per test, separated by [<<<>>>]:

    {v
    # <test name>
    @config <theme config>
    @variant <matchVariant directive>     (optional, repeatable)
    @theme-var <name> <value>             (optional, repeatable)
    @theme-mode <name> <modifiers>        (optional, repeatable)
    @utility-def <name> <body>            (optional, repeatable)
    @layer-wrap <layer>                   (optional)
    <space-separated class list>
    ---
    <expected CSS>
    v}

    A generated fixture opens with a [#!] provenance banner naming the number of
    blocks the extractor wrote; {!blocks} and {!declared_blocks} read the two
    counts so a caller can hold one against the other. {!read} returns exactly
    one case per block or raises {!Malformed}: it never skips a block it cannot
    read.

    Both the fixture replay in [test/upstream/test.ml] and the parse-parity gate
    in [test/test_tw.ml] read the same corpus, so they read it here: two readers
    drifting apart is two different corpora behind one set of fixtures. See
    [extract_tests.ml] for how the fixtures are generated. *)

exception Malformed of string
(** Raised by {!read} on a line the fixture grammar has no place for, and by
    {!config_of_string} on an unknown [@config] name. Both fixtures are
    machine-written, so such a line means [extract_tests.ml] and this reader
    have drifted apart, or the fixture was edited by hand. The message names the
    file, the line and what was expected there. *)

type config =
  | Theme  (** [@theme { ... }] *)
  | Theme_inline  (** [@theme inline { ... }] *)
  | Theme_reference  (** [@theme reference { ... }] *)
  | Theme_inline_reference  (** [@theme inline reference { ... }] *)
  | No_theme  (** no [@theme] block *)
  | Run  (** the [run()] helper rather than [compileCss()] *)

val config_of_string : string -> config
(** [config_of_string s] reads an [@config] line's argument.

    @raise Malformed on a name the extractor cannot have written. *)

type case = {
  source : string;  (** Fixture the case was read from. *)
  name : string;
  config : config;
  classes : string list;
  expected : string option;
      (** The [---] section, or [None] for a block the extractor wrote without
          one: an upstream test that asserts the compile throws has no CSS to
          replay. *)
  variants : string list;  (** [matchVariant] directive lines for this test. *)
  theme_vars : (string * string) list;
      (** [@theme] token overrides (name, value) captured from the test's CSS
          template by the extractor (e.g. text-shadow sizes Tailwind inlines).
      *)
  theme_modes : (string * string list) list;
      (** The modifiers of the [@theme] block each token was declared in, for
          the tokens whose block had any. [inline] and [reference] change how a
          token reads, and one test can declare two tokens of a namespace in
          blocks that differ, so the mode belongs to the token. *)
  utility_defs : (string * string) list;
      (** [@utility] declarations (name, body) the test's CSS template makes. A
          case that has any is compiled through the declared-utility path rather
          than class by class. *)
  layer_wrap : string option;
      (** The layer the test's CSS template compiles [@tailwind utilities] into,
          when it names one. Tailwind puts the generated utilities in it and
          everything else beside it. *)
  layer_before_theme : bool;
      (** Whether the wrapped utilities precede a later [@theme] block in the
          source template. *)
}

val split_classes : string -> string list
(** [split_classes line] splits a class list on spaces, keeping a bracket value
    such as [data-[foo_=_bar]:flex] whole. *)

val read : string -> case list
(** [read path] reads every block of the fixture at [path], one case per block,
    so [List.length (read path)] is {!blocks}. A file that does not exist reads
    as no cases, which the caller's floor turns into a failure.

    @raise Malformed
      on any line the grammar above has no place for, rather than skipping the
      block it sits in. *)

val blocks : string -> int
(** [blocks path] is the number of blocks in the fixture at [path], counted on
    the [<<<>>>] separator {!read} splits on. A file that does not exist has
    none. *)

val declared_blocks : string -> int option
(** [declared_blocks path] is the block count the fixture's [#!] provenance
    banner declares, or [None] for a file without one: a hand-maintained
    fixture, or one that does not exist. A generated fixture whose {!blocks}
    disagrees with this has been edited since it was generated, and the edit is
    what the next regeneration would drop. *)

val path : string -> string option
(** [path basename] is where [basename] sits relative to the running test:
    beside the executable under the dune sandbox, or under [upstream/] or
    [test/upstream/] when the test runs from a parent directory. *)

val extract_root_vars : string -> (string * string) list
(** [extract_root_vars expected] is the [--name: value] pairs a fixture's
    expected CSS declares, so a case's theme tokens can be read back out of the
    output Tailwind produced for it. *)

val extract_var_fallbacks : string -> (string * string) list
(** [extract_var_fallbacks expected] is the [var(--name, fallback)] pairs in a
    fixture's expected CSS. A [@theme reference] block declares no [:root]
    value, so the fallback is the only place its token appears. *)

val is_runtime_var : string -> bool
(** [is_runtime_var name] is whether [name] is a [tw-*] variable, i.e. a
    utility's own output rather than a theme token the test declared. *)
