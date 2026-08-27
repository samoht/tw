(** Reader for the generated upstream fixtures ([utilities.txt],
    [variants.txt]).

    The fixtures are one block per upstream test, separated by [<<<>>>]:

    {v
    # <test name>
    @config <theme config>
    @variant <matchVariant directive>     (optional, repeatable)
    @theme-var <name> <value>             (optional, repeatable)
    <space-separated class list>
    ---
    <expected CSS>
    v}

    Both the fixture replay in [test/upstream/test.ml] and the parse-parity gate
    in [test/test_tw.ml] read the same corpus, so they read it here: two readers
    drifting apart is two different corpora behind one set of fixtures. See
    [extract_tests.ml] for how the fixtures are generated. *)

type config =
  | Theme  (** [@theme { ... }] *)
  | Theme_inline  (** [@theme inline { ... }] *)
  | Theme_reference  (** [@theme reference { ... }] *)
  | Theme_inline_reference  (** [@theme inline reference { ... }] *)
  | No_theme  (** no [@theme] block *)
  | Run  (** the [run()] helper rather than [compileCss()] *)

val config_of_string : string -> config
(** [config_of_string s] reads an [@config] line's argument. An unknown name
    reads as {!constructor-No_theme}, the shape a fixture without a theme has.
*)

type case = {
  source : string;  (** Fixture the case was read from. *)
  name : string;
  config : config;
  classes : string list;
  expected : string;
  variants : string list;  (** [matchVariant] directive lines for this test. *)
  theme_vars : (string * string) list;
      (** [@theme] token overrides (name, value) captured from the test's CSS
          template by the extractor (e.g. text-shadow sizes Tailwind inlines).
      *)
}

val split_classes : string -> string list
(** [split_classes line] splits a class list on spaces, keeping a bracket value
    such as [data-[foo_=_bar]:flex] whole. *)

val read : string -> case list
(** [read path] reads every block of the fixture at [path]. A file that does not
    exist reads as no cases, which the caller's floor turns into a failure. *)

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
