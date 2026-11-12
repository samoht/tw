(** What an entrypoint asks of the [tailwindcss] package.

    [\@import "tailwindcss"] brings the package in, whole or in parts, with
    option functions CSS has no grammar for: [theme(static)], [prefix(tw)],
    [source(none)] and the bare word [important]. [\@tailwind utilities],
    [\@reference], [\@plugin] and [\@config] name the package too. Each is read
    off the source text, as cascade's parser drops what it cannot read. *)

(** {1 Parts} *)

(** The part of the generated sheet an import asks for. The package's
    [index.css] imports [theme.css], [preflight.css] and [utilities.css] in
    turn, so the bare import is the whole sheet. *)
type part = Whole | Theme_part | Preflight_part | Utilities_part

val part_of_url : string -> part option
(** [part_of_url url] is the part the import of [url] stands for: [tailwindcss]
    is {!Whole}, each of its three files its own part, and any other
    [tailwindcss/] subpath the package. A URL outside the package is [None]. *)

val is_package_url : string -> bool
(** [is_package_url url] is [true] when [part_of_url url] names a part. *)

val utilities_as_import : string -> string
(** [utilities_as_import css] is [css] with each [\@tailwind utilities] written
    as the import of [tailwindcss/utilities], which is all that file holds. *)

val parts : string -> part list
(** [parts css] is the parts the imports of [css] ask for, in source order. *)

val imports_preflight : string -> bool
(** [imports_preflight css] is [true] unless [css] imports Tailwind only in
    parts that leave the reset out: [tailwindcss/theme.css] and
    [tailwindcss/utilities.css] (or [\@tailwind utilities]) without
    [tailwindcss/preflight.css]. An entrypoint that does not import Tailwind at
    all is [true], as nothing it splices depends on it. *)

(** {1 Options} *)

val imports_static_theme : string -> bool
(** [imports_static_theme css] is [true] when [css] asks for the whole theme
    with [@import "tailwindcss" theme(static)], rather than only the variables
    its utilities read. *)

val import_prefix : string -> string option
(** [import_prefix css] is the name [prefix(tw)] on the [\@import] asks for, and
    [None] when the import carries no such option. Every candidate is then
    spelled [tw:p-4] and every theme token declared as [--tw-spacing]. *)

val imports_important : string -> bool
(** [imports_important css] is [true] when [css] imports the package with
    [@import "tailwindcss" important], which marks every declaration a utility
    emits [!important]. *)

val strip_options : string -> string
(** [strip_options css] drops the option functions Tailwind adds to [@import] -
    [theme(static)], [source(none)], [prefix(tw)] - which CSS has no grammar
    for. *)

(** {1 Other directives} *)

val references_tailwind : string -> bool
(** [references_tailwind css] is [true] when [css] says
    [\@reference "tailwindcss"], which brings the theme into scope for [\@apply]
    without emitting any of it. *)

val forms_base : string -> bool
(** [forms_base css] is [true] when [css] loads [@tailwindcss/forms] with its
    default strategy, which resets native form controls in the base layer, and
    [false] when it names no such plugin or its options ask for
    [strategy: "class"]. *)

val config_directives : string -> string list
(** [config_directives css] is what each [\@config] directive in [css] names, as
    written and in source order: a v3 JavaScript config, which tw does not
    evaluate. The CLI refuses an entrypoint for which this is not empty. *)
