(** Where an entrypoint's candidates come from.

    [\@source "<path>"] adds files to scan and [\@source not "<path>"] takes
    them back out; [\@source inline("...")] safelists candidates outright and
    [\@source not inline("...")] blocks them. The [source()] option on the
    import moves or switches off the automatic detection. *)

val inline : string -> string list * string list
(** [inline css] is the candidates the [@source inline("...")] directives of
    [css] safelist, and those its [@source not inline("...")] directives block.
    Each argument is a space-separated list of patterns, expanded by
    {!Brace.expand}: [{hover:,}bg-red-{500,600}] is four candidates and
    [p-{0..8..4}] three. A blocked candidate is left out wherever it comes from,
    the markup included. *)

val paths : string -> string list * string list
(** [paths css] is the paths the [@source "<path>"] directives of [css] name for
    scanning, and those its [@source not "<path>"] directives take back out,
    each as written and in source order. A path is relative to the stylesheet
    and may be a glob; the [inline()] forms are {!inline}'s. *)

val root : string -> [ `Detect | `None | `Dir of string ]
(** [root css] is where the automatic source detection of [css] starts: [`None]
    when its [\@import] (or [\@tailwind utilities]) says [source(none)],
    [`Dir dir] when it says [source("dir")], a path relative to the stylesheet,
    and [`Detect] otherwise, which starts at the working directory. *)

val rooted : dir:string -> string -> string
(** [rooted ~dir css] is [css] with the relative path of each [\@import],
    [\@source], [\@plugin], [\@config] and [\@reference] resolved against [dir],
    the directory [css] was read from, so the text compiles from another
    directory. A path is relative when it starts [./] or [../]; a package name
    such as [tailwindcss] is left as written, and so is an
    [\@source inline(...)] argument, which names candidates. *)
