(** Surface the cascade revision a local run is actually built against.

    The [cascade/] directory at the repo root is a symlink to a sibling checkout
    that other sessions move independently; CI has no such symlink and instead
    resolves the [cascade] dependency through opam against the version range
    [dune-project] pins. A local test run can therefore pass against code CI
    never sees. {!report} prints both sides to stdout so the mismatch is visible
    instead of silently green; when no [cascade/] checkout is found (as on CI,
    or when the repo root cannot be located from the process's working
    directory) it prints nothing. It never raises. *)

val report : unit -> unit
(** [report ()] prints the local [cascade/] checkout's git revision, the branch
    it sits on, how many of its commits are not in [origin/main] and whether it
    has uncommitted changes, next to the version range [dune-project] pins for
    the [cascade] dependency. It warns when the checkout is not sitting exactly
    on a release tag (the common case for a live sibling checkout, and the case
    where CI's opam-resolved cascade can diverge from what this run compiled
    against). The distance is the part that reads a failure for you: a suite
    failing against a branch nobody has merged is a question about cascade, not
    about tw. *)
