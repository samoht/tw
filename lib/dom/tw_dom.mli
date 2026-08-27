(** Client-side CSS injection for js_of_ocaml apps.

    Manages a single [<style>] element in the document head. Utilities are
    registered at runtime and their CSS is injected into the DOM. The style
    element grows monotonically — utilities are never removed.

    Usage with Helix:
    {[
    let init_tw_dom () = Tw_dom.init ()
    let card_class () = Tw_dom.use Tw.[ flex; p 4; bg blue; rounded_lg ]
    ]}

    Usage standalone:
    {[
    let app_class () = Tw_dom.use Tw.[ flex; p 4 ]
    ]} *)

val init : ?base:bool -> unit -> unit
(** [init ?base ()] initializes the style injection system. Creates a [<style>]
    element in the document head. If [base] is [true] (the default), the
    Tailwind base/preflight layer is included. Call once at application startup.
*)

val use : Tw.t list -> string
(** [use styles] registers the given utilities and returns a space-separated
    class name string. Already-registered utilities are not re-injected.

    Injecting the new rules is deferred to a microtask, so a pass of component
    mounts that each call [use] compiles the sheet once rather than once per
    mount. The browser drains microtasks before it paints, so the rules are in
    the document by the time anything is rendered; call {!flush} to inject them
    at once. *)

val flush : unit -> unit
(** [flush ()] injects any rules {!use} has registered but not yet written to
    the document, and does nothing when there are none. Only needed to read the
    style element back within the task that registered the utilities. *)

val use_str : string -> string
(** [use_str s] parses a space-separated Tailwind class string, registers the
    utilities, and returns the class name string. Raises [Invalid_argument] if
    any class is not recognized. *)

val css : unit -> string
(** [css ()] returns the current accumulated CSS as a string. *)
