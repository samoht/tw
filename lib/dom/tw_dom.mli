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
    class name string. New CSS rules are injected into the DOM immediately.
    Already-registered utilities are not re-injected. *)

val use_str : string -> string
(** [use_str s] parses a space-separated Tailwind class string, registers the
    utilities, and returns the class name string. Raises [Invalid_argument] if
    any class is not recognized. *)

val css : unit -> string
(** [css ()] returns the current accumulated CSS as a string. *)
