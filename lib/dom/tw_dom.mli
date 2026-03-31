(** Client-side CSS injection for js_of_ocaml apps.

    Manages a single [<style>] element in the document head. Utilities are
    registered at runtime and their CSS is injected into the DOM. The style
    element grows monotonically — utilities are never removed.

    Usage with Helix:
    {[
    open Tw

    let () = Tw_dom.init ()

    (* Helper: compose with your framework's class attribute *)
    let tw styles = Html.class_name (Tw_dom.use styles)

    let card () =
      Html.div [ tw [ flex; p 4; bg blue; rounded_lg ] ] [ Html.text "Hello" ]
    ]}

    Usage standalone:
    {[
      let () = Tw_dom.init ()

      let el = Brr.Document.find_el_by_id (Brr.G.document) "app" in
      Brr.El.set_at "class" (Some (Tw_dom.use Tw.[flex; p 4])) el
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
