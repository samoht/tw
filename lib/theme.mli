(** The theme a stylesheet is generated against.

    A theme is what a project declared in its [@theme] block, over Tailwind's
    own defaults: named colours, explicit steps of the spacing and radius
    scales, the widths a bare [border]/[ring]/[outline] takes, the responsive
    breakpoints, and raw token overrides. Utilities read it while generating
    CSS, so two stylesheets built from different [@theme] blocks are two values,
    not two states of one process. *)

open Cascade

(** How a theme spells a colour. A colour given as {!Hex} makes an opacity
    modifier fall back to hex-with-alpha; {!Oklch} makes it fall back to
    [color-mix]. *)
type color_value =
  | Hex of string  (** e.g. ["#ef4444"]. *)
  | Oklch of { l : float; c : float; h : float }
      (** e.g. [oklch(63.7% 0.237 25.331)]. *)

type t
(** The type for themes. *)

val default : t
(** [default] is Tailwind v4's own theme: oklch colours, a spacing scale
    computed from [--spacing] rather than tabulated, and 1px default widths. *)

val to_string : t -> string
(** [to_string t] is a one-line summary of [t], for diagnostics. *)

(** {1 Extending a theme}

    Each of these is a value on top of an existing theme; none mutates. *)

val with_colors : (string * color_value) list -> t -> t
(** [with_colors cs t] is [t] with the colours [cs] added, keyed by Tailwind
    name such as ["red-500"]. An entry replaces one [t] already had. *)

val with_spacing : (int * Css.length) list -> t -> t
(** [with_spacing ss t] is [t] with the spacing steps [ss] added, keyed by
    multiplier: step [4] is [--spacing-4]. A utility reads [var(--spacing-4)]
    for a step given here and [calc(var(--spacing) * 4)] for one that is not. *)

val with_radius : (string * Css.length) list -> t -> t
(** [with_radius rs t] is [t] with the radii [rs] added, keyed by name such as
    ["sm"] or ["full"]. A utility reads [var(--radius-sm)] for a radius given
    here and the raw value for one that is not. *)

val with_breakpoints : (string * float) list -> t -> t
(** [with_breakpoints bs t] is [t] with the breakpoints [bs] added, in px. A
    responsive variant on a breakpoint given here emits [(min-width: Xpx)]
    rather than the rem form. *)

val with_widths : ?border:int -> ?ring:int -> ?outline:int -> t -> t
(** [with_widths ?border ?ring ?outline t] is [t] with the widths a bare
    [border], [ring] or [outline] takes, in px. These are Tailwind's
    [--default-border-width] and its siblings. *)

val with_overrides : ?inline:string list -> t -> (string * string) list -> t
(** [with_overrides ?inline t overrides] is [t] with the raw token [overrides]
    applied on top of its own, keyed by variable name without the leading [--];
    a new entry wins. [inline] names the tokens that came from an
    [\@theme inline] block, which a utility inlines rather than referencing. *)

val with_static : t -> t
(** [with_static t] is [t] emitting every theme variable rather than only the
    ones a utility read, as [theme(static)] asks for. *)

(** {1 Reading a theme} *)

val color : t -> string -> color_value option
(** [color t name] is the colour [t] gives [name], and [None] when it gives
    none. *)

val hex_color : t -> string -> string option
(** [hex_color t name] is the hex spelling of [name] in [t], and [None] when [t]
    spells it in oklch or gives it no colour at all. *)

val is_hex_color : t -> string -> bool
(** [is_hex_color t name] is [hex_color t name <> None]. *)

val spacing : t -> int -> Css.length option
(** [spacing t n] is the length [t] gives step [n], and [None] when the step is
    computed from [--spacing] instead. *)

val has_explicit_spacing : t -> int -> bool
(** [has_explicit_spacing t n] is [spacing t n <> None]. *)

val radius : t -> string -> Css.length option
(** [radius t name] is the length [t] gives radius [name], and [None] when it
    gives none. *)

val has_explicit_radius : t -> string -> bool
(** [has_explicit_radius t name] is [radius t name <> None]. *)

val breakpoint : t -> string -> float option
(** [breakpoint t name] is the px width [t] gives breakpoint [name], and [None]
    when it gives none. *)

val breakpoints : t -> (string * float) list
(** [breakpoints t] is every breakpoint [t] gives, in declaration order. *)

val border_width : t -> int
(** [border_width t] is the px width a bare [border] takes under [t]. *)

val ring_width : t -> int
(** [ring_width t] is the px width a bare [ring] takes under [t]. *)

val outline_width : t -> int
(** [outline_width t] is the px width a bare [outline] takes under [t]. *)

val token : t -> string -> string option
(** [token t name] is the CSS [t] resolves the theme token [name] to: its own
    override if it has one, else the default the owning family published, else
    [None]. [name] carries no leading [--]. *)

val token_override : t -> string -> string option
(** [token_override t name] is the override [t] itself carries for [name],
    ignoring any published default. *)

val is_inline_token : t -> string -> bool
(** [is_inline_token t name] is whether [name] came from an [\@theme inline]
    block, so a utility reading it inlines the value rather than emitting
    [var(--name)]. *)

val inline_tokens : t -> string list
(** [inline_tokens t] is every token [t] marks inline. *)

val is_static : t -> bool
(** [is_static t] is whether [t] emits every theme variable rather than only the
    ones a utility read. *)

val theme_value : t option -> string -> string option
(** [theme_value t name] is [token_override] on [t], and [None] when no theme is
    threaded. *)
