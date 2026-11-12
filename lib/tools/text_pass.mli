(** Text passes over an entrypoint's source.

    Tailwind's at-rules are not CSS, so they are found in the source text, at
    the offsets {!Cascade.Source_index} reads off cascade's component values,
    and a pass copies the author's own bytes around what it rewrites. A
    parse-and-reprint would lose the author's spelling. *)

(** The component values of a source text and the offsets each one spans. *)
module Components : sig
  val array : string -> Cascade.Component.t array
  (** [array css] is the top-level component values of [css], in source order.
  *)

  val start : Cascade.Component.t -> int
  (** [start c] is the offset [c] starts at. *)

  val stop : Cascade.Component.t -> int
  (** [stop c] is the offset just past [c]. *)

  val after_whitespace : Cascade.Component.t array -> int -> int
  (** [after_whitespace cs i] is the index of the first component from [i] on
      that is not whitespace, or the length of [cs]. *)

  val semicolon_from : Cascade.Component.t array -> int -> int
  (** [semicolon_from cs i] is the index of the first top-level semicolon from
      [i] on, or the length of [cs]. *)

  val punctuation : Cascade.Component.t list -> (int * int) list
  (** [punctuation cs] is the source ranges of the [;], [{] and [}] that
      separate declarations in [cs], nested curly blocks included, in source
      order. *)
end

val mentions : string -> string -> bool
(** [mentions name css] is [true] when [css] spells [name] anywhere. *)

val rewrite :
  names:string list ->
  string ->
  (Cascade.Source_index.t -> Buffer.t -> int -> int option) ->
  string
(** [rewrite ~names css step] is [css] copied with [step] rewriting it.
    [step index buf i] runs at each offset nothing has been copied from yet: it
    writes to [buf] what stands for the source at [i] and answers where to
    resume, or [None] to have the byte at [i] copied. [names] is what the pass
    matches; a source spelling none of them is handed back unindexed. *)

val strip_quotes : string -> string option
(** [strip_quotes s] is [s] without the matching quotes around it, and [None]
    when it has none. *)

val unquote : string -> string
(** [unquote s] is [s] unquoted when it is quoted, and [s] otherwise. *)

val quoted_contents : string -> string option
(** [quoted_contents body] is the text of the quoted argument [body], blanks
    around it ignored, and [None] when it is not quoted. *)
