(** Comparison policy for tw against the Tailwind reference. *)

val diff :
  ?mode:Cascade_diff.Css_compare.mode ->
  string ->
  string ->
  Cascade_diff.Css_compare.t
(** [diff expected actual] preserves every declaration on both sides. In
    particular, an author custom property is observable outside the generated
    sheet and must not be pruned as dead. *)

val uncovered : html:string -> string list -> string list
(** [uncovered ~html classes] is the [classes] the document [html] names
    nowhere, in the order given: a class no element carries is compared on no
    element by a browser run. The document is read the way sources are scanned,
    so a class written in the document's text counts as carried. *)

val browser :
  html:string ->
  classes:string list ->
  tailwind:string ->
  tw:string ->
  (Browser_compare.t, string) result
(** [browser ~html ~classes ~tailwind ~tw] renders [html] under Tailwind's sheet
    and under tw's with {!Browser_compare.run}, the first named [Tailwind] and
    the second [tw]. It is [Error] when a class of [classes] is {!uncovered},
    before any browser runs, and when {!Browser_compare.run} is. *)
