(** Comparison policy for tw against the Tailwind reference. *)

val diff :
  ?mode:Cascade_diff.Css_compare.mode ->
  string ->
  string ->
  Cascade_diff.Css_compare.t
(** [diff expected actual] preserves every declaration on both sides. In
    particular, an author custom property is observable outside the generated
    sheet and must not be pruned as dead. *)
