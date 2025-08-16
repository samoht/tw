(** Variable tracking for CSS composition groups *)

type tally
(** The tally of variable usage *)

val empty : tally
(** Empty tally *)

val analyze_properties : Css.property list -> tally
(** Analyze CSS properties to track variable assignments and references *)

val generate_properties_layer : tally -> (string * string) list
(** Generate properties layer initializers for groups that need them *)
