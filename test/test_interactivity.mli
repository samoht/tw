val all_utilities : unit -> Tw.t list
(** [all_utilities ()] returns all interactivity utilities for testing. *)

val suite : string * unit Alcotest.test_case list
(** [suite] test suite. *)
