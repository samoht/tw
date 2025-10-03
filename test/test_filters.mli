val all_utilities : unit -> Tw.t list
(** [all_utilities ()] returns all filters utilities for testing. *)

val suite : string * unit Alcotest.test_case list
(** [suite] test suite. *)
