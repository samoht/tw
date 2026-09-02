(** Multicore regression tests for the caches used by dynamic constructors. *)

val suite : string * unit Alcotest.test_case list
(** Alcotest cases that exercise concurrent construction and parsing. *)
