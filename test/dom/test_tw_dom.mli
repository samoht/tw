(** Tests for {!Tw_dom}. *)

val suite : string * unit Alcotest.test_case list
(** [suite] is the Alcotest suite for {!Tw_dom}. In Node.js it only includes
    cases that do not touch the DOM; in a browser it includes the full suite
    with live DOM injection. *)
