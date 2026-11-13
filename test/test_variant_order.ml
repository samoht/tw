open Alcotest

(* The variant cascade the one table defines, read as a ladder: every token
   sorts strictly after the one before it. A token the table has no position for
   returns 0, and the comparator reads 0 as "this rule carries no variant" and
   puts the rule in another group entirely, so a missing rung is not a small
   ordering error. *)
let variant_cascade_ladder =
  [
    "*";
    "**";
    "not-hover";
    "group-hover";
    "peer-focus";
    "first-letter";
    "marker";
    "selection";
    "file";
    "placeholder";
    "backdrop";
    "details-content";
    "before";
    "after";
    "first";
    "last";
    "only";
    "odd";
    "even";
    "first-of-type";
    "last-of-type";
    "only-of-type";
    "visited";
    "target";
    "open";
    "default";
    "checked";
    "indeterminate";
    "placeholder-shown";
    "autofill";
    "optional";
    "required";
    "valid";
    "invalid";
    "user-valid";
    "user-invalid";
    "in-range";
    "out-of-range";
    "read-only";
    "read-write";
    "empty";
    "focus-within";
    "hover";
    "focus";
    "focus-visible";
    "active";
    "enabled";
    "disabled";
    "inert";
    "in-focus";
    "has-checked";
    "aria-checked";
    "aria-[modal]";
    "data-active";
    "data-[open]";
    "nth-3";
    "nth-last-3";
    "nth-of-type-3";
    "nth-last-of-type-3";
    "hocus";
    "supports-grid";
    "motion-safe";
    "motion-reduce";
    "contrast-more";
    "contrast-less";
    "md";
    "@md";
    "portrait";
    "landscape";
    "ltr";
    "rtl";
    "dark";
    "starting";
    "print";
    "forced-colors";
    "inverted-colors";
    "pointer-fine";
    "any-pointer-fine";
    "noscript";
    "prose-h1";
    "[&>*]";
  ]

let test_variant_cascade_ladder () =
  List.iter
    (fun token ->
      check bool
        (token ^ " has a position in the cascade")
        true
        (Tw.Variant_order.of_prefix token > 0))
    variant_cascade_ladder;
  List.iteri
    (fun i token ->
      match List.nth_opt variant_cascade_ladder (i + 1) with
      | None -> ()
      | Some next ->
          check bool
            (token ^ " sorts before " ^ next)
            true
            (Tw.Variant_order.of_prefix token < Tw.Variant_order.of_prefix next))
    variant_cascade_ladder

(* Two tokens in the same position are separated by what they carry: a [not-]
   sorts where the variant it negates sorts, a [group-]/[peer-] where the state
   it wraps sorts. Both read the same table as the position itself. *)
let test_variant_inner_order () =
  List.iter
    (fun (token, inner) ->
      check int
        (token ^ " carries " ^ inner)
        (Tw.Variant_order.of_prefix inner)
        (Tw.Variant_order.inner_order token))
    [
      ("not-hover", "hover");
      ("not-md", "md");
      ("not-supports-grid", "supports-grid");
      ("group-focus", "focus");
      ("peer-checked", "checked");
    ];
  check int "a plain token carries nothing" 0
    (Tw.Variant_order.inner_order "hover")

let tests =
  [
    test_case "variant cascade ladder" `Quick test_variant_cascade_ladder;
    test_case "variant inner order" `Quick test_variant_inner_order;
  ]

let suite = ("variant_order", tests)
