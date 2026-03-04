let test_split_empty () =
  let props, rest = Tw.Property.split [] in
  Alcotest.(check int) "no props" 0 (List.length props);
  Alcotest.(check int) "no rest" 0 (List.length rest)

let test_dedup_empty () =
  let deduped = Tw.Property.dedup [] in
  Alcotest.(check int) "dedup empty" 0 (List.length deduped)

let test_initial_values_empty () =
  let vals = Tw.Property.initial_values [] in
  Alcotest.(check int) "no values" 0 (List.length vals)

let tests =
  Alcotest.
    [
      test_case "split empty" `Quick test_split_empty;
      test_case "dedup empty" `Quick test_dedup_empty;
      test_case "initial_values empty" `Quick test_initial_values_empty;
    ]

let suite = ("property", tests)
