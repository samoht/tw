open Alcotest

let test_version () =
  let v = Version.version in
  check bool "version not empty" true (String.length v > 0)

let test_header () =
  let h = Version.header in
  check bool "header contains tw" true (String.contains_string h "tw");
  check bool "header contains MIT" true (String.contains_string h "MIT");
  check bool "header is CSS comment" true
    (String.starts_with ~prefix:"/*!" h && String.ends_with ~suffix:"*/" h)

let tests =
  [
    test_case "version string" `Quick test_version;
    test_case "header format" `Quick test_header;
  ]

let suite = ("version", tests)
