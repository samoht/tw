open Alcotest
open Test_helpers

let check = check_handler_roundtrip (module Tw.Margin.Handler)

let of_string_valid () =
  check [ "m"; "0" ];
  check [ "m"; "1" ];
  check [ "m"; "4" ];
  check [ "m"; "px" ];
  check [ "m"; "0.5" ];
  check [ "m"; "1.5" ];
  check [ "m"; "auto" ];
  check [ "-m"; "1" ];
  check [ "-m"; "4" ];

  check [ "mx"; "auto" ];
  check [ "mx"; "0" ];
  check [ "mx"; "4" ];
  check [ "-mx"; "2" ];
  check [ "-mx"; "4" ];
  check [ "my"; "8" ];
  check [ "my"; "2" ];
  check [ "-my"; "2" ];
  check [ "-my"; "8" ];

  check [ "mt"; "auto" ];
  check [ "mt"; "0" ];
  check [ "mt"; "2" ];
  check [ "mr"; "0" ];
  check [ "mr"; "4" ];
  check [ "mb"; "6" ];
  check [ "ml"; "8" ];
  check [ "-mt"; "2" ];
  check [ "-mr"; "4" ];
  check [ "-mb"; "6" ];
  check [ "-ml"; "8" ]

let of_string_invalid () =
  let fail_maybe = check_invalid_input (module Tw.Margin.Handler) in

  fail_maybe [ "m" ];
  (* Missing value *)
  fail_maybe [ "m"; "invalid" ]
(* Invalid value *)

let suborder_matches_tailwind () =
  let open Tw in
  let spacing_values = [ 0; 1; 2; 3; 4; 6; 8; 12 ] in

  (* Generate all margin utilities: all axes × all spacing values *)
  let margin_utils =
    List.concat_map
      (fun n -> [ m n; mx n; my n; mt n; mb n; ml n; mr n ])
      spacing_values
  in

  (* Shuffle utilities to test ordering, not insertion order *)
  let shuffled =
    let arr = Array.of_list margin_utils in
    let n = Array.length arr in
    for i = n - 1 downto 1 do
      let j = Random.int (i + 1) in
      let temp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- temp
    done;
    Array.to_list arr
  in

  Test_helpers.check_ordering_matches
    ~test_name:"margin suborder matches Tailwind" shuffled

let tests =
  [
    test_case "margin of_string - valid values" `Quick of_string_valid;
    test_case "margin of_string - invalid values" `Quick of_string_invalid;
    test_case "margin suborder matches Tailwind" `Slow suborder_matches_tailwind;
  ]

let suite = ("margin", tests)
