open Tw.Color

let () =
  let blue_500 = { r = 59; g = 130; b = 246 } in
  let oklch = rgb_to_oklch blue_500 in
  Printf.printf "Blue-500 (#3b82f6):\n";
  Printf.printf "  RGB: { r = %d; g = %d; b = %d }\n" blue_500.r blue_500.g
    blue_500.b;
  Printf.printf "  OKLCH: { l = %.3f; c = %.3f; h = %.3f }\n" oklch.l oklch.c
    oklch.h;
  Printf.printf "  Expected: { l = 62.3; c = 0.214; h = 259.815 }\n";
  Printf.printf "  Diff: { l = %.3f; c = %.3f; h = %.3f }\n" (oklch.l -. 62.3)
    (oklch.c -. 0.214) (oklch.h -. 259.815)
