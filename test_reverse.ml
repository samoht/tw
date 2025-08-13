open Tw.Color

let () =
  (* Tailwind's exact OKLCH for blue-500 *)
  let oklch = { l = 62.3; c = 0.214; h = 259.815 } in
  let rgb = oklch_to_rgb oklch in
  Printf.printf "OKLCH(62.3%% 0.214 259.815) -> RGB(%d, %d, %d)\n" rgb.r rgb.g
    rgb.b;
  Printf.printf "Expected: RGB(59, 130, 246) [#3b82f6]\n";

  (* Now convert back *)
  let oklch2 = rgb_to_oklch rgb in
  Printf.printf "RGB(%d, %d, %d) -> OKLCH(%.1f%% %.3f %.3f)\n" rgb.r rgb.g rgb.b
    oklch2.l oklch2.c oklch2.h
