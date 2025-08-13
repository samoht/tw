open Tw.Color

let test_color name hex expected_l expected_c expected_h =
  match hex_to_rgb hex with
  | Some rgb ->
      let oklch = rgb_to_oklch rgb in
      Printf.printf "%s (%s):\n" name hex;
      Printf.printf "  Our:      L=%.4f C=%.4f H=%.3f\n" oklch.l oklch.c oklch.h;
      Printf.printf "  Expected: L=%.1f C=%.3f H=%.3f\n" expected_l expected_c
        expected_h;
      Printf.printf "  Diff:     L=%.4f C=%.4f H=%.3f\n\n"
        (oklch.l -. expected_l) (oklch.c -. expected_c) (oklch.h -. expected_h)
  | None -> Printf.printf "Failed to parse %s\n" hex

let () =
  test_color "blue-500" "#3b82f6" 62.3 0.214 259.815;
  test_color "red-500" "#ef4444" 63.7 0.237 25.331;
  test_color "gray-500" "#6b7280" 55.1 0.027 264.364
