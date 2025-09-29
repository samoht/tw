(** String difference formatting. *)

type config = {
  max_width : int;
  short_threshold : int;
  show_caret : bool;
  indent : int;
}

let default_config =
  { max_width = 60; short_threshold = 30; show_caret = true; indent = 0 }

let first_diff_pos s1 s2 =
  let len1 = String.length s1 in
  let len2 = String.length s2 in
  let rec find i =
    if i >= len1 || i >= len2 then if len1 = len2 then None else Some i
    else if s1.[i] <> s2.[i] then Some i
    else find (i + 1)
  in
  find 0

let truncate_middle max_len s =
  let len = String.length s in
  if len <= max_len then s
  else
    let ellipsis_length = 3 in
    let half_len = (max_len - ellipsis_length) / 2 in
    let start_part = String.sub s 0 half_len in
    let end_part = String.sub s (len - half_len) half_len in
    start_part ^ "..." ^ end_part

type t =
  [ `Equal
  | `Diff_short of string * string
  | `Diff_medium of string * string * int
  | `Diff_long of string * string * int ]

let diff ?(config = default_config) ~expected actual =
  match first_diff_pos expected actual with
  | None -> `Equal
  | Some diff_pos ->
      let len1 = String.length expected in
      let len2 = String.length actual in

      if len1 <= config.short_threshold && len2 <= config.short_threshold then
        `Diff_short (expected, actual)
      else if len1 <= config.max_width && len2 <= config.max_width then
        `Diff_medium (expected, actual, diff_pos)
      else
        (* Calculate window centered on the diff position *)
        let half = config.max_width / 2 in
        let window_start = max 0 (diff_pos - half) in
        let window_end =
          min (max len1 len2) (window_start + config.max_width)
        in

        (* Helper to extract and format a window from a string *)
        let extract_window s len =
          if window_start >= len then ("...", 3)
          else
            let actual_end = min len window_end in
            let snippet =
              String.sub s window_start (actual_end - window_start)
            in
            let has_prefix = window_start > 0 in
            let has_suffix = window_end < len in
            let prefix = if has_prefix then "..." else "" in
            let suffix = if has_suffix then "..." else "" in
            let full_string = prefix ^ snippet ^ suffix in
            let prefix_len = if has_prefix then 3 else 0 in
            (full_string, prefix_len)
        in

        let s1_display, prefix_len1 = extract_window expected len1 in
        let s2_display, _prefix_len2 = extract_window actual len2 in

        (* Adjust caret position to account for window and prefix *)
        let adjusted_pos =
          if diff_pos < window_start then 0
          else if diff_pos >= window_end then String.length s1_display - 1
          else prefix_len1 + (diff_pos - window_start)
        in
        `Diff_long (s1_display, s2_display, adjusted_pos)

let pp_caret ?(indent = 0) fmt pos =
  Fmt.pf fmt "%s^@," (String.make (pos + indent) ' ')
