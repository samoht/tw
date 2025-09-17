(** Concise diff formatting utilities

    Provides utilities to format string differences in a concise 2-line + caret
    format, extracted from test/css/test_css.ml for reuse across the codebase.
*)

(** Format a concise diff showing the first difference between two strings.
    Returns a formatted string with:
    - Original string snippet (up to 80 chars around difference)
    - New string snippet (same range)
    - Caret pointing to the difference position

    @param original the original/expected string
    @param actual the actual/generated string
    @return formatted diff string with newlines *)
let format_diff ~original ~actual =
  let len_a = String.length original in
  let len_b = String.length actual in
  let rec first_diff i =
    if i >= len_a || i >= len_b then i
    else if original.[i] <> actual.[i] then i
    else first_diff (i + 1)
  in
  let i = first_diff 0 in
  let start = max 0 (i - 40) in
  let take n s =
    let l = String.length s in
    let k = min n (l - start) in
    if k <= 0 then "" else String.sub s start k
  in
  let a_snip = take 80 original in
  let b_snip = take 80 actual in
  let caret_pos = i - start in
  let caret = String.make (max 0 caret_pos) ' ' ^ "^" in
  Fmt.str "- %S\n+ %S\n  %s" a_snip b_snip caret

(** Print a concise diff to stderr.
    @param original the original/expected string
    @param actual the actual/generated string *)
let eprintf_diff ~original ~actual =
  let diff = format_diff ~original ~actual in
  Fmt.epr "%s@." diff
