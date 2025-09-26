# Reader atomic/lookahead review — findings and updates

This document tracks where `atomic`/`lookahead` are used, what we changed, and the next candidates to improve.

## Updates applied

- lib/css/reader.ml: enum_or_calls_impl
  - Fixed fallback consumption: if the probed name does not match a known call or ident, we now do NOT consume it before running the default parser. This unblocked shorthands (e.g., `background: red`) that rely on the default parser seeing the leading identifier.

- lib/css/reader.ml: take
  - The “too many values” check now uses non‑consuming `lookahead` to probe for an extra item, avoiding accidental input consumption when raising the error.

- lib/css/selector.ml: read_nth
  - Hardened numeric parsing using `int_of_string_opt` with `Reader.err` on failure. Prevents raw exceptions (e.g., `n+`) and provides consistent parse errors.

- lib/css/reader.ml: enum_calls_impl
  - Verified it already uses `would_start_identifier` and `lookahead ident` for dispatch; no change required.

## Current state by area

### selector.ml

- read_nth: Improved error handling; structure uses `enum` + `option` appropriately. Optional cleanups include replacing some manual `peek_string` checks with a small `lookahead` probe for clarity.
- read_ns, read_attr_flag: OK — both use `Reader.option` appropriately.
- read_nth_selector: OK — optional “of …” handled with `Reader.option`, which is atomic; explicit lookahead not required.

### properties.ml

- Shorthands (background, border, text‑decoration, …): Component readers are chosen with `Reader.one_of` (atomic per branch) and combined via `fold_many`. With the enum_or_calls fix, default parsers now see the correct input when a leading ident isn’t a mapped call/ident.

### values.ml

- read_color and variants: OK — leverages `one_of`/`enum_or_calls` as intended.
- read_calc and related: Still a candidate to add small `atomic` blocks around operator/operand parsing to make backtracking explicit and simplify error paths.

## Patterns and guidance

- Prefer `lookahead` over manual peeking when checking for a construct without consuming input.
- Wrap multi‑step parses that must be all‑or‑nothing in `atomic` to keep backtracking explicit and consistent.
- Keep existing “made no progress” guards in loops (`many`, `list_impl`) to prevent infinite loops when a parser returns without consuming.

## Completed improvements (2025-09-26)

1) selector.read_nth: Reviewed - already uses Reader.peek appropriately, no changes needed.
2) values.read_calc family: Added `atomic` blocks around operator/operand parsing in `read_calc_expr` and `read_calc_term` for clearer backtracking semantics.
3) Hardened numeric micro‑syntaxes: Converted `int_of_string` and `float_of_string` to `_opt` variants with proper error handling in reader.ml.

## Benefits (observed)

- Clearer intent and fewer subtle consumption bugs (e.g., background shorthand fallback).
- Better error surfaces for invalid micro‑syntax (e.g., nth).
- Non‑consuming checks avoid confusing state when reporting errors (e.g., take overflow).
