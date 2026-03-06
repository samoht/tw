(** Arbitrary property utilities: [[property:value]] with optional [/opacity].

    Parses class names like [[color:red]], [[background:blue]/50], and
    [[border-color:oklch(0.5_0.2_250)]/[var(--x)]] into typed CSS declarations
    with proper color-mix support for opacity modifiers. *)

open Utility
module Handler : Handler
