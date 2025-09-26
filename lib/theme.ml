(** Shared theme variables for consistent ordering and avoiding conflicts *)

(** Main ordering scheme: 1. Font families (1) - basic font families 2. Colors
    (2) - color variables 3. Spacing (3) - spacing variables 4. Breakpoints (4)
    \- breakpoint variables 5. Containers (5) - container variables 6.
    Typography (6) - text sizes and other typography 7. Border radius (7) -
    border radius variables 8. Animation/timing (8) - animation variables 9.
    Default fonts (9) - default font family variables *)

(** {1 Spacing Variables} *)

(* Shared spacing variable used across padding, margin, positioning, etc. *)
let spacing_var = Var.theme Css.Length "spacing" ~order:(3, 0)
