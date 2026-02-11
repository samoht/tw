(** Shared theme variables for consistent ordering and avoiding conflicts *)

(** Main ordering scheme: 1. Font families (1) - basic font families 2. Colors
    (2) - color variables 3. Spacing (3) - spacing variables 4. Breakpoints (4)
    \- breakpoint variables 5. Containers (5) - container variables 6.
    Typography (6) - text sizes and other typography 7. Border radius (7) -
    border radius variables 8. Animation/timing (8) - animation variables 9.
    Default fonts (9) - default font family variables *)

(** {1 Spacing Variables} *)

(* Shared spacing variable used across padding, margin, positioning, etc.
   Tailwind v4 uses a single --spacing: 0.25rem variable and calc() for
   values. *)
let spacing_var = Var.theme Css.Length "spacing" ~order:(3, 0)

(* The base spacing value: 0.25rem *)
let spacing_base : Css.length = Rem 0.25

(* Create a spacing length value using calc(var(--spacing) * n). Returns the
   theme declaration and the calculated length. *)
let spacing_calc n : Css.declaration * Css.length =
  let decl, spacing_ref = Var.binding spacing_var spacing_base in
  let len : Css.length =
    (* calc(var(--spacing) * n) for all values including 0 and negative *)
    Css.Calc
      (Css.Calc.mul
         (Css.Calc.length (Css.Var spacing_ref))
         (Css.Calc.float (float_of_int n)))
  in
  (decl, len)
