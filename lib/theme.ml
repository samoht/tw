(** Shared theme variables for consistent ordering and avoiding conflicts *)

module Css = Cascade.Css

(** Main ordering scheme: 1. Font families (1) - basic font families 2. Colors
    (2) - color variables 3. Spacing (3) - spacing variables 4. Breakpoints (4)
    \- breakpoint variables 5. Containers (5) - container variables 6.
    Typography (6) - text sizes and other typography 7. Border radius (7) -
    border radius variables 8. Animation/timing (8) - animation variables 9.
    Default fonts (9) - default font family variables *)

(** {1 Spacing Variables} *)

(* Resolve the optionally-threaded theme, defaulting to the base scheme. *)
let resolve_scheme = function Some s -> s | None -> Scheme.default

(* Shared spacing variable used across padding, margin, positioning, etc.
   Tailwind v4 uses a single --spacing: 0.25rem variable and calc() for
   values. *)
let spacing_var = Var.theme Css.Length "spacing" ~runtime:true ~order:(3, 0)

(* The base spacing value: 0.25rem *)
let spacing_base : Css.length = Rem 0.25

(* Publish the spacing step through the theme-token registry, the way rule.ml
   publishes the breakpoints, so [theme()] in a project's CSS resolves against
   the same value the utilities use. *)
let () =
  Scheme.register_default_token (Var.name spacing_var)
    (Css.Pp.to_string Css.pp_length spacing_base)

(* The spacing step times [n], rendered. Tailwind's v3 [spacing] and
   [lineHeight] scales are both that product, and v4 keeps no token per step, so
   a [theme(spacing.4)] has to be computed rather than looked up. *)
let spacing_times n =
  match spacing_base with
  | Css.Rem v -> Some (Css.Pp.to_string Css.pp_length (Css.Rem (v *. n)))
  | Css.Px v -> Some (Css.Pp.to_string Css.pp_length (Css.Px (v *. n)))
  | Css.Em v -> Some (Css.Pp.to_string Css.pp_length (Css.Em (v *. n)))
  | _ -> None

(* Create a spacing variable for explicit spacing values (e.g., --spacing-4) *)
let spacing_n_var n = Var.theme Css.Length ("spacing-" ^ Pp.int n) ~order:(3, n)

(* The length the theme binds to step [n] outright, as [--spacing-<n>]. Tailwind
   reads a bare step off that binding first and off the [--spacing] multiplier
   only when there is none. *)
let explicit_spacing scheme n =
  match Scheme.spacing scheme n with
  | Some _ as length -> length
  | None ->
      Option.bind (Scheme.token scheme ("spacing-" ^ Pp.int n)) Css.parse_length

(* Whether the bare step [n] of the spacing scale still resolves: [@theme {
   --spacing: initial }] removes the multiplier, and every step that relied on
   it stops being a utility. *)
let has_spacing_step ?theme n =
  let scheme = resolve_scheme theme in
  Float.is_integer n
  && explicit_spacing scheme (int_of_float (Float.abs n)) <> None
  || Scheme.token scheme (Var.name spacing_var) <> None

(* Create a spacing length value. When scheme has explicit spacing for |n|,
   returns var(--spacing-|n|) or calc(var(--spacing-|n|) * -1) for negatives.
   Otherwise returns calc(var(--spacing) * n). Returns the theme declaration and
   the length. *)
let spacing_calc ?theme n : Css.declaration * Css.length =
  let abs_n = abs n in
  let is_negative = n < 0 in
  match explicit_spacing (resolve_scheme theme) abs_n with
  | Some explicit_length ->
      (* Scheme has explicit spacing: use var(--spacing-|n|) *)
      let spacing_n = spacing_n_var abs_n in
      let decl, spacing_ref = Var.binding spacing_n explicit_length in
      if is_negative then
        (* Negative: wrap in calc(... * -1) *)
        let neg_len : Css.length =
          Css.Calc
            (Css.Calc.mul
               (Css.Calc.length (Css.Var spacing_ref))
               (Css.Calc.float (-1.0)))
        in
        (decl, neg_len)
      else (decl, (Css.Var spacing_ref : Css.length))
  | None ->
      (* Default: calc(var(--spacing) * n). For the unit multiplier we emit a
         bare var(--spacing) rather than calc(var(--spacing) * 1). This shortcut
         exists only to match Tailwind core byte-for-byte: the fixture has p-1
         producing "padding: var(--spacing)" next to p-4 producing
         "calc(var(--spacing) * 4)". Without it our output diverges and the
         examples/parity comparisons flag the difference.

         Runtime expectation: --spacing must resolve to a single length. That is
         the spacing-scale contract (the default 0.25rem and any single-value
         @theme override). The shortcut is NOT sound under a multi-term runtime
         redefinition such as ".dense { --spacing: 1px + 3px }": bare
         var(--spacing) then expands to invalid bare math and falls back to the
         initial value, whereas calc(var(--spacing) * 1) would still compute
         (4px). We inherit this fragility from Tailwind. cascade must not
         perform the equivalent calc(var(--spacing)) -> var(--spacing) rewrite,
         because it optimises arbitrary CSS and cannot assume that contract. *)
      let decl, spacing_ref = Var.binding spacing_var spacing_base in
      (* The zero step is a plain [0px], as Tailwind emits it: the scale factor
         makes [calc(var(--spacing) * 0)] zero for any spacing, and only the
         optimiser can see that once [--spacing] is a literal. *)
      if n = 0 then (decl, (Px 0. : Css.length))
      else if n = 1 then (decl, (Css.Var spacing_ref : Css.length))
      else
        let len : Css.length =
          Css.Calc
            (Css.Calc.mul
               (Css.Calc.length (Css.Var spacing_ref))
               (Css.Calc.float (float_of_int n)))
        in
        (decl, len)

(* Create a spacing length value for float multipliers like 2.5. For integer
   values, checks scheme for explicit spacing. Otherwise uses calc. This handles
   cases like my-2.5 which need calc(var(--spacing) * 2.5). *)
let spacing_calc_float ?theme (n : float) : Css.declaration * Css.length =
  let abs_n = Float.abs n in
  let is_negative = n < 0.0 in
  (* Check if this is an integer value that might have explicit spacing *)
  let is_integer = Float.is_integer abs_n in
  if is_integer then
    (* Use integer version which checks scheme *)
    spacing_calc ?theme (int_of_float n)
  else
    (* Fractional value: always use calc *)
    let decl, spacing_ref = Var.binding spacing_var spacing_base in
    let mult = if is_negative then -.abs_n else abs_n in
    let len : Css.length =
      Css.Calc
        (Css.Calc.mul
           (Css.Calc.length (Css.Var spacing_ref))
           (Css.Calc.float mult))
    in
    (decl, len)
