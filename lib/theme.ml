module Css = Cascade.Css

(* Publish a theme variable's built-in default through the token registry. The
   text comes from the binding the utility itself emits, so [theme(--x)] in a
   class, [theme()] in a project's CSS and [theme(static)] all read exactly what
   the theme layer would declare for the token. A registry entry is a token
   stream rather than a typed value, and the printer leaves its numbers as
   written, so the shortest spelling is the one to store: it is what the sheet
   carries in either output mode. *)
let register_default var value =
  let decl = Var.set var value in
  Scheme.register_default_token (Var.name var)
    (Css.declaration_value ~minify:true decl)

(* Tailwind v4 has a single [--spacing: 0.25rem] token, and a step is a calc()
   over it. *)
let spacing_var = Var.theme Css.Length "spacing" ~runtime:true ~order:(3, 0)
let spacing_base : Css.length = Rem 0.25

(* Publish the spacing step through the theme-token registry, the way rule.ml
   publishes the breakpoints, so [theme()] in a project's CSS resolves against
   the same value the utilities use. *)
let () = register_default spacing_var spacing_base

(* The spacing step times [n], rendered. Tailwind's v3 [spacing] and
   [lineHeight] scales are both that product, and v4 keeps no token per step, so
   a [theme(spacing.4)] has to be computed rather than looked up. *)
let spacing_times n =
  match spacing_base with
  | Css.Rem v -> Some (Css.Pp.to_string Css.pp_length (Css.Rem (v *. n)))
  | Css.Px v -> Some (Css.Pp.to_string Css.pp_length (Css.Px (v *. n)))
  | Css.Em v -> Some (Css.Pp.to_string Css.pp_length (Css.Em (v *. n)))
  | _ -> None

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
  let scheme = Scheme.or_default theme in
  Float.is_integer n
  && explicit_spacing scheme (int_of_float (Float.abs n)) <> None
  || Scheme.token scheme (Var.name spacing_var) <> None

(* The step the scheme binds outright, as var(--spacing-|n|) with a negative
   multiplier folded into calc(... * -1). None when the scheme binds no such
   step, or when the step is not a whole one: only whole steps get a token. *)
let explicit_spacing_length ?theme (n : float) =
  let abs_n = Float.abs n in
  if not (Float.is_integer abs_n) then None
  else
    let abs_n = int_of_float abs_n in
    match explicit_spacing (Scheme.or_default theme) abs_n with
    | None -> None
    | Some explicit_length ->
        let spacing_n = spacing_n_var abs_n in
        let decl, spacing_ref = Var.binding spacing_n explicit_length in
        if n < 0.0 then
          let neg_len : Css.length =
            Css.Calc
              (Css.Calc.mul
                 (Css.Calc.length (Css.Var spacing_ref))
                 (Css.Calc.float (-1.0)))
          in
          Some (decl, neg_len)
        else Some (decl, (Css.Var spacing_ref : Css.length))

(* The step itself: the [var(--spacing)] reference, or the value the project
   gave the token when it declared it in an [@theme inline] block, which has no
   declaration of its own for a reference to read. The declaration carried
   beside it is the dependency carrier the utility attaches either way; an
   unreferenced runtime one is dropped at the sheet. *)
let spacing_step ?theme () : Css.declaration * Css.length =
  let decl, spacing_ref = Var.binding spacing_var spacing_base in
  let inline =
    match theme with
    | Some t when Scheme.is_inline_token t (Var.name spacing_var) ->
        Option.bind
          (Scheme.theme_value theme (Var.name spacing_var))
          Css.parse_length
    | Some _ | None -> None
  in
  match inline with
  | Some length -> (decl, length)
  | None -> (decl, (Css.Var spacing_ref : Css.length))

(* The spacing step times [n], written out as calc(var(--spacing) * n), or
   calc(.25rem * n) over an inline token. The product is what a utility emits
   when the scheme binds no token for the step and the multiplier is not one
   Tailwind folds. *)
let spacing_product ?theme (n : float) : Css.declaration * Css.length =
  match explicit_spacing_length ?theme n with
  | Some result -> result
  | None ->
      let decl, step = spacing_step ?theme () in
      let len : Css.length =
        Css.Calc (Css.Calc.mul (Css.Calc.length step) (Css.Calc.float n))
      in
      (decl, len)

let spacing_calc ?theme n : Css.declaration * Css.length =
  match explicit_spacing_length ?theme (float_of_int n) with
  | Some result -> result
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
         because it optimises arbitrary CSS and cannot assume that contract.

         The zero step is a plain [0px], as Tailwind emits it: the scale factor
         makes [calc(var(--spacing) * 0)] zero for any spacing, and only the
         optimiser can see that once [--spacing] is a literal. *)
      if n <> 0 && n <> 1 then spacing_product ?theme (float_of_int n)
      else
        let decl, step = spacing_step ?theme () in
        if n = 0 then (decl, (Px 0. : Css.length)) else (decl, step)

let spacing_calc_float ?theme (n : float) : Css.declaration * Css.length =
  (* Only an integer step can be bound outright by the scheme, and only an
     integer step folds. *)
  if Float.is_integer n then spacing_calc ?theme (int_of_float n)
  else spacing_product ?theme n
