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

(* Memoization table for numbered spacing variables *)
let spacing_var_cache : (int, Css.length Var.theme) Hashtbl.t =
  Hashtbl.create 64

(* Get or create a spacing variable for a specific multiplier. For example,
   get_spacing_var 4 returns --spacing-4 *)
let get_spacing_var n =
  match Hashtbl.find_opt spacing_var_cache n with
  | Some var -> var
  | None ->
      let name = "spacing-" ^ string_of_int n in
      (* Order: (3, 100+n) puts numbered spacing after base spacing (3, 0) *)
      let var = Var.theme Css.Length name ~order:(3, 100 + n) in
      Hashtbl.add spacing_var_cache n var;
      var

(* Calculate the concrete length value for a spacing multiplier *)
let spacing_value n : Css.length =
  (* Tailwind's default spacing is 0.25rem per unit *)
  Rem (float_of_int n *. 0.25)
