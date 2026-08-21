(** Sizing utilities for width and height *)

module Css = Cascade.Css

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

module Handler = struct
  let class_float = Pp.float

  open Style
  open Css

  (** The property a sizing family sets. [Width_and_height] is [size-*], which
      sets both. *)
  type prop =
    | Width
    | Height
    | Min_width
    | Min_height
    | Max_width
    | Max_height
    | Width_and_height
    | Inline_size
    | Min_inline_size
    | Max_inline_size
    | Block_size
    | Min_block_size
    | Max_block_size

  (** The value a sizing class carries. [Keyword] and [Themed] come from the
      family's own table, so they carry their class spelling and their suborder
      with them; a [Themed] value also carries the theme-variable declaration
      and the [var()] reference the utility emits. *)
  type value =
    | Keyword of { spelling : string; length : length; order : int }
    | Themed of {
        spelling : string;
        decl : declaration;
        value : length;
        order : int;
      }
    | Spacing of float
    | Fraction of string
    | Arbitrary of string * length

  type t =
    | Sized of prop * value
    | Aspect_auto
    | Aspect_square
    | Aspect_video
    | Aspect_ratio of float * float (* aspect-4/3, aspect-8.5/11 *)
    | Aspect_bracket of float * float (* aspect-[10/9] *)
    | Aspect_bracket_num of string (* aspect-[1.333] single number *)

  type Utility.base += Self of t

  let name = "sizing"

  (* Tailwind spacing order helper: matches canonical spacing scale order. n is
     the rem value, so we convert to class units (multiply by 4). *)
  let spacing_suborder n =
    let class_units = n *. 4. in
    let integer = int_of_float (floor class_units) in
    let frac = class_units -. float_of_int integer in
    (* Fractional part order: .0, .5, .25, .75 (not numeric!) *)
    let frac_order =
      if frac = 0.0 then 0
      else if abs_float (frac -. 0.5) < 0.01 then 1
      else if abs_float (frac -. 0.25) < 0.01 then 2
      else if abs_float (frac -. 0.75) < 0.01 then 3
      else 4 (* fallback for other values *)
    in
    (integer * 4) + frac_order

  (* Tailwind interleaves spacing values and fractions within a sizing family by
     the integer part of their magnitude (spacing value or fraction numerator):
     e.g. w-0.5, w-1, w-1.5, w-1/2, w-1/3, w-2, w-2/3, w-3/4. Spacing sorts by
     value, fractions of numerator n come just after the spacing values with
     that integer part, ordered by denominator. Both stay well under the
     per-family arbitrary offset (5_000_000). *)
  let spacing_value_order n = spacing_suborder n * 100

  (* A spacing value is stored as rem (class number * 0.25). A fraction n/m has
     numerator [n], whose class number is [n], so [spacing_suborder (n *. 0.25)]
     puts it on the spacing scale. [+ 4] steps to the next class boundary
     (spacing_suborder for an integer class k is 4k); [- 50 + m] pulls it just
     before that boundary, after every floor-n spacing value, by denominator. *)
  let fraction_value_order f =
    match String.split_on_char '/' f with
    | [ n; m ] -> (
        match (int_of_string_opt n, int_of_string_opt m) with
        | Some n, Some m ->
            ((spacing_suborder (float_of_int n *. 0.25) + 4) * 100) - 50 + m
        | _ -> 490000)
    | _ -> 490000

  (* Within a family: spacing and fractions interleaved by magnitude, then an
     arbitrary value, then the keywords. *)
  let arbitrary_off = 5_000_000
  let keyword_off = 6_000_000

  (* Family bases are 10M apart so the interleaved spacing/fraction range (< 5M)
     and the arbitrary/keyword offsets never overflow into the next family. *)
  (* size-* (width+height) sorts first in Tailwind, before h/w/max/min. *)
  let size_base = 0
  let h_base = 10_000_000
  let max_h_base = 20_000_000
  let min_h_base = 30_000_000
  let w_base = 40_000_000
  let max_w_base = 50_000_000
  let min_w_base = 60_000_000
  let aspect_base = 70_000_000

  (* Tailwind registers the logical sizing utilities after every other one, so
     they sort last rather than beside w-* and h-*; [logical_priority] carries
     that. Their bases run in the alphabetical order Tailwind falls back to. *)
  let logical = 80_000_000
  let block_base = logical
  let inline_base = 90_000_000
  let max_block_base = 100_000_000
  let max_inline_base = 110_000_000
  let min_block_base = 120_000_000
  let min_inline_base = 130_000_000
  let logical_priority = 38

  (* A Tailwind sizing fraction [n/m] resolves to [n/m * 100%]. Tailwind emits
     calc(n / m * 100%) and folds it to 6 significant figures (e.g. 33.3333,
     8.33333); we emit the percentage rounded the same way so the two match. *)
  let fraction_pct frac =
    match String.split_on_char '/' frac with
    | [ n; m ] -> (
        match (int_of_string_opt n, int_of_string_opt m) with
        (* Any denominator: Tailwind reads [w-<number>/<number>] as a
           percentage, not from a fixed scale. *)
        | Some n, Some m when m > 0 && n > 0 && n < m ->
            let pct = float_of_int n /. float_of_int m *. 100. in
            let digits = 6. -. Float.ceil (Float.log10 pct) in
            let factor = 10. ** digits in
            Some (Float.round (pct *. factor) /. factor)
        | _ -> None)
    | _ -> None

  (* Container size theme variables - ordered from smallest to largest *)
  let container_3xs = Var.theme Css.Length "container-3xs" ~order:(5, 0)
  let container_2xs = Var.theme Css.Length "container-2xs" ~order:(5, 1)
  let container_xs = Var.theme Css.Length "container-xs" ~order:(5, 2)
  let container_sm = Var.theme Css.Length "container-sm" ~order:(5, 3)
  let container_md = Var.theme Css.Length "container-md" ~order:(5, 4)
  let container_lg = Var.theme Css.Length "container-lg" ~order:(5, 5)
  let container_xl = Var.theme Css.Length "container-xl" ~order:(5, 6)
  let container_2xl = Var.theme Css.Length "container-2xl" ~order:(5, 7)
  let container_3xl = Var.theme Css.Length "container-3xl" ~order:(5, 8)
  let container_4xl = Var.theme Css.Length "container-4xl" ~order:(5, 9)
  let container_5xl = Var.theme Css.Length "container-5xl" ~order:(5, 10)
  let container_6xl = Var.theme Css.Length "container-6xl" ~order:(5, 11)
  let container_7xl = Var.theme Css.Length "container-7xl" ~order:(5, 12)

  (* The container scale doubles as the named width scale in v4. This is the
     only place its names, theme variables, defaults and order live. *)
  let container_scale =
    [
      ("3xs", container_3xs, (Rem 16.0 : length));
      ("2xs", container_2xs, Rem 18.0);
      ("xs", container_xs, Rem 20.0);
      ("sm", container_sm, Rem 24.0);
      ("md", container_md, Rem 28.0);
      ("lg", container_lg, Rem 32.0);
      ("xl", container_xl, Rem 36.0);
      ("2xl", container_2xl, Rem 42.0);
      ("3xl", container_3xl, Rem 48.0);
      ("4xl", container_4xl, Rem 56.0);
      ("5xl", container_5xl, Rem 64.0);
      ("6xl", container_6xl, Rem 72.0);
      ("7xl", container_7xl, Rem 80.0);
    ]

  let container_binding name =
    List.find_map
      (fun (n, var, default) -> if n = name then Some (var, default) else None)
      container_scale

  (* Breakpoint theme vars, referenced by the (v3) max-w-screen-* utilities.
     Negative suborders keep them before --container-* in the theme layer, as
     Tailwind emits them. *)
  let breakpoint_sm = Var.theme Css.Length "breakpoint-sm" ~order:(5, -5)
  let breakpoint_md = Var.theme Css.Length "breakpoint-md" ~order:(5, -4)
  let breakpoint_lg = Var.theme Css.Length "breakpoint-lg" ~order:(5, -3)
  let breakpoint_xl = Var.theme Css.Length "breakpoint-xl" ~order:(5, -2)
  let breakpoint_2xl = Var.theme Css.Length "breakpoint-2xl" ~order:(5, -1)

  (* Table entries: [kw] resolves to a literal length, [themed] to a theme
     variable carrying its default. *)
  let kw spelling length order = Keyword { spelling; length; order }

  let themed spelling var default order =
    let decl, ref_ = Var.binding var default in
    Themed { spelling; decl; value = Var ref_; order }

  (* The whole container scale, in scale order, from suborder offset [off]. *)
  let container_entries off =
    List.mapi
      (fun i (spelling, var, default) -> themed spelling var default (off + i))
      container_scale

  (* The container scale in a bespoke suborder, given as name/offset pairs. *)
  let container_entries_by offs =
    List.filter_map
      (fun (spelling, var, default) ->
        Option.map
          (fun off -> themed spelling var default off)
          (List.assoc_opt spelling offs))
      container_scale

  type family = {
    prefix : string;  (** class prefix, e.g. ["min-w"] *)
    css_name : string;  (** property name, as error messages spell it *)
    decls : (length -> declaration) list;
    base : int;  (** suborder base for the family *)
    spacing_off : int;  (** extra offset in front of the spacing band *)
    entries : value list;  (** the keywords this family admits *)
  }

  let width_family =
    {
      prefix = "w";
      css_name = "width";
      decls = [ width ];
      base = w_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "dvw" (Dvw 100.) (keyword_off + 1);
          kw "dvh" (Dvh 100.) (keyword_off + 1);
          kw "lvh" (Lvh 100.) (keyword_off + 1);
          kw "svh" (Svh 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.0) (keyword_off + 3);
          kw "lvw" (Lvw 100.) (keyword_off + 4);
          kw "max" Max_content (keyword_off + 5);
          kw "min" Min_content (keyword_off + 6);
          kw "px" (Px 1.0) (keyword_off + 7);
          kw "screen" (Vw 100.0) (keyword_off + 8);
          kw "svw" (Svw 100.) (keyword_off + 9);
        ]
        @ container_entries (keyword_off + 10);
    }

  let height_family =
    {
      prefix = "h";
      css_name = "height";
      decls = [ height ];
      base = h_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "dvh" (Dvh 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.0) (keyword_off + 3);
          kw "lh" (Lh 1.) (keyword_off + 4);
          kw "lvh" (Lvh 100.) (keyword_off + 5);
          kw "max" Max_content (keyword_off + 6);
          kw "min" Min_content (keyword_off + 7);
          kw "px" (Px 1.0) (keyword_off + 8);
          kw "screen" (Vh 100.0) (keyword_off + 9);
          kw "svh" (Svh 100.) (keyword_off + 10);
          kw "dvw" (Dvw 100.) (keyword_off + 10);
          kw "lvw" (Lvw 100.) (keyword_off + 10);
          kw "svw" (Svw 100.) (keyword_off + 10);
        ];
    }

  let min_width_family =
    {
      prefix = "min-w";
      css_name = "min-width";
      decls = [ min_width ];
      base = min_w_base;
      spacing_off = 0;
      entries =
        [
          kw "0" (Px 0.) 0;
          kw "auto" Auto (keyword_off + 0);
          kw "fit" Fit_content (keyword_off + 1);
          kw "full" (Pct 100.0) (keyword_off + 2);
          kw "dvh" (Dvh 100.) (keyword_off + 2);
          kw "dvw" (Dvw 100.) (keyword_off + 2);
          kw "lvh" (Lvh 100.) (keyword_off + 2);
          kw "lvw" (Lvw 100.) (keyword_off + 2);
          kw "svh" (Svh 100.) (keyword_off + 2);
          kw "svw" (Svw 100.) (keyword_off + 2);
          kw "px" (Px 1.0) (keyword_off + 2);
          kw "screen" (Vw 100.0) (keyword_off + 2);
          kw "max" Max_content (keyword_off + 3);
          kw "min" Min_content (keyword_off + 4);
        ]
        @ container_entries (keyword_off + 5);
    }

  let min_height_family =
    {
      prefix = "min-h";
      css_name = "min-height";
      decls = [ min_height ];
      base = min_h_base;
      spacing_off = 0;
      entries =
        [
          kw "0" (Px 0.) 0;
          kw "auto" Auto (keyword_off + 0);
          kw "dvh" (Dvh 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.0) (keyword_off + 3);
          kw "lh" (Lh 1.) (keyword_off + 4);
          kw "lvh" (Lvh 100.) (keyword_off + 5);
          kw "max" Max_content (keyword_off + 6);
          kw "min" Min_content (keyword_off + 7);
          kw "screen" (Vh 100.0) (keyword_off + 8);
          kw "svh" (Svh 100.) (keyword_off + 9);
          kw "px" (Px 1.0) (keyword_off + 9);
          kw "dvw" (Dvw 100.) (keyword_off + 9);
          kw "lvw" (Lvw 100.) (keyword_off + 9);
          kw "svw" (Svw 100.) (keyword_off + 9);
        ];
    }

  (* Tailwind orders max-width in three bands: the container scale sizes
     (2xl..7xl) by number, then the numeric spacing values, then an arbitrary
     value, then the letter-prefixed keywords alphabetically. Only this family
     shifts its spacing band ([spacing_off]); its fractions stay put. *)
  let max_width_family =
    {
      prefix = "max-w";
      css_name = "max-width";
      decls = [ max_width ];
      base = max_w_base;
      spacing_off = 1_000_000;
      entries =
        [
          kw "dvh" (Dvh 100.) 0;
          kw "dvw" (Dvw 100.) 0;
          kw "lvh" (Lvh 100.) 0;
          kw "lvw" (Lvw 100.) 0;
          kw "svh" (Svh 100.) 0;
          kw "svw" (Svw 100.) 0;
          kw "px" (Px 1.0) 0;
          kw "screen" (Vw 100.0) 0;
          kw "fit" Fit_content (keyword_off + 0);
          kw "full" (Pct 100.0) (keyword_off + 1);
          kw "max" Max_content (keyword_off + 3);
          kw "min" Min_content (keyword_off + 5);
          kw "none" None (keyword_off + 6);
          kw "prose" (Ch 65.0) (keyword_off + 7);
          themed "screen-2xl" breakpoint_2xl (Rem 96.) (keyword_off + 8);
          themed "screen-lg" breakpoint_lg (Rem 64.) (keyword_off + 9);
          themed "screen-md" breakpoint_md (Rem 48.) (keyword_off + 10);
          themed "screen-sm" breakpoint_sm (Rem 40.) (keyword_off + 11);
          themed "screen-xl" breakpoint_xl (Rem 80.) (keyword_off + 12);
        ]
        @ container_entries_by
            [
              ("2xs", 0);
              ("3xs", 0);
              ("2xl", 0);
              ("3xl", 1);
              ("4xl", 2);
              ("5xl", 3);
              ("6xl", 4);
              ("7xl", 5);
              ("lg", keyword_off + 2);
              ("md", keyword_off + 4);
              ("sm", keyword_off + 13);
              ("xl", keyword_off + 14);
              ("xs", keyword_off + 15);
            ];
    }

  let max_height_family =
    {
      prefix = "max-h";
      css_name = "max-height";
      decls = [ max_height ];
      base = max_h_base;
      spacing_off = 0;
      entries =
        [
          kw "dvh" (Dvh 100.) (keyword_off + 0);
          kw "fit" Fit_content (keyword_off + 1);
          kw "full" (Pct 100.0) (keyword_off + 2);
          kw "lh" (Lh 1.) (keyword_off + 3);
          kw "lvh" (Lvh 100.) (keyword_off + 4);
          kw "max" Max_content (keyword_off + 5);
          kw "min" Min_content (keyword_off + 6);
          kw "none" None (keyword_off + 7);
          kw "screen" (Vh 100.0) (keyword_off + 8);
          kw "svh" (Svh 100.) (keyword_off + 9);
          kw "px" (Px 1.0) (keyword_off + 9);
          kw "dvw" (Dvw 100.) (keyword_off + 9);
          kw "lvw" (Lvw 100.) (keyword_off + 9);
          kw "svw" (Svw 100.) (keyword_off + 9);
        ];
    }

  let size_family =
    {
      prefix = "size";
      css_name = "size";
      decls = [ width; height ];
      base = size_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "fit" Fit_content (keyword_off + 1);
          kw "full" (Pct 100.0) (keyword_off + 2);
          kw "dvh" (Dvh 100.) (keyword_off + 2);
          kw "dvw" (Dvw 100.) (keyword_off + 2);
          kw "lvh" (Lvh 100.) (keyword_off + 2);
          kw "lvw" (Lvw 100.) (keyword_off + 2);
          kw "svh" (Svh 100.) (keyword_off + 2);
          kw "svw" (Svw 100.) (keyword_off + 2);
          kw "px" (Px 1.0) (keyword_off + 2);
          kw "max" Max_content (keyword_off + 3);
          kw "min" Min_content (keyword_off + 4);
        ];
    }

  let inline_family =
    {
      prefix = "inline";
      css_name = "inline-size";
      decls = [ inline_size ];
      base = inline_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "dvw" (Dvw 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.) (keyword_off + 3);
          kw "lvw" (Lvw 100.) (keyword_off + 4);
          kw "max" Max_content (keyword_off + 5);
          kw "min" Min_content (keyword_off + 6);
          kw "screen" (Vw 100.) (keyword_off + 7);
          kw "svw" (Svw 100.) (keyword_off + 8);
          kw "px" (Px 1.) (keyword_off + 20);
        ]
        @ container_entries (keyword_off + 9);
    }

  let min_inline_family =
    {
      prefix = "min-inline";
      css_name = "min-inline-size";
      decls = [ min_inline_size ];
      base = min_inline_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "fit" Fit_content (keyword_off + 1);
          kw "full" (Pct 100.) (keyword_off + 2);
          kw "screen" (Vw 100.0) (keyword_off + 2);
          kw "px" (Px 1.0) (keyword_off + 2);
          kw "svw" (Svw 100.) (keyword_off + 2);
          kw "lvw" (Lvw 100.) (keyword_off + 2);
          kw "dvw" (Dvw 100.) (keyword_off + 2);
          kw "max" Max_content (keyword_off + 3);
          kw "min" Min_content (keyword_off + 4);
        ]
        @ container_entries (keyword_off + 5);
    }

  let max_inline_family =
    {
      prefix = "max-inline";
      css_name = "max-inline-size";
      decls = [ max_inline_size ];
      base = max_inline_base;
      spacing_off = 0;
      entries =
        [
          kw "fit" Fit_content (keyword_off + 0);
          kw "full" (Pct 100.) (keyword_off + 1);
          kw "min" Min_content (keyword_off + 1);
          kw "screen" (Vw 100.0) (keyword_off + 1);
          kw "px" (Px 1.0) (keyword_off + 1);
          kw "svw" (Svw 100.) (keyword_off + 1);
          kw "lvw" (Lvw 100.) (keyword_off + 1);
          kw "dvw" (Dvw 100.) (keyword_off + 1);
          kw "max" Max_content (keyword_off + 2);
          kw "none" None (keyword_off + 3);
        ]
        @ container_entries (keyword_off + 4);
    }

  let block_family =
    {
      prefix = "block";
      css_name = "block-size";
      decls = [ block_size ];
      base = block_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "dvh" (Dvh 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.) (keyword_off + 3);
          kw "lh" (Lh 1.) (keyword_off + 4);
          kw "lvh" (Lvh 100.) (keyword_off + 5);
          kw "max" Max_content (keyword_off + 6);
          kw "min" Min_content (keyword_off + 7);
          kw "screen" (Vh 100.) (keyword_off + 8);
          kw "svh" (Svh 100.) (keyword_off + 9);
          kw "px" (Px 1.) (keyword_off + 20);
        ];
    }

  let min_block_family =
    {
      prefix = "min-block";
      css_name = "min-block-size";
      decls = [ min_block_size ];
      base = min_block_base;
      spacing_off = 0;
      entries =
        [
          kw "auto" Auto (keyword_off + 0);
          kw "dvh" (Dvh 100.) (keyword_off + 1);
          kw "fit" Fit_content (keyword_off + 2);
          kw "full" (Pct 100.) (keyword_off + 3);
          kw "px" (Px 1.0) (keyword_off + 3);
          kw "lh" (Lh 1.) (keyword_off + 4);
          kw "lvh" (Lvh 100.) (keyword_off + 5);
          kw "max" Max_content (keyword_off + 6);
          kw "min" Min_content (keyword_off + 7);
          kw "screen" (Vh 100.) (keyword_off + 8);
          kw "svh" (Svh 100.) (keyword_off + 9);
        ];
    }

  let max_block_family =
    {
      prefix = "max-block";
      css_name = "max-block-size";
      decls = [ max_block_size ];
      base = max_block_base;
      spacing_off = 0;
      entries =
        [
          kw "dvh" (Dvh 100.) (keyword_off + 0);
          kw "fit" Fit_content (keyword_off + 1);
          kw "full" (Pct 100.) (keyword_off + 2);
          kw "px" (Px 1.0) (keyword_off + 2);
          kw "lh" (Lh 1.) (keyword_off + 3);
          kw "lvh" (Lvh 100.) (keyword_off + 4);
          kw "max" Max_content (keyword_off + 5);
          kw "min" Min_content (keyword_off + 6);
          kw "none" None (keyword_off + 7);
          kw "screen" (Vh 100.) (keyword_off + 8);
          kw "svh" (Svh 100.) (keyword_off + 9);
        ];
    }

  let family = function
    | Width -> width_family
    | Height -> height_family
    | Min_width -> min_width_family
    | Min_height -> min_height_family
    | Max_width -> max_width_family
    | Max_height -> max_height_family
    | Width_and_height -> size_family
    | Inline_size -> inline_family
    | Min_inline_size -> min_inline_family
    | Max_inline_size -> max_inline_family
    | Block_size -> block_family
    | Min_block_size -> min_block_family
    | Max_block_size -> max_block_family

  (* The part of the class name that follows the family prefix. *)
  let class_suffix = function
    | Keyword k -> k.spelling
    | Themed t -> t.spelling
    | Spacing n -> class_float (n *. 4.)
    | Fraction f -> f
    | Arbitrary (raw, _) -> "[" ^ raw ^ "]"

  let value_order f = function
    | Keyword k -> k.order
    | Themed t -> t.order
    | Spacing n -> f.spacing_off + spacing_value_order n
    | Fraction s -> fraction_value_order s
    | Arbitrary _ -> arbitrary_off

  let lookup f spelling =
    List.find_opt (fun v -> class_suffix v = spelling) f.entries

  (* [keyword prop name] is family [prop]'s entry for keyword [name]; every
     caller names one the family's table lists. *)
  let keyword prop name =
    let f = family prop in
    match lookup f name with
    | Some v -> Sized (prop, v)
    | None -> invalid_arg ("sizing: no such utility: " ^ f.prefix ^ "-" ^ name)

  let aspect_auto' = style [ Css.aspect_ratio Auto ]

  (* Theme variables for aspect ratios *)
  let aspect_video_var = Var.theme Css.Aspect_ratio "aspect-video" ~order:(4, 1)

  (* aspect-square inlines the 1/1 ratio in v4 (no --aspect-square theme token),
     unlike aspect-video which references the --aspect-video token. *)
  let aspect_square' = style [ Css.aspect_ratio (Ratio (1., 1.)) ]

  let aspect_video' =
    let decl, r = Var.binding aspect_video_var (Ratio (16., 9.)) in
    style (decl :: [ Css.aspect_ratio (Var r) ])

  let aspect_ratio' w h = style [ Css.aspect_ratio (Ratio (w, h)) ]

  (* v4 resolves w-<name> to --width-<name> when the theme defines it, and
     otherwise to the --container-<name> scale (the default). Only w-* reads
     --width-*. *)
  let width_override theme prop spelling : Style.t option =
    match prop with
    | Width -> (
        match Scheme.theme_value (Some theme) ("width-" ^ spelling) with
        | None -> None
        | Some v ->
            let decl =
              Css.custom_property ~layer:"theme" ("--width-" ^ spelling) v
            in
            Some
              (style
                 [ decl; width (Var (Var.theme_ref ("width-" ^ spelling))) ]))
    | _ -> None

  let sized_style theme prop v =
    let f = family prop in
    let set len = style (List.map (fun decl -> decl len) f.decls) in
    let set_arbitrary raw len =
      let theme_decl : Css.declaration option =
        let bare_name = Parse.extract_var_name raw in
        if String.equal bare_name raw then Option.None
        else
          Scheme.token theme bare_name
          |> Option.map (fun value ->
              Css.custom_property ~layer:"theme" ("--" ^ bare_name) value)
      in
      style (Option.to_list theme_decl @ List.map (fun decl -> decl len) f.decls)
    in
    match v with
    | Keyword k -> set k.length
    | Arbitrary (raw, len) -> set_arbitrary raw len
    | Fraction s -> (
        match fraction_pct s with
        | Some pct -> set (Pct pct)
        | None -> failwith ("Unknown " ^ f.css_name ^ " fraction: " ^ s))
    | Spacing n ->
        (* [n] is in rem; the class number is [n * 4] because --spacing is
           0.25rem. calc(var(--spacing) * n) keeps v4 compatibility. *)
        let decl, len = Theme.spacing_calc_float ~theme (n *. 4.) in
        style (decl :: List.map (fun d -> d len) f.decls)
    | Themed t -> (
        match width_override theme prop t.spelling with
        | Some s -> s
        | None -> style (t.decl :: List.map (fun d -> d t.value) f.decls))

  let to_style theme = function
    | Sized (prop, v) -> sized_style theme prop v
    | Aspect_auto -> aspect_auto'
    | Aspect_square -> aspect_square'
    | Aspect_video -> aspect_video'
    | Aspect_ratio (w, h) -> aspect_ratio' w h
    | Aspect_bracket (w, h) -> aspect_ratio' w h
    | Aspect_bracket_num s -> aspect_ratio' (float_of_string s) 1.

  let err_not_utility = Error (`Msg "Not a sizing utility")

  let err_invalid_value name value =
    Error (`Msg ("Invalid " ^ name ^ " value: " ^ value))

  (* A [/] inside a bracket belongs to the value ([w-[calc(2px/2)]]), not to a
     fraction. *)
  let is_bracket v = String.length v > 0 && v.[0] = '['

  let parse_arbitrary s : (string * Css.length) option =
    (* Parse bracket values: [4px], [1rem], [calc(100vh-4rem)], etc. Uses
       Css.parse_length for full CSS length parsing including calc(). Returns
       (raw_inner, parsed_length) where raw_inner is used for the CSS class name
       selector (preserving original formatting). *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      let css_value =
        Parse.normalize_css_math_operators (Parse.decode_arbitrary_value inner)
      in
      match Css.parse_length css_value with
      | Some l -> Some (inner, l)
      | None -> None
    else None

  (* One parser for all thirteen sizing families: the family's own keyword
     table, then the fraction, bracket and spacing tails they share. *)
  let parse_sized prop v =
    let f = family prop in
    match lookup f v with
    | Some value -> Ok (Sized (prop, value))
    | None -> (
        if String.contains v '/' && not (is_bracket v) then
          if fraction_pct v <> None then Ok (Sized (prop, Fraction v))
          else err_invalid_value (f.css_name ^ " fraction") v
        else if is_bracket v then
          match parse_arbitrary v with
          | Some (raw, len) -> Ok (Sized (prop, Arbitrary (raw, len)))
          | None -> err_invalid_value f.css_name v
        else
          match Parse.decimal_float v with
          | Some n when n >= 0. -> Ok (Sized (prop, Spacing (n *. 0.25)))
          | _ -> err_invalid_value f.css_name v)

  let parse_max_w_screen s =
    match lookup max_width_family ("screen-" ^ s) with
    | Some value -> Ok (Sized (Max_width, value))
    | None -> err_invalid_value "max-width screen size" s

  (* Tailwind accepts a ratio part only when it is a non-negative multiple of
     0.25 ([isValidSpacingMultiplier]), so [8.5/11] is valid but [1.23/4.56] is
     not. *)
  let is_quarter_multiple f = f >= 0. && Float.rem f 0.25 = 0.

  let parse_aspect_ratio s mk =
    match String.split_on_char '/' s with
    | [ w; h ] -> (
        match (float_of_string_opt w, float_of_string_opt h) with
        | Some w, Some h when is_quarter_multiple w && is_quarter_multiple h ->
            Ok (mk w h)
        | _ -> err_not_utility)
    | _ -> err_not_utility

  let of_class _theme class_name =
    match Parse.split_class class_name with
    | [ "w"; value ] -> parse_sized Width value
    | [ "h"; value ] -> parse_sized Height value
    | [ "min"; "w"; value ] -> parse_sized Min_width value
    | [ "min"; "h"; value ] -> parse_sized Min_height value
    | [ "max"; "w"; value ] -> parse_sized Max_width value
    | [ "max"; "w"; "screen"; size ] -> parse_max_w_screen size
    | [ "max"; "h"; value ] -> parse_sized Max_height value
    | [ "size"; value ] -> parse_sized Width_and_height value
    | [ "inline"; value ] -> parse_sized Inline_size value
    | [ "min"; "inline"; value ] -> parse_sized Min_inline_size value
    | [ "max"; "inline"; value ] -> parse_sized Max_inline_size value
    | [ "block"; value ] -> parse_sized Block_size value
    | [ "min"; "block"; value ] -> parse_sized Min_block_size value
    | [ "max"; "block"; value ] -> parse_sized Max_block_size value
    | [ "aspect"; "auto" ] -> Ok Aspect_auto
    | [ "aspect"; "square" ] -> Ok Aspect_square
    | [ "aspect"; "video" ] -> Ok Aspect_video
    | [ "aspect"; value ] when Parse.is_bracket_value value -> (
        let inner = Parse.bracket_inner value in
        match parse_aspect_ratio inner (fun w h -> Aspect_bracket (w, h)) with
        | Ok _ as ok -> ok
        | Error _ -> (
            (* A bare number arbitrary ratio (aspect-[1.333]) is a single-value
               aspect-ratio, which minifies to just the number. *)
            match float_of_string_opt inner with
            | Some f when f > 0. -> Ok (Aspect_bracket_num inner)
            | _ -> err_not_utility))
    | [ "aspect"; value ] ->
        parse_aspect_ratio value (fun w h -> Aspect_ratio (w, h))
    | _ -> err_not_utility

  let suborder = function
    | Sized (prop, v) ->
        let f = family prop in
        f.base + value_order f v
    (* Aspect: ratios -> brackets -> keywords *)
    | Aspect_ratio (rw, rh) ->
        aspect_base + int_of_float (rw *. 10.) + int_of_float rh
    | Aspect_bracket (rw, rh) ->
        aspect_base + 1000 + int_of_float (rw *. 10.) + int_of_float rh
    | Aspect_bracket_num s ->
        aspect_base + 1000 + int_of_float (float_of_string s *. 10.) + 1
    | Aspect_auto -> aspect_base + 2000
    | Aspect_square -> aspect_base + 2001
    | Aspect_video -> aspect_base + 2002

  (** Priority 6: sizing utilities (w-*, h-*, max-w-*, ...) come before
      flex-1/flex-col in Tailwind's order. The logical ones are registered after
      every other utility, so they sort last instead of beside w-* and h-*. *)
  let priority u = if suborder u >= logical then logical_priority else 6

  let to_class = function
    | Sized (prop, v) -> (family prop).prefix ^ "-" ^ class_suffix v
    | Aspect_auto -> "aspect-auto"
    | Aspect_square -> "aspect-square"
    | Aspect_video -> "aspect-video"
    | Aspect_ratio (w, h) ->
        let num f =
          if Float.is_integer f then string_of_int (int_of_float f)
          else string_of_float f
        in
        "aspect-" ^ num w ^ "/" ^ num h
    | Aspect_bracket (w, h) ->
        let num f =
          if Float.is_integer f then string_of_int (int_of_float f)
          else string_of_float f
        in
        "aspect-[" ^ num w ^ "/" ^ num h ^ "]"
    | Aspect_bracket_num s -> "aspect-[" ^ s ^ "]"

  let examples =
    [
      keyword Height "auto";
      keyword Max_height "none";
      keyword Min_height "auto";
      keyword Width "auto";
      keyword Max_width "none";
      keyword Min_width "auto";
      Aspect_auto;
      keyword Block_size "auto";
      keyword Max_block_size "none";
      keyword Min_block_size "auto";
      keyword Inline_size "auto";
      keyword Max_inline_size "none";
      keyword Min_inline_size "auto";
    ]
end

open Handler

(** Register the sizing utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let () = () (* Ensure utility is defined before usage below *)

(* Expose prime helpers wrapped as Utility.t *)
let prime_size_utility ~none ~xs ~sm ~md ~lg ~xl ~xl_2 ~xl_3 ~full ~rem =
  function
  | `None -> utility none
  | `Xs -> utility xs
  | `Sm -> utility sm
  | `Md -> utility md
  | `Lg -> utility lg
  | `Xl -> utility xl
  | `Xl_2 -> utility xl_2
  | `Xl_3 -> utility xl_3
  | `Full -> utility full
  | `Rem n -> utility (rem n)

(* [n] is in rem units; the spacing scale steps by 0.25rem. *)
let spacing prop n = Sized (prop, Spacing n)
let fraction prop f = Sized (prop, Fraction f)
let scaled prop n = utility (spacing prop (float_of_int n *. 0.25))

let w' =
  prime_size_utility ~none:(spacing Width 0.) ~xs:(spacing Width 0.5)
    ~sm:(spacing Width 1.0) ~md:(spacing Width 1.5) ~lg:(spacing Width 2.0)
    ~xl:(spacing Width 3.0) ~xl_2:(spacing Width 4.0) ~xl_3:(spacing Width 6.0)
    ~full:(keyword Width "full") ~rem:(spacing Width)

let h' =
  prime_size_utility ~none:(spacing Height 0.) ~xs:(spacing Height 0.5)
    ~sm:(spacing Height 1.0) ~md:(spacing Height 1.5) ~lg:(spacing Height 2.0)
    ~xl:(spacing Height 3.0) ~xl_2:(spacing Height 4.0)
    ~xl_3:(spacing Height 6.0) ~full:(keyword Height "full")
    ~rem:(spacing Height)

let min_w' =
  prime_size_utility ~none:(keyword Min_width "0") ~xs:(spacing Min_width 0.5)
    ~sm:(spacing Min_width 1.0) ~md:(spacing Min_width 1.5)
    ~lg:(spacing Min_width 2.0) ~xl:(spacing Min_width 3.0)
    ~xl_2:(spacing Min_width 4.0) ~xl_3:(spacing Min_width 6.0)
    ~full:(keyword Min_width "full") ~rem:(spacing Min_width)

let max_w' =
  prime_size_utility ~none:(keyword Max_width "none")
    ~xs:(keyword Max_width "xs") ~sm:(keyword Max_width "sm")
    ~md:(keyword Max_width "md") ~lg:(keyword Max_width "lg")
    ~xl:(keyword Max_width "xl") ~xl_2:(keyword Max_width "2xl")
    ~xl_3:(keyword Max_width "3xl") ~full:(keyword Max_width "full")
    ~rem:(spacing Max_width)

let min_h' =
  prime_size_utility ~none:(keyword Min_height "0") ~xs:(spacing Min_height 0.5)
    ~sm:(spacing Min_height 1.0) ~md:(spacing Min_height 1.5)
    ~lg:(spacing Min_height 2.0) ~xl:(spacing Min_height 3.0)
    ~xl_2:(spacing Min_height 4.0) ~xl_3:(spacing Min_height 6.0)
    ~full:(keyword Min_height "full")
    ~rem:(spacing Min_height)

let max_h' =
  prime_size_utility
    ~none:(keyword Max_height "none")
    ~xs:(spacing Max_height 0.5) ~sm:(spacing Max_height 1.0)
    ~md:(spacing Max_height 1.5) ~lg:(spacing Max_height 2.0)
    ~xl:(spacing Max_height 3.0) ~xl_2:(spacing Max_height 4.0)
    ~xl_3:(spacing Max_height 6.0)
    ~full:(keyword Max_height "full")
    ~rem:(spacing Max_height)

(* Top-level wrappers returning Utility.t, following the Utility.Handler
   pattern *)

(* Width *)
let w n = scaled Width n
let w_auto = utility (keyword Width "auto")
let w_full = utility (keyword Width "full")
let w_screen = utility (keyword Width "screen")
let w_min = utility (keyword Width "min")
let w_max = utility (keyword Width "max")
let w_fit = utility (keyword Width "fit")
let w_1_2 = utility (fraction Width "1/2")
let w_1_3 = utility (fraction Width "1/3")
let w_2_3 = utility (fraction Width "2/3")
let w_1_4 = utility (fraction Width "1/4")
let w_3_4 = utility (fraction Width "3/4")
let w_1_5 = utility (fraction Width "1/5")
let w_2_5 = utility (fraction Width "2/5")
let w_3_5 = utility (fraction Width "3/5")
let w_4_5 = utility (fraction Width "4/5")

(* Height *)
let h n = scaled Height n
let h_auto = utility (keyword Height "auto")
let h_full = utility (keyword Height "full")
let h_screen = utility (keyword Height "screen")
let h_min = utility (keyword Height "min")
let h_max = utility (keyword Height "max")
let h_fit = utility (keyword Height "fit")
let h_1_2 = utility (fraction Height "1/2")
let h_1_3 = utility (fraction Height "1/3")
let h_2_3 = utility (fraction Height "2/3")
let h_1_4 = utility (fraction Height "1/4")
let h_3_4 = utility (fraction Height "3/4")
let h_1_5 = utility (fraction Height "1/5")
let h_2_5 = utility (fraction Height "2/5")
let h_3_5 = utility (fraction Height "3/5")
let h_4_5 = utility (fraction Height "4/5")

(* Min width *)
let min_w n = scaled Min_width n
let min_w_0 = utility (keyword Min_width "0")
let min_w_full = utility (keyword Min_width "full")
let min_w_min = utility (keyword Min_width "min")
let min_w_max = utility (keyword Min_width "max")
let min_w_fit = utility (keyword Min_width "fit")

(* Max width *)
let max_w n = scaled Max_width n
let max_w_none = utility (keyword Max_width "none")
let max_w_xs = utility (keyword Max_width "xs")
let max_w_sm = utility (keyword Max_width "sm")
let max_w_md = utility (keyword Max_width "md")
let max_w_lg = utility (keyword Max_width "lg")
let max_w_xl = utility (keyword Max_width "xl")
let max_w_2xl = utility (keyword Max_width "2xl")
let max_w_3xl = utility (keyword Max_width "3xl")
let max_w_4xl = utility (keyword Max_width "4xl")
let max_w_5xl = utility (keyword Max_width "5xl")
let max_w_6xl = utility (keyword Max_width "6xl")
let max_w_7xl = utility (keyword Max_width "7xl")
let max_w_full = utility (keyword Max_width "full")
let max_w_min = utility (keyword Max_width "min")
let max_w_max = utility (keyword Max_width "max")
let max_w_fit = utility (keyword Max_width "fit")
let max_w_prose = utility (keyword Max_width "prose")
let max_w_screen_sm = utility (keyword Max_width "screen-sm")
let max_w_screen_md = utility (keyword Max_width "screen-md")
let max_w_screen_lg = utility (keyword Max_width "screen-lg")
let max_w_screen_xl = utility (keyword Max_width "screen-xl")
let max_w_screen_2xl = utility (keyword Max_width "screen-2xl")

(* Min height *)
let min_h n = scaled Min_height n
let min_h_0 = utility (keyword Min_height "0")
let min_h_full = utility (keyword Min_height "full")
let min_h_screen = utility (keyword Min_height "screen")
let min_h_min = utility (keyword Min_height "min")
let min_h_max = utility (keyword Min_height "max")
let min_h_fit = utility (keyword Min_height "fit")

(* Max height *)
let max_h n = scaled Max_height n
let max_h_none = utility (keyword Max_height "none")
let max_h_full = utility (keyword Max_height "full")
let max_h_screen = utility (keyword Max_height "screen")
let max_h_min = utility (keyword Max_height "min")
let max_h_max = utility (keyword Max_height "max")
let max_h_fit = utility (keyword Max_height "fit")

(* Size (width and height combined) *)
let size n = scaled Width_and_height n
let size_auto = utility (keyword Width_and_height "auto")
let size_full = utility (keyword Width_and_height "full")
let size_min = utility (keyword Width_and_height "min")
let size_max = utility (keyword Width_and_height "max")
let size_fit = utility (keyword Width_and_height "fit")

(* Aspect ratio *)
let aspect_auto = utility Aspect_auto
let aspect_square = utility Aspect_square
let aspect_video = utility Aspect_video
let aspect_ratio w h = utility (Aspect_ratio (float_of_int w, float_of_int h))

(* Order exposure for this module *)
let order (u : Utility.base) =
  match u with Self x -> Some (priority x, suborder x) | _ -> None

(* Export container theme variables for use by other modules (e.g., Columns) *)
let container_binding = Handler.container_binding
let container_3xs = Handler.container_3xs
let container_2xs = Handler.container_2xs
let container_xs = Handler.container_xs
let container_sm = Handler.container_sm
let container_md = Handler.container_md
let container_lg = Handler.container_lg
let container_xl = Handler.container_xl
let container_2xl = Handler.container_2xl
let container_3xl = Handler.container_3xl
let container_4xl = Handler.container_4xl
let container_5xl = Handler.container_5xl
let container_6xl = Handler.container_6xl
let container_7xl = Handler.container_7xl
