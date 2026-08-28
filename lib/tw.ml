(** A type-safe, ergonomic DSL for Tailwind CSS using nominal types.

    This library takes inspiration from Tailwind CSS v3's utility-first approach
    while leveraging OCaml's type system for compile-time safety. We cherry-pick
    concepts that work well with OCaml and add our own innovations where
    appropriate.

    Key design decisions:
    - Pure OCaml implementation without external CSS dependencies
    - Type-safe API that prevents invalid CSS at compile time
    - Simplified spacing functions that accept integers directly
    - Support for modern CSS features like container queries and 3D transforms
    - Minimal bundle size for js_of_ocaml by avoiding Format module. *)

type t = Utility.t

include Color
include Backgrounds
include Margin
include Gap
include Padding
include Sizing
include Typography
include Layout
include Overflow
include Overscroll
include Overflow_wrap
include Box_sizing
include Tab
include Scrollbar
include Zoom
include Field_sizing
include Grid
include Grid_item
include Grid_template
include Flex
include Flex_props
include Flex_layout
include Alignment
include Borders
include Effects
include Text_shadow
include Transforms
include Cursor
include Touch
include Divide
include Interactivity
include Containers
include Filters
include Masks
include Mask_gradient
include Clipping
include Position
include Animations
include Transitions
include Forms
include Tables
include Svg
include Accessibility
include Modifiers
include Prose
include Columns
include Contain
include Scroll
include Arbitrary

let to_css ?theme ?(base = Build.default_config.base) ?forms
    ?(layers = Build.default_config.layers) ?extra utilities =
  Build.to_css ?theme ~config:{ base; forms; layers } ?extra utilities

let to_inline_style ?theme utilities = Build.to_inline_style ?theme utilities
let preflight = Preflight.stylesheet

(* Class generation functions *)
let pp utility = Utility.to_class utility
let to_classes styles = styles |> List.map Utility.to_class |> String.concat " "
let modifiers_of_string = Modifiers.of_string

let is_whitespace = function
  | ' ' | '\t' | '\n' | '\r' | '\012' -> true
  | _ -> false

let split_whitespace s =
  let current = Buffer.create 16 in
  let tokens = ref [] in
  let flush () =
    if Buffer.length current > 0 then (
      tokens := Buffer.contents current :: !tokens;
      Buffer.clear current)
  in
  String.iter
    (fun c -> if is_whitespace c then flush () else Buffer.add_char current c)
    s;
  flush ();
  List.rev !tokens

(* The v4 [prop-(--x)] shorthand is [prop-[var(--x)]] in value but keeps its own
   class name. Rewrite the trailing [(...)] to its bracket form for parsing; the
   original spelling is restored via [Utility.alias]. Handles the bare var
   [(--x)] / [(--x,fallback)] -> [[var(--x...)]] and the typed
   [(family-name:--x)] -> [[family-name:var(--x)]] forms. Returns [None] when
   there is no paren shorthand. *)
let normalize_paren_var base_class =
  let n = String.length base_class in
  if n > 4 && base_class.[n - 1] = ')' then
    match String.rindex_opt base_class '(' with
    | Some lp when lp > 0 && base_class.[lp - 1] = '-' -> (
        let prefix = String.sub base_class 0 lp in
        let inner = String.sub base_class (lp + 1) (n - lp - 2) in
        let ilen = String.length inner in
        if ilen > 1 && inner.[0] = '-' && inner.[1] = '-' then
          (* bare var (with optional ,fallback) *)
          Some (prefix ^ "[var(" ^ inner ^ ")]")
        else
          (* typed hint: <type>:--name[,fallback] *)
          match String.index_opt inner ':' with
          | Some ci
            when ci + 2 < ilen && inner.[ci + 1] = '-' && inner.[ci + 2] = '-'
            ->
              let typ = String.sub inner 0 ci in
              let v = String.sub inner (ci + 1) (ilen - ci - 1) in
              Some (prefix ^ "[" ^ typ ^ ":var(" ^ v ^ ")]")
          | _ -> None)
    | _ -> None
  else None

(* Split the [!] important marker off the base class: the v3 prefix ([!flex],
   [md:!flex]) or the v4 trailing form ([flex!]). Each keeps its form in the
   generated selector so it matches the source class. *)
let split_importance base_class =
  let n = String.length base_class in
  if n > 1 && base_class.[0] = '!' then
    (`Prefix, String.sub base_class 1 (n - 1))
  else if n > 1 && base_class.[n - 1] = '!' then
    (`Suffix, String.sub base_class 0 (n - 1))
  else (`None, base_class)

(* The alpha a [theme()] path carries, as the percentage to mix at: a percentage
   as written, a bare number as the fraction Tailwind reads it for. *)
let theme_alpha_percent a =
  let n = String.length a in
  if n > 1 && a.[n - 1] = '%' then float_of_string_opt (String.sub a 0 (n - 1))
  else Stdlib.Option.map (fun f -> f *. 100.) (float_of_string_opt a)

(* Tailwind applies a [theme()] alpha by mixing the colour with transparent,
   which reads whatever the [@theme] bound the palette entry to. *)
let theme_color_with_alpha base a =
  match (Cascade.Css.parse_color base, theme_alpha_percent a) with
  | Some color, Some percent1 ->
      Some
        (Cascade.Css.Pp.to_string Cascade.Css.pp_color
           (Cascade.Css.color_mix ~in_space:Oklab ~percent1 color
              Cascade.Css.Transparent))
  | _ -> None

(* Resolve a Tailwind theme() dot-path to its static value:
   colors.<name>.<shade> (optionally with a /<alpha> suffix) becomes the
   colour's value mixed with the alpha, and spacing.<n> becomes <n * 0.25>rem.
   Returns None for paths not resolved. *)
let theme_resolve_path ~theme inner =
  let path, alpha =
    match String.index_opt inner '/' with
    | Some k ->
        ( String.sub inner 0 k,
          Some (String.sub inner (k + 1) (String.length inner - k - 1)) )
    | None -> (inner, None)
  in
  match String.split_on_char '.' path with
  | [ "colors"; name; shade ] -> (
      match (Color.of_string name, int_of_string_opt shade) with
      | Ok c, Some sh -> (
          (* A project that renames a palette entry in its [@theme] gets its own
             value back here, the way [bg-<name>-<shade>] already does. *)
          let token = String.concat "-" [ "color"; name; shade ] in
          let base =
            match Scheme.theme_value (Some theme) token with
            | Some v -> v
            | None -> Color.to_oklch_css c sh
          in
          match alpha with
          | Some a -> theme_color_with_alpha base a
          | None -> Some base)
      | _ -> None)
  | [ "spacing"; n ] ->
      (* The step here is Tailwind's own 0.25rem and not whatever [--spacing]
         the project set: checked against the CLI with [@theme { --spacing:
         0.5rem }], both sheets still resolve [theme(spacing.4)] to [1rem], so
         reading the theme would lose parity rather than gain it.
         [Theme.spacing_times] is that same fixed product, and going through it
         is what keeps the two spellings of it in step. *)
      Stdlib.Option.bind (float_of_string_opt n) Theme.spacing_times
  | _ -> None

(* Replace each theme(<path>) in a class string with its resolved value, spaces
   re-encoded as [_] so downstream arbitrary-value decoding treats them as
   spaces. Unresolved theme() calls are left verbatim. *)
let resolve_theme_functions ~theme s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if !i + 6 <= n && String.sub s !i 6 = "theme(" then begin
      let j = ref (!i + 6) and depth = ref 1 in
      while !j < n && !depth > 0 do
        if s.[!j] = '(' then incr depth else if s.[!j] = ')' then decr depth;
        if !depth > 0 then incr j
      done;
      (* [!j] is the closing paren, or [n] when the call never closed - a
         truncated attribute or template artefact in scanned markup. An
         unterminated call has no path to resolve, so the rest of the string is
         copied through and the class stays unresolved. *)
      let closed = !depth = 0 in
      let inner = String.sub s (!i + 6) (!j - (!i + 6)) in
      (match if closed then theme_resolve_path ~theme inner else None with
      | Some v ->
          Buffer.add_string buf
            (String.map (fun c -> if c = ' ' then '_' else c) v)
      | None ->
          let stop = if closed then !j + 1 else n in
          Buffer.add_string buf (String.sub s !i (stop - !i)));
      i := if closed then !j + 1 else n
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* The rejection a class gets once no handler has claimed it. A bracket class
   that looks like an arbitrary property but that nothing accepted is malformed
   rather than unsupported: the bracket has no property name or does not end the
   class, or the modifier after it is not an opacity on a colour. *)
let unknown_class_error ~base_class class_str =
  if
    String.length base_class > 2
    && base_class.[0] = '['
    && String.contains base_class ':'
  then
    Error
      (`Msg
         ("Invalid arbitrary property '" ^ class_str
        ^ "': expected [property:value], optionally followed by an /opacity \
           modifier on a colour value (e.g. [color:var(--x)]/50)"))
  else Error (`Msg ("Unknown class: " ^ class_str))

(* Parse a single class string into a Tw.t *)
let of_string ?(theme = Scheme.default) class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  let importance, base_class = split_importance base_class in
  (* Wrap [important] around the base before applying modifiers, so a
     responsive/state prefix stays outermost: md:!flex -> md:(!flex). An
     optional [alias] sits inside importance so [w-(--w)!] keeps both forms. *)
  let finish ?alias base_utility =
    let base_util = Utility.base base_utility in
    let base_util =
      match alias with
      | Some cls -> Utility.alias cls base_util
      | None -> base_util
    in
    let base_util =
      match importance with
      | `Prefix -> Utility.important base_util
      | `Suffix -> Utility.important ~suffix:true base_util
      | `None -> base_util
    in
    match Modifiers.apply ~theme modifiers base_util with
    | Some u -> Ok u
    | None -> Error (`Msg ("Unknown modifier in: " ^ class_str))
  in
  (* Resolve theme() dot-paths for dispatch, keeping the original spelling as
     the class-name alias so the utility still round-trips. *)
  let resolved_base = resolve_theme_functions ~theme base_class in
  let theme_alias =
    if resolved_base = base_class then None else Some base_class
  in
  match Utility.base_of_class theme resolved_base with
  | Ok base_utility -> finish ?alias:theme_alias base_utility
  | Error _ -> (
      (* Fallback: the v4 [prop-(--x)] shorthand for handlers that accept the
         [prop-[var(--x)]] form but not the paren spelling directly. Handlers
         that support [(--x)] natively (e.g. rotate) already matched above, so
         this never overrides them. The original spelling is kept via the
         alias. *)
      match normalize_paren_var base_class with
      | Some normalized -> (
          match Utility.base_of_class theme normalized with
          | Ok base_utility -> finish ~alias:base_class base_utility
          | Error _ -> Error (`Msg ("Unknown class: " ^ class_str)))
      | None -> unknown_class_error ~base_class class_str)

(* A name the parser rejects may be a typo or a deliberate non-tw class - a
   framework hook, a JS selector - and nothing here can tell the two apart. So
   parsing a class string never fails: it hands back what it recognised and what
   it did not, and the caller judges. *)
let of_classes ?theme s =
  let styles, unknown =
    List.fold_left
      (fun (styles, unknown) cls ->
        match of_string ?theme cls with
        | Ok t -> (t :: styles, unknown)
        | Error _ -> (styles, cls :: unknown))
      ([], []) (split_whitespace s)
  in
  (List.rev styles, List.rev unknown)

let str s = fst (of_classes s)

(** {1 Module Exports} *)

module Style = Style
module Margin = Margin
module Padding = Padding
module Gap = Gap
module Spacing = Spacing
module Flex = Flex
module Flex_props = Flex_props
module Flex_layout = Flex_layout
module Alignment = Alignment
module Cursor = Cursor
module Borders = Borders
module Backgrounds = Backgrounds
module Sizing = Sizing
module Layout = Layout
module Overflow = Overflow
module Overscroll = Overscroll
module Overflow_wrap = Overflow_wrap
module Box_sizing = Box_sizing
module Tab = Tab
module Scrollbar = Scrollbar
module Zoom = Zoom
module Field_sizing = Field_sizing
module Grid = Grid
module Grid_item = Grid_item
module Grid_template = Grid_template
module Typography = Typography
module Divide = Divide
module Effects = Effects
module Text_shadow = Text_shadow
module Transforms = Transforms
module Interactivity = Interactivity
module Containers = Containers
module Filters = Filters
module Masks = Masks
module Position = Position
module Animations = Animations
module Transitions = Transitions
module Forms = Forms
module Tables = Tables
module Svg = Svg
module Accessibility = Accessibility
module Output = Output
module Rule = Rule
module Build = Build
module Prose = Prose
module Css = Cascade.Css
module Color = Color
module Modifiers = Modifiers
module Var = Var
module Theme = Theme
module Scheme = Scheme
module Utility = Utility
module Columns = Columns
module Contain = Contain
module Scroll = Scroll
module Arbitrary = Arbitrary
module Touch = Touch
module Mask_gradient = Mask_gradient
module Property = Property

(* Include flex utilities *)
include Flex

(* Include grid utilities *)
include Grid

(* Include cursor utilities *)
include Cursor
