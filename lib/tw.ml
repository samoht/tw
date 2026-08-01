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
module Declared = Build.Declared

module Mask_stop = struct
  type t = Mask_gradient.Handler.value =
    | Spacing of float
    | Percent of float
    | Arbitrary of string
end

module Mask_position = struct
  type t = Mask_gradient.Handler.radial_at_position =
    | Keyword of string
    | Custom of string
end

let to_css ?theme ?(base = Build.default_config.base) ?forms
    ?(layers = Build.default_config.layers) ?declared utilities =
  Build.to_css ?theme ~config:{ base; forms; layers } ?declared utilities

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

(* Resolve a Tailwind theme() dot-path to its static value:
   colors.<name>.<shade> (optionally with a /<alpha> suffix) becomes the
   colour's oklch value, and spacing.<n> becomes <n * 0.25>rem. Returns None for
   paths not resolved. *)
let theme_resolve_path inner =
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
      | Ok c, Some sh ->
          let base = Color.to_oklch_css c sh in
          Some
            (match alpha with
            | Some a
              when String.length base > 0 && base.[String.length base - 1] = ')'
              ->
                (* oklch(L C H) -> oklch(L C H / a); cascade folds the alpha
                   form to Tailwind's oklab(...) under canonical comparison. *)
                String.concat ""
                  [ String.sub base 0 (String.length base - 1); " / "; a; ")" ]
            | _ -> base)
      | _ -> None)
  | [ "spacing"; n ] -> (
      match float_of_string_opt n with
      | Some nf ->
          let rem = nf *. 0.25 in
          let s =
            if Float.is_integer rem then string_of_int (int_of_float rem)
            else string_of_float rem
          in
          Some (String.concat "" [ s; "rem" ])
      | None -> None)
  | _ -> None

(* Replace each theme(<path>) in a class string with its resolved value, spaces
   re-encoded as [_] so downstream arbitrary-value decoding treats them as
   spaces. Unresolved theme() calls are left verbatim. *)
let resolve_theme_functions s =
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
      (match if closed then theme_resolve_path inner else None with
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

(* Parse a single class string into a Tw.t *)
let of_string ?(theme = Theme.default) class_str =
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
    match Modifiers.apply modifiers base_util with
    | Some u -> Ok u
    | None -> Error (`Msg ("Unknown modifier in: " ^ class_str))
  in
  (* Resolve theme() dot-paths for dispatch, keeping the original spelling as
     the class-name alias so the utility still round-trips. *)
  let resolved_base = resolve_theme_functions base_class in
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
      | None ->
          (* An arbitrary-property class ([prop:value]) that no handler accepted
             gets actionable feedback: only colour properties with an /opacity
             modifier are emitted today. *)
          if
            String.length base_class > 2
            && base_class.[0] = '['
            && String.contains base_class ':'
          then
            Error
              (`Msg
                 ("Unsupported arbitrary property '" ^ class_str
                ^ "': only colour properties with an /opacity modifier are \
                   emitted (e.g. [color:var(--x)]/50); plain [--name:value] \
                   declarations and non-colour properties are not yet \
                   supported"))
          else Error (`Msg ("Unknown class: " ^ class_str)))

let str s =
  let classes = split_whitespace s in
  List.map
    (fun cls ->
      match of_string cls with Ok t -> t | Error (`Msg msg) -> invalid_arg msg)
    classes

(** {1 Module Exports} *)

module Css = Cascade.Css
module Theme = Theme

module Private = struct
  module Accessibility = Accessibility
  module Alignment = Alignment
  module Animations = Animations
  module Arbitrary = Arbitrary
  module Backgrounds = Backgrounds
  module Borders = Borders
  module Box_sizing = Box_sizing
  module Build = Build
  module Color = Color
  module Columns = Columns
  module Contain = Contain
  module Containers = Containers
  module Cursor = Cursor
  module Divide = Divide
  module Effects = Effects
  module Field_sizing = Field_sizing
  module Filters = Filters
  module Flex = Flex
  module Flex_layout = Flex_layout
  module Flex_props = Flex_props
  module Forms = Forms
  module Gap = Gap
  module Grid = Grid
  module Grid_item = Grid_item
  module Grid_template = Grid_template
  module Interactivity = Interactivity
  module Layout = Layout
  module Margin = Margin
  module Mask_gradient = Mask_gradient
  module Masks = Masks
  module Modifiers = Modifiers
  module Output = Output
  module Overflow = Overflow
  module Overflow_wrap = Overflow_wrap
  module Overscroll = Overscroll
  module Padding = Padding
  module Position = Position
  module Property = Property
  module Prose = Prose
  module Rule = Rule
  module Scroll = Scroll
  module Scrollbar = Scrollbar
  module Sizing = Sizing
  module Spacing = Spacing
  module Spacing_scale = Spacing_scale
  module Style = Style
  module Svg = Svg
  module Tab = Tab
  module Tables = Tables
  module Text_shadow = Text_shadow
  module Touch = Touch
  module Transforms = Transforms
  module Transitions = Transitions
  module Typography = Typography
  module Utility = Utility
  module Var = Var
  module Zoom = Zoom
end

(* Include flex utilities *)
include Flex

(* Include grid utilities *)
include Grid

(* Include cursor utilities *)
include Cursor
