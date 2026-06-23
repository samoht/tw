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
    ?(layers = Build.default_config.layers) utilities =
  Build.to_css ?theme ~config:{ base; forms; layers } utilities

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
   class name. Rewrite the trailing [(--x)] to [[var(--x)]] for parsing; the
   original spelling is restored via [Utility.alias]. Returns [None] when there
   is no paren shorthand. *)
let normalize_paren_var base_class =
  let n = String.length base_class in
  if n > 4 && base_class.[n - 1] = ')' then
    match String.rindex_opt base_class '(' with
    | Some lp
      when lp > 0
           && base_class.[lp - 1] = '-'
           && lp + 2 < n
           && base_class.[lp + 1] = '-'
           && base_class.[lp + 2] = '-' ->
        let prefix = String.sub base_class 0 lp in
        let inner = String.sub base_class (lp + 1) (n - lp - 2) in
        Some (prefix ^ "[var(" ^ inner ^ ")]")
    | _ -> None
  else None

(* Parse a single class string into a Tw.t *)
let of_string ?(theme = Scheme.default) class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  (* The [!] important marker sits right next to the utility: the v3 prefix
     ([!flex], [md:!flex]) or the v4 trailing form ([flex!]). Each keeps its
     form in the generated selector so it matches the source class. *)
  let importance, base_class =
    let n = String.length base_class in
    if n > 1 && base_class.[0] = '!' then
      (`Prefix, String.sub base_class 1 (n - 1))
    else if n > 1 && base_class.[n - 1] = '!' then
      (`Suffix, String.sub base_class 0 (n - 1))
    else (`None, base_class)
  in
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
  match Utility.base_of_class theme base_class with
  | Ok base_utility -> finish base_utility
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
