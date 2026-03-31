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

let to_css ?(base = Build.default_config.base) ?forms
    ?(mode = Build.default_config.mode) ?(layers = Build.default_config.layers)
    ?(optimize = Build.default_config.optimize) utilities =
  Build.to_css ~config:{ base; forms; mode; layers; optimize } utilities

let to_inline_style utilities = Build.to_inline_style utilities
let preflight = Preflight.stylesheet

(* Class generation functions *)
let pp utility = Utility.to_class utility
let to_classes styles = styles |> List.map Utility.to_class |> String.concat " "
let modifiers_of_string = Modifiers.of_string

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  match Utility.base_of_class base_class with
  | Error _ -> Error (`Msg ("Unknown class: " ^ class_str))
  | Ok base_utility -> (
      let base_util = Utility.base base_utility in
      match Modifiers.apply modifiers base_util with
      | Some u -> Ok u
      | None -> Error (`Msg ("Unknown modifier in: " ^ class_str)))

let str s =
  let classes = String.split_on_char ' ' s |> List.filter (fun s -> s <> "") in
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
