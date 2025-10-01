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

include Style
include Color
include Backgrounds
include Margin
include Gap
include Padding
include Sizing
include Typography
include Layout
include Display
include Grid
include Grid_template
include Flex
include Alignment
include Borders
include Effects
include Transforms
include Cursor
include Interactivity
include Containers
include Filters
include Clipping
include Positioning
include Animations
include Forms
include Tables
include Svg
include Accessibility
include Modifiers
include Prose

(* CSS rule generation from Rules module *)
let to_css ?(base = Rules.default_config.base)
    ?(mode = Rules.default_config.mode)
    ?(optimize = Rules.default_config.optimize) tw_classes =
  Rules.to_css ~config:{ base; mode; optimize } tw_classes

let to_inline_style = Rules.to_inline_style
let preflight = Preflight.stylesheet

(* Class generation functions *)
let rec pp = function
  | Style { name = class_name; _ } -> class_name
  | Modified (modifier, t) ->
      let base_class = pp t in
      Modifiers.pp_modifier modifier ^ ":" ^ base_class
  | Group styles -> styles |> List.map pp |> String.concat " "

let to_classes styles = styles |> List.map pp |> String.concat " "
let modifiers_of_string = Modifiers.of_string

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  let parts = String.split_on_char '-' base_class in
  match Utility.base_of_string parts with
  | Error _ -> Error (`Msg ("Unknown class: " ^ class_str))
  | Ok base_utility ->
      let base_style = Utility.base_to_style base_utility in
      Ok (Modifiers.apply modifiers base_style)

(** {1 Module Exports} *)

module Style = Style
module Parse = Parse
module Margin = Margin
module Padding = Padding
module Gap = Gap
module Display = Display
module Flex = Flex
module Alignment = Alignment
module Cursor = Cursor
module Borders = Borders
module Backgrounds = Backgrounds
module Sizing = Sizing
module Layout = Layout
module Grid = Grid
module Grid_template = Grid_template
module Typography = Typography
module Effects = Effects
module Transforms = Transforms
module Interactivity = Interactivity
module Containers = Containers
module Filters = Filters
module Positioning = Positioning
module Animations = Animations
module Forms = Forms
module Tables = Tables
module Svg = Svg
module Accessibility = Accessibility
module Rules = Rules
module Prose = Prose
module Css = Css
module Color = Color
module Modifiers = Modifiers
module Var = Var
module Theme = Theme
module Utility = Utility
