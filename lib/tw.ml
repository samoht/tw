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

include Core
include Color
include Backgrounds
include Spacing
include Sizing
include Typography
include Layout
include Flow
include Borders
include Effects
include Transforms
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
let to_css = Rules.to_css
let to_inline_style = Rules.to_inline_style

(* Prose stylesheet *)
let prose_stylesheet () =
  let rules = Prose.stylesheet () in
  Css.stylesheet (rules |> List.map (fun rule -> Css.Rule rule))

(* Preflight reset rules *)
let preflight = Preflight.stylesheet

(* Class generation functions *)
let rec pp = function
  | Style { name = class_name; _ } -> class_name
  | Modified (modifier, t) ->
      let base_class = pp t in
      Modifiers.pp_modifier modifier ^ ":" ^ base_class
  | Group styles -> styles |> List.map pp |> String.concat " "

let to_classes styles = styles |> List.map pp |> String.concat " "

(* Helper for "try this or else try that" *)
let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

(* Modifiers parsing is now in Modifiers module *)
let modifiers_of_string = Modifiers.of_string

(* Apply modifiers to a base style *)

(* Parse a single class string into a Tw.t *)
let of_string class_str =
  let modifiers, base_class = modifiers_of_string class_str in
  let parts = String.split_on_char '-' base_class in
  let base_result =
    Color.classes_of_string parts
    <|> Spacing.of_string parts <|> Sizing.of_string parts
    <|> Layout.of_string parts <|> Flow.of_string parts
    <|> Typography.of_string parts <|> Borders.of_string parts
    <|> Effects.of_string parts <|> Transforms.of_string parts
    <|> Interactivity.of_string parts
    <|> Containers.of_string parts <|> Filters.of_string parts
    <|> Positioning.of_string parts
    <|> Animations.of_string parts <|> Forms.of_string parts
    <|> Tables.of_string parts <|> Svg.of_string parts
    <|> Accessibility.of_string parts
    <|> Prose.of_string parts
    <|> Error (`Msg ("Unknown class: " ^ class_str))
  in
  match base_result with
  | Error _ as e -> e
  | Ok base_style -> Ok (Modifiers.apply modifiers base_style)

(** {1 Module Exports} *)

module Core = Core
module Parse = Parse
module Spacing = Spacing
module Borders = Borders
module Backgrounds = Backgrounds
module Sizing = Sizing
module Layout = Layout
module Flow = Flow
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
