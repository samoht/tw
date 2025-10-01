(** Utility module for common utility types and functions *)

(** Base utility type without modifiers *)
type base =
  | Positioning of Positioning.utility
  | Grid of Grid.utility
  | Margin of Margin.utility
  | Containers of Containers.utility
  | Prose of Prose.utility
  | Display of Display.utility
  | Layout of Layout.utility
  | Tables of Tables.utility
  | Sizing of Sizing.utility
  | Cursor of Cursor.utility
  | Grid_template of Grid_template.utility
  | Flex of Flex.utility
  | Alignment of Alignment.utility
  | Gap of Gap.utility
  | Borders of Borders.utility
  | Backgrounds of Backgrounds.utility
  | Padding of Padding.utility
  | Typography of Typography.utility
  | Color of Color.utility
  | Effects of Effects.utility
  | Filters of Filters.utility
  | Transforms of Transforms.utility
  | Animations of Animations.utility
  | Interactivity of Interactivity.utility
  | Forms of Forms.utility
  | Svg of Svg.utility
  | Accessibility of Accessibility.utility

(** Utility with optional modifiers *)
type t = Utility of base | Modified of Style.modifier * t | Group of t list

(** Parse CSS string into AST *)
let css_of_string = Css.of_string

(** Parse a class string into a base utility (without modifiers) *)
let base_of_string parts =
  let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2 in
  Result.map (fun u -> Backgrounds u) (Backgrounds.of_string parts)
  <|> Result.map (fun u -> Color u) (Color.utility_of_string parts)
  <|> Result.map (fun u -> Padding u) (Padding.of_string parts)
  <|> Result.map (fun u -> Margin u) (Margin.of_string parts)
  <|> Result.map (fun u -> Gap u) (Gap.of_string parts)
  <|> Result.map (fun u -> Sizing u) (Sizing.of_string parts)
  <|> Result.map (fun u -> Layout u) (Layout.of_string parts)
  <|> Result.map (fun u -> Display u) (Display.of_string parts)
  <|> Result.map (fun u -> Grid u) (Grid.of_string parts)
  <|> Result.map (fun u -> Grid_template u) (Grid_template.of_string parts)
  <|> Result.map (fun u -> Flex u) (Flex.of_string parts)
  <|> Result.map (fun u -> Alignment u) (Alignment.of_string parts)
  <|> Result.map (fun u -> Typography u) (Typography.of_string parts)
  <|> Result.map (fun u -> Borders u) (Borders.of_string parts)
  <|> Result.map (fun u -> Effects u) (Effects.of_string parts)
  <|> Result.map (fun u -> Transforms u) (Transforms.of_string parts)
  <|> Result.map (fun u -> Cursor u) (Cursor.of_string parts)
  <|> Result.map (fun u -> Interactivity u) (Interactivity.of_string parts)
  <|> Result.map (fun u -> Containers u) (Containers.of_string parts)
  <|> Result.map (fun u -> Filters u) (Filters.of_string parts)
  <|> Result.map (fun u -> Positioning u) (Positioning.of_string parts)
  <|> Result.map (fun u -> Animations u) (Animations.of_string parts)
  <|> Result.map (fun u -> Forms u) (Forms.of_string parts)
  <|> Result.map (fun u -> Tables u) (Tables.of_string parts)
  <|> Result.map (fun u -> Svg u) (Svg.of_string parts)
  <|> Result.map (fun u -> Accessibility u) (Accessibility.of_string parts)
  <|> Result.map (fun u -> Prose u) (Prose.of_string parts)

(** Convert base utility to Core.t (style) *)
let base_to_style = function
  | Positioning u -> Positioning.to_style u
  | Grid u -> Grid.to_style u
  | Margin u -> Margin.to_style u
  | Containers u -> Containers.to_style u
  | Prose u -> Prose.to_style u
  | Display u -> Display.to_style u
  | Layout u -> Layout.to_style u
  | Tables u -> Tables.to_style u
  | Sizing u -> Sizing.to_style u
  | Cursor u -> Cursor.to_style u
  | Grid_template u -> Grid_template.to_style u
  | Flex u -> Flex.to_style u
  | Alignment u -> Alignment.to_style u
  | Gap u -> Gap.to_style u
  | Borders u -> Borders.to_style u
  | Backgrounds u -> Backgrounds.to_style u
  | Padding u -> Padding.to_style u
  | Typography u -> Typography.to_style u
  | Color u -> Color.to_style u
  | Effects u -> Effects.to_style u
  | Filters u -> Filters.to_style u
  | Transforms u -> Transforms.to_style u
  | Animations u -> Animations.to_style u
  | Interactivity u -> Interactivity.to_style u
  | Forms u -> Forms.to_style u
  | Svg u -> Svg.to_style u
  | Accessibility u -> Accessibility.to_style u

(** Convert Utility.t (with modifiers) to Core.t *)
let rec to_style = function
  | Utility u -> base_to_style u
  | Modified (m, u) -> Style.Modified (m, to_style u)
  | Group us -> Style.Group (List.map to_style us)

(** Get priority for a base utility constructor *)
let priority = function
  | Positioning _ -> 0
  | Grid _ -> 1
  | Margin _ | Containers _ | Prose _ -> 2
  | Display _ | Layout _ | Tables _ -> 10
  | Sizing _ -> 12
  | Cursor _ -> 13
  | Grid_template _ -> 14
  | Flex _ -> 15
  | Alignment _ | Gap _ -> 16
  | Borders _ -> 17
  | Backgrounds _ -> 18
  | Padding _ -> 19
  | Typography _ -> 100
  (* Background colors have same priority as Backgrounds (18), text colors have
     Typography priority (100) *)
  | Color c -> if Color.is_background_color c then 18 else 100
  | Effects _ | Filters _ -> 700
  | Transforms _ -> 710
  | Animations _ -> 720
  | Interactivity _ | Forms _ | Svg _ | Accessibility _ -> 800

(** Get ordering information (priority, suborder) for a base utility *)
let order (u : base) : int * int =
  let p = priority u in
  match u with
  | Positioning u -> (p, Positioning.suborder u)
  | Grid u -> (p, Grid.suborder u)
  | Margin u -> (p, Margin.suborder u)
  | Containers u -> (p, Containers.suborder u)
  | Prose u -> (p, Prose.suborder u)
  | Display u -> (p, Display.suborder u)
  | Layout u -> (p, Layout.suborder u)
  | Tables u -> (p, Tables.suborder u)
  | Sizing u -> (p, Sizing.suborder u)
  | Cursor u -> (p, Cursor.suborder u)
  | Grid_template u -> (p, Grid_template.suborder u)
  | Flex u -> (p, Flex.suborder u)
  | Alignment u -> (p, Alignment.suborder u)
  | Gap u -> (p, Gap.suborder u)
  | Borders u -> (p, Borders.suborder u)
  | Backgrounds u -> (p, Backgrounds.suborder u)
  | Padding u -> (p, Padding.suborder u)
  | Typography u -> (p, Typography.suborder u)
  | Color u -> (p, Color.suborder u)
  | Effects u -> (p, Effects.suborder u)
  | Filters u -> (p, Filters.suborder u)
  | Transforms u -> (p, Transforms.suborder u)
  | Animations u -> (p, Animations.suborder u)
  | Interactivity u -> (p, Interactivity.suborder u)
  | Forms u -> (p, Forms.suborder u)
  | Svg u -> (p, Svg.suborder u)
  | Accessibility u -> (p, Accessibility.suborder u)

(** Deduplicate utilities while preserving order (last occurrence wins) *)
let deduplicate utilities =
  let rec go seen acc = function
    | [] -> List.rev acc
    | u :: rest ->
        if List.mem u seen then go seen acc rest
        else go (u :: seen) (u :: acc) rest
  in
  go [] [] (List.rev utilities)
