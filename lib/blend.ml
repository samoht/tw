(** Blend mode utilities: mix-blend-mode and background-blend-mode. *)

module Css = Cascade.Css

module Handler = struct
  open Style
  open Css

  type t =
    | Mix_blend_normal
    | Mix_blend_multiply
    | Mix_blend_screen
    | Mix_blend_overlay
    | Mix_blend_darken
    | Mix_blend_lighten
    | Mix_blend_color_dodge
    | Mix_blend_color_burn
    | Mix_blend_hard_light
    | Mix_blend_soft_light
    | Mix_blend_difference
    | Mix_blend_exclusion
    | Mix_blend_hue
    | Mix_blend_saturation
    | Mix_blend_color
    | Mix_blend_luminosity
    | Mix_blend_plus_darker
    | Mix_blend_plus_lighter
    | Bg_blend_normal
    | Bg_blend_multiply
    | Bg_blend_screen
    | Bg_blend_overlay
    | Bg_blend_darken
    | Bg_blend_lighten
    | Bg_blend_color_dodge
    | Bg_blend_color_burn
    | Bg_blend_hard_light
    | Bg_blend_soft_light
    | Bg_blend_difference
    | Bg_blend_exclusion
    | Bg_blend_hue
    | Bg_blend_saturation
    | Bg_blend_color
    | Bg_blend_luminosity

  let name = "blend"
  let priority _ = 27
  let mix_blend_normal = style [ mix_blend_mode Normal ]
  let mix_blend_multiply = style [ mix_blend_mode Multiply ]
  let mix_blend_screen = style [ mix_blend_mode Screen ]
  let mix_blend_overlay = style [ mix_blend_mode Overlay ]
  let mix_blend_darken = style [ mix_blend_mode Darken ]
  let mix_blend_lighten = style [ mix_blend_mode Lighten ]
  let mix_blend_color_dodge = style [ mix_blend_mode Color_dodge ]
  let mix_blend_color_burn = style [ mix_blend_mode Color_burn ]
  let mix_blend_hard_light = style [ mix_blend_mode Hard_light ]
  let mix_blend_soft_light = style [ mix_blend_mode Soft_light ]
  let mix_blend_difference = style [ mix_blend_mode Difference ]
  let mix_blend_exclusion = style [ mix_blend_mode Exclusion ]
  let mix_blend_hue = style [ mix_blend_mode Hue ]
  let mix_blend_saturation = style [ mix_blend_mode Saturation ]
  let mix_blend_color = style [ mix_blend_mode Color ]
  let mix_blend_luminosity = style [ mix_blend_mode Luminosity ]
  let mix_blend_plus_darker = style [ mix_blend_mode Plus_darker ]
  let mix_blend_plus_lighter = style [ mix_blend_mode Plus_lighter ]

  (* Background blend mode styles *)
  let bg_blend_normal = style [ background_blend_mode Normal ]
  let bg_blend_multiply = style [ background_blend_mode Multiply ]
  let bg_blend_screen = style [ background_blend_mode Screen ]
  let bg_blend_overlay = style [ background_blend_mode Overlay ]
  let bg_blend_darken = style [ background_blend_mode Darken ]
  let bg_blend_lighten = style [ background_blend_mode Lighten ]
  let bg_blend_color_dodge = style [ background_blend_mode Color_dodge ]
  let bg_blend_color_burn = style [ background_blend_mode Color_burn ]
  let bg_blend_hard_light = style [ background_blend_mode Hard_light ]
  let bg_blend_soft_light = style [ background_blend_mode Soft_light ]
  let bg_blend_difference = style [ background_blend_mode Difference ]
  let bg_blend_exclusion = style [ background_blend_mode Exclusion ]
  let bg_blend_hue = style [ background_blend_mode Hue ]
  let bg_blend_saturation = style [ background_blend_mode Saturation ]
  let bg_blend_color = style [ background_blend_mode Color ]
  let bg_blend_luminosity = style [ background_blend_mode Luminosity ]

  let to_style _theme = function
    | Mix_blend_normal -> mix_blend_normal
    | Mix_blend_multiply -> mix_blend_multiply
    | Mix_blend_screen -> mix_blend_screen
    | Mix_blend_overlay -> mix_blend_overlay
    | Mix_blend_darken -> mix_blend_darken
    | Mix_blend_lighten -> mix_blend_lighten
    | Mix_blend_color_dodge -> mix_blend_color_dodge
    | Mix_blend_color_burn -> mix_blend_color_burn
    | Mix_blend_hard_light -> mix_blend_hard_light
    | Mix_blend_soft_light -> mix_blend_soft_light
    | Mix_blend_difference -> mix_blend_difference
    | Mix_blend_exclusion -> mix_blend_exclusion
    | Mix_blend_hue -> mix_blend_hue
    | Mix_blend_saturation -> mix_blend_saturation
    | Mix_blend_color -> mix_blend_color
    | Mix_blend_luminosity -> mix_blend_luminosity
    | Mix_blend_plus_darker -> mix_blend_plus_darker
    | Mix_blend_plus_lighter -> mix_blend_plus_lighter
    | Bg_blend_normal -> bg_blend_normal
    | Bg_blend_multiply -> bg_blend_multiply
    | Bg_blend_screen -> bg_blend_screen
    | Bg_blend_overlay -> bg_blend_overlay
    | Bg_blend_darken -> bg_blend_darken
    | Bg_blend_lighten -> bg_blend_lighten
    | Bg_blend_color_dodge -> bg_blend_color_dodge
    | Bg_blend_color_burn -> bg_blend_color_burn
    | Bg_blend_hard_light -> bg_blend_hard_light
    | Bg_blend_soft_light -> bg_blend_soft_light
    | Bg_blend_difference -> bg_blend_difference
    | Bg_blend_exclusion -> bg_blend_exclusion
    | Bg_blend_hue -> bg_blend_hue
    | Bg_blend_saturation -> bg_blend_saturation
    | Bg_blend_color -> bg_blend_color
    | Bg_blend_luminosity -> bg_blend_luminosity

  let err_not_utility = Error (`Msg "Not a blend utility")

  let of_class _theme class_name =
    match Parse.split_class class_name with
    | [ "mix"; "blend"; "normal" ] -> Ok Mix_blend_normal
    | [ "mix"; "blend"; "multiply" ] -> Ok Mix_blend_multiply
    | [ "mix"; "blend"; "screen" ] -> Ok Mix_blend_screen
    | [ "mix"; "blend"; "overlay" ] -> Ok Mix_blend_overlay
    | [ "mix"; "blend"; "darken" ] -> Ok Mix_blend_darken
    | [ "mix"; "blend"; "lighten" ] -> Ok Mix_blend_lighten
    | [ "mix"; "blend"; "color"; "dodge" ] -> Ok Mix_blend_color_dodge
    | [ "mix"; "blend"; "color"; "burn" ] -> Ok Mix_blend_color_burn
    | [ "mix"; "blend"; "hard"; "light" ] -> Ok Mix_blend_hard_light
    | [ "mix"; "blend"; "soft"; "light" ] -> Ok Mix_blend_soft_light
    | [ "mix"; "blend"; "difference" ] -> Ok Mix_blend_difference
    | [ "mix"; "blend"; "exclusion" ] -> Ok Mix_blend_exclusion
    | [ "mix"; "blend"; "hue" ] -> Ok Mix_blend_hue
    | [ "mix"; "blend"; "saturation" ] -> Ok Mix_blend_saturation
    | [ "mix"; "blend"; "color" ] -> Ok Mix_blend_color
    | [ "mix"; "blend"; "luminosity" ] -> Ok Mix_blend_luminosity
    | [ "mix"; "blend"; "plus"; "darker" ] -> Ok Mix_blend_plus_darker
    | [ "mix"; "blend"; "plus"; "lighter" ] -> Ok Mix_blend_plus_lighter
    | [ "bg"; "blend"; "normal" ] -> Ok Bg_blend_normal
    | [ "bg"; "blend"; "multiply" ] -> Ok Bg_blend_multiply
    | [ "bg"; "blend"; "screen" ] -> Ok Bg_blend_screen
    | [ "bg"; "blend"; "overlay" ] -> Ok Bg_blend_overlay
    | [ "bg"; "blend"; "darken" ] -> Ok Bg_blend_darken
    | [ "bg"; "blend"; "lighten" ] -> Ok Bg_blend_lighten
    | [ "bg"; "blend"; "color"; "dodge" ] -> Ok Bg_blend_color_dodge
    | [ "bg"; "blend"; "color"; "burn" ] -> Ok Bg_blend_color_burn
    | [ "bg"; "blend"; "hard"; "light" ] -> Ok Bg_blend_hard_light
    | [ "bg"; "blend"; "soft"; "light" ] -> Ok Bg_blend_soft_light
    | [ "bg"; "blend"; "difference" ] -> Ok Bg_blend_difference
    | [ "bg"; "blend"; "exclusion" ] -> Ok Bg_blend_exclusion
    | [ "bg"; "blend"; "hue" ] -> Ok Bg_blend_hue
    | [ "bg"; "blend"; "saturation" ] -> Ok Bg_blend_saturation
    | [ "bg"; "blend"; "color" ] -> Ok Bg_blend_color
    | [ "bg"; "blend"; "luminosity" ] -> Ok Bg_blend_luminosity
    | _ -> err_not_utility

  let to_class = function
    | Mix_blend_normal -> "mix-blend-normal"
    | Mix_blend_multiply -> "mix-blend-multiply"
    | Mix_blend_screen -> "mix-blend-screen"
    | Mix_blend_overlay -> "mix-blend-overlay"
    | Mix_blend_darken -> "mix-blend-darken"
    | Mix_blend_lighten -> "mix-blend-lighten"
    | Mix_blend_color_dodge -> "mix-blend-color-dodge"
    | Mix_blend_color_burn -> "mix-blend-color-burn"
    | Mix_blend_hard_light -> "mix-blend-hard-light"
    | Mix_blend_soft_light -> "mix-blend-soft-light"
    | Mix_blend_difference -> "mix-blend-difference"
    | Mix_blend_exclusion -> "mix-blend-exclusion"
    | Mix_blend_hue -> "mix-blend-hue"
    | Mix_blend_saturation -> "mix-blend-saturation"
    | Mix_blend_color -> "mix-blend-color"
    | Mix_blend_luminosity -> "mix-blend-luminosity"
    | Mix_blend_plus_darker -> "mix-blend-plus-darker"
    | Mix_blend_plus_lighter -> "mix-blend-plus-lighter"
    | Bg_blend_normal -> "bg-blend-normal"
    | Bg_blend_multiply -> "bg-blend-multiply"
    | Bg_blend_screen -> "bg-blend-screen"
    | Bg_blend_overlay -> "bg-blend-overlay"
    | Bg_blend_darken -> "bg-blend-darken"
    | Bg_blend_lighten -> "bg-blend-lighten"
    | Bg_blend_color_dodge -> "bg-blend-color-dodge"
    | Bg_blend_color_burn -> "bg-blend-color-burn"
    | Bg_blend_hard_light -> "bg-blend-hard-light"
    | Bg_blend_soft_light -> "bg-blend-soft-light"
    | Bg_blend_difference -> "bg-blend-difference"
    | Bg_blend_exclusion -> "bg-blend-exclusion"
    | Bg_blend_hue -> "bg-blend-hue"
    | Bg_blend_saturation -> "bg-blend-saturation"
    | Bg_blend_color -> "bg-blend-color"
    | Bg_blend_luminosity -> "bg-blend-luminosity"

  let suborder = function
    (* Background blend modes come after opacity, before mix-blend *)
    | Bg_blend_color -> 22000
    | Bg_blend_color_burn -> 22001
    | Bg_blend_color_dodge -> 22002
    | Bg_blend_darken -> 22003
    | Bg_blend_difference -> 22004
    | Bg_blend_exclusion -> 22005
    | Bg_blend_hard_light -> 22006
    | Bg_blend_hue -> 22007
    | Bg_blend_lighten -> 22008
    | Bg_blend_luminosity -> 22009
    | Bg_blend_multiply -> 22010
    | Bg_blend_normal -> 22011
    | Bg_blend_overlay -> 22012
    | Bg_blend_saturation -> 22013
    | Bg_blend_screen -> 22014
    | Bg_blend_soft_light -> 22015
    (* Mix blend modes come after bg-blend, before shadows *)
    | Mix_blend_color -> 24000
    | Mix_blend_color_burn -> 24001
    | Mix_blend_color_dodge -> 24002
    | Mix_blend_darken -> 24003
    | Mix_blend_difference -> 24004
    | Mix_blend_exclusion -> 24005
    | Mix_blend_hard_light -> 24006
    | Mix_blend_hue -> 24007
    | Mix_blend_lighten -> 24008
    | Mix_blend_luminosity -> 24009
    | Mix_blend_multiply -> 24010
    | Mix_blend_normal -> 24011
    | Mix_blend_overlay -> 24012
    | Mix_blend_plus_darker -> 24013
    | Mix_blend_plus_lighter -> 24014
    | Mix_blend_saturation -> 24015
    | Mix_blend_screen -> 24016
    | Mix_blend_soft_light -> 24017

  let examples = [ Mix_blend_normal; Bg_blend_normal ]
end

open Handler
module Utility_factory = Utility.Make (Handler)

let utility = Utility_factory.v
let mix_blend_normal = utility Mix_blend_normal
let mix_blend_multiply = utility Mix_blend_multiply
let mix_blend_screen = utility Mix_blend_screen
let mix_blend_overlay = utility Mix_blend_overlay
let mix_blend_darken = utility Mix_blend_darken
let mix_blend_lighten = utility Mix_blend_lighten
let mix_blend_color_dodge = utility Mix_blend_color_dodge
let mix_blend_color_burn = utility Mix_blend_color_burn
let mix_blend_hard_light = utility Mix_blend_hard_light
let mix_blend_soft_light = utility Mix_blend_soft_light
let mix_blend_difference = utility Mix_blend_difference
let mix_blend_exclusion = utility Mix_blend_exclusion
let mix_blend_hue = utility Mix_blend_hue
let mix_blend_saturation = utility Mix_blend_saturation
let mix_blend_color = utility Mix_blend_color
let mix_blend_luminosity = utility Mix_blend_luminosity
let mix_blend_plus_darker = utility Mix_blend_plus_darker
let mix_blend_plus_lighter = utility Mix_blend_plus_lighter
let bg_blend_normal = utility Bg_blend_normal
let bg_blend_multiply = utility Bg_blend_multiply
let bg_blend_screen = utility Bg_blend_screen
let bg_blend_overlay = utility Bg_blend_overlay
let bg_blend_darken = utility Bg_blend_darken
let bg_blend_lighten = utility Bg_blend_lighten
let bg_blend_color_dodge = utility Bg_blend_color_dodge
let bg_blend_color_burn = utility Bg_blend_color_burn
let bg_blend_hard_light = utility Bg_blend_hard_light
let bg_blend_soft_light = utility Bg_blend_soft_light
let bg_blend_difference = utility Bg_blend_difference
let bg_blend_exclusion = utility Bg_blend_exclusion
let bg_blend_hue = utility Bg_blend_hue
let bg_blend_saturation = utility Bg_blend_saturation
let bg_blend_color = utility Bg_blend_color
let bg_blend_luminosity = utility Bg_blend_luminosity
