(** Sizing utilities for width and height *)

module Css = Cascade.Css

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

module Handler = struct
  let class_float = Pp.float

  open Style
  open Css

  type t =
    (* Width utilities *)
    | W_auto
    | W_full
    | W_screen
    | W_min
    | W_max
    | W_fit
    | W_px
    | W_spacing of float
    | W_fraction of string
    | W_arbitrary of string * Css.length
    | W_dvw (* 100dvw - dynamic viewport width *)
    | W_lvw (* 100lvw - large viewport width *)
    | W_svw (* 100svw - small viewport width *)
    | W_container of string (* width: var(--container-<name>) *)
    (* Height utilities *)
    | H_auto
    | H_full
    | H_screen
    | H_min
    | H_max
    | H_fit
    | H_px
    | H_spacing of float
    | H_fraction of string
    | H_arbitrary of string * Css.length
    | H_dvh (* 100dvh - dynamic viewport height *)
    | H_lvh (* 100lvh - large viewport height *)
    | H_svh (* 100svh - small viewport height *)
    | H_dvw
    | H_lvw
    | H_svw
    | H_lh (* 1lh - line height *)
    (* Min-width utilities *)
    | Min_w_0
    | Min_w_full
    | Min_w_min
    | Min_w_max
    | Min_w_fit
    | Min_w_auto
    | Min_w_spacing of float
    | Min_w_arbitrary of string * Css.length
    | Min_w_container of string (* min-width: var(--container-<name>) *)
    (* Max-width utilities *)
    | Max_w_none
    | Max_w_xs
    | Max_w_sm
    | Max_w_md
    | Max_w_lg
    | Max_w_xl
    | Max_w_2xl
    | Max_w_3xl
    | Max_w_4xl
    | Max_w_5xl
    | Max_w_6xl
    | Max_w_7xl
    | Max_w_full
    | Max_w_min
    | Max_w_max
    | Max_w_fit
    | Max_w_prose
    | Max_w_screen_sm
    | Max_w_screen_md
    | Max_w_screen_lg
    | Max_w_screen_xl
    | Max_w_screen_2xl
    | Max_w_spacing of float
    | Max_w_arbitrary of string * Css.length
    (* Min-height utilities *)
    | Min_h_0
    | Min_h_full
    | Min_h_screen
    | Min_h_min
    | Min_h_max
    | Min_h_fit
    | Min_h_auto
    | Min_h_dvh
    | Min_h_lvh
    | Min_h_svh
    | Min_h_px
    | Min_h_dvw
    | Min_h_lvw
    | Min_h_svw
    | Min_h_lh
    | Min_h_spacing of float
    | Min_h_arbitrary of string * Css.length
    (* Max-height utilities *)
    | Max_h_none
    | Max_h_full
    | Max_h_screen
    | Max_h_min
    | Max_h_max
    | Max_h_fit
    | Max_h_dvh
    | Max_h_lvh
    | Max_h_svh
    | Max_h_px
    | Max_h_dvw
    | Max_h_lvw
    | Max_h_svw
    | Max_h_lh
    | Max_h_spacing of float
    | Max_h_arbitrary of string * Css.length
    (* Size utilities (both width and height) *)
    | Size_auto
    | Size_full
    | Size_min
    | Size_max
    | Size_fit
    | Size_spacing of float
    | Size_fraction of string
    | Size_arbitrary of string * Css.length
    (* inline-size utilities *)
    | Inline_fraction of string
    | Inline_spacing of float
    | Inline_arbitrary of string * Css.length
    | Inline_auto
    | Inline_dvw
    | Inline_fit
    | Inline_full
    | Inline_lvw
    | Inline_max
    | Inline_min
    | Inline_screen
    | Inline_svw
    | Inline_container of string
    (* min-inline-size utilities *)
    | Min_inline_spacing of float
    | Min_inline_arbitrary of string * Css.length
    | Min_inline_auto
    | Min_inline_fit
    | Min_inline_full
    | Min_inline_max
    | Min_inline_min
    | Min_inline_container of string
    (* max-inline-size utilities *)
    | Max_inline_spacing of float
    | Max_inline_arbitrary of string * Css.length
    | Max_inline_fit
    | Max_inline_full
    | Max_inline_max
    | Max_inline_none
    | Max_inline_container of string
    (* block-size utilities *)
    | Block_fraction of string
    | Block_spacing of float
    | Block_arbitrary of string * Css.length
    | Block_auto
    | Block_dvh
    | Block_fit
    | Block_full
    | Block_lh
    | Block_lvh
    | Block_max
    | Block_min
    | Block_screen
    | Block_svh
    (* min-block-size utilities *)
    | Min_block_spacing of float
    | Min_block_arbitrary of string * Css.length
    | Min_block_auto
    | Min_block_dvh
    | Min_block_fit
    | Min_block_full
    | Min_block_lh
    | Min_block_lvh
    | Min_block_max
    | Min_block_min
    | Min_block_screen
    | Min_block_svh
    (* max-block-size utilities *)
    | Max_block_spacing of float
    | Max_block_arbitrary of string * Css.length
    | Max_block_dvh
    | Max_block_fit
    | Max_block_full
    | Max_block_lh
    | Max_block_lvh
    | Max_block_max
    | Max_block_min
    | Max_block_none
    | Max_block_screen
    | Max_block_svh
    (* Aspect utilities *)
    | Aspect_auto
    | Aspect_square
    | Aspect_video
    | Aspect_ratio of float * float (* aspect-4/3, aspect-8.5/11 *)
    | Aspect_bracket of float * float (* aspect-[10/9] *)

  type Utility.base += Self of t

  let name = "sizing"

  (** Priority 6: Sizing utilities (w-*, h-*, max-w-*, etc.) come before
      flex-1/flex-col etc. in Tailwind's order. *)
  let priority _ = 6

  (** Helper to create spacing-based utilities with consistent pattern. [n] is
      in rem units (e.g., 16.0 for w-64). We convert to class units by
      multiplying by 4, since --spacing is 0.25rem. Uses calc(var(--spacing) *
      n) for Tailwind v4 compatibility. *)
  let spacing_utility ?theme css_prop n =
    let decl, spacing_value = Theme.spacing_calc_float ?theme (n *. 4.) in
    style (decl :: [ css_prop spacing_value ])

  let w' ?theme size =
    match size with
    | `None -> style [ width (Px 0.) ]
    | `Xs -> style [ width (Rem 0.5) ]
    | `Sm -> style [ width (Rem 1.0) ]
    | `Md -> style [ width (Rem 1.5) ]
    | `Lg -> style [ width (Rem 2.0) ]
    | `Xl -> style [ width (Rem 3.0) ]
    | `Xl_2 -> style [ width (Rem 4.0) ]
    | `Xl_3 -> style [ width (Rem 6.0) ]
    | `Full -> style [ width (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme width n

  let w_auto' = style [ width Auto ]
  let w_px' = style [ width (Px 1.0) ]
  let w_full' = style [ width (Pct 100.0) ]
  let w_screen' = style [ width (Vw 100.0) ]
  let w_min' = style [ width Min_content ]
  let w_max' = style [ width Max_content ]
  let w_fit' = style [ width Fit_content ]

  (* Int-based width function for Tailwind scale (n * 0.25rem) *)

  let h' ?theme size =
    match size with
    | `None -> style [ height (Px 0.) ]
    | `Xs -> style [ height (Rem 0.5) ]
    | `Sm -> style [ height (Rem 1.0) ]
    | `Md -> style [ height (Rem 1.5) ]
    | `Lg -> style [ height (Rem 2.0) ]
    | `Xl -> style [ height (Rem 3.0) ]
    | `Xl_2 -> style [ height (Rem 4.0) ]
    | `Xl_3 -> style [ height (Rem 6.0) ]
    | `Full -> style [ height (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme height n

  let h_auto' = style [ height Auto ]
  let h_px' = style [ height (Px 1.0) ]
  let h_full' = style [ height (Pct 100.0) ]
  let h_screen' = style [ height (Vh 100.0) ]
  let h_min' = style [ height Min_content ]
  let h_max' = style [ height Max_content ]
  let h_fit' = style [ height Fit_content ]

  (* Int-based height function for Tailwind scale (n * 0.25rem) *)

  let min_w' ?theme size =
    match size with
    | `None -> style [ min_width (Px 0.) ]
    | `Xs -> style [ min_width (Rem 0.5) ]
    | `Sm -> style [ min_width (Rem 1.0) ]
    | `Md -> style [ min_width (Rem 1.5) ]
    | `Lg -> style [ min_width (Rem 2.0) ]
    | `Xl -> style [ min_width (Rem 3.0) ]
    | `Xl_2 -> style [ min_width (Rem 4.0) ]
    | `Xl_3 -> style [ min_width (Rem 6.0) ]
    | `Full -> style [ min_width (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme min_width n

  let min_w_0' = style [ min_width (Px 0.) ]
  let min_w_full' = style [ min_width (Pct 100.0) ]
  let min_w_min' = style [ min_width Min_content ]
  let min_w_max' = style [ min_width Max_content ]
  let min_w_fit' = style [ min_width Fit_content ]

  (* Int-based min-width function for Tailwind scale (n * 0.25rem) *)

  let max_w' ?theme size =
    match size with
    | `None -> style [ max_width None ]
    | `Xs -> style [ max_width (Rem 20.0) ]
    | `Sm -> style [ max_width (Rem 24.0) ]
    | `Md -> style [ max_width (Rem 28.0) ]
    | `Lg -> style [ max_width (Rem 32.0) ]
    | `Xl -> style [ max_width (Rem 36.0) ]
    | `Xl_2 -> style [ max_width (Rem 42.0) ]
    | `Xl_3 -> style [ max_width (Rem 48.0) ]
    | `Full -> style [ max_width (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme max_width n

  (* Named width theme variable *)

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

  (* The container scale doubles as the named width scale in v4. Map a name to
     its theme var and default, and to its position in the scale. *)
  let container_binding = function
    | "3xs" -> Some (container_3xs, (Rem 16.0 : length))
    | "2xs" -> Some (container_2xs, Rem 18.0)
    | "xs" -> Some (container_xs, Rem 20.0)
    | "sm" -> Some (container_sm, Rem 24.0)
    | "md" -> Some (container_md, Rem 28.0)
    | "lg" -> Some (container_lg, Rem 32.0)
    | "xl" -> Some (container_xl, Rem 36.0)
    | "2xl" -> Some (container_2xl, Rem 42.0)
    | "3xl" -> Some (container_3xl, Rem 48.0)
    | "4xl" -> Some (container_4xl, Rem 56.0)
    | "5xl" -> Some (container_5xl, Rem 64.0)
    | "6xl" -> Some (container_6xl, Rem 72.0)
    | "7xl" -> Some (container_7xl, Rem 80.0)
    | _ -> None

  let container_order = function
    | "3xs" -> 0
    | "2xs" -> 1
    | "xs" -> 2
    | "sm" -> 3
    | "md" -> 4
    | "lg" -> 5
    | "xl" -> 6
    | "2xl" -> 7
    | "3xl" -> 8
    | "4xl" -> 9
    | "5xl" -> 10
    | "6xl" -> 11
    | "7xl" -> 12
    | _ -> 99

  (* Breakpoint theme vars, referenced by the (v3) max-w-screen-* utilities.
     Negative suborders keep them before --container-* in the theme layer, as
     Tailwind emits them. *)
  let breakpoint_sm = Var.theme Css.Length "breakpoint-sm" ~order:(5, -5)
  let breakpoint_md = Var.theme Css.Length "breakpoint-md" ~order:(5, -4)
  let breakpoint_lg = Var.theme Css.Length "breakpoint-lg" ~order:(5, -3)
  let breakpoint_xl = Var.theme Css.Length "breakpoint-xl" ~order:(5, -2)
  let breakpoint_2xl = Var.theme Css.Length "breakpoint-2xl" ~order:(5, -1)
  let max_w_none' = style [ max_width None ]

  let max_w_xs' =
    let decl, ref_ = Var.binding container_xs (Rem 20.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_sm' =
    let decl, ref_ = Var.binding container_sm (Rem 24.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_md' =
    let decl, ref_ = Var.binding container_md (Rem 28.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_lg' =
    let decl, ref_ = Var.binding container_lg (Rem 32.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_xl' =
    let decl, ref_ = Var.binding container_xl (Rem 36.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_2xl' =
    let decl, ref_ = Var.binding container_2xl (Rem 42.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_3xl' =
    let decl, ref_ = Var.binding container_3xl (Rem 48.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_4xl' =
    let decl, ref_ = Var.binding container_4xl (Rem 56.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_5xl' =
    let decl, ref_ = Var.binding container_5xl (Rem 64.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_6xl' =
    let decl, ref_ = Var.binding container_6xl (Rem 72.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_7xl' =
    let decl, ref_ = Var.binding container_7xl (Rem 80.0) in
    style [ decl; max_width (Var ref_) ]

  let max_w_full' = style [ max_width (Pct 100.0) ]
  let max_w_min' = style [ max_width Min_content ]
  let max_w_max' = style [ max_width Max_content ]
  let max_w_fit' = style [ max_width Fit_content ]
  let max_w_prose' = style [ max_width (Ch 65.0) ]

  let max_w_screen_of var rem =
    let decl, r = Var.binding var (Rem rem : Css.length) in
    style (decl :: [ max_width (Var r) ])

  let max_w_screen_sm' = max_w_screen_of breakpoint_sm 40.
  let max_w_screen_md' = max_w_screen_of breakpoint_md 48.
  let max_w_screen_lg' = max_w_screen_of breakpoint_lg 64.
  let max_w_screen_xl' = max_w_screen_of breakpoint_xl 80.
  let max_w_screen_2xl' = max_w_screen_of breakpoint_2xl 96.

  (* Int-based max-width function for Tailwind scale (n * 0.25rem) *)

  let min_h' ?theme size =
    match size with
    | `None -> style [ min_height (Px 0.) ]
    | `Xs -> style [ min_height (Rem 0.5) ]
    | `Sm -> style [ min_height (Rem 1.0) ]
    | `Md -> style [ min_height (Rem 1.5) ]
    | `Lg -> style [ min_height (Rem 2.0) ]
    | `Xl -> style [ min_height (Rem 3.0) ]
    | `Xl_2 -> style [ min_height (Rem 4.0) ]
    | `Xl_3 -> style [ min_height (Rem 6.0) ]
    | `Full -> style [ min_height (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme min_height n

  let min_h_0' = style [ min_height (Px 0.) ]
  let min_h_full' = style [ min_height (Pct 100.0) ]
  let min_h_screen' = style [ min_height (Vh 100.0) ]
  let min_h_min' = style [ min_height Min_content ]
  let min_h_max' = style [ min_height Max_content ]
  let min_h_fit' = style [ min_height Fit_content ]

  (* Int-based min-height function for Tailwind scale (n * 0.25rem) *)

  let max_h' ?theme size =
    match size with
    | `None -> style [ max_height None ]
    | `Xs -> style [ max_height (Rem 0.5) ]
    | `Sm -> style [ max_height (Rem 1.0) ]
    | `Md -> style [ max_height (Rem 1.5) ]
    | `Lg -> style [ max_height (Rem 2.0) ]
    | `Xl -> style [ max_height (Rem 3.0) ]
    | `Xl_2 -> style [ max_height (Rem 4.0) ]
    | `Xl_3 -> style [ max_height (Rem 6.0) ]
    | `Full -> style [ max_height (Pct 100.0) ]
    | `Rem n -> spacing_utility ?theme max_height n

  let max_h_none' = style [ max_height None ]
  let max_h_full' = style [ max_height (Pct 100.0) ]
  let max_h_screen' = style [ max_height (Vh 100.0) ]
  let max_h_min' = style [ max_height Min_content ]
  let max_h_max' = style [ max_height Max_content ]
  let max_h_fit' = style [ max_height Fit_content ]

  (* Int-based max-height function for Tailwind scale (n * 0.25rem) *)

  (* A Tailwind sizing fraction [n/m] resolves to [n/m * 100%]. Tailwind emits
     calc(n / m * 100%) and folds it to 6 significant figures (e.g. 33.3333,
     8.33333); we emit the percentage rounded the same way so the two match.
     Denominators follow Tailwind's scale. *)
  let fraction_pct frac =
    match String.split_on_char '/' frac with
    | [ n; m ] -> (
        match (int_of_string_opt n, int_of_string_opt m) with
        | Some n, Some m
          when m > 0 && n > 0 && n < m && List.mem m [ 2; 3; 4; 5; 6; 12 ] ->
            let pct = float_of_int n /. float_of_int m *. 100. in
            let digits = 6. -. Float.ceil (Float.log10 pct) in
            let factor = 10. ** digits in
            Some (Float.round (pct *. factor) /. factor)
        | _ -> None)
    | _ -> None

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

  let to_style theme =
    let spacing_utility css_prop n = spacing_utility ~theme css_prop n in
    let w' size = w' ~theme size in
    let h' size = h' ~theme size in
    let min_w' size = min_w' ~theme size in
    let max_w' size = max_w' ~theme size in
    let min_h' size = min_h' ~theme size in
    let max_h' size = max_h' ~theme size in
    function
    (* Width utilities *)
    | W_auto -> w_auto'
    | W_px -> w_px'
    | W_full -> w_full'
    | W_screen -> w_screen'
    | W_min -> w_min'
    | W_max -> w_max'
    | W_fit -> w_fit'
    | W_spacing n -> w' (`Rem n)
    | W_fraction f -> (
        match fraction_pct f with
        | Some pct -> style [ width (Pct pct) ]
        | None -> failwith ("Unknown width fraction: " ^ f))
    | W_arbitrary (_, len) -> style [ width len ]
    | W_dvw -> style [ width (Dvw 100.) ]
    | W_lvw -> style [ width (Lvw 100.) ]
    | W_svw -> style [ width (Svw 100.) ]
    | W_container name -> (
        (* v4 resolves w-<name> to --width-<name> when the theme defines it, and
           otherwise to the --container-<name> scale (the default). *)
        match Scheme.theme_value (Some theme) ("width-" ^ name) with
        | Some v ->
            let decl =
              Css.custom_property ~layer:"theme" ("--width-" ^ name) v
            in
            style [ decl; width (Var (Var.theme_ref ("width-" ^ name))) ]
        | None -> (
            match container_binding name with
            | Some (v, d) ->
                let decl, ref_ = Var.binding v d in
                style [ decl; width (Var ref_) ]
            | None ->
                style [ width (Var (Var.theme_ref ("container-" ^ name))) ]))
    (* Height utilities *)
    | H_auto -> h_auto'
    | H_px -> h_px'
    | H_full -> h_full'
    | H_screen -> h_screen'
    | H_min -> h_min'
    | H_max -> h_max'
    | H_fit -> h_fit'
    | H_spacing n -> h' (`Rem n)
    | H_fraction f -> (
        match fraction_pct f with
        | Some pct -> style [ height (Pct pct) ]
        | None -> failwith ("Unknown height fraction: " ^ f))
    | H_arbitrary (_, len) -> style [ height len ]
    | H_dvh -> style [ height (Dvh 100.) ]
    | H_lvh -> style [ height (Lvh 100.) ]
    | H_svh -> style [ height (Svh 100.) ]
    | H_dvw -> style [ height (Dvw 100.) ]
    | H_lvw -> style [ height (Lvw 100.) ]
    | H_svw -> style [ height (Svw 100.) ]
    | H_lh -> style [ height (Lh 1.) ]
    (* Min-width utilities *)
    | Min_w_0 -> min_w_0'
    | Min_w_full -> min_w_full'
    | Min_w_min -> min_w_min'
    | Min_w_max -> min_w_max'
    | Min_w_fit -> min_w_fit'
    | Min_w_auto -> style [ min_width Auto ]
    | Min_w_spacing n -> min_w' (`Rem n)
    | Min_w_arbitrary (_, len) -> style [ min_width len ]
    | Min_w_container name -> (
        match container_binding name with
        | Some (v, d) ->
            let decl, ref_ = Var.binding v d in
            style [ decl; min_width (Var ref_) ]
        | None ->
            style [ min_width (Var (Var.theme_ref ("container-" ^ name))) ])
    (* Max-width utilities *)
    | Max_w_none -> max_w_none'
    | Max_w_xs -> max_w_xs'
    | Max_w_sm -> max_w_sm'
    | Max_w_md -> max_w_md'
    | Max_w_lg -> max_w_lg'
    | Max_w_xl -> max_w_xl'
    | Max_w_2xl -> max_w_2xl'
    | Max_w_3xl -> max_w_3xl'
    | Max_w_4xl -> max_w_4xl'
    | Max_w_5xl -> max_w_5xl'
    | Max_w_6xl -> max_w_6xl'
    | Max_w_7xl -> max_w_7xl'
    | Max_w_full -> max_w_full'
    | Max_w_min -> max_w_min'
    | Max_w_max -> max_w_max'
    | Max_w_fit -> max_w_fit'
    | Max_w_prose -> max_w_prose'
    | Max_w_screen_sm -> max_w_screen_sm'
    | Max_w_screen_md -> max_w_screen_md'
    | Max_w_screen_lg -> max_w_screen_lg'
    | Max_w_screen_xl -> max_w_screen_xl'
    | Max_w_screen_2xl -> max_w_screen_2xl'
    | Max_w_spacing n -> max_w' (`Rem n)
    | Max_w_arbitrary (_, len) -> style [ max_width len ]
    (* Min-height utilities *)
    | Min_h_0 -> min_h_0'
    | Min_h_full -> min_h_full'
    | Min_h_screen -> min_h_screen'
    | Min_h_min -> min_h_min'
    | Min_h_max -> min_h_max'
    | Min_h_fit -> min_h_fit'
    | Min_h_auto -> style [ min_height Auto ]
    | Min_h_dvh -> style [ min_height (Dvh 100.) ]
    | Min_h_lvh -> style [ min_height (Lvh 100.) ]
    | Min_h_svh -> style [ min_height (Svh 100.) ]
    | Min_h_px -> style [ min_height (Px 1.0) ]
    | Min_h_dvw -> style [ min_height (Dvw 100.) ]
    | Min_h_lvw -> style [ min_height (Lvw 100.) ]
    | Min_h_svw -> style [ min_height (Svw 100.) ]
    | Min_h_lh -> style [ min_height (Lh 1.) ]
    | Min_h_spacing n -> min_h' (`Rem n)
    | Min_h_arbitrary (_, len) -> style [ min_height len ]
    (* Max-height utilities *)
    | Max_h_none -> max_h_none'
    | Max_h_full -> max_h_full'
    | Max_h_screen -> max_h_screen'
    | Max_h_min -> max_h_min'
    | Max_h_max -> max_h_max'
    | Max_h_fit -> max_h_fit'
    | Max_h_dvh -> style [ max_height (Dvh 100.) ]
    | Max_h_lvh -> style [ max_height (Lvh 100.) ]
    | Max_h_svh -> style [ max_height (Svh 100.) ]
    | Max_h_px -> style [ max_height (Px 1.0) ]
    | Max_h_dvw -> style [ max_height (Dvw 100.) ]
    | Max_h_lvw -> style [ max_height (Lvw 100.) ]
    | Max_h_svw -> style [ max_height (Svw 100.) ]
    | Max_h_lh -> style [ max_height (Lh 1.) ]
    | Max_h_spacing n -> max_h' (`Rem n)
    | Max_h_arbitrary (_, len) -> style [ max_height len ]
    (* Size utilities *)
    | Size_auto -> style [ width Auto; height Auto ]
    | Size_full -> style [ width (Pct 100.0); height (Pct 100.0) ]
    | Size_min -> style [ width Min_content; height Min_content ]
    | Size_max -> style [ width Max_content; height Max_content ]
    | Size_fit -> style [ width Fit_content; height Fit_content ]
    | Size_spacing n ->
        let decl, spacing_value = Theme.spacing_calc_float ~theme (n *. 4.) in
        style (decl :: [ width spacing_value; height spacing_value ])
    | Size_fraction f -> (
        match
          List.assoc_opt f
            [
              ("1/2", 50.0);
              ("1/3", 33.3333);
              ("2/3", 66.6667);
              ("1/4", 25.0);
              ("3/4", 75.0);
              ("1/5", 20.0);
              ("2/5", 40.0);
              ("3/5", 60.0);
              ("4/5", 80.0);
              ("1/6", 16.6667);
              ("5/6", 83.3333);
            ]
        with
        | Some pct -> style [ width (Pct pct); height (Pct pct) ]
        | None -> failwith ("Unknown size fraction: " ^ f))
    | Size_arbitrary (_, len) -> style [ width len; height len ]
    (* inline-size utilities *)
    | Inline_fraction f -> (
        match fraction_pct f with
        | Some pct -> style [ inline_size (Pct pct) ]
        | None -> failwith ("Unknown inline-size fraction: " ^ f))
    | Inline_spacing n -> spacing_utility inline_size n
    | Inline_arbitrary (_, len) -> style [ inline_size len ]
    | Inline_auto -> style [ inline_size Auto ]
    | Inline_dvw -> style [ inline_size (Dvw 100.) ]
    | Inline_fit -> style [ inline_size Fit_content ]
    | Inline_full -> style [ inline_size (Pct 100.) ]
    | Inline_lvw -> style [ inline_size (Lvw 100.) ]
    | Inline_max -> style [ inline_size Max_content ]
    | Inline_min -> style [ inline_size Min_content ]
    | Inline_screen -> style [ inline_size (Vw 100.) ]
    | Inline_svw -> style [ inline_size (Svw 100.) ]
    | Inline_container name -> (
        match container_binding name with
        | Some (v, d) ->
            let decl, ref_ = Var.binding v d in
            style [ decl; inline_size (Var ref_) ]
        | None ->
            style [ inline_size (Var (Var.theme_ref ("container-" ^ name))) ])
    (* min-inline-size utilities *)
    | Min_inline_spacing n -> spacing_utility min_inline_size n
    | Min_inline_arbitrary (_, len) -> style [ min_inline_size len ]
    | Min_inline_auto -> style [ min_inline_size Auto ]
    | Min_inline_fit -> style [ min_inline_size Fit_content ]
    | Min_inline_full -> style [ min_inline_size (Pct 100.) ]
    | Min_inline_max -> style [ min_inline_size Max_content ]
    | Min_inline_min -> style [ min_inline_size Min_content ]
    | Min_inline_container name -> (
        match container_binding name with
        | Some (v, d) ->
            let decl, ref_ = Var.binding v d in
            style [ decl; min_inline_size (Var ref_) ]
        | None ->
            style
              [ min_inline_size (Var (Var.theme_ref ("container-" ^ name))) ])
    (* max-inline-size utilities *)
    | Max_inline_spacing n -> spacing_utility max_inline_size n
    | Max_inline_arbitrary (_, len) -> style [ max_inline_size len ]
    | Max_inline_fit -> style [ max_inline_size Fit_content ]
    | Max_inline_full -> style [ max_inline_size (Pct 100.) ]
    | Max_inline_max -> style [ max_inline_size Max_content ]
    | Max_inline_none -> style [ max_inline_size None ]
    | Max_inline_container name -> (
        match container_binding name with
        | Some (v, d) ->
            let decl, ref_ = Var.binding v d in
            style [ decl; max_inline_size (Var ref_) ]
        | None ->
            style
              [ max_inline_size (Var (Var.theme_ref ("container-" ^ name))) ])
    (* block-size utilities *)
    | Block_fraction f -> (
        match fraction_pct f with
        | Some pct -> style [ block_size (Pct pct) ]
        | None -> failwith ("Unknown block-size fraction: " ^ f))
    | Block_spacing n -> spacing_utility block_size n
    | Block_arbitrary (_, len) -> style [ block_size len ]
    | Block_auto -> style [ block_size Auto ]
    | Block_dvh -> style [ block_size (Dvh 100.) ]
    | Block_fit -> style [ block_size Fit_content ]
    | Block_full -> style [ block_size (Pct 100.) ]
    | Block_lh -> style [ block_size (Lh 1.) ]
    | Block_lvh -> style [ block_size (Lvh 100.) ]
    | Block_max -> style [ block_size Max_content ]
    | Block_min -> style [ block_size Min_content ]
    | Block_screen -> style [ block_size (Vh 100.) ]
    | Block_svh -> style [ block_size (Svh 100.) ]
    (* min-block-size utilities *)
    | Min_block_spacing n -> spacing_utility min_block_size n
    | Min_block_arbitrary (_, len) -> style [ min_block_size len ]
    | Min_block_auto -> style [ min_block_size Auto ]
    | Min_block_dvh -> style [ min_block_size (Dvh 100.) ]
    | Min_block_fit -> style [ min_block_size Fit_content ]
    | Min_block_full -> style [ min_block_size (Pct 100.) ]
    | Min_block_lh -> style [ min_block_size (Lh 1.) ]
    | Min_block_lvh -> style [ min_block_size (Lvh 100.) ]
    | Min_block_max -> style [ min_block_size Max_content ]
    | Min_block_min -> style [ min_block_size Min_content ]
    | Min_block_screen -> style [ min_block_size (Vh 100.) ]
    | Min_block_svh -> style [ min_block_size (Svh 100.) ]
    (* max-block-size utilities *)
    | Max_block_spacing n -> spacing_utility max_block_size n
    | Max_block_arbitrary (_, len) -> style [ max_block_size len ]
    | Max_block_dvh -> style [ max_block_size (Dvh 100.) ]
    | Max_block_fit -> style [ max_block_size Fit_content ]
    | Max_block_full -> style [ max_block_size (Pct 100.) ]
    | Max_block_lh -> style [ max_block_size (Lh 1.) ]
    | Max_block_lvh -> style [ max_block_size (Lvh 100.) ]
    | Max_block_max -> style [ max_block_size Max_content ]
    | Max_block_min -> style [ max_block_size Min_content ]
    | Max_block_none -> style [ max_block_size None ]
    | Max_block_screen -> style [ max_block_size (Vh 100.) ]
    | Max_block_svh -> style [ max_block_size (Svh 100.) ]
    (* Aspect utilities *)
    | Aspect_auto -> aspect_auto'
    | Aspect_square -> aspect_square'
    | Aspect_video -> aspect_video'
    | Aspect_ratio (w, h) -> aspect_ratio' w h
    | Aspect_bracket (w, h) -> aspect_ratio' w h

  let err_not_utility = Error (`Msg "Not a sizing utility")

  let err_invalid_value name value =
    Error (`Msg ("Invalid " ^ name ^ " value: " ^ value))

  let parse_arbitrary s : (string * Css.length) option =
    (* Parse bracket values: [4px], [1rem], [calc(100vh-4rem)], etc. Uses
       Css.parse_length for full CSS length parsing including calc(). Returns
       (raw_inner, parsed_length) where raw_inner is used for the CSS class name
       selector (preserving original formatting). *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      let css_value = Parse.decode_arbitrary_value inner in
      match Css.parse_length css_value with
      | Some l -> Some (inner, l)
      | None -> None
    else None

  let parse_w = function
    | "auto" -> Ok W_auto
    | "px" -> Ok W_px
    | "full" -> Ok W_full
    | "screen" -> Ok W_screen
    | "min" -> Ok W_min
    | "max" -> Ok W_max
    | "fit" -> Ok W_fit
    | "dvw" -> Ok W_dvw
    | "lvw" -> Ok W_lvw
    | "svw" -> Ok W_svw
    | name when container_binding name <> None -> Ok (W_container name)
    | frac when String.contains frac '/' ->
        if fraction_pct frac <> None then Ok (W_fraction frac)
        else err_invalid_value "width fraction" frac
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (W_arbitrary (raw, len))
        | None -> err_invalid_value "width" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (W_spacing (n *. 0.25))
        | _ -> err_invalid_value "width" v)

  let parse_h = function
    | "auto" -> Ok H_auto
    | "px" -> Ok H_px
    | "full" -> Ok H_full
    | "screen" -> Ok H_screen
    | "min" -> Ok H_min
    | "max" -> Ok H_max
    | "fit" -> Ok H_fit
    | "dvh" -> Ok H_dvh
    | "lvh" -> Ok H_lvh
    | "svh" -> Ok H_svh
    | "dvw" -> Ok H_dvw
    | "lvw" -> Ok H_lvw
    | "svw" -> Ok H_svw
    | "lh" -> Ok H_lh
    | frac when String.contains frac '/' ->
        if fraction_pct frac <> None then Ok (H_fraction frac)
        else err_invalid_value "height fraction" frac
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (H_arbitrary (raw, len))
        | None -> err_invalid_value "height" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (H_spacing (n *. 0.25))
        | _ -> err_invalid_value "height" v)

  let parse_min_w = function
    | "0" -> Ok Min_w_0
    | "full" -> Ok Min_w_full
    | "min" -> Ok Min_w_min
    | "max" -> Ok Min_w_max
    | "fit" -> Ok Min_w_fit
    | "auto" -> Ok Min_w_auto
    | name when container_binding name <> None -> Ok (Min_w_container name)
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Min_w_arbitrary (raw, len))
        | None -> err_invalid_value "min-width" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_w_spacing (n *. 0.25))
        | _ -> err_invalid_value "min-width" v)

  let parse_min_h = function
    | "0" -> Ok Min_h_0
    | "full" -> Ok Min_h_full
    | "screen" -> Ok Min_h_screen
    | "min" -> Ok Min_h_min
    | "max" -> Ok Min_h_max
    | "fit" -> Ok Min_h_fit
    | "auto" -> Ok Min_h_auto
    | "dvh" -> Ok Min_h_dvh
    | "lvh" -> Ok Min_h_lvh
    | "svh" -> Ok Min_h_svh
    | "px" -> Ok Min_h_px
    | "dvw" -> Ok Min_h_dvw
    | "lvw" -> Ok Min_h_lvw
    | "svw" -> Ok Min_h_svw
    | "lh" -> Ok Min_h_lh
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Min_h_arbitrary (raw, len))
        | None -> err_invalid_value "min-height" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_h_spacing (n *. 0.25))
        | _ -> err_invalid_value "min-height" v)

  let parse_max_w = function
    | "none" -> Ok Max_w_none
    | "xs" -> Ok Max_w_xs
    | "sm" -> Ok Max_w_sm
    | "md" -> Ok Max_w_md
    | "lg" -> Ok Max_w_lg
    | "xl" -> Ok Max_w_xl
    | "2xl" -> Ok Max_w_2xl
    | "3xl" -> Ok Max_w_3xl
    | "4xl" -> Ok Max_w_4xl
    | "5xl" -> Ok Max_w_5xl
    | "6xl" -> Ok Max_w_6xl
    | "7xl" -> Ok Max_w_7xl
    | "full" -> Ok Max_w_full
    | "min" -> Ok Max_w_min
    | "max" -> Ok Max_w_max
    | "fit" -> Ok Max_w_fit
    | "prose" -> Ok Max_w_prose
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Max_w_arbitrary (raw, len))
        | None -> err_invalid_value "max-width" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_w_spacing (n *. 0.25))
        | _ -> err_invalid_value "max-width" v)

  let parse_max_w_screen = function
    | "sm" -> Ok Max_w_screen_sm
    | "md" -> Ok Max_w_screen_md
    | "lg" -> Ok Max_w_screen_lg
    | "xl" -> Ok Max_w_screen_xl
    | "2xl" -> Ok Max_w_screen_2xl
    | s -> err_invalid_value "max-width screen size" s

  let parse_max_h = function
    | "none" -> Ok Max_h_none
    | "full" -> Ok Max_h_full
    | "screen" -> Ok Max_h_screen
    | "min" -> Ok Max_h_min
    | "max" -> Ok Max_h_max
    | "fit" -> Ok Max_h_fit
    | "dvh" -> Ok Max_h_dvh
    | "lvh" -> Ok Max_h_lvh
    | "svh" -> Ok Max_h_svh
    | "px" -> Ok Max_h_px
    | "dvw" -> Ok Max_h_dvw
    | "lvw" -> Ok Max_h_lvw
    | "svw" -> Ok Max_h_svw
    | "lh" -> Ok Max_h_lh
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Max_h_arbitrary (raw, len))
        | None -> err_invalid_value "max-height" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_h_spacing (n *. 0.25))
        | _ -> err_invalid_value "max-height" v)

  let parse_size = function
    | "auto" -> Ok Size_auto
    | "full" -> Ok Size_full
    | "min" -> Ok Size_min
    | "max" -> Ok Size_max
    | "fit" -> Ok Size_fit
    | frac when String.contains frac '/' ->
        if fraction_pct frac <> None then Ok (Size_fraction frac)
        else err_invalid_value "size fraction" frac
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Size_arbitrary (raw, len))
        | None -> err_invalid_value "size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Size_spacing (n *. 0.25))
        | _ -> err_invalid_value "size" v)

  let parse_inline = function
    | "auto" -> Ok Inline_auto
    | "dvw" -> Ok Inline_dvw
    | "fit" -> Ok Inline_fit
    | "full" -> Ok Inline_full
    | "lvw" -> Ok Inline_lvw
    | "max" -> Ok Inline_max
    | "min" -> Ok Inline_min
    | "screen" -> Ok Inline_screen
    | "svw" -> Ok Inline_svw
    | name when container_binding name <> None -> Ok (Inline_container name)
    | frac when String.contains frac '/' ->
        if fraction_pct frac <> None then Ok (Inline_fraction frac)
        else err_invalid_value "inline-size fraction" frac
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Inline_arbitrary (raw, len))
        | None -> err_invalid_value "inline-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Inline_spacing (n *. 0.25))
        | _ -> err_invalid_value "inline-size" v)

  let parse_min_inline = function
    | "auto" -> Ok Min_inline_auto
    | "fit" -> Ok Min_inline_fit
    | "full" -> Ok Min_inline_full
    | "max" -> Ok Min_inline_max
    | "min" -> Ok Min_inline_min
    | name when container_binding name <> None -> Ok (Min_inline_container name)
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Min_inline_arbitrary (raw, len))
        | None -> err_invalid_value "min-inline-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_inline_spacing (n *. 0.25))
        | _ -> err_invalid_value "min-inline-size" v)

  let parse_max_inline = function
    | "fit" -> Ok Max_inline_fit
    | "full" -> Ok Max_inline_full
    | "max" -> Ok Max_inline_max
    | "none" -> Ok Max_inline_none
    | name when container_binding name <> None -> Ok (Max_inline_container name)
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Max_inline_arbitrary (raw, len))
        | None -> err_invalid_value "max-inline-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_inline_spacing (n *. 0.25))
        | _ -> err_invalid_value "max-inline-size" v)

  let parse_block = function
    | "auto" -> Ok Block_auto
    | "dvh" -> Ok Block_dvh
    | "fit" -> Ok Block_fit
    | "full" -> Ok Block_full
    | "lh" -> Ok Block_lh
    | "lvh" -> Ok Block_lvh
    | "max" -> Ok Block_max
    | "min" -> Ok Block_min
    | "screen" -> Ok Block_screen
    | "svh" -> Ok Block_svh
    | frac when String.contains frac '/' ->
        if fraction_pct frac <> None then Ok (Block_fraction frac)
        else err_invalid_value "block-size fraction" frac
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Block_arbitrary (raw, len))
        | None -> err_invalid_value "block-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Block_spacing (n *. 0.25))
        | _ -> err_invalid_value "block-size" v)

  let parse_min_block = function
    | "auto" -> Ok Min_block_auto
    | "dvh" -> Ok Min_block_dvh
    | "fit" -> Ok Min_block_fit
    | "full" -> Ok Min_block_full
    | "lh" -> Ok Min_block_lh
    | "lvh" -> Ok Min_block_lvh
    | "max" -> Ok Min_block_max
    | "min" -> Ok Min_block_min
    | "screen" -> Ok Min_block_screen
    | "svh" -> Ok Min_block_svh
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Min_block_arbitrary (raw, len))
        | None -> err_invalid_value "min-block-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Min_block_spacing (n *. 0.25))
        | _ -> err_invalid_value "min-block-size" v)

  let parse_max_block = function
    | "dvh" -> Ok Max_block_dvh
    | "fit" -> Ok Max_block_fit
    | "full" -> Ok Max_block_full
    | "lh" -> Ok Max_block_lh
    | "lvh" -> Ok Max_block_lvh
    | "max" -> Ok Max_block_max
    | "min" -> Ok Max_block_min
    | "none" -> Ok Max_block_none
    | "screen" -> Ok Max_block_screen
    | "svh" -> Ok Max_block_svh
    | v when String.length v > 0 && v.[0] = '[' -> (
        match parse_arbitrary v with
        | Some (raw, len) -> Ok (Max_block_arbitrary (raw, len))
        | None -> err_invalid_value "max-block-size" v)
    | v -> (
        match float_of_string_opt v with
        | Some n when n >= 0. -> Ok (Max_block_spacing (n *. 0.25))
        | _ -> err_invalid_value "max-block-size" v)

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
    | [ "w"; value ] -> parse_w value
    | [ "h"; value ] -> parse_h value
    | [ "min"; "w"; value ] -> parse_min_w value
    | [ "min"; "h"; value ] -> parse_min_h value
    | [ "max"; "w"; value ] -> parse_max_w value
    | [ "max"; "w"; "screen"; size ] -> parse_max_w_screen size
    | [ "max"; "h"; value ] -> parse_max_h value
    | [ "size"; value ] -> parse_size value
    | [ "inline"; value ] -> parse_inline value
    | [ "min"; "inline"; value ] -> parse_min_inline value
    | [ "max"; "inline"; value ] -> parse_max_inline value
    | [ "block"; value ] -> parse_block value
    | [ "min"; "block"; value ] -> parse_min_block value
    | [ "max"; "block"; value ] -> parse_max_block value
    | [ "aspect"; "auto" ] -> Ok Aspect_auto
    | [ "aspect"; "square" ] -> Ok Aspect_square
    | [ "aspect"; "video" ] -> Ok Aspect_video
    | [ "aspect"; value ] when Parse.is_bracket_value value ->
        parse_aspect_ratio (Parse.bracket_inner value) (fun w h ->
            Aspect_bracket (w, h))
    | [ "aspect"; value ] ->
        parse_aspect_ratio value (fun w h -> Aspect_ratio (w, h))
    | _ -> err_not_utility

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

  (* Per-family base; families keep their previous relative order but with wide
     (10M) bands so the interleaved spacing/fraction range never overflows. *)
  let arbitrary_off = 5_000_000
  let keyword_off = 6_000_000

  let suborder =
    (* Family bases are 10M apart so the interleaved spacing/fraction range (<
       5M) and arbitrary/keyword offsets never overflow into the next family.
       Within a family: spacing/fractions interleaved by magnitude, then
       arbitrary, then keywords (alphabetical). *)
    (* size-* (width+height) sorts first in Tailwind, before h/w/max/min. *)
    let size = 0 in
    let h = 10_000_000 in
    let max_h = 20_000_000 in
    let min_h = 30_000_000 in
    let w = 40_000_000 in
    let max_w = 50_000_000 in
    let min_w = 60_000_000 in
    let inline = 70_000_000 in
    let min_inline = 80_000_000 in
    let max_inline = 90_000_000 in
    let block = 100_000_000 in
    let min_block = 110_000_000 in
    let max_block = 120_000_000 in
    let aspect = 130_000_000 in
    function
    (* Height *)
    | H_fraction f -> h + fraction_value_order f
    | H_spacing n -> h + spacing_value_order n
    | H_arbitrary _ -> h + arbitrary_off
    | H_auto -> h + keyword_off + 0
    | H_dvh -> h + keyword_off + 1
    | H_fit -> h + keyword_off + 2
    | H_full -> h + keyword_off + 3
    | H_lh -> h + keyword_off + 4
    | H_lvh -> h + keyword_off + 5
    | H_max -> h + keyword_off + 6
    | H_min -> h + keyword_off + 7
    | H_px -> h + keyword_off + 8
    | H_screen -> h + keyword_off + 9
    | H_svh -> h + keyword_off + 10
    | H_dvw -> h + keyword_off + 10
    | H_lvw -> h + keyword_off + 10
    | H_svw -> h + keyword_off + 10
    (* Max-height *)
    | Max_h_spacing n -> max_h + spacing_value_order n
    | Max_h_arbitrary _ -> max_h + arbitrary_off
    | Max_h_dvh -> max_h + keyword_off + 0
    | Max_h_fit -> max_h + keyword_off + 1
    | Max_h_full -> max_h + keyword_off + 2
    | Max_h_lh -> max_h + keyword_off + 3
    | Max_h_lvh -> max_h + keyword_off + 4
    | Max_h_max -> max_h + keyword_off + 5
    | Max_h_min -> max_h + keyword_off + 6
    | Max_h_none -> max_h + keyword_off + 7
    | Max_h_screen -> max_h + keyword_off + 8
    | Max_h_svh -> max_h + keyword_off + 9
    | Max_h_px -> max_h + keyword_off + 9
    | Max_h_dvw -> max_h + keyword_off + 9
    | Max_h_lvw -> max_h + keyword_off + 9
    | Max_h_svw -> max_h + keyword_off + 9
    (* Min-height *)
    | Min_h_0 -> min_h + 0
    | Min_h_spacing n -> min_h + spacing_value_order n
    | Min_h_arbitrary _ -> min_h + arbitrary_off
    | Min_h_auto -> min_h + keyword_off + 0
    | Min_h_dvh -> min_h + keyword_off + 1
    | Min_h_fit -> min_h + keyword_off + 2
    | Min_h_full -> min_h + keyword_off + 3
    | Min_h_lh -> min_h + keyword_off + 4
    | Min_h_lvh -> min_h + keyword_off + 5
    | Min_h_max -> min_h + keyword_off + 6
    | Min_h_min -> min_h + keyword_off + 7
    | Min_h_screen -> min_h + keyword_off + 8
    | Min_h_svh -> min_h + keyword_off + 9
    | Min_h_px -> min_h + keyword_off + 9
    | Min_h_dvw -> min_h + keyword_off + 9
    | Min_h_lvw -> min_h + keyword_off + 9
    | Min_h_svw -> min_h + keyword_off + 9
    (* Width *)
    | W_fraction f -> w + fraction_value_order f
    | W_spacing n -> w + spacing_value_order n
    | W_arbitrary _ -> w + arbitrary_off
    | W_auto -> w + keyword_off + 0
    | W_dvw -> w + keyword_off + 1
    | W_fit -> w + keyword_off + 2
    | W_full -> w + keyword_off + 3
    | W_lvw -> w + keyword_off + 4
    | W_max -> w + keyword_off + 5
    | W_min -> w + keyword_off + 6
    | W_px -> w + keyword_off + 7
    | W_screen -> w + keyword_off + 8
    | W_svw -> w + keyword_off + 9
    | W_container name -> w + keyword_off + 10 + container_order name
    (* Max-width *)
    (* Tailwind orders max-width in three bands: the container scale sizes
       (2xl..7xl) by number, then the numeric spacing values, then an arbitrary
       value, then the letter-prefixed keywords alphabetically. *)
    | Max_w_2xl -> max_w + 0
    | Max_w_3xl -> max_w + 1
    | Max_w_4xl -> max_w + 2
    | Max_w_5xl -> max_w + 3
    | Max_w_6xl -> max_w + 4
    | Max_w_7xl -> max_w + 5
    | Max_w_spacing n -> max_w + 1_000_000 + spacing_value_order n
    | Max_w_arbitrary _ -> max_w + arbitrary_off
    | Max_w_fit -> max_w + keyword_off + 0
    | Max_w_full -> max_w + keyword_off + 1
    | Max_w_lg -> max_w + keyword_off + 2
    | Max_w_max -> max_w + keyword_off + 3
    | Max_w_md -> max_w + keyword_off + 4
    | Max_w_min -> max_w + keyword_off + 5
    | Max_w_none -> max_w + keyword_off + 6
    | Max_w_prose -> max_w + keyword_off + 7
    | Max_w_screen_2xl -> max_w + keyword_off + 8
    | Max_w_screen_lg -> max_w + keyword_off + 9
    | Max_w_screen_md -> max_w + keyword_off + 10
    | Max_w_screen_sm -> max_w + keyword_off + 11
    | Max_w_screen_xl -> max_w + keyword_off + 12
    | Max_w_sm -> max_w + keyword_off + 13
    | Max_w_xl -> max_w + keyword_off + 14
    | Max_w_xs -> max_w + keyword_off + 15
    (* Min-width *)
    | Min_w_0 -> min_w + 0
    | Min_w_spacing n -> min_w + spacing_value_order n
    | Min_w_arbitrary _ -> min_w + arbitrary_off
    | Min_w_auto -> min_w + keyword_off + 0
    | Min_w_fit -> min_w + keyword_off + 1
    | Min_w_full -> min_w + keyword_off + 2
    | Min_w_max -> min_w + keyword_off + 3
    | Min_w_min -> min_w + keyword_off + 4
    | Min_w_container name -> min_w + keyword_off + 5 + container_order name
    (* Size *)
    | Size_fraction f -> size + fraction_value_order f
    | Size_spacing n -> size + spacing_value_order n
    | Size_arbitrary _ -> size + arbitrary_off
    | Size_auto -> size + keyword_off + 0
    | Size_fit -> size + keyword_off + 1
    | Size_full -> size + keyword_off + 2
    | Size_max -> size + keyword_off + 3
    | Size_min -> size + keyword_off + 4
    (* inline-size *)
    | Inline_fraction f -> inline + fraction_value_order f
    | Inline_spacing n -> inline + spacing_value_order n
    | Inline_arbitrary _ -> inline + arbitrary_off
    | Inline_auto -> inline + keyword_off + 0
    | Inline_dvw -> inline + keyword_off + 1
    | Inline_fit -> inline + keyword_off + 2
    | Inline_full -> inline + keyword_off + 3
    | Inline_lvw -> inline + keyword_off + 4
    | Inline_max -> inline + keyword_off + 5
    | Inline_min -> inline + keyword_off + 6
    | Inline_screen -> inline + keyword_off + 7
    | Inline_svw -> inline + keyword_off + 8
    | Inline_container name -> inline + keyword_off + 9 + container_order name
    (* min-inline-size *)
    | Min_inline_spacing n -> min_inline + spacing_value_order n
    | Min_inline_arbitrary _ -> min_inline + arbitrary_off
    | Min_inline_auto -> min_inline + keyword_off + 0
    | Min_inline_fit -> min_inline + keyword_off + 1
    | Min_inline_full -> min_inline + keyword_off + 2
    | Min_inline_max -> min_inline + keyword_off + 3
    | Min_inline_min -> min_inline + keyword_off + 4
    | Min_inline_container name ->
        min_inline + keyword_off + 5 + container_order name
    (* max-inline-size *)
    | Max_inline_spacing n -> max_inline + spacing_value_order n
    | Max_inline_arbitrary _ -> max_inline + arbitrary_off
    | Max_inline_fit -> max_inline + keyword_off + 0
    | Max_inline_full -> max_inline + keyword_off + 1
    | Max_inline_max -> max_inline + keyword_off + 2
    | Max_inline_none -> max_inline + keyword_off + 3
    | Max_inline_container name ->
        max_inline + keyword_off + 4 + container_order name
    (* block-size *)
    | Block_fraction f -> block + fraction_value_order f
    | Block_spacing n -> block + spacing_value_order n
    | Block_arbitrary _ -> block + arbitrary_off
    | Block_auto -> block + keyword_off + 0
    | Block_dvh -> block + keyword_off + 1
    | Block_fit -> block + keyword_off + 2
    | Block_full -> block + keyword_off + 3
    | Block_lh -> block + keyword_off + 4
    | Block_lvh -> block + keyword_off + 5
    | Block_max -> block + keyword_off + 6
    | Block_min -> block + keyword_off + 7
    | Block_screen -> block + keyword_off + 8
    | Block_svh -> block + keyword_off + 9
    (* min-block-size *)
    | Min_block_spacing n -> min_block + spacing_value_order n
    | Min_block_arbitrary _ -> min_block + arbitrary_off
    | Min_block_auto -> min_block + keyword_off + 0
    | Min_block_dvh -> min_block + keyword_off + 1
    | Min_block_fit -> min_block + keyword_off + 2
    | Min_block_full -> min_block + keyword_off + 3
    | Min_block_lh -> min_block + keyword_off + 4
    | Min_block_lvh -> min_block + keyword_off + 5
    | Min_block_max -> min_block + keyword_off + 6
    | Min_block_min -> min_block + keyword_off + 7
    | Min_block_screen -> min_block + keyword_off + 8
    | Min_block_svh -> min_block + keyword_off + 9
    (* max-block-size *)
    | Max_block_spacing n -> max_block + spacing_value_order n
    | Max_block_arbitrary _ -> max_block + arbitrary_off
    | Max_block_dvh -> max_block + keyword_off + 0
    | Max_block_fit -> max_block + keyword_off + 1
    | Max_block_full -> max_block + keyword_off + 2
    | Max_block_lh -> max_block + keyword_off + 3
    | Max_block_lvh -> max_block + keyword_off + 4
    | Max_block_max -> max_block + keyword_off + 5
    | Max_block_min -> max_block + keyword_off + 6
    | Max_block_none -> max_block + keyword_off + 7
    | Max_block_screen -> max_block + keyword_off + 8
    | Max_block_svh -> max_block + keyword_off + 9
    (* Aspect: ratios -> brackets -> keywords *)
    | Aspect_ratio (rw, rh) ->
        aspect + int_of_float (rw *. 10.) + int_of_float rh
    | Aspect_bracket (rw, rh) ->
        aspect + 1000 + int_of_float (rw *. 10.) + int_of_float rh
    | Aspect_auto -> aspect + 2000
    | Aspect_square -> aspect + 2001
    | Aspect_video -> aspect + 2002

  let to_class = function
    (* Width utilities *)
    | W_auto -> "w-auto"
    | W_full -> "w-full"
    | W_screen -> "w-screen"
    | W_min -> "w-min"
    | W_max -> "w-max"
    | W_fit -> "w-fit"
    | W_px -> "w-px"
    | W_spacing n -> "w-" ^ class_float (n *. 4.)
    | W_fraction f -> "w-" ^ f
    | W_arbitrary (raw, _) -> "w-[" ^ raw ^ "]"
    | W_dvw -> "w-dvw"
    | W_lvw -> "w-lvw"
    | W_svw -> "w-svw"
    | W_container name -> "w-" ^ name
    (* Height utilities *)
    | H_auto -> "h-auto"
    | H_full -> "h-full"
    | H_screen -> "h-screen"
    | H_min -> "h-min"
    | H_max -> "h-max"
    | H_fit -> "h-fit"
    | H_px -> "h-px"
    | H_spacing n -> "h-" ^ class_float (n *. 4.)
    | H_fraction f -> "h-" ^ f
    | H_arbitrary (raw, _) -> "h-[" ^ raw ^ "]"
    | H_dvh -> "h-dvh"
    | H_lh -> "h-lh"
    | H_lvh -> "h-lvh"
    | H_svh -> "h-svh"
    | H_dvw -> "h-dvw"
    | H_lvw -> "h-lvw"
    | H_svw -> "h-svw"
    (* Min-width utilities *)
    | Min_w_0 -> "min-w-0"
    | Min_w_full -> "min-w-full"
    | Min_w_min -> "min-w-min"
    | Min_w_max -> "min-w-max"
    | Min_w_fit -> "min-w-fit"
    | Min_w_auto -> "min-w-auto"
    | Min_w_spacing n -> "min-w-" ^ class_float (n *. 4.)
    | Min_w_arbitrary (raw, _) -> "min-w-[" ^ raw ^ "]"
    | Min_w_container name -> "min-w-" ^ name
    (* Max-width utilities *)
    | Max_w_none -> "max-w-none"
    | Max_w_xs -> "max-w-xs"
    | Max_w_sm -> "max-w-sm"
    | Max_w_md -> "max-w-md"
    | Max_w_lg -> "max-w-lg"
    | Max_w_xl -> "max-w-xl"
    | Max_w_2xl -> "max-w-2xl"
    | Max_w_3xl -> "max-w-3xl"
    | Max_w_4xl -> "max-w-4xl"
    | Max_w_5xl -> "max-w-5xl"
    | Max_w_6xl -> "max-w-6xl"
    | Max_w_7xl -> "max-w-7xl"
    | Max_w_full -> "max-w-full"
    | Max_w_min -> "max-w-min"
    | Max_w_max -> "max-w-max"
    | Max_w_fit -> "max-w-fit"
    | Max_w_prose -> "max-w-prose"
    | Max_w_screen_sm -> "max-w-screen-sm"
    | Max_w_screen_md -> "max-w-screen-md"
    | Max_w_screen_lg -> "max-w-screen-lg"
    | Max_w_screen_xl -> "max-w-screen-xl"
    | Max_w_screen_2xl -> "max-w-screen-2xl"
    | Max_w_spacing n -> "max-w-" ^ class_float (n *. 4.)
    | Max_w_arbitrary (raw, _) -> "max-w-[" ^ raw ^ "]"
    (* Min-height utilities *)
    | Min_h_0 -> "min-h-0"
    | Min_h_full -> "min-h-full"
    | Min_h_screen -> "min-h-screen"
    | Min_h_min -> "min-h-min"
    | Min_h_max -> "min-h-max"
    | Min_h_fit -> "min-h-fit"
    | Min_h_auto -> "min-h-auto"
    | Min_h_dvh -> "min-h-dvh"
    | Min_h_lvh -> "min-h-lvh"
    | Min_h_svh -> "min-h-svh"
    | Min_h_px -> "min-h-px"
    | Min_h_dvw -> "min-h-dvw"
    | Min_h_lvw -> "min-h-lvw"
    | Min_h_svw -> "min-h-svw"
    | Min_h_lh -> "min-h-lh"
    | Min_h_spacing n -> "min-h-" ^ class_float (n *. 4.)
    | Min_h_arbitrary (raw, _) -> "min-h-[" ^ raw ^ "]"
    (* Max-height utilities *)
    | Max_h_none -> "max-h-none"
    | Max_h_full -> "max-h-full"
    | Max_h_screen -> "max-h-screen"
    | Max_h_min -> "max-h-min"
    | Max_h_max -> "max-h-max"
    | Max_h_fit -> "max-h-fit"
    | Max_h_dvh -> "max-h-dvh"
    | Max_h_lvh -> "max-h-lvh"
    | Max_h_svh -> "max-h-svh"
    | Max_h_px -> "max-h-px"
    | Max_h_dvw -> "max-h-dvw"
    | Max_h_lvw -> "max-h-lvw"
    | Max_h_svw -> "max-h-svw"
    | Max_h_lh -> "max-h-lh"
    | Max_h_spacing n -> "max-h-" ^ class_float (n *. 4.)
    | Max_h_arbitrary (raw, _) -> "max-h-[" ^ raw ^ "]"
    (* Size utilities *)
    | Size_auto -> "size-auto"
    | Size_full -> "size-full"
    | Size_min -> "size-min"
    | Size_max -> "size-max"
    | Size_fit -> "size-fit"
    | Size_spacing n -> "size-" ^ class_float (n *. 4.)
    | Size_fraction f -> "size-" ^ f
    | Size_arbitrary (raw, _) -> "size-[" ^ raw ^ "]"
    (* inline-size utilities *)
    | Inline_fraction f -> "inline-" ^ f
    | Inline_spacing n -> "inline-" ^ class_float (n *. 4.)
    | Inline_arbitrary (raw, _) -> "inline-[" ^ raw ^ "]"
    | Inline_auto -> "inline-auto"
    | Inline_dvw -> "inline-dvw"
    | Inline_fit -> "inline-fit"
    | Inline_full -> "inline-full"
    | Inline_lvw -> "inline-lvw"
    | Inline_max -> "inline-max"
    | Inline_min -> "inline-min"
    | Inline_screen -> "inline-screen"
    | Inline_svw -> "inline-svw"
    | Inline_container name -> "inline-" ^ name
    (* min-inline-size utilities *)
    | Min_inline_spacing n -> "min-inline-" ^ class_float (n *. 4.)
    | Min_inline_arbitrary (raw, _) -> "min-inline-[" ^ raw ^ "]"
    | Min_inline_auto -> "min-inline-auto"
    | Min_inline_fit -> "min-inline-fit"
    | Min_inline_full -> "min-inline-full"
    | Min_inline_max -> "min-inline-max"
    | Min_inline_min -> "min-inline-min"
    | Min_inline_container name -> "min-inline-" ^ name
    (* max-inline-size utilities *)
    | Max_inline_spacing n -> "max-inline-" ^ class_float (n *. 4.)
    | Max_inline_arbitrary (raw, _) -> "max-inline-[" ^ raw ^ "]"
    | Max_inline_fit -> "max-inline-fit"
    | Max_inline_full -> "max-inline-full"
    | Max_inline_max -> "max-inline-max"
    | Max_inline_none -> "max-inline-none"
    | Max_inline_container name -> "max-inline-" ^ name
    (* block-size utilities *)
    | Block_fraction f -> "block-" ^ f
    | Block_spacing n -> "block-" ^ class_float (n *. 4.)
    | Block_arbitrary (raw, _) -> "block-[" ^ raw ^ "]"
    | Block_auto -> "block-auto"
    | Block_dvh -> "block-dvh"
    | Block_fit -> "block-fit"
    | Block_full -> "block-full"
    | Block_lh -> "block-lh"
    | Block_lvh -> "block-lvh"
    | Block_max -> "block-max"
    | Block_min -> "block-min"
    | Block_screen -> "block-screen"
    | Block_svh -> "block-svh"
    (* min-block-size utilities *)
    | Min_block_spacing n -> "min-block-" ^ class_float (n *. 4.)
    | Min_block_arbitrary (raw, _) -> "min-block-[" ^ raw ^ "]"
    | Min_block_auto -> "min-block-auto"
    | Min_block_dvh -> "min-block-dvh"
    | Min_block_fit -> "min-block-fit"
    | Min_block_full -> "min-block-full"
    | Min_block_lh -> "min-block-lh"
    | Min_block_lvh -> "min-block-lvh"
    | Min_block_max -> "min-block-max"
    | Min_block_min -> "min-block-min"
    | Min_block_screen -> "min-block-screen"
    | Min_block_svh -> "min-block-svh"
    (* max-block-size utilities *)
    | Max_block_spacing n -> "max-block-" ^ class_float (n *. 4.)
    | Max_block_arbitrary (raw, _) -> "max-block-[" ^ raw ^ "]"
    | Max_block_dvh -> "max-block-dvh"
    | Max_block_fit -> "max-block-fit"
    | Max_block_full -> "max-block-full"
    | Max_block_lh -> "max-block-lh"
    | Max_block_lvh -> "max-block-lvh"
    | Max_block_max -> "max-block-max"
    | Max_block_min -> "max-block-min"
    | Max_block_none -> "max-block-none"
    | Max_block_screen -> "max-block-screen"
    | Max_block_svh -> "max-block-svh"
    (* Aspect utilities *)
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

let w' =
  prime_size_utility ~none:(W_spacing 0.) ~xs:(W_spacing 0.5)
    ~sm:(W_spacing 1.0) ~md:(W_spacing 1.5) ~lg:(W_spacing 2.0)
    ~xl:(W_spacing 3.0) ~xl_2:(W_spacing 4.0) ~xl_3:(W_spacing 6.0) ~full:W_full
    ~rem:(fun n -> W_spacing n)

let h' =
  prime_size_utility ~none:(H_spacing 0.) ~xs:(H_spacing 0.5)
    ~sm:(H_spacing 1.0) ~md:(H_spacing 1.5) ~lg:(H_spacing 2.0)
    ~xl:(H_spacing 3.0) ~xl_2:(H_spacing 4.0) ~xl_3:(H_spacing 6.0) ~full:H_full
    ~rem:(fun n -> H_spacing n)

let min_w' =
  prime_size_utility ~none:Min_w_0 ~xs:(Min_w_spacing 0.5)
    ~sm:(Min_w_spacing 1.0) ~md:(Min_w_spacing 1.5) ~lg:(Min_w_spacing 2.0)
    ~xl:(Min_w_spacing 3.0) ~xl_2:(Min_w_spacing 4.0) ~xl_3:(Min_w_spacing 6.0)
    ~full:Min_w_full ~rem:(fun n -> Min_w_spacing n)

let max_w' =
  prime_size_utility ~none:Max_w_none ~xs:Max_w_xs ~sm:Max_w_sm ~md:Max_w_md
    ~lg:Max_w_lg ~xl:Max_w_xl ~xl_2:Max_w_2xl ~xl_3:Max_w_3xl ~full:Max_w_full
    ~rem:(fun n -> Max_w_spacing n)

let min_h' =
  prime_size_utility ~none:Min_h_0 ~xs:(Min_h_spacing 0.5)
    ~sm:(Min_h_spacing 1.0) ~md:(Min_h_spacing 1.5) ~lg:(Min_h_spacing 2.0)
    ~xl:(Min_h_spacing 3.0) ~xl_2:(Min_h_spacing 4.0) ~xl_3:(Min_h_spacing 6.0)
    ~full:Min_h_full ~rem:(fun n -> Min_h_spacing n)

let max_h' =
  prime_size_utility ~none:Max_h_none ~xs:(Max_h_spacing 0.5)
    ~sm:(Max_h_spacing 1.0) ~md:(Max_h_spacing 1.5) ~lg:(Max_h_spacing 2.0)
    ~xl:(Max_h_spacing 3.0) ~xl_2:(Max_h_spacing 4.0) ~xl_3:(Max_h_spacing 6.0)
    ~full:Max_h_full ~rem:(fun n -> Max_h_spacing n)

(* Top-level wrappers returning Utility.t, following the Utility.Handler
   pattern *)

(* Width *)
let w n = utility (W_spacing (float_of_int n *. 0.25))
let w_auto = utility W_auto
let w_full = utility W_full
let w_screen = utility W_screen
let w_min = utility W_min
let w_max = utility W_max
let w_fit = utility W_fit
let w_1_2 = utility (W_fraction "1/2")
let w_1_3 = utility (W_fraction "1/3")
let w_2_3 = utility (W_fraction "2/3")
let w_1_4 = utility (W_fraction "1/4")
let w_3_4 = utility (W_fraction "3/4")
let w_1_5 = utility (W_fraction "1/5")
let w_2_5 = utility (W_fraction "2/5")
let w_3_5 = utility (W_fraction "3/5")
let w_4_5 = utility (W_fraction "4/5")

(* Height *)
let h n = utility (H_spacing (float_of_int n *. 0.25))
let h_auto = utility H_auto
let h_full = utility H_full
let h_screen = utility H_screen
let h_min = utility H_min
let h_max = utility H_max
let h_fit = utility H_fit
let h_1_2 = utility (H_fraction "1/2")
let h_1_3 = utility (H_fraction "1/3")
let h_2_3 = utility (H_fraction "2/3")
let h_1_4 = utility (H_fraction "1/4")
let h_3_4 = utility (H_fraction "3/4")
let h_1_5 = utility (H_fraction "1/5")
let h_2_5 = utility (H_fraction "2/5")
let h_3_5 = utility (H_fraction "3/5")
let h_4_5 = utility (H_fraction "4/5")

(* Min width *)
let min_w n = utility (Min_w_spacing (float_of_int n *. 0.25))
let min_w_0 = utility Min_w_0
let min_w_full = utility Min_w_full
let min_w_min = utility Min_w_min
let min_w_max = utility Min_w_max
let min_w_fit = utility Min_w_fit

(* Max width *)
let max_w n = utility (Max_w_spacing (float_of_int n *. 0.25))
let max_w_none = utility Max_w_none
let max_w_xs = utility Max_w_xs
let max_w_sm = utility Max_w_sm
let max_w_md = utility Max_w_md
let max_w_lg = utility Max_w_lg
let max_w_xl = utility Max_w_xl
let max_w_2xl = utility Max_w_2xl
let max_w_3xl = utility Max_w_3xl
let max_w_4xl = utility Max_w_4xl
let max_w_5xl = utility Max_w_5xl
let max_w_6xl = utility Max_w_6xl
let max_w_7xl = utility Max_w_7xl
let max_w_full = utility Max_w_full
let max_w_min = utility Max_w_min
let max_w_max = utility Max_w_max
let max_w_fit = utility Max_w_fit
let max_w_prose = utility Max_w_prose
let max_w_screen_sm = utility Max_w_screen_sm
let max_w_screen_md = utility Max_w_screen_md
let max_w_screen_lg = utility Max_w_screen_lg
let max_w_screen_xl = utility Max_w_screen_xl
let max_w_screen_2xl = utility Max_w_screen_2xl

(* Min height *)
let min_h n = utility (Min_h_spacing (float_of_int n *. 0.25))
let min_h_0 = utility Min_h_0
let min_h_full = utility Min_h_full
let min_h_screen = utility Min_h_screen
let min_h_min = utility Min_h_min
let min_h_max = utility Min_h_max
let min_h_fit = utility Min_h_fit

(* Max height *)
let max_h n = utility (Max_h_spacing (float_of_int n *. 0.25))
let max_h_none = utility Max_h_none
let max_h_full = utility Max_h_full
let max_h_screen = utility Max_h_screen
let max_h_min = utility Max_h_min
let max_h_max = utility Max_h_max
let max_h_fit = utility Max_h_fit

(* Size (width and height combined) *)
let size n = utility (Size_spacing (float_of_int n *. 0.25))
let size_auto = utility Size_auto
let size_full = utility Size_full
let size_min = utility Size_min
let size_max = utility Size_max
let size_fit = utility Size_fit

(* Aspect ratio *)
let aspect_auto = utility Aspect_auto
let aspect_square = utility Aspect_square
let aspect_video = utility Aspect_video
let aspect_ratio w h = utility (Aspect_ratio (float_of_int w, float_of_int h))

(* Order exposure for this module *)
let order (u : Utility.base) =
  match u with Self x -> Some (priority x, suborder x) | _ -> None

(* Export container theme variables for use by other modules (e.g., Columns) *)
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
