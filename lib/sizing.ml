(** Sizing utilities for width and height *)

type size =
  [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full | `Rem of float ]

module Handler = struct
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
    | W_arbitrary of Css.length
    | W_dvw (* 100dvw - dynamic viewport width *)
    | W_lvw (* 100lvw - large viewport width *)
    | W_svw (* 100svw - small viewport width *)
    | W_xl (* width: var(--width-xl) *)
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
    | H_arbitrary of Css.length
    | H_dvh (* 100dvh - dynamic viewport height *)
    | H_lvh (* 100lvh - large viewport height *)
    | H_svh (* 100svh - small viewport height *)
    | H_lh (* 1lh - line height *)
    (* Min-width utilities *)
    | Min_w_0
    | Min_w_full
    | Min_w_min
    | Min_w_max
    | Min_w_fit
    | Min_w_auto
    | Min_w_spacing of float
    | Min_w_arbitrary of Css.length
    | Min_w_xl (* min-width: var(--container-xl) *)
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
    | Min_h_lh
    | Min_h_spacing of float
    | Min_h_arbitrary of Css.length
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
    | Max_h_lh
    | Max_h_spacing of float
    | Max_h_arbitrary of Css.length
    (* Size utilities (both width and height) *)
    | Size_auto
    | Size_full
    | Size_min
    | Size_max
    | Size_fit
    | Size_spacing of float
    | Size_fraction of string
    | Size_arbitrary of Css.length
    (* inline-size utilities *)
    | Inline_fraction of string
    | Inline_spacing of float
    | Inline_arbitrary of Css.length
    | Inline_auto
    | Inline_dvw
    | Inline_fit
    | Inline_full
    | Inline_lvw
    | Inline_max
    | Inline_min
    | Inline_screen
    | Inline_svw
    | Inline_xl
    (* min-inline-size utilities *)
    | Min_inline_spacing of float
    | Min_inline_arbitrary of Css.length
    | Min_inline_auto
    | Min_inline_fit
    | Min_inline_full
    | Min_inline_max
    | Min_inline_min
    | Min_inline_xl
    (* max-inline-size utilities *)
    | Max_inline_spacing of float
    | Max_inline_arbitrary of Css.length
    | Max_inline_fit
    | Max_inline_full
    | Max_inline_max
    | Max_inline_none
    | Max_inline_xl
    (* block-size utilities *)
    | Block_fraction of string
    | Block_spacing of float
    | Block_arbitrary of Css.length
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
    | Min_block_arbitrary of Css.length
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
    | Max_block_arbitrary of Css.length
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
    | Aspect_ratio of int * int

  type Utility.base += Self of t

  let name = "sizing"

  (** Priority 6: Sizing utilities (w-*, h-*, max-w-*, etc.) come before
      flex-1/flex-col etc. in Tailwind's order. *)
  let priority = 6

  (** Helper to create spacing-based utilities with consistent pattern. [n] is
      in rem units (e.g., 16.0 for w-64). We convert to class units by
      multiplying by 4, since --spacing is 0.25rem. Now uses named spacing
      variables like --spacing-4 for Tailwind v4 compatibility. *)
  let spacing_utility css_prop n =
    let class_units = int_of_float (n *. 4.) in
    let spacing_v = Theme.get_spacing_var class_units in
    let concrete_value = Theme.spacing_value class_units in
    let decl, spacing_ref = Var.binding spacing_v concrete_value in
    let spacing_value : Css.length = Css.Var spacing_ref in
    style (decl :: [ css_prop spacing_value ])

  let w' size =
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
    | `Rem n -> spacing_utility width n

  let w_auto' = style [ width Auto ]
  let w_px' = style [ width (Px 1.0) ]
  let w_full' = style [ width (Pct 100.0) ]
  let w_screen' = style [ width (Vw 100.0) ]
  let w_min' = style [ width Min_content ]
  let w_max' = style [ width Max_content ]
  let w_fit' = style [ width Fit_content ]
  let w_1_2' = style [ width (Pct 50.0) ]
  let w_1_3' = style [ width (Pct 33.333333) ]
  let w_2_3' = style [ width (Pct 66.666667) ]
  let w_1_4' = style [ width (Pct 25.0) ]
  let w_3_4' = style [ width (Pct 75.0) ]
  let w_1_5' = style [ width (Pct 20.0) ]
  let w_2_5' = style [ width (Pct 40.0) ]
  let w_3_5' = style [ width (Pct 60.0) ]
  let w_4_5' = style [ width (Pct 80.0) ]

  (* Int-based width function for Tailwind scale (n * 0.25rem) *)

  let h' size =
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
    | `Rem n -> spacing_utility height n

  let h_auto' = style [ height Auto ]
  let h_px' = style [ height (Px 1.0) ]
  let h_full' = style [ height (Pct 100.0) ]
  let h_screen' = style [ height (Vh 100.0) ]
  let h_min' = style [ height Min_content ]
  let h_max' = style [ height Max_content ]
  let h_fit' = style [ height Fit_content ]
  let h_1_2' = style [ height (Pct 50.0) ]
  let h_1_3' = style [ height (Pct 33.333333) ]
  let h_2_3' = style [ height (Pct 66.666667) ]
  let h_1_4' = style [ height (Pct 25.0) ]
  let h_3_4' = style [ height (Pct 75.0) ]
  let h_1_5' = style [ height (Pct 20.0) ]
  let h_2_5' = style [ height (Pct 40.0) ]
  let h_3_5' = style [ height (Pct 60.0) ]
  let h_4_5' = style [ height (Pct 80.0) ]

  (* Int-based height function for Tailwind scale (n * 0.25rem) *)

  let min_w' size =
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
    | `Rem n -> spacing_utility min_width n

  let min_w_0' = style [ min_width (Px 0.) ]
  let min_w_full' = style [ min_width (Pct 100.0) ]
  let min_w_min' = style [ min_width Min_content ]
  let min_w_max' = style [ min_width Max_content ]
  let min_w_fit' = style [ min_width Fit_content ]

  (* Int-based min-width function for Tailwind scale (n * 0.25rem) *)

  let max_w' size =
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
    | `Rem n -> spacing_utility max_width n

  (* Named width theme variable *)
  let width_xl = Var.theme Css.Length "width-xl" ~order:(5, 20)

  (* Container size theme variables *)
  let container_xs = Var.theme Css.Length "container-xs" ~order:(5, 0)
  let container_sm = Var.theme Css.Length "container-sm" ~order:(5, 1)
  let container_md = Var.theme Css.Length "container-md" ~order:(5, 2)
  let container_lg = Var.theme Css.Length "container-lg" ~order:(5, 3)
  let container_xl = Var.theme Css.Length "container-xl" ~order:(5, 4)
  let container_2xl = Var.theme Css.Length "container-2xl" ~order:(5, 5)
  let container_3xl = Var.theme Css.Length "container-3xl" ~order:(5, 6)
  let container_4xl = Var.theme Css.Length "container-4xl" ~order:(5, 7)
  let container_5xl = Var.theme Css.Length "container-5xl" ~order:(5, 8)
  let container_6xl = Var.theme Css.Length "container-6xl" ~order:(5, 9)
  let container_7xl = Var.theme Css.Length "container-7xl" ~order:(5, 10)
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
  let max_w_screen_sm' = style [ max_width (Px 640.) ]
  let max_w_screen_md' = style [ max_width (Px 768.) ]
  let max_w_screen_lg' = style [ max_width (Px 1024.) ]
  let max_w_screen_xl' = style [ max_width (Px 1280.) ]
  let max_w_screen_2xl' = style [ max_width (Px 1536.) ]

  (* Int-based max-width function for Tailwind scale (n * 0.25rem) *)

  let min_h' size =
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
    | `Rem n -> spacing_utility min_height n

  let min_h_0' = style [ min_height (Px 0.) ]
  let min_h_full' = style [ min_height (Pct 100.0) ]
  let min_h_screen' = style [ min_height (Vh 100.0) ]
  let min_h_min' = style [ min_height Min_content ]
  let min_h_max' = style [ min_height Max_content ]
  let min_h_fit' = style [ min_height Fit_content ]

  (* Int-based min-height function for Tailwind scale (n * 0.25rem) *)

  let max_h' size =
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
    | `Rem n -> spacing_utility max_height n

  let max_h_none' = style [ max_height None ]
  let max_h_full' = style [ max_height (Pct 100.0) ]
  let max_h_screen' = style [ max_height (Vh 100.0) ]
  let max_h_min' = style [ max_height Min_content ]
  let max_h_max' = style [ max_height Max_content ]
  let max_h_fit' = style [ max_height Fit_content ]

  (* Int-based max-height function for Tailwind scale (n * 0.25rem) *)

  let fraction_table =
    [
      ("1/2", 50.0);
      ("1/3", 33.333333);
      ("2/3", 66.666667);
      ("1/4", 25.0);
      ("3/4", 75.0);
      ("1/5", 20.0);
      ("2/5", 40.0);
      ("3/5", 60.0);
      ("4/5", 80.0);
      ("1/6", 16.666667);
      ("5/6", 83.333333);
    ]

  let aspect_auto' = style [ Css.aspect_ratio Auto ]
  let aspect_square' = style [ Css.aspect_ratio (Ratio (1., 1.)) ]
  let aspect_video' = style [ Css.aspect_ratio (Ratio (16., 9.)) ]

  let aspect_ratio' w h =
    style [ Css.aspect_ratio (Ratio (float_of_int w, float_of_int h)) ]

  let to_style = function
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
        match f with
        | "1/2" -> w_1_2'
        | "1/3" -> w_1_3'
        | "2/3" -> w_2_3'
        | "1/4" -> w_1_4'
        | "3/4" -> w_3_4'
        | "1/5" -> w_1_5'
        | "2/5" -> w_2_5'
        | "3/5" -> w_3_5'
        | "4/5" -> w_4_5'
        | _ -> failwith ("Unknown width fraction: " ^ f))
    | W_arbitrary len -> style [ width len ]
    | W_dvw -> style [ width (Dvw 100.) ]
    | W_lvw -> style [ width (Lvw 100.) ]
    | W_svw -> style [ width (Svw 100.) ]
    | W_xl ->
        let decl, ref_ = Var.binding width_xl (Rem 36.0) in
        style [ decl; width (Var ref_) ]
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
        match f with
        | "1/2" -> h_1_2'
        | "1/3" -> h_1_3'
        | "2/3" -> h_2_3'
        | "1/4" -> h_1_4'
        | "3/4" -> h_3_4'
        | "1/5" -> h_1_5'
        | "2/5" -> h_2_5'
        | "3/5" -> h_3_5'
        | "4/5" -> h_4_5'
        | _ -> failwith ("Unknown height fraction: " ^ f))
    | H_arbitrary len -> style [ height len ]
    | H_dvh -> style [ height (Dvh 100.) ]
    | H_lvh -> style [ height (Lvh 100.) ]
    | H_svh -> style [ height (Svh 100.) ]
    | H_lh -> style [ height (Lh 1.) ]
    (* Min-width utilities *)
    | Min_w_0 -> min_w_0'
    | Min_w_full -> min_w_full'
    | Min_w_min -> min_w_min'
    | Min_w_max -> min_w_max'
    | Min_w_fit -> min_w_fit'
    | Min_w_auto -> style [ min_width Auto ]
    | Min_w_spacing n -> min_w' (`Rem n)
    | Min_w_arbitrary len -> style [ min_width len ]
    | Min_w_xl ->
        let decl, ref_ = Var.binding container_xl (Rem 36.0) in
        style [ decl; min_width (Var ref_) ]
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
    | Min_h_lh -> style [ min_height (Lh 1.) ]
    | Min_h_spacing n -> min_h' (`Rem n)
    | Min_h_arbitrary len -> style [ min_height len ]
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
    | Max_h_lh -> style [ max_height (Lh 1.) ]
    | Max_h_spacing n -> max_h' (`Rem n)
    | Max_h_arbitrary len -> style [ max_height len ]
    (* Size utilities *)
    | Size_auto -> style [ width Auto; height Auto ]
    | Size_full -> style [ width (Pct 100.0); height (Pct 100.0) ]
    | Size_min -> style [ width Min_content; height Min_content ]
    | Size_max -> style [ width Max_content; height Max_content ]
    | Size_fit -> style [ width Fit_content; height Fit_content ]
    | Size_spacing n ->
        let class_units = int_of_float (n *. 4.) in
        let spacing_v = Theme.get_spacing_var class_units in
        let concrete_value = Theme.spacing_value class_units in
        let decl, spacing_ref = Var.binding spacing_v concrete_value in
        let spacing_value : Css.length = Css.Var spacing_ref in
        style (decl :: [ width spacing_value; height spacing_value ])
    | Size_fraction f -> (
        match
          List.assoc_opt f
            [
              ("1/2", 50.0);
              ("1/3", 33.333333);
              ("2/3", 66.666667);
              ("1/4", 25.0);
              ("3/4", 75.0);
              ("1/5", 20.0);
              ("2/5", 40.0);
              ("3/5", 60.0);
              ("4/5", 80.0);
              ("1/6", 16.666667);
              ("5/6", 83.333333);
            ]
        with
        | Some pct -> style [ width (Pct pct); height (Pct pct) ]
        | None -> failwith ("Unknown size fraction: " ^ f))
    | Size_arbitrary len -> style [ width len; height len ]
    (* inline-size utilities *)
    | Inline_fraction f -> (
        match List.assoc_opt f fraction_table with
        | Some pct -> style [ inline_size (Pct pct) ]
        | None -> failwith ("Unknown inline-size fraction: " ^ f))
    | Inline_spacing n -> spacing_utility inline_size n
    | Inline_arbitrary len -> style [ inline_size len ]
    | Inline_auto -> style [ inline_size Auto ]
    | Inline_dvw -> style [ inline_size (Dvw 100.) ]
    | Inline_fit -> style [ inline_size Fit_content ]
    | Inline_full -> style [ inline_size (Pct 100.) ]
    | Inline_lvw -> style [ inline_size (Lvw 100.) ]
    | Inline_max -> style [ inline_size Max_content ]
    | Inline_min -> style [ inline_size Min_content ]
    | Inline_screen -> style [ inline_size (Vw 100.) ]
    | Inline_svw -> style [ inline_size (Svw 100.) ]
    | Inline_xl ->
        let decl, ref_ = Var.binding container_xl (Rem 36.0) in
        style [ decl; inline_size (Var ref_) ]
    (* min-inline-size utilities *)
    | Min_inline_spacing n -> spacing_utility min_inline_size n
    | Min_inline_arbitrary len -> style [ min_inline_size len ]
    | Min_inline_auto -> style [ min_inline_size Auto ]
    | Min_inline_fit -> style [ min_inline_size Fit_content ]
    | Min_inline_full -> style [ min_inline_size (Pct 100.) ]
    | Min_inline_max -> style [ min_inline_size Max_content ]
    | Min_inline_min -> style [ min_inline_size Min_content ]
    | Min_inline_xl ->
        let decl, ref_ = Var.binding container_xl (Rem 36.0) in
        style [ decl; min_inline_size (Var ref_) ]
    (* max-inline-size utilities *)
    | Max_inline_spacing n -> spacing_utility max_inline_size n
    | Max_inline_arbitrary len -> style [ max_inline_size len ]
    | Max_inline_fit -> style [ max_inline_size Fit_content ]
    | Max_inline_full -> style [ max_inline_size (Pct 100.) ]
    | Max_inline_max -> style [ max_inline_size Max_content ]
    | Max_inline_none -> style [ max_inline_size None ]
    | Max_inline_xl ->
        let decl, ref_ = Var.binding container_xl (Rem 36.0) in
        style [ decl; max_inline_size (Var ref_) ]
    (* block-size utilities *)
    | Block_fraction f -> (
        match List.assoc_opt f fraction_table with
        | Some pct -> style [ block_size (Pct pct) ]
        | None -> failwith ("Unknown block-size fraction: " ^ f))
    | Block_spacing n -> spacing_utility block_size n
    | Block_arbitrary len -> style [ block_size len ]
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
    | Min_block_arbitrary len -> style [ min_block_size len ]
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
    | Max_block_arbitrary len -> style [ max_block_size len ]
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

  let err_not_utility = Error (`Msg "Not a sizing utility")

  let err_invalid_value name value =
    Error (`Msg ("Invalid " ^ name ^ " value: " ^ value))

  let parse_arbitrary s : Css.length option =
    (* Parse [4px] or [1rem] etc. *)
    let len = String.length s in
    if len > 2 && s.[0] = '[' && s.[len - 1] = ']' then
      let inner = String.sub s 1 (len - 2) in
      if String.ends_with ~suffix:"px" inner then
        let n = String.sub inner 0 (String.length inner - 2) in
        match float_of_string_opt n with
        | Some f -> Some (Css.Px f)
        | None -> None
      else if String.ends_with ~suffix:"rem" inner then
        let n = String.sub inner 0 (String.length inner - 3) in
        match float_of_string_opt n with
        | Some f -> Some (Css.Rem f)
        | None -> None
      else None
    else None

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
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
      | "xl" -> Ok W_xl
      | frac when String.contains frac '/' ->
          if List.mem_assoc frac fraction_table then Ok (W_fraction frac)
          else err_invalid_value "width fraction" frac
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (W_arbitrary len)
          | None -> err_invalid_value "width" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (W_spacing (n *. 0.25))
          | _ -> err_invalid_value "width" v)
    in
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
      | "lh" -> Ok H_lh
      | frac when String.contains frac '/' ->
          if List.mem_assoc frac fraction_table then Ok (H_fraction frac)
          else err_invalid_value "height fraction" frac
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (H_arbitrary len)
          | None -> err_invalid_value "height" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (H_spacing (n *. 0.25))
          | _ -> err_invalid_value "height" v)
    in
    let parse_min_w = function
      | "0" -> Ok Min_w_0
      | "full" -> Ok Min_w_full
      | "min" -> Ok Min_w_min
      | "max" -> Ok Min_w_max
      | "fit" -> Ok Min_w_fit
      | "auto" -> Ok Min_w_auto
      | "xl" -> Ok Min_w_xl
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Min_w_arbitrary len)
          | None -> err_invalid_value "min-width" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Min_w_spacing (n *. 0.25))
          | _ -> err_invalid_value "min-width" v)
    in
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
      | "lh" -> Ok Min_h_lh
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Min_h_arbitrary len)
          | None -> err_invalid_value "min-height" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Min_h_spacing (n *. 0.25))
          | _ -> err_invalid_value "min-height" v)
    in
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
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Max_w_spacing (n *. 0.25))
          | _ -> err_invalid_value "max-width" v)
    in
    let parse_max_w_screen = function
      | "sm" -> Ok Max_w_screen_sm
      | "md" -> Ok Max_w_screen_md
      | "lg" -> Ok Max_w_screen_lg
      | "xl" -> Ok Max_w_screen_xl
      | "2xl" -> Ok Max_w_screen_2xl
      | s -> err_invalid_value "max-width screen size" s
    in
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
      | "lh" -> Ok Max_h_lh
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Max_h_arbitrary len)
          | None -> err_invalid_value "max-height" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Max_h_spacing (n *. 0.25))
          | _ -> err_invalid_value "max-height" v)
    in
    let parse_size = function
      | "auto" -> Ok Size_auto
      | "full" -> Ok Size_full
      | "min" -> Ok Size_min
      | "max" -> Ok Size_max
      | "fit" -> Ok Size_fit
      | frac when String.contains frac '/' ->
          if List.mem_assoc frac fraction_table then Ok (Size_fraction frac)
          else err_invalid_value "size fraction" frac
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Size_arbitrary len)
          | None -> err_invalid_value "size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Size_spacing (n *. 0.25))
          | _ -> err_invalid_value "size" v)
    in
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
      | "xl" -> Ok Inline_xl
      | frac when String.contains frac '/' ->
          if List.mem_assoc frac fraction_table then Ok (Inline_fraction frac)
          else err_invalid_value "inline-size fraction" frac
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Inline_arbitrary len)
          | None -> err_invalid_value "inline-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Inline_spacing (n *. 0.25))
          | _ -> err_invalid_value "inline-size" v)
    in
    let parse_min_inline = function
      | "auto" -> Ok Min_inline_auto
      | "fit" -> Ok Min_inline_fit
      | "full" -> Ok Min_inline_full
      | "max" -> Ok Min_inline_max
      | "min" -> Ok Min_inline_min
      | "xl" -> Ok Min_inline_xl
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Min_inline_arbitrary len)
          | None -> err_invalid_value "min-inline-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Min_inline_spacing (n *. 0.25))
          | _ -> err_invalid_value "min-inline-size" v)
    in
    let parse_max_inline = function
      | "fit" -> Ok Max_inline_fit
      | "full" -> Ok Max_inline_full
      | "max" -> Ok Max_inline_max
      | "none" -> Ok Max_inline_none
      | "xl" -> Ok Max_inline_xl
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Max_inline_arbitrary len)
          | None -> err_invalid_value "max-inline-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Max_inline_spacing (n *. 0.25))
          | _ -> err_invalid_value "max-inline-size" v)
    in
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
          if List.mem_assoc frac fraction_table then Ok (Block_fraction frac)
          else err_invalid_value "block-size fraction" frac
      | v when String.length v > 0 && v.[0] = '[' -> (
          match parse_arbitrary v with
          | Some len -> Ok (Block_arbitrary len)
          | None -> err_invalid_value "block-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Block_spacing (n *. 0.25))
          | _ -> err_invalid_value "block-size" v)
    in
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
          | Some len -> Ok (Min_block_arbitrary len)
          | None -> err_invalid_value "min-block-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Min_block_spacing (n *. 0.25))
          | _ -> err_invalid_value "min-block-size" v)
    in
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
          | Some len -> Ok (Max_block_arbitrary len)
          | None -> err_invalid_value "max-block-size" v)
      | v -> (
          match float_of_string_opt v with
          | Some n when n >= 0. -> Ok (Max_block_spacing (n *. 0.25))
          | _ -> err_invalid_value "max-block-size" v)
    in
    match parts with
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

  let suborder = function
    (* Height utilities (0-99999) Tailwind sorts: spacing values first, then
       keywords alphabetically (auto, fit, full, max, min, px, screen).
       Fractions sort with their integer part. *)
    | H_spacing n -> spacing_suborder n
    | H_fraction f -> (
        (* Extract the numerator to sort fractions with their integer part *)
        match String.split_on_char '/' f with
        | [ num; _ ] -> (
            try int_of_string num * 10000
            with Failure _ -> 50000 (* Invalid numerator *))
        | _ -> 50000)
    | H_auto -> 90000
    | H_fit -> 90001
    | H_full -> 90002
    | H_max -> 90003
    | H_min -> 90004
    | H_px -> 90005
    | H_screen -> 90006
    | H_dvh -> 90007
    | H_lh -> 90008
    | H_lvh -> 90009
    | H_svh -> 90010
    | H_arbitrary _ -> 95000
    (* Max-height utilities (100000-199999) - comes after height *)
    (* Order: fit, full, max, min, none, screen *)
    | Max_h_fit -> 100000
    | Max_h_full -> 100001
    | Max_h_max -> 100002
    | Max_h_min -> 100003
    | Max_h_none -> 100004
    | Max_h_screen -> 100005
    | Max_h_dvh -> 100006
    | Max_h_lvh -> 100007
    | Max_h_svh -> 100008
    | Max_h_lh -> 100009
    | Max_h_spacing n -> 100100 + spacing_suborder n
    | Max_h_arbitrary _ -> 100200
    (* Min-height utilities (200000-299999) - comes after max-height *)
    | Min_h_0 -> 200000
    | Min_h_full -> 200001
    | Min_h_screen -> 200002
    | Min_h_min -> 200003
    | Min_h_max -> 200004
    | Min_h_fit -> 200005
    | Min_h_auto -> 200006
    | Min_h_dvh -> 200007
    | Min_h_lvh -> 200008
    | Min_h_svh -> 200009
    | Min_h_lh -> 200010
    | Min_h_spacing n -> 200100 + spacing_suborder n
    | Min_h_arbitrary _ -> 200200
    (* Width utilities (300000-399999) Numeric widths (w-0, w-1, etc.) are
       ordered by their numeric value via spacing_suborder. Keyword widths
       (w-auto, w-full, etc.) use fixed suborders and sort alphabetically within
       that group. *)
    | W_spacing n -> 300000 + spacing_suborder n
    (* Keyword widths: use a suborder range (300500-300599) that comes after all
       numeric widths (which max out around 300000 + 96*4 = 300384 for w-96),
       allowing them to sort alphabetically. *)
    | W_auto -> 300500
    | W_fit -> 300501
    | W_full -> 300502
    | W_max -> 300503
    | W_min -> 300504
    | W_px -> 300505
    | W_screen -> 300506
    | W_dvw -> 300507
    | W_lvw -> 300508
    | W_svw -> 300509
    (* Named widths after dvw/lvw/svw *)
    | W_xl -> 300510
    (* Fractions come after keywords *)
    | W_fraction _ -> 300600
    | W_arbitrary _ -> 300700
    (* Max-width utilities (400000-499999) - comes after width *)
    (* Numbered containers first *)
    | Max_w_xs -> 400000
    | Max_w_sm -> 400001
    | Max_w_md -> 400002
    | Max_w_lg -> 400003
    | Max_w_xl -> 400004
    | Max_w_2xl -> 400005
    | Max_w_3xl -> 400006
    | Max_w_4xl -> 400007
    | Max_w_5xl -> 400008
    | Max_w_6xl -> 400009
    | Max_w_7xl -> 400010
    (* Keywords after numbered containers - order: fit, full, max, min, none,
       prose *)
    | Max_w_fit -> 400011
    | Max_w_full -> 400012
    | Max_w_max -> 400013
    | Max_w_min -> 400014
    | Max_w_none -> 400015
    | Max_w_prose -> 400016
    | Max_w_screen_sm -> 400020
    | Max_w_screen_md -> 400021
    | Max_w_screen_lg -> 400022
    | Max_w_screen_xl -> 400023
    | Max_w_screen_2xl -> 400024
    | Max_w_spacing n -> 400100 + spacing_suborder n
    (* Min-width utilities (500000-599999) - comes after max-width *)
    | Min_w_0 -> 500000
    | Min_w_full -> 500001
    | Min_w_min -> 500002
    | Min_w_max -> 500003
    | Min_w_fit -> 500004
    | Min_w_auto -> 500005
    | Min_w_spacing n -> 500100 + spacing_suborder n
    | Min_w_arbitrary _ -> 500200
    | Min_w_xl -> 500006
    (* Size utilities (600000-699999) *)
    | Size_auto -> 600000
    | Size_full -> 600001
    | Size_min -> 600002
    | Size_max -> 600003
    | Size_fit -> 600004
    | Size_spacing n -> 600100 + spacing_suborder n
    | Size_fraction _ -> 650000
    | Size_arbitrary _ -> 660000
    (* inline-size utilities (700000-799999) *)
    | Inline_spacing n -> 700000 + spacing_suborder n
    | Inline_fraction _ -> 700500
    | Inline_arbitrary _ -> 700600
    | Inline_auto -> 700700
    | Inline_dvw -> 700701
    | Inline_fit -> 700702
    | Inline_full -> 700703
    | Inline_lvw -> 700704
    | Inline_max -> 700705
    | Inline_min -> 700706
    | Inline_screen -> 700707
    | Inline_svw -> 700708
    | Inline_xl -> 700709
    (* min-inline-size utilities (750000-799999) *)
    | Min_inline_spacing n -> 750000 + spacing_suborder n
    | Min_inline_arbitrary _ -> 750500
    | Min_inline_auto -> 750600
    | Min_inline_fit -> 750601
    | Min_inline_full -> 750602
    | Min_inline_max -> 750603
    | Min_inline_min -> 750604
    | Min_inline_xl -> 750605
    (* max-inline-size utilities (780000-799999) *)
    | Max_inline_spacing n -> 780000 + spacing_suborder n
    | Max_inline_arbitrary _ -> 780500
    | Max_inline_fit -> 780600
    | Max_inline_full -> 780601
    | Max_inline_max -> 780602
    | Max_inline_none -> 780603
    | Max_inline_xl -> 780604
    (* block-size utilities (800000-899999) *)
    | Block_spacing n -> 800000 + spacing_suborder n
    | Block_fraction _ -> 800500
    | Block_arbitrary _ -> 800600
    | Block_auto -> 800700
    | Block_dvh -> 800701
    | Block_fit -> 800702
    | Block_full -> 800703
    | Block_lh -> 800704
    | Block_lvh -> 800705
    | Block_max -> 800706
    | Block_min -> 800707
    | Block_screen -> 800708
    | Block_svh -> 800709
    (* min-block-size utilities (850000-899999) *)
    | Min_block_spacing n -> 850000 + spacing_suborder n
    | Min_block_arbitrary _ -> 850500
    | Min_block_auto -> 850600
    | Min_block_dvh -> 850601
    | Min_block_fit -> 850602
    | Min_block_full -> 850603
    | Min_block_lh -> 850604
    | Min_block_lvh -> 850605
    | Min_block_max -> 850606
    | Min_block_min -> 850607
    | Min_block_screen -> 850608
    | Min_block_svh -> 850609
    (* max-block-size utilities (880000-899999) *)
    | Max_block_spacing n -> 880000 + spacing_suborder n
    | Max_block_arbitrary _ -> 880500
    | Max_block_dvh -> 880600
    | Max_block_fit -> 880601
    | Max_block_full -> 880602
    | Max_block_lh -> 880603
    | Max_block_lvh -> 880604
    | Max_block_max -> 880605
    | Max_block_min -> 880606
    | Max_block_none -> 880607
    | Max_block_screen -> 880608
    | Max_block_svh -> 880609
    (* Aspect utilities (900000-) *)
    | Aspect_auto -> 900000
    | Aspect_square -> 900001
    | Aspect_video -> 900002
    | Aspect_ratio (w, h) -> 900100 + (w * 100) + h

  let to_class = function
    (* Width utilities *)
    | W_auto -> "w-auto"
    | W_full -> "w-full"
    | W_screen -> "w-screen"
    | W_min -> "w-min"
    | W_max -> "w-max"
    | W_fit -> "w-fit"
    | W_px -> "w-px"
    | W_spacing n -> "w-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | W_fraction f -> "w-" ^ f
    | W_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "w-[" ^ len_str ^ "]"
    | W_dvw -> "w-dvw"
    | W_lvw -> "w-lvw"
    | W_svw -> "w-svw"
    | W_xl -> "w-xl"
    (* Height utilities *)
    | H_auto -> "h-auto"
    | H_full -> "h-full"
    | H_screen -> "h-screen"
    | H_min -> "h-min"
    | H_max -> "h-max"
    | H_fit -> "h-fit"
    | H_px -> "h-px"
    | H_spacing n -> "h-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | H_fraction f -> "h-" ^ f
    | H_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "h-[" ^ len_str ^ "]"
    | H_dvh -> "h-dvh"
    | H_lh -> "h-lh"
    | H_lvh -> "h-lvh"
    | H_svh -> "h-svh"
    (* Min-width utilities *)
    | Min_w_0 -> "min-w-0"
    | Min_w_full -> "min-w-full"
    | Min_w_min -> "min-w-min"
    | Min_w_max -> "min-w-max"
    | Min_w_fit -> "min-w-fit"
    | Min_w_auto -> "min-w-auto"
    | Min_w_spacing n -> "min-w-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Min_w_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "min-w-[" ^ len_str ^ "]"
    | Min_w_xl -> "min-w-xl"
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
    | Max_w_spacing n -> "max-w-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
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
    | Min_h_lh -> "min-h-lh"
    | Min_h_spacing n -> "min-h-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Min_h_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "min-h-[" ^ len_str ^ "]"
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
    | Max_h_lh -> "max-h-lh"
    | Max_h_spacing n -> "max-h-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Max_h_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "max-h-[" ^ len_str ^ "]"
    (* Size utilities *)
    | Size_auto -> "size-auto"
    | Size_full -> "size-full"
    | Size_min -> "size-min"
    | Size_max -> "size-max"
    | Size_fit -> "size-fit"
    | Size_spacing n -> "size-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Size_fraction f -> "size-" ^ f
    | Size_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "size-[" ^ len_str ^ "]"
    (* inline-size utilities *)
    | Inline_fraction f -> "inline-" ^ f
    | Inline_spacing n -> "inline-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Inline_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "inline-[" ^ len_str ^ "]"
    | Inline_auto -> "inline-auto"
    | Inline_dvw -> "inline-dvw"
    | Inline_fit -> "inline-fit"
    | Inline_full -> "inline-full"
    | Inline_lvw -> "inline-lvw"
    | Inline_max -> "inline-max"
    | Inline_min -> "inline-min"
    | Inline_screen -> "inline-screen"
    | Inline_svw -> "inline-svw"
    | Inline_xl -> "inline-xl"
    (* min-inline-size utilities *)
    | Min_inline_spacing n ->
        "min-inline-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Min_inline_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "min-inline-[" ^ len_str ^ "]"
    | Min_inline_auto -> "min-inline-auto"
    | Min_inline_fit -> "min-inline-fit"
    | Min_inline_full -> "min-inline-full"
    | Min_inline_max -> "min-inline-max"
    | Min_inline_min -> "min-inline-min"
    | Min_inline_xl -> "min-inline-xl"
    (* max-inline-size utilities *)
    | Max_inline_spacing n ->
        "max-inline-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Max_inline_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "max-inline-[" ^ len_str ^ "]"
    | Max_inline_fit -> "max-inline-fit"
    | Max_inline_full -> "max-inline-full"
    | Max_inline_max -> "max-inline-max"
    | Max_inline_none -> "max-inline-none"
    | Max_inline_xl -> "max-inline-xl"
    (* block-size utilities *)
    | Block_fraction f -> "block-" ^ f
    | Block_spacing n -> "block-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Block_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "block-[" ^ len_str ^ "]"
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
    | Min_block_spacing n ->
        "min-block-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Min_block_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "min-block-[" ^ len_str ^ "]"
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
    | Max_block_spacing n ->
        "max-block-" ^ Css.Pp.to_string Css.Pp.float (n *. 4.)
    | Max_block_arbitrary len ->
        let pp_float n =
          let s = string_of_float n in
          if String.ends_with ~suffix:"." s then
            String.sub s 0 (String.length s - 1)
          else s
        in
        let len_str =
          match len with
          | Css.Px n -> pp_float n ^ "px"
          | Css.Rem n -> pp_float n ^ "rem"
          | _ -> Css.Pp.to_string Css.pp_length len
        in
        "max-block-[" ^ len_str ^ "]"
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
        "aspect-[" ^ string_of_int w ^ "/" ^ string_of_int h ^ "]"
end

open Handler

(** Register the sizing utility handlers *)
let () = Utility.register (module Handler)

(** Public API returning Utility.t *)
let utility x = Utility.base (Self x)

let () = () (* Ensure utility is defined before usage below *)

(* Expose prime helpers wrapped as Utility.t *)
let w' = function
  | `None -> utility (W_spacing 0.)
  | `Xs -> utility (W_spacing 0.5)
  | `Sm -> utility (W_spacing 1.0)
  | `Md -> utility (W_spacing 1.5)
  | `Lg -> utility (W_spacing 2.0)
  | `Xl -> utility (W_spacing 3.0)
  | `Xl_2 -> utility (W_spacing 4.0)
  | `Xl_3 -> utility (W_spacing 6.0)
  | `Full -> utility W_full
  | `Rem n -> utility (W_spacing n)

let h' = function
  | `None -> utility (H_spacing 0.)
  | `Xs -> utility (H_spacing 0.5)
  | `Sm -> utility (H_spacing 1.0)
  | `Md -> utility (H_spacing 1.5)
  | `Lg -> utility (H_spacing 2.0)
  | `Xl -> utility (H_spacing 3.0)
  | `Xl_2 -> utility (H_spacing 4.0)
  | `Xl_3 -> utility (H_spacing 6.0)
  | `Full -> utility H_full
  | `Rem n -> utility (H_spacing n)

let min_w' = function
  | `None -> utility Min_w_0
  | `Xs -> utility (Min_w_spacing 0.5)
  | `Sm -> utility (Min_w_spacing 1.0)
  | `Md -> utility (Min_w_spacing 1.5)
  | `Lg -> utility (Min_w_spacing 2.0)
  | `Xl -> utility (Min_w_spacing 3.0)
  | `Xl_2 -> utility (Min_w_spacing 4.0)
  | `Xl_3 -> utility (Min_w_spacing 6.0)
  | `Full -> utility Min_w_full
  | `Rem n -> utility (Min_w_spacing n)

let max_w' = function
  | `None -> utility Max_w_none
  | `Xs -> utility Max_w_xs
  | `Sm -> utility Max_w_sm
  | `Md -> utility Max_w_md
  | `Lg -> utility Max_w_lg
  | `Xl -> utility Max_w_xl
  | `Xl_2 -> utility Max_w_2xl
  | `Xl_3 -> utility Max_w_3xl
  | `Full -> utility Max_w_full
  | `Rem n -> utility (Max_w_spacing n)

let min_h' = function
  | `None -> utility Min_h_0
  | `Xs -> utility (Min_h_spacing 0.5)
  | `Sm -> utility (Min_h_spacing 1.0)
  | `Md -> utility (Min_h_spacing 1.5)
  | `Lg -> utility (Min_h_spacing 2.0)
  | `Xl -> utility (Min_h_spacing 3.0)
  | `Xl_2 -> utility (Min_h_spacing 4.0)
  | `Xl_3 -> utility (Min_h_spacing 6.0)
  | `Full -> utility Min_h_full
  | `Rem n -> utility (Min_h_spacing n)

let max_h' = function
  | `None -> utility Max_h_none
  | `Xs -> utility (Max_h_spacing 0.5)
  | `Sm -> utility (Max_h_spacing 1.0)
  | `Md -> utility (Max_h_spacing 1.5)
  | `Lg -> utility (Max_h_spacing 2.0)
  | `Xl -> utility (Max_h_spacing 3.0)
  | `Xl_2 -> utility (Max_h_spacing 4.0)
  | `Xl_3 -> utility (Max_h_spacing 6.0)
  | `Full -> utility Max_h_full
  | `Rem n -> utility (Max_h_spacing n)

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
let aspect_ratio w h = utility (Aspect_ratio (w, h))

(* Order exposure for this module *)
let order (u : Utility.base) =
  match u with Self x -> Some (priority, suborder x) | _ -> None
