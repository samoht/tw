(** Prose typography utilities

    Complete implementation of Tailwind Typography plugin with:
    - All HTML elements styled with :where() for low specificity
    - not-prose exclusion mechanism
    - Full CSS variable theming
    - Size and color variants *)

open Css

(* Helper to create the :where() selector with not-prose exclusion *)
let where_selector base element =
  Printf.sprintf
    "%s :where(%s):not(:where([class~=not-prose],[class~=not-prose] *))" base
    element

(* Helper to define color properties using oklch *)
let oklch_color l c h = Oklch { l; c; h }

(* Prose variant type *)
type variant =
  [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate | `Zinc | `Neutral | `Stone ]

(* Create CSS variable definitions with oklch colors *)
(* Base prose colors *)
let prose_body_def, prose_body_var =
  Var.utility Var.Prose_body (oklch_color 37.3 0.034 259.733)

let prose_headings_def, prose_headings_var =
  Var.utility Var.Prose_headings (oklch_color 21.0 0.034 264.665)

let prose_lead_def, prose_lead_var =
  Var.utility Var.Prose_lead (oklch_color 44.6 0.030 256.802)

let prose_links_def, prose_links_var =
  Var.utility Var.Prose_links (oklch_color 21.0 0.034 264.665)

let prose_bold_def, prose_bold_var =
  Var.utility Var.Prose_bold (oklch_color 21.0 0.034 264.665)

let prose_counters_def, prose_counters_var =
  Var.utility Var.Prose_counters (oklch_color 55.1 0.027 264.364)

let prose_bullets_def, prose_bullets_var =
  Var.utility Var.Prose_bullets (oklch_color 87.2 0.010 258.338)

let prose_hr_def, prose_hr_var =
  Var.utility Var.Prose_hr (oklch_color 92.8 0.006 264.531)

let prose_quotes_def, prose_quotes_var =
  Var.utility Var.Prose_quotes (oklch_color 21.0 0.034 264.665)

let prose_quote_borders_def, prose_quote_borders_var =
  Var.utility Var.Prose_quote_borders (oklch_color 92.8 0.006 264.531)

let prose_captions_def, prose_captions_var =
  Var.utility Var.Prose_captions (oklch_color 55.1 0.027 264.364)

let prose_kbd_def, prose_kbd_var =
  Var.utility Var.Prose_kbd (oklch_color 21.0 0.034 264.665)

(* Special handling for kbd-shadows - it's RGB values without rgb() wrapper *)
let prose_kbd_shadows_def, _prose_kbd_shadows_var =
  Var.utility Var.Prose_kbd_shadows
    "NaN NaN NaN" (* This will need special handling *)

let prose_code_def, prose_code_var =
  Var.utility Var.Prose_code (oklch_color 21.0 0.034 264.665)

let prose_pre_code_def, prose_pre_code_var =
  Var.utility Var.Prose_pre_code (oklch_color 92.8 0.006 264.531)

let prose_pre_bg_def, prose_pre_bg_var =
  Var.utility Var.Prose_pre_bg (oklch_color 27.8 0.033 256.848)

let prose_th_borders_def, prose_th_borders_var =
  Var.utility Var.Prose_th_borders (oklch_color 87.2 0.010 258.338)

let prose_td_borders_def, prose_td_borders_var =
  Var.utility Var.Prose_td_borders (oklch_color 92.8 0.006 264.531)

(* Invert variants for dark mode *)
let prose_invert_body_def, _prose_invert_body_var =
  Var.utility Var.Prose_invert_body (oklch_color 87.2 0.010 258.338)

let prose_invert_headings_def, _prose_invert_headings_var =
  Var.utility Var.Prose_invert_headings (Hex { hash = true; value = "fff" })

let prose_invert_lead_def, _prose_invert_lead_var =
  Var.utility Var.Prose_invert_lead (oklch_color 70.7 0.022 261.325)

let prose_invert_links_def, _prose_invert_links_var =
  Var.utility Var.Prose_invert_links (Hex { hash = true; value = "fff" })

let prose_invert_bold_def, _prose_invert_bold_var =
  Var.utility Var.Prose_invert_bold (Hex { hash = true; value = "fff" })

let prose_invert_counters_def, _prose_invert_counters_var =
  Var.utility Var.Prose_invert_counters (oklch_color 70.7 0.022 261.325)

let prose_invert_bullets_def, _prose_invert_bullets_var =
  Var.utility Var.Prose_invert_bullets (oklch_color 44.6 0.030 256.802)

let prose_invert_hr_def, _prose_invert_hr_var =
  Var.utility Var.Prose_invert_hr (oklch_color 37.3 0.034 259.733)

let prose_invert_quotes_def, _prose_invert_quotes_var =
  Var.utility Var.Prose_invert_quotes (oklch_color 96.7 0.003 264.542)

let prose_invert_quote_borders_def, _prose_invert_quote_borders_var =
  Var.utility Var.Prose_invert_quote_borders (oklch_color 37.3 0.034 259.733)

let prose_invert_captions_def, _prose_invert_captions_var =
  Var.utility Var.Prose_invert_captions (oklch_color 70.7 0.022 261.325)

let prose_invert_kbd_def, _prose_invert_kbd_var =
  Var.utility Var.Prose_invert_kbd (Hex { hash = true; value = "fff" })

let prose_invert_kbd_shadows_def, _prose_invert_kbd_shadows_var =
  Var.utility Var.Prose_invert_kbd_shadows "255 255 255"

let prose_invert_code_def, _prose_invert_code_var =
  Var.utility Var.Prose_invert_code (Hex { hash = true; value = "fff" })

let prose_invert_pre_code_def, _prose_invert_pre_code_var =
  Var.utility Var.Prose_invert_pre_code (oklch_color 87.2 0.010 258.338)

let prose_invert_pre_bg_def, _prose_invert_pre_bg_var =
  Var.utility Var.Prose_invert_pre_bg (Hex { hash = true; value = "00000080" })

let prose_invert_th_borders_def, _prose_invert_th_borders_var =
  Var.utility Var.Prose_invert_th_borders (oklch_color 44.6 0.030 256.802)

let prose_invert_td_borders_def, _prose_invert_td_borders_var =
  Var.utility Var.Prose_invert_td_borders (oklch_color 37.3 0.034 259.733)

(* Export CSS variable definitions for external use *)

(** Convert prose variant to CSS class name *)
let css_variables =
  [
    prose_body_def;
    prose_headings_def;
    prose_lead_def;
    prose_links_def;
    prose_bold_def;
    prose_counters_def;
    prose_bullets_def;
    prose_hr_def;
    prose_quotes_def;
    prose_quote_borders_def;
    prose_captions_def;
    prose_kbd_def;
    prose_kbd_shadows_def;
    prose_code_def;
    prose_pre_code_def;
    prose_pre_bg_def;
    prose_th_borders_def;
    prose_td_borders_def;
    (* Invert variants *)
    prose_invert_body_def;
    prose_invert_headings_def;
    prose_invert_lead_def;
    prose_invert_links_def;
    prose_invert_bold_def;
    prose_invert_counters_def;
    prose_invert_bullets_def;
    prose_invert_hr_def;
    prose_invert_quotes_def;
    prose_invert_quote_borders_def;
    prose_invert_captions_def;
    prose_invert_kbd_def;
    prose_invert_kbd_shadows_def;
    prose_invert_code_def;
    prose_invert_pre_code_def;
    prose_invert_pre_bg_def;
    prose_invert_th_borders_def;
    prose_invert_td_borders_def;
  ]

let to_class = function
  | `Base -> "prose"
  | `Sm -> "prose-sm"
  | `Lg -> "prose-lg"
  | `Xl -> "prose-xl"
  | `Xl2 -> "prose-2xl"
  | `Gray -> "prose-gray"
  | `Slate -> "prose-slate"
  | `Zinc -> "prose-zinc"
  | `Neutral -> "prose-neutral"
  | `Stone -> "prose-stone"

(** Generate CSS rules for prose elements *)

(* Paragraph and lead styles *)
let paragraph_rules base =
  [
    Css.rule ~selector:(where_selector base "p")
      [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
    Css.rule
      ~selector:(where_selector base "[class~=lead]")
      [
        color (Var prose_lead_var);
        margin_top (Em 1.2);
        margin_bottom (Em 1.2);
        font_size (Em 1.25);
        line_height (Num 1.6);
      ];
  ]

(* Link styles *)
let link_rules base =
  [
    Css.rule ~selector:(where_selector base "a")
      [
        color (Var prose_links_var);
        font_weight (Weight 500);
        text_decoration Underline;
      ];
  ]

(* Strong and bold styles *)
let strong_rules base =
  [
    Css.rule
      ~selector:(where_selector base "strong")
      [ color (Var prose_bold_var); font_weight (Weight 600) ];
    (* Strong inherits color in certain contexts *)
    Css.rule ~selector:(where_selector base "a strong") [ color Inherit ];
    Css.rule
      ~selector:(where_selector base "blockquote strong")
      [ color Inherit ];
    Css.rule ~selector:(where_selector base "thead th strong") [ color Inherit ];
  ]

(* List styles *)
let list_rules base =
  [
    Css.rule ~selector:(where_selector base "ol")
      [
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_inline_start (Em 1.625);
        list_style_type Decimal;
      ];
    Css.rule
      ~selector:(where_selector base "ol[type=A]")
      [ list_style_type Upper_alpha ];
    Css.rule
      ~selector:(where_selector base "ol[type=a]")
      [ list_style_type Lower_alpha ];
    Css.rule
      ~selector:(where_selector base "ol[type=A s]")
      [ list_style_type Upper_alpha ];
    Css.rule
      ~selector:(where_selector base "ol[type=a s]")
      [ list_style_type Lower_alpha ];
    Css.rule
      ~selector:(where_selector base "ol[type=I]")
      [ list_style_type Upper_roman ];
    Css.rule
      ~selector:(where_selector base "ol[type=i]")
      [ list_style_type Lower_roman ];
    Css.rule
      ~selector:(where_selector base "ol[type=I s]")
      [ list_style_type Upper_roman ];
    Css.rule
      ~selector:(where_selector base "ol[type=i s]")
      [ list_style_type Lower_roman ];
    Css.rule
      ~selector:(where_selector base "ol[type=\"1\"]")
      [ list_style_type Decimal ];
    Css.rule ~selector:(where_selector base "ul")
      [
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_inline_start (Em 1.625);
        list_style_type Disc;
      ];
    Css.rule
      ~selector:(where_selector base "ol>li::marker")
      [ color (Var prose_counters_var); font_weight (Weight 400) ];
    Css.rule
      ~selector:(where_selector base "ul>li::marker")
      [ color (Var prose_bullets_var) ];
    Css.rule ~selector:(where_selector base "dt")
      [
        color (Var prose_headings_var);
        margin_top (Em 1.25);
        font_weight (Weight 600);
      ];
  ]

(* Heading styles *)
let heading_rules base =
  [
    Css.rule ~selector:(where_selector base "h1")
      [
        color (Var prose_headings_var);
        margin_top Zero;
        margin_bottom (Em 0.888889);
        font_size (Em 2.25);
        font_weight (Weight 800);
        line_height (Num 1.11111);
      ];
    Css.rule
      ~selector:(where_selector base "h1 strong")
      [ color Inherit; font_weight (Weight 900) ];
    Css.rule ~selector:(where_selector base "h2")
      [
        color (Var prose_headings_var);
        margin_top (Em 2.0);
        margin_bottom (Em 1.0);
        font_size (Em 1.5);
        font_weight (Weight 700);
        line_height (Num 1.33333);
      ];
    Css.rule
      ~selector:(where_selector base "h2 strong")
      [ color Inherit; font_weight (Weight 800) ];
    Css.rule ~selector:(where_selector base "h3")
      [
        color (Var prose_headings_var);
        margin_top (Em 1.6);
        margin_bottom (Em 0.6);
        font_size (Em 1.25);
        font_weight (Weight 600);
        line_height (Num 1.6);
      ];
    Css.rule
      ~selector:(where_selector base "h3 strong")
      [ color Inherit; font_weight (Weight 700) ];
    Css.rule ~selector:(where_selector base "h4")
      [
        color (Var prose_headings_var);
        margin_top (Em 1.5);
        margin_bottom (Em 0.5);
        font_weight (Weight 600);
        line_height (Num 1.5);
      ];
    Css.rule
      ~selector:(where_selector base "h4 strong")
      [ color Inherit; font_weight (Weight 700) ];
  ]

(* Horizontal rule styles *)
let hr_rules base =
  [
    Css.rule ~selector:(where_selector base "hr")
      [
        border_color (Var prose_hr_var);
        border_top_width (Px 1);
        margin_top (Em 3.0);
        margin_bottom (Em 3.0);
      ];
  ]

(* Blockquote styles *)
let blockquote_rules base =
  [
    Css.rule
      ~selector:(where_selector base "blockquote")
      [
        color (Var prose_quotes_var);
        border_inline_start_width (Rem 0.25);
        border_inline_start_color (Var prose_quote_borders_var);
        quotes "\"\\201C\"\"\\201D\"\"\\2018\"\"\\2019\"";
        margin_top (Em 1.6);
        margin_bottom (Em 1.6);
        padding_inline_start (Em 1.0);
        font_style Italic;
        font_weight (Weight 500);
      ];
    Css.rule
      ~selector:(where_selector base "blockquote p:first-of-type:before")
      [ content (String "open-quote") ];
    Css.rule
      ~selector:(where_selector base "blockquote p:last-of-type:after")
      [ content (String "close-quote") ];
  ]

(* Media element styles *)
let media_rules base =
  [
    Css.rule
      ~selector:(where_selector base "img")
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
    Css.rule
      ~selector:(where_selector base "picture")
      [ margin_top (Em 2.0); margin_bottom (Em 2.0); display Block ];
    Css.rule
      ~selector:(where_selector base "video")
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
  ]

(* Keyboard styles *)
let kbd_rules base =
  [
    Css.rule
      ~selector:(where_selector base "kbd")
      [
        color (Var prose_kbd_var);
        box_shadow
          (Raw
             "0 0 0 1px rgb(var(--tw-prose-kbd-shadows)/10%),0 3px 0 \
              rgb(var(--tw-prose-kbd-shadows)/10%)");
        padding_top (Em 0.1875);
        padding_inline_end (Em 0.375);
        padding_bottom (Em 0.1875);
        border_radius (Rem 0.3125);
        padding_inline_start (Em 0.375);
        font_family [ Inherit ];
        font_size (Em 0.875);
        font_weight (Weight 500);
      ];
  ]

(* Code styles *)
let code_rules base =
  [
    Css.rule
      ~selector:(where_selector base "code")
      [
        color (Var prose_code_var);
        font_size (Em 0.875);
        font_weight (Weight 600);
      ];
    Css.rule
      ~selector:(where_selector base "code:before")
      [ content (String "`") ];
    Css.rule
      ~selector:(where_selector base "code:after")
      [ content (String "`") ];
    (* Code inherits color in certain contexts *)
    Css.rule ~selector:(where_selector base "a code") [ color Inherit ];
    Css.rule ~selector:(where_selector base "h1 code") [ color Inherit ];
    Css.rule
      ~selector:(where_selector base "h2 code")
      [ color Inherit; font_size (Em 0.875) ];
    Css.rule
      ~selector:(where_selector base "h3 code")
      [ color Inherit; font_size (Em 0.9) ];
    Css.rule ~selector:(where_selector base "h4 code") [ color Inherit ];
    Css.rule ~selector:(where_selector base "blockquote code") [ color Inherit ];
    Css.rule ~selector:(where_selector base "thead th code") [ color Inherit ];
    (* Pre code block styles *)
    Css.rule
      ~selector:(where_selector base "pre")
      [
        color (Var prose_pre_code_var);
        background_color (Var prose_pre_bg_var);
        padding_top (Em 0.857143);
        padding_inline_end (Em 1.14286);
        padding_bottom (Em 0.857143);
        border_radius (Rem 0.375);
        margin_top (Em 1.71429);
        margin_bottom (Em 1.71429);
        padding_inline_start (Em 1.14286);
        font_size (Em 0.875);
        font_weight (Weight 400);
        line_height (Num 1.71429);
        overflow_x Auto;
      ];
    Css.rule
      ~selector:(where_selector base "pre code")
      [
        font_weight Inherit;
        color Inherit;
        font_size Inherit;
        font_family [ Inherit ];
        line_height Inherit;
        background_color Transparent;
        border_width Zero;
        border_radius Zero;
        padding Zero;
      ];
    Css.rule ~selector:(where_selector base "pre code:before") [ content None ];
    Css.rule ~selector:(where_selector base "pre code:after") [ content None ];
  ]

(* Table styles *)
let table_rules base =
  [
    Css.rule
      ~selector:(where_selector base "table")
      [
        table_layout Auto;
        width (Pct 100.0);
        margin_top (Em 2.0);
        margin_bottom (Em 2.0);
        font_size (Em 0.875);
        line_height (Num 1.71429);
      ];
    Css.rule
      ~selector:(where_selector base "thead")
      [
        border_bottom_width (Px 1);
        border_bottom_color (Var prose_th_borders_var);
      ];
    Css.rule
      ~selector:(where_selector base "thead th")
      [
        color (Var prose_headings_var);
        vertical_align Bottom;
        padding_inline_end (Em 0.571429);
        padding_bottom (Em 0.571429);
        padding_inline_start (Em 0.571429);
        font_weight (Weight 600);
      ];
    Css.rule
      ~selector:(where_selector base "tbody tr")
      [
        border_bottom_width (Px 1);
        border_bottom_color (Var prose_td_borders_var);
      ];
    Css.rule
      ~selector:(where_selector base "tbody tr:last-child")
      [ border_bottom_width Zero ];
    Css.rule
      ~selector:(where_selector base "tbody td")
      [ vertical_align Baseline ];
    Css.rule
      ~selector:(where_selector base "tfoot")
      [ border_top_width (Px 1); border_top_color (Var prose_th_borders_var) ];
    Css.rule ~selector:(where_selector base "tfoot td") [ vertical_align Top ];
    Css.rule ~selector:(where_selector base "th,td") [ text_align Start ];
  ]

(* Figure and figcaption styles *)
let figure_rules base =
  [
    Css.rule
      ~selector:(where_selector base "figure>*")
      [ margin_top Zero; margin_bottom Zero ];
    Css.rule
      ~selector:(where_selector base "figcaption")
      [
        color (Var prose_captions_var);
        margin_top (Em 0.857143);
        font_size (Em 0.875);
        line_height (Num 1.42857);
      ];
  ]

(* Additional list and spacing rules *)
let additional_rules base =
  [
    (* Picture img special handling *)
    Css.rule
      ~selector:(where_selector base "picture>img")
      [ margin_top Zero; margin_bottom Zero ];
    (* List item styles *)
    Css.rule ~selector:(where_selector base "li")
      [ margin_top (Em 0.5); margin_bottom (Em 0.5) ];
    Css.rule
      ~selector:(where_selector base "ol>li")
      [ padding_inline_start (Em 0.375) ];
    Css.rule
      ~selector:(where_selector base "ul>li")
      [ padding_inline_start (Em 0.375) ];
    (* Nested list paragraph spacing *)
    Css.rule
      ~selector:(where_selector base ".prose>ul>li p")
      [ margin_top (Em 0.75); margin_bottom (Em 0.75) ];
    Css.rule
      ~selector:(where_selector base ".prose>ul>li>p:first-child")
      [ margin_top (Em 1.25) ];
    Css.rule
      ~selector:(where_selector base ".prose>ul>li>p:last-child")
      [ margin_bottom (Em 1.25) ];
    Css.rule
      ~selector:(where_selector base ".prose>ol>li>p:first-child")
      [ margin_top (Em 1.25) ];
    Css.rule
      ~selector:(where_selector base ".prose>ol>li>p:last-child")
      [ margin_bottom (Em 1.25) ];
    (* Nested lists *)
    Css.rule
      ~selector:(where_selector base "ul ul,ul ol,ol ul,ol ol")
      [ margin_top (Em 0.75); margin_bottom (Em 0.75) ];
    (* Definition lists *)
    Css.rule ~selector:(where_selector base "dl")
      [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
    Css.rule ~selector:(where_selector base "dd")
      [ margin_top (Em 0.5); padding_inline_start (Em 1.625) ];
    (* Adjacent element spacing *)
    Css.rule ~selector:(where_selector base "hr+*") [ margin_top Zero ];
    Css.rule ~selector:(where_selector base "h2+*") [ margin_top Zero ];
    Css.rule ~selector:(where_selector base "h3+*") [ margin_top Zero ];
    Css.rule ~selector:(where_selector base "h4+*") [ margin_top Zero ];
    (* Table column padding *)
    Css.rule
      ~selector:(where_selector base "thead th:first-child")
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:(where_selector base "thead th:last-child")
      [ padding_inline_end Zero ];
    Css.rule
      ~selector:(where_selector base "tbody td,tfoot td")
      [
        padding_top (Em 0.571429);
        padding_inline_end (Em 0.571429);
        padding_bottom (Em 0.571429);
        padding_inline_start (Em 0.571429);
      ];
    Css.rule
      ~selector:
        (where_selector base "tbody td:first-child,tfoot td:first-child")
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:(where_selector base "tbody td:last-child,tfoot td:last-child")
      [ padding_inline_end Zero ];
    (* Figure *)
    Css.rule
      ~selector:(where_selector base "figure")
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
    (* First and last child margins *)
    Css.rule
      ~selector:(where_selector base ".prose>:first-child")
      [ margin_top Zero ];
    Css.rule
      ~selector:(where_selector base ".prose>:last-child")
      [ margin_bottom Zero ];
  ]

(* Base prose rules - combines all element rules *)
let base_prose_rules () =
  let base = ".prose" in

  (* First prose rule with just base styles *)
  let main_rule =
    Css.rule ~selector:".prose"
      [ color (Var prose_body_var); max_width (Ch 65.0) ]
  in

  (* Second prose rule with CSS variables - comes at the end *)
  let variables_rule =
    Css.rule ~selector:".prose"
      (css_variables @ [ font_size (Rem 1.0); line_height (Num 1.75) ])
  in

  let descendant_rules =
    paragraph_rules base @ link_rules base @ strong_rules base @ list_rules base
    @ heading_rules base @ hr_rules base @ blockquote_rules base
    @ media_rules base @ kbd_rules base @ code_rules base @ table_rules base
    @ figure_rules base @ additional_rules base
  in

  let all_rules = [ main_rule ] @ descendant_rules @ [ variables_rule ] in

  (* TW_DEBUG_START *)
  let prose_count =
    List.fold_left
      (fun acc rule -> if Css.selector rule = ".prose" then acc + 1 else acc)
      0 all_rules
  in
  Printf.printf "TW_DEBUG: base_prose_rules %d/%d\n" prose_count
    (List.length all_rules);

  (* TW_DEBUG_END *)
  all_rules

let sm_size_rules selector =
  [
    Css.rule ~selector [ font_size (Rem 0.875); line_height (Num 1.7142857) ];
    Css.rule
      ~selector:(where_selector selector "p")
      [ margin_top (Em 1.1428571); margin_bottom (Em 1.1428571) ];
    Css.rule
      ~selector:(where_selector selector "h1")
      [
        font_size (Em 2.1428571);
        margin_top Zero;
        margin_bottom (Em 0.8);
        line_height (Num 1.2);
      ];
    Css.rule
      ~selector:(where_selector selector "h2")
      [
        font_size (Em 1.4285714);
        margin_top (Em 1.6);
        margin_bottom (Em 0.8);
        line_height (Num 1.4);
      ];
    Css.rule
      ~selector:(where_selector selector "h3")
      [
        font_size (Em 1.2857143);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.4444444);
        line_height (Num 1.5555556);
      ];
  ]

let lg_size_rules selector =
  [
    Css.rule ~selector [ font_size (Rem 1.125); line_height (Num 1.7777778) ];
    Css.rule
      ~selector:(where_selector selector "p")
      [ margin_top (Em 1.3333333); margin_bottom (Em 1.3333333) ];
    Css.rule
      ~selector:(where_selector selector "h1")
      [
        font_size (Em 2.6666667);
        margin_top Zero;
        margin_bottom (Em 0.8333333);
        line_height (Num 1.0);
      ];
    Css.rule
      ~selector:(where_selector selector "h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.8888889);
        line_height (Num 1.3333333);
      ];
    Css.rule
      ~selector:(where_selector selector "h3")
      [
        font_size (Em 1.6666667);
        margin_top (Em 1.6666667);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.5);
      ];
  ]

let xl_size_rules selector =
  [
    Css.rule ~selector [ font_size (Rem 1.25); line_height (Num 1.8) ];
    Css.rule
      ~selector:(where_selector selector "p")
      [ margin_top (Em 1.2); margin_bottom (Em 1.2) ];
    Css.rule
      ~selector:(where_selector selector "h1")
      [
        font_size (Em 2.8);
        margin_top Zero;
        margin_bottom (Em 0.8571429);
        line_height (Num 1.0);
      ];
    Css.rule
      ~selector:(where_selector selector "h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.4);
        margin_bottom (Em 0.8);
        line_height (Num 1.2727273);
      ];
    Css.rule
      ~selector:(where_selector selector "h3")
      [
        font_size (Em 1.6);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.4444444);
      ];
  ]

let xl2_size_rules selector =
  [
    Css.rule ~selector [ font_size (Rem 1.5); line_height (Num 1.6666667) ];
    Css.rule
      ~selector:(where_selector selector "p")
      [ margin_top (Em 1.3333333); margin_bottom (Em 1.3333333) ];
    Css.rule
      ~selector:(where_selector selector "h1")
      [
        font_size (Em 2.6666667);
        margin_top Zero;
        margin_bottom (Em 0.875);
        line_height (Num 1.0);
      ];
    Css.rule
      ~selector:(where_selector selector "h2")
      [
        font_size (Em 2.0);
        margin_top (Em 1.5);
        margin_bottom (Em 0.8333333);
        line_height (Num 1.0833333);
      ];
    Css.rule
      ~selector:(where_selector selector "h3")
      [
        font_size (Em 1.5);
        margin_top (Em 1.5555556);
        margin_bottom (Em 0.6666667);
        line_height (Num 1.2222222);
      ];
  ]

(* Helper to update color variable definitions for color themes *)
let update_color_vars theme_name =
  match theme_name with
  | "gray" ->
      [
        Var.utility Var.Prose_body (oklch_color 55.1 0.027 264.364) |> fst;
        Var.utility Var.Prose_headings (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_lead (oklch_color 44.6 0.030 256.802) |> fst;
        Var.utility Var.Prose_links (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_bold (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_counters (oklch_color 55.1 0.027 264.364) |> fst;
        Var.utility Var.Prose_bullets (oklch_color 87.2 0.010 258.338) |> fst;
        Var.utility Var.Prose_hr (oklch_color 92.8 0.006 264.531) |> fst;
        Var.utility Var.Prose_quotes (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_quote_borders (oklch_color 92.8 0.006 264.531)
        |> fst;
        Var.utility Var.Prose_captions (oklch_color 70.7 0.022 261.325) |> fst;
        Var.utility Var.Prose_code (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_pre_code (oklch_color 92.8 0.006 264.531) |> fst;
        Var.utility Var.Prose_pre_bg (oklch_color 27.8 0.033 256.848) |> fst;
        Var.utility Var.Prose_th_borders (oklch_color 87.2 0.010 258.338) |> fst;
        Var.utility Var.Prose_td_borders (oklch_color 92.8 0.006 264.531) |> fst;
      ]
  | "slate" ->
      [
        Var.utility Var.Prose_body (oklch_color 52.5 0.027 257.576) |> fst;
        Var.utility Var.Prose_headings (oklch_color 19.2 0.025 268.573) |> fst;
        Var.utility Var.Prose_lead (oklch_color 42.3 0.029 255.908) |> fst;
        Var.utility Var.Prose_links (oklch_color 19.2 0.025 268.573) |> fst;
        Var.utility Var.Prose_bold (oklch_color 19.2 0.025 268.573) |> fst;
        Var.utility Var.Prose_counters (oklch_color 52.5 0.027 257.576) |> fst;
        Var.utility Var.Prose_bullets (oklch_color 85.8 0.012 255.094) |> fst;
        Var.utility Var.Prose_hr (oklch_color 91.9 0.007 264.837) |> fst;
        Var.utility Var.Prose_quotes (oklch_color 19.2 0.025 268.573) |> fst;
        Var.utility Var.Prose_quote_borders (oklch_color 91.9 0.007 264.837)
        |> fst;
        Var.utility Var.Prose_captions (oklch_color 67.8 0.024 254.628) |> fst;
        Var.utility Var.Prose_code (oklch_color 19.2 0.025 268.573) |> fst;
        Var.utility Var.Prose_pre_code (oklch_color 91.9 0.007 264.837) |> fst;
        Var.utility Var.Prose_pre_bg (oklch_color 26.5 0.028 268.761) |> fst;
        Var.utility Var.Prose_th_borders (oklch_color 85.8 0.012 255.094) |> fst;
        Var.utility Var.Prose_td_borders (oklch_color 91.9 0.007 264.837) |> fst;
      ]
  | "zinc" ->
      [
        Var.utility Var.Prose_body (oklch_color 52.5 0.013 265.454) |> fst;
        Var.utility Var.Prose_headings (oklch_color 21.0 0.017 264.665) |> fst;
        Var.utility Var.Prose_lead (oklch_color 42.2 0.015 268.024) |> fst;
        Var.utility Var.Prose_links (oklch_color 21.0 0.017 264.665) |> fst;
        Var.utility Var.Prose_bold (oklch_color 21.0 0.017 264.665) |> fst;
        Var.utility Var.Prose_counters (oklch_color 52.5 0.013 265.454) |> fst;
        Var.utility Var.Prose_bullets (oklch_color 87.1 0.006 286.287) |> fst;
        Var.utility Var.Prose_hr (oklch_color 92.8 0.004 286.032) |> fst;
        Var.utility Var.Prose_quotes (oklch_color 21.0 0.017 264.665) |> fst;
        Var.utility Var.Prose_quote_borders (oklch_color 92.8 0.004 286.032)
        |> fst;
        Var.utility Var.Prose_captions (oklch_color 67.9 0.011 285.934) |> fst;
        Var.utility Var.Prose_code (oklch_color 21.0 0.017 264.665) |> fst;
        Var.utility Var.Prose_pre_code (oklch_color 92.8 0.004 286.032) |> fst;
        Var.utility Var.Prose_pre_bg (oklch_color 27.9 0.017 264.663) |> fst;
        Var.utility Var.Prose_th_borders (oklch_color 87.1 0.006 286.287) |> fst;
        Var.utility Var.Prose_td_borders (oklch_color 92.8 0.004 286.032) |> fst;
      ]
  | "neutral" ->
      [
        Var.utility Var.Prose_body (oklch_color 48.8 0.014 106.429) |> fst;
        Var.utility Var.Prose_headings (oklch_color 23.0 0.021 92.591) |> fst;
        Var.utility Var.Prose_lead (oklch_color 42.2 0.015 110.211) |> fst;
        Var.utility Var.Prose_links (oklch_color 23.0 0.021 92.591) |> fst;
        Var.utility Var.Prose_bold (oklch_color 23.0 0.021 92.591) |> fst;
        Var.utility Var.Prose_counters (oklch_color 48.8 0.014 106.429) |> fst;
        Var.utility Var.Prose_bullets (oklch_color 87.1 0.004 106.424) |> fst;
        Var.utility Var.Prose_hr (oklch_color 92.9 0.003 106.423) |> fst;
        Var.utility Var.Prose_quotes (oklch_color 23.0 0.021 92.591) |> fst;
        Var.utility Var.Prose_quote_borders (oklch_color 92.9 0.003 106.423)
        |> fst;
        Var.utility Var.Prose_captions (oklch_color 67.8 0.009 106.424) |> fst;
        Var.utility Var.Prose_code (oklch_color 23.0 0.021 92.591) |> fst;
        Var.utility Var.Prose_pre_code (oklch_color 92.9 0.003 106.423) |> fst;
        Var.utility Var.Prose_pre_bg (oklch_color 31.1 0.019 93.761) |> fst;
        Var.utility Var.Prose_th_borders (oklch_color 87.1 0.004 106.424) |> fst;
        Var.utility Var.Prose_td_borders (oklch_color 92.9 0.003 106.423) |> fst;
      ]
  | "stone" ->
      [
        Var.utility Var.Prose_body (oklch_color 49.9 0.017 56.205) |> fst;
        Var.utility Var.Prose_headings (oklch_color 27.9 0.019 68.952) |> fst;
        Var.utility Var.Prose_lead (oklch_color 42.3 0.016 56.359) |> fst;
        Var.utility Var.Prose_links (oklch_color 27.9 0.019 68.952) |> fst;
        Var.utility Var.Prose_bold (oklch_color 27.9 0.019 68.952) |> fst;
        Var.utility Var.Prose_counters (oklch_color 49.9 0.017 56.205) |> fst;
        Var.utility Var.Prose_bullets (oklch_color 87.1 0.006 56.211) |> fst;
        Var.utility Var.Prose_hr (oklch_color 92.9 0.003 56.214) |> fst;
        Var.utility Var.Prose_quotes (oklch_color 27.9 0.019 68.952) |> fst;
        Var.utility Var.Prose_quote_borders (oklch_color 92.9 0.003 56.214)
        |> fst;
        Var.utility Var.Prose_captions (oklch_color 67.8 0.011 56.211) |> fst;
        Var.utility Var.Prose_code (oklch_color 27.9 0.019 68.952) |> fst;
        Var.utility Var.Prose_pre_code (oklch_color 92.9 0.003 56.214) |> fst;
        Var.utility Var.Prose_pre_bg (oklch_color 35.1 0.019 68.936) |> fst;
        Var.utility Var.Prose_th_borders (oklch_color 87.1 0.006 56.211) |> fst;
        Var.utility Var.Prose_td_borders (oklch_color 92.9 0.003 56.214) |> fst;
      ]
  | _ -> []

let to_css_rules variant =
  match variant with
  | `Base -> base_prose_rules ()
  | `Sm -> sm_size_rules ".prose-sm"
  | `Lg -> lg_size_rules ".prose-lg"
  | `Xl -> xl_size_rules ".prose-xl"
  | `Xl2 -> xl2_size_rules ".prose-2xl"
  | `Gray -> [ Css.rule ~selector:".prose-gray" (update_color_vars "gray") ]
  | `Slate -> [ Css.rule ~selector:".prose-slate" (update_color_vars "slate") ]
  | `Zinc -> [ Css.rule ~selector:".prose-zinc" (update_color_vars "zinc") ]
  | `Neutral ->
      [ Css.rule ~selector:".prose-neutral" (update_color_vars "neutral") ]
  | `Stone -> [ Css.rule ~selector:".prose-stone" (update_color_vars "stone") ]

(** Get base CSS properties for inline styles *)
let to_base_properties variant =
  (* Extract the base properties for each variant *)
  match variant with
  | `Base ->
      (* Base prose rules are handled entirely by base_prose_rules() *)
      []
  | `Sm -> [ font_size (Rem 0.875); line_height (Num 1.7142857) ]
  | `Lg -> [ font_size (Rem 1.125); line_height (Num 1.7777778) ]
  | `Xl -> [ font_size (Rem 1.25); line_height (Num 1.8) ]
  | `Xl2 -> [ font_size (Rem 1.5); line_height (Num 1.6666667) ]
  | `Gray | `Slate | `Zinc | `Neutral | `Stone ->
      [] (* Color themes only affect CSS variables *)

let pp = function
  | `Base -> "Base"
  | `Sm -> "Sm"
  | `Lg -> "Lg"
  | `Xl -> "Xl"
  | `Xl2 -> "Xl2"
  | `Gray -> "Gray"
  | `Slate -> "Slate"
  | `Zinc -> "Zinc"
  | `Neutral -> "Neutral"
  | `Stone -> "Stone"

(** Prose utility constructors *)
let prose_style variant =
  let name = to_class variant in
  let rules = to_css_rules variant in
  let props = to_base_properties variant in

  (* TW_DEBUG_START *)
  let prose_count =
    List.fold_left
      (fun acc rule -> if Css.selector rule = ".prose" then acc + 1 else acc)
      0 rules
  in
  Printf.printf "TW_DEBUG: prose_style %d/%d\n" prose_count (List.length rules);

  (* TW_DEBUG_END *)
  let result =
    match variant with
    | `Base ->
        (* For Base, everything is in rules - no main rule needed *)
        Core.style ~rules:(Some rules) name []
    | _ ->
        (* For other variants, use props for main rule *)
        Core.style ~rules:(Some rules) name props
  in
  result

let prose = prose_style `Base
let prose_sm = prose_style `Sm
let prose_lg = prose_style `Lg
let prose_xl = prose_style `Xl
let prose_2xl = prose_style `Xl2
let prose_gray = prose_style `Gray
let prose_slate = prose_style `Slate
let prose_zinc = prose_style `Zinc
let prose_neutral = prose_style `Neutral
let prose_stone = prose_style `Stone

(** Parse prose utilities from string *)
let of_string = function
  | [ "prose" ] -> Ok prose
  | [ "prose"; "sm" ] -> Ok prose_sm
  | [ "prose"; "lg" ] -> Ok prose_lg
  | [ "prose"; "xl" ] -> Ok prose_xl
  | [ "prose"; "2xl" ] -> Ok prose_2xl
  | [ "prose"; "gray" ] -> Ok prose_gray
  | [ "prose"; "slate" ] -> Ok prose_slate
  | [ "prose"; "zinc" ] -> Ok (prose_style `Zinc)
  | [ "prose"; "neutral" ] -> Ok (prose_style `Neutral)
  | [ "prose"; "stone" ] -> Ok (prose_style `Stone)
  | _ -> Error (`Msg "Not a prose utility")

(** Generate complete prose stylesheet *)
let stylesheet () =
  (* Generate CSS for all prose variants *)
  let base_rules = to_css_rules `Base in
  let sm_rules = to_css_rules `Sm in
  let lg_rules = to_css_rules `Lg in
  let xl_rules = to_css_rules `Xl in
  let xl2_rules = to_css_rules `Xl2 in
  let gray_rules = to_css_rules `Gray in
  let slate_rules = to_css_rules `Slate in

  (* Combine all rules *)
  let all_rules =
    List.concat
      [
        base_rules;
        sm_rules;
        lg_rules;
        xl_rules;
        xl2_rules;
        gray_rules;
        slate_rules;
      ]
  in
  (* Return raw rules; caller composes into a stylesheet if needed *)
  all_rules

(** Marker utilities (no CSS output) *)
let prose_lead = Core.style ~rules:(Some []) "lead" []

let not_prose = Core.style ~rules:(Some []) "not-prose" []
