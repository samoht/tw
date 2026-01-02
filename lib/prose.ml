(** Prose typography utilities

    Complete implementation of Tailwind Typography plugin with:
    - All HTML elements styled with :where() for low specificity
    - not-prose exclusion mechanism
    - Full CSS variable theming
    - Size and color variants *)

type variant =
  [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate | `Zinc | `Neutral | `Stone ]

open Css

(* Create prose variables using the new API *)
let prose_body_var = Var.channel Color "tw-prose-body"
let prose_headings_var = Var.channel Color "tw-prose-headings"
let prose_lead_var = Var.channel Color "tw-prose-lead"
let prose_links_var = Var.channel Color "tw-prose-links"
let prose_bold_var = Var.channel Color "tw-prose-bold"
let prose_counters_var = Var.channel Color "tw-prose-counters"
let prose_bullets_var = Var.channel Color "tw-prose-bullets"
let prose_hr_var = Var.channel Color "tw-prose-hr"
let prose_quotes_var = Var.channel Color "tw-prose-quotes"
let prose_quote_borders_var = Var.channel Color "tw-prose-quote-borders"
let prose_captions_var = Var.channel Color "tw-prose-captions"
let prose_kbd_var = Var.channel Color "tw-prose-kbd"

(* Special handling for kbd-shadows - it's RGB values without rgb() wrapper *)
let prose_kbd_shadows_var = Var.channel Rgb "tw-prose-kbd-shadows"
let prose_code_var = Var.channel Color "tw-prose-code"
let prose_pre_code_var = Var.channel Color "tw-prose-pre-code"
let prose_pre_bg_var = Var.channel Color "tw-prose-pre-bg"
let prose_th_borders_var = Var.channel Color "tw-prose-th-borders"
let prose_td_borders_var = Var.channel Color "tw-prose-td-borders"

(* Invert variants for dark mode *)
let prose_invert_body_var = Var.channel Color "tw-prose-invert-body"
let prose_invert_headings_var = Var.channel Color "tw-prose-invert-headings"
let prose_invert_lead_var = Var.channel Color "tw-prose-invert-lead"
let prose_invert_links_var = Var.channel Color "tw-prose-invert-links"
let prose_invert_bold_var = Var.channel Color "tw-prose-invert-bold"
let prose_invert_counters_var = Var.channel Color "tw-prose-invert-counters"
let prose_invert_bullets_var = Var.channel Color "tw-prose-invert-bullets"
let prose_invert_hr_var = Var.channel Color "tw-prose-invert-hr"
let prose_invert_quotes_var = Var.channel Color "tw-prose-invert-quotes"

let prose_invert_quote_borders_var =
  Var.channel Color "tw-prose-invert-quote-borders"

let prose_invert_captions_var = Var.channel Color "tw-prose-invert-captions"
let prose_invert_kbd_var = Var.channel Color "tw-prose-invert-kbd"
let prose_invert_kbd_shadows_var = Var.channel Rgb "tw-prose-invert-kbd-shadows"
let prose_invert_code_var = Var.channel Color "tw-prose-invert-code"
let prose_invert_pre_code_var = Var.channel Color "tw-prose-invert-pre-code"
let prose_invert_pre_bg_var = Var.channel Color "tw-prose-invert-pre-bg"
let prose_invert_th_borders_var = Var.channel Color "tw-prose-invert-th-borders"
let prose_invert_td_borders_var = Var.channel Color "tw-prose-invert-td-borders"

(* ======================================================================== *)
(* Selector Helper Functions *)
(* ======================================================================== *)

let ( ++ ) = Css.Selector.( ++ )
let ( >> ) = Css.Selector.( >> )
let ( && ) = Css.Selector.( && )
(* let ( || ) = Css.Selector.( || ) -- Not used anymore, using list instead *)

(* Shorthand for attribute selectors *)
let attr name match_type = Css.Selector.attribute name match_type
let class_attr value = attr "class" (Whitespace_list value)
let type_attr value = attr "type" (Exact value)

(* Helper to create a prose element selector with :where() and :not()
   exclusion *)
let prose_where_element element_selector =
  let open Css.Selector in
  (* Create the not-prose selector: [class~=not-prose] *)
  let not_prose_class = attribute "class" (Whitespace_list "not-prose") in
  (* Create the descendant selector: [class~=not-prose] * *)
  let not_prose_descendant = combine not_prose_class Descendant universal in
  (* Create :where([class~=not-prose],[class~=not-prose] * ) *)
  let not_prose_where = where [ not_prose_class; not_prose_descendant ] in
  (* Create :not(:where([class~=not-prose],[class~=not-prose] * )) *)
  let not_selector = not [ not_prose_where ] in
  (* Create :where(element):not(:where(...)) structure *)
  compound [ where [ element_selector ]; not_selector ]

(* Helper to create a typed selector with :where() and :not() for prose *)
let where base_class elt =
  let open Css.Selector in
  (* For now, just use the new helper *)
  class_ base_class ++ prose_where_element elt

(* Helper for child selectors in prose size variants like prose-sm *)
let prose_child_selector parent_selector child_element =
  (* Create selector like: .prose-sm :where(.prose-sm>child):not(...) *)
  let open Css.Selector in
  let parent_child = combine parent_selector Child child_element in
  let not_prose_class = attribute "class" (Whitespace_list "not-prose") in
  let not_prose_descendant = combine not_prose_class Descendant universal in
  let not_prose_where = where [ not_prose_class; not_prose_descendant ] in
  let not_selector = not [ not_prose_where ] in
  (* Wrap the parent>child in :where() then add :not() *)
  parent_selector ++ compound [ where [ parent_child ]; not_selector ]

let li = Css.Selector.element "li"
let ol = Css.Selector.element "ol"
let ul = Css.Selector.element "ul"
let dd = Css.Selector.element "dd"
let dl = Css.Selector.element "dl"
let hr = Css.Selector.element "hr"
let h1 = Css.Selector.element "h1"
let h2 = Css.Selector.element "h2"
let h3 = Css.Selector.element "h3"
let h4 = Css.Selector.element "h4"
let thead = Css.Selector.element "thead"
let tbody = Css.Selector.element "tbody"
let tfoot = Css.Selector.element "tfoot"
let figure = Css.Selector.element "figure"
let p = Css.Selector.element "p"
let th = Css.Selector.element "th"
let td = Css.Selector.element "td"
let first_child = Css.Selector.First_child
let last_child = Css.Selector.Last_child
let sibling e = Css.Selector.(combine e Next_sibling universal)
let a = Css.Selector.element "a"
let code = Css.Selector.element "code"
let pre = Css.Selector.element "pre"
let blockquote = Css.Selector.element "blockquote"
let tr = Css.Selector.element "tr"
let table = Css.Selector.element "table"
let figcaption = Css.Selector.element "figcaption"
let picture = Css.Selector.element "picture"
let img = Css.Selector.element "img"
let dt = Css.Selector.element "dt"
let strong = Css.Selector.element "strong"
let video = Css.Selector.element "video"
(* kbd selector no longer needed; size rules use direct element selectors *)

(* Lead paragraph selector - now unused since prose_where_element is used
   directly *)

(* Compound selectors for ol with type attributes *)
(* ol type selectors were only used in removed duplicate rules *)

(* Pseudo-element helpers that apply pseudo AFTER the where clause *)
let with_before selector =
  Css.Selector.compound [ selector; Css.Selector.Before ]

let with_after selector = Css.Selector.compound [ selector; Css.Selector.After ]

(* blockquote paragraph pseudo-element selectors were only used in removed
   duplicates *)
let blockquote_code = blockquote ++ code
let thead_th_code = thead ++ th ++ code
let pre_code = pre ++ code
let thead_th = thead ++ th
let tbody_tr = tbody ++ tr
let tbody_tr_last_child = tbody ++ (tr && last_child)

(* tbody_td and tfoot_td now constructed inline *)
(* picture_img now constructed inline *)
(* ol_li and ul_li now constructed inline *)
(* h2_code and h3_code now constructed inline in typography_rules *)
let nested_lists = Css.Selector.list [ ul ++ ul; ul ++ ol; ol ++ ul; ol ++ ol ]
(* These compound selectors are now constructed inline in typography_rules *)

(* Theme record for all prose color variables *)
type prose_theme = {
  body : Css.declaration * Css.color Css.var;
  headings : Css.declaration * Css.color Css.var;
  lead : Css.declaration * Css.color Css.var;
  links : Css.declaration * Css.color Css.var;
  bold : Css.declaration * Css.color Css.var;
  counters : Css.declaration * Css.color Css.var;
  bullets : Css.declaration * Css.color Css.var;
  hr : Css.declaration * Css.color Css.var;
  quotes : Css.declaration * Css.color Css.var;
  quote_borders : Css.declaration * Css.color Css.var;
  captions : Css.declaration * Css.color Css.var;
  kbd : Css.declaration * Css.color Css.var;
  kbd_shadows : Css.declaration * Css.rgb Css.var;
  code : Css.declaration * Css.color Css.var;
  pre_code : Css.declaration * Css.color Css.var;
  pre_bg : Css.declaration * Css.color Css.var;
  th_borders : Css.declaration * Css.color Css.var;
  td_borders : Css.declaration * Css.color Css.var;
  (* Invert variants *)
  invert_body : Css.declaration * Css.color Css.var;
  invert_headings : Css.declaration * Css.color Css.var;
  invert_lead : Css.declaration * Css.color Css.var;
  invert_links : Css.declaration * Css.color Css.var;
  invert_bold : Css.declaration * Css.color Css.var;
  invert_counters : Css.declaration * Css.color Css.var;
  invert_bullets : Css.declaration * Css.color Css.var;
  invert_hr : Css.declaration * Css.color Css.var;
  invert_quotes : Css.declaration * Css.color Css.var;
  invert_quote_borders : Css.declaration * Css.color Css.var;
  invert_captions : Css.declaration * Css.color Css.var;
  invert_kbd : Css.declaration * Css.color Css.var;
  invert_kbd_shadows : Css.declaration * Css.rgb Css.var;
  invert_code : Css.declaration * Css.color Css.var;
  invert_pre_code : Css.declaration * Css.color Css.var;
  invert_pre_bg : Css.declaration * Css.color Css.var;
  invert_th_borders : Css.declaration * Css.color Css.var;
  invert_td_borders : Css.declaration * Css.color Css.var;
}

(* Default prose theme - created once and shared across all utilities *)
let default_prose_theme : prose_theme =
  let body_d, body_v =
    Var.binding prose_body_var (Css.oklch 37.3 0.034 259.733)
  in
  let headings_d, headings_v =
    Var.binding prose_headings_var (Css.oklch 21.0 0.034 264.665)
  in
  let lead_d, lead_v =
    Var.binding prose_lead_var (Css.oklch 44.6 0.030 256.802)
  in
  let links_d, links_v =
    Var.binding prose_links_var (Css.oklch 21.0 0.034 264.665)
  in
  let bold_d, bold_v =
    Var.binding prose_bold_var (Css.oklch 21.0 0.034 264.665)
  in
  let counters_d, counters_v =
    Var.binding prose_counters_var (Css.oklch 55.1 0.027 264.364)
  in
  let bullets_d, bullets_v =
    Var.binding prose_bullets_var (Css.oklch 87.2 0.010 258.338)
  in
  let hr_d, hr_v = Var.binding prose_hr_var (Css.oklch 92.8 0.006 264.531) in
  let quotes_d, quotes_v =
    Var.binding prose_quotes_var (Css.oklch 21.0 0.034 264.665)
  in
  let quote_borders_d, quote_borders_v =
    Var.binding prose_quote_borders_var (Css.oklch 92.8 0.006 264.531)
  in
  let captions_d, captions_v =
    Var.binding prose_captions_var (Css.oklch 55.1 0.027 264.364)
  in
  let kbd_d, kbd_v = Var.binding prose_kbd_var (Css.oklch 21.0 0.034 264.665) in
  let kbd_shadows_d, kbd_shadows_v =
    (* Use NaN float values to match Tailwind's buggy behavior *)
    Var.binding prose_kbd_shadows_var
      (Css.Channels { r = Css.Num nan; g = Css.Num nan; b = Css.Num nan })
  in
  let code_d, code_v =
    Var.binding prose_code_var (Css.oklch 21.0 0.034 264.665)
  in
  let pre_code_d, pre_code_v =
    Var.binding prose_pre_code_var (Css.oklch 92.8 0.006 264.531)
  in
  let pre_bg_d, pre_bg_v =
    Var.binding prose_pre_bg_var (Css.oklch 27.8 0.033 256.848)
  in
  let th_borders_d, th_borders_v =
    Var.binding prose_th_borders_var (Css.oklch 87.2 0.010 258.338)
  in
  let td_borders_d, td_borders_v =
    Var.binding prose_td_borders_var (Css.oklch 92.8 0.006 264.531)
  in
  (* Invert variants *)
  let invert_body_d, invert_body_v =
    Var.binding prose_invert_body_var (Css.oklch 87.2 0.010 258.338)
  in
  let invert_headings_d, invert_headings_v =
    Var.binding prose_invert_headings_var (Css.hex "fff")
  in
  let invert_lead_d, invert_lead_v =
    Var.binding prose_invert_lead_var (Css.oklch 70.7 0.022 261.325)
  in
  let invert_links_d, invert_links_v =
    Var.binding prose_invert_links_var (Css.hex "fff")
  in
  let invert_bold_d, invert_bold_v =
    Var.binding prose_invert_bold_var (Css.hex "fff")
  in
  let invert_counters_d, invert_counters_v =
    Var.binding prose_invert_counters_var (Css.oklch 70.7 0.022 261.325)
  in
  let invert_bullets_d, invert_bullets_v =
    Var.binding prose_invert_bullets_var (Css.oklch 44.6 0.030 256.802)
  in
  let invert_hr_d, invert_hr_v =
    Var.binding prose_invert_hr_var (Css.oklch 37.3 0.034 259.733)
  in
  let invert_quotes_d, invert_quotes_v =
    Var.binding prose_invert_quotes_var (Css.oklch 96.7 0.003 264.542)
  in
  let invert_quote_borders_d, invert_quote_borders_v =
    Var.binding prose_invert_quote_borders_var (Css.oklch 37.3 0.034 259.733)
  in
  let invert_captions_d, invert_captions_v =
    Var.binding prose_invert_captions_var (Css.oklch 70.7 0.022 261.325)
  in
  let invert_kbd_d, invert_kbd_v =
    Var.binding prose_invert_kbd_var (Css.hex "fff")
  in
  let invert_kbd_shadows_d, invert_kbd_shadows_v =
    Var.binding prose_invert_kbd_shadows_var
      (Css.Channels { r = Css.Int 255; g = Css.Int 255; b = Css.Int 255 })
  in
  let invert_code_d, invert_code_v =
    Var.binding prose_invert_code_var (Css.hex "fff")
  in
  let invert_pre_code_d, invert_pre_code_v =
    Var.binding prose_invert_pre_code_var (Css.oklch 87.2 0.010 258.338)
  in
  let invert_pre_bg_d, invert_pre_bg_v =
    Var.binding prose_invert_pre_bg_var (Css.hex "00000080")
  in
  let invert_th_borders_d, invert_th_borders_v =
    Var.binding prose_invert_th_borders_var (Css.oklch 44.6 0.030 256.802)
  in
  let invert_td_borders_d, invert_td_borders_v =
    Var.binding prose_invert_td_borders_var (Css.oklch 37.3 0.034 259.733)
  in
  {
    body = (body_d, body_v);
    headings = (headings_d, headings_v);
    lead = (lead_d, lead_v);
    links = (links_d, links_v);
    bold = (bold_d, bold_v);
    counters = (counters_d, counters_v);
    bullets = (bullets_d, bullets_v);
    hr = (hr_d, hr_v);
    quotes = (quotes_d, quotes_v);
    quote_borders = (quote_borders_d, quote_borders_v);
    captions = (captions_d, captions_v);
    kbd = (kbd_d, kbd_v);
    kbd_shadows = (kbd_shadows_d, kbd_shadows_v);
    code = (code_d, code_v);
    pre_code = (pre_code_d, pre_code_v);
    pre_bg = (pre_bg_d, pre_bg_v);
    th_borders = (th_borders_d, th_borders_v);
    td_borders = (td_borders_d, td_borders_v);
    (* Invert variants *)
    invert_body = (invert_body_d, invert_body_v);
    invert_headings = (invert_headings_d, invert_headings_v);
    invert_lead = (invert_lead_d, invert_lead_v);
    invert_links = (invert_links_d, invert_links_v);
    invert_bold = (invert_bold_d, invert_bold_v);
    invert_counters = (invert_counters_d, invert_counters_v);
    invert_bullets = (invert_bullets_d, invert_bullets_v);
    invert_hr = (invert_hr_d, invert_hr_v);
    invert_quotes = (invert_quotes_d, invert_quotes_v);
    invert_quote_borders = (invert_quote_borders_d, invert_quote_borders_v);
    invert_captions = (invert_captions_d, invert_captions_v);
    invert_kbd = (invert_kbd_d, invert_kbd_v);
    invert_kbd_shadows = (invert_kbd_shadows_d, invert_kbd_shadows_v);
    invert_code = (invert_code_d, invert_code_v);
    invert_pre_code = (invert_pre_code_d, invert_pre_code_v);
    invert_pre_bg = (invert_pre_bg_d, invert_pre_bg_v);
    invert_th_borders = (invert_th_borders_d, invert_th_borders_v);
    invert_td_borders = (invert_td_borders_d, invert_td_borders_v);
  }

(** Access theme record variables *)
let theme = default_prose_theme

(* Extract individual variables from theme record for backward compatibility *)
let prose_body_d, prose_body_v = theme.body
let prose_headings_d, prose_headings_v = theme.headings
let prose_lead_d, prose_lead_v = theme.lead
let prose_links_d, prose_links_v = theme.links
let prose_bold_d, prose_bold_v = theme.bold
let prose_counters_d, prose_counters_v = theme.counters
let prose_bullets_d, prose_bullets_v = theme.bullets
let prose_hr_d, prose_hr_v = theme.hr
let prose_quotes_d, prose_quotes_v = theme.quotes
let prose_quote_borders_d, prose_quote_borders_v = theme.quote_borders
let prose_captions_d, prose_captions_v = theme.captions
let prose_kbd_d, prose_kbd_v = theme.kbd
let prose_kbd_shadows_d, prose_kbd_shadows_v = theme.kbd_shadows
let prose_code_d, prose_code_v = theme.code
let prose_pre_code_d, prose_pre_code_v = theme.pre_code
let prose_pre_bg_d, prose_pre_bg_v = theme.pre_bg
let prose_th_borders_d, prose_th_borders_v = theme.th_borders
let prose_td_borders_d, prose_td_borders_v = theme.td_borders

(* Local helpers for prose utilities *)
let rgba_var rgb_var alpha = Css.Rgba { rgb = Css.Var rgb_var; a = alpha }

(* Invert variants *)
let prose_invert_body_d, _prose_invert_body_v = theme.invert_body
let prose_invert_headings_d, _prose_invert_headings_v = theme.invert_headings
let prose_invert_lead_d, _prose_invert_lead_v = theme.invert_lead
let prose_invert_links_d, _prose_invert_links_v = theme.invert_links
let prose_invert_bold_d, _prose_invert_bold_v = theme.invert_bold
let prose_invert_counters_d, _prose_invert_counters_v = theme.invert_counters
let prose_invert_bullets_d, _prose_invert_bullets_v = theme.invert_bullets
let prose_invert_hr_d, _prose_invert_hr_v = theme.invert_hr
let prose_invert_quotes_d, _prose_invert_quotes_v = theme.invert_quotes

let prose_invert_quote_borders_d, _prose_invert_quote_borders_v =
  theme.invert_quote_borders

let prose_invert_captions_d, _prose_invert_captions_v = theme.invert_captions
let prose_invert_kbd_d, _prose_invert_kbd_v = theme.invert_kbd

let prose_invert_kbd_shadows_d, _prose_invert_kbd_shadows_v =
  theme.invert_kbd_shadows

let prose_invert_code_d, _prose_invert_code_v = theme.invert_code
let prose_invert_pre_code_d, _prose_invert_pre_code_v = theme.invert_pre_code
let prose_invert_pre_bg_d, _prose_invert_pre_bg_v = theme.invert_pre_bg

let prose_invert_th_borders_d, _prose_invert_th_borders_v =
  theme.invert_th_borders

let prose_invert_td_borders_d, _prose_invert_td_borders_v =
  theme.invert_td_borders

(** Collect all CSS variable declarations *)
let css_variables =
  [
    prose_body_d;
    prose_headings_d;
    prose_lead_d;
    prose_links_d;
    prose_bold_d;
    prose_counters_d;
    prose_bullets_d;
    prose_hr_d;
    prose_quotes_d;
    prose_quote_borders_d;
    prose_captions_d;
    prose_kbd_d;
    prose_kbd_shadows_d;
    prose_code_d;
    prose_pre_code_d;
    prose_pre_bg_d;
    prose_th_borders_d;
    prose_td_borders_d;
    prose_invert_body_d;
    prose_invert_headings_d;
    prose_invert_lead_d;
    prose_invert_links_d;
    prose_invert_bold_d;
    prose_invert_counters_d;
    prose_invert_bullets_d;
    prose_invert_hr_d;
    prose_invert_quotes_d;
    prose_invert_quote_borders_d;
    prose_invert_captions_d;
    prose_invert_kbd_d;
    prose_invert_kbd_shadows_d;
    prose_invert_code_d;
    prose_invert_pre_code_d;
    prose_invert_pre_bg_d;
    prose_invert_th_borders_d;
    prose_invert_td_borders_d;
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

(* Paragraph and lead styles are defined inline in base_prose_rules *)

(* Link styles are defined inline in base_prose_rules *)

(* Strong styles are defined inline in base_prose_rules *)

(* List styles are defined inline in base_prose_rules *)

(* Heading styles *)
let heading_rules base =
  [
    Css.rule ~selector:(where base h1)
      [
        Css.color (Css.Var prose_headings_v);
        Css.margin_top Zero;
        Css.margin_bottom (Em 0.888889);
        Css.font_size (Em 2.25);
        Css.font_weight (Weight 800);
        Css.line_height (Num 1.11111);
      ];
    Css.rule
      ~selector:(where base (h1 ++ strong))
      [ Css.color Css.Inherit; Css.font_weight (Css.Weight 900) ];
    Css.rule ~selector:(where base h2)
      [
        Css.color (Css.Var prose_headings_v);
        Css.margin_top (Em 2.0);
        Css.margin_bottom (Em 1.0);
        Css.font_size (Em 1.5);
        Css.font_weight (Weight 700);
        Css.line_height (Num 1.33333);
      ];
    Css.rule
      ~selector:(where base (h2 ++ strong))
      [ Css.color Css.Inherit; Css.font_weight (Css.Weight 800) ];
    Css.rule ~selector:(where base h3)
      [
        Css.color (Css.Var prose_headings_v);
        Css.margin_top (Em 1.6);
        Css.margin_bottom (Em 0.6);
        Css.font_size (Em 1.25);
        Css.font_weight (Weight 600);
        Css.line_height (Num 1.6);
      ];
    Css.rule
      ~selector:(where base (h3 ++ strong))
      [ Css.color Inherit; Css.font_weight (Weight 700) ];
    Css.rule ~selector:(where base h4)
      [
        Css.color (Css.Var prose_headings_v);
        Css.margin_top (Em 1.5);
        Css.margin_bottom (Em 0.5);
        Css.font_weight (Weight 600);
        Css.line_height (Num 1.5);
      ];
    Css.rule
      ~selector:(where base (h4 ++ strong))
      [ Css.color Inherit; Css.font_weight (Weight 700) ];
  ]

(* Horizontal rule styles are defined inline in base_prose_rules *)

(* Blockquote styles are defined inline in base_prose_rules *)

(* Media element styles *)
let media_rules base =
  [
    Css.rule ~selector:(where base img)
      [ Css.margin_top (Em 2.0); Css.margin_bottom (Em 2.0) ];
    Css.rule ~selector:(where base picture)
      [ Css.margin_top (Em 2.0); Css.margin_bottom (Em 2.0); Css.display Block ];
    Css.rule ~selector:(where base video)
      [ Css.margin_top (Em 2.0); Css.margin_bottom (Em 2.0) ];
  ]

(* Keyboard styles *)
let kbd_rules base =
  [
    Css.rule
      ~selector:(where base (Css.Selector.element "kbd"))
      [
        Css.color (Css.Var prose_kbd_v);
        Css.box_shadows
          [
            Css.Shadow
              {
                inset = false;
                inset_var = None;
                h_offset = Zero;
                v_offset = Zero;
                blur = Some Zero;
                spread = Some (Px 1.0);
                color = Some (rgba_var prose_kbd_shadows_v (Css.Pct 10.));
              };
            Css.Shadow
              {
                inset = false;
                inset_var = None;
                h_offset = Zero;
                v_offset = Px 3.0;
                blur = Some Zero;
                spread = None;
                color = Some (rgba_var prose_kbd_shadows_v (Css.Pct 10.));
              };
          ];
        Css.padding_top (Em 0.1875);
        Css.padding_inline_end (Em 0.375);
        Css.padding_bottom (Em 0.1875);
        border_radius (Rem 0.3125);
        Css.padding_inline_start (Em 0.375);
        font_family Inherit;
        Css.font_size (Em 0.875);
        Css.font_weight (Weight 500);
      ];
  ]

(* Code styles *)
let code_rules base =
  [
    Css.rule ~selector:(where base code)
      [
        Css.color (Css.Var prose_code_v);
        Css.font_size (Em 0.875);
        Css.font_weight (Weight 600);
      ];
    (* Code pseudo-element content - group ::before and ::after in one rule *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [ with_before (where base code); with_after (where base code) ])
      [ content (String "`") ];
    (* Code inherits color in certain contexts *)
    Css.rule
      ~selector:
        (Css.Selector.list [ where base (a ++ code); where base (h1 ++ code) ])
      [ Css.color Inherit ];
    Css.rule
      ~selector:(where base (h2 ++ code))
      [ Css.color Inherit; Css.font_size (Em 0.875) ];
    Css.rule
      ~selector:(where base (h3 ++ code))
      [ Css.color Inherit; Css.font_size (Em 0.9) ];
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             where base (h4 ++ code);
             where base blockquote_code;
             where base thead_th_code;
           ])
      [ Css.color Inherit ];
    (* Pre code block styles *)
    Css.rule ~selector:(where base pre)
      [
        Css.color (Css.Var prose_pre_code_v);
        Css.background_color (Css.Var prose_pre_bg_v);
        Css.padding_top (Em 0.857143);
        Css.padding_inline_end (Em 1.14286);
        Css.padding_bottom (Em 0.857143);
        border_radius (Rem 0.375);
        Css.margin_top (Em 1.71429);
        Css.margin_bottom (Em 1.71429);
        Css.padding_inline_start (Em 1.14286);
        Css.font_size (Em 0.875);
        Css.font_weight (Weight 400);
        Css.line_height (Num 1.71429);
        overflow_x Auto;
      ];
    Css.rule ~selector:(where base pre_code)
      [
        Css.font_weight Inherit;
        Css.color Css.Inherit;
        Css.font_size Inherit;
        font_family Inherit;
        Css.line_height Inherit;
        Css.background_color (hex "#0000");
        border_width Zero;
        border_radius Zero;
        padding [ Zero ];
      ];
    (* Pre code pseudo-element content removal - group ::before and ::after *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             with_before (where base pre_code); with_after (where base pre_code);
           ])
      [ content None ];
  ]

(* Table styles *)
let table_rules base =
  [
    Css.rule ~selector:(where base table)
      [
        table_layout Auto;
        width (Pct 100.0);
        Css.margin_top (Em 2.0);
        Css.margin_bottom (Em 2.0);
        Css.font_size (Em 0.875);
        Css.line_height (Num 1.71429);
      ];
    Css.rule
      ~selector:(where base (Css.Selector.element "thead"))
      [
        Css.border_bottom_width (Px 1.);
        Css.border_bottom_color (Css.Var prose_th_borders_v);
      ];
    Css.rule ~selector:(where base thead_th)
      [
        Css.color (Css.Var prose_headings_v);
        Css.vertical_align Bottom;
        Css.padding_inline_end (Em 0.571429);
        Css.padding_bottom (Em 0.571429);
        Css.padding_inline_start (Em 0.571429);
        Css.font_weight (Weight 600);
      ];
    Css.rule ~selector:(where base tbody_tr)
      [
        Css.border_bottom_width (Px 1.);
        Css.border_bottom_color (Css.Var prose_td_borders_v);
      ];
    Css.rule
      ~selector:(where base tbody_tr_last_child)
      [ border_bottom_width Zero ];
    Css.rule ~selector:(where base (tbody ++ td)) [ vertical_align Baseline ];
    Css.rule ~selector:(where base tfoot)
      [
        border_top_width (Px 1.); border_top_color (Css.Var prose_th_borders_v);
      ];
    Css.rule ~selector:(where base (tfoot ++ td)) [ vertical_align Top ];
    Css.rule
      ~selector:(where base (Css.Selector.list [ th; td ]))
      [ text_align Start ];
  ]

(* Figure and figcaption styles *)
let figure_rules base =
  [
    Css.rule
      ~selector:(where base Css.Selector.(combine figure Child universal))
      [ Css.margin_top Zero; Css.margin_bottom Zero ];
    Css.rule ~selector:(where base figcaption)
      [
        Css.color (Css.Var prose_captions_v);
        Css.margin_top (Em 0.857143);
        Css.font_size (Em 0.875);
        Css.line_height (Num 1.42857);
      ];
  ]

(* Additional list and spacing rules *)
let additional_rules base =
  [
    (* Picture img special handling *)
    Css.rule
      ~selector:(where base (picture >> img))
      [ Css.margin_top Zero; Css.margin_bottom Zero ];
    (* List item styles *)
    Css.rule ~selector:(where base li)
      [ Css.margin_top (Em 0.5); Css.margin_bottom (Em 0.5) ];
    Css.rule
      ~selector:
        (Css.Selector.list [ where base (ol >> li); where base (ul >> li) ])
      [ Css.padding_inline_start (Em 0.375) ];
    (* Nested list paragraph spacing *)
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ul >> li ++ p)))
      [ Css.margin_top (Em 0.75); Css.margin_bottom (Em 0.75) ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ul >> (li >> (p && first_child)))))
      [ Css.margin_top (Em 1.25) ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ul >> (li >> (p && last_child)))))
      [ Css.margin_bottom (Em 1.25) ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ol >> (li >> (p && first_child)))))
      [ Css.margin_top (Em 1.25) ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ol >> (li >> (p && last_child)))))
      [ Css.margin_bottom (Em 1.25) ];
    (* Nested lists *)
    Css.rule ~selector:(where base nested_lists)
      [ Css.margin_top (Em 0.75); Css.margin_bottom (Em 0.75) ];
    (* Definition lists *)
    Css.rule ~selector:(where base dl)
      [ Css.margin_top (Em 1.25); Css.margin_bottom (Em 1.25) ];
    Css.rule ~selector:(where base dd)
      [ Css.margin_top (Em 0.5); padding_inline_start (Em 1.625) ];
    (* Adjacent element spacing *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             where base (sibling hr);
             where base (sibling h2);
             where base (sibling h3);
             where base (sibling h4);
           ])
      [ Css.margin_top Zero ];
    (* Table column padding *)
    Css.rule
      ~selector:(where base (thead ++ (th && first_child)))
      [ Css.padding_inline_start Zero ];
    Css.rule
      ~selector:(where base (thead ++ (th && last_child)))
      [ Css.padding_inline_end Zero ];
    Css.rule
      ~selector:(where base (Css.Selector.list [ tbody ++ td; tfoot ++ td ]))
      [
        Css.padding_top (Em 0.571429);
        Css.padding_inline_end (Em 0.571429);
        Css.padding_bottom (Em 0.571429);
        Css.padding_inline_start (Em 0.571429);
      ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.list
              [ tbody ++ (td && first_child); tfoot ++ (td && first_child) ]))
      [ Css.padding_inline_start Zero ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.list
              [ tbody ++ (td && last_child); tfoot ++ (td && last_child) ]))
      [ Css.padding_inline_end Zero ];
    (* Figure *)
    Css.rule ~selector:(where base figure)
      [ Css.margin_top (Em 2.0); Css.margin_bottom (Em 2.0) ];
    (* First and last child margins *)
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Css.Selector.Child first_child))
      [ Css.margin_top Zero ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Css.Selector.Child last_child))
      [ Css.margin_bottom Zero ];
  ]

(* Base prose rules - combines all element rules *)
let base_prose_rules () =
  (* Simplified prose rules to match Tailwind exactly *)
  let base = "prose" in

  (* Main prose rule with basic styles only *)
  let main_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "prose")
      [ Css.color (Css.Var prose_body_v); Css.max_width (Ch 65.0) ]
  in

  (* Create a separate rule for CSS variables - like Tailwind does *)
  let variables_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "prose")
      (css_variables @ [ Css.font_size (Rem 1.0); Css.line_height (Num 1.75) ])
  in

  (* Split into organized utility rule functions *)
  let paragraph_and_text_rules =
    [
      (* Paragraphs *)
      Css.rule ~selector:(where base p)
        [ Css.margin_top (Em 1.25); Css.margin_bottom (Em 1.25) ];
      (* Lead text *)
      Css.rule
        ~selector:(where base (class_attr "lead"))
        [
          color (Css.Var prose_lead_v);
          margin_top (Em 1.2);
          margin_bottom (Em 1.2);
          font_size (Em 1.25);
          line_height (Num 1.6);
        ];
    ]
  in

  let link_and_strong_rules =
    [
      (* Links *)
      Css.rule ~selector:(where base a)
        [
          color (Css.Var prose_links_v);
          Css.font_weight (Weight 500);
          text_decoration
            (Shorthand
               {
                 lines = [ Underline ];
                 style = None;
                 color = None;
                 thickness = None;
               });
        ];
      (* Strong *)
      Css.rule ~selector:(where base strong)
        [ Css.color (Css.Var prose_bold_v); Css.font_weight (Weight 600) ];
      (* Strong inherit rules - group multiple selectors *)
      Css.rule
        ~selector:
          (Css.Selector.list
             [
               where base
                 (Css.Selector.combine a Css.Selector.Descendant strong);
               where base
                 (Css.Selector.combine blockquote Css.Selector.Descendant strong);
               where base
                 (Css.Selector.combine
                    (Css.Selector.combine thead Css.Selector.Descendant th)
                    Css.Selector.Descendant strong);
             ])
        [ Css.color Inherit ];
    ]
  in

  let list_and_marker_rules =
    [
      (* Lists *)
      Css.rule ~selector:(where base ol)
        [
          margin_top (Em 1.25);
          margin_bottom (Em 1.25);
          padding_inline_start (Em 1.625);
          list_style_type Decimal;
        ];
      (* List type variants - compound selector without space *)
      Css.rule
        ~selector:(where base (Css.Selector.compound [ ol; type_attr "A" ]))
        [ list_style_type Upper_alpha ];
      Css.rule
        ~selector:(where base (Css.Selector.compound [ ol; type_attr "a" ]))
        [ list_style_type Lower_alpha ];
      Css.rule
        ~selector:
          (where base
             (Css.Selector.compound
                [
                  ol;
                  Css.Selector.attribute ~flag:Case_sensitive "type" (Exact "A");
                ]))
        [ list_style_type Upper_alpha ];
      Css.rule
        ~selector:
          (where base
             (Css.Selector.compound
                [
                  ol;
                  Css.Selector.attribute ~flag:Case_sensitive "type" (Exact "a");
                ]))
        [ list_style_type Lower_alpha ];
      Css.rule
        ~selector:(where base (Css.Selector.compound [ ol; type_attr "I" ]))
        [ list_style_type Upper_roman ];
      Css.rule
        ~selector:(where base (Css.Selector.compound [ ol; type_attr "i" ]))
        [ list_style_type Lower_roman ];
      Css.rule
        ~selector:
          (where base
             (Css.Selector.compound
                [
                  ol;
                  Css.Selector.attribute ~flag:Case_sensitive "type" (Exact "I");
                ]))
        [ list_style_type Upper_roman ];
      Css.rule
        ~selector:
          (where base
             (Css.Selector.compound
                [
                  ol;
                  Css.Selector.attribute ~flag:Case_sensitive "type" (Exact "i");
                ]))
        [ list_style_type Lower_roman ];
      Css.rule
        ~selector:(where base (Css.Selector.compound [ ol; type_attr "1" ]))
        [ list_style_type Decimal ];
      Css.rule ~selector:(where base ul)
        [
          margin_top (Em 1.25);
          margin_bottom (Em 1.25);
          padding_inline_start (Em 1.625);
          list_style_type Disc;
        ];
      (* Markers with correct syntax to match Tailwind *)
      Css.rule
        ~selector:
          (Css.Selector.compound
             [
               where base (Css.Selector.combine ol Css.Selector.Child li);
               Css.Selector.Marker;
             ])
        [ Css.color (Css.Var prose_counters_v); Css.font_weight (Weight 400) ];
      Css.rule
        ~selector:
          (Css.Selector.compound
             [
               where base (Css.Selector.combine ul Css.Selector.Child li);
               Css.Selector.Marker;
             ])
        [ Css.color (Css.Var prose_bullets_v) ];
    ]
  in

  let structural_element_rules =
    [
      (* Definition terms *)
      Css.rule ~selector:(where base dt)
        [
          color (Css.Var prose_headings_v);
          margin_top (Em 1.25);
          Css.font_weight (Weight 600);
        ];
      (* Horizontal rules *)
      Css.rule ~selector:(where base hr)
        [
          border_color (Css.Var prose_hr_v);
          border_top_width (Px 1.0);
          margin_top (Em 3.0);
          margin_bottom (Em 3.0);
        ];
      (* Blockquotes *)
      Css.rule ~selector:(where base blockquote)
        [
          color (Css.Var prose_quotes_v);
          border_inline_start_width (Rem 0.25);
          border_inline_start_color (Css.Var prose_quote_borders_v);
          quotes
            (Pairs
               [
                 (* " and " - left/right double quotes *)
                 ("\xe2\x80\x9c", "\xe2\x80\x9d");
                 (* ' and ' - left/right single quotes *)
                 ("\xe2\x80\x98", "\xe2\x80\x99");
               ]);
          margin_top (Em 1.6);
          margin_bottom (Em 1.6);
          padding_inline_start (Em 1.0);
          font_style Italic;
          Css.font_weight (Weight 500);
        ];
      (* Blockquote pseudo-elements - apply pseudo AFTER the where clause *)
      Css.rule
        ~selector:
          (with_before
             (where base
                (Css.Selector.compound
                   [
                     Css.Selector.combine blockquote Css.Selector.Descendant p;
                     Css.Selector.First_of_type;
                   ])))
        [ content Open_quote ];
      Css.rule
        ~selector:
          (with_after
             (where base
                (Css.Selector.compound
                   [
                     Css.Selector.combine blockquote Css.Selector.Descendant p;
                     Css.Selector.Last_of_type;
                   ])))
        [ content Close_quote ];
    ]
  in

  (* All prose rules to match Tailwind exactly *)
  (* Put main_rule first, then element rules, then variables_rule at end like Tailwind *)
  [ main_rule ] @ paragraph_and_text_rules @ link_and_strong_rules
  @ list_and_marker_rules @ structural_element_rules @ heading_rules base
  @ media_rules base @ kbd_rules base @ code_rules base @ table_rules base
  @ figure_rules base
  @ [ variables_rule ] (* Variables rule at the end like Tailwind *)
  @ additional_rules base

(* Configuration type for typography sizes *)
type size_config = {
  base_font_size : Css.length;
  base_line_height : float;
  p_margin_y : Css.length;
  lead_margin_top : Css.length;
  lead_margin_bottom : Css.length;
  lead_font_size : Css.length;
  lead_line_height : float;
  blockquote_margin_y : Css.length;
  blockquote_padding_start : Css.length;
  h1_margin_top : Css.length;
  h1_margin_bottom : Css.length;
  h1_font_size : Css.length;
  h1_line_height : float;
  h2_margin_top : Css.length;
  h2_margin_bottom : Css.length;
  h2_font_size : Css.length;
  h2_line_height : float;
  h3_margin_top : Css.length;
  h3_margin_bottom : Css.length;
  h3_font_size : Css.length;
  h3_line_height : float;
  h4_margin_top : Css.length;
  h4_margin_bottom : Css.length;
  h4_line_height : float;
  img_margin_y : Css.length;
  kbd_padding_y : Css.length;
  kbd_padding_x : Css.length;
  kbd_font_size : Css.length;
  kbd_border_radius : Css.length;
  code_font_size : Css.length;
  h2_code_font_size : Css.length;
  h3_code_font_size : Css.length;
  pre_padding_y : Css.length;
  pre_padding_x : Css.length;
  pre_border_radius : Css.length;
  pre_margin_top : Css.length;
  pre_margin_bottom : Css.length;
  pre_font_size : Css.length;
  pre_line_height : float;
  list_margin_y : Css.length;
  list_padding_start : Css.length;
  li_margin_y : Css.length;
  li_padding_start : Css.length;
  li_p_margin_y : Css.length;
  li_p_first_margin_top : Css.length;
  li_p_last_margin_bottom : Css.length;
  nested_list_margin_y : Css.length;
  dl_margin_y : Css.length;
  dt_margin_top : Css.length;
  dd_margin_top : Css.length;
  dd_padding_start : Css.length;
  hr_margin_y : Css.length;
  table_font_size : Css.length;
  table_line_height : float;
  thead_th_padding_x : Css.length;
  thead_th_padding_bottom : Css.length;
  tbody_td_padding_y : Css.length;
  tbody_td_padding_x : Css.length;
  figure_margin_y : Css.length;
  figcaption_margin_top : Css.length;
  figcaption_font_size : Css.length;
  figcaption_line_height : float;
}

(* Helper function to generate typography rules for different sizes *)
let typography_rules selector c =
  (* Combinator to avoid repeating pattern *)
  let open Selector in
  [
    (* Base rule *)
    Css.rule ~selector
      [
        Css.font_size c.base_font_size; Css.line_height (Num c.base_line_height);
      ];
    (* Paragraph *)
    Css.rule
      ~selector:(selector ++ prose_where_element p)
      [ Css.margin_top c.p_margin_y; Css.margin_bottom c.p_margin_y ];
    (* Lead paragraph *)
    Css.rule
      ~selector:(selector ++ prose_where_element (class_attr "lead"))
      [
        Css.margin_top c.lead_margin_top;
        Css.margin_bottom c.lead_margin_bottom;
        Css.font_size c.lead_font_size;
        Css.line_height (Num c.lead_line_height);
      ];
    (* Blockquote *)
    Css.rule
      ~selector:(selector ++ prose_where_element blockquote)
      [
        Css.margin_top c.blockquote_margin_y;
        Css.margin_bottom c.blockquote_margin_y;
        Css.padding_inline_start c.blockquote_padding_start;
      ];
    (* Headings *)
    Css.rule
      ~selector:(selector ++ prose_where_element h1)
      [
        Css.margin_top c.h1_margin_top;
        Css.margin_bottom c.h1_margin_bottom;
        Css.font_size c.h1_font_size;
        Css.line_height (Num c.h1_line_height);
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element h2)
      [
        Css.margin_top c.h2_margin_top;
        Css.margin_bottom c.h2_margin_bottom;
        Css.font_size c.h2_font_size;
        Css.line_height (Num c.h2_line_height);
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element h3)
      [
        Css.margin_top c.h3_margin_top;
        Css.margin_bottom c.h3_margin_bottom;
        Css.font_size c.h3_font_size;
        Css.line_height (Num c.h3_line_height);
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element h4)
      [
        Css.margin_top c.h4_margin_top;
        Css.margin_bottom c.h4_margin_bottom;
        Css.line_height (Num c.h4_line_height);
      ];
    (* Images *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             selector ++ prose_where_element img;
             selector ++ prose_where_element picture;
           ])
      [ Css.margin_top c.img_margin_y; Css.margin_bottom c.img_margin_y ];
    Css.rule
      ~selector:(selector ++ prose_where_element (picture >> img))
      [ Css.margin_top Zero; Css.margin_bottom Zero ];
    Css.rule
      ~selector:(selector ++ prose_where_element video)
      [ Css.margin_top c.img_margin_y; Css.margin_bottom c.img_margin_y ];
    (* Code elements *)
    Css.rule
      ~selector:(selector ++ prose_where_element (element "kbd"))
      [
        Css.padding_top c.kbd_padding_y;
        Css.padding_inline_end c.kbd_padding_x;
        Css.padding_bottom c.kbd_padding_y;
        border_radius c.kbd_border_radius;
        Css.padding_inline_start c.kbd_padding_x;
        Css.font_size c.kbd_font_size;
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element code)
      [ Css.font_size c.code_font_size ];
    Css.rule
      ~selector:(selector ++ prose_where_element (h2 ++ code))
      [ Css.font_size c.h2_code_font_size ];
    Css.rule
      ~selector:(selector ++ prose_where_element (h3 ++ code))
      [ Css.font_size c.h3_code_font_size ];
    Css.rule
      ~selector:(selector ++ prose_where_element pre)
      [
        Css.padding_top c.pre_padding_y;
        Css.padding_inline_end c.pre_padding_x;
        Css.padding_bottom c.pre_padding_y;
        border_radius c.pre_border_radius;
        Css.margin_top c.pre_margin_top;
        Css.margin_bottom c.pre_margin_bottom;
        Css.padding_inline_start c.pre_padding_x;
        Css.font_size c.pre_font_size;
        Css.line_height (Num c.pre_line_height);
      ];
    (* Lists *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             selector ++ prose_where_element ol;
             selector ++ prose_where_element ul;
           ])
      [
        Css.margin_top c.list_margin_y;
        Css.margin_bottom c.list_margin_y;
        Css.padding_inline_start c.list_padding_start;
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element li)
      [ Css.margin_top c.li_margin_y; Css.margin_bottom c.li_margin_y ];
    (* List items with padding *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             selector ++ prose_where_element (ol >> li);
             selector ++ prose_where_element (ul >> li);
           ])
      [ Css.padding_inline_start c.li_padding_start ];
    (* Nested list paragraphs *)
    Css.rule
      ~selector:(prose_child_selector selector (ul >> li ++ p))
      [ Css.margin_top c.li_p_margin_y; Css.margin_bottom c.li_p_margin_y ];
    Css.rule
      ~selector:(prose_child_selector selector (ul >> li >> (p && first_child)))
      [ Css.margin_top c.li_p_first_margin_top ];
    Css.rule
      ~selector:(prose_child_selector selector (ul >> li >> (p && last_child)))
      [ Css.margin_bottom c.li_p_last_margin_bottom ];
    Css.rule
      ~selector:(prose_child_selector selector (ol >> li >> (p && first_child)))
      [ Css.margin_top c.li_p_first_margin_top ];
    Css.rule
      ~selector:(prose_child_selector selector (ol >> li >> (p && last_child)))
      [ Css.margin_bottom c.li_p_last_margin_bottom ];
    (* Nested lists *)
    Css.rule
      ~selector:(selector ++ prose_where_element nested_lists)
      [
        Css.margin_top c.nested_list_margin_y;
        Css.margin_bottom c.nested_list_margin_y;
      ];
    (* Definition lists *)
    Css.rule
      ~selector:(selector ++ prose_where_element dl)
      [ Css.margin_top c.dl_margin_y; Css.margin_bottom c.dl_margin_y ];
    Css.rule
      ~selector:(selector ++ prose_where_element dt)
      [ Css.margin_top c.dt_margin_top ];
    Css.rule
      ~selector:(selector ++ prose_where_element dd)
      [
        Css.margin_top c.dd_margin_top; padding_inline_start c.dd_padding_start;
      ];
    (* Horizontal rule *)
    Css.rule
      ~selector:(selector ++ prose_where_element hr)
      [ Css.margin_top c.hr_margin_y; Css.margin_bottom c.hr_margin_y ];
    (* Following elements *)
    Css.rule
      ~selector:
        (Css.Selector.list
           [
             selector ++ prose_where_element (sibling hr);
             selector ++ prose_where_element (sibling h2);
             selector ++ prose_where_element (sibling h3);
             selector ++ prose_where_element (sibling h4);
           ])
      [ Css.margin_top Zero ];
    (* Tables *)
    Css.rule
      ~selector:(selector ++ prose_where_element table)
      [
        Css.font_size c.table_font_size;
        Css.line_height (Num c.table_line_height);
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element (thead ++ th))
      [
        Css.padding_inline_end c.thead_th_padding_x;
        Css.padding_bottom c.thead_th_padding_bottom;
        Css.padding_inline_start c.thead_th_padding_x;
      ];
    Css.rule
      ~selector:(selector ++ prose_where_element (thead ++ (th && first_child)))
      [ Css.padding_inline_start Zero ];
    Css.rule
      ~selector:(selector ++ prose_where_element (thead ++ (th && last_child)))
      [ Css.padding_inline_end Zero ];
    (* Table body and footer cells *)
    Css.rule
      ~selector:
        (selector
        ++ prose_where_element (Css.Selector.list [ tbody ++ td; tfoot ++ td ])
        )
      [
        Css.padding_top c.tbody_td_padding_y;
        Css.padding_inline_end c.tbody_td_padding_x;
        Css.padding_bottom c.tbody_td_padding_y;
        Css.padding_inline_start c.tbody_td_padding_x;
      ];
    (* First and last table cells *)
    Css.rule
      ~selector:
        (selector
        ++ prose_where_element
             (Css.Selector.list
                [ tbody ++ (td && first_child); tfoot ++ (td && first_child) ])
        )
      [ Css.padding_inline_start Zero ];
    Css.rule
      ~selector:
        (selector
        ++ prose_where_element
             (Css.Selector.list
                [ tbody ++ (td && last_child); tfoot ++ (td && last_child) ]))
      [ Css.padding_inline_end Zero ];
    (* Figure *)
    Css.rule
      ~selector:(selector ++ prose_where_element figure)
      [ Css.margin_top c.figure_margin_y; Css.margin_bottom c.figure_margin_y ];
    Css.rule
      ~selector:
        (selector ++ prose_where_element (combine figure Child universal))
      [ Css.margin_top Zero; Css.margin_bottom Zero ];
    Css.rule
      ~selector:(selector ++ prose_where_element figcaption)
      [
        Css.margin_top c.figcaption_margin_top;
        Css.font_size c.figcaption_font_size;
        Css.line_height (Num c.figcaption_line_height);
      ];
    (* First and last child margins *)
    Css.rule
      ~selector:(prose_child_selector selector first_child)
      [ Css.margin_top Zero ];
    Css.rule
      ~selector:(prose_child_selector selector last_child)
      [ Css.margin_bottom Zero ];
  ]

(* Size configuration for prose-sm *)
let sm_config =
  {
    base_font_size = Rem 0.875;
    base_line_height = 1.71429;
    p_margin_y = Em 1.14286;
    lead_margin_top = Em 0.888889;
    lead_margin_bottom = Em 0.888889;
    lead_font_size = Em 1.28571;
    lead_line_height = 1.55556;
    blockquote_margin_y = Em 1.33333;
    blockquote_padding_start = Em 1.11111;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.8;
    h1_font_size = Em 2.14286;
    h1_line_height = 1.2;
    h2_margin_top = Em 1.6;
    h2_margin_bottom = Em 0.8;
    h2_font_size = Em 1.42857;
    h2_line_height = 1.4;
    h3_margin_top = Em 1.55556;
    h3_margin_bottom = Em 0.444444;
    h3_font_size = Em 1.28571;
    h3_line_height = 1.55556;
    h4_margin_top = Em 1.42857;
    h4_margin_bottom = Em 0.571429;
    h4_line_height = 1.42857;
    img_margin_y = Em 1.71429;
    kbd_padding_y = Em 0.142857;
    kbd_padding_x = Em 0.357143;
    kbd_font_size = Em 0.857143;
    kbd_border_radius = Rem 0.3125;
    code_font_size = Em 0.857143;
    h2_code_font_size = Em 0.9;
    h3_code_font_size = Em 0.888889;
    pre_padding_y = Em 0.666667;
    pre_padding_x = Em 1.0;
    pre_border_radius = Rem 0.25;
    pre_margin_top = Em 1.66667;
    pre_margin_bottom = Em 1.66667;
    pre_font_size = Em 0.857143;
    pre_line_height = 1.66667;
    list_margin_y = Em 1.14286;
    list_padding_start = Em 1.57143;
    li_margin_y = Em 0.285714;
    li_padding_start = Em 0.428571;
    li_p_margin_y = Em 0.571429;
    li_p_first_margin_top = Em 1.14286;
    li_p_last_margin_bottom = Em 1.14286;
    nested_list_margin_y = Em 0.571429;
    dl_margin_y = Em 1.14286;
    dt_margin_top = Em 1.14286;
    dd_margin_top = Em 0.285714;
    dd_padding_start = Em 1.57143;
    hr_margin_y = Em 2.85714;
    table_font_size = Em 0.857143;
    table_line_height = 1.5;
    thead_th_padding_x = Em 1.0;
    thead_th_padding_bottom = Em 0.666667;
    tbody_td_padding_y = Em 0.666667;
    tbody_td_padding_x = Em 1.0;
    figure_margin_y = Em 1.71429;
    figcaption_margin_top = Em 0.666667;
    figcaption_font_size = Em 0.857143;
    figcaption_line_height = 1.33333;
  }

(* Size configuration for prose-lg *)
let lg_config =
  {
    base_font_size = Rem 1.125;
    base_line_height = 1.77778;
    p_margin_y = Em 1.33333;
    lead_margin_top = Em 1.09091;
    lead_margin_bottom = Em 1.09091;
    lead_font_size = Em 1.22222;
    lead_line_height = 1.45455;
    blockquote_margin_y = Em 1.66667;
    blockquote_padding_start = Em 1.0;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.833333;
    h1_font_size = Em 2.66667;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.86667;
    h2_margin_bottom = Em 1.06667;
    h2_font_size = Em 1.66667;
    h2_line_height = 1.33333;
    h3_margin_top = Em 1.66667;
    h3_margin_bottom = Em 0.666667;
    h3_font_size = Em 1.33333;
    h3_line_height = 1.5;
    h4_margin_top = Em 1.77778;
    h4_margin_bottom = Em 0.444444;
    h4_line_height = 1.55556;
    img_margin_y = Em 1.77778;
    kbd_padding_y = Em 0.222222;
    kbd_padding_x = Em 0.444444;
    kbd_font_size = Em 0.888889;
    kbd_border_radius = Rem 0.3125;
    code_font_size = Em 0.888889;
    h2_code_font_size = Em 0.866667;
    h3_code_font_size = Em 0.875;
    pre_padding_y = Em 1.0;
    pre_padding_x = Em 1.5;
    pre_border_radius = Rem 0.375;
    pre_margin_top = Em 2.0;
    pre_margin_bottom = Em 2.0;
    pre_font_size = Em 0.888889;
    pre_line_height = 1.75;
    list_margin_y = Em 1.33333;
    list_padding_start = Em 1.55556;
    li_margin_y = Em 0.666667;
    li_padding_start = Em 0.444444;
    li_p_margin_y = Em 0.888889;
    li_p_first_margin_top = Em 1.33333;
    li_p_last_margin_bottom = Em 1.33333;
    nested_list_margin_y = Em 0.888889;
    dl_margin_y = Em 1.33333;
    dt_margin_top = Em 1.33333;
    dd_margin_top = Em 0.666667;
    dd_padding_start = Em 1.55556;
    hr_margin_y = Em 3.11111;
    table_font_size = Em 0.888889;
    table_line_height = 1.5;
    thead_th_padding_x = Em 0.75;
    thead_th_padding_bottom = Em 0.75;
    tbody_td_padding_y = Em 0.75;
    tbody_td_padding_x = Em 0.75;
    figure_margin_y = Em 1.77778;
    figcaption_margin_top = Em 1.0;
    figcaption_font_size = Em 0.888889;
    figcaption_line_height = 1.5;
  }

(* Size configuration for prose-xl *)
let xl_config =
  {
    base_font_size = Rem 1.25;
    base_line_height = 1.8;
    p_margin_y = Em 1.2;
    lead_margin_top = Em 1.0;
    lead_margin_bottom = Em 1.0;
    lead_font_size = Em 1.2;
    lead_line_height = 1.5;
    blockquote_margin_y = Em 1.6;
    blockquote_padding_start = Em 1.06667;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.857143;
    h1_font_size = Em 2.8;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.55556;
    h2_margin_bottom = Em 0.888889;
    h2_font_size = Em 1.8;
    h2_line_height = 1.11111;
    h3_margin_top = Em 1.6;
    h3_margin_bottom = Em 0.666667;
    h3_font_size = Em 1.5;
    h3_line_height = 1.33333;
    h4_margin_top = Em 1.8;
    h4_margin_bottom = Em 0.6;
    h4_line_height = 1.6;
    img_margin_y = Em 2.0;
    kbd_padding_y = Em 0.25;
    kbd_padding_x = Em 0.4;
    kbd_font_size = Em 0.9;
    kbd_border_radius = Rem 0.3125;
    code_font_size = Em 0.9;
    h2_code_font_size = Em 0.861111;
    h3_code_font_size = Em 0.9;
    pre_padding_y = Em 1.11111;
    pre_padding_x = Em 1.33333;
    pre_border_radius = Rem 0.5;
    pre_margin_top = Em 2.0;
    pre_margin_bottom = Em 2.0;
    pre_font_size = Em 0.9;
    pre_line_height = 1.77778;
    list_margin_y = Em 1.2;
    list_padding_start = Em 1.6;
    li_margin_y = Em 0.6;
    li_padding_start = Em 0.4;
    li_p_margin_y = Em 0.8;
    li_p_first_margin_top = Em 1.2;
    li_p_last_margin_bottom = Em 1.2;
    nested_list_margin_y = Em 0.8;
    dl_margin_y = Em 1.2;
    dt_margin_top = Em 1.2;
    dd_margin_top = Em 0.6;
    dd_padding_start = Em 1.6;
    hr_margin_y = Em 2.8;
    table_font_size = Em 0.9;
    table_line_height = 1.55556;
    thead_th_padding_x = Em 0.666667;
    thead_th_padding_bottom = Em 0.888889;
    tbody_td_padding_y = Em 0.888889;
    tbody_td_padding_x = Em 0.666667;
    figure_margin_y = Em 2.0;
    figcaption_margin_top = Em 1.0;
    figcaption_font_size = Em 0.9;
    figcaption_line_height = 1.55556;
  }

(* Size configuration for prose-2xl *)
let xl2_config =
  {
    base_font_size = Rem 1.5;
    base_line_height = 1.66667;
    p_margin_y = Em 1.33333;
    lead_margin_top = Em 1.06667;
    lead_margin_bottom = Em 1.06667;
    lead_font_size = Em 1.25;
    lead_line_height = 1.46667;
    blockquote_margin_y = Em 1.77778;
    blockquote_padding_start = Em 1.11111;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.875;
    h1_font_size = Em 2.66667;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.5;
    h2_margin_bottom = Em 0.833333;
    h2_font_size = Em 2.0;
    h2_line_height = 1.08333;
    h3_margin_top = Em 1.55556;
    h3_margin_bottom = Em 0.666667;
    h3_font_size = Em 1.5;
    h3_line_height = 1.22222;
    h4_margin_top = Em 1.66667;
    h4_margin_bottom = Em 0.666667;
    h4_line_height = 1.5;
    img_margin_y = Em 2.0;
    kbd_padding_y = Em 0.25;
    kbd_padding_x = Em 0.333333;
    kbd_font_size = Em 0.833333;
    kbd_border_radius = Rem 0.375;
    code_font_size = Em 0.833333;
    h2_code_font_size = Em 0.875;
    h3_code_font_size = Em 0.888889;
    pre_padding_y = Em 1.2;
    pre_padding_x = Em 1.6;
    pre_border_radius = Rem 0.5;
    pre_margin_top = Em 2.0;
    pre_margin_bottom = Em 2.0;
    pre_font_size = Em 0.833333;
    pre_line_height = 1.8;
    list_margin_y = Em 1.33333;
    list_padding_start = Em 1.58333;
    li_margin_y = Em 0.5;
    li_padding_start = Em 0.416667;
    li_p_margin_y = Em 0.833333;
    li_p_first_margin_top = Em 1.33333;
    li_p_last_margin_bottom = Em 1.33333;
    nested_list_margin_y = Em 0.666667;
    dl_margin_y = Em 1.33333;
    dt_margin_top = Em 1.33333;
    dd_margin_top = Em 0.5;
    dd_padding_start = Em 1.58333;
    hr_margin_y = Em 3.0;
    table_font_size = Em 0.833333;
    table_line_height = 1.4;
    thead_th_padding_x = Em 0.6;
    thead_th_padding_bottom = Em 0.8;
    tbody_td_padding_y = Em 0.8;
    tbody_td_padding_x = Em 0.6;
    figure_margin_y = Em 2.0;
    figcaption_margin_top = Em 1.0;
    figcaption_font_size = Em 0.833333;
    figcaption_line_height = 1.6;
  }

let sm_size_rules selector =
  typography_rules (Css.Selector.class_ selector) sm_config

let lg_size_rules selector =
  typography_rules (Css.Selector.class_ selector) lg_config

let xl_size_rules selector =
  typography_rules (Css.Selector.class_ selector) xl_config

let xl2_size_rules selector =
  typography_rules (Css.Selector.class_ selector) xl2_config

(* Helper to create color variable bindings for color themes - returns
   declarations *)
let color_theme_bindings theme_name =
  match theme_name with
  | "gray" ->
      (* Normal theme *)
      let d1, _ = Var.binding prose_body_var (Css.oklch 37.3 0.034 259.733) in
      let d2, _ =
        Var.binding prose_headings_var (Css.oklch 21.0 0.034 264.665)
      in
      let d3, _ = Var.binding prose_lead_var (Css.oklch 44.6 0.030 256.802) in
      let d4, _ = Var.binding prose_links_var (Css.oklch 21.0 0.034 264.665) in
      let d5, _ = Var.binding prose_bold_var (Css.oklch 21.0 0.034 264.665) in
      let d6, _ =
        Var.binding prose_counters_var (Css.oklch 55.1 0.027 264.364)
      in
      let d7, _ =
        Var.binding prose_bullets_var (Css.oklch 87.2 0.010 258.338)
      in
      let d8, _ = Var.binding prose_hr_var (Css.oklch 92.8 0.006 264.531) in
      let d9, _ = Var.binding prose_quotes_var (Css.oklch 21.0 0.034 264.665) in
      let d10, _ =
        Var.binding prose_quote_borders_var (Css.oklch 92.8 0.006 264.531)
      in
      let d11, _ =
        Var.binding prose_captions_var (Css.oklch 55.1 0.027 264.364)
      in
      let d12, _ = Var.binding prose_kbd_var (Css.oklch 21.0 0.034 264.665) in
      let d13, _ =
        Var.binding prose_kbd_shadows_var
          (Css.Channels { r = Num nan; g = Num nan; b = Num nan })
      in
      let d14, _ = Var.binding prose_code_var (Css.oklch 21.0 0.034 264.665) in
      let d15, _ =
        Var.binding prose_pre_code_var (Css.oklch 92.8 0.006 264.531)
      in
      let d16, _ =
        Var.binding prose_pre_bg_var (Css.oklch 27.8 0.033 256.848)
      in
      let d17, _ =
        Var.binding prose_th_borders_var (Css.oklch 87.2 0.010 258.338)
      in
      let d18, _ =
        Var.binding prose_td_borders_var (Css.oklch 92.8 0.006 264.531)
      in
      (* Invert theme *)
      let d19, _ =
        Var.binding prose_invert_body_var (Css.oklch 87.2 0.010 258.338)
      in
      let d20, _ = Var.binding prose_invert_headings_var (Css.hex "fff") in
      let d21, _ =
        Var.binding prose_invert_lead_var (Css.oklch 70.7 0.022 261.325)
      in
      let d22, _ = Var.binding prose_invert_links_var (Css.hex "fff") in
      let d23, _ = Var.binding prose_invert_bold_var (Css.hex "fff") in
      let d24, _ =
        Var.binding prose_invert_counters_var (Css.oklch 70.7 0.022 261.325)
      in
      let d25, _ =
        Var.binding prose_invert_bullets_var (Css.oklch 44.6 0.030 256.802)
      in
      let d26, _ =
        Var.binding prose_invert_hr_var (Css.oklch 37.3 0.034 259.733)
      in
      let d27, _ =
        Var.binding prose_invert_quotes_var (Css.oklch 96.7 0.003 264.542)
      in
      let d28, _ =
        Var.binding prose_invert_quote_borders_var
          (Css.oklch 37.3 0.034 259.733)
      in
      let d29, _ =
        Var.binding prose_invert_captions_var (Css.oklch 70.7 0.022 261.325)
      in
      let d30, _ = Var.binding prose_invert_kbd_var (Css.hex "fff") in
      let d31, _ =
        Var.binding prose_invert_kbd_shadows_var
          (Css.Channels { r = Num 255.0; g = Num 255.0; b = Num 255.0 })
      in
      let d32, _ = Var.binding prose_invert_code_var (Css.hex "fff") in
      let d33, _ =
        Var.binding prose_invert_pre_code_var (Css.oklch 87.2 0.010 258.338)
      in
      let d34, _ = Var.binding prose_invert_pre_bg_var (Css.hex "00000080") in
      let d35, _ =
        Var.binding prose_invert_th_borders_var (Css.oklch 44.6 0.030 256.802)
      in
      let d36, _ =
        Var.binding prose_invert_td_borders_var (Css.oklch 37.3 0.034 259.733)
      in
      [
        d1;
        d2;
        d3;
        d4;
        d5;
        d6;
        d7;
        d8;
        d9;
        d10;
        d11;
        d12;
        d13;
        d14;
        d15;
        d16;
        d17;
        d18;
        d19;
        d20;
        d21;
        d22;
        d23;
        d24;
        d25;
        d26;
        d27;
        d28;
        d29;
        d30;
        d31;
        d32;
        d33;
        d34;
        d35;
        d36;
      ]
  | "slate" ->
      (* Normal theme *)
      let d1, _ = Var.binding prose_body_var (Css.oklch 37.2 0.044 257.287) in
      let d2, _ =
        Var.binding prose_headings_var (Css.oklch 20.8 0.042 265.755)
      in
      let d3, _ = Var.binding prose_lead_var (Css.oklch 44.6 0.043 257.281) in
      let d4, _ = Var.binding prose_links_var (Css.oklch 20.8 0.042 265.755) in
      let d5, _ = Var.binding prose_bold_var (Css.oklch 20.8 0.042 265.755) in
      let d6, _ =
        Var.binding prose_counters_var (Css.oklch 55.4 0.046 257.417)
      in
      let d7, _ =
        Var.binding prose_bullets_var (Css.oklch 86.9 0.022 252.894)
      in
      let d8, _ = Var.binding prose_hr_var (Css.oklch 92.9 0.013 255.508) in
      let d9, _ = Var.binding prose_quotes_var (Css.oklch 20.8 0.042 265.755) in
      let d10, _ =
        Var.binding prose_quote_borders_var (Css.oklch 92.9 0.013 255.508)
      in
      let d11, _ =
        Var.binding prose_captions_var (Css.oklch 55.4 0.046 257.417)
      in
      let d12, _ = Var.binding prose_kbd_var (Css.oklch 20.8 0.042 265.755) in
      let d13, _ =
        Var.binding prose_kbd_shadows_var
          (Css.Channels { r = Num nan; g = Num nan; b = Num nan })
      in
      let d14, _ = Var.binding prose_code_var (Css.oklch 20.8 0.042 265.755) in
      let d15, _ =
        Var.binding prose_pre_code_var (Css.oklch 92.9 0.013 255.508)
      in
      let d16, _ =
        Var.binding prose_pre_bg_var (Css.oklch 27.9 0.041 260.031)
      in
      let d17, _ =
        Var.binding prose_th_borders_var (Css.oklch 86.9 0.022 252.894)
      in
      let d18, _ =
        Var.binding prose_td_borders_var (Css.oklch 92.9 0.013 255.508)
      in
      (* Invert theme *)
      let d19, _ =
        Var.binding prose_invert_body_var (Css.oklch 86.9 0.022 252.894)
      in
      let d20, _ = Var.binding prose_invert_headings_var (Css.hex "fff") in
      let d21, _ =
        Var.binding prose_invert_lead_var (Css.oklch 70.4 0.040 256.788)
      in
      let d22, _ = Var.binding prose_invert_links_var (Css.hex "fff") in
      let d23, _ = Var.binding prose_invert_bold_var (Css.hex "fff") in
      let d24, _ =
        Var.binding prose_invert_counters_var (Css.oklch 70.4 0.040 256.788)
      in
      let d25, _ =
        Var.binding prose_invert_bullets_var (Css.oklch 44.6 0.043 257.281)
      in
      let d26, _ =
        Var.binding prose_invert_hr_var (Css.oklch 37.2 0.044 257.287)
      in
      let d27, _ =
        Var.binding prose_invert_quotes_var (Css.oklch 96.8 0.007 247.896)
      in
      let d28, _ =
        Var.binding prose_invert_quote_borders_var
          (Css.oklch 37.2 0.044 257.287)
      in
      let d29, _ =
        Var.binding prose_invert_captions_var (Css.oklch 70.4 0.040 256.788)
      in
      let d30, _ = Var.binding prose_invert_kbd_var (Css.hex "fff") in
      let d31, _ =
        Var.binding prose_invert_kbd_shadows_var
          (Css.Channels { r = Num 255.0; g = Num 255.0; b = Num 255.0 })
      in
      let d32, _ = Var.binding prose_invert_code_var (Css.hex "fff") in
      let d33, _ =
        Var.binding prose_invert_pre_code_var (Css.oklch 86.9 0.022 252.894)
      in
      let d34, _ = Var.binding prose_invert_pre_bg_var (Css.hex "00000080") in
      let d35, _ =
        Var.binding prose_invert_th_borders_var (Css.oklch 44.6 0.043 257.281)
      in
      let d36, _ =
        Var.binding prose_invert_td_borders_var (Css.oklch 37.2 0.044 257.287)
      in
      [
        d1;
        d2;
        d3;
        d4;
        d5;
        d6;
        d7;
        d8;
        d9;
        d10;
        d11;
        d12;
        d13;
        d14;
        d15;
        d16;
        d17;
        d18;
        d19;
        d20;
        d21;
        d22;
        d23;
        d24;
        d25;
        d26;
        d27;
        d28;
        d29;
        d30;
        d31;
        d32;
        d33;
        d34;
        d35;
        d36;
      ]
  | "zinc" -> [] (* TODO: Add zinc theme bindings *)
  | "neutral" -> [] (* TODO: Add neutral theme bindings *)
  | "stone" -> [] (* TODO: Add stone theme bindings *)
  | _ -> []

let to_css_rules variant =
  match variant with
  | `Base -> base_prose_rules ()
  | `Sm -> sm_size_rules "prose-sm"
  | `Lg -> lg_size_rules "prose-lg"
  | `Xl -> xl_size_rules "prose-xl"
  | `Xl2 -> xl2_size_rules "prose-2xl"
  | `Gray ->
      [
        Css.rule
          ~selector:(Css.Selector.class_ "prose-gray")
          (color_theme_bindings "gray");
      ]
  | `Slate ->
      [
        Css.rule
          ~selector:(Css.Selector.class_ "prose-slate")
          (color_theme_bindings "slate");
      ]
  | `Zinc ->
      [
        Css.rule
          ~selector:(Css.Selector.class_ "prose-zinc")
          (color_theme_bindings "zinc");
      ]
  | `Neutral ->
      [
        Css.rule
          ~selector:(Css.Selector.class_ "prose-neutral")
          (color_theme_bindings "neutral");
      ]
  | `Stone ->
      [
        Css.rule
          ~selector:(Css.Selector.class_ "prose-stone")
          (color_theme_bindings "stone");
      ]

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
  let rules = to_css_rules variant in
  Style.style ~rules:(Some rules) []

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

let niet = Style.style ~rules:(Some []) []

(* Handler for prose size variants - priority 2 *)
module Handler = struct
  (** Local prose utility type for size variants *)
  type t =
    | Prose
    | Prose_sm
    | Prose_lg
    | Prose_xl
    | Prose_2xl
    | Lead
    | Not_prose

  (** Extensible variant for prose utilities *)
  type Utility.base += Self of t

  (** Priority for prose size utilities *)
  let name = "prose"

  let priority = 2 (* Same as margin to allow interleaving by suborder *)

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Prose -> prose_style `Base
    | Prose_sm -> prose_style `Sm
    | Prose_lg -> prose_style `Lg
    | Prose_xl -> prose_style `Xl
    | Prose_2xl -> prose_style `Xl2
    | Lead -> niet
    | Not_prose -> niet

  let to_class = function
    | Prose -> "prose"
    | Prose_sm -> "prose-sm"
    | Prose_lg -> "prose-lg"
    | Prose_xl -> "prose-xl"
    | Prose_2xl -> "prose-2xl"
    | Lead -> "lead"
    | Not_prose -> "not-prose"

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "prose" ] -> Ok Prose
    | [ "prose"; "sm" ] -> Ok Prose_sm
    | [ "prose"; "lg" ] -> Ok Prose_lg
    | [ "prose"; "xl" ] -> Ok Prose_xl
    | [ "prose"; "2xl" ] -> Ok Prose_2xl
    | [ "lead" ] -> Ok Lead
    | [ "not"; "prose" ] -> Ok Not_prose
    | _ -> Error (`Msg "Not a prose size utility")

  let suborder = function
    (* Prose size utilities appear between my-* (200000-299999) and mt-*
       (300000+). Place them at 290000+ to come after all my-* utilities. *)
    | Prose -> 290000
    | Prose_2xl -> 290001 (* Alphabetical: 2xl before lg, sm, xl *)
    | Prose_lg -> 290002
    | Prose_sm -> 290003
    | Prose_xl -> 290004
    | Lead -> 290005
    | Not_prose -> 290006
end

(* Handler for prose color variants - priority 21 *)
module Color_Handler = struct
  (** Local prose utility type for color variants *)
  type t =
    | Prose_gray
    | Prose_slate
    | Prose_zinc
    | Prose_neutral
    | Prose_stone
    | Prose_invert

  (** Extensible variant for prose color utilities *)
  type Utility.base += Self of t

  (** Priority for prose color utilities *)
  let name = "prose-color"

  let priority = 23 (* Same as color utilities *)

  (** {1 Utility Conversion Functions} *)

  let to_style = function
    | Prose_gray -> prose_style `Gray
    | Prose_slate -> prose_style `Slate
    | Prose_zinc -> prose_style `Zinc
    | Prose_neutral -> prose_style `Neutral
    | Prose_stone -> prose_style `Stone
    | Prose_invert -> niet

  let to_class = function
    | Prose_gray -> "prose-gray"
    | Prose_slate -> "prose-slate"
    | Prose_zinc -> "prose-zinc"
    | Prose_neutral -> "prose-neutral"
    | Prose_stone -> "prose-stone"
    | Prose_invert -> "prose-invert"

  let of_class class_name =
    let parts = String.split_on_char '-' class_name in
    match parts with
    | [ "prose"; "gray" ] -> Ok Prose_gray
    | [ "prose"; "slate" ] -> Ok Prose_slate
    | [ "prose"; "zinc" ] -> Ok Prose_zinc
    | [ "prose"; "neutral" ] -> Ok Prose_neutral
    | [ "prose"; "stone" ] -> Ok Prose_stone
    | [ "prose"; "invert" ] -> Ok Prose_invert
    | _ -> Error (`Msg "Not a prose color utility")

  let suborder = function
    (* Prose color utilities come after text colors (20000-29999) *)
    | Prose_gray -> 30000
    | Prose_slate -> 30001
    | Prose_zinc -> 30002
    | Prose_neutral -> 30003
    | Prose_stone -> 30004
    | Prose_invert -> 30005
end

(** Register both handlers with Utility system *)
let () = Utility.register (module Handler)

let () = Utility.register (module Color_Handler)

(** Public API *)
let utility x = Utility.base (Handler.Self x)

let color_utility x = Utility.base (Color_Handler.Self x)
let prose = utility Handler.Prose
let prose_sm = utility Handler.Prose_sm
let prose_lg = utility Handler.Prose_lg
let prose_xl = utility Handler.Prose_xl
let prose_2xl = utility Handler.Prose_2xl
let prose_gray = color_utility Color_Handler.Prose_gray
let prose_slate = color_utility Color_Handler.Prose_slate
let prose_zinc = color_utility Color_Handler.Prose_zinc
let prose_neutral = color_utility Color_Handler.Prose_neutral
let prose_stone = color_utility Color_Handler.Prose_stone
let prose_lead = utility Handler.Lead
let not_prose = utility Handler.Not_prose
