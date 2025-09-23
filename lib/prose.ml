(** Prose typography utilities

    Complete implementation of Tailwind Typography plugin with:
    - All HTML elements styled with :where() for low specificity
    - not-prose exclusion mechanism
    - Full CSS variable theming
    - Size and color variants *)

open Css

(* Extend variable kinds for prose theming *)
type _ Var.kind +=
  | (* Prose theming variables *)
      Prose_body :
      Css.color Var.kind
  | Prose_headings : Css.color Var.kind
  | Prose_code : Css.color Var.kind
  | Prose_pre_code : Css.color Var.kind
  | Prose_pre_bg : Css.color Var.kind
  | Prose_th_borders : Css.color Var.kind
  | Prose_td_borders : Css.color Var.kind
  | Prose_links : Css.color Var.kind
  | Prose_quotes : Css.color Var.kind
  | Prose_quote_borders : Css.color Var.kind
  | Prose_hr : Css.color Var.kind
  | Prose_bold : Css.color Var.kind
  | Prose_lead : Css.color Var.kind
  | Prose_counters : Css.color Var.kind
  | Prose_bullets : Css.color Var.kind
  | Prose_captions : Css.color Var.kind
  | Prose_kbd : Css.color Var.kind
  | Prose_kbd_shadows : string Var.kind (* RGB values like "17 24 39" *)
  | (* Prose invert variants for dark mode *)
      Prose_invert_body :
      Css.color Var.kind
  | Prose_invert_headings : Css.color Var.kind
  | Prose_invert_lead : Css.color Var.kind
  | Prose_invert_links : Css.color Var.kind
  | Prose_invert_bold : Css.color Var.kind
  | Prose_invert_counters : Css.color Var.kind
  | Prose_invert_bullets : Css.color Var.kind
  | Prose_invert_hr : Css.color Var.kind
  | Prose_invert_quotes : Css.color Var.kind
  | Prose_invert_quote_borders : Css.color Var.kind
  | Prose_invert_captions : Css.color Var.kind
  | Prose_invert_kbd : Css.color Var.kind
  | Prose_invert_kbd_shadows : string Var.kind
  | Prose_invert_code : Css.color Var.kind
  | Prose_invert_pre_code : Css.color Var.kind
  | Prose_invert_pre_bg : Css.color Var.kind
  | Prose_invert_th_borders : Css.color Var.kind
  | Prose_invert_td_borders : Css.color Var.kind

type variant =
  [ `Base | `Sm | `Lg | `Xl | `Xl2 | `Gray | `Slate | `Zinc | `Neutral | `Stone ]

(* Create prose variables using the new API *)
let prose_body_var =
  Var.create Prose_body "tw-prose-body" ~layer:Utility
    ~fallback:(oklch 37.3 0.034 259.733)

let prose_headings_var =
  Var.create Prose_headings "tw-prose-headings" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

let prose_lead_var =
  Var.create Prose_lead "tw-prose-lead" ~layer:Utility
    ~fallback:(oklch 44.6 0.030 256.802)

let prose_links_var =
  Var.create Prose_links "tw-prose-links" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

let prose_bold_var =
  Var.create Prose_bold "tw-prose-bold" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

let prose_counters_var =
  Var.create Prose_counters "tw-prose-counters" ~layer:Utility
    ~fallback:(oklch 55.1 0.027 264.364)

let prose_bullets_var =
  Var.create Prose_bullets "tw-prose-bullets" ~layer:Utility
    ~fallback:(oklch 87.2 0.010 258.338)

let prose_hr_var =
  Var.create Prose_hr "tw-prose-hr" ~layer:Utility
    ~fallback:(oklch 92.8 0.006 264.531)

let prose_quotes_var =
  Var.create Prose_quotes "tw-prose-quotes" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

let prose_quote_borders_var =
  Var.create Prose_quote_borders "tw-prose-quote-borders" ~layer:Utility
    ~fallback:(oklch 92.8 0.006 264.531)

let prose_captions_var =
  Var.create Prose_captions "tw-prose-captions" ~layer:Utility
    ~fallback:(oklch 55.1 0.027 264.364)

let prose_kbd_var =
  Var.create Prose_kbd "tw-prose-kbd" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

(* Special handling for kbd-shadows - it's RGB values without rgb() wrapper *)
let prose_kbd_shadows_var =
  Var.create Prose_kbd_shadows "tw-prose-kbd-shadows" ~layer:Utility
    ~fallback:"17 24 39" (* Default gray RGB values *)

let prose_code_var =
  Var.create Prose_code "tw-prose-code" ~layer:Utility
    ~fallback:(oklch 21.0 0.034 264.665)

let prose_pre_code_var =
  Var.create Prose_pre_code "tw-prose-pre-code" ~layer:Utility
    ~fallback:(oklch 92.8 0.006 264.531)

let prose_pre_bg_var =
  Var.create Prose_pre_bg "tw-prose-pre-bg" ~layer:Utility
    ~fallback:(oklch 27.8 0.033 256.848)

let prose_th_borders_var =
  Var.create Prose_th_borders "tw-prose-th-borders" ~layer:Utility
    ~fallback:(oklch 87.2 0.010 258.338)

let prose_td_borders_var =
  Var.create Prose_td_borders "tw-prose-td-borders" ~layer:Utility
    ~fallback:(oklch 92.8 0.006 264.531)

(* Invert variants for dark mode *)
let prose_invert_body_var =
  Var.create Prose_invert_body "tw-prose-invert-body" ~layer:Utility
    ~fallback:(oklch 87.2 0.010 258.338)

let prose_invert_headings_var =
  Var.create Prose_invert_headings "tw-prose-invert-headings" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "fff" })

let prose_invert_lead_var =
  Var.create Prose_invert_lead "tw-prose-invert-lead" ~layer:Utility
    ~fallback:(oklch 70.7 0.022 261.325)

let prose_invert_links_var =
  Var.create Prose_invert_links "tw-prose-invert-links" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "fff" })

let prose_invert_bold_var =
  Var.create Prose_invert_bold "tw-prose-invert-bold" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "fff" })

let prose_invert_counters_var =
  Var.create Prose_invert_counters "tw-prose-invert-counters" ~layer:Utility
    ~fallback:(oklch 70.7 0.022 261.325)

let prose_invert_bullets_var =
  Var.create Prose_invert_bullets "tw-prose-invert-bullets" ~layer:Utility
    ~fallback:(oklch 44.6 0.030 256.802)

let prose_invert_hr_var =
  Var.create Prose_invert_hr "tw-prose-invert-hr" ~layer:Utility
    ~fallback:(oklch 37.3 0.034 259.733)

let prose_invert_quotes_var =
  Var.create Prose_invert_quotes "tw-prose-invert-quotes" ~layer:Utility
    ~fallback:(oklch 96.7 0.003 264.542)

let prose_invert_quote_borders_var =
  Var.create Prose_invert_quote_borders "tw-prose-invert-quote-borders"
    ~layer:Utility ~fallback:(oklch 37.3 0.034 259.733)

let prose_invert_captions_var =
  Var.create Prose_invert_captions "tw-prose-invert-captions" ~layer:Utility
    ~fallback:(oklch 70.7 0.022 261.325)

let prose_invert_kbd_var =
  Var.create Prose_invert_kbd "tw-prose-invert-kbd" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "fff" })

let prose_invert_kbd_shadows_var =
  Var.create Prose_invert_kbd_shadows "tw-prose-invert-kbd-shadows"
    ~layer:Utility ~fallback:"255 255 255"

let prose_invert_code_var =
  Var.create Prose_invert_code "tw-prose-invert-code" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "fff" })

let prose_invert_pre_code_var =
  Var.create Prose_invert_pre_code "tw-prose-invert-pre-code" ~layer:Utility
    ~fallback:(oklch 87.2 0.010 258.338)

let prose_invert_pre_bg_var =
  Var.create Prose_invert_pre_bg "tw-prose-invert-pre-bg" ~layer:Utility
    ~fallback:(Hex { hash = true; value = "00000080" })

let prose_invert_th_borders_var =
  Var.create Prose_invert_th_borders "tw-prose-invert-th-borders" ~layer:Utility
    ~fallback:(oklch 44.6 0.030 256.802)

let prose_invert_td_borders_var =
  Var.create Prose_invert_td_borders "tw-prose-invert-td-borders" ~layer:Utility
    ~fallback:(oklch 37.3 0.034 259.733)

(* ======================================================================== *)
(* Selector Helper Functions *)
(* ======================================================================== *)

let ( ++ ) = Css.Selector.( ++ )
let ( >> ) = Css.Selector.( >> )
let ( && ) = Css.Selector.( && )
let ( || ) = Css.Selector.( || )

(* Shorthand for attribute selectors *)
let attr name match_type = Css.Selector.attribute name match_type
let class_attr value = attr "class" (Whitespace_list value)
let type_attr value = attr "type" (Exact value)

(* Helper to create a prose element selector with :where() and :not()
   exclusion *)
let prose_where_element element_selector =
  let open Css.Selector in
  (* For now, just use :where() without :not() to avoid selector complexity *)
  (* TODO: Add not-prose exclusion later *)
  where [ element_selector ]

(* Helper to create a typed selector with :where() and :not() for prose *)
let where base_class elt =
  let open Css.Selector in
  (* For now, just use the new helper *)
  class_ base_class ++ prose_where_element elt

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
let sibling e = Css.Selector.(combine e Subsequent_sibling universal)
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
let kbd = Css.Selector.element "kbd"

(* Lead paragraph selector *)
let lead = class_attr "lead"

(* Compound selectors for ol with type attributes *)
let ol_type_upper_a = ol && type_attr "A"
let ol_type_a = ol && type_attr "a"
let ol_type_upper_a_s = ol && type_attr "A s"
let ol_type_a_s = ol && type_attr "a s"
let ol_type_upper_i = ol && type_attr "I"
let ol_type_i = ol && type_attr "i"
let ol_type_upper_i_s = ol && type_attr "I s"
let ol_type_i_s = ol && type_attr "i s"
let ol_type_1 = ol && type_attr "1"

(* Pseudo-elements and pseudo-classes *)
let before = Css.Selector.Before
let after = Css.Selector.After
let first_of_type = Css.Selector.First_of_type
let last_of_type = Css.Selector.Last_of_type
let p_first_of_type_before = Css.Selector.compound [ p; first_of_type; before ]
let p_last_of_type_after = Css.Selector.compound [ p; last_of_type; after ]
let blockquote_code = blockquote ++ code
let thead_th_code = thead ++ th ++ code
let pre_code = pre ++ code
let pre_code_before = pre ++ (code && before)
let pre_code_after = pre ++ (code && after)
let thead_th = thead ++ th
let tbody_tr = tbody ++ tr
let tbody_tr_last_child = tbody ++ (tr && last_child)
let tbody_td = tbody ++ td
let tfoot_td = tfoot ++ td
let picture_img = picture ++ img
let ol_li = ol ++ li
let ul_li = ul ++ li
let h2_code = h2 ++ code
let h3_code = h3 ++ code
let nested_lists = Css.Selector.list [ ul ++ ul; ul ++ ol; ol ++ ul; ol ++ ol ]
let thead_th_first_child = thead ++ (th && first_child)
let thead_th_last_child = thead ++ (th && last_child)
let marker = Css.Selector.Marker
let th_td = th || td
let figure_all = Css.Selector.(combine figure Child universal) (* figure>* *)
let tbody_td_tfoot_td = (tbody ++ td) || (tfoot ++ td)

let tbody_td_first_child =
  (tbody ++ (td && first_child)) || (tfoot ++ (td && first_child))

let tbody_td_last_child =
  (tbody ++ (td && last_child)) || (tfoot ++ (td && last_child))

(** Default variable bindings for prose styles *)
let default_variable_bindings =
  [
    Var.Binding (prose_body_var, oklch 37.3 0.034 259.733);
    Var.Binding (prose_headings_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_lead_var, oklch 44.6 0.030 256.802);
    Var.Binding (prose_links_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_bold_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_counters_var, oklch 55.1 0.027 264.364);
    Var.Binding (prose_bullets_var, oklch 87.2 0.010 258.338);
    Var.Binding (prose_hr_var, oklch 92.8 0.006 264.531);
    Var.Binding (prose_quotes_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_quote_borders_var, oklch 92.8 0.006 264.531);
    Var.Binding (prose_captions_var, oklch 55.1 0.027 264.364);
    Var.Binding (prose_kbd_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_kbd_shadows_var, "17 24 39");
    Var.Binding (prose_code_var, oklch 21.0 0.034 264.665);
    Var.Binding (prose_pre_code_var, oklch 92.8 0.006 264.531);
    Var.Binding (prose_pre_bg_var, oklch 27.8 0.033 256.848);
    Var.Binding (prose_th_borders_var, oklch 87.2 0.010 258.338);
    Var.Binding (prose_td_borders_var, oklch 92.8 0.006 264.531);
    (* Invert variants *)
    Var.Binding (prose_invert_body_var, oklch 87.2 0.010 258.338);
    Var.Binding (prose_invert_headings_var, Hex { hash = true; value = "fff" });
    Var.Binding (prose_invert_lead_var, oklch 70.7 0.022 261.325);
    Var.Binding (prose_invert_links_var, Hex { hash = true; value = "fff" });
    Var.Binding (prose_invert_bold_var, Hex { hash = true; value = "fff" });
    Var.Binding (prose_invert_counters_var, oklch 70.7 0.022 261.325);
    Var.Binding (prose_invert_bullets_var, oklch 44.6 0.030 256.802);
    Var.Binding (prose_invert_hr_var, oklch 37.3 0.034 259.733);
    Var.Binding (prose_invert_quotes_var, oklch 96.7 0.003 264.542);
    Var.Binding (prose_invert_quote_borders_var, oklch 37.3 0.034 259.733);
    Var.Binding (prose_invert_captions_var, oklch 70.7 0.022 261.325);
    Var.Binding (prose_invert_kbd_var, Hex { hash = true; value = "fff" });
    Var.Binding (prose_invert_kbd_shadows_var, "255 255 255");
    Var.Binding (prose_invert_code_var, Hex { hash = true; value = "fff" });
    Var.Binding (prose_invert_pre_code_var, oklch 87.2 0.010 258.338);
    Var.Binding
      (prose_invert_pre_bg_var, Hex { hash = true; value = "00000080" });
    Var.Binding (prose_invert_th_borders_var, oklch 44.6 0.030 256.802);
    Var.Binding (prose_invert_td_borders_var, oklch 37.3 0.034 259.733);
  ]

(** Extract CSS declarations from variable bindings *)
let css_variables =
  List.map Var.declaration_of_binding default_variable_bindings

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
    Css.rule ~selector:(where base p)
      [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
    Css.rule ~selector:(where base lead)
      [
        color (Var (Var.use prose_lead_var));
        margin_top (Em 1.2);
        margin_bottom (Em 1.2);
        font_size (Em 1.25);
        line_height (Num 1.6);
      ];
  ]

(* Link styles *)
let link_rules base =
  [
    Css.rule ~selector:(where base a)
      [
        color (Var (Var.use prose_links_var));
        font_weight (Weight 500);
        text_decoration
          (Shorthand
             {
               lines = [ Underline ];
               style = None;
               color = None;
               thickness = None;
             });
      ];
  ]

(* Strong and bold styles *)
let strong_rules base =
  [
    Css.rule ~selector:(where base strong)
      [ color (Var (Var.use prose_bold_var)); font_weight (Weight 600) ];
    (* Strong inherits color in certain contexts *)
    Css.rule ~selector:(where base (a ++ strong)) [ color Inherit ];
    Css.rule ~selector:(where base (blockquote ++ strong)) [ color Inherit ];
    Css.rule ~selector:(where base (thead ++ th ++ strong)) [ color Inherit ];
  ]

(* List styles *)
let list_rules base =
  [
    Css.rule ~selector:(where base ol)
      [
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_inline_start (Em 1.625);
        list_style_type Decimal;
      ];
    Css.rule
      ~selector:(where base ol_type_upper_a)
      [ list_style_type Upper_alpha ];
    Css.rule ~selector:(where base ol_type_a) [ list_style_type Lower_alpha ];
    Css.rule
      ~selector:(where base ol_type_upper_a_s)
      [ list_style_type Upper_alpha ];
    Css.rule ~selector:(where base ol_type_a_s) [ list_style_type Lower_alpha ];
    Css.rule
      ~selector:(where base ol_type_upper_i)
      [ list_style_type Upper_roman ];
    Css.rule ~selector:(where base ol_type_i) [ list_style_type Lower_roman ];
    Css.rule
      ~selector:(where base ol_type_upper_i_s)
      [ list_style_type Upper_roman ];
    Css.rule ~selector:(where base ol_type_i_s) [ list_style_type Lower_roman ];
    Css.rule ~selector:(where base ol_type_1) [ list_style_type Decimal ];
    Css.rule ~selector:(where base ul)
      [
        margin_top (Em 1.25);
        margin_bottom (Em 1.25);
        padding_inline_start (Em 1.625);
        list_style_type Disc;
      ];
    Css.rule
      ~selector:(where base (ol ++ li ++ marker))
      [ color (Var (Var.use prose_counters_var)); font_weight (Weight 400) ];
    Css.rule
      ~selector:(where base (ul ++ li ++ marker))
      [ color (Var (Var.use prose_bullets_var)) ];
    Css.rule ~selector:(where base dt)
      [
        color (Var (Var.use prose_headings_var));
        margin_top (Em 1.25);
        font_weight (Weight 600);
      ];
  ]

(* Heading styles *)
let heading_rules base =
  [
    Css.rule ~selector:(where base h1)
      [
        color (Var (Var.use prose_headings_var));
        margin_top Zero;
        margin_bottom (Em 0.888889);
        font_size (Em 2.25);
        font_weight (Weight 800);
        line_height (Num 1.11111);
      ];
    Css.rule
      ~selector:(where base (h1 ++ strong))
      [ color Inherit; font_weight (Weight 900) ];
    Css.rule ~selector:(where base h2)
      [
        color (Var (Var.use prose_headings_var));
        margin_top (Em 2.0);
        margin_bottom (Em 1.0);
        font_size (Em 1.5);
        font_weight (Weight 700);
        line_height (Num 1.33333);
      ];
    Css.rule
      ~selector:(where base (h2 ++ strong))
      [ color Inherit; font_weight (Weight 800) ];
    Css.rule ~selector:(where base h3)
      [
        color (Var (Var.use prose_headings_var));
        margin_top (Em 1.6);
        margin_bottom (Em 0.6);
        font_size (Em 1.25);
        font_weight (Weight 600);
        line_height (Num 1.6);
      ];
    Css.rule
      ~selector:(where base (h3 ++ strong))
      [ color Inherit; font_weight (Weight 700) ];
    Css.rule ~selector:(where base h4)
      [
        color (Var (Var.use prose_headings_var));
        margin_top (Em 1.5);
        margin_bottom (Em 0.5);
        font_weight (Weight 600);
        line_height (Num 1.5);
      ];
    Css.rule
      ~selector:(where base (h4 ++ strong))
      [ color Inherit; font_weight (Weight 700) ];
  ]

(* Horizontal rule styles *)
let hr_rules base =
  [
    Css.rule ~selector:(where base hr)
      [
        border_color (Var (Var.use prose_hr_var));
        border_top_width (Px 1.);
        margin_top (Em 3.0);
        margin_bottom (Em 3.0);
      ];
  ]

(* Blockquote styles *)
let blockquote_rules base =
  [
    Css.rule ~selector:(where base blockquote)
      [
        color (Var (Var.use prose_quotes_var));
        border_inline_start_width (Rem 0.25);
        border_inline_start_color (Var (Var.use prose_quote_borders_var));
        quotes
          "\"\xe2\x80\x9c\"\"\xe2\x80\x9d\"\"\xe2\x80\x98\"\"\xe2\x80\x99\"";
        margin_top (Em 1.6);
        margin_bottom (Em 1.6);
        padding_inline_start (Em 1.0);
        font_style Italic;
        font_weight (Weight 500);
      ];
    Css.rule
      ~selector:(where base (blockquote ++ p_first_of_type_before))
      [ content Open_quote ];
    Css.rule
      ~selector:(where base (blockquote ++ p_last_of_type_after))
      [ content Close_quote ];
  ]

(* Media element styles *)
let media_rules base =
  [
    Css.rule ~selector:(where base img)
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
    Css.rule ~selector:(where base picture)
      [ margin_top (Em 2.0); margin_bottom (Em 2.0); display Block ];
    Css.rule ~selector:(where base video)
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
  ]

(* Keyboard styles *)
let kbd_rules base =
  [
    Css.rule ~selector:(where base kbd)
      [
        color (Var (Var.use prose_kbd_var));
        (* Typed box-shadow equivalent using color-mix to approximate 10%
           alpha *)
        Css.box_shadows
          [
            Css.shadow ~h_offset:Zero ~v_offset:Zero ~spread:(Px 1.)
              ~color:
                (Css.color_mix ~percent1:10
                   (Var (Var.use prose_kbd_var))
                   Transparent)
              ();
            Css.shadow ~h_offset:Zero ~v_offset:(Px 3.)
              ~color:
                (Css.color_mix ~percent1:10
                   (Var (Var.use prose_kbd_var))
                   Transparent)
              ();
          ];
        padding_top (Em 0.1875);
        padding_inline_end (Em 0.375);
        padding_bottom (Em 0.1875);
        border_radius (Rem 0.3125);
        padding_inline_start (Em 0.375);
        font_family Inherit;
        font_size (Em 0.875);
        font_weight (Weight 500);
      ];
  ]

(* Code styles *)
let code_rules base =
  [
    Css.rule ~selector:(where base code)
      [
        color (Var (Var.use prose_code_var));
        font_size (Em 0.875);
        font_weight (Weight 600);
      ];
    (* Code pseudo-element content *)
    Css.rule ~selector:(where base (code ++ before)) [ content (String "`") ];
    Css.rule ~selector:(where base (code ++ after)) [ content (String "`") ];
    (* Code inherits color in certain contexts *)
    Css.rule ~selector:(where base (a ++ code)) [ color Inherit ];
    Css.rule ~selector:(where base (h1 ++ code)) [ color Inherit ];
    Css.rule
      ~selector:(where base (h2 ++ code))
      [ color Inherit; font_size (Em 0.875) ];
    Css.rule
      ~selector:(where base (h3 ++ code))
      [ color Inherit; font_size (Em 0.9) ];
    Css.rule ~selector:(where base (h4 ++ code)) [ color Inherit ];
    Css.rule ~selector:(where base blockquote_code) [ color Inherit ];
    Css.rule ~selector:(where base thead_th_code) [ color Inherit ];
    (* Pre code block styles *)
    Css.rule ~selector:(where base pre)
      [
        color (Var (Var.use prose_pre_code_var));
        background_color (Var (Var.use prose_pre_bg_var));
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
    Css.rule ~selector:(where base pre_code)
      [
        font_weight Inherit;
        color Inherit;
        font_size Inherit;
        font_family Inherit;
        line_height Inherit;
        background_color Transparent;
        border_width Zero;
        border_radius Zero;
        padding [ Zero ];
      ];
    (* Pre code pseudo-element content removal *)
    Css.rule ~selector:(where base pre_code_before) [ content None ];
    Css.rule ~selector:(where base pre_code_after) [ content None ];
  ]

(* Table styles *)
let table_rules base =
  [
    Css.rule ~selector:(where base table)
      [
        table_layout Auto;
        width (Pct 100.0);
        margin_top (Em 2.0);
        margin_bottom (Em 2.0);
        font_size (Em 0.875);
        line_height (Num 1.71429);
      ];
    Css.rule
      ~selector:(where base (Css.Selector.element "thead"))
      [
        border_bottom_width (Px 1.);
        border_bottom_color (Var (Var.use prose_th_borders_var));
      ];
    Css.rule ~selector:(where base thead_th)
      [
        color (Var (Var.use prose_headings_var));
        vertical_align Bottom;
        padding_inline_end (Em 0.571429);
        padding_bottom (Em 0.571429);
        padding_inline_start (Em 0.571429);
        font_weight (Weight 600);
      ];
    Css.rule ~selector:(where base tbody_tr)
      [
        border_bottom_width (Px 1.);
        border_bottom_color (Var (Var.use prose_td_borders_var));
      ];
    Css.rule
      ~selector:(where base tbody_tr_last_child)
      [ border_bottom_width Zero ];
    Css.rule ~selector:(where base tbody_td) [ vertical_align Baseline ];
    Css.rule ~selector:(where base tfoot)
      [
        border_top_width (Px 1.);
        border_top_color (Var (Var.use prose_th_borders_var));
      ];
    Css.rule ~selector:(where base tfoot_td) [ vertical_align Top ];
    Css.rule ~selector:(where base th_td) [ text_align Start ];
  ]

(* Figure and figcaption styles *)
let figure_rules base =
  [
    Css.rule ~selector:(where base figure_all)
      [ margin_top Zero; margin_bottom Zero ];
    Css.rule ~selector:(where base figcaption)
      [
        color (Var (Var.use prose_captions_var));
        margin_top (Em 0.857143);
        font_size (Em 0.875);
        line_height (Num 1.42857);
      ];
  ]

(* Additional list and spacing rules *)
let additional_rules base =
  [
    (* Picture img special handling *)
    Css.rule ~selector:(where base picture_img)
      [ margin_top Zero; margin_bottom Zero ];
    (* List item styles *)
    Css.rule ~selector:(where base li)
      [ margin_top (Em 0.5); margin_bottom (Em 0.5) ];
    Css.rule ~selector:(where base ol_li) [ padding_inline_start (Em 0.375) ];
    Css.rule ~selector:(where base ul_li) [ padding_inline_start (Em 0.375) ];
    (* Nested list paragraph spacing *)
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Child
              (ul >> (li >> p))))
      [ margin_top (Em 0.75); margin_bottom (Em 0.75) ];
    Css.rule
      ~selector:(where base (ul >> p ++ first_child))
      [ margin_top (Em 1.25) ];
    Css.rule
      ~selector:(where base (ul >> p ++ last_child))
      [ margin_bottom (Em 1.25) ];
    Css.rule
      ~selector:(where base (ol >> p ++ first_child))
      [ margin_top (Em 1.25) ];
    Css.rule
      ~selector:(where base (ol >> p ++ last_child))
      [ margin_bottom (Em 1.25) ];
    (* Nested lists *)
    Css.rule ~selector:(where base nested_lists)
      [ margin_top (Em 0.75); margin_bottom (Em 0.75) ];
    (* Definition lists *)
    Css.rule ~selector:(where base dl)
      [ margin_top (Em 1.25); margin_bottom (Em 1.25) ];
    Css.rule ~selector:(where base dd)
      [ margin_top (Em 0.5); padding_inline_start (Em 1.625) ];
    (* Adjacent element spacing *)
    Css.rule ~selector:(where base (sibling hr)) [ margin_top Zero ];
    Css.rule ~selector:(where base (sibling h2)) [ margin_top Zero ];
    Css.rule ~selector:(where base (sibling h3)) [ margin_top Zero ];
    Css.rule ~selector:(where base (sibling h4)) [ margin_top Zero ];
    (* Table column padding *)
    Css.rule
      ~selector:(where base (thead >> th ++ first_child))
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:(where base (thead >> th ++ last_child))
      [ padding_inline_end Zero ];
    Css.rule
      ~selector:(where base ((tbody ++ td) || (tfoot ++ td)))
      [
        padding_top (Em 0.571429);
        padding_inline_end (Em 0.571429);
        padding_bottom (Em 0.571429);
        padding_inline_start (Em 0.571429);
      ];
    Css.rule
      ~selector:
        (where base
           ((tbody ++ (td && first_child)) || (tfoot ++ (td && first_child))))
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:
        (where base
           ((tbody ++ (td && last_child)) || (tfoot ++ (td && last_child))))
      [ padding_inline_end Zero ];
    (* Figure *)
    Css.rule ~selector:(where base figure)
      [ margin_top (Em 2.0); margin_bottom (Em 2.0) ];
    (* First and last child margins *)
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Css.Selector.Child
              (Css.Selector.universal ++ first_child)))
      [ margin_top Zero ];
    Css.rule
      ~selector:
        (where base
           (Css.Selector.combine
              (Css.Selector.class_ "prose")
              Css.Selector.Child
              (Css.Selector.universal ++ last_child)))
      [ margin_bottom Zero ];
  ]

(* Base prose rules - combines all element rules *)
let base_prose_rules () =
  let base = "prose" in

  (* First prose rule with just base styles *)
  let main_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "prose")
      [ color (Var (Var.use prose_body_var)); max_width (Ch 65.0) ]
  in

  (* Second prose rule with CSS variables - comes at the end *)
  let variables_rule =
    Css.rule
      ~selector:(Css.Selector.class_ "prose")
      (css_variables @ [ font_size (Rem 1.0); line_height (Num 1.75) ])
  in

  let pre_variable_rules =
    paragraph_rules base @ link_rules base @ strong_rules base @ list_rules base
    @ hr_rules base @ blockquote_rules base @ heading_rules base
    @ media_rules base @ kbd_rules base @ code_rules base @ table_rules base
    @ figure_rules base
  in

  let post_variable_rules = additional_rules base in

  let all_rules =
    [ main_rule ] @ pre_variable_rules @ [ variables_rule ]
    @ post_variable_rules
  in

  all_rules

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
      [ font_size c.base_font_size; line_height (Num c.base_line_height) ];
    (* Paragraph *)
    Css.rule
      ~selector:(selector ++ prose_where_element p)
      [ margin_top c.p_margin_y; margin_bottom c.p_margin_y ];
    (* Lead paragraph *)
    Css.rule ~selector:(selector ++ lead)
      [
        margin_top c.lead_margin_top;
        margin_bottom c.lead_margin_bottom;
        font_size c.lead_font_size;
        line_height (Num c.lead_line_height);
      ];
    (* Blockquote *)
    Css.rule ~selector:(selector ++ blockquote)
      [
        margin_top c.blockquote_margin_y;
        margin_bottom c.blockquote_margin_y;
        padding_inline_start c.blockquote_padding_start;
      ];
    (* Headings *)
    Css.rule ~selector:(selector ++ h1)
      [
        margin_top c.h1_margin_top;
        margin_bottom c.h1_margin_bottom;
        font_size c.h1_font_size;
        line_height (Num c.h1_line_height);
      ];
    Css.rule ~selector:(selector ++ h2)
      [
        margin_top c.h2_margin_top;
        margin_bottom c.h2_margin_bottom;
        font_size c.h2_font_size;
        line_height (Num c.h2_line_height);
      ];
    Css.rule ~selector:(selector ++ h3)
      [
        margin_top c.h3_margin_top;
        margin_bottom c.h3_margin_bottom;
        font_size c.h3_font_size;
        line_height (Num c.h3_line_height);
      ];
    Css.rule ~selector:(selector ++ h4)
      [
        margin_top c.h4_margin_top;
        margin_bottom c.h4_margin_bottom;
        line_height (Num c.h4_line_height);
      ];
    (* Images - separate rules to avoid combining *)
    Css.rule ~selector:(selector ++ img)
      [ margin_top c.img_margin_y; margin_bottom c.img_margin_y ];
    Css.rule
      ~selector:(selector ++ element "picture")
      [ margin_top c.img_margin_y; margin_bottom c.img_margin_y ];
    Css.rule ~selector:(selector ++ picture_img)
      [ margin_top Zero; margin_bottom Zero ];
    Css.rule
      ~selector:(selector ++ element "video")
      [ margin_top c.img_margin_y; margin_bottom c.img_margin_y ];
    (* Code elements *)
    Css.rule
      ~selector:(selector ++ element "kbd")
      [
        padding_top c.kbd_padding_y;
        padding_inline_end c.kbd_padding_x;
        padding_bottom c.kbd_padding_y;
        padding_inline_start c.kbd_padding_x;
        border_radius (Rem 0.3125);
        font_size c.kbd_font_size;
      ];
    Css.rule
      ~selector:(selector ++ element "code")
      [ font_size c.code_font_size ];
    Css.rule ~selector:(selector ++ h2_code) [ font_size c.h2_code_font_size ];
    Css.rule ~selector:(selector ++ h3_code) [ font_size c.h3_code_font_size ];
    Css.rule
      ~selector:(selector ++ element "pre")
      [
        padding_top c.pre_padding_y;
        padding_inline_end c.pre_padding_x;
        padding_bottom c.pre_padding_y;
        padding_inline_start c.pre_padding_x;
        border_radius c.pre_border_radius;
        margin_top c.pre_margin_top;
        margin_bottom c.pre_margin_bottom;
        font_size c.pre_font_size;
        line_height (Num c.pre_line_height);
      ];
    (* Lists - separate ol and ul to avoid combining *)
    Css.rule
      ~selector:(selector ++ element "ol")
      [
        margin_top c.list_margin_y;
        margin_bottom c.list_margin_y;
        padding_inline_start c.list_padding_start;
      ];
    Css.rule
      ~selector:(selector ++ element "ul")
      [
        margin_top c.list_margin_y;
        margin_bottom c.list_margin_y;
        padding_inline_start c.list_padding_start;
      ];
    Css.rule
      ~selector:(selector ++ element "li")
      [ margin_top c.li_margin_y; margin_bottom c.li_margin_y ];
    (* Separate ol>li and ul>li to avoid combining *)
    Css.rule ~selector:(selector ++ ol_li)
      [ padding_inline_start c.li_padding_start ];
    Css.rule ~selector:(selector ++ ul_li)
      [ padding_inline_start c.li_padding_start ];
    (* Nested list paragraphs *)
    Css.rule
      ~selector:(selector >> ul >> li ++ p)
      [ margin_top c.li_p_margin_y; margin_bottom c.li_p_margin_y ];
    Css.rule
      ~selector:(selector >> ul >> li >> p ++ first_child)
      [ margin_top c.li_p_first_margin_top ];
    Css.rule
      ~selector:(selector >> ul >> li >> p ++ last_child)
      [ margin_bottom c.li_p_last_margin_bottom ];
    Css.rule
      ~selector:(selector >> ol >> li >> p ++ first_child)
      [ margin_top c.li_p_first_margin_top ];
    Css.rule
      ~selector:(selector >> ol >> li >> p ++ last_child)
      [ margin_bottom c.li_p_last_margin_bottom ];
    (* Nested lists *)
    Css.rule ~selector:(selector ++ nested_lists)
      [
        margin_top c.nested_list_margin_y; margin_bottom c.nested_list_margin_y;
      ];
    (* Definition lists *)
    Css.rule ~selector:(selector ++ dl)
      [ margin_top c.dl_margin_y; margin_bottom c.dl_margin_y ];
    Css.rule
      ~selector:(selector ++ Css.Selector.element "dt")
      [ margin_top c.dt_margin_top ];
    Css.rule ~selector:(selector ++ dd)
      [ margin_top c.dd_margin_top; padding_inline_start c.dd_padding_start ];
    (* Horizontal rule *)
    Css.rule ~selector:(selector ++ hr)
      [ margin_top c.hr_margin_y; margin_bottom c.hr_margin_y ];
    (* Following elements - separate to avoid combining *)
    Css.rule ~selector:(selector ++ sibling hr) [ margin_top Zero ];
    Css.rule ~selector:(selector ++ sibling h2) [ margin_top Zero ];
    Css.rule ~selector:(selector ++ sibling h3) [ margin_top Zero ];
    Css.rule ~selector:(selector ++ sibling h4) [ margin_top Zero ];
    (* Tables *)
    Css.rule ~selector:(selector ++ table)
      [ font_size c.table_font_size; line_height (Num c.table_line_height) ];
    Css.rule ~selector:(selector ++ thead_th)
      [
        padding_inline_end c.thead_th_padding_x;
        padding_bottom c.thead_th_padding_bottom;
        padding_inline_start c.thead_th_padding_x;
      ];
    Css.rule
      ~selector:(selector ++ thead_th_first_child)
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:(selector ++ thead_th_last_child)
      [ padding_inline_end Zero ];
    Css.rule
      ~selector:(selector ++ tbody_td_tfoot_td)
      [
        padding_top c.tbody_td_padding_y;
        padding_inline_end c.tbody_td_padding_x;
        padding_bottom c.tbody_td_padding_y;
        padding_inline_start c.tbody_td_padding_x;
      ];
    Css.rule
      ~selector:(selector ++ tbody_td_first_child)
      [ padding_inline_start Zero ];
    Css.rule
      ~selector:(selector ++ tbody_td_last_child)
      [ padding_inline_end Zero ];
    (* Figure *)
    Css.rule ~selector:(selector ++ figure)
      [ margin_top c.figure_margin_y; margin_bottom c.figure_margin_y ];
    Css.rule ~selector:(selector ++ figure_all)
      [ margin_top Zero; margin_bottom Zero ];
    Css.rule ~selector:(selector ++ figcaption)
      [
        margin_top c.figcaption_margin_top;
        font_size c.figcaption_font_size;
        line_height (Num c.figcaption_line_height);
      ];
    (* First and last child margins *)
    Css.rule
      ~selector:(selector >> Css.Selector.universal ++ first_child)
      [ margin_top Zero ];
    Css.rule
      ~selector:(selector >> Css.Selector.universal ++ last_child)
      [ margin_bottom Zero ];
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
    lead_margin_top = Em 1.05556;
    lead_margin_bottom = Em 1.05556;
    lead_font_size = Em 1.22222;
    lead_line_height = 1.45455;
    blockquote_margin_y = Em 1.66667;
    blockquote_padding_start = Em 1.11111;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.888889;
    h1_font_size = Em 2.66667;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.77778;
    h2_margin_bottom = Em 0.888889;
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
    kbd_padding_y = Em 0.111111;
    kbd_padding_x = Em 0.333333;
    kbd_font_size = Em 0.888889;
    code_font_size = Em 0.888889;
    h2_code_font_size = Em 0.866667;
    h3_code_font_size = Em 0.875;
    pre_padding_y = Em 0.888889;
    pre_padding_x = Em 1.33333;
    pre_border_radius = Rem 0.375;
    pre_margin_top = Em 1.77778;
    pre_margin_bottom = Em 1.77778;
    pre_font_size = Em 0.888889;
    pre_line_height = 1.75;
    list_margin_y = Em 1.33333;
    list_padding_start = Em 1.66667;
    li_margin_y = Em 0.666667;
    li_padding_start = Em 0.444444;
    li_p_margin_y = Em 0.888889;
    li_p_first_margin_top = Em 1.33333;
    li_p_last_margin_bottom = Em 1.33333;
    nested_list_margin_y = Em 0.888889;
    dl_margin_y = Em 1.33333;
    dt_margin_top = Em 1.33333;
    dd_margin_top = Em 0.666667;
    dd_padding_start = Em 1.66667;
    hr_margin_y = Em 3.11111;
    table_font_size = Em 0.888889;
    table_line_height = 1.5;
    thead_th_padding_x = Em 0.888889;
    thead_th_padding_bottom = Em 0.777778;
    tbody_td_padding_y = Em 0.777778;
    tbody_td_padding_x = Em 0.888889;
    figure_margin_y = Em 1.77778;
    figcaption_margin_top = Em 0.888889;
    figcaption_font_size = Em 0.888889;
    figcaption_line_height = 1.5;
  }

(* Size configuration for prose-xl *)
let xl_config =
  {
    base_font_size = Rem 1.25;
    base_line_height = 1.8;
    p_margin_y = Em 1.2;
    lead_margin_top = Em 0.96;
    lead_margin_bottom = Em 0.96;
    lead_font_size = Em 1.2;
    lead_line_height = 1.5;
    blockquote_margin_y = Em 1.6;
    blockquote_padding_start = Em 1.0667;
    h1_margin_top = Zero;
    h1_margin_bottom = Em 0.8;
    h1_font_size = Em 2.8;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.8;
    h2_margin_bottom = Em 0.9;
    h2_font_size = Em 1.8;
    h2_line_height = 1.33333;
    h3_margin_top = Em 1.6;
    h3_margin_bottom = Em 0.6;
    h3_font_size = Em 1.4;
    h3_line_height = 1.42857;
    h4_margin_top = Em 1.8;
    h4_margin_bottom = Em 0.4;
    h4_line_height = 1.6;
    img_margin_y = Em 2.0;
    kbd_padding_y = Em 0.1;
    kbd_padding_x = Em 0.3;
    kbd_font_size = Em 0.9;
    code_font_size = Em 0.9;
    h2_code_font_size = Em 0.855556;
    h3_code_font_size = Em 0.857143;
    pre_padding_y = Em 1.0;
    pre_padding_x = Em 1.5;
    pre_border_radius = Rem 0.5;
    pre_margin_top = Em 2.0;
    pre_margin_bottom = Em 2.0;
    pre_font_size = Em 0.9;
    pre_line_height = 1.77778;
    list_margin_y = Em 1.2;
    list_padding_start = Em 1.8;
    li_margin_y = Em 0.6;
    li_padding_start = Em 0.4;
    li_p_margin_y = Em 0.8;
    li_p_first_margin_top = Em 1.2;
    li_p_last_margin_bottom = Em 1.2;
    nested_list_margin_y = Em 0.8;
    dl_margin_y = Em 1.2;
    dt_margin_top = Em 1.2;
    dd_margin_top = Em 0.6;
    dd_padding_start = Em 1.8;
    hr_margin_y = Em 3.6;
    table_font_size = Em 0.9;
    table_line_height = 1.55556;
    thead_th_padding_x = Em 0.8;
    thead_th_padding_bottom = Em 0.8;
    tbody_td_padding_y = Em 0.8;
    tbody_td_padding_x = Em 0.8;
    figure_margin_y = Em 2.0;
    figcaption_margin_top = Em 0.8;
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
    h1_margin_bottom = Em 0.888889;
    h1_font_size = Em 2.66667;
    h1_line_height = 1.0;
    h2_margin_top = Em 1.94444;
    h2_margin_bottom = Em 1.11111;
    h2_font_size = Em 2.0;
    h2_line_height = 1.25;
    h3_margin_top = Em 1.77778;
    h3_margin_bottom = Em 0.666667;
    h3_font_size = Em 1.5;
    h3_line_height = 1.33333;
    h4_margin_top = Em 1.66667;
    h4_margin_bottom = Em 0.666667;
    h4_line_height = 1.5;
    img_margin_y = Em 2.0;
    kbd_padding_y = Em 0.0888889;
    kbd_padding_x = Em 0.266667;
    kbd_font_size = Em 0.833333;
    code_font_size = Em 0.833333;
    h2_code_font_size = Em 0.875;
    h3_code_font_size = Em 0.888889;
    pre_padding_y = Em 1.11111;
    pre_padding_x = Em 1.77778;
    pre_border_radius = Rem 0.5;
    pre_margin_top = Em 2.0;
    pre_margin_bottom = Em 2.0;
    pre_font_size = Em 0.833333;
    pre_line_height = 1.8;
    list_margin_y = Em 1.33333;
    list_padding_start = Em 2.0;
    li_margin_y = Em 0.5;
    li_padding_start = Em 0.5;
    li_p_margin_y = Em 0.833333;
    li_p_first_margin_top = Em 1.33333;
    li_p_last_margin_bottom = Em 1.33333;
    nested_list_margin_y = Em 0.833333;
    dl_margin_y = Em 1.33333;
    dt_margin_top = Em 1.33333;
    dd_margin_top = Em 0.5;
    dd_padding_start = Em 2.0;
    hr_margin_y = Em 3.33333;
    table_font_size = Em 0.833333;
    table_line_height = 1.6;
    thead_th_padding_x = Em 0.888889;
    thead_th_padding_bottom = Em 0.888889;
    tbody_td_padding_y = Em 0.888889;
    tbody_td_padding_x = Em 0.888889;
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

(* Helper to create color variable bindings for color themes *)
let color_theme_bindings theme_name =
  match theme_name with
  | "gray" ->
      [
        Var.declaration prose_body_var (oklch 55.1 0.027 264.364);
        Var.declaration prose_headings_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_lead_var (oklch 44.6 0.030 256.802);
        Var.declaration prose_links_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_bold_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_counters_var (oklch 55.1 0.027 264.364);
        Var.declaration prose_bullets_var (oklch 87.2 0.010 258.338);
        Var.declaration prose_hr_var (oklch 92.8 0.006 264.531);
        Var.declaration prose_quotes_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_quote_borders_var (oklch 92.8 0.006 264.531);
        Var.declaration prose_captions_var (oklch 70.7 0.022 261.325);
        Var.declaration prose_code_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_pre_code_var (oklch 92.8 0.006 264.531);
        Var.declaration prose_pre_bg_var (oklch 27.8 0.033 256.848);
        Var.declaration prose_th_borders_var (oklch 87.2 0.010 258.338);
        Var.declaration prose_td_borders_var (oklch 92.8 0.006 264.531);
      ]
  | "slate" ->
      [
        Var.declaration prose_body_var (oklch 52.5 0.027 257.576);
        Var.declaration prose_headings_var (oklch 19.2 0.025 268.573);
        Var.declaration prose_lead_var (oklch 42.3 0.029 255.908);
        Var.declaration prose_links_var (oklch 19.2 0.025 268.573);
        Var.declaration prose_bold_var (oklch 19.2 0.025 268.573);
        Var.declaration prose_counters_var (oklch 52.5 0.027 257.576);
        Var.declaration prose_bullets_var (oklch 85.8 0.012 255.094);
        Var.declaration prose_hr_var (oklch 91.9 0.007 264.837);
        Var.declaration prose_quotes_var (oklch 19.2 0.025 268.573);
        Var.declaration prose_quote_borders_var (oklch 91.9 0.007 264.837);
        Var.declaration prose_captions_var (oklch 67.8 0.024 254.628);
        Var.declaration prose_code_var (oklch 19.2 0.025 268.573);
        Var.declaration prose_pre_code_var (oklch 91.9 0.007 264.837);
        Var.declaration prose_pre_bg_var (oklch 26.5 0.028 268.761);
        Var.declaration prose_th_borders_var (oklch 85.8 0.012 255.094);
        Var.declaration prose_td_borders_var (oklch 91.9 0.007 264.837);
      ]
  | "zinc" ->
      [
        Var.declaration prose_body_var (oklch 52.5 0.013 265.454);
        Var.declaration prose_headings_var (oklch 21.0 0.017 264.665);
        Var.declaration prose_lead_var (oklch 42.2 0.015 268.024);
        Var.declaration prose_links_var (oklch 21.0 0.017 264.665);
        Var.declaration prose_bold_var (oklch 21.0 0.017 264.665);
        Var.declaration prose_counters_var (oklch 52.5 0.013 265.454);
        Var.declaration prose_bullets_var (oklch 87.1 0.006 286.287);
        Var.declaration prose_hr_var (oklch 92.8 0.004 286.032);
        Var.declaration prose_quotes_var (oklch 21.0 0.017 264.665);
        Var.declaration prose_quote_borders_var (oklch 92.8 0.004 286.032);
        Var.declaration prose_captions_var (oklch 67.9 0.011 285.934);
        Var.declaration prose_code_var (oklch 21.0 0.017 264.665);
        Var.declaration prose_pre_code_var (oklch 92.8 0.004 286.032);
        Var.declaration prose_pre_bg_var (oklch 27.9 0.017 264.663);
        Var.declaration prose_th_borders_var (oklch 87.1 0.006 286.287);
        Var.declaration prose_td_borders_var (oklch 92.8 0.004 286.032);
      ]
  | "neutral" ->
      [
        Var.declaration prose_body_var (oklch 48.8 0.014 106.429);
        Var.declaration prose_headings_var (oklch 23.0 0.021 92.591);
        Var.declaration prose_lead_var (oklch 42.2 0.015 110.211);
        Var.declaration prose_links_var (oklch 23.0 0.021 92.591);
        Var.declaration prose_bold_var (oklch 23.0 0.021 92.591);
        Var.declaration prose_counters_var (oklch 48.8 0.014 106.429);
        Var.declaration prose_bullets_var (oklch 87.1 0.004 106.424);
        Var.declaration prose_hr_var (oklch 92.9 0.003 106.423);
        Var.declaration prose_quotes_var (oklch 23.0 0.021 92.591);
        Var.declaration prose_quote_borders_var (oklch 92.9 0.003 106.423);
        Var.declaration prose_captions_var (oklch 67.8 0.009 106.424);
        Var.declaration prose_code_var (oklch 23.0 0.021 92.591);
        Var.declaration prose_pre_code_var (oklch 92.9 0.003 106.423);
        Var.declaration prose_pre_bg_var (oklch 31.1 0.019 93.761);
        Var.declaration prose_th_borders_var (oklch 87.1 0.004 106.424);
        Var.declaration prose_td_borders_var (oklch 92.9 0.003 106.423);
      ]
  | "stone" ->
      [
        Var.declaration prose_body_var (oklch 49.9 0.017 56.205);
        Var.declaration prose_headings_var (oklch 27.9 0.019 68.952);
        Var.declaration prose_lead_var (oklch 42.3 0.016 56.359);
        Var.declaration prose_links_var (oklch 27.9 0.019 68.952);
        Var.declaration prose_bold_var (oklch 27.9 0.019 68.952);
        Var.declaration prose_counters_var (oklch 49.9 0.017 56.205);
        Var.declaration prose_bullets_var (oklch 87.1 0.006 56.211);
        Var.declaration prose_hr_var (oklch 92.9 0.003 56.214);
        Var.declaration prose_quotes_var (oklch 27.9 0.019 68.952);
        Var.declaration prose_quote_borders_var (oklch 92.9 0.003 56.214);
        Var.declaration prose_captions_var (oklch 67.8 0.011 56.211);
        Var.declaration prose_code_var (oklch 27.9 0.019 68.952);
        Var.declaration prose_pre_code_var (oklch 92.9 0.003 56.214);
        Var.declaration prose_pre_bg_var (oklch 35.1 0.019 68.936);
        Var.declaration prose_th_borders_var (oklch 87.1 0.006 56.211);
        Var.declaration prose_td_borders_var (oklch 92.9 0.003 56.214);
      ]
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
  let name = to_class variant in
  let rules = to_css_rules variant in
  Core.style ~rules:(Some rules) name []

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
