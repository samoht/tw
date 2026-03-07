(** Feature: Layout demo

    Shows comprehensive Flexbox and Grid utilities with responsive examples,
    alignment options, spacing, and real-world layout patterns. *)

open Tw_html

(* Page header *)
let page_header =
  header
    ~tw:Tw.[ bg_white; shadow_sm; dark [ bg gray 900 ] ]
    [
      div
        ~tw:Tw.[ max_w_6xl; mx_auto; px 4; py 4 ]
        [
          h1
            ~tw:Tw.[ text_2xl; font_bold; text gray 900; dark [ text_white ] ]
            [ txt "Layout Demo" ];
        ];
    ]

(* Shared demo styles *)
let demo_label =
  Tw.[ text_sm; font_semibold; text gray 700; mb 2; dark [ text gray 300 ] ]

let demo_bg = Tw.[ p 4; bg gray 100; rounded_lg; dark [ bg gray 700 ] ]

(* Helper for demo boxes *)
let box ?(extra = []) content =
  div
    ~tw:
      Tw.(
        [
          p 4;
          bg blue 100;
          rounded_lg;
          text_center;
          font_medium;
          text blue 800;
          dark [ bg blue 900; text blue 100 ];
        ]
        @ extra)
    [ txt content ]

let numbered_box n = box (string_of_int n)

(* Flexbox Direction Section *)
let flex_direction_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
    (List.map
       (fun (name, dir_style) ->
         div
           [
             h3 ~tw:demo_label [ txt name ];
             div
               ~tw:Tw.([ flex; dir_style; gap 2 ] @ demo_bg)
               [ numbered_box 1; numbered_box 2; numbered_box 3 ];
           ])
       [
         ("flex-row (default)", Tw.flex_row);
         ("flex-row-reverse", Tw.flex_row_reverse);
         ("flex-col", Tw.flex_col);
         ("flex-col-reverse", Tw.flex_col_reverse);
       ])

let flex_direction_section =
  section
    ~at:[ At.v "aria-labelledby" "flex-dir-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "flex-dir-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Flex Direction" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt "Control the direction of flex items with row and column layouts.";
        ];
      flex_direction_demos;
    ]

(* Justify Content Section *)
let justify_demos =
  div
    ~tw:Tw.[ flex; flex_col; gap 4 ]
    (List.map
       (fun (name, justify_style) ->
         div
           [
             h3 ~tw:demo_label [ txt name ];
             div
               ~tw:Tw.([ flex; justify_style; gap 2 ] @ demo_bg)
               [ box "A"; box "B"; box "C" ];
           ])
       [
         ("justify-start", Tw.justify_start);
         ("justify-center", Tw.justify_center);
         ("justify-end", Tw.justify_end);
         ("justify-between", Tw.justify_between);
         ("justify-around", Tw.justify_around);
         ("justify-evenly", Tw.justify_evenly);
       ])

let justify_section =
  section
    ~at:[ At.v "aria-labelledby" "justify-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "justify-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Justify Content" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Control how items are positioned along the main axis." ];
      justify_demos;
    ]

(* Align Items Section *)
let align_demos =
  div
    ~tw:Tw.[ grid; grid_cols 1; gap 4; md [ grid_cols 2 ]; lg [ grid_cols 3 ] ]
    (List.map
       (fun (name, align_style) ->
         div
           [
             h3 ~tw:demo_label [ txt name ];
             div
               ~tw:Tw.([ flex; align_style; gap 2; h 24 ] @ demo_bg)
               [
                 box ~extra:Tw.[ h 8 ] "A";
                 box ~extra:Tw.[ h 12 ] "B";
                 box ~extra:Tw.[ h 6 ] "C";
               ];
           ])
       [
         ("items-start", Tw.items_start);
         ("items-center", Tw.items_center);
         ("items-end", Tw.items_end);
         ("items-baseline", Tw.items_baseline);
         ("items-stretch", Tw.items_stretch);
       ])

let align_section =
  section
    ~at:[ At.v "aria-labelledby" "align-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "align-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Align Items" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Control how items are positioned along the cross axis." ];
      align_demos;
    ]

(* Flex Wrap Section *)
let wrap_demo =
  div
    [
      h3 ~tw:demo_label [ txt "flex-wrap" ];
      div
        ~tw:Tw.([ flex; flex_wrap; gap 2 ] @ demo_bg)
        (List.init 8 (fun i -> box ~extra:Tw.[ w 24 ] (string_of_int (i + 1))));
    ]

let nowrap_demo =
  div
    [
      h3 ~tw:demo_label [ txt "flex-nowrap (overflow)" ];
      div
        ~tw:Tw.([ flex; flex_nowrap; gap 2; overflow_x_auto ] @ demo_bg)
        (List.init 8 (fun i ->
             box ~extra:Tw.[ min_w 20 ] (string_of_int (i + 1))));
    ]

let wrap_section =
  section
    ~at:[ At.v "aria-labelledby" "wrap-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "wrap-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Flex Wrap" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Control whether flex items wrap to new lines." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; lg [ grid_cols 2 ] ]
        [ wrap_demo; nowrap_demo ];
    ]

(* Grid Columns Section *)
let grid_cols_demos =
  div
    ~tw:Tw.[ flex; flex_col; gap 6 ]
    (List.map
       (fun (name, cols, count) ->
         div
           [
             h3 ~tw:demo_label [ txt name ];
             div
               ~tw:Tw.([ grid; grid_cols cols; gap 2 ] @ demo_bg)
               (List.init count (fun i -> numbered_box (i + 1)));
           ])
       [ ("grid-cols-2", 2, 4); ("grid-cols-3", 3, 6); ("grid-cols-4", 4, 8) ])

let grid_cols_section =
  section
    ~at:[ At.v "aria-labelledby" "grid-cols-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "grid-cols-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Grid Columns" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Create multi-column layouts with CSS Grid." ];
      grid_cols_demos;
    ]

(* Grid Span Section *)
let grid_span_section =
  section
    ~at:[ At.v "aria-labelledby" "grid-span-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "grid-span-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Grid Column & Row Span" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Make items span multiple columns or rows." ];
      div
        ~tw:Tw.([ grid; grid_cols 4; gap 2 ] @ demo_bg)
        [
          box ~extra:Tw.[ col_span 2 ] "col-span-2";
          box "1";
          box "2";
          box "3";
          box ~extra:Tw.[ col_span 3 ] "col-span-3";
          box ~extra:Tw.[ col_span 4 ] "span-all-4";
        ];
    ]

(* Responsive Grid Section *)
let responsive_grid_section =
  section
    ~at:[ At.v "aria-labelledby" "responsive-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "responsive-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Responsive Grid" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [
          txt
            "Resize your browser to see the grid adapt: 1 column on mobile, 2 \
             on tablet, 4 on desktop.";
        ];
      div
        ~tw:
          Tw.
            [ grid; grid_cols 1; sm [ grid_cols 2 ]; lg [ grid_cols 4 ]; gap 4 ]
        [
          box "Card 1";
          box "Card 2";
          box "Card 3";
          box "Card 4";
          box "Card 5";
          box "Card 6";
          box "Card 7";
          box "Card 8";
        ];
    ]

(* Gap Utilities Section *)
let uniform_gap_demos =
  List.map
    (fun (name, gap_val) ->
      div
        [
          h3 ~tw:demo_label [ txt name ];
          div
            ~tw:Tw.([ grid; grid_cols 3; gap_val ] @ demo_bg)
            (List.init 6 (fun i -> numbered_box (i + 1)));
        ])
    [ ("gap-2 (0.5rem)", Tw.gap 2); ("gap-4 (1rem)", Tw.gap 4) ]

let asymmetric_gap_demo =
  div
    ~tw:Tw.[ col_span 2; md [ col_span 2 ] ]
    [
      h3 ~tw:demo_label [ txt "gap-x-8 gap-y-2 (asymmetric)" ];
      div
        ~tw:Tw.([ grid; grid_cols 4; gap_x 8; gap_y 2 ] @ demo_bg)
        (List.init 8 (fun i -> numbered_box (i + 1)));
    ]

let gap_section =
  section
    ~at:[ At.v "aria-labelledby" "gap-heading" ]
    ~tw:Tw.[ bg_white; p 6; rounded_xl; shadow_sm; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "gap-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Gap Utilities" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "Control spacing between grid and flex items." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; gap 6; md [ grid_cols 2 ] ]
        (uniform_gap_demos @ [ asymmetric_gap_demo ]);
    ]

(* Real-world Layout Pattern *)
let sidebar_nav =
  div
    ~tw:
      Tw.
        [
          lg [ col_span 1 ];
          bg_white;
          rounded_lg;
          p 4;
          shadow_sm;
          dark [ bg gray 700 ];
        ]
    [
      h3
        ~tw:Tw.[ font_semibold; text gray 800; mb 4; dark [ text_white ] ]
        [ txt "Navigation" ];
      div
        ~tw:Tw.[ flex; flex_col; gap 2 ]
        (List.map
           (fun label ->
             div
               ~tw:
                 Tw.
                   [
                     px 3;
                     py 2;
                     rounded;
                     text gray 600;
                     hover [ bg gray 100 ];
                     dark [ text gray 300; hover [ bg gray 600 ] ];
                   ]
               [ txt label ])
           [ "Dashboard"; "Analytics"; "Reports"; "Settings" ]);
    ]

let stats_row =
  div
    ~tw:Tw.[ grid; grid_cols 1; sm [ grid_cols 3 ]; gap 4 ]
    (List.map
       (fun (label, value) ->
         div
           ~tw:Tw.[ bg_white; rounded_lg; p 4; shadow_sm; dark [ bg gray 700 ] ]
           [
             p
               ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
               [ txt label ];
             p
               ~tw:
                 Tw.[ text_2xl; font_bold; text gray 900; dark [ text_white ] ]
               [ txt value ];
           ])
       [ ("Users", "1,234"); ("Revenue", "$45.2k"); ("Orders", "892") ])

let content_area =
  div
    ~tw:
      Tw.[ flex_1; bg_white; rounded_lg; p 6; shadow_sm; dark [ bg gray 700 ] ]
    [
      h3
        ~tw:Tw.[ font_semibold; text gray 800; mb 4; dark [ text_white ] ]
        [ txt "Content Area" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ] ]
        [
          txt
            "This area uses flex-1 to fill the remaining space. The sidebar \
             has a set width on large screens, while the main content area \
             expands to fill available space.";
        ];
    ]

let layout_pattern_section =
  section
    ~at:[ At.v "aria-labelledby" "pattern-heading" ]
    ~tw:Tw.[ bg blue 50; p 6; rounded_xl; dark [ bg gray 800 ] ]
    [
      h2
        ~at:[ At.id "pattern-heading" ]
        ~tw:Tw.[ text_xl; font_bold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Real-World Layout: Dashboard" ];
      p
        ~tw:Tw.[ text gray 600; dark [ text gray 300 ]; mb 6 ]
        [ txt "A typical dashboard layout using grid and flex utilities." ];
      div
        ~tw:Tw.[ grid; grid_cols 1; lg [ grid_cols 4 ]; gap 4 ]
        [
          sidebar_nav;
          div
            ~tw:Tw.[ lg [ col_span 3 ]; flex; flex_col; gap 4 ]
            [ stats_row; content_area ];
        ];
    ]

(* Main page *)
let page_intro =
  div
    ~tw:Tw.[ text_center; py 8 ]
    [
      h1
        ~tw:
          Tw.
            [
              text_3xl;
              md [ text_4xl ];
              font_bold;
              text gray 900;
              mb 4;
              dark [ text_white ];
            ]
        [ txt "Layout Utilities" ];
      p
        ~tw:
          Tw.
            [
              text_lg; text gray 600; max_w_2xl; mx_auto; dark [ text gray 400 ];
            ]
        [
          txt
            "Build complex responsive layouts with Flexbox and Grid utilities. \
             See direction, alignment, wrapping, and real-world patterns.";
        ];
    ]

let page_view =
  page ~title:"Layout Demo" ~tw_css:"layout.css" []
    [
      page_header;
      main
        ~at:[ At.id "main-content" ]
        ~tw:Tw.[ max_w_6xl; mx_auto; p 6; md [ p 8 ] ]
        [
          div
            ~tw:Tw.[ flex; flex_col; gap 8 ]
            [
              page_intro;
              flex_direction_section;
              justify_section;
              align_section;
              wrap_section;
              grid_cols_section;
              grid_span_section;
              responsive_grid_section;
              gap_section;
              layout_pattern_section;
            ];
        ];
    ]

let () =
  let html_str = html page_view in
  let css_file, css_stylesheet = css page_view in
  let css_str = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_str;
  close_out oc_html;
  let oc_css = open_out css_file in
  output_string oc_css css_str;
  close_out oc_css;
  ()
