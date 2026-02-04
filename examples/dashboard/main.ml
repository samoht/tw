(** Component: Analytics Dashboard

    Demonstrates a modern analytics dashboard with:
    - Sidebar navigation with icons
    - Stats cards with gradients and trends
    - Charts placeholder with data visualization styling
    - Responsive layout with mobile-first design
    - Dark mode support
    - Interactive hover states and transitions *)

open Tw_html

(* ========== Icon Components (SVG as text placeholders) ========== *)

let icon_home =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "H" ]

let icon_chart =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "C" ]

let icon_users =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "U" ]

let icon_settings =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "S" ]

let icon_bell =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "N" ]

let icon_search =
  span ~tw:Tw.[ w 5; h 5; flex; items_center; justify_center ] [ txt "?" ]

(* ========== Sidebar Navigation ========== *)

let nav_item ~active ~icon label =
  let base_styles =
    Tw.
      [
        flex;
        items_center;
        gap 3;
        px 4;
        py 3;
        rounded_lg;
        text_sm;
        font_medium;
        transition_colors;
        duration 200;
        cursor_pointer;
      ]
  in
  let state_styles =
    if active then Tw.[ bg blue 600; text_white; dark [ bg blue 500 ] ]
    else
      Tw.
        [
          text gray 600;
          hover [ bg gray 100; text gray 900 ];
          dark [ text gray 300; hover [ bg gray 700; text_white ] ];
        ]
  in
  div ~tw:(base_styles @ state_styles) [ icon; txt label ]

let sidebar =
  aside
    ~tw:
      Tw.
        [
          (* Hidden on mobile, shown on lg screens *)
          hidden;
          lg [ flex; w 64 ];
          min_h_screen;
          bg_white;
          border_r;
          border_color gray 200;
          flex_col;
          dark [ bg gray 800; border_color gray 700 ];
        ]
    [
      (* Logo *)
      div
        ~tw:
          Tw.
            [
              p 6;
              border_b;
              border_color gray 200;
              dark [ border_color gray 700 ];
            ]
        [
          div
            ~tw:Tw.[ flex; items_center; gap 3 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      w 10;
                      h 10;
                      rounded_lg;
                      bg_gradient_to Bottom_right;
                      from_color ~shade:500 blue;
                      to_color ~shade:600 indigo;
                      flex;
                      items_center;
                      justify_center;
                      text_white;
                      font_bold;
                    ]
                [ txt "D" ];
              span
                ~tw:
                  Tw.[ text_xl; font_bold; text gray 900; dark [ text_white ] ]
                [ txt "Dashboard" ];
            ];
        ];
      (* Navigation *)
      nav
        ~tw:Tw.[ flex_1; p 4; space_y 2 ]
        [
          nav_item ~active:true ~icon:icon_home "Overview";
          nav_item ~active:false ~icon:icon_chart "Analytics";
          nav_item ~active:false ~icon:icon_users "Customers";
          nav_item ~active:false ~icon:icon_settings "Settings";
        ];
      (* User profile at bottom *)
      div
        ~tw:
          Tw.
            [
              p 4;
              border_t;
              border_color gray 200;
              dark [ border_color gray 700 ];
            ]
        [
          div
            ~tw:Tw.[ flex; items_center; gap 3 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      w 10;
                      h 10;
                      rounded_full;
                      bg gray 300;
                      flex;
                      items_center;
                      justify_center;
                      text gray 600;
                      font_medium;
                    ]
                [ txt "JD" ];
              div
                ~tw:Tw.[ flex_1 ]
                [
                  p
                    ~tw:
                      Tw.
                        [
                          text_sm;
                          font_medium;
                          text gray 900;
                          dark [ text_white ];
                        ]
                    [ txt "John Doe" ];
                  p
                    ~tw:Tw.[ text_xs; text gray 500; dark [ text gray 400 ] ]
                    [ txt "Admin" ];
                ];
            ];
        ];
    ]

(* ========== Header ========== *)

let icon_menu =
  span ~tw:Tw.[ w 6; h 6; flex; items_center; justify_center ] [ txt "☰" ]

let mobile_logo =
  div
    ~tw:Tw.[ flex; items_center; gap 2; lg [ hidden ] ]
    [
      div
        ~tw:
          Tw.
            [
              w 8;
              h 8;
              rounded_lg;
              bg_gradient_to Bottom_right;
              from_color ~shade:500 blue;
              to_color ~shade:600 indigo;
              flex;
              items_center;
              justify_center;
              text_white;
              font_bold;
              text_sm;
            ]
        [ txt "D" ];
      span
        ~tw:Tw.[ font_bold; text gray 900; dark [ text_white ] ]
        [ txt "Dashboard" ];
    ]

let header_section =
  header
    ~tw:
      Tw.
        [
          bg_white;
          border_b;
          border_color gray 200;
          px 4;
          py 3;
          md [ px 6; py 4 ];
          dark [ bg gray 800; border_color gray 700 ];
        ]
    [
      div
        ~tw:Tw.[ flex; items_center; justify_between; gap 4 ]
        [
          (* Mobile: menu button and logo *)
          div
            ~tw:Tw.[ flex; items_center; gap 3 ]
            [
              (* Menu button - only on mobile *)
              div
                ~tw:
                  Tw.
                    [
                      lg [ hidden ];
                      p 2;
                      rounded_lg;
                      hover [ bg gray 100 ];
                      cursor_pointer;
                      dark [ hover [ bg gray 700 ] ];
                    ]
                [ icon_menu ];
              mobile_logo;
            ];
          (* Search bar - hidden on small screens, shown from md *)
          div
            ~tw:
              Tw.
                [
                  hidden;
                  md [ flex ];
                  items_center;
                  gap 3;
                  bg gray 100;
                  rounded_lg;
                  px 4;
                  py 2;
                  flex_1;
                  max_w_md;
                  dark [ bg gray 700 ];
                ]
            [
              icon_search;
              span
                ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
                [ txt "Search..." ];
            ];
          (* Right side actions *)
          div
            ~tw:Tw.[ flex; items_center; gap 2; md [ gap 4 ] ]
            [
              (* Search icon on mobile *)
              div
                ~tw:
                  Tw.
                    [
                      md [ hidden ];
                      p 2;
                      rounded_lg;
                      hover [ bg gray 100 ];
                      cursor_pointer;
                      dark [ hover [ bg gray 700 ] ];
                    ]
                [ icon_search ];
              (* Notifications *)
              div
                ~tw:
                  Tw.
                    [
                      relative;
                      p 2;
                      rounded_lg;
                      hover [ bg gray 100 ];
                      cursor_pointer;
                      transition_colors;
                      dark [ hover [ bg gray 700 ] ];
                    ]
                [
                  icon_bell;
                  (* Notification badge *)
                  span
                    ~tw:
                      Tw.
                        [
                          absolute;
                          top 0;
                          right 0;
                          w 5;
                          h 5;
                          bg red 500;
                          text_white;
                          text_xs;
                          font_bold;
                          rounded_full;
                          flex;
                          items_center;
                          justify_center;
                        ]
                    [ txt "3" ];
                ];
            ];
        ];
    ]

(* ========== Stats Cards ========== *)

let stat_card ~title ~value ~change ~trend ~gradient_from ~gradient_to =
  div
    ~tw:
      Tw.
        [
          bg_white;
          rounded_xl;
          shadow_sm;
          p 6;
          relative;
          overflow_hidden;
          transition_all;
          duration 300;
          hover [ shadow_md; transform; scale 102 ];
          dark [ bg gray 800 ];
        ]
    [
      (* Gradient accent *)
      div
        ~tw:
          Tw.
            [
              absolute;
              top 0;
              right 0;
              w 32;
              h 32;
              bg_gradient_to Bottom_left;
              from_color ~shade:100 gradient_from;
              to_color ~shade:50 gradient_to;
              rounded_full;
              opacity 50;
              dark [ opacity 20 ];
            ]
        [];
      (* Content *)
      div
        ~tw:Tw.[ relative ]
        [
          p
            ~tw:
              Tw.[ text_sm; font_medium; text gray 500; dark [ text gray 400 ] ]
            [ txt title ];
          p
            ~tw:
              Tw.
                [
                  text_3xl; font_bold; text gray 900; mt 2; dark [ text_white ];
                ]
            [ txt value ];
          div
            ~tw:Tw.[ flex; items_center; gap 2; mt 2 ]
            [
              span
                ~tw:
                  (if trend = "up" then
                     Tw.[ text_sm; font_medium; text green 600 ]
                   else Tw.[ text_sm; font_medium; text red 600 ])
                [
                  txt
                    (if trend = "up" then "+" ^ change ^ "%"
                     else "-" ^ change ^ "%");
                ];
              span
                ~tw:Tw.[ text_sm; text gray 500; dark [ text gray 400 ] ]
                [ txt "vs last month" ];
            ];
        ];
    ]

let stats_section =
  div
    ~tw:Tw.[ grid; grid_cols 1; md [ grid_cols 2 ]; lg [ grid_cols 4 ]; gap 6 ]
    [
      stat_card ~title:"Total Revenue" ~value:"$45,231" ~change:"12.5"
        ~trend:"up" ~gradient_from:Tw.blue ~gradient_to:Tw.indigo;
      stat_card ~title:"Active Users" ~value:"2,338" ~change:"8.2" ~trend:"up"
        ~gradient_from:Tw.emerald ~gradient_to:Tw.teal;
      stat_card ~title:"New Signups" ~value:"1,234" ~change:"5.4" ~trend:"up"
        ~gradient_from:Tw.violet ~gradient_to:Tw.purple;
      stat_card ~title:"Bounce Rate" ~value:"23.4%" ~change:"2.1" ~trend:"down"
        ~gradient_from:Tw.orange ~gradient_to:Tw.amber;
    ]

(* ========== Chart Section ========== *)

let chart_card ~title ~subtitle children =
  div
    ~tw:Tw.[ bg_white; rounded_xl; shadow_sm; p 6; dark [ bg gray 800 ] ]
    [
      div
        ~tw:Tw.[ flex; items_center; justify_between; mb 6 ]
        [
          div
            [
              h3
                ~tw:
                  Tw.
                    [
                      text_lg; font_semibold; text gray 900; dark [ text_white ];
                    ]
                [ txt title ];
              p
                ~tw:Tw.[ text_sm; text gray 500; mt 1; dark [ text gray 400 ] ]
                [ txt subtitle ];
            ];
          div
            ~tw:
              Tw.
                [
                  px 3;
                  py 1;
                  bg gray 100;
                  rounded_lg;
                  text_sm;
                  font_medium;
                  text gray 600;
                  cursor_pointer;
                  hover [ bg gray 200 ];
                  dark [ bg gray 700; text gray 300; hover [ bg gray 600 ] ];
                ]
            [ txt "This Month" ];
        ];
      div ~tw:Tw.[ h 64 ] children;
    ]

(* Simulated bar chart *)
let bar_chart =
  let bar height color =
    div
      ~tw:
        Tw.
          [
            flex_1;
            bg color 200;
            rounded_t_lg;
            transition_all;
            duration 300;
            hover [ bg color 300 ];
            dark [ bg color 700; hover [ bg color 600 ] ];
          ]
      ~at:[ At.v "style" (Printf.sprintf "height: %d%%" height) ]
      []
  in
  div
    ~tw:Tw.[ flex; items_end; gap 2; h_full ]
    [
      bar 45 Tw.blue;
      bar 72 Tw.blue;
      bar 58 Tw.blue;
      bar 85 Tw.blue;
      bar 62 Tw.blue;
      bar 90 Tw.blue;
      bar 78 Tw.blue;
      bar 65 Tw.blue;
      bar 82 Tw.blue;
      bar 70 Tw.blue;
      bar 88 Tw.blue;
      bar 95 Tw.blue;
    ]

(* Recent activity *)
let activity_row ~name ~action ~time ~status =
  div
    ~tw:
      Tw.
        [
          flex;
          items_center;
          justify_between;
          py 3;
          border_b;
          border_color gray 100;
          dark [ border_color gray 700 ];
        ]
    [
      div
        ~tw:Tw.[ flex; items_center; gap 3 ]
        [
          div
            ~tw:
              Tw.
                [
                  w 10;
                  h 10;
                  rounded_full;
                  bg gray 200;
                  flex;
                  items_center;
                  justify_center;
                  font_medium;
                  text gray 600;
                  dark [ bg gray 600; text gray 300 ];
                ]
            [ txt (String.sub name 0 1) ];
          div
            [
              p
                ~tw:
                  Tw.
                    [ text_sm; font_medium; text gray 900; dark [ text_white ] ]
                [ txt name ];
              p
                ~tw:Tw.[ text_xs; text gray 500; dark [ text gray 400 ] ]
                [ txt action ];
            ];
        ];
      div
        ~tw:Tw.[ text_right ]
        [
          p
            ~tw:Tw.[ text_xs; text gray 500; dark [ text gray 400 ] ]
            [ txt time ];
          span
            ~tw:
              (match status with
              | "success" ->
                  Tw.
                    [
                      text_xs;
                      font_medium;
                      text green 600;
                      bg green 100;
                      px 2;
                      py 1;
                      rounded_full;
                      dark [ bg green 900; text green 300 ];
                    ]
              | "pending" ->
                  Tw.
                    [
                      text_xs;
                      font_medium;
                      text yellow 600;
                      bg yellow 100;
                      px 2;
                      py 1;
                      rounded_full;
                      dark [ bg yellow 900; text yellow 300 ];
                    ]
              | _ ->
                  Tw.
                    [
                      text_xs;
                      font_medium;
                      text gray 600;
                      bg gray 100;
                      px 2;
                      py 1;
                      rounded_full;
                      dark [ bg gray 700; text gray 300 ];
                    ])
            [ txt status ];
        ];
    ]

let activity_section =
  div
    ~tw:Tw.[ bg_white; rounded_xl; shadow_sm; p 6; dark [ bg gray 800 ] ]
    [
      h3
        ~tw:
          Tw.
            [ text_lg; font_semibold; text gray 900; mb 4; dark [ text_white ] ]
        [ txt "Recent Activity" ];
      div
        [
          activity_row ~name:"Alice Johnson" ~action:"Completed purchase"
            ~time:"2 min ago" ~status:"success";
          activity_row ~name:"Bob Smith" ~action:"Started subscription"
            ~time:"15 min ago" ~status:"success";
          activity_row ~name:"Carol Davis" ~action:"Payment processing"
            ~time:"1 hour ago" ~status:"pending";
          activity_row ~name:"David Wilson" ~action:"Account created"
            ~time:"3 hours ago" ~status:"success";
        ];
    ]

let charts_section =
  div
    ~tw:Tw.[ grid; grid_cols 1; lg [ grid_cols 3 ]; gap 6 ]
    [
      div
        ~tw:Tw.[ lg [ col_span 2 ] ]
        [
          chart_card ~title:"Revenue Overview"
            ~subtitle:"Monthly revenue trends" [ bar_chart ];
        ];
      activity_section;
    ]

(* ========== Main Layout ========== *)

let main_content =
  main
    ~tw:Tw.[ flex_1; bg gray 50; dark [ bg gray 900 ]; min_w 0 ]
    [
      header_section;
      div
        ~tw:Tw.[ p 4; space_y 4; md [ p 6; space_y 6 ] ]
        [
          (* Page title *)
          div
            [
              h1
                ~tw:
                  Tw.
                    [
                      text_xl;
                      md [ text_2xl ];
                      font_bold;
                      text gray 900;
                      dark [ text_white ];
                    ]
                [ txt "Dashboard Overview" ];
              p
                ~tw:
                  Tw.
                    [
                      text_sm;
                      md [ text_base ];
                      text gray 500;
                      mt 1;
                      dark [ text gray 400 ];
                    ]
                [ txt "Welcome back! Here's what's happening today." ];
            ];
          stats_section;
          charts_section;
        ];
    ]

let page_view =
  page ~title:"Analytics Dashboard" ~tw_css:"dashboard.css"
    ~meta:
      [
        ( "description",
          "Modern analytics dashboard built with OCaml and Tailwind" );
        ("viewport", "width=device-width, initial-scale=1.0");
      ]
    []
    [ div ~tw:Tw.[ flex; min_h_screen ] [ sidebar; main_content ] ]

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
