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
    if active then Tw.[ bg ~shade:600 blue; text white; dark [ bg blue ] ]
    else
      Tw.
        [
          text ~shade:600 gray;
          hover [ bg ~shade:100 gray; text ~shade:900 gray ];
          dark
            [ text ~shade:300 gray; hover [ bg ~shade:700 gray; text white ] ];
        ]
  in
  div ~tw:(base_styles @ state_styles) [ icon; txt label ]

let sidebar_logo =
  div
    ~tw:
      Tw.
        [
          p 6;
          border_b;
          border_color ~shade:200 gray;
          dark [ border_color ~shade:700 gray ];
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
                  text white;
                  font_bold;
                ]
            [ txt "D" ];
          span
            ~tw:
              Tw.
                [
                  text_xl; font_bold; text ~shade:900 gray; dark [ text white ];
                ]
            [ txt "Dashboard" ];
        ];
    ]

let sidebar_user =
  div
    ~tw:
      Tw.
        [
          p 4;
          border_t;
          border_color ~shade:200 gray;
          dark [ border_color ~shade:700 gray ];
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
                  bg ~shade:300 gray;
                  flex;
                  items_center;
                  justify_center;
                  text ~shade:600 gray;
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
                      text ~shade:900 gray;
                      dark [ text white ];
                    ]
                [ txt "John Doe" ];
              p
                ~tw:Tw.[ text_xs; text gray; dark [ text ~shade:400 gray ] ]
                [ txt "Admin" ];
            ];
        ];
    ]

let sidebar =
  aside
    ~tw:
      Tw.
        [
          hidden;
          lg [ flex; w 64 ];
          min_h_screen;
          bg white;
          border_r;
          border_color ~shade:200 gray;
          flex_col;
          dark [ bg ~shade:800 gray; border_color ~shade:700 gray ];
        ]
    [
      sidebar_logo;
      nav
        ~tw:Tw.[ flex_1; p 4; space_y 2. ]
        [
          nav_item ~active:true ~icon:icon_home "Overview";
          nav_item ~active:false ~icon:icon_chart "Analytics";
          nav_item ~active:false ~icon:icon_users "Customers";
          nav_item ~active:false ~icon:icon_settings "Settings";
        ];
      sidebar_user;
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
              text white;
              font_bold;
              text_sm;
            ]
        [ txt "D" ];
      span
        ~tw:Tw.[ font_bold; text ~shade:900 gray; dark [ text white ] ]
        [ txt "Dashboard" ];
    ]

let header_search =
  div
    ~tw:
      Tw.
        [
          hidden;
          md [ flex ];
          items_center;
          gap 3;
          bg ~shade:100 gray;
          rounded_lg;
          px 4;
          py 2;
          flex_1;
          max_w_md;
          dark [ bg ~shade:700 gray ];
        ]
    [
      icon_search;
      span
        ~tw:Tw.[ text_sm; text gray; dark [ text ~shade:400 gray ] ]
        [ txt "Search..." ];
    ]

let notification_badge =
  span
    ~tw:
      Tw.
        [
          absolute;
          top 0;
          right 0;
          w 5;
          h 5;
          bg red;
          text white;
          text_xs;
          font_bold;
          rounded_full;
          flex;
          items_center;
          justify_center;
        ]
    [ txt "3" ]

let header_actions =
  div
    ~tw:Tw.[ flex; items_center; gap 2; md [ gap 4 ] ]
    [
      div
        ~tw:
          Tw.
            [
              md [ hidden ];
              p 2;
              rounded_lg;
              hover [ bg ~shade:100 gray ];
              cursor_pointer;
              dark [ hover [ bg ~shade:700 gray ] ];
            ]
        [ icon_search ];
      div
        ~tw:
          Tw.
            [
              relative;
              p 2;
              rounded_lg;
              hover [ bg ~shade:100 gray ];
              cursor_pointer;
              transition_colors;
              dark [ hover [ bg ~shade:700 gray ] ];
            ]
        [ icon_bell; notification_badge ];
    ]

let header_section =
  header
    ~tw:
      Tw.
        [
          bg white;
          border_b;
          border_color ~shade:200 gray;
          px 4;
          py 3;
          md [ px 6; py 4 ];
          dark [ bg ~shade:800 gray; border_color ~shade:700 gray ];
        ]
    [
      div
        ~tw:Tw.[ flex; items_center; justify_between; gap 4 ]
        [
          div
            ~tw:Tw.[ flex; items_center; gap 3 ]
            [
              div
                ~tw:
                  Tw.
                    [
                      lg [ hidden ];
                      p 2;
                      rounded_lg;
                      hover [ bg ~shade:100 gray ];
                      cursor_pointer;
                      dark [ hover [ bg ~shade:700 gray ] ];
                    ]
                [ icon_menu ];
              mobile_logo;
            ];
          header_search;
          header_actions;
        ];
    ]

(* ========== Stats Cards ========== *)

let stat_card_accent ~gradient_from ~gradient_to =
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
    []

let stat_card_content ~title ~value ~change ~trend =
  div
    ~tw:Tw.[ relative ]
    [
      p
        ~tw:
          Tw.[ text_sm; font_medium; text gray; dark [ text ~shade:400 gray ] ]
        [ txt title ];
      p
        ~tw:
          Tw.
            [
              text_3xl;
              font_bold;
              text ~shade:900 gray;
              mt 2;
              dark [ text white ];
            ]
        [ txt value ];
      div
        ~tw:Tw.[ flex; items_center; gap 2; mt 2 ]
        [
          span
            ~tw:
              (if trend = "up" then
                 Tw.[ text_sm; font_medium; text ~shade:600 green ]
               else Tw.[ text_sm; font_medium; text ~shade:600 red ])
            [
              txt
                (if trend = "up" then "+" ^ change ^ "%" else "-" ^ change ^ "%");
            ];
          span
            ~tw:Tw.[ text_sm; text gray; dark [ text ~shade:400 gray ] ]
            [ txt "vs last month" ];
        ];
    ]

let stat_card ~title ~value ~change ~trend ~gradient_from ~gradient_to =
  div
    ~tw:
      Tw.
        [
          bg white;
          rounded_xl;
          shadow_sm;
          p 6;
          relative;
          overflow_hidden;
          transition_all;
          duration 300;
          hover [ shadow_md; transform; scale 102 ];
          dark [ bg ~shade:800 gray ];
        ]
    [
      stat_card_accent ~gradient_from ~gradient_to;
      stat_card_content ~title ~value ~change ~trend;
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
    ~tw:Tw.[ bg white; rounded_xl; shadow_sm; p 6; dark [ bg ~shade:800 gray ] ]
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
                      text_lg;
                      font_semibold;
                      text ~shade:900 gray;
                      dark [ text white ];
                    ]
                [ txt title ];
              p
                ~tw:
                  Tw.[ text_sm; text gray; mt 1; dark [ text ~shade:400 gray ] ]
                [ txt subtitle ];
            ];
          div
            ~tw:
              Tw.
                [
                  px 3;
                  py 1;
                  bg ~shade:100 gray;
                  rounded_lg;
                  text_sm;
                  font_medium;
                  text ~shade:600 gray;
                  cursor_pointer;
                  hover [ bg ~shade:200 gray ];
                  dark
                    [
                      bg ~shade:700 gray;
                      text ~shade:300 gray;
                      hover [ bg ~shade:600 gray ];
                    ];
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
            bg ~shade:200 color;
            rounded_t_lg;
            transition_all;
            duration 300;
            hover [ bg ~shade:300 color ];
            dark [ bg ~shade:700 color; hover [ bg ~shade:600 color ] ];
          ]
      ~at:
        [
          At.v "style"
            (String.concat "" [ "height: "; string_of_int height; "%" ]);
        ]
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
let status_badge_style = function
  | "success" ->
      Tw.
        [
          text_xs;
          font_medium;
          text ~shade:600 green;
          bg ~shade:100 green;
          px 2;
          py 1;
          rounded_full;
          dark [ bg ~shade:900 green; text ~shade:300 green ];
        ]
  | "pending" ->
      Tw.
        [
          text_xs;
          font_medium;
          text ~shade:600 yellow;
          bg ~shade:100 yellow;
          px 2;
          py 1;
          rounded_full;
          dark [ bg ~shade:900 yellow; text ~shade:300 yellow ];
        ]
  | _ ->
      Tw.
        [
          text_xs;
          font_medium;
          text ~shade:600 gray;
          bg ~shade:100 gray;
          px 2;
          py 1;
          rounded_full;
          dark [ bg ~shade:700 gray; text ~shade:300 gray ];
        ]

let activity_avatar name =
  div
    ~tw:
      Tw.
        [
          w 10;
          h 10;
          rounded_full;
          bg ~shade:200 gray;
          flex;
          items_center;
          justify_center;
          font_medium;
          text ~shade:600 gray;
          dark [ bg ~shade:600 gray; text ~shade:300 gray ];
        ]
    [ txt (String.sub name 0 1) ]

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
          border_color ~shade:100 gray;
          dark [ border_color ~shade:700 gray ];
        ]
    [
      div
        ~tw:Tw.[ flex; items_center; gap 3 ]
        [
          activity_avatar name;
          div
            [
              p
                ~tw:
                  Tw.
                    [
                      text_sm;
                      font_medium;
                      text ~shade:900 gray;
                      dark [ text white ];
                    ]
                [ txt name ];
              p
                ~tw:Tw.[ text_xs; text gray; dark [ text ~shade:400 gray ] ]
                [ txt action ];
            ];
        ];
      div
        ~tw:Tw.[ text_right ]
        [
          p
            ~tw:Tw.[ text_xs; text gray; dark [ text ~shade:400 gray ] ]
            [ txt time ];
          span ~tw:(status_badge_style status) [ txt status ];
        ];
    ]

let activity_section =
  div
    ~tw:Tw.[ bg white; rounded_xl; shadow_sm; p 6; dark [ bg ~shade:800 gray ] ]
    [
      h3
        ~tw:
          Tw.
            [
              text_lg;
              font_semibold;
              text ~shade:900 gray;
              mb 4;
              dark [ text white ];
            ]
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

let content =
  main
    ~tw:Tw.[ flex_1; bg ~shade:50 gray; dark [ bg ~shade:900 gray ]; min_w 0 ]
    [
      header_section;
      div
        ~tw:Tw.[ p 4; space_y 4.; md [ p 6; space_y 6. ] ]
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
                      text ~shade:900 gray;
                      dark [ text white ];
                    ]
                [ txt "Dashboard Overview" ];
              p
                ~tw:
                  Tw.
                    [
                      text_sm;
                      md [ text_base ];
                      text gray;
                      mt 1;
                      dark [ text ~shade:400 gray ];
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
    [ div ~tw:Tw.[ flex; min_h_screen ] [ sidebar; content ] ]

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
