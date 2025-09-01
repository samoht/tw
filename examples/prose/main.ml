(** Prose showcase example

    Demonstrates Tw.Prose typography with:
    - Base prose container with comprehensive child selectors
    - Size variants: prose-sm, prose-lg, prose-xl, prose-2xl
    - Color themes: prose-gray, prose-slate
    - not-prose exclusion inside a prose container *)

open Tw_html

let sample_article () =
  [
    h1 [ txt "Prose Typography Manual" ];
    p
      ~tw:Tw.[ prose_lead ]
      [ txt "Lead paragraph that sets the tone and summarizes the topic." ];
    p
      [
        txt "This paragraph demonstrates inline elements: ";
        a ~at:[ At.href "#" ] [ txt "link" ];
        txt ", ";
        strong [ txt "strong" ];
        txt ", ";
        em [ txt "emphasis" ];
        txt ", and inline ";
        code [ txt "let x = 1" ];
        txt ".";
      ];
    blockquote
      [
        p
          [
            txt
              "A thoughtful quote enhances the narrative and provides context.";
          ];
        p
          [
            txt "Second paragraph in the quote to show closing quotes handling.";
          ];
      ];
    h2 [ txt "Headings" ];
    h3 [ txt "Section H3" ];
    p [ txt "Supporting text under an H3 heading." ];
    h4 [ txt "Subsection H4" ];
    p [ txt "Supporting text under an H4 heading." ];
    hr ();
    h2 [ txt "Lists" ];
    p [ txt "Unordered list:" ];
    ul [ li [ txt "Item one" ]; li [ txt "Item two" ]; li [ txt "Item three" ] ];
    p [ txt "Ordered list (decimal):" ];
    ol [ li [ txt "First" ]; li [ txt "Second" ]; li [ txt "Third" ] ];
    p [ txt "Ordered list (upper-alpha):" ];
    ol ~at:[ At.type' "A" ] [ li [ txt "Alpha" ]; li [ txt "Beta" ] ];
    p [ txt "Ordered list (upper-roman):" ];
    ol ~at:[ At.type' "I" ] [ li [ txt "One" ]; li [ txt "Two" ] ];
    p [ txt "Nested lists:" ];
    ul
      [ li [ txt "Parent"; ul [ li [ txt "Child A" ]; li [ txt "Child B" ] ] ] ];
    h2 [ txt "Definition List" ];
    dl
      [
        dt [ txt "Term" ];
        dd [ txt "Definition explaining the term in detail." ];
        dt [ txt "API" ];
        dd [ txt "Application Programming Interface." ];
      ];
    h2 [ txt "Code" ];
    p [ txt "Inline "; code [ txt "List.map f xs" ]; txt " and block:" ];
    pre
      [
        code [ txt "let rec fact n =\n  if n <= 1 then 1 else n * fact (n-1)" ];
      ];
    h2 [ txt "Keyboard Input" ];
    p
      [
        txt "Press ";
        kbd [ txt "Ctrl" ];
        txt "+";
        kbd [ txt "C" ];
        txt " to copy.";
      ];
    h2 [ txt "Table" ];
    table
      [
        thead [ tr [ th [ txt "Name" ]; th [ txt "Value" ] ] ];
        tbody
          [
            tr [ td [ txt "Alpha" ]; td [ code [ txt "42" ] ] ];
            tr [ td [ txt "Beta" ]; td [ txt "Text" ] ];
          ];
        tfoot [ tr [ td [ txt "Total" ]; td [ txt "—" ] ] ];
      ];
    h2 [ txt "Media" ];
    figure
      [
        img
          ~at:[ At.src "https://via.placeholder.com/600x200"; At.alt "img" ]
          ();
        figcaption [ txt "A descriptive caption for the image." ];
      ];
    hr ();
    h2 [ txt "Exclusions" ];
    p [ txt "Content marked as not-prose is excluded from prose styling:" ];
    div
      ~tw:Tw.[ not_prose ]
      [
        h4 [ txt "Heading unaffected by prose" ];
        p [ txt "This paragraph retains default spacing and size." ];
      ];
  ]

let prose_section ~title ~tw_classes content =
  section
    ~tw:Tw.[ mb 12 ]
    [
      h2 ~tw:Tw.[ text_2xl; font_semibold; mb 4 ] [ txt title ];
      div ~tw:tw_classes content;
    ]

let page_content =
  let article = sample_article () in
  [
    h1 ~tw:Tw.[ text_4xl; font_bold; mb 8; text_center ] [ txt "Prose Demo" ];
    prose_section ~title:"Base prose" ~tw_classes:Tw.[ prose ] article;
    prose_section ~title:"Size variants"
      ~tw_classes:Tw.[ grid; grid_cols 1; md [ grid_cols 2 ]; gap 8 ]
      [
        div ~tw:Tw.[ prose_sm ] (sample_article ());
        div ~tw:Tw.[ prose_lg ] (sample_article ());
        div ~tw:Tw.[ prose_xl ] (sample_article ());
        div ~tw:Tw.[ prose_2xl ] (sample_article ());
      ];
    prose_section ~title:"Color themes"
      ~tw_classes:Tw.[ grid; grid_cols 1; md [ grid_cols 2 ]; gap 8 ]
      [
        div ~tw:Tw.[ prose; prose_gray ] (sample_article ());
        div ~tw:Tw.[ prose; prose_slate ] (sample_article ());
      ];
  ]

let main_page =
  page ~title:"Prose Typography Demo" ~tw_css:"prose.css" []
    [
      div ~tw:Tw.[ max_w_4xl; mx_auto; p 8; flex; flex_col; gap 8 ] page_content;
    ]

let () =
  let html_out = html main_page in
  let css_file, css_stylesheet = css main_page in
  let css_out = Tw.Css.to_string ~minify:true ~optimize:true css_stylesheet in
  let oc_html = open_out "index.html" in
  output_string oc_html html_out;
  close_out oc_html;
  let oc_css = open_out css_file in
  output_string oc_css css_out;
  close_out oc_css;
  ()
