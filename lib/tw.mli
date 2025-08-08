(** Type-safe CSS styling

    This module provides a typed interface for generating CSS styles without
    writing raw CSS. It prevents typos and invalid combinations at compile-time
    by using OCaml's type system.

    {1:concepts Core Concepts}

    - {b Utility-first}: Instead of writing CSS classes with multiple
      properties, you compose small, single-purpose utilities (e.g., [bg blue]
      for background color, [p 4] for padding)

    - {b Type-safe}: The OCaml compiler catches errors like misspelled utilities
      or invalid value combinations at compile time

    - {b Composable}: Styles are composed by creating lists of utilities that
      work together

    {1:model How It Works}

    1. Each function returns a value of type {!t} representing a single style 2.
    Multiple styles are combined by creating a [t list] 3. The list is converted
    to CSS using {!to_classes} 4. The resulting string is used with HTML
    elements

    {1:units Units and Scales}

    - {b Spacing}: Integer values create spacing in 0.25rem increments. [4] =
      1rem = 16px (by default)
    - {b Sizes}: Predefined sizes like [sm], [md], [lg] provide consistent
      scaling
    - {b Colors}: Colors use an optional shade parameter (50-900) where higher
      numbers are darker

    {1:usage Usage Example}

    {[
      let button =
        let styles =
          [
            (* Background and text colors *)
            bg blue;
            (* blue background *)
            text white;
            (* white text *)

            (* Spacing: padding of 1rem vertical, 2rem horizontal *)
            py 4;
            (* 4 * 0.25rem = 1rem *)
            px 8;
            (* 8 * 0.25rem = 2rem *)

            (* Typography and borders *)
            font_bold;
            (* font-weight: 700 *)
            rounded md;
            (* medium border radius *)

            (* Interactive states *)
            on_hover [ bg ~shade:700 blue ];
            (* darker blue on hover *)
            transition_colors;
            (* smooth color transitions *)

            (* Responsive design *)
            on_sm [ px 6 ];
            (* less padding on small screens *)
          ]
        in
        Html.button ~tw:styles [ Html.txt "Click Me" ]
    ]}

    {1:links Learn More}

    - The API design follows {{:https://tailwindcss.com/}Tailwind CSS}
      conventions
    - Colors, spacing, and sizes use consistent scales throughout *)

(** {1 Core Types}
    @see <https://tailwindcss.com/docs/customizing-colors> Customizing Colors *)

type t
(** The abstract type representing a single CSS style utility. You cannot create
    values of this type directly - use the provided functions. *)

type color
(** Abstract type for colors. Use color constructors like [red], [blue], etc.
    Colors can have shades from 50 (lightest) to 900 (darkest). *)

(** {1 Color Constructors} *)

val black : color
(** Pure black color. *)

val white : color
(** Pure white color. *)

val gray : color
(** Neutral gray color family. *)

val slate : color
(** Cool gray with blue undertones. *)

val zinc : color
(** Neutral gray with modern feel. *)

val red : color
(** Classic red color family. *)

val orange : color
(** Vibrant orange color family. *)

val amber : color
(** Warm yellow-orange color family. *)

val yellow : color
(** Bright yellow color family. *)

val lime : color
(** Electric green-yellow color family. *)

val green : color
(** Natural green color family. *)

val emerald : color
(** Rich blue-green color family. *)

val teal : color
(** Blue-green color family. *)

val cyan : color
(** Bright blue-cyan color family. *)

val sky : color
(** Light blue color family. *)

val blue : color
(** Classic blue color family. *)

val indigo : color
(** Deep blue-purple color family. *)

val violet : color
(** Purple-blue color family. *)

val purple : color
(** Classic purple color family. *)

val fuchsia : color
(** Bright pink-purple color family. *)

val pink : color
(** Soft pink color family. *)

val rose : color
(** Warm pink color family. *)

(** {1 Color & Background} *)

val bg : color -> int -> t
(** [bg color shade] sets the background color with a specific shade.

    Examples:
    - [bg blue 500]: Medium blue background
    - [bg gray 100]: Light gray background
    - [bg slate 900]: Very dark slate background

    Shades range from 50 (lightest) to 900 (darkest). *)

val bg_transparent : t
(** Makes background fully transparent (invisible). *)

val bg_current : t
(** Sets background color to match the element's text color. If the element has
    [text ~shade:500 blue], the background will also be blue-500. Useful for
    icons and decorative elements that should match text. *)

val bg_black : t
(** Black background. Same as {!bg} {!black}. *)

val bg_white : t
(** White background. Same as {!bg} {!white}. *)

val bg_gray : t
(** Default gray background. Same as [bg gray]. *)

val bg_slate : t
(** Default slate background. Same as [bg slate]. *)

val bg_zinc : t
(** Default zinc background. Same as [bg zinc]. *)

val bg_red : t
(** Default red background. Same as [bg red]. *)

val bg_orange : t
(** Default orange background. Same as [bg orange]. *)

val bg_amber : t
(** Default amber background. Same as [bg amber]. *)

val bg_yellow : t
(** Default yellow background. Same as [bg yellow]. *)

val bg_lime : t
(** Default lime background. Same as [bg lime]. *)

val bg_green : t
(** Default green background. Same as [bg green]. *)

val bg_emerald : t
(** Default emerald background. Same as [bg emerald]. *)

val bg_teal : t
(** Default teal background. Same as [bg teal]. *)

val bg_cyan : t
(** Default cyan background. Same as [bg cyan]. *)

val bg_sky : t
(** Default sky background. Same as [bg sky]. *)

val bg_blue : t
(** Default blue background. Same as [bg blue]. *)

val bg_indigo : t
(** Default indigo background. Same as [bg indigo]. *)

val bg_violet : t
(** Default violet background. Same as [bg violet]. *)

val bg_purple : t
(** Default purple background. Same as [bg purple]. *)

val bg_fuchsia : t
(** Default fuchsia background. Same as [bg fuchsia]. *)

val bg_pink : t
(** Default pink background. Same as [bg pink]. *)

val bg_rose : t
(** Default rose background. Same as [bg rose]. *)

val text : color -> int -> t
(** [text color shade] sets text color.

    Examples:
    - [text black 100]: Pure black text (black doesn't use shades, so value is
      ignored)
    - [text gray 600]: Dark gray for body text
    - [text gray 500]: Medium gray for secondary text
    - [text blue 700]: Dark blue for links

    Higher shade numbers (700-900) ensure readability on light backgrounds. *)

val text_transparent : t
(** Transparent text color. *)

val text_current : t
(** Explicitly sets text color to "currentColor" (the inherited text color).
    This is rarely needed since text naturally inherits color from parents. *)

val text_black : t
(** Black text. Same as [text black 500]. *)

val text_white : t
(** White text. Same as [text white 500]. *)

val text_gray : t
(** Default gray text. Same as [text gray 500]. *)

val text_slate : t
(** Default slate text. Same as [text slate 500]. *)

val text_zinc : t
(** Default zinc text. Same as [text zinc 500]. *)

val text_blue : t
(** Default blue text. Same as [text blue 500]. *)

val text_red : t
(** Default red text. Same as [text red 500]. *)

val text_orange : t
(** Default orange text. Same as [text orange 500]. *)

val text_amber : t
(** Default amber text. Same as [text amber 500]. *)

val text_yellow : t
(** Default yellow text. Same as [text yellow 500]. *)

val text_lime : t
(** Default lime text. Same as [text lime 500]. *)

val text_green : t
(** Default green text. Same as [text green 500]. *)

val text_emerald : t
(** Default emerald text. Same as [text emerald 500]. *)

val text_teal : t
(** Default teal text. Same as [text teal 500]. *)

val text_cyan : t
(** Default cyan text. Same as [text cyan 500]. *)

val text_sky : t
(** Default sky text. Same as [text sky 500]. *)

val text_indigo : t
(** Default indigo text. Same as [text indigo 500]. *)

val text_violet : t
(** Default violet text. Same as [text violet 500]. *)

val text_purple : t
(** Default purple text. Same as [text purple 500]. *)

val text_fuchsia : t
(** Default fuchsia text. Same as [text fuchsia 500]. *)

val text_pink : t
(** Default pink text. Same as [text pink 500]. *)

val text_rose : t
(** Default rose text. Same as [text rose 500]. *)

val border_color : color -> int -> t
(** [border_color color shade] creates a border color with a specific shade. *)

val border_transparent : t
(** Transparent border. *)

val border_current : t
(** Sets border color to match the text color. For example:
    {[
      div ~tw:[ text ~shade:600 red; border xs; border_current ]
      (* Border will be red-600, same as the text *)
    ]}

    This is the default behavior in Tailwind v4, but can be explicitly set. *)

val border_black : t
(** Black border. Same as [border_color black 500]. *)

val border_white : t
(** White border. Same as [border_color white 500]. *)

val border_gray : t
(** Default gray border. Same as [border_color gray 500]. *)

val border_slate : t
(** Default slate border. Same as [border_color slate 500]. *)

val border_zinc : t
(** Default zinc border. Same as [border_color zinc 500]. *)

val border_red : t
(** Default red border. Same as [border_color red 500]. *)

val border_orange : t
(** Default orange border. Same as [border_color orange 500]. *)

val border_amber : t
(** Default amber border. Same as [border_color amber 500]. *)

val border_yellow : t
(** Default yellow border. Same as [border_color yellow 500]. *)

val border_lime : t
(** Default lime border. Same as [border_color lime 500]. *)

val border_green : t
(** Default green border. Same as [border_color green 500]. *)

val border_emerald : t
(** Default emerald border. Same as [border_color emerald 500]. *)

val border_teal : t
(** Default teal border. Same as [border_color teal 500]. *)

val border_cyan : t
(** Default cyan border. Same as [border_color cyan 500]. *)

val border_sky : t
(** Default sky border. Same as [border_color sky 500]. *)

val border_blue : t
(** Default blue border. Same as [border_color blue 500]. *)

val border_indigo : t
(** Default indigo border. Same as [border_color indigo 500]. *)

val border_violet : t
(** Default violet border. Same as [border_color violet 500]. *)

val border_purple : t
(** Default purple border. Same as [border_color purple 500]. *)

val border_fuchsia : t
(** Default fuchsia border. Same as [border_color fuchsia 500]. *)

val border_pink : t
(** Default pink border. Same as [border_color pink 500]. *)

val border_rose : t
(** Default rose border. Same as [border_color rose 500]. *)

val bg_gradient_to_b : t
(** Creates a gradient from top to bottom. Must be used with from_color and
    to_color.

    Example:
    {[
      div
        ~tw:
          [
            bg_gradient_to_b;
            from_color ~shade:100 blue;
            to_color ~shade:600 blue;
          ]
        [ txt "Gradient background" ]
    ]} *)

val bg_gradient_to_br : t
(** Creates a gradient from top-left to bottom-right (diagonal). *)

val bg_gradient_to_t : t
(** Creates a gradient from bottom to top. *)

val bg_gradient_to_tr : t
(** Creates a gradient from bottom-left to top-right. *)

val bg_gradient_to_r : t
(** Creates a gradient from left to right. *)

val bg_gradient_to_bl : t
(** Creates a gradient from top-right to bottom-left. *)

val bg_gradient_to_l : t
(** Creates a gradient from right to left. *)

val bg_gradient_to_tl : t
(** Creates a gradient from bottom-right to top-left. *)

val from_color : color -> int -> t
(** [from_color color shade] sets the starting color of a gradient. *)

val to_color : color -> int -> t
(** [to_color color shade] sets the ending color of a gradient. *)

(** {1 Spacing}
    @see <https://tailwindcss.com/docs/padding> Padding
    @see <https://tailwindcss.com/docs/margin> Margin
    @see <https://tailwindcss.com/docs/space> Space Between
    @see <https://tailwindcss.com/docs/gap> Gap *)

val p : int -> t
(** [p n] sets padding (inner spacing) on all sides using Tailwind scale (n ×
    0.25rem).

    Examples:
    - [p 4]: 1rem padding on all sides
    - [p 0]: Remove all padding
    - [p 8]: 2rem padding on all sides. *)

val px : int -> t
(** [px n] sets horizontal padding (left and right). Common for buttons and
    cards to have more horizontal than vertical padding. *)

val py : int -> t
(** [py n] sets vertical padding (top and bottom). Often smaller than horizontal
    padding for better proportions. *)

val pt : int -> t
(** [pt n] sets top padding. *)

val pr : int -> t
(** [pr n] sets right padding. *)

val pb : int -> t
(** [pb n] sets bottom padding. *)

val pl : int -> t
(** [pl n] sets left padding. *)

(** {2 Special padding values} *)

val p_px : t
(** [p_px] sets 1px padding on all sides. *)

val p_full : t
(** [p_full] sets 100% padding on all sides. *)

val px_px : t
(** [px_px] sets 1px horizontal padding. *)

val px_full : t
(** [px_full] sets 100% horizontal padding. *)

val py_px : t
(** [py_px] sets 1px vertical padding. *)

val py_full : t
(** [py_full] sets 100% vertical padding. *)

val pt_px : t
(** [pt_px] sets 1px top padding. *)

val pt_full : t
(** [pt_full] sets 100% top padding. *)

val pr_px : t
(** [pr_px] sets 1px right padding. *)

val pr_full : t
(** [pr_full] sets 100% right padding. *)

val pb_px : t
(** [pb_px] sets 1px bottom padding. *)

val pb_full : t
(** [pb_full] sets 100% bottom padding. *)

val pl_px : t
(** [pl_px] sets 1px left padding. *)

val pl_full : t
(** [pl_full] sets 100% left padding. *)

val m : int -> t
(** [m n] sets margin (outer spacing) on all sides using Tailwind scale (n ×
    0.25rem).

    Examples:
    - [m 4]: 1rem margin on all sides
    - [m 0]: Remove all margins. *)

val mx : int -> t
(** [mx n] sets horizontal margin (left and right). *)

val my : int -> t
(** [my n] sets vertical margin (top and bottom). Useful for spacing between
    sections. *)

val mt : int -> t
(** [mt n] sets top margin. *)

val mr : int -> t
(** [mr n] sets right margin. *)

val mb : int -> t
(** [mb n] sets bottom margin. *)

val ml : int -> t
(** [ml n] sets left margin. *)

(** {2 Common margin utilities} *)

val m_auto : t
(** [m_auto] sets auto margins on all sides (centers if width is defined). *)

val mx_auto : t
(** [mx_auto] centers block elements horizontally. Very commonly used. *)

val my_auto : t
(** [my_auto] sets auto vertical margins. *)

val mt_auto : t
(** [mt_auto] pushes element to bottom by setting auto top margin. *)

val mr_auto : t
(** [mr_auto] pushes element to left by setting auto right margin. *)

val mb_auto : t
(** [mb_auto] pushes element to top by setting auto bottom margin. *)

val ml_auto : t
(** [ml_auto] pushes element to right by setting auto left margin. *)

val neg_mt : int -> t
(** [neg_mt n] pulls element upward with negative margin using Tailwind scale.
    Useful for overlapping elements or compensating for padding.

    Example: [neg_mt 4] creates -1rem top margin. *)

val neg_mr : int -> t
(** [neg_mr n] pulls element rightward with negative margin. *)

val neg_mb : int -> t
(** [neg_mb n] pulls element (and following content) upward. *)

val neg_ml : int -> t
(** [neg_ml n] pulls element leftward with negative margin. *)

(** {2 Special negative margin variants} *)

val neg_mt_px : t
(** [neg_mt_px] creates -1px top margin. *)

val neg_mr_px : t
(** [neg_mr_px] creates -1px right margin. *)

val neg_mb_px : t
(** [neg_mb_px] creates -1px bottom margin. *)

val neg_ml_px : t
(** [neg_ml_px] creates -1px left margin. *)

val gap : int -> t
(** [gap n] sets spacing between items in flex/grid containers using Tailwind
    scale. More modern and flexible than using margins on children.

    Example:
    {[
      div
        ~tw:[ flex; gap 4 ]
        [
          (* All children will have 1rem space between them *)
          button [ txt "Save" ];
          button [ txt "Cancel" ];
        ]
    ]} *)

val gap_x : int -> t
(** [gap_x n] sets only horizontal gaps in flex/grid containers. *)

val gap_y : int -> t
(** [gap_y n] sets only vertical gaps in flex/grid containers. *)

(** {2 Special gap values} *)

val gap_px : t
(** [gap_px] sets 1px gap between items. *)

val gap_full : t
(** [gap_full] sets 100% gap between items. *)

(** {1 Sizing}
    @see <https://tailwindcss.com/docs/width> Width and Height *)

val w : int -> t
(** [w n] sets element width using Tailwind scale (n × 0.25rem).

    Common patterns:
    - [w 24]: Fixed width of 6rem (96px)
    - [w 0]: Zero width Use variants for special cases like [w_full],
      [w_screen], etc. *)

val h : int -> t
(** [h n] sets element height using Tailwind scale (n × 0.25rem).

    Common patterns:
    - [h 16]: Fixed height of 4rem (64px)
    - [h 0]: Zero height Use variants for special cases like [h_full],
      [h_screen], etc. *)

val min_w : int -> t
(** [min_w n] sets minimum width using Tailwind scale. *)

val min_h : int -> t
(** [min_h n] sets minimum height using Tailwind scale. *)

val max_w : int -> t
(** [max_w n] sets maximum width using Tailwind scale. For named sizes, use
    variants like [max_w_md], [max_w_xl], etc. *)

val max_h : int -> t
(** [max_h n] sets maximum height using Tailwind scale. *)

(** {2 Common size utilities} *)

val w_full : t
(** [w_full] sets 100% of parent width. Very commonly used. *)

val h_full : t
(** [h_full] sets 100% of parent height. Very commonly used. *)

val w_fit : t
(** [w_fit] uses fit-content sizing. Common for buttons and chips. *)

val h_fit : t
(** [h_fit] uses fit-content sizing for height. *)

val w_screen : t
(** [w_screen] sets width to full viewport width (100vw). *)

val h_screen : t
(** [h_screen] sets height to full viewport height (100vh). *)

val w_min : t
(** [w_min] sets width to min-content (minimum intrinsic size). *)

val h_min : t
(** [h_min] sets height to min-content (minimum intrinsic size). *)

val w_max : t
(** [w_max] sets width to max-content (maximum intrinsic size). *)

val h_max : t
(** [h_max] sets height to max-content (maximum intrinsic size). *)

val min_h_screen : t
(** [min_h_screen] sets viewport minimum height. Common for hero sections. *)

val min_w_full : t
(** [min_w_full] sets minimum width to 100% of parent. *)

val min_h_full : t
(** [min_h_full] sets minimum height to 100% of parent. *)

val max_w_2xl : t
(** [max_w_2xl] sets 42rem maximum width. Common for article text. *)

val max_w_3xl : t
(** [max_w_3xl] sets 48rem maximum width. Common for content sections. *)

val max_w_4xl : t
(** [max_w_4xl] sets 56rem maximum width. Common for content sections. *)

val max_w_none : t
(** [max_w_none] removes the maximum width constraint. *)

val max_w_full : t
(** [max_w_full] sets maximum width to 100% of parent. *)

val max_h_full : t
(** [max_h_full] sets maximum height to 100% of parent. *)

(** {1 Layout}
    @see <https://tailwindcss.com/docs/display> Display
    @see <https://tailwindcss.com/docs/flex> Flexbox
    @see <https://tailwindcss.com/docs/position> Position *)

val block : t
(** Makes element a block - takes full width, stacks vertically. Default for
    div, p, h1-h6. *)

val inline : t
(** Makes element inline - flows with text, width based on content. Default for
    span, a, strong. *)

val inline_block : t
(** Hybrid - flows inline but can have width/height like a block. *)

val flex : t
(** Creates a flex container for flexible layouts. Children can be arranged
    horizontally/vertically with gaps.

    Example:
    {[
      div ~tw:[ flex; items_center; gap 4 ] [ icon; span [ txt "Dashboard" ] ]
    ]} *)

val inline_flex : t
(** Like flex but the container itself is inline. *)

val grid : t
(** Creates a grid container for 2D layouts with rows and columns. More
    structured than flexbox. *)

val inline_grid : t
(** Like grid but the container itself is inline. *)

val hidden : t
(** Completely hides element - no space reserved, screen readers skip it. Use
    [sr_only] to hide visually but keep accessible. *)

val flex_col : t
(** Stacks flex items vertically (top to bottom). Changes the main axis to
    vertical. *)

val flex_row : t
(** Arranges flex items horizontally (left to right). This is the default for
    flex containers. *)

val flex_row_reverse : t
(** Arranges flex items horizontally but reversed (right to left). *)

val flex_col_reverse : t
(** Stacks flex items vertically but reversed (bottom to top). *)

val flex_wrap : t
(** Flex wrap. *)

val flex_wrap_reverse : t
(** Flex wrap reverse. *)

val flex_nowrap : t
(** Prevent flex items from wrapping. *)

val flex_1 : t
(** Item grows and shrinks as needed, ignoring initial size. Perfect for
    elements that should fill available space equally.

    Example:
    {[
      (* Three columns of equal width *)
      div ~tw:[ flex ]
        [
          div ~tw:[ flex_1 ] [ content1 ];
          (* 33.33% *)
          div ~tw:[ flex_1 ] [ content2 ];
          (* 33.33% *)
          div ~tw:[ flex_1 ] [ content3 ];
          (* 33.33% *)
        ]
    ]} *)

val flex_auto : t
(** Item grows and shrinks but considers its content size. Good for text that
    should expand but not squish too much. *)

val flex_initial : t
(** Item can shrink but won't grow beyond its content. Default flex behavior. *)

val flex_none : t
(** Item stays at its natural size - won't grow or shrink. Use for fixed-size
    elements like icons or buttons. *)

val flex_grow : t
(** Allow flex item to grow. *)

val flex_grow_0 : t
(** Prevent flex item from growing. *)

val flex_shrink : t
(** Allow flex item to shrink. *)

val flex_shrink_0 : t
(** Prevent flex item from shrinking. *)

val items_start : t
(** Aligns flex/grid items to the start of their container's cross axis. In a
    row, this is the top. In a column, this is the left. *)

val items_end : t
(** Aligns flex/grid items to the end of their container's cross axis. In a row,
    this is the bottom. In a column, this is the right. *)

val items_center : t
(** Centers flex/grid items along the container's cross axis. Very common for
    vertically centering content. *)

val items_baseline : t
(** Aligns flex/grid items along their text baseline. Useful when items have
    different font sizes. *)

val items_stretch : t
(** Stretches items to fill the container's cross axis. Default behavior - makes
    all items same height in a row. *)

val justify_start : t
(** Packs flex/grid items toward the start of the main axis. In a row (default),
    items align left. In a column, items align top. *)

val justify_end : t
(** Packs flex/grid items toward the end of the main axis. In a row, items align
    right. In a column, items align bottom. *)

val justify_center : t
(** Centers flex/grid items along the main axis. Common for centering content
    horizontally. *)

val justify_between : t
(** Distributes items evenly - first at start, last at end, equal space between.
*)

val justify_around : t
(** Distributes items evenly with equal space around each item. Items have
    half-size space on the edges. *)

val justify_evenly : t
(** Distributes items evenly with equal space between and around all items. All
    gaps including edges are the same size. *)

(** {2 Align Content (Multi-line Flex/Grid)} *)

val content_start : t
(** Aligns lines/rows to the start of the container's cross axis. For flex
    containers with multiple lines or grid containers with multiple rows. *)

val content_end : t
(** Aligns lines/rows to the end of the container's cross axis. *)

val content_center : t
(** Centers lines/rows along the container's cross axis. *)

val content_between : t
(** Distributes lines/rows evenly - first at start, last at end. *)

val content_around : t
(** Distributes lines/rows evenly with equal space around each. *)

val content_evenly : t
(** Distributes lines/rows evenly with equal space between and around all. *)

val content_stretch : t
(** Stretches lines/rows to fill the container's cross axis. *)

(** {2 Place Content (Grid Shorthand)} *)

val place_content_start : t
(** Aligns content to start in both axes. Shorthand for align-content and
    justify-content. *)

val place_content_end : t
(** Aligns content to end in both axes. *)

val place_content_center : t
(** Centers content in both axes. Perfect for centering grid content. *)

val place_content_between : t
(** Distributes content with space between in both axes. *)

val place_content_around : t
(** Distributes content with space around in both axes. *)

val place_content_evenly : t
(** Distributes content evenly in both axes. *)

val place_content_stretch : t
(** Stretches content to fill both axes. *)

(** {2 Place Items (Grid Shorthand)} *)

val place_items_start : t
(** Aligns items to start in both axes. Shorthand for align-items and
    justify-items. *)

val place_items_end : t
(** Aligns items to end in both axes. *)

val place_items_center : t
(** Centers items in both axes. Common for centering grid items. *)

val place_items_stretch : t
(** Stretches items to fill both axes (default). *)

(** {2 Self Alignment} *)

val self_auto : t
(** Uses parent's align-items value (default). *)

val self_start : t
(** Aligns item to the start of the container's cross axis. *)

val self_end : t
(** Aligns item to the end of the container's cross axis. *)

val self_center : t
(** Centers item along the container's cross axis. *)

val self_baseline : t
(** Aligns item along text baseline. *)

val self_stretch : t
(** Stretches item to fill the container's cross axis. *)

(** {2 Justify Self (Grid Items)} *)

val justify_self_auto : t
(** Uses parent's justify-items value (default). *)

val justify_self_start : t
(** Aligns item to the start of its grid area. *)

val justify_self_end : t
(** Aligns item to the end of its grid area. *)

val justify_self_center : t
(** Centers item within its grid area. *)

val justify_self_stretch : t
(** Stretches item to fill its grid area. *)

(** {2 Place Self (Grid Shorthand)} *)

val place_self_auto : t
(** Uses parent's place-items value. Shorthand for align-self and justify-self.
*)

val place_self_start : t
(** Aligns to start in both axes. *)

val place_self_end : t
(** Aligns to end in both axes. *)

val place_self_center : t
(** Centers in both axes. *)

val place_self_stretch : t
(** Stretches to fill both axes. *)

val grid_cols : int -> t
(** [grid_cols n] creates a grid with n equal columns.

    Example:
    {[
      (* 3-column card layout *)
      div
        ~tw:[ grid; grid_cols 3; gap 4 ]
        [
          card1;
          card2;
          card3;
          (* Each takes 1 column *)
          card4;
          card5;
          card6;
          (* Wraps to next row *)
        ]
    ]} *)

val grid_rows : int -> t
(** [grid_rows n] creates a grid with n equal rows. *)

val static : t
(** Default positioning - element flows normally in the document. Ignores
    top/right/bottom/left properties. *)

val relative : t
(** Position relative to element's normal position. Can use
    top/right/bottom/left to nudge from original spot. Creates positioning
    context for absolute children. *)

val absolute : t
(** Removes from normal flow, positions relative to nearest positioned parent.
    Use with top/right/bottom/left for exact placement.

    Example:
    {[
      (* Notification badge on icon *)
      div ~tw:[ relative ]
        [ icon; span ~tw:[ absolute; top (-2); right (-2) ] [ txt "3" ] ]
    ]} *)

val fixed : t
(** Like absolute but relative to viewport - stays in place when scrolling. *)

val sticky : t
(** Hybrid - scrolls normally until it reaches viewport edge, then sticks. Great
    for table headers and sidebars that follow scroll. *)

val inset_0 : t
(** Set all inset values to 0. *)

val inset_x_0 : t
(** Set left and right to 0. *)

val inset_y_0 : t
(** Set top and bottom to 0. *)

val top : int -> t
(** [top n] sets top position value. *)

val right : int -> t
(** [right n] sets right position value. *)

val bottom : int -> t
(** [bottom n] sets bottom position value. *)

val left : int -> t
(** [left n] sets left position value. *)

val z : int -> t
(** [z n] controls stacking order - higher numbers appear on top.

    Common values:
    - [z 0]: Default layer
    - [z 10]: Dropdowns, tooltips
    - [z 20]: Modals
    - [z 30]: Notifications
    - [z 40]: Critical overlays
    - [z 50]: Maximum (use sparingly)

    Negative values like [z (-1)] place elements behind others. *)

(** {1 Typography}
    @see <https://tailwindcss.com/docs/font-size> Typography *)

val text_xs : t
(** Extra small text (12px) - for captions, labels, fine print. *)

val text_sm : t
(** Small text (14px) - for secondary content, form labels. *)

val text_base : t
(** Base text (16px) - default body text size, good readability. *)

val text_lg : t
(** Large text (18px) - for emphasized paragraphs, lead text. *)

val text_xl : t
(** Extra large text (20px) - for section introductions. *)

val text_2xl : t
(** 2x large text size (1.5rem). *)

val text_3xl : t
(** 3x large text size (1.875rem). *)

val text_4xl : t
(** 4x large text size (2.25rem). *)

val text_5xl : t
(** 5x large text size (3rem). *)

val font_thin : t
(** Thinnest font weight (100) - use sparingly, may not be visible with all
    fonts. *)

val font_light : t
(** Light font weight (300) - for subtle, delicate text. *)

val font_normal : t
(** Normal font weight (400) - default for body text. *)

val font_medium : t
(** Medium font weight (500) - slightly bolder than normal, good for UI labels.
*)

val font_semibold : t
(** Semi-bold font weight (600) - for subheadings and emphasis. *)

val font_bold : t
(** Bold font weight (700) - for headings and strong emphasis. *)

val font_extrabold : t
(** Extra bold font weight (800) - for major headings. *)

val font_black : t
(** Heaviest font weight (900) - for maximum impact, hero text. *)

val font_sans : t
(** Sans-serif font family. *)

val font_serif : t
(** Serif font family. *)

val font_mono : t
(** Monospace font family. *)

val italic : t
(** Italic text style. *)

val not_italic : t
(** Remove italic text style. *)

val underline : t
(** Underlined text decoration. *)

val line_through : t
(** Line-through text decoration. *)

val no_underline : t
(** Remove text decoration. *)

(** {2 Text Transform} *)

val uppercase : t
(** Transform text to UPPERCASE. Useful for labels, badges, and emphasis. *)

val lowercase : t
(** Transform text to lowercase. Less common but useful for specific designs. *)

val capitalize : t
(** Capitalize The First Letter Of Each Word. Good for titles and headings. *)

val normal_case : t
(** Remove text transformation (default). Use to override parent text-transform.
*)

(** {2 Text Decoration Style} *)

val underline_solid : t
(** Sets text decoration style to solid (default). *)

val underline_double : t
(** Sets text decoration style to double line. *)

val underline_dotted : t
(** Sets text decoration style to dotted line. *)

val underline_dashed : t
(** Sets text decoration style to dashed line. *)

val underline_wavy : t
(** Sets text decoration style to wavy line. Good for spell check indicators. *)

(** {2 Text Underline Offset} *)

val underline_offset_auto : t
(** Sets text underline offset to auto (browser default). *)

val underline_offset_0 : t
(** Sets text underline offset to 0 (underline touches text). *)

val underline_offset_1 : t
(** Sets text underline offset to 1px. *)

val underline_offset_2 : t
(** Sets text underline offset to 2px. *)

val underline_offset_4 : t
(** Sets text underline offset to 4px. *)

val underline_offset_8 : t
(** Sets text underline offset to 8px. *)

val text_left : t
(** Left-aligned text. *)

val text_center : t
(** Center-aligned text. *)

val text_right : t
(** Right-aligned text. *)

val text_justify : t
(** Justified text. *)

val leading_none : t
(** Line height 1 - text lines touch. Only for large display text. *)

val leading_tight : t
(** Line height 1.25 - compact spacing for headings. *)

val leading_snug : t
(** Line height 1.375 - slightly tighter than normal. *)

val leading_normal : t
(** Line height 1.5 - default, optimal readability for body text. *)

val leading_relaxed : t
(** Line height 1.625 - more open, easier scanning for long text. *)

val leading_loose : t
(** Line height 2 - very open, good for short text blocks that need breathing
    room. *)

val leading_6 : t
(** Line height of 1.5rem. *)

val tracking_tighter : t
(** Letter spacing of -0.05em. *)

val tracking_tight : t
(** Letter spacing of -0.025em. *)

val tracking_normal : t
(** Letter spacing of 0. *)

val tracking_wide : t
(** Letter spacing of 0.025em. *)

val tracking_wider : t
(** Letter spacing of 0.05em. *)

val tracking_widest : t
(** Letter spacing of 0.1em. *)

val whitespace_normal : t
(** Normal whitespace handling. *)

val whitespace_nowrap : t
(** Prevent text wrapping. *)

val whitespace_pre : t
(** Preserve whitespace. *)

val whitespace_pre_line : t
(** Preserve line breaks. *)

val whitespace_pre_wrap : t
(** Preserve whitespace and wrap. *)

val antialiased : t
(** Enables antialiased font smoothing for better text rendering. This is
    usually the default but can be explicitly set. *)

(** {1 Borders}
    @see <https://tailwindcss.com/docs/border-width> Borders *)

val border : t
(** Default border (1px). Same as {!border_xs}. *)

val border_none : t
(** No border (0px). *)

val border_xs : t
(** Extra small border (1px). Same as {!border}. *)

val border_sm : t
(** Small border (2px). *)

val border_md : t
(** Medium border (4px). *)

val border_lg : t
(** Large border (4px). *)

val border_xl : t
(** Extra large border (8px). *)

val border_2xl : t
(** 2x large border (8px). *)

val border_3xl : t
(** 3x large border (8px). *)

val border_full : t
(** Full border (8px). *)

val border_t : t
(** Top border (1px). *)

val border_r : t
(** Right border (1px). *)

val border_b : t
(** Bottom border (1px). *)

val border_l : t
(** Left border (1px). *)

val rounded_none : t
(** Sharp corners (0px). *)

val rounded_sm : t
(** Subtle rounding (2px). *)

val rounded : t
(** Default rounding (4px). Same as {!rounded_md}. *)

val rounded_md : t
(** Medium rounding (6px). *)

val rounded_lg : t
(** Noticeably rounded (8px). *)

val rounded_xl : t
(** Extra rounded (12px). *)

val rounded_2xl : t
(** 2x rounded (16px). *)

val rounded_3xl : t
(** 3x rounded (24px). *)

val rounded_full : t
(** Fully rounded (9999px) - makes circles/pills. *)

val border_collapse : t
(** Collapse table borders. *)

val border_separate : t
(** Separate table borders. *)

val border_spacing : int -> t
(** [border_spacing n] sets border spacing using spacing scale. *)

(** {1 Effects & Filters}
    @see <https://tailwindcss.com/docs/box-shadow> Effects
    @see <https://tailwindcss.com/docs/blur> Filters
    @see <https://tailwindcss.com/docs/backdrop-blur> Backdrop Filters *)

val shadow_none : t
(** Remove shadow. *)

val shadow_sm : t
(** Subtle shadow for cards. *)

val shadow : t
(** Default shadow. Same as {!shadow_md}. *)

val shadow_md : t
(** Medium shadow. Same as {!shadow}. *)

val shadow_lg : t
(** Large shadow for modals, dropdowns. *)

val shadow_xl : t
(** Extra large shadow. *)

val shadow_2xl : t
(** 2x large shadow. *)

val shadow_inner : t
(** Inset shadow for pressed/sunken effect. *)

val opacity : int -> t
(** [opacity n] controls transparency (0-100).
    - 0: Fully transparent (invisible but takes space)
    - 50: Half transparent
    - 100: Fully opaque (default). *)

val outline_none : t
(** Remove outline. *)

val ring_none : t
(** Remove ring. *)

val ring_xs : t
(** Extra small ring (1px). *)

val ring_sm : t
(** Small ring (2px). *)

val ring : t
(** Default ring (3px). Same as {!ring_md}. *)

val ring_md : t
(** Medium ring (3px). Same as {!ring}. *)

val ring_lg : t
(** Large ring (4px). *)

val ring_xl : t
(** Extra large ring (8px). *)

(** Rings use box-shadow and don't affect layout. By default, rings are blue
    with 50% opacity. To customize:
    - Use [ring_color] to change color: [ring_sm; ring_color purple 500]
    - Rings are often used for focus states: [on_focus [ ring ]]
    - Unlike borders, rings don't take up space in the layout. *)

val ring_color : color -> int -> t
(** [ring_color color shade] sets the color of outline rings. *)

val isolate : t
(** Creates a new stacking context to isolate z-index behavior. Useful to
    prevent z-index values from affecting elements outside this container. *)

val brightness : int -> t
(** [brightness n] sets brightness filter (0-200, where 100 is normal). *)

val contrast : int -> t
(** [contrast n] sets contrast filter (0-200, where 100 is normal). *)

val blur_none : t
(** No blur. *)

val blur_xs : t
(** Extra small blur (2px). *)

val blur_sm : t
(** Small blur (4px). *)

val blur : t
(** Default blur (8px). Same as {!blur_md}. *)

val blur_md : t
(** Medium blur (12px). *)

val blur_lg : t
(** Large blur (16px). *)

val blur_xl : t
(** Extra large blur (24px). *)

val blur_2xl : t
(** 2x large blur (40px). *)

val blur_3xl : t
(** 3x large blur (64px). *)

val grayscale : int -> t
(** [grayscale n] sets the grayscale filter (0-100). *)

val saturate : int -> t
(** [saturate n] sets the saturation filter (0-200, where 100 is normal). *)

val sepia : int -> t
(** [sepia n] sets the sepia filter (0-100). *)

val invert : int -> t
(** [invert n] sets the invert filter (0-100). *)

val hue_rotate : int -> t
(** [hue_rotate n] rotates the hue by n degrees. *)

val backdrop_brightness : int -> t
(** [backdrop_brightness n] applies brightness filter to content behind element.
    Values: 0-200, where 100 is normal. Useful for frosted glass effects.

    Example:
    {[
      (* Frosted glass overlay *)
      div
        ~tw:
          [
            backdrop_brightness 75;
            backdrop_saturate 150;
            bg ~shade:100 white;
            opacity 30;
          ]
        [ txt "Overlay content" ]
    ]} *)

val backdrop_contrast : int -> t
(** [backdrop_contrast n] sets backdrop contrast filter (0-200, where 100 is
    normal). *)

val backdrop_opacity : int -> t
(** [backdrop_opacity n] sets backdrop opacity filter (0-100). *)

val backdrop_saturate : int -> t
(** [backdrop_saturate n] sets backdrop saturation filter (0-200, where 100 is
    normal). *)

val backdrop_blur_none : t
(** No backdrop blur. *)

val backdrop_blur_xs : t
(** Extra small backdrop blur (2px). *)

val backdrop_blur_sm : t
(** Small backdrop blur (4px). *)

val backdrop_blur : t
(** Default backdrop blur (8px). Same as {!backdrop_blur_md}. *)

val backdrop_blur_md : t
(** Medium backdrop blur (12px). *)

val backdrop_blur_lg : t
(** Large backdrop blur (16px). *)

val backdrop_blur_xl : t
(** Extra large backdrop blur (24px). *)

val backdrop_blur_2xl : t
(** 2x large backdrop blur (40px). *)

val backdrop_blur_3xl : t
(** 3x large backdrop blur (64px). *)

(** {1 Transitions & Animations}
    @see <https://tailwindcss.com/docs/animation> Animations *)

val transition_none : t
(** No transition. *)

val transition_all : t
(** Transition all properties. *)

val transition_colors : t
(** Smoothly animates color changes (background, text, border). Essential for
    hover effects to feel polished.

    Example:
    {[
      button
        ~tw:
          [
            bg blue;
            transition_colors;
            (* Smooth color change *)
            on_hover [ bg ~shade:700 blue ];
          ]
    ]}

    Duration is 150ms by default. *)

val transition_opacity : t
(** Transition opacity. *)

val transition_shadow : t
(** Transition box shadow. *)

val transition_transform : t
(** Transition transform. *)

val duration : int -> t
(** [duration n] sets transition duration in milliseconds. *)

val ease_linear : t
(** Linear transition timing function. *)

val ease_in : t
(** Ease-in transition timing function. *)

val ease_out : t
(** Ease-out transition timing function. *)

val ease_in_out : t
(** Ease-in-out transition timing function. *)

val scale : int -> t
(** [scale n] resizes element by percentage (100 = normal size).

    Examples:
    - [scale 95]: Slightly smaller (95%)
    - [scale 100]: Normal size
    - [scale 105]: Slightly larger (105%) - nice for hover effects
    - [scale 150]: 1.5x larger

    Often combined with transition_transform for smooth scaling. *)

val rotate : int -> t
(** [rotate n] sets rotate transformation (degrees). *)

val translate_x : int -> t
(** [translate_x n] sets horizontal translation. *)

val translate_y : int -> t
(** [translate_y n] sets vertical translation. *)

val transform : t
(** Enable transform utilities. *)

val transform_none : t
(** Disable transforms. *)

val transform_gpu : t
(** Use GPU acceleration for transforms. *)

val animate_none : t
(** No animation. *)

val animate_spin : t
(** Spin animation - rotates element 360° continuously. Perfect for loading
    spinners. *)

val animate_ping : t
(** Ping animation - scales and fades out like a radar ping. Great for
    notification badges or attention-grabbing indicators. *)

val animate_pulse : t
(** Pulse animation - gently fades in and out. Useful for skeleton screens or
    loading placeholders. *)

val animate_bounce : t
(** Bounce animation - makes element bounce up and down. Good for scroll
    indicators or playful UI elements. *)

(** {1 Tables}
    @see <https://tailwindcss.com/docs/table-layout> Tables *)

val table_auto : t
(** Automatic table layout. *)

val table_fixed : t
(** Fixed table layout. *)

(** {1 Forms}
    @see <https://github.com/tailwindlabs/tailwindcss-forms> Forms Plugin *)

val form_input : t
(** Base styles for input elements - resets browser defaults and provides
    consistent styling across browsers. Use with input elements. *)

val form_textarea : t
(** Base styles for textarea elements - provides consistent cross-browser
    appearance and behavior. *)

val form_select : t
(** Base styles for select dropdowns - normalizes appearance across browsers
    while maintaining native functionality. *)

val form_checkbox : t
(** Base styles for checkbox inputs - provides custom styling while maintaining
    accessibility. *)

val form_radio : t
(** Base styles for radio inputs - provides custom styling while maintaining
    accessibility. *)

(** {1 Interactivity & Scroll}
    @see <https://tailwindcss.com/docs/scroll-snap-type> Scroll Snap *)

val cursor_auto : t
(** Automatic cursor. *)

val cursor_default : t
(** Default cursor. *)

val cursor_pointer : t
(** Pointer cursor. *)

val cursor_wait : t
(** Wait cursor. *)

val cursor_move : t
(** Move cursor. *)

val cursor_not_allowed : t
(** Not-allowed cursor. *)

val select_none : t
(** Disable text selection. *)

val select_text : t
(** Enable text selection. *)

val select_all : t
(** Select all text on focus. *)

val select_auto : t
(** Automatic text selection. *)

val pointer_events_none : t
(** Disable pointer events. *)

val pointer_events_auto : t
(** Enable pointer events. *)

val overflow_auto : t
(** Automatic overflow handling. *)

val overflow_hidden : t
(** Clips content that exceeds container bounds - no scrolling. Common for:
    - Image containers to prevent overflow
    - Modals to prevent body scrolling
    - Containers with rounded corners. *)

val overflow_visible : t
(** Content can extend beyond container bounds (default behavior). *)

val overflow_scroll : t
(** Always shows scrollbars even if content fits. Use overflow_auto instead for
    better UX. *)

val overflow_x_auto : t
(** Auto horizontal overflow. *)

val overflow_x_hidden : t
(** Hide horizontal overflow. *)

val overflow_x_visible : t
(** Show horizontal overflow. *)

val overflow_x_scroll : t
(** Always show horizontal scrollbar. *)

val overflow_y_auto : t
(** Auto vertical overflow. *)

val overflow_y_hidden : t
(** Hide vertical overflow. *)

val overflow_y_visible : t
(** Show vertical overflow. *)

val overflow_y_scroll : t
(** Always show vertical scrollbar. *)

val snap_none : t
(** No scroll snapping. *)

val snap_x : t
(** Horizontal scroll snapping for carousel-like interfaces. Must be used with
    snap_start/center/end on children.

    Example:
    {[
      (* Horizontal carousel *)
      div
        ~tw:[ flex; overflow_x_auto; snap_x; snap_mandatory ]
        [
          div ~tw:[ snap_center; flex_shrink_0; w full ] [ img1 ];
          div ~tw:[ snap_center; flex_shrink_0; w full ] [ img2 ];
        ]
    ]} *)

val snap_y : t
(** Vertical scroll snapping. Similar to snap_x but for vertical scrolling. *)

val snap_both : t
(** Both horizontal and vertical scroll snapping. *)

val snap_mandatory : t
(** Mandatory scroll snapping. *)

val snap_proximity : t
(** Proximity-based scroll snapping. *)

val snap_start : t
(** Snap to start of container. *)

val snap_end : t
(** Snap to end of container. *)

val snap_center : t
(** Snap to center of container. *)

val snap_align_none : t
(** No snap alignment. *)

val snap_normal : t
(** Normal snap stop behavior. *)

val snap_always : t
(** Always stop at snap positions. *)

val scroll_auto : t
(** Auto scroll behavior. *)

val scroll_smooth : t
(** Smooth scroll behavior. *)

val object_contain : t
(** Scales image to fit container while preserving aspect ratio. The entire
    image will be visible but may have empty space.

    Example:
    {[
      img ~tw:[ object_contain; h (int 48); w full ] ~src:"..." ()
    ]} *)

val object_cover : t
(** Scales image to cover entire container while preserving aspect ratio. Parts
    of the image may be clipped to fill the container. *)

val object_fill : t
(** Stretches image to fill container, ignoring aspect ratio. May cause
    distortion. *)

val object_none : t
(** Image retains original size, may overflow or underflow container. *)

val object_scale_down : t
(** Scales down only if image is larger than container, otherwise original size.
*)

val object_top : t
(** Sets object position to top. *)

val object_right : t
(** Sets object position to right. *)

val object_bottom : t
(** Sets object position to bottom. *)

val object_left : t
(** Sets object position to left. *)

val object_center : t
(** Sets object position to center. *)

val appearance_none : t
(** Removes default browser styling from form elements. *)

val resize_none : t
(** Prevents textarea resizing. *)

val resize_y : t
(** Allows vertical resizing only. *)

val resize_x : t
(** Allows horizontal resizing only. *)

val resize : t
(** Allows both horizontal and vertical resizing. *)

val will_change_auto : t
(** Sets will-change to auto. *)

val will_change_scroll : t
(** Optimizes for scroll position changes. *)

val will_change_contents : t
(** Optimizes for content changes. *)

val will_change_transform : t
(** Optimizes for transform changes. *)

val contain_none : t
(** No containment. *)

val contain_content : t
(** Contains layout and paint. *)

val contain_layout : t
(** Contains layout only. *)

val contain_paint : t
(** Contains paint only. *)

val contain_size : t
(** Contains size. *)

val sr_only : t
(** Screen reader only - visually hides content while keeping it accessible. Use
    this for content that should be read by screen readers but not visible.

    Example:
    {[
      label
        [
          span ~tw:[ sr_only ] [ txt "Search" ];
          input ~at:[ At.type_ "search" ] [];
        ]
    ]} *)

val not_sr_only : t
(** Reverses sr_only - makes previously screen-reader-only content visible. *)

(** {1 State & Responsive Modifiers} *)

val on_hover : t list -> t
(** [on_hover styles] applies multiple styles on hover. *)

val on_focus : t list -> t
(** [on_focus styles] applies multiple styles on focus. *)

val focus_visible : t
(** Shows focus ring only for keyboard navigation, not mouse clicks. This
    provides better UX by showing focus indicators only when needed. *)

val on_active : t list -> t
(** [on_active styles] applies styles when the element is being actively
    interacted with (e.g., button being pressed). *)

val on_disabled : t list -> t
(** [on_disabled styles] applies styles when the element is disabled. *)

val on_group_hover : t list -> t
(** [on_group_hover styles] applies styles to this element when its parent with
    the [group] class is hovered. The parent must have the [group] class for
    this to work.

    See {!group} for usage examples. *)

val on_group_focus : t list -> t
(** [on_group_focus styles] applies styles to this element when its parent with
    the [group] class is focused. The parent must have the [group] class for
    this to work. *)

val on_dark : t list -> t
(** [on_dark styles] applies styles when dark mode is enabled. Dark mode is
    typically controlled by a [dark] class on the HTML element or by system
    preferences. *)

val on_sm : t list -> t
(** [on_sm styles] applies styles on small screens and up (640px+).
    Mobile-first: base styles apply to mobile, these override for larger
    screens.

    Example:
    {[
      div
        ~tw:
          [
            text_base;
            (* Mobile: normal text *)
            on_sm [ text_lg ] (* Tablet+: larger text *);
          ]
    ]} *)

val on_md : t list -> t
(** [on_md styles] applies styles on medium screens and up (768px+). Typically
    tablet-sized devices. *)

val on_lg : t list -> t
(** [on_lg styles] applies styles on large screens and up (1024px+). Typically
    laptops and smaller desktops. *)

val on_xl : t list -> t
(** [on_xl styles] applies styles on extra large screens and up (1280px+).
    Desktop monitors. *)

val on_2xl : t list -> t
(** [on_2xl styles] applies styles on 2x large screens and up (1536px+). Large
    desktop monitors. *)

val peer : t
(** Marker class for peer relationships. Use this on an element to enable
    peer-based styling on its siblings.

    Example:
    {[
      (* When checkbox is checked, label text becomes bold *)
      input ~at:[ At.type_ "checkbox" ] ~tw:[ peer ] [];
      label ~tw:[ peer_checked font_bold ] [ txt "Accept terms" ]
    ]} *)

val group : t
(** Marker class for group relationships. Add this to a parent element to enable
    group-based styling on its children.

    Example:
    {[
      (* When hovering the card, both title and description change color *)
      div
        ~tw:[ group; p (int 4); border ]
        [
          h3 ~tw:[ on_group_hover [ text blue ] ] [ txt "Title" ];
          p
            ~tw:[ on_group_hover [ text ~shade:700 gray ] ]
            [ txt "Description" ];
        ]
    ]} *)

val on_peer_hover : t list -> t
(** [on_peer_hover styles] applies styles when a sibling peer element is
    hovered. *)

val on_peer_focus : t list -> t
(** [on_peer_focus styles] applies styles when a sibling peer element is
    focused. *)

val on_peer_checked : t list -> t
(** [on_peer_checked styles] applies multiple styles when a sibling peer
    checkbox/radio is checked. *)

val on_aria_checked : t list -> t
(** [on_aria_checked styles] applies multiple styles when aria-checked="true".
*)

val on_aria_expanded : t list -> t
(** [on_aria_expanded styles] applies multiple styles when aria-expanded="true".
*)

val on_aria_selected : t list -> t
(** [on_aria_selected styles] applies multiple styles when aria-selected="true".
*)

val on_aria_disabled : t list -> t
(** [on_aria_disabled styles] applies styles when aria-disabled="true". Ensures
    disabled states are properly styled for accessibility. *)

(** {2 Data Attribute Variants}
    @see <https://tailwindcss.com/docs/hover-focus-and-other-states#data-attributes>
      Data Attributes *)

val data_state : string -> t -> t
(** [data_state value style] applies style when data-state="value". Common in UI
    libraries for component states.

    Example:
    {[
      (* Styles applied when data-state="open" *)
      div ~tw:[ data_state "open" (opacity 100); opacity 0 ] [ content ]
    ]} *)

val data_variant : string -> t -> t
(** [data_variant value style] applies style when data-variant="value". Useful
    for component variants without JavaScript. *)

val on_data_active : t list -> t
(** [on_data_active styles] applies styles when data-active attribute is
    present. *)

val on_data_inactive : t list -> t
(** [on_data_inactive styles] applies styles when data-inactive attribute is
    present. *)

val data_custom : string -> string -> t -> t
(** [data_custom key value style] applies style when data-[key]="[value]". *)

(** {1 Prose Typography}

    The prose classes provide beautiful typographic defaults for long-form
    content like articles, blog posts, or documentation. They automatically
    style headings, paragraphs, lists, code blocks, and more.

    @see <https://tailwindcss.com/docs/typography-plugin> Typography Plugin *)

val prose : t
(** Default prose styling for article-like content. Automatically styles h1-h6,
    p, ul, ol, blockquote, code, and more.

    Example:
    {[
      article
        ~tw:[ prose; prose_lg; max_w none ]
        [
          h1 [ txt "Article Title" ];
          p [ txt "This paragraph will be beautifully styled..." ];
          (* All child elements get appropriate typography *)
        ]
    ]} *)

val prose_sm : t
(** Small prose styling (0.875rem base font). *)

val prose_lg : t
(** Large prose styling (1.125rem base font). *)

val prose_xl : t
(** Extra large prose styling (1.25rem base font). *)

val prose_2xl : t
(** 2x large prose styling (1.5rem base font). *)

val prose_gray : t
(** Gray prose color theme. *)

val prose_slate : t
(** Slate prose color theme. *)

val prose_stylesheet : unit -> Css.stylesheet
(** [prose_stylesheet ()] generates a complete prose stylesheet with all
    typography rules. This includes descendant selectors for all HTML elements
    (h1-h6, p, a, strong, em, code, pre, blockquote, ul, ol, li, etc.) and all
    size variants.

    Usage:
    {[
      let prose_css = prose_stylesheet () |> Css.to_string ~minify:true in
      (* Include this CSS in your page to enable full prose functionality *)
    ]} *)

val line_clamp : int -> t
(** [line_clamp n] truncates text to n lines with ellipsis. Use 0 to remove
    clamping. Useful for consistent card heights.

    Example:
    {[
      p
        ~tw:[ line_clamp 3 ]
        [ txt "This very long text will be truncated after three lines..." ]
    ]} *)

(** {1 Class Generation & Internals} *)

val to_classes : t list -> string
(** [to_classes styles] converts your style list to a CSS class string. This is
    the main function you'll use with HTML elements.

    Example:
    {[
      let button_styles = [ bg blue; text white; px (int 4); py (int 2) ] in
      button ~at:[ At.class_ (to_classes button_styles) ] [ txt "Click" ]
    ]} *)

val pp : t -> string
(** [pp style] generates a class name from a style. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string class_str] parses a Tailwind class string into a style.

    Example:
    {[
      of_string "bg-blue-500" = Ok (bg_blue)
      of_string "p-4" = Ok (p (int 4))
      of_string "text-center" = Ok text_center
      of_string "unknown-class" = Error (`Msg "Unknown class: unknown-class")
    ]}

    Returns [Error (`Msg reason)] if the class string is not recognized. *)

val classes_to_string : t list -> string
(** [classes_to_string styles] is an alias for to_classes. *)

val color_to_string : color -> string
(** [color_to_string c] converts a color to its string representation. *)

(** {2 CSS Generation}

    This library generates Tailwind-like class names using [to_classes].

    {b Important}: Class tracking and CSS file generation should be handled by
    the library user. For example, the {!Html} module collects all used Tw
    classes and generates the appropriate CSS file.

    For dynamic styles that change at runtime, use [to_inline_style] to generate
    CSS properties directly for the style attribute. *)

val to_inline_style : t list -> string
(** [to_inline_style styles] generates inline CSS for the style attribute.

    {b Note:} This generates {i only} the CSS properties for the given styles,
    without any Tailwind reset/prelude. The reset is only included in [to_css]
    since it's meant for complete stylesheets, not individual elements.

    Perfect for tweaking individual HTML nodes with custom styles:
    {[
      (* Create inline styles *)
      let inline_styles =
        to_inline_style [ bg blue 100; p 4; rounded_md; text_white ]
      in

      (* Use in HTML *)
      Html.div
        ~at:[ Html.At.style inline_styles ]
        [ Html.txt "This div has inline styles" ]
      (* Generates: style="background-color:rgb(219 234
         254);padding:1rem;border-radius:0.375rem;color:rgb(255 255 255)" *)
    ]}

    {b When to use [to_inline_style] vs [to_css]:}

    {b Use [to_inline_style] when:}
    - You need dynamic styles that change at runtime
    - You want to override specific styles on individual elements
    - You're working with existing HTML that you can't modify classes for
    - You need precise control over a single element's styling

    {b Use [to_css] (preferred) when:}
    - You want to generate a stylesheet that can be cached and reused
    - You're building a full website with consistent styling
    - You want better performance (CSS classes are more efficient than inline
      styles)

    {b Performance considerations:} Inline styles are
    {i significantly less performant} for large-scale use compared to
    stylesheets because:
    - They increase HTML payload size (styles are repeated for each element)
    - They cannot be cached by the browser like external stylesheets
    - They have higher CSS specificity, making them harder to override
    - They don't benefit from CSS compression and minification

    Use [to_inline_style] sparingly for dynamic or element-specific styling
    only. *)

module Css = Css
(** CSS generation utilities

    The Css module provides lower-level CSS types and functions for working with
    stylesheets, rules, and properties. Most users will only need the high-level
    functions like [to_css] and [stylesheet_to_string], but the Css module is
    exposed for advanced use cases requiring direct manipulation of CSS
    structures. *)

module Prose = Prose
(** Simple prose utilities

    Provides convenient access to prose variants:
    - Size variants: [Prose.sm], [Prose.lg], [Prose.xl], [Prose.xl2]
    - Color themes: [Prose.gray], [Prose.slate]

    Usage: [div ~tw:[prose; Prose.lg] [...]] *)

val to_css : ?reset:bool -> t list -> Css.stylesheet
(** [to_css ?reset styles] generates a CSS stylesheet for the given styles.

    @param reset Whether to include CSS reset rules (default: [true])

    When [reset=true] (default), includes:
    - CSS reset rules (normalize margins/padding, set box-sizing, base
      typography)
    - The generated utility classes for your specific styles

    When [reset=false], includes only the utility classes.

    Use this to generate your main stylesheet for inclusion in HTML [<head>]. *)

val aspect_ratio : float -> float -> t
(** [aspect_ratio width height] maintains element proportions.

    Example:
    {[
      (* 16:9 video container *)
      div ~tw:[ aspect_ratio 16. 9.; bg black ] [ video ]
    ]} *)

val clip_path : string -> t
(** [clip_path value] clips element to custom shape using SVG path or shape.

    Example:
    {[
      (* Create a triangular badge/indicator *)
      span
        ~tw:
          [
            clip_path "polygon(50% 0%, 0% 100%, 100% 100%)";
            bg red;
            w (int 6);
            h (int 6);
          ]
        []
    ]} *)
