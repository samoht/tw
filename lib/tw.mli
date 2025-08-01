(** Type-safe CSS styling

    This module provides a typed interface for generating CSS styles without
    writing raw CSS. It prevents typos and invalid combinations at compile-time
    by using OCaml's type system.

    {1:concepts Core Concepts}

    - {b Utility-first}: Instead of writing CSS classes with multiple
      properties, you compose small, single-purpose utilities (e.g., [bg blue]
      for background color, [p (int 4)] for padding)

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

    - {b Spacing}: The [int] constructor creates values in 0.25rem increments.
      [int 4] = 1rem = 16px (by default)
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
            py (int 4);
            (* 4 * 0.25rem = 1rem *)
            px (int 8);
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
            on_sm [ px (int 6) ];
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

type size = [ `None | `Xs | `Sm | `Md | `Lg | `Xl | `Xl_2 | `Xl_3 | `Full ]
(** Standard size scale used consistently across the library:
    - [`None]: 0 (removes the property)
    - [`Xs]: extra small
    - [`Sm]: small
    - [`Md]: medium (usually the default)
    - [`Lg]: large
    - [`Xl] to [`Xl_3]: extra large sizes
    - [`Full]: 100% of parent *)

type spacing = [ `Px | `Full | `Val of float ]
(** Scale for spacing utilities (padding, margin, gap):
    - [`Px]: exactly 1 pixel
    - [`Full]: 100% of parent
    - [`Val f]: custom value where f is in rem units *)

type margin = [ spacing | `Auto ]
(** Same as spacing but includes [`Auto] for automatic margins (centering). *)

type scale = [ spacing | size | `Screen | `Min | `Max | `Fit ]
(** Extended scale for width/height:
    - Includes all spacing and size values
    - [`Screen]: 100vw or 100vh (viewport width/height)
    - [`Min]: min-content
    - [`Max]: max-content
    - [`Fit]: fit-content *)

type max_scale = [ scale | `Xl_4 | `Xl_5 | `Xl_6 | `Xl_7 ]
(** Scale for max-width including larger sizes for wide containers. *)

type shadow = [ size | `Inner ]
(** Shadow intensities from subtle to dramatic, plus [`Inner] for inset shadows.
*)

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

(** {1 Value Constructors} *)

(** {2 Spacing Constructors}

    These create values for spacing utilities like padding, margin, and gaps. *)

val int : int -> [> `Val of float ]
(** [int n] creates spacing values: n × 0.25rem.

    Common values:
    - [int 0]: 0
    - [int 1]: 0.25rem (4px)
    - [int 2]: 0.5rem (8px)
    - [int 4]: 1rem (16px) - base unit
    - [int 8]: 2rem (32px)
    - [int 16]: 4rem (64px)

    This is the primary way to create consistent spacing. *)

val one_px : [> `Px ]
(** [one_px] is exactly 1 pixel spacing. *)

val rem : float -> [> `Val of float ]
(** [rem f] creates a custom spacing value in rem units. *)

val full : [> `Full ]
(** [full] is 100% of parent container. *)

(** {2 Margin Constructors}

    Additional constructors for margin utilities (includes all spacing
    constructors). *)

val auto : [> `Auto ]
(** [auto] creates automatic margins that center elements horizontally. *)

(** {2 Size Constructors}

    Constructors for width/height utilities (includes all spacing constructors).
*)

val screen : [> `Screen ]
(** [screen] is full viewport size (100vw for width, 100vh for height). *)

val min : [> `Min ]
(** [min] is min-content sizing (shrinks to minimum needed). *)

val max : [> `Max ]
(** [max] is max-content sizing (expands to natural width). *)

val fit : [> `Fit ]
(** [fit] is fit-content sizing (uses available space but not more than
    max-content). *)

(** {2 Standard Size Scale}

    Named size values used consistently across the design system. *)

val none : [> `None ]
(** [none] removes the property or sets it to zero. *)

val xs : [> `Xs ]
(** [xs] is extra small size in the design scale. *)

val sm : [> `Sm ]
(** [sm] is small size in the design scale. *)

val md : [> `Md ]
(** [md] is medium size in the design scale. *)

val lg : [> `Lg ]
(** [lg] is large size in the design scale. *)

val xl : [> `Xl ]
(** [xl] is extra large size in the design scale. *)

val xl_2 : [> `Xl_2 ]
(** [xl_2] is 2x extra large size in the design scale. *)

val xl_3 : [> `Xl_3 ]
(** [xl_3] is 3x extra large size in the design scale. *)

val xl_4 : [> `Xl_4 ]
(** [xl_4] is 4x extra large size in the design scale. *)

val xl_5 : [> `Xl_5 ]
(** [xl_5] is 5x extra large size in the design scale. *)

val xl_6 : [> `Xl_6 ]
(** [xl_6] is 6x extra large size in the design scale. *)

val xl_7 : [> `Xl_7 ]
(** [xl_7] is 7x extra large size in the design scale. *)

(** {2 Effect Constructors}

    Specialized constructors for effects like shadows. *)

val inner : [> `Inner ]
(** [inner] creates inset shadows that appear inside the element. *)

(** {1 Color & Background} *)

val bg : ?shade:int -> color -> t
(** [bg ?shade color] sets the background color.

    Examples:
    - [bg blue]: Default blue background
    - [bg ~shade:100 gray]: Light gray background
    - [bg ~shade:900 slate]: Very dark slate background

    Shades range from 50 (lightest) to 900 (darkest). *)

val bg_transparent : t
(** Makes background fully transparent (invisible). *)

val bg_current : t
(** Sets background color to match the element's text color. If the element has
    [text ~shade:500 blue], the background will also be blue-500. Useful for
    icons and decorative elements that should match text. *)

val text : ?shade:int -> color -> t
(** [text ?shade color] sets text color.

    Examples:
    - [text black]: Pure black text
    - [text ~shade:600 gray]: Dark gray for body text
    - [text ~shade:500 gray]: Medium gray for secondary text
    - [text ~shade:700 blue]: Dark blue for links

    Higher shade numbers (700-900) ensure readability on light backgrounds. *)

val text_transparent : t
(** Transparent text color. *)

val text_current : t
(** Explicitly sets text color to "currentColor" (the inherited text color).
    This is rarely needed since text naturally inherits color from parents. *)

val border_color : ?shade:int -> color -> t
(** [border_color ?shade color] creates a border color with optional shade. *)

val border_transparent : t
(** Transparent border. *)

val border_current : t
(** Sets border color to match the text color. For example:
    {[
      div ~tw:[ text ~shade:600 red; border `Default; border_current ]
      (* Border will be red-600, same as the text *)
    ]}

    This is the default behavior in Tailwind v4, but can be explicitly set. *)

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

val from_color : ?shade:int -> color -> t
(** [from_color ?shade c] sets the starting color of a gradient. *)

val to_color : ?shade:int -> color -> t
(** [to_color ?shade c] sets the ending color of a gradient. *)

(** {1 Spacing}
    @see <https://tailwindcss.com/docs/padding> Padding
    @see <https://tailwindcss.com/docs/margin> Margin
    @see <https://tailwindcss.com/docs/space> Space Between
    @see <https://tailwindcss.com/docs/gap> Gap *)

val p : spacing -> t
(** [p spacing] sets padding (inner spacing) on all sides.

    Examples:
    - [p (int 4)]: 1rem padding on all sides
    - [p (int 0)]: Remove all padding
    - [p full]: Padding equal to parent width (rarely used). *)

val px : spacing -> t
(** [px spacing] sets horizontal padding (left and right). Common for buttons
    and cards to have more horizontal than vertical padding. *)

val py : spacing -> t
(** [py spacing] sets vertical padding (top and bottom). Often smaller than
    horizontal padding for better proportions. *)

val pt : spacing -> t
(** [pt spacing] sets top padding. *)

val pr : spacing -> t
(** [pr spacing] sets right padding. *)

val pb : spacing -> t
(** [pb spacing] sets bottom padding. *)

val pl : spacing -> t
(** [pl spacing] sets left padding. *)

val m : margin -> t
(** [m margin] sets margin (outer spacing) on all sides.

    Examples:
    - [m (int 4)]: 1rem margin on all sides
    - [m (int 0)]: Remove all margins
    - [m auto]: Center element if it has a defined width. *)

val mx : margin -> t
(** [mx margin] sets horizontal margin (left and right). [mx auto] centers block
    elements horizontally. *)

val my : margin -> t
(** [my margin] sets vertical margin (top and bottom). Useful for spacing
    between sections. *)

val mt : margin -> t
(** [mt margin] sets top margin. *)

val mr : margin -> t
(** [mr margin] sets right margin. *)

val mb : margin -> t
(** [mb margin] sets bottom margin. *)

val ml : margin -> t
(** [ml margin] sets left margin. *)

val neg_mt : spacing -> t
(** [neg_mt spacing] pulls element upward with negative margin. Useful for
    overlapping elements or compensating for padding. *)

val neg_mr : spacing -> t
(** [neg_mr spacing] pulls element rightward with negative margin. *)

val neg_mb : spacing -> t
(** [neg_mb spacing] pulls element (and following content) upward. *)

val neg_ml : spacing -> t
(** [neg_ml spacing] pulls element leftward with negative margin. *)

val gap : spacing -> t
(** [gap spacing] sets spacing between items in flex/grid containers. More
    modern and flexible than using margins on children.

    Example:
    {[
      div
        ~tw:[ flex; gap (int 4) ]
        [
          (* All children will have 1rem space between them *)
          button [ txt "Save" ];
          button [ txt "Cancel" ];
        ]
    ]} *)

val gap_x : spacing -> t
(** [gap_x spacing] sets only horizontal gaps in flex/grid containers. *)

val gap_y : spacing -> t
(** [gap_y spacing] sets only vertical gaps in flex/grid containers. *)

(** {1 Sizing}
    @see <https://tailwindcss.com/docs/width> Width and Height *)

val w : scale -> t
(** [w scale] sets element width.

    Common patterns:
    - [w full]: 100% of parent width
    - [w (int 24)]: Fixed width of 6rem (96px)
    - [w screen]: Full viewport width
    - [w max]: Shrink to content width
    - [w (rem 20.0)]: Custom width in rem. *)

val h : scale -> t
(** [h scale] sets element height.

    Common patterns:
    - [h full]: 100% of parent height
    - [h screen]: Full viewport height (great for hero sections)
    - [h (int 16)]: Fixed height of 4rem (64px)
    - [h auto]: Height based on content (default). *)

val min_w : scale -> t
(** [min_w scale] sets minimum width. *)

val min_h : scale -> t
(** [min_h scale] sets minimum height using the scale. *)

val max_w : max_scale -> t
(** [max_w scale] sets maximum width - element won't grow beyond this.

    Common for readable content:
    - [max_w md]: ~28rem - for cards and small containers
    - [max_w xl_2]: ~42rem - optimal for article text (65-75 characters)
    - [max_w xl_4]: ~56rem - for wider content sections
    - [max_w full]: Allow full width
    - [max_w screen]: Never exceed viewport width. *)

val max_h : scale -> t
(** [max_h scale] sets maximum height using the scale. *)

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
      div
        ~tw:[ flex; items_center; gap (int 4) ]
        [ icon; span [ txt "Dashboard" ] ]
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

val grid_cols : int -> t
(** [grid_cols n] creates a grid with n equal columns.

    Example:
    {[
      (* 3-column card layout *)
      div
        ~tw:[ grid; grid_cols 3; gap (int 4) ]
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

type width = [ size | `Default ]
(** Width options for borders and rings:
    - [`None]: 0px
    - [`Xs]: 1px
    - [`Sm]: 2px
    - [`Default] or [`Md]: 3px (default for rings), 1px (default for borders)
    - [`Lg]: 4px
    - [`Xl]: 8px *)

(** {1 Borders}
    @see <https://tailwindcss.com/docs/border-width> Borders *)

val border : width -> t
(** [border width] sets border width on all sides. Default is 1px when using
    [`Default].

    The border color defaults to the current text color. To set a specific
    color:
    - Use [border_color]: [border `Default; border_color ~shade:200 gray]
    - Use [border_current] to explicitly use text color:
      [text blue; border `Default; border_current]
    - Use [border_transparent] for invisible borders that preserve spacing. *)

val border_t : t
(** Top border (1px). *)

val border_r : t
(** Right border (1px). *)

val border_b : t
(** Bottom border (1px). *)

val border_l : t
(** Left border (1px). *)

val rounded : size -> t
(** [rounded size] sets corner roundness.

    Common values:
    - [rounded none]: Sharp corners (0px)
    - [rounded sm]: Subtle rounding (2px)
    - [rounded md]: Medium rounding (6px)
    - [rounded lg]: Noticeably rounded (8px)
    - [rounded full]: Fully rounded (9999px) - makes circles/pills. *)

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

val shadow : shadow -> t
(** [shadow s] adds drop shadow for depth and elevation.

    Common values:
    - [shadow sm]: Subtle shadow for cards
    - [shadow md]: Default shadow for raised elements
    - [shadow lg]: Strong shadow for modals, dropdowns
    - [shadow none]: Remove shadow
    - [shadow inner]: Inset shadow for pressed/sunken effect. *)

val opacity : int -> t
(** [opacity n] controls transparency (0-100).
    - 0: Fully transparent (invisible but takes space)
    - 50: Half transparent
    - 100: Fully opaque (default). *)

val outline_none : t
(** Remove outline. *)

val ring : width -> t
(** [ring width] adds an outline ring of the specified width. Rings use
    box-shadow and don't affect layout.

    By default, rings are blue with 50% opacity. To customize:
    - Use [ring_color] to change color: [ring `Sm; ring_color ~shade:500 purple]
    - Rings are often used for focus states: [on_focus [ ring `Md ]]
    - Unlike borders, rings don't take up space in the layout. *)

val ring_color : ?shade:int -> color -> t
(** [ring_color ?shade color] sets the color of outline rings. *)

val isolate : t
(** Creates a new stacking context to isolate z-index behavior. Useful to
    prevent z-index values from affecting elements outside this container. *)

val brightness : int -> t
(** [brightness n] sets brightness filter (0-200, where 100 is normal). *)

val contrast : int -> t
(** [contrast n] sets contrast filter (0-200, where 100 is normal). *)

val blur : size -> t
(** [blur size] sets the blur filter. *)

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

val backdrop_blur : size -> t
(** [backdrop_blur size] applies blur filter to content behind element. *)

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

val active : t -> t
(** [active style] applies style on active state. *)

val on_active : t list -> t
(** [on_active styles] applies multiple styles on active state. *)

val disabled : t -> t
(** [disabled style] applies style when disabled. *)

val on_disabled : t list -> t
(** [on_disabled styles] applies multiple styles when disabled. *)

val on_group_hover : t list -> t
(** [on_group_hover styles] applies styles to this element when its parent with
    the [group] class is hovered. The parent must have the [group] class for
    this to work.

    See {!group} for usage examples. *)

val on_group_focus : t list -> t
(** [on_group_focus styles] applies styles to this element when its parent with
    the [group] class is focused. The parent must have the [group] class for
    this to work. *)

val dark : t -> t
(** [dark style] applies style in dark mode. *)

val on_dark : t list -> t
(** [on_dark styles] applies multiple styles in dark mode. *)

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

val peer_checked : t -> t
(** [peer_checked style] applies style when a sibling peer checkbox/radio is
    checked. *)

val on_peer_checked : t list -> t
(** [on_peer_checked styles] applies multiple styles when a sibling peer
    checkbox/radio is checked. *)

val aria_checked : t -> t
(** [aria_checked style] applies style when aria-checked="true". Useful for
    custom checkbox/radio styling with proper accessibility. *)

val on_aria_checked : t list -> t
(** [on_aria_checked styles] applies multiple styles when aria-checked="true".
*)

val aria_expanded : t -> t
(** [aria_expanded style] applies style when aria-expanded="true". Common for
    accordions, dropdowns, and collapsible sections. *)

val on_aria_expanded : t list -> t
(** [on_aria_expanded styles] applies multiple styles when aria-expanded="true".
*)

val aria_selected : t -> t
(** [aria_selected style] applies style when aria-selected="true". Used in
    custom select menus, tabs, and list selections. *)

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
(** [prose_stylesheet ()] generates a complete prose stylesheet with all typography rules.
    This includes descendant selectors for all HTML elements (h1-h6, p, a, strong,
    em, code, pre, blockquote, ul, ol, li, etc.) and all size variants.

    Usage:
    {[
      let prose_css = prose_stylesheet () |> Css.to_string in
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

val to_class : t -> string
(** [to_class style] generates a class name from a style. *)

val to_classes : t list -> string
(** [to_classes styles] converts your style list to a CSS class string. This is
    the main function you'll use with HTML elements.

    Example:
    {[
      let button_styles = [ bg blue; text white; px (int 4); py (int 2) ] in
      button ~at:[ At.class_ (to_classes button_styles) ] [ txt "Click" ]
    ]} *)

val to_string : t -> string
(** [to_string style] converts a single style to a class string. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string class_str] parses a Tailwind class string into a style.

    Example:
    {[
      of_string "bg-blue-500" = Ok (bg ~shade:500 blue)
      of_string "p-4" = Ok (p (int 4))
      of_string "text-center" = Ok text_center
      of_string "unknown-class" = Error (`Msg "Unknown class: unknown-class")
    ]}

    Returns [Error (`Msg reason)] if the class string is not recognized. *)

val classes_to_string : t list -> string
(** [classes_to_string styles] is an alias for to_classes. *)

val color_to_string : color -> string
(** [color_to_string c] converts a color to its string representation. *)

val pp : t Pp.t
(** [pp t] pretty-prints Tailwind class [t]. *)

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
        to_inline_style
          [ bg ~shade:100 blue; p (int 4); rounded md; text white ]
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

val stylesheet_to_string : ?minify:bool -> Css.stylesheet -> string
(** [stylesheet_to_string ?minify stylesheet] converts a CSS stylesheet to
    string.

    @param minify Whether to minify the CSS output (default: [false])

    This is useful when you need to output the result of [to_css] as a string.
*)

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
