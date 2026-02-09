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
            hover [ bg ~shade:700 blue ];
            (* darker blue on hover *)
            transition_colors;
            (* smooth color transitions *)

            (* Responsive design *)
            sm [ px 6 ];
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
    @see <https://tailwindcss.com/docs/colors> Colors *)

type t
(** The abstract type representing a single CSS style utility. *)

type color
(** Abstract type for colors. Use color constructors like [red], [blue], etc.
    Colors can have shades from 50 (lightest) to 900 (darkest). *)

(** {1:tailwind_colors Tailwind colors}

    Base colors map to the Tailwind v4 palette (shades 50–950). Use
    [bg color shade], [text color shade], and [border_color color shade] to
    produce utilities.

    Palettes include: [black], [white], [gray], [slate], [zinc], [neutral],
    [stone], [red], [orange], [amber], [yellow], [lime], [green], [emerald],
    [teal], [cyan], [sky], [blue], [indigo], [violet], [purple], [fuchsia],
    [pink], [rose].

    For the full palette and OKLCH values, see
    @see <https://tailwindcss.com/docs/colors> Colors. *)

(** {1 Color Constructors} *)

val black : color
(** [black] is Tailwind black palette. See {!tailwind_colors}. *)

val white : color
(** [white] is Tailwind white palette. See {!tailwind_colors}. *)

val gray : color
(** [gray] is Tailwind gray palette. See {!tailwind_colors}. *)

val slate : color
(** [slate] is Tailwind slate palette. See {!tailwind_colors}. *)

val zinc : color
(** [zinc] is Tailwind zinc palette. See {!tailwind_colors}. *)

val neutral : color
(** [neutral] is Tailwind neutral palette. See {!tailwind_colors}. *)

val stone : color
(** [stone] is Tailwind stone palette. See {!tailwind_colors}. *)

val red : color
(** [red] is Tailwind red palette. See {!tailwind_colors}. *)

val orange : color
(** [orange] is Tailwind orange palette. See {!tailwind_colors}. *)

val amber : color
(** [amber] is Tailwind amber palette. See {!tailwind_colors}. *)

val yellow : color
(** [yellow] is Tailwind yellow palette. See {!tailwind_colors}. *)

val lime : color
(** [lime] is Tailwind lime palette. See {!tailwind_colors}. *)

val green : color
(** [green] is Tailwind green palette. See {!tailwind_colors}. *)

val emerald : color
(** [emerald] is Tailwind emerald palette. See {!tailwind_colors}. *)

val teal : color
(** [teal] is Tailwind teal palette. See {!tailwind_colors}. *)

val cyan : color
(** [cyan] is Tailwind cyan palette. See {!tailwind_colors}. *)

val sky : color
(** [sky] is Tailwind sky palette. See {!tailwind_colors}. *)

val blue : color
(** [blue] is Tailwind blue palette. See {!tailwind_colors}. *)

val indigo : color
(** [indigo] is Tailwind indigo palette. See {!tailwind_colors}. *)

val violet : color
(** [violet] is Tailwind violet palette. See {!tailwind_colors}. *)

val purple : color
(** [purple] is Tailwind purple palette. See {!tailwind_colors}. *)

val fuchsia : color
(** [fuchsia] is Tailwind fuchsia palette. See {!tailwind_colors}. *)

val pink : color
(** [pink] is Tailwind pink palette. See {!tailwind_colors}. *)

val rose : color
(** [rose] is Tailwind rose palette. See {!tailwind_colors}. *)

val hex : string -> color
(** [hex "#1da1f2"] creates a custom color from a hex string. The # prefix is
    optional.

    Examples:
    - [hex "#1da1f2"] - Twitter blue with # prefix.
    - [hex "ff5733"] - Orange without # prefix.
    - [hex "#f0f"] - 3-character hex codes are supported. *)

val rgb : int -> int -> int -> color
(** [rgb r g b] creates a custom color from RGB values (0-255).

    Examples:
    - [rgb 29 161 242] - Twitter blue
    - [rgb 255 87 51] - Orange

    Raises [Invalid_argument] if any channel value is outside 0-255 range. *)

(** {1 Base Styles} *)

(** {2 Preflight}
    @see <https://tailwindcss.com/docs/preflight> Preflight *)

(** Preflight provides Tailwind’s base CSS reset. It is included automatically
    when generating full CSS with this library. *)

val preflight : ?placeholder_supports:Css.t -> ?forms:bool -> unit -> Css.t
(** [preflight ?placeholder_supports ?forms ()] returns Tailwind's Preflight
    base reset rules.

    Use this to compose the base reset manually when needed. When using
    {!to_css} with [~base:true] (the default), the base reset is already
    included.

    If [forms] is [true], omits webkit datetime rules that the forms plugin
    provides (display:inline-flex and fields-wrapper padding) to avoid
    duplicates.

    Usage:
    - [Css.stylesheet (preflight () |> List.map (fun r -> Css.Rule r))]
    - Combine with {!to_css} output for a full stylesheet. *)

(** {1 Layout} *)

(** {2 Aspect Ratio}
    @see <https://tailwindcss.com/docs/aspect-ratio> Aspect Ratio *)

val aspect_auto : t
(** [aspect_auto] sets automatic aspect ratio based on content or CSS rules. *)

val aspect_square : t
(** [aspect_square] sets a square 1:1 aspect ratio. *)

val aspect_video : t
(** [aspect_video] sets a 16:9 aspect ratio. *)

val aspect_ratio : int -> int -> t
(** [aspect_ratio w h] maintains element proportions of [w:h]. *)

(* Layout subsections added to mirror Tailwind v4 order *)
(** {2 Columns}
    @see <https://tailwindcss.com/docs/columns> Columns *)

(** TODO: Implement multi-column layout utilities. *)

(** {2 Break After}
    @see <https://tailwindcss.com/docs/break-after> Break After *)

(** TODO: Implement break-after utilities. *)

(** {2 Break Before}
    @see <https://tailwindcss.com/docs/break-before> Break Before *)

(** TODO: Implement break-before utilities. *)

(** {2 Break Inside}
    @see <https://tailwindcss.com/docs/break-inside> Break Inside *)

(** TODO: Implement break-inside utilities. *)

(** {2 Box Decoration Break}
    @see <https://tailwindcss.com/docs/box-decoration-break>
      Box Decoration Break *)

val box_decoration_clone : t
(** [box_decoration_clone] replicates the element's box decoration across
    fragment boxes. *)

val box_decoration_slice : t
(** [box_decoration_slice] slices the box decoration across fragment boxes. *)

(** {2 Box Sizing}
    @see <https://tailwindcss.com/docs/box-sizing> Box Sizing *)

(** TODO: Implement box-sizing utilities. *)

(** {2 Display}
    @see <https://tailwindcss.com/docs/display> Display *)

val block : t
(** [block] makes the element a block; it takes full width and stacks
    vertically. Default for div, p, h1–h6. *)

val inline : t
(** [inline] makes the element inline; it flows with text and its width is based
    on content. Default for span, a, strong. *)

val inline_block : t
(** [inline_block] flows inline but can have width/height like a block. *)

val table : t
(** [table] makes the element behave like a <table> element. *)

val hidden : t
(** [hidden] completely hides the element; no space is reserved and screen
    readers skip it. Use [sr_only] to hide visually but keep accessible. *)

(** {2 Float}
    @see <https://tailwindcss.com/docs/float> Float *)

(** TODO: Implement float utilities. *)

(** {2 Clear}
    @see <https://tailwindcss.com/docs/clear> Clear *)

(** TODO: Implement clear utilities. *)

(** {2 Isolation}
    @see <https://tailwindcss.com/docs/isolation> Isolation *)

val isolate : t
(** [isolate] creates a new stacking context to isolate z-index behavior. Useful
    to prevent z-index values from affecting elements outside this container. *)

(** {2 Object Fit}
    @see <https://tailwindcss.com/docs/object-fit> Object Fit *)

val object_contain : t
(** [object_contain] scales image to fit container while preserving aspect
    ratio. The entire image will be visible but may have empty space. *)

val object_cover : t
(** [object_cover] scales image to cover the entire container while preserving
    aspect ratio. Parts of the image may be clipped to fill the container. *)

val object_fill : t
(** [object_fill] stretches image to fill container, ignoring aspect ratio. May
    cause distortion. *)

val object_none : t
(** [object_none] keeps the image's original size; it may overflow or underflow
    the container. *)

val object_scale_down : t
(** [object_scale_down] scales down only if the image is larger than the
    container; otherwise uses the original size. *)

(** {2 Object Position}
    @see <https://tailwindcss.com/docs/object-position> Object Position *)

val object_top : t
(** [object_top] sets object position to top. *)

val object_right : t
(** [object_right] sets object position to right. *)

val object_bottom : t
(** [object_bottom] sets object position to bottom. *)

val object_left : t
(** [object_left] sets object position to left. *)

val object_center : t
(** [object_center] sets object position to center. *)

(** {2 Overflow}
    @see <https://tailwindcss.com/docs/overflow> Overflow *)

val overflow_auto : t
(** [overflow_auto] uses automatic overflow handling. *)

val overflow_hidden : t
(** [overflow_hidden] clips content that exceeds container bounds (no
    scrolling). *)

val overflow_visible : t
(** [overflow_visible] allows content to extend beyond container bounds
    (default). *)

val overflow_scroll : t
(** [overflow_scroll] always shows scrollbars even if content fits. *)

val overflow_x_auto : t
(** [overflow_x_auto] uses automatic horizontal overflow. *)

val overflow_x_hidden : t
(** [overflow_x_hidden] hides horizontal overflow. *)

val overflow_x_visible : t
(** [overflow_x_visible] shows horizontal overflow. *)

val overflow_x_scroll : t
(** [overflow_x_scroll] always shows a horizontal scrollbar. *)

val overflow_y_auto : t
(** [overflow_y_auto] uses automatic vertical overflow. *)

val overflow_y_hidden : t
(** [overflow_y_hidden] hides vertical overflow. *)

val overflow_y_visible : t
(** [overflow_y_visible] shows vertical overflow. *)

val overflow_y_scroll : t
(** [overflow_y_scroll] always shows a vertical scrollbar. *)

(** {2 Overscroll Behavior}
    @see <https://tailwindcss.com/docs/overscroll-behavior> Overscroll Behavior
*)

val overscroll_auto : t
(** [overscroll_auto] uses default scroll chaining behavior. *)

val overscroll_contain : t
(** [overscroll_contain] prevents scroll chaining to parent elements. *)

val overscroll_none : t
(** [overscroll_none] prevents scroll chaining and overscroll effects. *)

val overscroll_x_auto : t
(** [overscroll_x_auto] uses default horizontal overscroll behavior. *)

val overscroll_x_contain : t
(** [overscroll_x_contain] prevents horizontal scroll chaining. *)

val overscroll_x_none : t
(** [overscroll_x_none] prevents horizontal scroll chaining and effects. *)

val overscroll_y_auto : t
(** [overscroll_y_auto] uses default vertical overscroll behavior. *)

val overscroll_y_contain : t
(** [overscroll_y_contain] prevents vertical scroll chaining. *)

val overscroll_y_none : t
(** [overscroll_y_none] prevents vertical scroll chaining and effects. *)

(** {2 Overflow Wrap}
    @see <https://tailwindcss.com/docs/overflow-wrap> Overflow Wrap *)

val wrap_normal : t
(** [wrap_normal] uses default word wrapping behavior. *)

val wrap_break_word : t
(** [wrap_break_word] breaks words to prevent overflow. *)

val wrap_anywhere : t
(** [wrap_anywhere] breaks at any character to prevent overflow. *)

(** {2 Box Sizing}
    @see <https://tailwindcss.com/docs/box-sizing> Box Sizing *)

val box_border : t
(** [box_border] includes borders and padding in the element's size. *)

val box_content : t
(** [box_content] excludes borders and padding from the element's size. *)

(** {2 Position}
    @see <https://tailwindcss.com/docs/position> Position *)

val static : t
(** [static] uses normal document flow positioning. Ignores
    top/right/bottom/left properties. *)

val relative : t
(** [relative] positions relative to the element's normal position. You can use
    top/right/bottom/left to nudge from the original spot. Creates positioning
    context for absolute children. *)

val absolute : t
(** [absolute] removes from normal flow and positions relative to the nearest
    positioned parent. Use with top/right/bottom/left for exact placement.

    Example:
    {[
      (* Notification badge on icon *)
      div ~tw:[ relative ]
        [ icon; span ~tw:[ absolute; top (-2); right (-2) ] [ txt "3" ] ]
    ]} *)

val fixed : t
(** [fixed] is like [absolute] but relative to the viewport; it stays in place
    when scrolling. *)

val sticky : t
(** [sticky] scrolls normally until it reaches a viewport edge, then sticks.
    Great for table headers and sidebars that follow scroll. *)

(** {2 Top / Right / Bottom / Left}
    @see <https://tailwindcss.com/docs/top-right-bottom-left>
      Top / Right / Bottom / Left *)

val inset_0 : t
(** [inset_0] sets all inset values to 0. *)

val inset_x_0 : t
(** [inset_x_0] sets left and right to 0. *)

val inset_y_0 : t
(** [inset_y_0] sets top and bottom to 0. *)

val top : int -> t
(** [top n] sets top position value. *)

val right : int -> t
(** [right n] sets right position value. *)

val bottom : int -> t
(** [bottom n] sets bottom position value. *)

val left : int -> t
(** [left n] sets left position value. *)

val inset : int -> t
(** [inset n] sets all position offsets (top, right, bottom, left) to the same
    value. *)

val top_1_2 : t
(** [top_1_2] positions the element at 50% from the top. *)

val left_1_2 : t
(** [left_1_2] positions the element at 50% from the left. *)

(** {2 Visibility}
    @see <https://tailwindcss.com/docs/visibility> Visibility *)

(** TODO: Implement visibility utilities. *)

(** {2 Z-Index}
    @see <https://tailwindcss.com/docs/z-index> Z-Index *)

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

(** {1 Flexbox & Grid} *)

(** {2 Flex Basis}
    @see <https://tailwindcss.com/docs/flex-basis> Flex Basis *)

val basis_0 : t
(** [basis_0] sets flex-basis to 0. *)

val basis_1 : t
(** [basis_1] sets flex-basis to 0.25rem. *)

val basis_auto : t
(** [basis_auto] sets flex-basis to auto. *)

val basis_full : t
(** [basis_full] sets flex-basis to 100%. *)

(** {2 Flex Direction}
    @see <https://tailwindcss.com/docs/flex-direction> Flex Direction *)

val flex_row : t
(** [flex_row] arranges flex items horizontally (left to right). This is the
    default for flex containers. *)

val flex_row_reverse : t
(** [flex_row_reverse] arranges flex items horizontally but reversed (right to
    left). *)

val flex_col : t
(** [flex_col] stacks flex items vertically (top to bottom). Changes the main
    axis to vertical. *)

val flex_col_reverse : t
(** [flex_col_reverse] stacks flex items vertically but reversed (bottom to
    top). *)

(** {2 Flex Wrap}
    @see <https://tailwindcss.com/docs/flex-wrap> Flex Wrap *)

val flex_wrap : t
(** [flex_wrap] allows flex items to wrap onto multiple lines. *)

val flex_wrap_reverse : t
(** [flex_wrap_reverse] wraps flex items in reverse order. *)

val flex_nowrap : t
(** [flex_nowrap] prevents flex items from wrapping. *)

(** {2 Flex}
    @see <https://tailwindcss.com/docs/flex> Flex *)

val flex : t
(** [flex] creates a flex container for flexible layouts. Children can be
    arranged horizontally/vertically with gaps.

    Example:
    {[
      div ~tw:[ flex; items_center; gap 4 ] [ icon; span [ txt "Dashboard" ] ]
    ]} *)

val inline_flex : t
(** [inline_flex] is like [flex] but the container itself is inline. *)

val flex_1 : t
(** [flex_1] item grows and shrinks as needed, ignoring initial size. Perfect
    for elements that should fill available space equally.

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
(** [flex_auto] item grows and shrinks but considers its content size. Good for
    text that should expand but not squish too much. *)

val flex_initial : t
(** [flex_initial] item can shrink but will not grow beyond its content. Default
    flex behavior. *)

val flex_none : t
(** [flex_none] keeps the item at its natural size; it will not grow or shrink.
    Use for fixed-size elements like icons or buttons. *)

(** {2 Flex Grow}
    @see <https://tailwindcss.com/docs/flex-grow> Flex Grow *)

val flex_grow : t
(** [flex_grow] allows the flex item to grow. *)

val flex_grow_0 : t
(** [flex_grow_0] prevents the flex item from growing. *)

(** {2 Flex Shrink}
    @see <https://tailwindcss.com/docs/flex-shrink> Flex Shrink *)

val flex_shrink : t
(** [flex_shrink] allows the flex item to shrink. *)

val flex_shrink_0 : t
(** [flex_shrink_0] prevents the flex item from shrinking. *)

(** {2 Order}
    @see <https://tailwindcss.com/docs/order> Order *)

val order : int -> t
(** [order n] sets order to n. *)

val order_first : t
(** [order_first] sets order to minimum value. *)

val order_last : t
(** [order_last] sets order to maximum value. *)

val order_none : t
(** [order_none] sets order to 0. *)

val grid : t
(** [grid] creates a grid container for 2D layouts with rows and columns. More
    structured than flexbox. *)

val inline_grid : t
(** [inline_grid] is like [grid] but the container itself is inline. *)

(** {2 Grid Template Columns}
    @see <https://tailwindcss.com/docs/grid-template-columns>
      Grid Template Columns *)

val grid_cols : int -> t
(** [grid_cols n] creates a grid with n equal columns. *)

val grid_cols_none : t
(** [grid_cols_none] removes grid template columns. *)

val grid_cols_subgrid : t
(** [grid_cols_subgrid] uses subgrid for columns. *)

(** {2 Grid Column}
    @see <https://tailwindcss.com/docs/grid-column> Grid Column *)

val col_auto : t
(** [col_auto] sets grid-column to auto. *)

val col_span : int -> t
(** [col_span n] makes element span n columns. *)

val col_span_full : t
(** [col_span_full] spans all columns. *)

val col_start : int -> t
(** [col_start n] starts at column line n. *)

val col_start_auto : t
(** [col_start_auto] auto-places column start. *)

val col_end : int -> t
(** [col_end n] ends at column line [n]. *)

val col_end_auto : t
(** [col_end_auto] auto-places column end. *)

(** {2 Grid Template Rows}
    @see <https://tailwindcss.com/docs/grid-template-rows> Grid Template Rows *)

val grid_rows : int -> t
(** [grid_rows n] creates a grid with n equal rows. *)

val grid_rows_none : t
(** [grid_rows_none] removes grid template rows. *)

val grid_rows_subgrid : t
(** [grid_rows_subgrid] uses subgrid for rows. *)

(** {2 Grid Row}
    @see <https://tailwindcss.com/docs/grid-row> Grid Row *)

val row_auto : t
(** [row_auto] sets grid-row to auto. *)

val row_span : int -> t
(** [row_span n] makes element span n rows. *)

val row_span_full : t
(** [row_span_full] spans all rows. *)

val row_start : int -> t
(** [row_start n] starts at row line n. *)

val row_start_auto : t
(** [row_start_auto] auto-places row start. *)

val row_end : int -> t
(** [row_end n] ends at row line n. *)

val row_end_auto : t
(** [row_end_auto] auto-places row end. *)

(** {2 Grid Auto Flow}
    @see <https://tailwindcss.com/docs/grid-auto-flow> Grid Auto Flow *)

val grid_flow_row : t
(** [grid_flow_row] sets auto-placement to fill rows. *)

val grid_flow_col : t
(** [grid_flow_col] sets auto-placement to fill columns. *)

val grid_flow_dense : t
(** [grid_flow_dense] uses dense packing algorithm. *)

val grid_flow_row_dense : t
(** [grid_flow_row_dense] fills rows with dense packing. *)

val grid_flow_col_dense : t
(** [grid_flow_col_dense] fills columns with dense packing. *)

(** {2 Grid Auto Columns}
    @see <https://tailwindcss.com/docs/grid-auto-columns> Grid Auto Columns *)

val auto_cols_auto : t
(** [auto_cols_auto] sets implicit columns to auto. *)

val auto_cols_min : t
(** [auto_cols_min] sets implicit columns to min-content. *)

val auto_cols_max : t
(** [auto_cols_max] sets implicit columns to max-content. *)

val auto_cols_fr : t
(** [auto_cols_fr] sets implicit columns to 1fr. *)

(** {2 Grid Auto Rows}
    @see <https://tailwindcss.com/docs/grid-auto-rows> Grid Auto Rows *)

val auto_rows_auto : t
(** [auto_rows_auto] sets implicit rows to auto. *)

val auto_rows_min : t
(** [auto_rows_min] sets implicit rows to min-content. *)

val auto_rows_max : t
(** [auto_rows_max] sets implicit rows to max-content. *)

val auto_rows_fr : t
(** [auto_rows_fr] sets implicit rows to 1fr. *)

(** {2 Gap}
    @see <https://tailwindcss.com/docs/gap> Gap *)

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

val gap_px : t
(** [gap_px] sets 1px gap between items. *)

val gap_full : t
(** [gap_full] sets 100% gap between items. *)

(** {2 Justify Content}
    @see <https://tailwindcss.com/docs/justify-content> Justify Content *)

val justify_start : t
(** [justify_start] aligns content to the start of the container. *)

val justify_end : t
(** [justify_end] aligns content to the end of the container. *)

val justify_center : t
(** [justify_center] centers content in the container. *)

val justify_between : t
(** [justify_between] distributes items evenly with first item at start and last
    at end. *)

val justify_around : t
(** [justify_around] distributes items evenly with equal space around each item.
*)

val justify_evenly : t
(** [justify_evenly] distributes items evenly with equal space between them. *)

(** {2 Justify Items}
    @see <https://tailwindcss.com/docs/justify-items> Justify Items *)

val justify_items_start : t
(** [justify_items_start] justifies items to start. *)

val justify_items_end : t
(** [justify_items_end] justifies items to end. *)

val justify_items_center : t
(** [justify_items_center] centers items. *)

val justify_items_stretch : t
(** [justify_items_stretch] stretches items. *)

(** {2 Justify Self}
    @see <https://tailwindcss.com/docs/justify-self> Justify Self *)

val justify_self_auto : t
(** [justify_self_auto] uses the default justification. *)

val justify_self_start : t
(** [justify_self_start] justifies item to the start. *)

val justify_self_end : t
(** [justify_self_end] justifies item to the end. *)

val justify_self_center : t
(** [justify_self_center] centers the item. *)

val justify_self_stretch : t
(** [justify_self_stretch] stretches the item. *)

(** {2 Align Content}
    @see <https://tailwindcss.com/docs/align-content> Align Content *)

val content_start : t
(** [content_start] aligns lines/rows to the start of the container's cross
    axis. For flex containers with multiple lines or grid containers with
    multiple rows. *)

val content_end : t
(** [content_end] aligns lines/rows to the end of the container's cross axis. *)

val content_center : t
(** [content_center] centers lines/rows along the container's cross axis. *)

val content_between : t
(** [content_between] distributes lines/rows evenly: first at start, last at
    end. *)

val content_around : t
(** [content_around] distributes lines/rows evenly with equal space around each.
*)

val content_evenly : t
(** [content_evenly] distributes lines/rows evenly with equal space between and
    around all. *)

val content_stretch : t
(** [content_stretch] stretches lines/rows to fill the container's cross axis.
*)

(** {2 Align Items}
    @see <https://tailwindcss.com/docs/align-items> Align Items *)

val items_start : t
(** [items_start] aligns items to the start of the cross axis. *)

val items_end : t
(** [items_end] aligns items to the end of the cross axis. *)

val items_center : t
(** [items_center] centers items along the cross axis. *)

val items_baseline : t
(** [items_baseline] aligns items along their text baseline. *)

val items_stretch : t
(** [items_stretch] stretches items to fill the cross axis (default). *)

(** {2 Align Self}
    @see <https://tailwindcss.com/docs/align-self> Align Self *)

val self_auto : t
(** [self_auto] uses parent's align-items value (default). *)

val self_start : t
(** [self_start] aligns the item to the start of the container's cross axis. *)

val self_end : t
(** [self_end] aligns the item to the end of the container's cross axis. *)

val self_center : t
(** [self_center] centers the item along the container's cross axis. *)

val self_baseline : t
(** [self_baseline] aligns the item along the text baseline. *)

val self_stretch : t
(** [self_stretch] stretches the item to fill the container's cross axis. *)

(** {2 Place Content}
    @see <https://tailwindcss.com/docs/place-content> Place Content *)

val place_content_start : t
(** [place_content_start] aligns content to start in both axes. Shorthand for
    align-content and justify-content. *)

val place_content_end : t
(** [place_content_end] aligns content to end in both axes. *)

val place_content_center : t
(** [place_content_center] centers content in both axes. Perfect for centering
    grid content. *)

val place_content_between : t
(** [place_content_between] distributes content with space between in both axes.
*)

val place_content_around : t
(** [place_content_around] distributes content with space around in both axes.
*)

val place_content_evenly : t
(** [place_content_evenly] distributes content evenly in both axes. *)

val place_content_stretch : t
(** [place_content_stretch] stretches content to fill both axes. *)

(** {2 Place Items}
    @see <https://tailwindcss.com/docs/place-items> Place Items *)

val place_items_start : t
(** [place_items_start] aligns items to start in both axes. Shorthand for
    align-items and justify-items. *)

val place_items_end : t
(** [place_items_end] aligns items to end in both axes. *)

val place_items_center : t
(** [place_items_center] centers items in both axes. Common for centering grid
    items. *)

val place_items_stretch : t
(** [place_items_stretch] stretches items to fill both axes (default). *)

(** {2 Place Self}
    @see <https://tailwindcss.com/docs/place-self> Place Self *)

val place_self_auto : t
(** [place_self_auto] uses parent's place-items value. Shorthand for align-self
    and justify-self. *)

val place_self_start : t
(** [place_self_start] aligns to start in both axes. *)

val place_self_end : t
(** [place_self_end] aligns to end in both axes. *)

val place_self_center : t
(** [place_self_center] centers in both axes. *)

val place_self_stretch : t
(** [place_self_stretch] stretches to fill both axes. *)

(** {1 Spacing} *)

(** {2 Padding}
    @see <https://tailwindcss.com/docs/padding> Padding *)

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

(** {2 Margin}
    @see <https://tailwindcss.com/docs/margin> Margin *)

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

val space_x : int -> t
(** [space_x n] sets horizontal space between child elements. *)

val space_y : int -> t
(** [space_y n] sets vertical space between child elements. *)

(** {1 Sizing} *)

(** {2 Width}
    @see <https://tailwindcss.com/docs/width> Width *)

val w : int -> t
(** [w n] sets element width using Tailwind scale (n × 0.25rem). *)

val w_full : t
(** [w_full] sets width to 100% of parent. *)

val w_fit : t
(** [w_fit] uses fit-content sizing. *)

val w_screen : t
(** [w_screen] sets width to full viewport width (100vw). *)

val w_min : t
(** [w_min] sets width to min-content. *)

val w_max : t
(** [w_max] sets width to max-content. *)

(** {2 Min Width}
    @see <https://tailwindcss.com/docs/min-width> Min Width *)

val min_w : int -> t
(** [min_w n] sets minimum width using Tailwind scale. *)

val min_w_full : t
(** [min_w_full] sets minimum width to 100% of parent. *)

(** {2 Max Width}
    @see <https://tailwindcss.com/docs/max-width> Max Width *)

val max_w : int -> t
(** [max_w n] sets maximum width using Tailwind scale. *)

val max_w_xs : t
(** [max_w_xs] sets maximum width to 20rem. *)

val max_w_sm : t
(** [max_w_sm] sets maximum width to 24rem. *)

val max_w_md : t
(** [max_w_md] sets maximum width to 28rem. *)

val max_w_lg : t
(** [max_w_lg] sets maximum width to 32rem. *)

val max_w_xl : t
(** [max_w_xl] sets maximum width to 36rem. *)

val max_w_2xl : t
(** [max_w_2xl] sets maximum width to 42rem. *)

val max_w_3xl : t
(** [max_w_3xl] sets maximum width to 48rem. *)

val max_w_4xl : t
(** [max_w_4xl] sets maximum width to 56rem. *)

val max_w_5xl : t
(** [max_w_5xl] sets maximum width to 64rem. *)

val max_w_6xl : t
(** [max_w_6xl] sets maximum width to 72rem. *)

val max_w_7xl : t
(** [max_w_7xl] sets maximum width to 80rem. *)

val max_w_none : t
(** [max_w_none] removes the maximum width constraint. *)

val max_w_full : t
(** [max_w_full] sets maximum width to 100% of parent. *)

val max_w_min : t
(** [max_w_min] sets maximum width to min-content. *)

val max_w_max : t
(** [max_w_max] sets maximum width to max-content. *)

val max_w_fit : t
(** [max_w_fit] sets maximum width to fit-content. *)

val max_w_prose : t
(** [max_w_prose] sets maximum width to 65ch (ideal for reading). *)

val max_w_screen_sm : t
(** [max_w_screen_sm] sets maximum width to 640px. *)

val max_w_screen_md : t
(** [max_w_screen_md] sets maximum width to 768px. *)

val max_w_screen_lg : t
(** [max_w_screen_lg] sets maximum width to 1024px. *)

val max_w_screen_xl : t
(** [max_w_screen_xl] sets maximum width to 1280px. *)

val max_w_screen_2xl : t
(** [max_w_screen_2xl] sets maximum width to 1536px. *)

(** {2 Height}
    @see <https://tailwindcss.com/docs/height> Height *)

val h : int -> t
(** [h n] sets element height using Tailwind scale (n × 0.25rem). *)

val h_full : t
(** [h_full] sets height to 100% of parent. *)

val h_fit : t
(** [h_fit] uses fit-content sizing for height. *)

val h_screen : t
(** [h_screen] sets height to full viewport height (100vh). *)

val h_min : t
(** [h_min] sets height to min-content. *)

val h_max : t
(** [h_max] sets height to max-content. *)

(** {2 Min Height}
    @see <https://tailwindcss.com/docs/min-height> Min Height *)

val min_h : int -> t
(** [min_h n] sets minimum height using Tailwind scale. *)

val min_h_screen : t
(** [min_h_screen] sets viewport minimum height. *)

val min_h_full : t
(** [min_h_full] sets minimum height to 100% of parent. *)

(** {2 Max Height}
    @see <https://tailwindcss.com/docs/max-height> Max Height *)

val max_h : int -> t
(** [max_h n] sets maximum height using Tailwind scale. *)

val max_h_full : t
(** [max_h_full] sets maximum height to 100% of parent. *)

(** {2 Size}
    @see <https://tailwindcss.com/docs/size> Size *)

val size : int -> t
(** [size n] sets both width and height using Tailwind scale (n × 0.25rem). *)

val size_auto : t
(** [size_auto] sets both width and height to auto. *)

val size_full : t
(** [size_full] sets both width and height to 100% of parent. *)

val size_min : t
(** [size_min] sets both width and height to min-content. *)

val size_max : t
(** [size_max] sets both width and height to max-content. *)

val size_fit : t
(** [size_fit] sets both width and height to fit-content. *)

(** {1 Typography} *)

(** {2 Font Family}
    @see <https://tailwindcss.com/docs/font-family> Font Family *)

val font_sans : t
(** [font_sans] selects a sans-serif font family. *)

val font_serif : t
(** [font_serif] selects a serif font family. *)

val font_mono : t
(** [font_mono] selects a monospace font family. *)

(** {2 Font Size}
    @see <https://tailwindcss.com/docs/font-size> Font Size *)

val text_xs : t
(** [text_xs] sets extra small text (12px) for captions, labels, and fine print.
*)

val text_sm : t
(** [text_sm] sets small text (14px) for secondary content and form labels. *)

val text_base : t
(** [text_base] sets base text (16px): default body text size with good
    readability. *)

val text_lg : t
(** [text_lg] sets large text (18px) for emphasized paragraphs and lead text. *)

val text_xl : t
(** [text_xl] sets extra large text (20px) for section introductions. *)

val text_2xl : t
(** [text_2xl] sets 2× large text size (1.5rem). *)

val text_3xl : t
(** [text_3xl] sets 3× large text size (1.875rem). *)

val text_4xl : t
(** [text_4xl] sets 4× large text size (2.25rem). *)

val text_5xl : t
(** [text_5xl] sets 5× large text size (3rem). *)

(** {2 Font Smoothing}
    @see <https://tailwindcss.com/docs/font-smoothing> Font Smoothing *)

val antialiased : t
(** [antialiased] enables font antialiasing for smoother text rendering. *)

(** {2 Font Style}
    @see <https://tailwindcss.com/docs/font-style> Font Style *)

val italic : t
(** [italic] applies italic text style. *)

val not_italic : t
(** [not_italic] removes italic text style. *)

(** {2 Font Weight}
    @see <https://tailwindcss.com/docs/font-weight> Font Weight *)

val font_thin : t
(** [font_thin] uses the thinnest font weight (100). Use sparingly; it may not
    be visible with all fonts. *)

val font_light : t
(** [font_light] uses light font weight (300) for subtle, delicate text. *)

val font_normal : t
(** [font_normal] uses normal font weight (400), the default for body text. *)

val font_medium : t
(** [font_medium] uses medium font weight (500), slightly bolder than normal;
    good for UI labels. *)

val font_semibold : t
(** [font_semibold] uses semi-bold font weight (600) for subheadings and
    emphasis. *)

val font_bold : t
(** [font_bold] uses bold font weight (700) for headings and strong emphasis. *)

val font_extrabold : t
(** [font_extrabold] uses extra bold font weight (800) for major headings. *)

val font_black : t
(** [font_black] uses the heaviest font weight (900) for maximum impact and hero
    text. *)

(** {2 Font Stretch}
    @see <https://tailwindcss.com/docs/font-stretch> Font Stretch *)

val font_stretch_normal : t
(** [font_stretch_normal] uses the normal font stretch. *)

val font_stretch_condensed : t
(** [font_stretch_condensed] uses a condensed font stretch. *)

val font_stretch_expanded : t
(** [font_stretch_expanded] uses an expanded font stretch. *)

val font_stretch_percent : int -> t
(** [font_stretch_percent n] sets font stretch to [n]% when available. *)

(** {2 Numeric Variants}
    @see <https://tailwindcss.com/docs/font-variant-numeric>
      Font Variant Numeric *)

val normal_nums : t
(** [normal_nums] resets to normal numeric glyphs. *)

val ordinal : t
(** [ordinal] enables ordinal markers (e.g. 1st, 2nd). *)

val slashed_zero : t
(** [slashed_zero] uses a zero with a slash. *)

val lining_nums : t
(** [lining_nums] uses lining numbers. *)

val oldstyle_nums : t
(** [oldstyle_nums] uses old‑style (text) figures. *)

val proportional_nums : t
(** [proportional_nums] uses proportional width numbers. *)

val tabular_nums : t
(** [tabular_nums] uses tabular width numbers. *)

val diagonal_fractions : t
(** [diagonal_fractions] uses diagonal fraction glyphs. *)

val stacked_fractions : t
(** [stacked_fractions] uses stacked fraction glyphs. *)

(* moved: Text Indent and Vertical Align subsections now appear later *)

(** {2 Letter Spacing}
    @see <https://tailwindcss.com/docs/letter-spacing> Letter Spacing *)

val tracking_tighter : t
(** [tracking_tighter] sets letter spacing to -0.05em. *)

val tracking_tight : t
(** [tracking_tight] sets letter spacing to -0.025em. *)

val tracking_normal : t
(** [tracking_normal] sets letter spacing to 0. *)

val tracking_wide : t
(** [tracking_wide] sets letter spacing to 0.025em. *)

val tracking_wider : t
(** [tracking_wider] sets letter spacing to 0.05em. *)

val tracking_widest : t
(** [tracking_widest] sets letter spacing to 0.1em. *)

(** {2 Line Clamp}
    @see <https://tailwindcss.com/docs/line-clamp> Line Clamp *)

val line_clamp : int -> t
(** [line_clamp n] truncates text to [n] lines with ellipsis. Use 0 to remove
    clamping. Useful for consistent card heights.

    Example:
    {[
      p
        ~tw:[ line_clamp 3 ]
        [ txt "This very long text will be truncated after three lines..." ]
    ]} *)

(** {2 Line Height}
    @see <https://tailwindcss.com/docs/line-height> Line Height *)

val leading_none : t
(** [leading_none] sets line height to 1; text lines touch. Only for large
    display text. *)

val leading_tight : t
(** [leading_tight] sets line height to 1.25; compact spacing for headings. *)

val leading_snug : t
(** [leading_snug] sets line height to 1.375; slightly tighter than normal. *)

val leading_normal : t
(** [leading_normal] sets line height to 1.5 (default), optimal readability for
    body text. *)

val leading_relaxed : t
(** [leading_relaxed] sets line height to 1.625; more open, easier scanning for
    long text. *)

val leading_loose : t
(** [leading_loose] sets line height to 2; very open, good for short text blocks
    that need breathing room. *)

val leading : int -> t
(** [leading n] sets arbitrary line-height from the spacing scale ([n] *
    0.25rem). *)

(** {2 List Style Image}
    @see <https://tailwindcss.com/docs/list-style-image> List Style Image *)

val list_image_none : t
(** [list_image_none] removes list-style-image. *)

val list_image_url : string -> t
(** [list_image_url url] sets list-style-image to [url]. *)

(** {2 List Style Position}
    @see <https://tailwindcss.com/docs/list-style-position> List Style Position
*)

val list_inside : t
(** [list_inside] places list markers inside content box. *)

val list_outside : t
(** [list_outside] places list markers outside content box. *)

(** {2 List Style Type}
    @see <https://tailwindcss.com/docs/list-style-type> List Style Type *)

val list_none : t
(** [list_none] removes bullets/numbers from lists. *)

val list_disc : t
(** [list_disc] uses disc bullets. *)

val list_decimal : t
(** [list_decimal] uses decimal numbering. *)

(** {2 Text Align}
    @see <https://tailwindcss.com/docs/text-align> Text Align *)

val text_left : t
(** [text_left] left-aligns text. *)

val text_center : t
(** [text_center] center-aligns text. *)

val text_right : t
(** [text_right] right-aligns text. *)

val text_justify : t
(** [text_justify] justifies text. *)

(** {2 Color}
    @see <https://tailwindcss.com/docs/color> Color *)

val text : color -> int -> t
(** [text color shade] sets text color using the Tailwind color scale. *)

val text_inherit : t
(** [text_inherit] inherits color from the parent element. *)

val text_current : t
(** [text_current] sets color to currentColor. *)

val text_transparent : t
(** [text_transparent] makes text fully transparent. *)

val text_black : t
(** [text_black] is default black text. *)

val text_white : t
(** [text_white] is default white text. *)

val text_slate : t
(** [text_slate] Default slate text. *)

val text_gray : t
(** [text_gray] Default gray text. *)

val text_zinc : t
(** [text_zinc] Default zinc text. *)

val text_neutral : t
(** [text_neutral] Default neutral text. *)

val text_stone : t
(** [text_stone] Default stone text. *)

val text_red : t
(** [text_red] Default red text. *)

val text_orange : t
(** [text_orange] Default orange text. *)

val text_amber : t
(** [text_amber] Default amber text. *)

val text_yellow : t
(** [text_yellow] Default yellow text. *)

val text_lime : t
(** [text_lime] Default lime text. *)

val text_green : t
(** [text_green] Default green text. *)

val text_emerald : t
(** [text_emerald] Default emerald text. *)

val text_teal : t
(** [text_teal] Default teal text. *)

val text_cyan : t
(** [text_cyan] Default cyan text. *)

val text_sky : t
(** [text_sky] Default sky text. *)

val text_blue : t
(** [text_blue] Default blue text. *)

val text_indigo : t
(** [text_indigo] Default indigo text. *)

val text_violet : t
(** [text_violet] Default violet text. *)

val text_purple : t
(** [text_purple] Default purple text. *)

val text_fuchsia : t
(** [text_fuchsia] Default fuchsia text. *)

val text_pink : t
(** [text_pink] Default pink text. *)

val text_rose : t
(** [text_rose] Default rose text. *)

(** {2 Text Decoration Line}
    @see <https://tailwindcss.com/docs/text-decoration-line>
      Text Decoration Line *)

val underline : t
(** [underline] applies an underlined text decoration. *)

val line_through : t
(** [line_through] applies a line-through text decoration. *)

val no_underline : t
(** [no_underline] removes text decoration. *)

(** {2 Text Decoration Color}
    @see <https://tailwindcss.com/docs/text-decoration-color>
      Text Decoration Color *)

val decoration_color : ?shade:int -> color -> t
(** [decoration_color ?shade color] sets text-decoration color. *)

(** {2 Text Decoration Style}
    @see <https://tailwindcss.com/docs/text-decoration-style>
      Text Decoration Style *)

val decoration_solid : t
(** [decoration_solid] sets text decoration style to solid (default). *)

val decoration_double : t
(** [decoration_double] sets text decoration style to a double line. *)

val decoration_dotted : t
(** [decoration_dotted] sets text decoration style to a dotted line. *)

val decoration_dashed : t
(** [decoration_dashed] sets text decoration style to a dashed line. *)

val decoration_wavy : t
(** [decoration_wavy] sets text decoration style to a wavy line. Good for spell
    check indicators. *)

(** {2 Text Decoration Thickness}
    @see <https://tailwindcss.com/docs/text-decoration-thickness>
      Text Decoration Thickness *)

val decoration_thickness : int -> t
(** [decoration_thickness n] sets text-decoration thickness to [n]px. *)

val decoration_from_font : t
(** [decoration_from_font] uses the font's default decoration thickness. *)

(** {2 Text Underline Offset}
    @see <https://tailwindcss.com/docs/text-underline-offset>
      Text Underline Offset *)

val underline_offset_auto : t
(** [underline_offset_auto] sets text underline offset to auto (browser
    default). *)

val underline_offset_0 : t
(** [underline_offset_0] sets text underline offset to 0 (underline touches
    text). *)

val underline_offset_1 : t
(** [underline_offset_1] sets text underline offset to 1px. *)

val underline_offset_2 : t
(** [underline_offset_2] sets text underline offset to 2px. *)

val underline_offset_4 : t
(** [underline_offset_4] sets text underline offset to 4px. *)

val underline_offset_8 : t
(** [underline_offset_8] sets text underline offset to 8px. *)

(** {2 Text Transform}
    @see <https://tailwindcss.com/docs/text-transform> Text Transform *)

val uppercase : t
(** [uppercase] transforms text to UPPERCASE. Useful for labels, badges, and
    emphasis. *)

val lowercase : t
(** [lowercase] transforms text to lowercase. Less common but useful for
    specific designs. *)

val capitalize : t
(** [capitalize] capitalizes the first letter of each word. Good for titles and
    headings. *)

val normal_case : t
(** [normal_case] removes text transformation (default). Use to override parent
    text-transform. *)

(** {2 Text Overflow}
    @see <https://tailwindcss.com/docs/text-overflow> Text Overflow *)

val text_ellipsis : t
(** [text_ellipsis] uses ellipsis when text overflows. *)

val text_clip : t
(** [text_clip] clips overflowing text. *)

(** {2 Text Wrap}
    @see <https://tailwindcss.com/docs/text-wrap> Text Wrap *)

val text_wrap : t
(** [text_wrap] enables automatic text wrapping. *)

val text_nowrap : t
(** [text_nowrap] disables text wrapping. *)

val text_balance : t
(** [text_balance] balances text across lines. *)

val text_pretty : t
(** [text_pretty] optimizes wrapping for readability. *)

(** {2 Text Indent}
    @see <https://tailwindcss.com/docs/text-indent> Text Indent *)

val indent : int -> t
(** [indent n] sets text-indent to [n] times the spacing scale (n * 0.25rem). *)

(** {2 Vertical Align}
    @see <https://tailwindcss.com/docs/vertical-align> Vertical Align *)

val align_baseline : t
(** [align_baseline] sets vertical-align to baseline. *)

val align_top : t
(** [align_top] sets vertical-align to top. *)

val align_middle : t
(** [align_middle] sets vertical-align to middle. *)

val align_bottom : t
(** [align_bottom] sets vertical-align to bottom. *)

val align_text_top : t
(** [align_text_top] sets vertical-align to text-top. *)

val align_text_bottom : t
(** [align_text_bottom] sets vertical-align to text-bottom. *)

val align_sub : t
(** [align_sub] sets vertical-align to sub. *)

val align_super : t
(** [align_super] sets vertical-align to super. *)

(** {2 White Space}
    @see <https://tailwindcss.com/docs/whitespace> White Space *)

val whitespace_normal : t
(** [whitespace_normal] uses normal whitespace handling. *)

val whitespace_nowrap : t
(** [whitespace_nowrap] prevents text wrapping. *)

val whitespace_pre : t
(** [whitespace_pre] preserves whitespace. *)

val whitespace_pre_line : t
(** [whitespace_pre_line] preserves line breaks. *)

val whitespace_pre_wrap : t
(** [whitespace_pre_wrap] preserves whitespace and wraps. *)

(** {2 Word Break}
    @see <https://tailwindcss.com/docs/word-break> Word Break *)

val break_normal : t
(** [break_normal] normal word/overflow wrapping. *)

val break_words : t
(** [break_words] break words as needed. *)

val break_all : t
(** [break_all] break within words to prevent overflow. *)

val break_keep : t
(** [break_keep] keep words intact (keep-all). *)

(** {2 Overflow Wrap}
    @see <https://tailwindcss.com/docs/overflow-wrap> Overflow Wrap *)

val overflow_wrap_normal : t
(** [overflow_wrap_normal] uses the browser default overflow wrapping behavior.
*)

val overflow_wrap_anywhere : t
(** [overflow_wrap_anywhere] allows breaks within words to prevent overflow. *)

val overflow_wrap_break_word : t
(** [overflow_wrap_break_word] breaks long words to prevent overflow. *)

(** {2 Hyphens}
    @see <https://tailwindcss.com/docs/hyphens> Hyphens *)

val hyphens_none : t
(** [hyphens_none] disables automatic hyphenation. *)

val hyphens_manual : t
(** [hyphens_manual] enables manual hyphenation via soft hyphen characters. *)

val hyphens_auto : t
(** [hyphens_auto] enables automatic hyphenation when supported. *)

(** {2 Content}
    @see <https://tailwindcss.com/docs/content> Content *)

val content_none : t
(** [content_none] clears pseudo‑element content. *)

val content : string -> t
(** [content value] sets pseudo‑element content to [value]. *)

(** {1 Backgrounds} *)

(** {2 Background Attachment}
    @see <https://tailwindcss.com/docs/background-attachment>
      Background Attachment *)

(** TODO: Implement background-attachment utilities. *)

(** {2 Background Clip}
    @see <https://tailwindcss.com/docs/background-clip> Background Clip *)

(** TODO: Implement background-clip utilities. *)

(** {2 Background Color}
    @see <https://tailwindcss.com/docs/background-color> Background Color *)

val bg : color -> int -> t
(** [bg color shade] sets the background color with a specific shade.

    Examples:
    - [bg blue 500]: Medium blue background
    - [bg gray 100]: Light gray background
    - [bg slate 900]: Very dark slate background

    Shades range from 50 (lightest) to 900 (darkest). *)

val bg_transparent : t
(** [bg_transparent] makes background fully transparent. *)

val bg_current : t
(** [bg_current] sets background color to match the element's text color. If the
    element has [text ~shade:500 blue], the background will also be blue-500.
    Useful for icons and decorative elements that should match text. *)

val bg_black : t
(** [bg_black] is default black background. See {!tailwind_colors}. *)

val bg_white : t
(** [bg_white] is default white background. See {!tailwind_colors}. *)

val bg_slate : t
(** [bg_slate] is default slate background. See {!tailwind_colors}. *)

val bg_gray : t
(** [bg_gray] is default gray background. See {!tailwind_colors}. *)

val bg_zinc : t
(** [bg_zinc] is default zinc background. See {!tailwind_colors}. *)

val bg_neutral : t
(** [bg_neutral] is default neutral background. See {!tailwind_colors}. *)

val bg_stone : t
(** [bg_stone] is default stone background. See {!tailwind_colors}. *)

val bg_red : t
(** [bg_red] is default red background. See {!tailwind_colors}. *)

val bg_orange : t
(** [bg_orange] is default orange background. See {!tailwind_colors}. *)

val bg_amber : t
(** [bg_amber] is default amber background. See {!tailwind_colors}. *)

val bg_yellow : t
(** [bg_yellow] is default yellow background. See {!tailwind_colors}. *)

val bg_lime : t
(** [bg_lime] is default lime background. See {!tailwind_colors}. *)

val bg_green : t
(** [bg_green] is default green background. See {!tailwind_colors}. *)

val bg_emerald : t
(** [bg_emerald] is default emerald background. See {!tailwind_colors}. *)

val bg_teal : t
(** [bg_teal] is default teal background. See {!tailwind_colors}. *)

val bg_cyan : t
(** [bg_cyan] is default cyan background. See {!tailwind_colors}. *)

val bg_sky : t
(** [bg_sky] is default sky background. See {!tailwind_colors}. *)

val bg_blue : t
(** [bg_blue] is default blue background. See {!tailwind_colors}. *)

val bg_indigo : t
(** [bg_indigo] is default indigo background. See {!tailwind_colors}. *)

val bg_violet : t
(** [bg_violet] is default violet background. See {!tailwind_colors}. *)

val bg_purple : t
(** [bg_purple] is default purple background. See {!tailwind_colors}. *)

val bg_fuchsia : t
(** [bg_fuchsia] is default fuchsia background. See {!tailwind_colors}. *)

val bg_pink : t
(** [bg_pink] is default pink background. See {!tailwind_colors}. *)

val bg_rose : t
(** [bg_rose] is default rose background. See {!tailwind_colors}. *)

(* Text color declarations moved under Typography → Color. *)

(** {2 Background Image}
    @see <https://tailwindcss.com/docs/background-image> Background Image *)

(** Gradient direction variants *)
type direction =
  | Bottom  (** Gradient towards bottom *)
  | Bottom_right  (** Gradient towards bottom-right corner *)
  | Right  (** Gradient towards right *)
  | Top_right  (** Gradient towards top-right corner *)
  | Top  (** Gradient towards top *)
  | Top_left  (** Gradient towards top-left corner *)
  | Left  (** Gradient towards left *)
  | Bottom_left  (** Gradient towards bottom-left corner *)

val bg_gradient_to : direction -> t
(** [bg_gradient_to dir] sets gradient direction using a typed direction.
    Combine with [from_color]/[via_color]/[to_color].

    Example:
    {[
      div
        ~tw:
          [
            bg_gradient_to Bottom;
            from_color ~shade:100 blue;
            to_color ~shade:600 blue;
          ]
        [ txt "Gradient background" ]
    ]} *)

val from_color : ?shade:int -> color -> t
(** [from_color ~shade:400 blue] sets the starting color of a gradient. Default
    shade is 500. Works with gradient direction utilities like bg_gradient_to_r.

    Example:
    {[
      div
        ~tw:
          [
            bg_gradient_to_r;
            from_color ~shade:400 blue;
            to_color ~shade:600 purple;
          ]
    ]} *)

val via_color : ?shade:int -> color -> t
(** [via_color purple] sets the middle color of a gradient. Default shade is
    500. Creates a three-color gradient when used with [bg_gradient_to] and
    [to_color]. *)

val to_color : ?shade:int -> color -> t
(** [to_color ~shade:600 pink] sets the ending color of a gradient. Default
    shade is 500. *)

(** {2 Background Origin}
    @see <https://tailwindcss.com/docs/background-origin> Background Origin *)

(** TODO: Implement background-origin utilities. *)

(** {2 Background Position}
    @see <https://tailwindcss.com/docs/background-position> Background Position
*)

(** TODO: Implement background-position utilities. *)

(** {2 Background Repeat}
    @see <https://tailwindcss.com/docs/background-repeat> Background Repeat *)

(** TODO: Implement background-repeat utilities. *)

(** {2 Background Size}
    @see <https://tailwindcss.com/docs/background-size> Background Size *)

(** TODO: Implement background-size utilities. *)

(** {1 Borders} *)

(** {2 Border Radius}
    @see <https://tailwindcss.com/docs/border-radius> Border Radius *)

(** {2 Border Width}
    @see <https://tailwindcss.com/docs/border-width> Border Width *)

val border : t
(** [border] sets the default border (1px). Same as {!border_xs}. *)

val border_none : t
(** [border_none] removes the border (0px). *)

val border_xs : t
(** [border_xs] sets extra small border (1px). Same as {!border}. *)

val border_sm : t
(** [border_sm] sets small border (2px). *)

val border_md : t
(** [border_md] sets medium border (4px). *)

val border_lg : t
(** [border_lg] sets large border (4px). *)

val border_xl : t
(** [border_xl] sets extra large border (8px). *)

val border_2xl : t
(** [border_2xl] sets 2× large border (8px). *)

val border_3xl : t
(** [border_3xl] sets 3× large border (8px). *)

val border_full : t
(** [border_full] sets full border (8px). *)

val border_t : t
(** [border_t] sets top border (1px). *)

val border_r : t
(** [border_r] sets right border (1px). *)

val border_b : t
(** [border_b] sets bottom border (1px). *)

val border_l : t
(** [border_l] sets left border (1px). *)

(** {2 Border Color}
    @see <https://tailwindcss.com/docs/border-color> Border Color *)

val border_color : color -> int -> t
(** [border_color color shade] creates a border color with a specific shade. *)

val border_transparent : t
(** [border_transparent] makes border fully transparent. *)

val border_current : t
(** [border_current] sets border color to match the text color. For example:
    {[
      div ~tw:[ text ~shade:600 red; border xs; border_current ]
      (* Border will be red-600, same as the text *)
    ]}

    This is the default behavior in Tailwind v4, but can be explicitly set. *)

val border_black : t
(** [border_black] is default black border. See {!tailwind_colors}. *)

val border_white : t
(** [border_white] is default white border. See {!tailwind_colors}. *)

val border_slate : t
(** [border_slate] is default slate border. See {!tailwind_colors}. *)

val border_gray : t
(** [border_gray] is default gray border. See {!tailwind_colors}. *)

val border_zinc : t
(** [border_zinc] is default zinc border. See {!tailwind_colors}. *)

val border_neutral : t
(** [border_neutral] is default neutral border. See {!tailwind_colors}. *)

val border_stone : t
(** [border_stone] is default stone border. See {!tailwind_colors}. *)

val border_red : t
(** [border_red] is default red border. See {!tailwind_colors}. *)

val border_orange : t
(** [border_orange] is default orange border. See {!tailwind_colors}. *)

val border_amber : t
(** [border_amber] is default amber border. See {!tailwind_colors}. *)

val border_yellow : t
(** [border_yellow] is default yellow border. See {!tailwind_colors}. *)

val border_lime : t
(** [border_lime] is default lime border. See {!tailwind_colors}. *)

val border_green : t
(** [border_green] is default green border. See {!tailwind_colors}. *)

val border_emerald : t
(** [border_emerald] is default emerald border. See {!tailwind_colors}. *)

val border_teal : t
(** [border_teal] is default teal border. See {!tailwind_colors}. *)

val border_cyan : t
(** [border_cyan] is default cyan border. See {!tailwind_colors}. *)

val border_sky : t
(** [border_sky] is default sky border. See {!tailwind_colors}. *)

val border_blue : t
(** [border_blue] is default blue border. See {!tailwind_colors}. *)

val border_indigo : t
(** [border_indigo] is default indigo border. See {!tailwind_colors}. *)

val border_violet : t
(** [border_violet] is default violet border. See {!tailwind_colors}. *)

val border_purple : t
(** [border_purple] is default purple border. See {!tailwind_colors}. *)

val border_fuchsia : t
(** [border_fuchsia] is default fuchsia border. See {!tailwind_colors}. *)

val border_pink : t
(** [border_pink] is default pink border. See {!tailwind_colors}. *)

val border_rose : t
(** [border_rose] is default rose border. See {!tailwind_colors}. *)

(** {2 Border Style}
    @see <https://tailwindcss.com/docs/border-style> Border Style *)

val border_solid : t
(** [border_solid] uses a solid border style. *)

val border_dashed : t
(** [border_dashed] uses a dashed border style. *)

val border_dotted : t
(** [border_dotted] uses a dotted border style. *)

val border_double : t
(** [border_double] uses a double border style. *)

(** {2 Outline Width}
    @see <https://tailwindcss.com/docs/outline-width> Outline Width *)

(** TODO: Implement outline-width utilities. *)

(** {2 Outline Color}
    @see <https://tailwindcss.com/docs/outline-color> Outline Color *)

(** TODO: Implement outline-color utilities. *)

(** {2 Outline Style}
    @see <https://tailwindcss.com/docs/outline-style> Outline Style *)

val outline_none : t
(** [outline_none] removes the outline. *)

(** {2 Outline Offset}
    @see <https://tailwindcss.com/docs/outline-offset> Outline Offset *)

val outline_offset_0 : t
(** [outline_offset_0] sets outline offset to 0px. *)

val outline_offset_1 : t
(** [outline_offset_1] sets outline offset to 1px. *)

val outline_offset_2 : t
(** [outline_offset_2] sets outline offset to 2px. *)

val outline_offset_4 : t
(** [outline_offset_4] sets outline offset to 4px. *)

val outline_offset_8 : t
(** [outline_offset_8] sets outline offset to 8px. *)

val rounded_none : t
(** [rounded_none] sets sharp corners (0px). *)

val rounded_sm : t
(** [rounded_sm] sets subtle rounding (2px). *)

val rounded : t
(** [rounded] sets default rounding (4px). Same as {!rounded_md}. *)

val rounded_md : t
(** [rounded_md] sets medium rounding (6px). *)

val rounded_lg : t
(** [rounded_lg] sets noticeably rounded corners (8px). *)

val rounded_xl : t
(** [rounded_xl] sets extra rounded corners (12px). *)

val rounded_2xl : t
(** [rounded_2xl] sets 2× rounded corners (16px). *)

val rounded_3xl : t
(** [rounded_3xl] sets 3× rounded corners (24px). *)

val rounded_full : t
(** [rounded_full] sets fully rounded corners (9999px). Makes circles/pills. *)

(** {2 Side-specific border radius}
    Round specific sides (top, right, bottom, left) with consistent radius
    values. *)

val rounded_t : t
(** [rounded_t] rounds the top corners with the default radius. *)

val rounded_t_none : t
(** [rounded_t_none] sets no rounding on top corners. *)

val rounded_t_sm : t
(** [rounded_t_sm] sets subtle rounding on top corners. *)

val rounded_t_md : t
(** [rounded_t_md] sets medium rounding on top corners. *)

val rounded_t_lg : t
(** [rounded_t_lg] sets large rounding on top corners. *)

val rounded_t_xl : t
(** [rounded_t_xl] sets extra large rounding on top corners. *)

val rounded_t_2xl : t
(** [rounded_t_2xl] sets 2× rounding on top corners. *)

val rounded_t_3xl : t
(** [rounded_t_3xl] sets 3× rounding on top corners. *)

val rounded_t_full : t
(** [rounded_t_full] sets full rounding on top corners. *)

val rounded_r : t
(** [rounded_r] rounds the right corners with the default radius. *)

val rounded_r_none : t
(** [rounded_r_none] sets no rounding on right corners. *)

val rounded_r_sm : t
(** [rounded_r_sm] sets subtle rounding on right corners. *)

val rounded_r_md : t
(** [rounded_r_md] sets medium rounding on right corners. *)

val rounded_r_lg : t
(** [rounded_r_lg] sets large rounding on right corners. *)

val rounded_r_xl : t
(** [rounded_r_xl] sets extra large rounding on right corners. *)

val rounded_r_2xl : t
(** [rounded_r_2xl] sets 2× rounding on right corners. *)

val rounded_r_3xl : t
(** [rounded_r_3xl] sets 3× rounding on right corners. *)

val rounded_r_full : t
(** [rounded_r_full] sets full rounding on right corners. *)

val rounded_b : t
(** [rounded_b] rounds the bottom corners with the default radius. *)

val rounded_b_none : t
(** [rounded_b_none] sets no rounding on bottom corners. *)

val rounded_b_sm : t
(** [rounded_b_sm] sets subtle rounding on bottom corners. *)

val rounded_b_md : t
(** [rounded_b_md] sets medium rounding on bottom corners. *)

val rounded_b_lg : t
(** [rounded_b_lg] sets large rounding on bottom corners. *)

val rounded_b_xl : t
(** [rounded_b_xl] sets extra large rounding on bottom corners. *)

val rounded_b_2xl : t
(** [rounded_b_2xl] sets 2× rounding on bottom corners. *)

val rounded_b_3xl : t
(** [rounded_b_3xl] sets 3× rounding on bottom corners. *)

val rounded_b_full : t
(** [rounded_b_full] sets full rounding on bottom corners. *)

val rounded_l : t
(** [rounded_l] rounds the left corners with the default radius. *)

val rounded_l_none : t
(** [rounded_l_none] sets no rounding on left corners. *)

val rounded_l_sm : t
(** [rounded_l_sm] sets subtle rounding on left corners. *)

val rounded_l_md : t
(** [rounded_l_md] sets medium rounding on left corners. *)

val rounded_l_lg : t
(** [rounded_l_lg] sets large rounding on left corners. *)

val rounded_l_xl : t
(** [rounded_l_xl] sets extra large rounding on left corners. *)

val rounded_l_2xl : t
(** [rounded_l_2xl] sets 2× rounding on left corners. *)

val rounded_l_3xl : t
(** [rounded_l_3xl] sets 3× rounding on left corners. *)

val rounded_l_full : t
(** [rounded_l_full] sets full rounding on left corners. *)

(** {2 Corner-specific border radius}
    Round individual corners (top-left, top-right, bottom-right, bottom-left).
*)

val rounded_tl : t
(** [rounded_tl] rounds the top-left corner with the default radius. *)

val rounded_tl_none : t
(** [rounded_tl_none] sets no rounding on top-left corner. *)

val rounded_tl_sm : t
(** [rounded_tl_sm] sets subtle rounding on top-left corner. *)

val rounded_tl_md : t
(** [rounded_tl_md] sets medium rounding on top-left corner. *)

val rounded_tl_lg : t
(** [rounded_tl_lg] sets large rounding on top-left corner. *)

val rounded_tl_xl : t
(** [rounded_tl_xl] sets extra large rounding on top-left corner. *)

val rounded_tl_2xl : t
(** [rounded_tl_2xl] sets 2× rounding on top-left corner. *)

val rounded_tl_3xl : t
(** [rounded_tl_3xl] sets 3× rounding on top-left corner. *)

val rounded_tl_full : t
(** [rounded_tl_full] sets full rounding on top-left corner. *)

val rounded_tr : t
(** [rounded_tr] rounds the top-right corner with the default radius. *)

val rounded_tr_none : t
(** [rounded_tr_none] sets no rounding on top-right corner. *)

val rounded_tr_sm : t
(** [rounded_tr_sm] sets subtle rounding on top-right corner. *)

val rounded_tr_md : t
(** [rounded_tr_md] sets medium rounding on top-right corner. *)

val rounded_tr_lg : t
(** [rounded_tr_lg] sets large rounding on top-right corner. *)

val rounded_tr_xl : t
(** [rounded_tr_xl] sets extra large rounding on top-right corner. *)

val rounded_tr_2xl : t
(** [rounded_tr_2xl] sets 2× rounding on top-right corner. *)

val rounded_tr_3xl : t
(** [rounded_tr_3xl] sets 3× rounding on top-right corner. *)

val rounded_tr_full : t
(** [rounded_tr_full] sets full rounding on top-right corner. *)

val rounded_br : t
(** [rounded_br] rounds the bottom-right corner with the default radius. *)

val rounded_br_none : t
(** [rounded_br_none] sets no rounding on bottom-right corner. *)

val rounded_br_sm : t
(** [rounded_br_sm] sets subtle rounding on bottom-right corner. *)

val rounded_br_md : t
(** [rounded_br_md] sets medium rounding on bottom-right corner. *)

val rounded_br_lg : t
(** [rounded_br_lg] sets large rounding on bottom-right corner. *)

val rounded_br_xl : t
(** [rounded_br_xl] sets extra large rounding on bottom-right corner. *)

val rounded_br_2xl : t
(** [rounded_br_2xl] sets 2× rounding on bottom-right corner. *)

val rounded_br_3xl : t
(** [rounded_br_3xl] sets 3× rounding on bottom-right corner. *)

val rounded_br_full : t
(** [rounded_br_full] sets full rounding on bottom-right corner. *)

val rounded_bl : t
(** [rounded_bl] rounds the bottom-left corner with the default radius. *)

val rounded_bl_none : t
(** [rounded_bl_none] sets no rounding on bottom-left corner. *)

val rounded_bl_sm : t
(** [rounded_bl_sm] sets subtle rounding on bottom-left corner. *)

val rounded_bl_md : t
(** [rounded_bl_md] sets medium rounding on bottom-left corner. *)

val rounded_bl_lg : t
(** [rounded_bl_lg] sets large rounding on bottom-left corner. *)

val rounded_bl_xl : t
(** [rounded_bl_xl] sets extra large rounding on bottom-left corner. *)

val rounded_bl_2xl : t
(** [rounded_bl_2xl] sets 2× rounding on bottom-left corner. *)

val rounded_bl_3xl : t
(** [rounded_bl_3xl] sets 3× rounding on bottom-left corner. *)

val rounded_bl_full : t
(** [rounded_bl_full] sets full rounding on bottom-left corner. *)

(* Table-related utilities moved under the Tables section. *)

(** {1 Effects} *)

(** {2 Box Shadow}
    @see <https://tailwindcss.com/docs/box-shadow> Box Shadow *)

val shadow_none : t
(** [shadow_none] removes the shadow. *)

val shadow_sm : t
(** [shadow_sm] applies a subtle shadow for cards. *)

val shadow : t
(** [shadow] applies the default shadow. Same as {!shadow_md}. *)

val shadow_md : t
(** [shadow_md] applies a medium shadow. Same as {!shadow}. *)

val shadow_lg : t
(** [shadow_lg] applies a large shadow for modals and dropdowns. *)

val shadow_xl : t
(** [shadow_xl] applies an extra large shadow. *)

val shadow_2xl : t
(** [shadow_2xl] applies a 2× large shadow. *)

val shadow_inner : t
(** [shadow_inner] applies an inset shadow for a pressed/sunken effect. *)

(** {2 Opacity}
    @see <https://tailwindcss.com/docs/opacity> Opacity *)

val opacity : int -> t
(** [opacity n] controls transparency (0-100).
    - 0: Fully transparent (invisible but takes space)
    - 50: Half transparent
    - 100: Fully opaque (default). *)

val ring_none : t
(** [ring_none] removes the ring. *)

val ring_xs : t
(** [ring_xs] sets an extra small ring (1px). *)

val ring_sm : t
(** [ring_sm] sets a small ring (2px). *)

val ring : t
(** [ring] applies the default ring (3px). Same as {!ring_md}. *)

val ring_md : t
(** [ring_md] sets a medium ring (3px). Same as {!ring}. *)

val ring_lg : t
(** [ring_lg] sets a large ring (4px). *)

val ring_xl : t
(** [ring_xl] sets an extra large ring (8px). *)

(** Rings use box-shadow and don't affect layout. By default, rings are blue
    with 50% opacity. To customize:
    - Use [ring_color] to change color: [ring_sm; ring_color purple 500]
    - Rings are often used for focus states: [on_focus [ ring ]]
    - Unlike borders, rings don't take up space in the layout. *)

(** {2 Ring Width}
    @see <https://tailwindcss.com/docs/ring-width> Ring Width *)

(** {2 Ring Color}
    @see <https://tailwindcss.com/docs/ring-color> Ring Color *)

val ring_color : color -> int -> t
(** [ring_color color shade] sets the color of outline rings. *)

(** {2 Ring Offset Width}
    @see <https://tailwindcss.com/docs/ring-offset-width> Ring Offset Width *)

(** TODO: Implement ring-offset-width utilities. *)

(** {2 Ring Offset Color}
    @see <https://tailwindcss.com/docs/ring-offset-color> Ring Offset Color *)

(** TODO: Implement ring-offset-color utilities. *)

(** {2 Text Shadow}
    @see <https://tailwindcss.com/docs/text-shadow> Text Shadow *)

(** TODO: Implement text-shadow utilities. *)

(** {2 Mix Blend Mode}
    @see <https://tailwindcss.com/docs/mix-blend-mode> Mix Blend Mode *)

(** TODO: Implement mix-blend-mode utilities. *)

(** {2 Background Blend Mode}
    @see <https://tailwindcss.com/docs/background-blend-mode>
      Background Blend Mode *)

(** TODO: Implement background-blend-mode utilities. *)

(** {2 Mask Clip}
    @see <https://tailwindcss.com/docs/mask-clip> Mask Clip *)

(** TODO: Implement mask-clip utilities. *)

(** {2 Mask Composite}
    @see <https://tailwindcss.com/docs/mask-composite> Mask Composite *)

(** TODO: Implement mask-composite utilities. *)

(** {2 Mask Image}
    @see <https://tailwindcss.com/docs/mask-image> Mask Image *)

(** TODO: Implement mask-image utilities. *)

(** {2 Mask Mode}
    @see <https://tailwindcss.com/docs/mask-mode> Mask Mode *)

(** TODO: Implement mask-mode utilities. *)

(** {2 Mask Origin}
    @see <https://tailwindcss.com/docs/mask-origin> Mask Origin *)

(** TODO: Implement mask-origin utilities. *)

(** {2 Mask Position}
    @see <https://tailwindcss.com/docs/mask-position> Mask Position *)

(** TODO: Implement mask-position utilities. *)

(** {2 Mask Repeat}
    @see <https://tailwindcss.com/docs/mask-repeat> Mask Repeat *)

(** TODO: Implement mask-repeat utilities. *)

(** {2 Mask Size}
    @see <https://tailwindcss.com/docs/mask-size> Mask Size *)

(** TODO: Implement mask-size utilities. *)

(** {2 Mask Type}
    @see <https://tailwindcss.com/docs/mask-type> Mask Type *)

(** TODO: Implement mask-type utilities. *)

(** {1 Filters} *)

(** {2 Filter}
    @see <https://tailwindcss.com/docs/filter> Filter *)

val brightness : int -> t
(** [brightness n] sets brightness filter (0-200, where 100 is normal). *)

val contrast : int -> t
(** [contrast n] sets contrast filter (0-200, where 100 is normal). *)

val blur_none : t
(** [blur_none] applies no blur. *)

val blur_xs : t
(** [blur_xs] applies an extra small blur (2px). *)

val blur_sm : t
(** [blur_sm] applies a small blur (4px). *)

val blur : t
(** [blur] applies the default blur (8px). Same as {!blur_md}. *)

val blur_md : t
(** [blur_md] applies a medium blur (12px). *)

val blur_lg : t
(** [blur_lg] applies a large blur (16px). *)

val blur_xl : t
(** [blur_xl] applies an extra large blur (24px). *)

val blur_2xl : t
(** [blur_2xl] applies a 2× large blur (40px). *)

val blur_3xl : t
(** [blur_3xl] applies a 3× large blur (64px). *)

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

(** {2 Backdrop Filter}
    @see <https://tailwindcss.com/docs/backdrop-filter> Backdrop Filter *)

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
(** [backdrop_blur_none] applies no backdrop blur. *)

val backdrop_blur_xs : t
(** [backdrop_blur_xs] applies an extra small backdrop blur (2px). *)

val backdrop_blur_sm : t
(** [backdrop_blur_sm] applies a small backdrop blur (4px). *)

val backdrop_blur : t
(** [backdrop_blur] applies the default backdrop blur (8px). Same as
    {!backdrop_blur_md}. *)

val backdrop_blur_md : t
(** [backdrop_blur_md] applies a medium backdrop blur (12px). *)

val backdrop_blur_lg : t
(** [backdrop_blur_lg] applies a large backdrop blur (16px). *)

val backdrop_blur_xl : t
(** [backdrop_blur_xl] applies an extra large backdrop blur (24px). *)

val backdrop_blur_2xl : t
(** [backdrop_blur_2xl] applies a 2× large backdrop blur (40px). *)

val backdrop_blur_3xl : t
(** [backdrop_blur_3xl] applies a 3× large backdrop blur (64px). *)

(** {1 Tables} *)

(** {2 Border Collapse}
    @see <https://tailwindcss.com/docs/border-collapse> Border Collapse *)

val border_collapse : t
(** [border_collapse] collapses table borders. *)

val border_separate : t
(** [border_separate] separates table borders. *)

(** {2 Border Spacing}
    @see <https://tailwindcss.com/docs/border-spacing> Border Spacing *)

val border_spacing : int -> t
(** [border_spacing n] sets border spacing using spacing scale. *)

(** {2 Table Layout}
    @see <https://tailwindcss.com/docs/table-layout> Table Layout *)

val table_auto : t
(** [table_auto] uses automatic table layout. *)

val table_fixed : t
(** [table_fixed] uses fixed table layout. *)

(** {2 Caption Side}
    @see <https://tailwindcss.com/docs/caption-side> Caption Side *)

(** TODO: Implement caption-side utilities. *)

(** {1 Transitions & Animations} *)

(** {2 Transition Property}
    @see <https://tailwindcss.com/docs/transition-property> Transition Property
*)

val transition_none : t
(** [transition_none] enables no transition. *)

val transition : t
(** [transition] enables transitions for common properties (colors, opacity,
    shadow, transform). *)

val transition_all : t
(** [transition_all] transitions all properties. *)

val transition_colors : t
(** [transition_colors] smoothly animates color changes (background, text,
    border). Essential for hover effects to feel polished.

    Example:
    {[
      button
        ~tw:
          [
            bg blue;
            transition_colors;
            (* Smooth color change *)
            hover [ bg ~shade:700 blue ];
          ]
    ]}

    Duration is 150ms by default. *)

val transition_opacity : t
(** [transition_opacity] transitions opacity. *)

val transition_shadow : t
(** [transition_shadow] transitions box shadow. *)

val transition_transform : t
(** [transition_transform] transitions transform. *)

(** {2 Transition Behavior}
    @see <https://tailwindcss.com/docs/transition-behavior> Transition Behavior
*)

val transition_behavior_normal : t
(** [transition_behavior_normal] uses normal transition behavior (default). *)

val transition_behavior_allow_discrete : t
(** [transition_behavior_allow_discrete] allows discrete transitions where
    supported. Useful for toggling properties like visibility in a discrete
    manner. *)

(** {2 Transition Duration}
    @see <https://tailwindcss.com/docs/transition-duration> Transition Duration
*)

val duration : int -> t
(** [duration n] sets transition duration in milliseconds. *)

(** {2 Transition Timing Function}
    @see <https://tailwindcss.com/docs/transition-timing-function>
      Transition Timing Function *)

val ease_linear : t
(** [ease_linear] uses a linear transition timing function. *)

val ease_in : t
(** [ease_in] uses an ease-in transition timing function. *)

val ease_out : t
(** [ease_out] uses an ease-out transition timing function. *)

val ease_in_out : t
(** [ease_in_out] uses an ease-in-out transition timing function. *)

(** {2 Transition Delay}
    @see <https://tailwindcss.com/docs/transition-delay> Transition Delay *)

val delay : int -> t
(** [delay ms] sets transition/animation delay in milliseconds. *)

(** {1 Transforms} *)

(** {2 Backface Visibility}
    @see <https://tailwindcss.com/docs/backface-visibility> Backface Visibility
*)

val backface_visible : t
(** [backface_visible] shows the element's back face when rotated (default). *)

val backface_hidden : t
(** [backface_hidden] hides the element's back face when rotated. *)

(** {2 Perspective}
    @see <https://tailwindcss.com/docs/perspective> Perspective *)

val perspective_none : t
(** [perspective_none] removes perspective ({i perspective: none}). *)

val perspective_dramatic : t
(** [perspective_dramatic] sets a short perspective distance for dramatic 3D
    effects. *)

val perspective_normal : t
(** [perspective_normal] sets a normal perspective distance. *)

(** {2 Perspective Origin}
    @see <https://tailwindcss.com/docs/perspective-origin> Perspective Origin *)

val perspective_origin_center : t
(** [perspective_origin_center] sets perspective origin to center (default). *)

val perspective_origin_top : t
(** [perspective_origin_top] sets perspective origin to top, making 3D
    transforms appear from above. *)

val perspective_origin_bottom : t
(** [perspective_origin_bottom] sets perspective origin to bottom, making 3D
    transforms appear from below. *)

val perspective_origin_left : t
(** [perspective_origin_left] sets perspective origin to left side. *)

val perspective_origin_right : t
(** [perspective_origin_right] sets perspective origin to right side. *)

(** {2 Rotate}
    @see <https://tailwindcss.com/docs/rotate> Rotate *)

val rotate : int -> t
(** [rotate n] sets rotate transformation (degrees). *)

(** {2 Scale}
    @see <https://tailwindcss.com/docs/scale> Scale *)

val scale : int -> t
(** [scale n] resizes element by percentage (100 = normal size).

    Examples:
    - [scale 95]: Slightly smaller (95%)
    - [scale 100]: Normal size
    - [scale 105]: Slightly larger (105%) - nice for hover effects
    - [scale 150]: 1.5x larger

    Often combined with transition_transform for smooth scaling. *)

(** {2 Skew}
    @see <https://tailwindcss.com/docs/skew> Skew *)

(** TODO: Implement skew utilities. *)

(** {2 Transform}
    @see <https://tailwindcss.com/docs/transform> Transform *)

val transform : t
(** [transform] enables transform utilities. *)

val transform_none : t
(** [transform_none] disables transforms. *)

val transform_gpu : t
(** [transform_gpu] uses GPU acceleration for transforms. *)

(** {2 Transform Origin}
    @see <https://tailwindcss.com/docs/transform-origin> Transform Origin *)

(** TODO: Implement transform-origin (2D) utilities. *)

(** {2 Transform Style}
    @see <https://tailwindcss.com/docs/transform-style> Transform Style *)

(** {2 Translate}
    @see <https://tailwindcss.com/docs/translate> Translate *)

val translate_x : int -> t
(** [translate_x n] sets horizontal translation. *)

val translate_y : int -> t
(** [translate_y n] sets vertical translation. *)

val neg_translate_x_1_2 : t
(** [neg_translate_x_1_2] translates the element -50% horizontally (for
    centering). *)

val neg_translate_y_1_2 : t
(** [neg_translate_y_1_2] translates the element -50% vertically (for
    centering). *)

val rotate_x : int -> t
(** [rotate_x n] rotates element around X-axis by n degrees. Positive values
    tilt element away from viewer at top. *)

val rotate_y : int -> t
(** [rotate_y n] rotates element around Y-axis by n degrees. Positive values
    rotate element clockwise when viewed from above. *)

val rotate_z : int -> t
(** [rotate_z n] rotates element around Z-axis by n degrees. Equivalent to
    regular rotate, positive values rotate clockwise. *)

val translate_z : int -> t
(** [translate_z n] moves element along Z-axis (toward/away from viewer).
    Positive values move element closer, negative values move it away. *)

val scale_z : int -> t
(** [scale_z n] scales element along Z-axis for 3D effects. Requires
    transform-style: preserve-3d on parent. *)

val transform_style_3d : t
(** [transform_style_3d] preserves 3D positioning of child elements. Required
    for nested 3D transforms. *)

val transform_style_flat : t
(** [transform_style_flat] flattens child elements into the parent's plane
    (default). *)

(** {2 Container Query Utilities}

    Container queries allow elements to respond to their container's size rather
    than the viewport. Inspired by modern CSS capabilities. *)

val container : t
(** [container] creates a responsive layout container (equivalent to
    [.container] in Tailwind v4). Sets [width: 100%]. Note: Responsive
    [max-width] at various breakpoints not yet implemented. *)

val at_container : t
(** [at_container] enables container queries based on inline-size (equivalent to
    [@container] in Tailwind v4). Sets [container-type: inline-size]. *)

val at_container_normal : t
(** [at_container_normal] disables container queries (equivalent to
    [@container-normal] in Tailwind v4). Sets [container-type: normal]. *)

val at_container_named : string -> t
(** [at_container_named name] creates a named container (equivalent to
    [@container/name] in Tailwind v4). Sets [container-type: inline-size] and
    [container-name: name]. *)

val container_sm : t list -> t
(** [container_sm styles] applies [styles] when container is ≥640px wide. *)

val container_md : t list -> t
(** [container_md styles] applies [styles] when container is ≥768px wide. *)

val container_lg : t list -> t
(** [container_lg styles] applies [styles] when container is ≥1024px wide. *)

val container_xl : t list -> t
(** [container_xl styles] applies [styles] when container is ≥1280px wide. *)

val container_2xl : t list -> t
(** [container_2xl styles] applies [styles] when container is ≥1536px wide. *)

val container_query : ?name:string -> int -> t list -> t
(** [container_query 500 styles] applies styles when container is ≥500px.
    [container_query ~name:"sidebar" 500 styles] targets a named container. *)

val animate_none : t
(** [animate_none] applies no animation. *)

val animate_spin : t
(** [animate_spin] spins the element 360° continuously. Perfect for loading
    spinners. *)

val animate_ping : t
(** [animate_ping] scales and fades out like a radar ping. Great for
    notification badges or attention-grabbing indicators. *)

val animate_pulse : t
(** [animate_pulse] gently fades in and out. Useful for skeleton screens or
    loading placeholders. *)

val animate_bounce : t
(** [animate_bounce] makes the element bounce up and down. Good for scroll
    indicators or playful UI elements. *)

(** {1 Forms}
    @see <https://github.com/tailwindlabs/tailwindcss-forms> Forms Plugin *)

val form_input : t
(** [form_input] provides base styles for input elements; resets browser
    defaults and provides consistent styling across browsers. Use with input
    elements. *)

val form_textarea : t
(** [form_textarea] provides base styles for textarea elements; ensures
    consistent cross-browser appearance and behavior. *)

val form_select : t
(** [form_select] provides base styles for select dropdowns; normalizes
    appearance across browsers while maintaining native functionality. *)

val form_checkbox : t
(** [form_checkbox] provides base styles for checkbox inputs; enables custom
    styling while maintaining accessibility. *)

val form_radio : t
(** [form_radio] provides base styles for radio inputs; enables custom styling
    while maintaining accessibility. *)

(** {1 Interactivity & Scroll} *)

(** {2 Accent Color}
    @see <https://tailwindcss.com/docs/accent-color> Accent Color *)

val accent : color -> int -> t
(** [accent color shade] sets the accent color for form controls like checkboxes
    and radio buttons. *)

(** {2 Caret Color}
    @see <https://tailwindcss.com/docs/caret-color> Caret Color *)

(** TODO: Implement caret-color utilities. *)

(** {2 Color Scheme}
    @see <https://tailwindcss.com/docs/color-scheme> Color Scheme *)

(** TODO: Implement color-scheme utilities. *)

(** {2 Cursor}
    @see <https://tailwindcss.com/docs/cursor> Cursor *)

val cursor_auto : t
(** [cursor_auto] uses the automatic cursor. *)

val cursor_default : t
(** [cursor_default] uses the default cursor. *)

val cursor_pointer : t
(** [cursor_pointer] uses the pointer cursor. *)

val cursor_wait : t
(** [cursor_wait] uses the wait cursor. *)

val cursor_move : t
(** [cursor_move] uses the move cursor. *)

val cursor_not_allowed : t
(** [cursor_not_allowed] uses the not-allowed cursor. *)

(** {2 Field Sizing}
    @see <https://tailwindcss.com/docs/field-sizing> Field Sizing *)

(** TODO: Implement field-sizing utilities. *)

(** {2 User Select}
    @see <https://tailwindcss.com/docs/user-select> User Select *)

val select_none : t
(** [select_none] disables text selection. *)

val select_text : t
(** [select_text] enables text selection. *)

val select_all : t
(** [select_all] selects all text on focus. *)

val select_auto : t
(** [select_auto] uses automatic text selection. *)

(** {2 Pointer Events}
    @see <https://tailwindcss.com/docs/pointer-events> Pointer Events *)

val pointer_events_none : t
(** [pointer_events_none] disables pointer events. *)

val pointer_events_auto : t
(** [pointer_events_auto] enables pointer events. *)

(** {2 Scroll Snap Type}
    @see <https://tailwindcss.com/docs/scroll-snap-type> Scroll Snap Type *)

val snap_none : t
(** [snap_none] disables scroll snapping. *)

val snap_x : t
(** [snap_x] enables horizontal scroll snapping for carousel-like interfaces.
    Must be used with snap_start/center/end on children.

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
(** [snap_y] enables vertical scroll snapping. Similar to [snap_x] but for
    vertical scrolling. *)

val snap_both : t
(** [snap_both] enables both horizontal and vertical scroll snapping. *)

val snap_mandatory : t
(** [snap_mandatory] enforces mandatory scroll snapping. *)

val snap_proximity : t
(** [snap_proximity] enables proximity-based scroll snapping. *)

(** {2 Scroll Snap Align}
    @see <https://tailwindcss.com/docs/scroll-snap-align> Scroll Snap Align *)

val snap_start : t
(** [snap_start] snaps to the start of the container. *)

val snap_end : t
(** [snap_end] snaps to the end of the container. *)

val snap_center : t
(** [snap_center] snaps to the center of the container. *)

val snap_align_none : t
(** [snap_align_none] disables snap alignment. *)

(** {2 Scroll Snap Stop}
    @see <https://tailwindcss.com/docs/scroll-snap-stop> Scroll Snap Stop *)

val snap_normal : t
(** [snap_normal] uses normal snap stop behavior. *)

val snap_always : t
(** [snap_always] always stops at snap positions. *)

(** {2 Scroll Behavior}
    @see <https://tailwindcss.com/docs/scroll-behavior> Scroll Behavior *)

val scroll_auto : t
(** [scroll_auto] uses auto scroll behavior. *)

val scroll_smooth : t
(** [scroll_smooth] uses smooth scroll behavior. *)

(** {2 Touch Action}
    @see <https://tailwindcss.com/docs/touch-action> Touch Action *)

val touch_auto : t
(** [touch_auto] sets touch-action to auto. *)

val touch_none : t
(** [touch_none] disables touch gestures. *)

val touch_manipulation : t
(** [touch_manipulation] enables panning and pinch zoom gestures. *)

val touch_pan_x : t
(** [touch_pan_x] enables horizontal panning. *)

val touch_pan_y : t
(** [touch_pan_y] enables vertical panning. *)

(** {2 Scroll Margin}
    @see <https://tailwindcss.com/docs/scroll-margin> Scroll Margin *)

(** TODO: Implement scroll-margin utilities. *)

(** {2 Scroll Padding}
    @see <https://tailwindcss.com/docs/scroll-padding> Scroll Padding *)

(** TODO: Implement scroll-padding utilities. *)

(** {2 Appearance}
    @see <https://tailwindcss.com/docs/appearance> Appearance *)

val appearance_none : t
(** [appearance_none] removes default browser styling from form elements. *)

(** {2 Resize}
    @see <https://tailwindcss.com/docs/resize> Resize *)

val resize_none : t
(** [resize_none] prevents textarea resizing. *)

val resize_y : t
(** [resize_y] allows vertical resizing only. *)

val resize_x : t
(** [resize_x] allows horizontal resizing only. *)

val resize : t
(** [resize] allows both horizontal and vertical resizing. *)

(** {2 Will Change}
    @see <https://tailwindcss.com/docs/will-change> Will Change *)

val will_change_auto : t
(** [will_change_auto] sets will-change to auto. *)

val will_change_scroll : t
(** [will_change_scroll] optimizes for scroll position changes. *)

val will_change_contents : t
(** [will_change_contents] optimizes for content changes. *)

val will_change_transform : t
(** [will_change_transform] optimizes for transform changes. *)

(** {1 SVG} *)

(** {2 Fill}
    @see <https://tailwindcss.com/docs/fill> Fill *)

(** Fill utilities are available in the Svg module. *)

(** {2 Stroke}
    @see <https://tailwindcss.com/docs/stroke> Stroke *)

(** Stroke utilities are available in the Svg module. *)

(** {2 Stroke Width}
    @see <https://tailwindcss.com/docs/stroke-width> Stroke Width *)

(** Stroke width utilities are available in the Svg module. *)

(** {1 Accessibility} *)

(** {2 Forced Color Adjust}
    @see <https://tailwindcss.com/docs/forced-color-adjust> Forced Color Adjust
*)

(** Accessibility utilities (e.g., sr-only) are available above. *)

val sr_only : t
(** [sr_only] Screen reader only; visually hides content while keeping it
    accessible. *)

val not_sr_only : t
(** [not_sr_only] reverses [sr_only]; it makes previously screen-reader-only
    content visible. *)

(** {1 Prose Typography}

    The prose classes provide beautiful typographic defaults for long-form
    content like articles, blog posts, or documentation. They automatically
    style headings, paragraphs, lists, code blocks, and more.

    @see <https://tailwindcss.com/docs/typography-plugin> Typography Plugin *)

val prose : t
(** [prose] applies default prose styling for article-like content.
    Automatically styles h1–h6, p, ul, ol, blockquote, code, and more.

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
(** [prose_sm] applies small prose styling (0.875rem base font). *)

val prose_lg : t
(** [prose_lg] applies large prose styling (1.125rem base font). *)

val prose_xl : t
(** [prose_xl] applies extra large prose styling (1.25rem base font). *)

val prose_2xl : t
(** [prose_2xl] applies 2× large prose styling (1.5rem base font). *)

val prose_lead : t
(** [prose_lead] applies lead paragraph styling for prose. *)

val prose_gray : t
(** [prose_gray] uses the gray prose color theme. *)

val prose_slate : t
(** [prose_slate] uses the slate prose color theme. *)

(** {2 Prose markers}

    Convenience utilities to apply marker classes used by prose selectors. *)

val not_prose : t
(** [not_prose] marks a subtree to be excluded from prose styling. Alias of
    [Prose.not_prose]. *)

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

(** {2 CSS Generation}

    This library generates Tailwind-like class names using [to_classes].

    {b Important}: Class tracking and CSS file generation should be handled by
    the library user. For example, the {!Tw_html} module collects all used Tw
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

module Color = Color
(** Color conversion utilities for Tailwind v4 compatibility

    Provides OKLCH color space conversion and Tailwind v4 color values. *)

module Modifiers = Modifiers

module Var = Var
(** CSS variable tracking and generation *)

(* Version module is now in the css library *)

val to_css :
  ?base:bool ->
  ?forms:bool ->
  ?mode:Css.mode ->
  ?layers:bool ->
  ?optimize:bool ->
  t list ->
  Css.t
(** [to_css ?base ?forms ?mode ?optimize styles] generates a CSS stylesheet for
    the given styles.

    The generated CSS follows Tailwind's layering and ordering conventions:

    {b 1. Layer Order:}
    - {i base}: resets, preflight styles, and semantic element defaults
    - {i components}: component classes (e.g., container or plugin-provided)
    - {i utilities}: atomic utility classes (p-4, bg-red-500, etc.)

    {b 2. Utility Ordering (within utilities layer):} Utilities are sorted by
    category in the following order:
    - {i Layout}: position, display, float, clear, isolation, object-fit,
      overflow, overscroll
    - {i Flexbox & Grid}: flex properties, grid properties, gap, order,
      place-content/items/self
    - {i Spacing}: padding, margin, space-between
    - {i Sizing}: width, height, min/max dimensions
    - {i Typography}: font-family, font-size, font-weight, letter-spacing,
      line-height, text properties
    - {i Backgrounds}: background-color, background-image, gradients
    - {i Borders}: border-width, border-color, border-radius, border-style
    - {i Effects}: box-shadow, opacity, mix-blend-mode
    - {i Filters}: filter, backdrop-filter, blur, brightness, contrast
    - {i Tables}: border-collapse, border-spacing, table-layout
    - {i Transitions & Animation}: transition, duration, timing, delay,
      animation
    - {i Transforms}: scale, rotate, translate, skew
    - {i Interactivity}: cursor, user-select, resize, scroll-behavior
    - {i SVG}: fill, stroke
    - {i Accessibility}: sr-only, not-sr-only

    Within each category, utilities maintain a stable order. Variants (hover:,
    sm:, etc.) are applied with responsive breakpoints in ascending order (sm →
    md → lg → xl → 2xl) and state modifiers in Tailwind's predetermined priority
    order.

    {b Mode and base behavior}
    - [mode=Variables] (default): emits layered output. When [base=true], the
      Base layer (Preflight reset and semantic defaults) is included; when
      [base=false], the Base layer is omitted but Theme/Components/Utilities
      layers remain present.
    - [mode=Inline]: resolves values and emits raw rules without any layers. In
      this mode, [reset] has no effect on layering.

    @param base Include base (Preflight) styles (default: [true])
    @param forms
      Include forms plugin base styles. When [true], adds base styles for native
      HTML form elements. When not specified, auto-detects based on utility
      classes (form-input, etc.)
    @param mode CSS generation mode (default: [Variables])

    Use this to generate your main stylesheet for inclusion in HTML [<head>]. *)

val clip_polygon : (float * float) list -> t
(** [clip_polygon points] clips element to a polygon defined by percentage
    points.

    Example:
    {[
      (* Create a triangular badge/indicator *)
      span
        ~tw:[ clip_polygon [ (50., 0.); (0., 100.); (100., 100.) ]; bg red ]
        []
    ]} *)

(** {1 Modifiers}
    Tailwind {e variants} like hover/focus, responsive breakpoints, dark mode,
    group/peer, and pseudo-elements.

    @see <https://tailwindcss.com/docs/hover-focus-and-other-states>
      Hover, Focus, and Other States
    @see <https://tailwindcss.com/docs/responsive-design> Responsive Design
    @see <https://tailwindcss.com/docs/dark-mode> Dark Mode *)

(** {2 State Variants} *)

val hover : t list -> t
(** [hover styles] applies [styles] on :hover. *)

val focus : t list -> t
(** [focus styles] applies [styles] on :focus. *)

val active : t list -> t
(** [active styles] applies [styles] on :active. *)

val disabled : t list -> t
(** [disabled styles] applies [styles] on :disabled. *)

val focus_within : t list -> t
(** [focus_within styles] applies [styles] when any descendant has focus. *)

val focus_visible : t list -> t
(** [focus_visible styles] applies [styles] when focus is {i visible}. *)

(** {2 Group/Peer Variants} *)

val group : t
(** [group] marks an element as a group parent. *)

val peer : t
(** [peer] marks an element as a peer for sibling selectors. *)

val group_hover : t list -> t
(** [group_hover styles] applies [styles] when [.group]:hover matches. *)

val group_focus : t list -> t
(** [group_focus styles] applies [styles] when [.group]:focus matches. *)

val peer_hover : t list -> t
(** [peer_hover styles] applies [styles] when a preceding [.peer]:hover matches.
*)

val peer_focus : t list -> t
(** [peer_focus styles] applies [styles] when a preceding [.peer]:focus matches.
*)

val has : string -> t list -> t
(** [has selector styles] applies [styles] with [:has(selector)]. *)

val group_has : string -> t list -> t
(** [group_has selector styles] applies [styles] under [.group:has(selector)].
*)

val peer_has : string -> t list -> t
(** [peer_has selector styles] applies [styles] when a preceding
    [.peer:has(selector)]. *)

(** {2 Theme/Motion/Contrast} *)

val dark : t list -> t
(** [dark styles] applies [styles] when user prefers dark mode.
    @see <https://tailwindcss.com/docs/dark-mode> Dark Mode. *)

val motion_safe : t list -> t
(** [motion_safe styles] applies [styles] when reduced motion is not requested.
*)

val motion_reduce : t list -> t
(** [motion_reduce styles] applies [styles] when reduced motion is requested. *)

val contrast_more : t list -> t
(** [contrast_more styles] applies [styles] when higher contrast is preferred.
*)

val contrast_less : t list -> t
(** [contrast_less styles] applies [styles] when lower contrast is preferred. *)

val starting : t list -> t
(** [starting styles] applies styles using [@starting-style]. *)

(** {2 Responsive Breakpoints}
    @see <https://tailwindcss.com/docs/responsive-design> Responsive Design *)

val sm : t list -> t
(** [sm styles] applies [styles] at small breakpoint. *)

val md : t list -> t
(** [md styles] applies [styles] at medium breakpoint. *)

val lg : t list -> t
(** [lg styles] applies [styles] at large breakpoint. *)

val xl : t list -> t
(** [xl styles] applies [styles] at extra-large breakpoint. *)

val xl2 : t list -> t
(** [xl2 styles] applies [styles] at 2xl breakpoint. *)

(** {2 Pseudo-elements} *)

val before : t list -> t
(** [before styles] applies [styles] to ::before. Combine with [content]
    utilities. *)

val after : t list -> t
(** [after styles] applies [styles] to ::after. Combine with [content]
    utilities. *)

(** {2 ARIA Variants} *)

val aria_checked : t list -> t
(** [aria_checked styles] applies [styles] when aria-checked=true. *)

val aria_expanded : t list -> t
(** [aria_expanded styles] applies [styles] when aria-expanded=true. *)

val aria_selected : t list -> t
(** [aria_selected styles] applies [styles] when aria-selected=true. *)

val aria_disabled : t list -> t
(** [aria_disabled styles] applies [styles] when aria-disabled=true. *)

(** {2 Data Attributes} *)

val data_state : string -> t -> t
(** [data_state value style] applies [style] when data-state=[value]. *)

val data_variant : string -> t -> t
(** [data_variant value style] applies [style] when data-variant=[value]. *)

val data_custom : string -> string -> t -> t
(** [data_custom key value style] applies [style] when data-[key]=[value]. *)

val data_active : t list -> t
(** [data_active styles] applies [styles] when data-active is present. *)

val data_inactive : t list -> t
(** [data_inactive styles] applies [styles] when data-inactive is present. *)

val peer_checked : t list -> t
(** [peer_checked styles] applies [styles] when a preceding [.peer]:checked. *)

module Style = Style
module Margin = Margin
module Padding = Padding
module Gap = Gap
module Flex = Flex
module Flex_props = Flex_props
module Flex_layout = Flex_layout
module Alignment = Alignment
module Cursor = Cursor
module Borders = Borders
module Backgrounds = Backgrounds
module Sizing = Sizing
module Layout = Layout
module Overflow = Overflow
module Typography = Typography
module Grid = Grid
module Grid_item = Grid_item
module Grid_template = Grid_template
module Effects = Effects
module Transforms = Transforms
module Interactivity = Interactivity
module Containers = Containers
module Filters = Filters
module Position = Position
module Animations = Animations
module Transitions = Transitions
module Forms = Forms
module Tables = Tables
module Svg = Svg
module Accessibility = Accessibility
module Rules = Rules
module Theme = Theme
module Utility = Utility
module Spacing = Spacing
