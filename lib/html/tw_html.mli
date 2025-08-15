(** A library for building HTML documents programmatically.

    This module provides a type-safe and declarative API for creating HTML
    elements and attributes. It is designed to be used for generating static
    HTML pages. *)

type t
(** The abstract type for an HTML node. *)

type tw = Tw.t
(** Type alias for Tailwind styles for convenience. *)

type attr
(** The abstract type for an HTML attribute. *)

module At : sig
  (** Functions for creating HTML attributes. *)

  val id : string -> attr
  (** [id "my-id"] creates an [id] attribute. *)

  (** {1 Global attributes} *)

  val title : string -> attr
  (** [title "My Title"] creates a [title] attribute. *)

  val lang : string -> attr
  (** [lang "en"] creates a [lang] attribute. *)

  val dir : string -> attr
  (** [dir "ltr"] creates a [dir] attribute. *)

  val tabindex : int -> attr
  (** [tabindex 0] creates a [tabindex] attribute. *)

  val contenteditable : bool -> attr
  (** [contenteditable true] creates a [contenteditable] attribute. *)

  val spellcheck : string -> attr
  (** [spellcheck value] sets the spellcheck attribute. Use "true" or "false".
  *)

  val onclick : string -> attr
  (** [onclick "..."] creates an [onclick] attribute. *)

  (** {1 Event attributes} *)

  val onchange : string -> attr
  (** [onchange "..."] creates an [onchange] attribute. *)

  val oninput : string -> attr
  (** [oninput "..."] creates an [oninput] attribute. *)

  val onsubmit : string -> attr
  (** [onsubmit "..."] creates an [onsubmit] attribute. *)

  val type' : string -> attr
  (** [type' "text/css"] creates a [type] attribute. *)

  (** {1 Form attributes} *)

  val value : string -> attr
  (** [value "..."] creates a [value] attribute. *)

  val name : string -> attr
  (** [name "description"] creates a [name] attribute. *)

  val placeholder : string -> attr
  (** [placeholder "..."] creates a [placeholder] attribute. *)

  val required : attr
  (** The [required] attribute. *)

  val disabled : attr
  (** The [disabled] attribute. *)

  val checked : attr
  (** The [checked] attribute. *)

  val readonly : attr
  (** The [readonly] attribute. *)

  val href : string -> attr
  (** [href "/path"] creates an [href] attribute. *)

  (** {1 Link attributes} *)

  val target : string -> attr
  (** [target "_blank"] creates a [target] attribute. *)

  val rel : string -> attr
  (** [rel "stylesheet"] creates a [rel] attribute. *)

  val download : string -> attr
  (** [download "file.pdf"] creates a [download] attribute. *)

  val src : string -> attr
  (** [src "/image.png"] creates a [src] attribute. *)

  (** {1 Media attributes} *)

  val alt : string -> attr
  (** [alt "description"] creates an [alt] attribute for images. *)

  val width : int -> attr
  (** [width 800] creates a [width] attribute. *)

  val height : int -> attr
  (** [height 600] creates a [height] attribute. *)

  val loading : string -> attr
  (** [loading "lazy"] creates a [loading] attribute. *)

  val charset : string -> attr
  (** [charset "utf-8"] creates a [charset] attribute. *)

  (** {1 Meta attributes} *)

  val content : string -> attr
  (** [content "..."] creates a [content] attribute. *)

  val property : string -> attr
  (** [property "og:title"] creates a [property] attribute. *)

  val style : string -> attr
  (** [style "color:red;"] creates a [style] attribute. *)

  (** {1 Style attribute} *)

  val datetime : string -> attr
  (** [datetime "2025-07-28"] creates a [datetime] attribute. *)

  (** {1 Time attributes} *)

  val srcset : string -> attr
  (** [srcset "..."] creates a [srcset] attribute for responsive images. *)

  (** {1 Image attributes} *)

  val sizes : string -> attr
  (** [sizes "..."] creates a [sizes] attribute for responsive images. *)

  val title' : string -> attr
  (** [title' "My Page"] creates a [title] attribute for elements. *)

  (** {1 Additional attributes} *)

  val loading_lazy : attr
  (** The [loading="lazy"] attribute. *)

  val v : string -> string -> attr
  (** [v key value] creates a generic key-value attribute. *)

  val void : attr
  (** [void] is an empty attribute that doesn't render. *)

  val true' : string -> attr
  (** [true' key] creates a boolean attribute with a "true" value. *)

  val false' : string -> attr
  (** [false' key] creates a boolean attribute with a "false" value. *)

  val if' : bool -> attr -> attr
  (** [if' cond at] is [at] if [cond] is true, otherwise returns a void
      attribute. *)

  val if_some : attr option -> attr
  (** [if_some o] is [at] if [o] is [Some at] and void if [o] is [None]. *)

  val is_void : attr -> bool
  (** [is_void at] is [true] if the attribute is void (won't render). *)

  val to_pair : attr -> string * string
  (** [to_pair at] returns the attribute as a (name, value) pair. *)

  val of_pair : string * string -> attr
  (** [of_pair (n, v)] creates an attribute from a (name, value) pair. *)

  (** {2 Additional HTML5 attributes} *)

  val accesskey : string -> attr
  (** [accesskey key] sets keyboard shortcut. *)

  val action : string -> attr
  (** [action url] sets form action URL. *)

  val autocomplete : string -> attr
  (** [autocomplete value] sets autocomplete behavior. *)

  val autofocus : attr
  (** [autofocus] sets element to be focused on page load. *)

  val cols : int -> attr
  (** [cols n] sets textarea columns. *)

  val colspan : int -> attr
  (** [colspan n] sets table cell column span. *)

  val defer : attr
  (** [defer] defers script execution. *)

  val draggable : bool -> attr
  (** [draggable b] sets whether element is draggable. *)

  val for' : string -> attr
  (** [for' id] associates label with form element. *)

  val hidden : attr
  (** [hidden] hides the element. *)

  val list : string -> attr
  (** [list id] associates input with datalist. *)

  val method' : string -> attr
  (** [method' m] sets form HTTP method. *)

  val media : string -> attr
  (** [media query] sets media query for resources. *)

  val popover : string -> attr
  (** [popover value] sets popover behavior. *)

  val popovertarget : string -> attr
  (** [popovertarget id] sets popover target element. *)

  val popovertargetaction : string -> attr
  (** [popovertargetaction action] sets popover action. *)

  val rows : int -> attr
  (** [rows n] sets textarea rows. *)

  val rowspan : int -> attr
  (** [rowspan n] sets table cell row span. *)

  val selected : attr
  (** [selected] marks option as selected. *)

  val wrap : string -> attr
  (** [wrap mode] sets textarea wrap mode. *)

  (** {2 SVG Attributes} *)

  val fill_rule : [ `evenodd ] -> attr
  (** [fill_rule `evenodd] sets the fill rule *)

  val clip_rule : [ `evenodd ] -> attr
  (** [clip_rule `evenodd] sets the clipping rule *)

  val cx : int -> attr
  (** [cx n] sets the center x coordinate *)

  val cy : int -> attr
  (** [cy n] sets the center y coordinate *)

  val r : int -> attr
  (** [r n] sets the radius *)

  val view_box : string -> attr
  (** [view_box "0 0 20 20"] sets the viewBox *)

  val fill : string -> attr
  (** [fill "currentColor"] sets the fill color *)

  val stroke : string -> attr
  (** [stroke "currentColor"] sets the stroke color *)

  val stroke_width : string -> attr
  (** [stroke_width "2"] sets the stroke width *)

  val stroke_linecap : string -> attr
  (** [stroke_linecap "round"] sets the stroke line cap *)

  val stroke_linejoin : string -> attr
  (** [stroke_linejoin "round"] sets the stroke line join *)

  val x : string -> attr
  (** [x "10"] sets the x coordinate *)

  val y : string -> attr
  (** [y "10"] sets the y coordinate *)

  val rx : string -> attr
  (** [rx "5"] sets the x radius for rounded rectangles *)

  val d : string -> attr
  (** [d "M10 10 L20 20"] sets the path data *)

  val x1 : string -> attr
  (** [x1 "0"] sets the first x coordinate *)

  val y1 : string -> attr
  (** [y1 "0"] sets the first y coordinate *)

  val x2 : string -> attr
  (** [x2 "20"] sets the second x coordinate *)

  val y2 : string -> attr
  (** [y2 "20"] sets the second y coordinate *)
end

(** {1 Text helpers} *)

val txt : string -> t
(** [txt s] creates a text node from string [s]. *)

val txtf : string list -> t
(** [txtf ["a"; "b"]] is equivalent to [txt "ab"]. *)

val raw : string -> t
(** [raw html] creates a node from raw HTML string. Use with caution. *)

val rawf : string list -> t
(** [rawf ["a"; "b"]] is equivalent to [raw "ab"]. *)

val empty : t
(** [empty] is the empty element, equivalent to no content. *)

(** {1 Aria module}

    Accessibility attributes following ARIA standards. *)
module Aria : sig
  val label : string -> attr
  (** [label "description"] creates an aria-label attribute. *)

  val labelledby : string -> attr
  (** [labelledby "id"] creates an aria-labelledby attribute. *)

  val describedby : string -> attr
  (** [describedby "id"] creates an aria-describedby attribute. *)

  val hidden : attr
  (** [hidden] is the aria-hidden="true" attribute. *)

  val expanded : bool -> attr
  (** [expanded true] creates an aria-expanded attribute. *)

  val current : string -> attr
  (** [current "page"] creates an aria-current attribute. *)

  val role : string -> attr
  (** [role "navigation"] creates a role attribute. *)
end

(** {1 Conversion functions} *)

val to_string : ?doctype:bool -> t -> string
(** [to_string ?doctype element] converts HTML element to string representation.
    If [doctype] is true (default: false), includes the HTML5 doctype
    declaration. *)

val to_tw : t -> tw list
(** [to_tw t] extracts all styling classes from an HTML tree. *)

(** {1 Page generation} *)

type page
(** Complete HTML page with integrated CSS.

    A [page] value represents a complete, self-contained HTML document with all
    required CSS automatically generated from the Tailwind utilities used in the
    HTML structure. This ensures consistency between your markup and styles
    without manual CSS management.

    {b Design rationale}: The type is abstract to enforce the invariant that the
    CSS contains exactly the styles needed for the HTML content. Direct
    construction would break this guarantee. *)

val page :
  ?lang:string ->
  ?meta:(string * string) list ->
  ?title:string ->
  ?charset:string ->
  ?tw_css:string ->
  t list ->
  t list ->
  page
(** [page ?lang ?meta ?title ?charset ?tw_css head body] generates a complete
    HTML page and its corresponding CSS.

    - [lang] defaults to ["en"]
    - [charset] defaults to ["utf-8"]
    - [meta] is a list of [(name, content)] pairs for meta tags
    - [title] is the page title
    - [tw_css] defaults to ["tw.css"] - the filename for the CSS, automatically
      included as a [<link>] tag in HTML head
    - [head] is additional content for the head section
    - [body] is the body content

    The HTML automatically includes a [<link rel="stylesheet" href="{tw_css}">]
    tag. Use {!html} to get the HTML string and {!css} to get the CSS filename
    and stylesheet. *)

val html : page -> string
(** [html page] extracts the HTML string from a page result. *)

val css : page -> string * Tw.Css.t
(** [css page] extracts the CSS filename and stylesheet from a page result. *)

(** {1 Livereload module}

    Development-time live reloading support. *)
module Livereload : sig
  val enabled : bool
  (** [enabled] is true if SITE_LIVERELOAD environment variable is "true". *)

  val endpoint : string
  (** [endpoint] is the WebSocket endpoint for livereload. Defaults to
      "ws://localhost:8080" or SITE_LIVERELOAD_ENDPOINT. *)

  val script : t
  (** [script] is the JavaScript code for livereload functionality. Only
      included when [enabled] is true. *)
end

(** {1 HTML Elements} *)

val div : ?at:attr list -> ?tw:tw list -> t list -> t
(** [div ?at ?tw children] is a div element. *)

val span : ?at:attr list -> ?tw:tw list -> t list -> t
(** [span ?at ?tw children] is a span element. *)

val p : ?at:attr list -> ?tw:tw list -> t list -> t
(** [p ?at ?tw children] is a paragraph element. *)

val a : ?at:attr list -> ?tw:tw list -> t list -> t
(** [a ?at ?tw children] is an anchor/link element. *)

val ul : ?at:attr list -> ?tw:tw list -> t list -> t
(** [ul ?at ?tw children] is an unordered list element. *)

val li : ?at:attr list -> ?tw:tw list -> t list -> t
(** [li ?at ?tw children] is a list item element. *)

val nav : ?at:attr list -> ?tw:tw list -> t list -> t
(** [nav ?at ?tw children] is a navigation element. *)

val section : ?at:attr list -> ?tw:tw list -> t list -> t
(** [section ?at ?tw children] is a section element. *)

val article : ?at:attr list -> ?tw:tw list -> t list -> t
(** [article ?at ?tw children] is an article element. *)

val header : ?at:attr list -> ?tw:tw list -> t list -> t
(** [header ?at ?tw children] is a header element. *)

val footer : ?at:attr list -> ?tw:tw list -> t list -> t
(** [footer ?at ?tw children] is a footer element. *)

val h1 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h1 ?at ?tw children] is an h1 heading element. *)

val h2 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h2 ?at ?tw children] is an h2 heading element. *)

val h3 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h3 ?at ?tw children] is an h3 heading element. *)

val h4 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h4 ?at ?tw children] is an h4 heading element. *)

val h5 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h5 ?at ?tw children] is an h5 heading element. *)

val h6 : ?at:attr list -> ?tw:tw list -> t list -> t
(** [h6 ?at ?tw children] is an h6 heading element. *)

val img : ?at:attr list -> ?tw:tw list -> unit -> t
(** [img ?at ?tw ()] is an img element. *)

val script : ?at:attr list -> ?tw:tw list -> t list -> t
(** [script ?at ?tw children] is a script element. *)

val meta : ?at:attr list -> ?tw:tw list -> unit -> t
(** [meta ?at ?tw ()] is a meta element. *)

val link : ?at:attr list -> ?tw:tw list -> unit -> t
(** [link ?at ?tw ()] is a link element. *)

val title : ?at:attr list -> ?tw:tw list -> t list -> t
(** [title ?at ?tw children] is a title element. *)

val head : ?at:attr list -> ?tw:tw list -> t list -> t
(** [head ?at ?tw children] is a head element. *)

val body : ?at:attr list -> ?tw:tw list -> t list -> t
(** [body ?at ?tw children] is a body element. *)

val root : ?at:attr list -> ?tw:tw list -> t list -> t
(** [root ?at ?tw children] is the root [<html>] element of a document. *)

val void : t
(** [void] is an empty void element. *)

val option : ?at:attr list -> ?tw:tw list -> t list -> t
(** [option ?at ?tw children] is an option element. *)

val select : ?at:attr list -> ?tw:tw list -> t list -> t
(** [select ?at ?tw children] is a select element. *)

val main : ?at:attr list -> ?tw:tw list -> t list -> t
(** [main ?at ?tw children] is a main element. *)

val aside : ?at:attr list -> ?tw:tw list -> t list -> t
(** [aside ?at ?tw children] is an aside element. *)

val time : ?at:attr list -> ?tw:tw list -> t list -> t
(** [time ?at ?tw children] is a time element. *)

val dialog : ?at:attr list -> ?tw:tw list -> t list -> t
(** [dialog ?at ?tw children] is a dialog element. *)

val data : ?at:attr list -> ?tw:tw list -> t list -> t
(** [data ?at ?tw children] is a data element. *)

val picture : ?at:attr list -> ?tw:tw list -> t list -> t
(** [picture ?at ?tw children] is a picture element. *)

val slot : ?at:attr list -> ?tw:tw list -> t list -> t
(** [slot ?at ?tw children] is a slot element. *)

val template : ?at:attr list -> ?tw:tw list -> t list -> t
(** [template ?at ?tw children] is a template element. *)

val form : ?at:attr list -> ?tw:tw list -> t list -> t
(** [form ?at ?tw children] is a form element. *)

val input : ?at:attr list -> ?tw:tw list -> unit -> t
(** [input ?at ?tw ()] is an input element. *)

val textarea : ?at:attr list -> ?tw:tw list -> t list -> t
(** [textarea ?at ?tw children] is a textarea element. *)

val button : ?at:attr list -> ?tw:tw list -> t list -> t
(** [button ?at ?tw children] is a button element. *)

val label : ?at:attr list -> ?tw:tw list -> t list -> t
(** [label ?at ?tw children] is a label element. *)

val fieldset : ?at:attr list -> ?tw:tw list -> t list -> t
(** [fieldset ?at ?tw children] is a fieldset element. *)

val legend : ?at:attr list -> ?tw:tw list -> t list -> t
(** [legend ?at ?tw children] is a legend element. *)

val details : ?at:attr list -> ?tw:tw list -> t list -> t
(** [details ?at ?tw children] is a details element. *)

val summary : ?at:attr list -> ?tw:tw list -> t list -> t
(** [summary ?at ?tw children] is a summary element. *)

val pre : ?at:attr list -> ?tw:tw list -> t list -> t
(** [pre ?at ?tw children] is a pre element. *)

val code : ?at:attr list -> ?tw:tw list -> t list -> t
(** [code ?at ?tw children] is a code element. *)

val em : ?at:attr list -> ?tw:tw list -> t list -> t
(** [em ?at ?tw children] is an emphasis element. *)

val strong : ?at:attr list -> ?tw:tw list -> t list -> t
(** [strong ?at ?tw children] is a strong element. *)

val small : ?at:attr list -> ?tw:tw list -> t list -> t
(** [small ?at ?tw children] is a small element. *)

val mark : ?at:attr list -> ?tw:tw list -> t list -> t
(** [mark ?at ?tw children] is a mark element. *)

val br : ?at:attr list -> ?tw:tw list -> unit -> t
(** [br ?at ?tw ()] is a br (line break) element. *)

val hr : ?at:attr list -> ?tw:tw list -> unit -> t
(** [hr ?at ?tw ()] is a hr (horizontal rule) element. *)

val table : ?at:attr list -> ?tw:tw list -> t list -> t
(** [table ?at ?tw children] is a table element. *)

val thead : ?at:attr list -> ?tw:tw list -> t list -> t
(** [thead ?at ?tw children] is a table head element. *)

val tbody : ?at:attr list -> ?tw:tw list -> t list -> t
(** [tbody ?at ?tw children] is a table body element. *)

val tr : ?at:attr list -> ?tw:tw list -> t list -> t
(** [tr ?at ?tw children] is a table row element. *)

val th : ?at:attr list -> ?tw:tw list -> t list -> t
(** [th ?at ?tw children] is a table header cell element. *)

val td : ?at:attr list -> ?tw:tw list -> t list -> t
(** [td ?at ?tw children] is a table data cell element. *)

val ol : ?at:attr list -> ?tw:tw list -> t list -> t
(** [ol ?at ?tw children] is an ordered list element. *)

val dl : ?at:attr list -> ?tw:tw list -> t list -> t
(** [dl ?at ?tw children] is a description list element. *)

val dt : ?at:attr list -> ?tw:tw list -> t list -> t
(** [dt ?at ?tw children] is a description term element. *)

val dd : ?at:attr list -> ?tw:tw list -> t list -> t
(** [dd ?at ?tw children] is a description definition element. *)

val blockquote : ?at:attr list -> ?tw:tw list -> t list -> t
(** [blockquote ?at ?tw children] is a blockquote element. *)

val figure : ?at:attr list -> ?tw:tw list -> t list -> t
(** [figure ?at ?tw children] is a figure element. *)

val figcaption : ?at:attr list -> ?tw:tw list -> t list -> t
(** [figcaption ?at ?tw children] is a figure caption element. *)

val video : ?at:attr list -> ?tw:tw list -> t list -> t
(** [video ?at ?tw children] is a video element. *)

val audio : ?at:attr list -> ?tw:tw list -> t list -> t
(** [audio ?at ?tw children] is an audio element. *)

val source : ?at:attr list -> ?tw:tw list -> unit -> t
(** [source ?at ?tw ()] is a source element. *)

val canvas : ?at:attr list -> ?tw:tw list -> t list -> t
(** [canvas ?at ?tw children] is a canvas element. *)

val iframe : ?at:attr list -> ?tw:tw list -> t list -> t
(** [iframe ?at ?tw children] is an iframe element. *)

(** {1 SVG Support} *)

(** {2 SVG Elements} *)

val svg : ?at:attr list -> ?tw:tw list -> t list -> t
(** [svg ?at ?tw children] creates an SVG root element. *)

val g : ?at:attr list -> ?tw:tw list -> t list -> t
(** [g ?at ?tw children] creates a group element. *)

val circle : ?at:attr list -> ?tw:tw list -> t list -> t
(** [circle ?at ?tw children] creates a circle element. *)

val rect : ?at:attr list -> ?tw:tw list -> t list -> t
(** [rect ?at ?tw children] creates a rectangle element. *)

val path : ?at:attr list -> ?tw:tw list -> t list -> t
(** [path ?at ?tw children] creates a path element. *)

val line : ?at:attr list -> ?tw:tw list -> t list -> t
(** [line ?at ?tw children] creates a line element. *)

val pp : t -> string
(** [pp t] pretty-prints HTML element [t]. *)
