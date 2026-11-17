(** HTML documents styled with Tailwind utilities.

    [Tw_html] builds HTML elements and attributes as typed values. Each element
    carries the {!Tw.t} utilities given through [~tw], so a {!page} generates
    exactly the CSS its markup uses. *)

type t
(** The abstract type for an HTML node. *)

type tw = Tw.t
(** See {!Tw.t}. *)

type attr
(** The abstract type for an HTML attribute. *)

module At : sig
  (** HTML attributes. *)

  val id : string -> attr
  (** [id "my-id"] is an [id] attribute. *)

  val class' : string -> attr
  (** [class' names] is a [class] attribute. The names in [names] that are
      utilities join the element's [~tw] ones, and the others are rendered as
      given and reported by {!unknown_classes}. *)

  (** {1 Global attributes} *)

  val title : string -> attr
  (** [title s] is a [title] attribute. *)

  val lang : string -> attr
  (** [lang "en"] is a [lang] attribute. *)

  val dir : string -> attr
  (** [dir "ltr"] is a [dir] attribute. *)

  val tabindex : int -> attr
  (** [tabindex 0] is a [tabindex] attribute. *)

  val contenteditable : bool -> attr
  (** [contenteditable true] is a [contenteditable] attribute. *)

  val spellcheck : string -> attr
  (** [spellcheck v] is a [spellcheck] attribute, [v] being ["true"] or
      ["false"]. *)

  (** {1 Event attributes} *)

  val onclick : string -> attr
  (** [onclick "..."] is an [onclick] attribute. *)

  val onchange : string -> attr
  (** [onchange "..."] is an [onchange] attribute. *)

  val oninput : string -> attr
  (** [oninput "..."] is an [oninput] attribute. *)

  val onsubmit : string -> attr
  (** [onsubmit "..."] is an [onsubmit] attribute. *)

  (** {1 Form attributes} *)

  val type' : string -> attr
  (** [type' "text/css"] is a [type] attribute. *)

  val value : string -> attr
  (** [value "..."] is a [value] attribute. *)

  val name : string -> attr
  (** [name "description"] is a [name] attribute. *)

  val placeholder : string -> attr
  (** [placeholder "..."] is a [placeholder] attribute. *)

  val required : attr
  (** [required] is the [required] attribute. *)

  val disabled : attr
  (** [disabled] is the [disabled] attribute. *)

  val checked : attr
  (** [checked] is the [checked] attribute. *)

  val readonly : attr
  (** [readonly] is the [readonly] attribute. *)

  (** {1 Link attributes} *)

  val href : string -> attr
  (** [href "/path"] is an [href] attribute. *)

  val target : string -> attr
  (** [target "_blank"] is a [target] attribute. *)

  val rel : string -> attr
  (** [rel "stylesheet"] is a [rel] attribute. *)

  val download : string -> attr
  (** [download "file.pdf"] is a [download] attribute. *)

  (** {1 Media attributes} *)

  val src : string -> attr
  (** [src "/image.png"] is a [src] attribute. *)

  val alt : string -> attr
  (** [alt "description"] is an [alt] attribute for images. *)

  val width : int -> attr
  (** [width 800] is a [width] attribute. *)

  val height : int -> attr
  (** [height 600] is a [height] attribute. *)

  val loading : string -> attr
  (** [loading "lazy"] is a [loading] attribute. *)

  (** {1 Meta attributes} *)

  val charset : string -> attr
  (** [charset "utf-8"] is a [charset] attribute. *)

  val content : string -> attr
  (** [content "..."] is a [content] attribute. *)

  val property : string -> attr
  (** [property "og:title"] is a [property] attribute. *)

  (** {1 Style attribute} *)

  val style : string -> attr
  (** [style "color:red;"] is a [style] attribute. *)

  (** {1 Time attributes} *)

  val datetime : string -> attr
  (** [datetime "2025-07-28"] is a [datetime] attribute. *)

  (** {1 Image attributes} *)

  val srcset : string -> attr
  (** [srcset "..."] is a [srcset] attribute for responsive images. *)

  val sizes : string -> attr
  (** [sizes "..."] is a [sizes] attribute for responsive images. *)

  (** {1 Additional attributes} *)

  val title' : string -> attr
  (** [title' s] is [title s]. *)

  val loading_lazy : attr
  (** [loading_lazy] is [loading "lazy"]. *)

  val v : string -> string -> attr
  (** [v key value] is the attribute [key] with value [value]. *)

  val void : attr
  (** [void] is an attribute that does not render. *)

  val true' : string -> attr
  (** [true' key] is the boolean attribute [key], with an empty value. *)

  val false' : string -> attr
  (** [false' key] is [v key "false"]. *)

  val if' : bool -> attr -> attr
  (** [if' cond at] is [at] if [cond] is [true] and {!void} otherwise. *)

  val if_some : attr option -> attr
  (** [if_some o] is [at] if [o] is [Some at] and {!void} if [o] is [None]. *)

  val is_void : attr -> bool
  (** [is_void at] is [true] iff [at] is {!void}. *)

  val to_pair : attr -> string * string
  (** [to_pair at] is the [(name, value)] pair of [at]. *)

  val of_pair : string * string -> attr
  (** [of_pair (n, value)] is [v n value]. *)

  (** {2 Additional HTML5 attributes} *)

  val accesskey : string -> attr
  (** [accesskey key] is an [accesskey] attribute, the element's keyboard
      shortcut. *)

  val action : string -> attr
  (** [action url] is a form's [action] attribute. *)

  val autocomplete : string -> attr
  (** [autocomplete value] is an [autocomplete] attribute. *)

  val autofocus : attr
  (** [autofocus] is the [autofocus] attribute: the element takes the focus on
      page load. *)

  val cols : int -> attr
  (** [cols n] is a textarea's [cols] attribute. *)

  val colspan : int -> attr
  (** [colspan n] is a table cell's [colspan] attribute. *)

  val defer : attr
  (** [defer] is a script's [defer] attribute. *)

  val draggable : bool -> attr
  (** [draggable b] is a [draggable] attribute, ["true"] or ["false"]. *)

  val for' : string -> attr
  (** [for' id] is a label's [for] attribute, naming the control [id]. *)

  val hidden : attr
  (** [hidden] is the [hidden] attribute. *)

  val list : string -> attr
  (** [list id] is an input's [list] attribute, naming the datalist [id]. *)

  val method' : string -> attr
  (** [method' m] is a form's [method] attribute. *)

  val media : string -> attr
  (** [media query] is a [media] attribute. *)

  val popover : string -> attr
  (** [popover value] is a [popover] attribute. *)

  val popovertarget : string -> attr
  (** [popovertarget id] is a [popovertarget] attribute naming the popover [id].
  *)

  val popovertargetaction : string -> attr
  (** [popovertargetaction action] is a [popovertargetaction] attribute. *)

  val rows : int -> attr
  (** [rows n] is a textarea's [rows] attribute. *)

  val rowspan : int -> attr
  (** [rowspan n] is a table cell's [rowspan] attribute. *)

  val selected : attr
  (** [selected] is an option's [selected] attribute. *)

  val wrap : string -> attr
  (** [wrap mode] is a textarea's [wrap] attribute. *)

  (** {2 SVG Attributes} *)

  val fill_rule : [ `evenodd ] -> attr
  (** [fill_rule `evenodd] is [fill-rule="evenodd"]. *)

  val clip_rule : [ `evenodd ] -> attr
  (** [clip_rule `evenodd] is [clip-rule="evenodd"]. *)

  val cx : int -> attr
  (** [cx n] is a [cx] attribute, the x coordinate of the centre. *)

  val cy : int -> attr
  (** [cy n] is a [cy] attribute, the y coordinate of the centre. *)

  val r : int -> attr
  (** [r n] is an [r] attribute, the radius. *)

  val view_box : string -> attr
  (** [view_box s] is a [viewBox] attribute. *)

  val fill : string -> attr
  (** [fill "currentColor"] is a [fill] attribute. *)

  val stroke : string -> attr
  (** [stroke "currentColor"] is a [stroke] attribute. *)

  val stroke_width : string -> attr
  (** [stroke_width "2"] is a [stroke-width] attribute. *)

  val stroke_linecap : string -> attr
  (** [stroke_linecap "round"] is a [stroke-linecap] attribute. *)

  val stroke_linejoin : string -> attr
  (** [stroke_linejoin "round"] is a [stroke-linejoin] attribute. *)

  val x : string -> attr
  (** [x "10"] is an [x] attribute. *)

  val y : string -> attr
  (** [y "10"] is a [y] attribute. *)

  val rx : string -> attr
  (** [rx "5"] is an [rx] attribute, the x radius of a rounded rectangle. *)

  val d : string -> attr
  (** [d s] is a [d] attribute, the path data. *)

  val x1 : string -> attr
  (** [x1 "0"] is an [x1] attribute. *)

  val y1 : string -> attr
  (** [y1 "0"] is a [y1] attribute. *)

  val x2 : string -> attr
  (** [x2 "20"] is an [x2] attribute. *)

  val y2 : string -> attr
  (** [y2 "20"] is a [y2] attribute. *)
end

(** {1 Text} *)

val txt : string -> t
(** [txt s] is a text node holding [s], escaped. *)

val txtf : string list -> t
(** [txtf strs] is [txt (String.concat "" strs)]. *)

val raw : string -> t
(** [raw html] is a node that writes [html] as given, unescaped.

    {b Warning.} [html] reaches the document verbatim: escape untrusted text
    with {!escape} first. *)

val rawf : string list -> t
(** [rawf strs] is [raw (String.concat "" strs)]. *)

val empty : t
(** [empty] is the empty element, equivalent to no content. *)

val escape : string -> string
(** [escape s] is [s] with the ampersand, the angle brackets and both quote
    marks replaced by character references, as {!txt} and attribute values are
    written. It is for markup assembled outside the tree, before it is given to
    {!raw}. *)

(** {1 ARIA} *)

(** ARIA accessibility attributes. *)
module Aria : sig
  val label : string -> attr
  (** [label "description"] is an [aria-label] attribute. *)

  val labelledby : string -> attr
  (** [labelledby "id"] is an [aria-labelledby] attribute. *)

  val describedby : string -> attr
  (** [describedby "id"] is an [aria-describedby] attribute. *)

  val hidden : attr
  (** [hidden] is the [aria-hidden="true"] attribute. *)

  val expanded : bool -> attr
  (** [expanded b] is an [aria-expanded] attribute, ["true"] or ["false"]. *)

  val current : string -> attr
  (** [current "page"] is an [aria-current] attribute. *)

  val role : string -> attr
  (** [role "navigation"] is a [role] attribute. *)
end

(** {1 Conversion functions} *)

val to_string : ?doctype:bool -> t -> string
(** [to_string ~doctype t] is [t] rendered as HTML, preceded by the HTML5
    doctype declaration if [doctype] is [true] (defaults to [false]). *)

val to_tw : t -> tw list
(** [to_tw t] are the utilities of every element in [t]: those given through
    [~tw] and the names of [class] attributes that are utilities. *)

val unknown_classes : t -> string list
(** [unknown_classes t] lists, in document order, every name in the tree's
    [class] attributes that {!Tw.of_classes} did not recognise as a utility.

    Such a name is rendered as given - a framework hook or a JS selector belongs
    in a [class] attribute - so this is how a typo among them is found. *)

val has_forms : t -> bool
(** [has_forms t] is [true] iff [t] contains a form element ([<input>],
    [<select>], [<textarea>]) that the forms plugin base styles. *)

(** {1:attrs Attributes over a tree} *)

val fold_attrs : (string -> attr list -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold_attrs f t acc] folds [f name attrs] over the elements of [t] in
    document order, an element before its children. [name] is the element's tag
    and [attrs] the attributes it renders, its [class] attribute included and
    {!At.void} left out. Markup given to {!raw} is not visited. *)

val map_attrs : (string -> attr list -> attr list) -> t -> t
(** [map_attrs f t] is [t] with the attributes of each element replaced by
    [f name attrs], [name] and [attrs] as {!fold_attrs} gives them. A [class]
    attribute [f] changes is read as one given through [~at] is: {!to_tw} and
    {!unknown_classes} report the names it carries. Markup given to {!raw} is
    left as it is. *)

(** {1 Page generation} *)

(** The type for how a {!page} delivers its generated CSS. *)
type tw_css =
  | Link of string
      (** Write the CSS to this file and reference it with a cache-busted
          [<link>] tag. *)
  | Inline
      (** Embed the CSS directly in the document inside a [<style>] tag. *)

type page
(** The type for complete HTML documents and the CSS generated from the
    utilities they use. It is abstract so that the CSS holds exactly the styles
    the HTML needs. *)

val page :
  ?lang:string ->
  ?meta:(string * string) list ->
  ?title:string ->
  ?charset:string ->
  ?tw_css:tw_css ->
  ?forms:bool ->
  ?prelude:t list ->
  ?theme:Tw.Scheme.t ->
  t list ->
  t list ->
  page
(** [page ?lang ?meta ?title ?charset ?tw_css ?forms ?prelude ?theme head body]
    is a complete HTML page and the CSS its utilities need, with:

    - [lang] defaults to ["en"]
    - [charset] defaults to ["utf-8"]
    - [meta] is a list of [(name, content)] pairs for meta tags
    - [title] is the page title
    - [tw_css] controls how the CSS is delivered (see {!type:tw_css}). Defaults
      to [Link "tw.css"], which references the CSS through a cache-busted
      [<link>] tag in the HTML head. Use {!constructor-Inline} to embed the CSS
      in the document and avoid an extra HTTP request.
    - [forms] explicitly enables the [\@tailwindcss/forms] plugin base layer,
      mirroring Tailwind's [\@plugin] opt-in. When omitted, the forms base is
      auto-detected from forms utility usage (like prose styling).
    - [prelude] (default [[]]) goes first in the head, right after the character
      set and before the [meta] tags, the title and the stylesheet: where a
      content security policy has to stand to cover them, and where a script
      that sets the theme runs before the stylesheet applies.
    - [theme] (default {!Tw.Scheme.default}) is the theme the CSS is generated
      with, as {!Tw.to_css} takes it: its {!Tw.Scheme.dark}, for one, decides
      what [Tw.dark] writes.
    - [head] is additional content for the head section
    - [body] is the body content

    Use {!html} to get the HTML string and {!css} to get the CSS filename and
    stylesheet. *)

val versioned : string -> contents:string -> string
(** [versioned path ~contents] is [path?v=h], where [h] is the first eight hex
    digits of the MD5 digest of [contents]. A [<link>] or [<script>] that names
    a file this way is fetched again whenever the file changes, as a {!page}
    names its stylesheet. *)

val html : page -> string
(** [html page] is the HTML document of [page]. *)

val css : page -> string option * Tw.Css.t
(** [css page] is the CSS filename and stylesheet of [page]. The filename is
    [Some file] when the page links to an external stylesheet ([Link file]) and
    [None] when the CSS is inlined ({!constructor-Inline}). *)

(** {1 Live reloading} *)

(** Live reloading during development. *)
module Livereload : sig
  val enabled : bool
  (** [enabled] is [true] iff the [SITE_LIVERELOAD] environment variable is
      ["true"]. *)

  val endpoint : string
  (** [endpoint] is the livereload WebSocket endpoint, read from
      [SITE_LIVERELOAD_ENDPOINT] (defaults to ["ws://localhost:8080"]). *)

  val script : t
  (** [script] is the livereload [<script>] element if {!val-enabled} is [true],
      and {!Tw_html.empty} otherwise. *)
end

(** {1 HTML Elements} *)

val el : string -> ?at:attr list -> ?tw:tw list -> t list -> t
(** [el name ?at ?tw children] is the element [name], built as the named
    constructors below build theirs, for a tag they do not cover. It always has
    an end tag: a void element such as {!val-br} has its own constructor. *)

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

val style : ?at:attr list -> ?tw:tw list -> string -> t
(** [style ?at ?tw css] is a style element holding [css]. [<style>] is a
    raw-text element, so [css] is written as given, unescaped. *)

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
(** [void] is {!empty}. *)

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

val kbd : ?at:attr list -> ?tw:tw list -> t list -> t
(** [kbd ?at ?tw children] is a keyboard input element. *)

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

val tfoot : ?at:attr list -> ?tw:tw list -> t list -> t
(** [tfoot ?at ?tw children] is a table footer element. *)

val tr : ?at:attr list -> ?tw:tw list -> t list -> t
(** [tr ?at ?tw children] is a table row element. *)

val th : ?at:attr list -> ?tw:tw list -> t list -> t
(** [th ?at ?tw children] is a table header cell element. *)

val td : ?at:attr list -> ?tw:tw list -> t list -> t
(** [td ?at ?tw children] is a table data cell element. *)

val caption : ?at:attr list -> ?tw:tw list -> t list -> t
(** [caption ?at ?tw children] is a table caption element. *)

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
(** [svg ?at ?tw children] is an SVG root element. *)

val g : ?at:attr list -> ?tw:tw list -> t list -> t
(** [g ?at ?tw children] is a group element. *)

val circle : ?at:attr list -> ?tw:tw list -> t list -> t
(** [circle ?at ?tw children] is a circle element. *)

val rect : ?at:attr list -> ?tw:tw list -> t list -> t
(** [rect ?at ?tw children] is a rectangle element. *)

val path : ?at:attr list -> ?tw:tw list -> t list -> t
(** [path ?at ?tw children] is a path element. *)

val line : ?at:attr list -> ?tw:tw list -> t list -> t
(** [line ?at ?tw children] is a line element. *)

val text : ?at:attr list -> ?tw:tw list -> t list -> t
(** [text ?at ?tw children] is a text element. *)

val tspan : ?at:attr list -> ?tw:tw list -> t list -> t
(** [tspan ?at ?tw children] is a tspan element, a run of text inside
    {!val-text}. *)

val desc : ?at:attr list -> ?tw:tw list -> t list -> t
(** [desc ?at ?tw children] is a desc element, the text description of its
    parent. *)

val defs : ?at:attr list -> ?tw:tw list -> t list -> t
(** [defs ?at ?tw children] is a defs element, holding elements drawn only where
    they are referenced. *)

val pattern : ?at:attr list -> ?tw:tw list -> t list -> t
(** [pattern ?at ?tw children] is a pattern element, a tile a fill or a stroke
    can repeat. *)

val pp : t -> string
(** [pp t] is [t] rendered as HTML and, when it has utilities, wrapped in an
    [<element with classes="...">] marker that lists them. *)
