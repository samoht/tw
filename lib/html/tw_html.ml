(* HTML component module implementation with integrated Tailwind CSS *)

(* Helper function to concatenate strings *)
let str = String.concat ""

(* Minimal Htmlit implementation - adapted from Htmlit library Original source:
   https://github.com/dbuenzli/htmlit Copyright (c) 2016 The htmlit programmers
   License: ISC

   This is a minimal subset of Htmlit functionality needed for tw. The
   implementation has been simplified to avoid Format dependency. *)
module El = struct
  type html =
    | Element of string * (string * string) list * html list
    | Text of string
    | Void
    | Raw of string

  let v ~at name children = Element (name, at, children)
  let txt s = Text s
  let void = Void
  let unsafe_raw s = Raw s

  let rec to_string ?(doctype = false) = function
    | Text s ->
        (* HTML escape *)
        let escape_html s =
          let b = Buffer.create (String.length s) in
          String.iter
            (function
              | '<' -> Buffer.add_string b "&lt;"
              | '>' -> Buffer.add_string b "&gt;"
              | '&' -> Buffer.add_string b "&amp;"
              | '"' -> Buffer.add_string b "&quot;"
              | '\'' -> Buffer.add_string b "&#x27;"
              | c -> Buffer.add_char b c)
            s;
          Buffer.contents b
        in
        escape_html s
    | Raw s -> s
    | Void -> ""
    | Element (name, attrs, children) ->
        let b = Buffer.create 256 in
        if doctype && name = "html" then Buffer.add_string b "<!DOCTYPE html>\n";
        Buffer.add_char b '<';
        Buffer.add_string b name;
        List.iter
          (fun (k, v) ->
            Buffer.add_char b ' ';
            Buffer.add_string b k;
            Buffer.add_string b "=\"";
            Buffer.add_string b v;
            Buffer.add_char b '"')
          attrs;
        if
          children = []
          && List.mem name [ "img"; "br"; "hr"; "input"; "meta"; "link" ]
        then Buffer.add_string b " />"
        else (
          Buffer.add_char b '>';
          List.iter
            (fun child -> Buffer.add_string b (to_string child))
            children;
          Buffer.add_string b "</";
          Buffer.add_string b name;
          Buffer.add_char b '>');
        Buffer.contents b
end

module At = struct
  type t = string * string

  let v name value = (name, value)
  let class' s = ("class", s)
  let id s = ("id", s)
  let title s = ("title", s)
  let lang s = ("lang", s)
  let dir s = ("dir", s)
  let tabindex i = ("tabindex", string_of_int i)
  let contenteditable b = ("contenteditable", if b then "true" else "false")
  let spellcheck s = ("spellcheck", s)
  let type' s = ("type", s)
  let value s = ("value", s)
  let name s = ("name", s)
  let placeholder s = ("placeholder", s)
  let required = ("required", "")
  let disabled = ("disabled", "")
  let checked = ("checked", "")
  let href s = ("href", s)
  let rel s = ("rel", s)
  let src s = ("src", s)
  let alt s = ("alt", s)
  let width i = ("width", string_of_int i)
  let height i = ("height", string_of_int i)
  let charset s = ("charset", s)
  let content s = ("content", s)
  let style s = ("style", s)

  (* Additional attributes from the second At module *)
  let onclick s = v "onclick" s
  let onchange s = v "onchange" s
  let oninput s = v "oninput" s
  let onsubmit s = v "onsubmit" s
  let readonly = v "readonly" ""
  let target s = v "target" s
  let download s = v "download" s
  let loading s = v "loading" s
  let property s = v "property" s
  let datetime s = v "datetime" s
  let srcset s = v "srcset" s
  let sizes s = v "sizes" s
  let title' s = v "title" s
  let loading_lazy = v "loading" "lazy"
  let true' name = v name ""
  let false' name = v name "false"

  (* Void attribute for conditionals *)
  let void = ("", "")
  let if' cond at = if cond then at else void
  let if_some = function Some at -> at | None -> void
  let is_void (name, _) = name = ""
  let to_pair at = at
  let of_pair p = p

  (* Additional HTML5 attributes *)
  let accesskey s = v "accesskey" s
  let action s = v "action" s
  let autocomplete s = v "autocomplete" s
  let autofocus = v "autofocus" ""
  let cols i = v "cols" (string_of_int i)
  let colspan i = v "colspan" (string_of_int i)
  let defer = v "defer" ""
  let draggable b = v "draggable" (if b then "true" else "false")
  let for' s = v "for" s
  let hidden = v "hidden" ""
  let list s = v "list" s
  let method' s = v "method" s
  let media s = v "media" s
  let popover s = v "popover" s
  let popovertarget s = v "popovertarget" s
  let popovertargetaction s = v "popovertargetaction" s
  let rows i = v "rows" (string_of_int i)
  let rowspan i = v "rowspan" (string_of_int i)
  let selected = v "selected" ""
  let wrap s = v "wrap" s

  (* SVG attributes *)
  let fill_rule `evenodd = v "fill-rule" "evenodd"
  let clip_rule `evenodd = v "clip-rule" "evenodd"
  let cx i = v "cx" (string_of_int i)
  let cy i = v "cy" (string_of_int i)
  let r i = v "r" (string_of_int i)
  let view_box s = v "viewBox" s
  let fill s = v "fill" s
  let stroke s = v "stroke" s
  let stroke_width s = v "stroke-width" s
  let stroke_linecap s = v "stroke-linecap" s
  let stroke_linejoin s = v "stroke-linejoin" s
  let x s = v "x" s
  let y s = v "y" s
  let rx s = v "rx" s
  let d s = v "d" s
  let x1 s = v "x1" s
  let y1 s = v "y1" s
  let x2 s = v "x2" s
  let y2 s = v "y2" s
end

type tw = Tw.t

(* Type that combines HTML element with its Tailwind classes *)
type t = { el : El.html; tw : tw list }

(* Attribute type - abstract to prevent direct usage of class' *)
type attr = At.t

(* Aria module *)
module Aria = struct
  let label s = At.v "aria-label" s
  let labelledby s = At.v "aria-labelledby" s
  let describedby s = At.v "aria-describedby" s
  let hidden = At.v "aria-hidden" "true"
  let expanded b = At.v "aria-expanded" (string_of_bool b)
  let current s = At.v "aria-current" s
  let role s = At.v "role" s
end

(* Internal helper to convert to El.html *)
let to_htmlit t = t.el
let to_tw t = t.tw

(* Text helpers *)
let txt s = { el = El.txt s; tw = [] }
let txtf segments = txt (str segments)
let raw s = { el = El.unsafe_raw s; tw = [] }
let rawf segments = raw (str segments)

(* Empty element *)
let empty = { el = El.void; tw = [] }

(* Helper to create elements - applies tw classes immediately *)
let el_with_tw name ?at ?(tw = []) children =
  let atts = Option.value ~default:[] at in
  (* Add tw classes to attributes *)
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  (* Convert children to Htmlit elements *)
  let child_els = List.map to_htmlit children in
  (* Collect all tw styles from this element and its children *)
  let all_tw = tw @ List.concat_map to_tw children in
  { el = El.v ~at:atts_with_tw name child_els; tw = all_tw }

(* Convert to string *)
let to_string ?(doctype = false) t = El.to_string ~doctype (to_htmlit t)

(* Livereload module *)
module Livereload = struct
  let enabled =
    try Sys.getenv "SITE_LIVERELOAD" = "true" with Not_found -> false

  let endpoint =
    try Sys.getenv "SITE_LIVERELOAD_ENDPOINT"
    with Not_found -> "ws://localhost:8080"

  let script =
    if enabled then
      raw
        (str
           [
             "<script>\n";
             "(function() {\n";
             "  const ws = new WebSocket('";
             endpoint;
             "');\n";
             "  ws.onmessage = (event) => {\n";
             "    if (event.data === 'reload') {\n";
             "      location.reload();\n";
             "    }\n";
             "  };\n";
             "})();\n";
             "</script>";
           ])
    else empty
end

(* HTML Elements with optional Tailwind classes *)
let div ?at ?tw children = el_with_tw "div" ?at ?tw children
let span ?at ?tw children = el_with_tw "span" ?at ?tw children
let p ?at ?tw children = el_with_tw "p" ?at ?tw children
let a ?at ?tw children = el_with_tw "a" ?at ?tw children
let ul ?at ?tw children = el_with_tw "ul" ?at ?tw children
let li ?at ?tw children = el_with_tw "li" ?at ?tw children
let nav ?at ?tw children = el_with_tw "nav" ?at ?tw children
let section ?at ?tw children = el_with_tw "section" ?at ?tw children
let article ?at ?tw children = el_with_tw "article" ?at ?tw children
let header ?at ?tw children = el_with_tw "header" ?at ?tw children
let footer ?at ?tw children = el_with_tw "footer" ?at ?tw children
let h1 ?at ?tw children = el_with_tw "h1" ?at ?tw children
let h2 ?at ?tw children = el_with_tw "h2" ?at ?tw children
let h3 ?at ?tw children = el_with_tw "h3" ?at ?tw children
let h4 ?at ?tw children = el_with_tw "h4" ?at ?tw children
let h5 ?at ?tw children = el_with_tw "h5" ?at ?tw children
let h6 ?at ?tw children = el_with_tw "h6" ?at ?tw children
let script ?at ?tw children = el_with_tw "script" ?at ?tw children
let title ?at ?tw children = el_with_tw "title" ?at ?tw children
let head ?at ?tw children = el_with_tw "head" ?at ?tw children
let body ?at ?tw children = el_with_tw "body" ?at ?tw children
let root ?at ?tw children = el_with_tw "html" ?at ?tw children
let option ?at ?tw children = el_with_tw "option" ?at ?tw children
let select ?at ?tw children = el_with_tw "select" ?at ?tw children
let main ?at ?tw children = el_with_tw "main" ?at ?tw children
let aside ?at ?tw children = el_with_tw "aside" ?at ?tw children
let time ?at ?tw children = el_with_tw "time" ?at ?tw children
let dialog ?at ?tw children = el_with_tw "dialog" ?at ?tw children
let data ?at ?tw children = el_with_tw "data" ?at ?tw children
let picture ?at ?tw children = el_with_tw "picture" ?at ?tw children
let slot ?at ?tw children = el_with_tw "slot" ?at ?tw children
let template ?at ?tw children = el_with_tw "template" ?at ?tw children

(* Void elements *)
let img ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "img" []; tw }

let meta ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "meta" []; tw }

let link ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "link" []; tw }

(* Void is now an alias for empty *)
let void = empty

(* Forms *)
let form ?at ?tw children = el_with_tw "form" ?at ?tw children

let input ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "input" []; tw }

let textarea ?at ?tw children = el_with_tw "textarea" ?at ?tw children
let button ?at ?tw children = el_with_tw "button" ?at ?tw children
let label ?at ?tw children = el_with_tw "label" ?at ?tw children
let fieldset ?at ?tw children = el_with_tw "fieldset" ?at ?tw children
let legend ?at ?tw children = el_with_tw "legend" ?at ?tw children

(* Interactive elements *)
let details ?at ?tw children = el_with_tw "details" ?at ?tw children
let summary ?at ?tw children = el_with_tw "summary" ?at ?tw children

(* Text content *)
let pre ?at ?tw children = el_with_tw "pre" ?at ?tw children
let code ?at ?tw children = el_with_tw "code" ?at ?tw children
let em ?at ?tw children = el_with_tw "em" ?at ?tw children
let strong ?at ?tw children = el_with_tw "strong" ?at ?tw children
let small ?at ?tw children = el_with_tw "small" ?at ?tw children
let mark ?at ?tw children = el_with_tw "mark" ?at ?tw children

(* Breaks *)
let br ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "br" []; tw }

let hr ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "hr" []; tw }

(* Tables *)
let table ?at ?tw children = el_with_tw "table" ?at ?tw children
let thead ?at ?tw children = el_with_tw "thead" ?at ?tw children
let tbody ?at ?tw children = el_with_tw "tbody" ?at ?tw children
let tr ?at ?tw children = el_with_tw "tr" ?at ?tw children
let th ?at ?tw children = el_with_tw "th" ?at ?tw children
let td ?at ?tw children = el_with_tw "td" ?at ?tw children

(* Lists *)
let ol ?at ?tw children = el_with_tw "ol" ?at ?tw children
let dl ?at ?tw children = el_with_tw "dl" ?at ?tw children
let dt ?at ?tw children = el_with_tw "dt" ?at ?tw children
let dd ?at ?tw children = el_with_tw "dd" ?at ?tw children

(* Quotations *)
let blockquote ?at ?tw children = el_with_tw "blockquote" ?at ?tw children

(* Figures *)
let figure ?at ?tw children = el_with_tw "figure" ?at ?tw children
let figcaption ?at ?tw children = el_with_tw "figcaption" ?at ?tw children

(* Media *)
let video ?at ?tw children = el_with_tw "video" ?at ?tw children
let audio ?at ?tw children = el_with_tw "audio" ?at ?tw children

let source ?at ?(tw = []) () =
  let atts = Option.value ~default:[] at in
  let atts_with_tw =
    match tw with
    | [] -> atts
    | tw_styles -> At.class' (Tw.to_classes tw_styles) :: atts
  in
  { el = El.v ~at:atts_with_tw "source" []; tw }

(* Embedded content *)
let canvas ?at ?tw children = el_with_tw "canvas" ?at ?tw children
let iframe ?at ?tw children = el_with_tw "iframe" ?at ?tw children

(* SVG elements *)
let svg ?at ?tw children = el_with_tw "svg" ?at ?tw children
let g ?at ?tw children = el_with_tw "g" ?at ?tw children
let circle ?at ?tw children = el_with_tw "circle" ?at ?tw children
let rect ?at ?tw children = el_with_tw "rect" ?at ?tw children
let path ?at ?tw children = el_with_tw "path" ?at ?tw children
let line ?at ?tw children = el_with_tw "line" ?at ?tw children

(* Type for page generation result *)
type page = { html : string; css : Tw.Css.t; tw_css : string }

let page_impl ~lang ~meta_list ?title_text ~charset ~tw_css head_content
    body_content =
  (* Build HTML tree with placeholder for CSS link *)
  let meta_charset = meta ~at:[ At.charset charset ] () in
  let meta_tags =
    meta_charset
    :: List.map
         (fun (name, content) ->
           meta ~at:[ At.name name; At.content content ] ())
         meta_list
  in

  (* Create the body and collect all Tw styles *)
  let body_element = body body_content in
  let all_tw = to_tw body_element in

  (* Add styles from head content *)
  let all_tw = all_tw @ List.concat_map to_tw head_content in

  (* Generate CSS and compute MD5 hash for cache busting *)
  let css_stylesheet = Tw.to_css all_tw in
  let css_string = Tw.Css.to_string ~minify:true css_stylesheet in

  (* Compute MD5 hash of the CSS content for cache busting *)
  let css_hash =
    let digest = Digest.string css_string in
    (* Convert first 8 bytes of MD5 to hex string *)
    let hex = Digest.to_hex digest in
    String.sub hex 0 8
  in

  (* Add cache busting query parameter to CSS URL *)
  let css_url_with_hash = String.concat "" [ tw_css; "?v="; css_hash ] in

  (* Build final HTML with cache-busted CSS link *)
  let css_link =
    link ~at:[ At.rel "stylesheet"; At.href css_url_with_hash ] ()
  in
  let head_children =
    meta_tags
    @ (match title_text with Some t -> [ title [ txt t ] ] | None -> [])
    @ [ css_link ] @ head_content
  in
  let html_tree =
    root ~at:[ At.lang lang ] [ head head_children; body_element ]
  in
  let html_string = to_string ~doctype:true html_tree in
  { html = html_string; css = css_stylesheet; tw_css }

(* Page generation with CSS - use renamed parameters to avoid shadowing *)
let page ?(lang = "en") ?(meta = []) ?title ?(charset = "utf-8")
    ?(tw_css = "tw.css") head_content body_content =
  page_impl ~lang ~meta_list:meta ?title_text:title ~charset ~tw_css
    head_content body_content

(* Page accessor functions *)
let html page = page.html
let css page = (page.tw_css, page.css)

(* Pretty printing *)
let pp t =
  let tw_classes = Tw.to_classes t.tw in
  let el_str = El.to_string ~doctype:false t.el in
  if tw_classes = "" then el_str
  else
    String.concat ""
      [ "<element with classes=\""; tw_classes; "\">"; el_str; "</element>" ]
