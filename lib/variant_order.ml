module Css = Cascade.Css

(* The order @tailwindcss/typography registers its element variants in, which is
   the order it emits their rules in. A variant missing from here sorts before
   every other, so its utility loses to one it should win against. *)
let prose_element_rank = function
  | "headings" -> 96001
  | "h1" -> 96002
  | "h2" -> 96003
  | "h3" -> 96004
  | "h4" -> 96005
  | "h5" -> 96006
  | "h6" -> 96007
  | "p" -> 96008
  | "a" -> 96009
  | "blockquote" -> 96010
  | "figure" -> 96011
  | "figcaption" -> 96012
  | "strong" -> 96013
  | "em" -> 96014
  | "kbd" -> 96015
  | "code" -> 96016
  | "pre" -> 96017
  | "ol" -> 96018
  | "ul" -> 96019
  | "li" -> 96020
  | "dl" -> 96021
  | "dt" -> 96022
  | "dd" -> 96023
  | "table" -> 96024
  | "thead" -> 96025
  | "tr" -> 96026
  | "th" -> 96027
  | "td" -> 96028
  | "img" -> 96029
  | "picture" -> 96030
  | "video" -> 96031
  | "hr" -> 96032
  | "lead" -> 96033
  | _ -> 96000

(* The Tailwind v4 cascade order of variants, written once. A [Slot.t] names one
   position in that order and [Slot.rank] is the only place a number appears;
   the readings below - a class-name token, a media condition a rule is nested
   in - each land on a slot rather than on a scale of their own. *)

module Slot = struct
  (* One position in the variant cascade. Named separately from [Style.modifier]
     because several modifiers share a position: every [group-*] spelling sorts
     where [group-] sorts, and a [not-X] sorts where X does. *)
  type t =
    | Child  (** [*:]: every direct child. *)
    | Descendant  (** [**:]: every descendant. *)
    | Negation  (** [not-X:], ordered inside the slot by the X it negates. *)
    | Group
    | Peer
    | Pseudo_first_letter
    | Pseudo_first_line
    | Pseudo_marker
    | Pseudo_selection
    | Pseudo_file
    | Pseudo_placeholder
    | Pseudo_backdrop
    | Pseudo_details_content
    | Pseudo_before
    | Pseudo_after
    | First
    | Last
    | Only
    | Odd
    | Even
    | First_of_type
    | Last_of_type
    | Only_of_type
    | Visited
    | Target
    | Open
    | Default
    | Checked
    | Indeterminate
    | Placeholder_shown
    | Autofill
    | Optional
    | Required
    | Valid
    | Invalid
    | User_valid
    | User_invalid
    | In_range
    | Out_of_range
    | Read_only
    | Read_write
    | Empty
    | Focus_within
    | Hover
    | Focus
    | Focus_visible
    | Active
    | Enabled
    | Disabled
    | Inert
    | Ancestor  (** [in-*]: a state on an ancestor. *)
    | Has
    | Aria_named
    | Aria_arbitrary
    | Data_named
    | Data_arbitrary
    | Nth
    | Nth_last
    | Nth_of_type
    | Nth_last_of_type
    | Hocus
    | Supports
    | Motion_safe
    | Motion_reduce
    | Contrast_more
    | Contrast_less
    | Pointer
    | Any_pointer
    | Breakpoint  (** Every [sm:]/[max-lg:]/[min-[32rem]:] spelling. *)
    | Portrait
    | Landscape
    | Ltr
    | Rtl
    | Dark
    | Print
    | Forced_colors
    | Noscript
    | Inverted_colors
    | Starting
    | Custom  (** A [\@custom-variant] the theme registers. *)
    | Prose of string  (** [prose-X:], ordered by the element X names. *)
    | Arbitrary  (** [[&>*]:], [not-[...]:] and the other bracket variants. *)
    | Container_query

  (* The cascade order itself. Every other function in this section maps onto a
     slot; this is the only place a position is a number. The gaps leave room to
     insert a slot without renumbering its neighbours. *)
  let rank = function
    | Child -> 100
    | Descendant -> 200
    | Negation -> 300
    | Group -> 500
    | Peer -> 600
    | Pseudo_first_letter -> 1000
    | Pseudo_first_line -> 1000
    | Pseudo_marker -> 1100
    | Pseudo_selection -> 1200
    | Pseudo_file -> 1300
    | Pseudo_placeholder -> 1400
    | Pseudo_backdrop -> 1401
    | Pseudo_details_content -> 1500
    | Pseudo_before -> 1600
    | Pseudo_after -> 1601
    | First -> 10100
    | Last -> 10200
    | Only -> 10300
    | Odd -> 10400
    | Even -> 10500
    | First_of_type -> 10600
    | Last_of_type -> 10700
    | Only_of_type -> 10800
    | Visited -> 10900
    | Target -> 11000
    | Open -> 11100
    | Default -> 11200
    | Checked -> 11300
    | Indeterminate -> 11400
    | Placeholder_shown -> 11500
    | Autofill -> 11600
    | Optional -> 11700
    | Required -> 11800
    | Valid -> 11900
    | Invalid -> 12000
    | User_valid -> 12010
    | User_invalid -> 12020
    | In_range -> 12100
    | Out_of_range -> 12200
    | Read_only -> 12300
    | Read_write -> 12310
    | Empty -> 12400
    | Focus_within -> 12500
    | Hover -> 20000
    | Focus -> 30100
    | Focus_visible -> 30200
    | Active -> 30300
    | Enabled -> 30400
    | Disabled -> 30500
    | Inert -> 30550
    | Ancestor -> 30560
    | Has -> 30600
    | Aria_named -> 30700
    | Aria_arbitrary -> 30790
    | Data_named -> 30800
    | Data_arbitrary -> 30810
    | Nth -> 30900
    | Nth_last -> 30910
    | Nth_of_type -> 30920
    | Nth_last_of_type -> 30930
    | Hocus -> 35000
    | Supports -> 40000
    | Motion_safe -> 50000
    | Motion_reduce -> 50100
    | Contrast_more -> 50200
    | Contrast_less -> 50300
    | Breakpoint -> 60000
    | Container_query -> 65000
    | Portrait -> 70000
    | Landscape -> 70100
    | Ltr -> 80000
    | Rtl -> 80100
    | Dark -> 90000
    | Starting -> 90500
    | Print -> 91000
    | Forced_colors -> 92000
    | Inverted_colors -> 93100
    | Pointer -> 93200
    | Any_pointer -> 93300
    | Noscript -> 93400
    | Custom -> 95500
    | Prose element -> prose_element_rank element
    | Arbitrary -> 100000
end

(* The slot a class-name token sorts in, or [None] when the token names no
   variant this table knows. The token is one modifier of a class name, the part
   between two ":" (["hover"], ["group-has-checked"], ["@min-[64rem]"]). *)
let slot_of_prefix prefix : Slot.t option =
  let starts_with prefix' = String.starts_with ~prefix:prefix' prefix in
  match prefix with
  | "*" -> Some Slot.Child
  | "**" -> Some Slot.Descendant
  | "first-letter" -> Some Slot.Pseudo_first_letter
  | "first-line" -> Some Slot.Pseudo_first_line
  | "marker" -> Some Slot.Pseudo_marker
  | "selection" -> Some Slot.Pseudo_selection
  | "file" -> Some Slot.Pseudo_file
  | "placeholder" -> Some Slot.Pseudo_placeholder
  | "backdrop" -> Some Slot.Pseudo_backdrop
  | "details-content" -> Some Slot.Pseudo_details_content
  | "before" -> Some Slot.Pseudo_before
  | "after" -> Some Slot.Pseudo_after
  | "first" -> Some Slot.First
  | "last" -> Some Slot.Last
  | "only" -> Some Slot.Only
  | "odd" -> Some Slot.Odd
  | "even" -> Some Slot.Even
  | "first-of-type" -> Some Slot.First_of_type
  | "last-of-type" -> Some Slot.Last_of_type
  | "only-of-type" -> Some Slot.Only_of_type
  | "visited" -> Some Slot.Visited
  | "target" -> Some Slot.Target
  | "open" -> Some Slot.Open
  | "default" -> Some Slot.Default
  | "checked" -> Some Slot.Checked
  | "indeterminate" -> Some Slot.Indeterminate
  | "placeholder-shown" -> Some Slot.Placeholder_shown
  | "autofill" -> Some Slot.Autofill
  | "optional" -> Some Slot.Optional
  | "required" -> Some Slot.Required
  | "valid" -> Some Slot.Valid
  | "invalid" -> Some Slot.Invalid
  | "user-valid" -> Some Slot.User_valid
  | "user-invalid" -> Some Slot.User_invalid
  | "in-range" -> Some Slot.In_range
  | "out-of-range" -> Some Slot.Out_of_range
  | "read-only" -> Some Slot.Read_only
  | "read-write" -> Some Slot.Read_write
  | "empty" -> Some Slot.Empty
  | "focus-within" -> Some Slot.Focus_within
  | "hover" -> Some Slot.Hover
  | "focus" -> Some Slot.Focus
  | "focus-visible" -> Some Slot.Focus_visible
  | "active" -> Some Slot.Active
  | "enabled" -> Some Slot.Enabled
  | "disabled" -> Some Slot.Disabled
  | "inert" -> Some Slot.Inert
  | "data-custom" | "data-active" | "data-inactive" -> Some Slot.Data_named
  | "hocus" | "device-hocus" -> Some Slot.Hocus
  | "motion-safe" -> Some Slot.Motion_safe
  | "motion-reduce" -> Some Slot.Motion_reduce
  | "contrast-more" -> Some Slot.Contrast_more
  | "contrast-less" -> Some Slot.Contrast_less
  | "portrait" -> Some Slot.Portrait
  | "landscape" -> Some Slot.Landscape
  | "ltr" -> Some Slot.Ltr
  | "rtl" -> Some Slot.Rtl
  | "dark" -> Some Slot.Dark
  | "print" -> Some Slot.Print
  | "forced-colors" -> Some Slot.Forced_colors
  | "noscript" -> Some Slot.Noscript
  | "inverted-colors" -> Some Slot.Inverted_colors
  | "starting" -> Some Slot.Starting
  | "sm" | "md" | "lg" | "xl" | "2xl" -> Some Slot.Breakpoint
  | _ when starts_with "group-" -> Some Slot.Group
  | _ when starts_with "peer-" -> Some Slot.Peer
  | _ when starts_with "has-" -> Some Slot.Has
  | _ when starts_with "aria-" ->
      (* [aria-] with nothing after it, and [aria-[expr]], take the arbitrary
         position; the shorthand names take the named one. *)
      if String.length prefix > 5 && prefix.[5] <> '[' then Some Slot.Aria_named
      else Some Slot.Aria_arbitrary
  | _ when starts_with "data-" ->
      if String.length prefix > 5 && prefix.[5] = '[' then
        Some Slot.Data_arbitrary
      else Some Slot.Data_named
  | _ when starts_with "supports" -> Some Slot.Supports
  | _ when starts_with "pointer-" -> Some Slot.Pointer
  | _ when starts_with "any-pointer-" -> Some Slot.Any_pointer
  | _ when starts_with "min-" || starts_with "max-" -> Some Slot.Breakpoint
  (* [nth-3], [nth-last-of-type-2]: the count is the argument, not part of the
     variant name. Longest spelling first, so [nth-last-of-type-] is not read as
     an [nth-last-] with a strange argument. *)
  | _ when starts_with "nth-last-of-type-" -> Some Slot.Nth_last_of_type
  | _ when starts_with "nth-of-type-" -> Some Slot.Nth_of_type
  | _ when starts_with "nth-last-" -> Some Slot.Nth_last
  | _ when starts_with "nth-" -> Some Slot.Nth
  (* [in-focus], [in-data-open], [in-[.foo]]: a state on an ancestor. The
     pseudo-classes that merely start with the same letters ([in-range],
     [inert], [invalid]) are matched by name above. *)
  | _ when starts_with "in-" -> Some Slot.Ancestor
  (* Every [not-X] sorts together, ahead of the variants it can negate;
     [inner_order] puts the X back inside the slot. *)
  | _ when starts_with "not-" -> Some Slot.Negation
  | _ when starts_with "prose-" ->
      Some (Slot.Prose (String.sub prefix 6 (String.length prefix - 6)))
  | _ when starts_with "[" -> Some Slot.Arbitrary
  | _ when starts_with "@" -> Some Slot.Container_query
  | _ -> None

(* The slot a media condition sorts in. A rule can take its position from the
   query it is nested in rather than from its class name: [dark:group-hover]
   writes a [(hover: hover)] query, and sorts where a bare [hover] sorts. *)
let slot_of_media_cond (cond : Css.Media.t) : Slot.t option =
  let open Css.Media in
  match cond with
  | Cond (Feature (Plain (Hover, Ident Hover))) -> Some Slot.Hover
  | Cond (Feature (Plain (Prefers_reduced_motion, Ident No_preference))) ->
      Some Slot.Motion_safe
  | Cond (Feature (Plain (Prefers_reduced_motion, Ident Reduce))) ->
      Some Slot.Motion_reduce
  | Cond (Feature (Plain (Prefers_contrast, Ident More))) ->
      Some Slot.Contrast_more
  | Cond (Feature (Plain (Prefers_contrast, Ident Less))) ->
      Some Slot.Contrast_less
  | Cond (Feature (Plain (Orientation, Ident Portrait))) -> Some Slot.Portrait
  | Cond (Feature (Plain (Orientation, Ident Landscape))) -> Some Slot.Landscape
  | Cond (Feature (Plain (Prefers_color_scheme, (Ident Dark | Ident Light)))) ->
      Some Slot.Dark
  | Type { prefix = None; type_ = Print; trailing = None } -> Some Slot.Print
  | Cond (Feature (Plain (Forced_colors, Ident Active))) ->
      Some Slot.Forced_colors
  | Cond (Feature (Plain (Inverted_colors, Ident Inverted))) ->
      Some Slot.Inverted_colors
  | _ -> None

(* The variant wrapped by one compound token, if any. Keeping this extraction in
   one place lets ordering follow group-not-has-peer-not-data-active all the way
   to its data predicate instead of stopping at the first [not] or [has]
   slot. *)
let inner_token token =
  let after n = String.sub token n (String.length token - n) in
  let anchor_inner n =
    let inner = after n in
    (* A slash inside an arbitrary selector belongs to the selector. Named
       simple states have no brackets, so their suffix can be removed here. *)
    if String.contains inner '[' then inner
    else fst (Variant_parse.split_name inner)
  in
  if String.starts_with ~prefix:"group-" token then Some (anchor_inner 6)
  else if String.starts_with ~prefix:"peer-" token then Some (anchor_inner 5)
  else if String.starts_with ~prefix:"not-" token then Some (after 4)
  else if String.starts_with ~prefix:"has-" token then Some (after 4)
  else
    (* [in-focus] names a state on an ancestor; [in-range] names one on the
       element itself, and the table matches that by name. *)
    match slot_of_prefix token with
    | Some Slot.Ancestor -> Some (after 3)
    | Some _ | None -> None

let of_prefix ?theme prefix =
  match theme with
  | Some theme when Variant_parse.is_custom_variant theme prefix ->
      (* Tailwind registers [dark] before it reads [@custom-variant]. Replacing
         that exact registration changes its body but retains its slot. *)
      if String.equal prefix "dark" then Slot.rank Slot.Dark
      else Slot.rank Slot.Custom
  (* A breakpoint the project declared, [xs:] under [--breakpoint-xs], sorts
     with the built-in ones; the table above knows the built-in names alone. *)
  | Some theme when Scheme.has_breakpoint theme prefix ->
      Slot.rank Slot.Breakpoint
  | Some _ | None -> (
      match slot_of_prefix prefix with Some slot -> Slot.rank slot | None -> 0)

let rec inner_order_path ?theme token =
  match inner_token token with
  | None -> []
  | Some inner ->
      let order = of_prefix ?theme inner in
      if order = 0 then [] else order :: inner_order_path ?theme inner

(* The first wrapped slot retained for callers that only need the immediate
   compound variant. *)
let inner_order token =
  match inner_order_path token with order :: _ -> order | [] -> 0

let of_media_cond cond =
  match slot_of_media_cond cond with Some slot -> Slot.rank slot | None -> 0
