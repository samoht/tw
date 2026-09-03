(** CSS rule extraction from Tailwind utilities.

    Converts modifier structures into CSS rules, handling all modifier types:
    pseudo-classes, media queries, container queries, etc. *)

module Css = Cascade.Css
open Output

(* ======================================================================== *)
(* Basic Utilities *)
(* ======================================================================== *)

let string_of_breakpoint = function
  | `Sm -> "sm"
  | `Md -> "md"
  | `Lg -> "lg"
  | `Xl -> "xl"
  | `Xl_2 -> "2xl"

(* Delegate escaping to the selector printer for correctness and parity with the
   rest of the system. This covers all special characters per CSS rules,
   including ones Tailwind often uses (e.g., !, |, ^, ~, etc.) We strip the
   leading '.' from the rendered class selector. *)
let escape_class_name name =
  let rendered = Css.Selector.to_string (Css.Selector.class_ name) in
  if String.length rendered > 0 && rendered.[0] = '.' then
    String.sub rendered 1 (String.length rendered - 1)
  else rendered

(* ======================================================================== *)
(* Rule Extraction - Convert Core.t to CSS rules *)
(* ======================================================================== *)

(* Selector helpers: centralized operations for transforming selector ASTs when
   applying modifiers. This removes brittle string-based handling and keeps
   selector semantics together. *)
module Rules_selector = struct
  (* Replace every occurrence of a class name in a selector AST with a selector
     of its own, so a variant can put structure where the class sat and keep
     whatever the inner rule built around it. *)
  let rec replace_class_with ~old_class ~replacement = function
    | Css.Selector.Class cls when String.equal cls old_class -> replacement
    | Css.Selector.Compound selectors ->
        Css.Selector.Compound
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Combined (a, comb, b) ->
        Css.Selector.Combined
          ( replace_class_with ~old_class ~replacement a,
            comb,
            replace_class_with ~old_class ~replacement b )
    | Css.Selector.Relative (comb, b) ->
        Css.Selector.Relative
          (comb, replace_class_with ~old_class ~replacement b)
    | Css.Selector.List selectors ->
        Css.Selector.List
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Is selectors ->
        Css.Selector.Is
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Where selectors ->
        Css.Selector.Where
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Not selectors ->
        Css.Selector.Not
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Has selectors ->
        Css.Selector.Has
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Slotted selectors ->
        Css.Selector.Slotted
          (List.map (replace_class_with ~old_class ~replacement) selectors)
    | Css.Selector.Cue selectors ->
        Css.Selector.Cue
          (Option.map
             (List.map (replace_class_with ~old_class ~replacement))
             selectors)
    | Css.Selector.Cue_region selectors ->
        Css.Selector.Cue_region
          (Option.map
             (List.map (replace_class_with ~old_class ~replacement))
             selectors)
    | Css.Selector.Nth_child (nth, of_) ->
        Css.Selector.Nth_child
          ( nth,
            Option.map
              (List.map (replace_class_with ~old_class ~replacement))
              of_ )
    | Css.Selector.Nth_last_child (nth, of_) ->
        Css.Selector.Nth_last_child
          ( nth,
            Option.map
              (List.map (replace_class_with ~old_class ~replacement))
              of_ )
    | Css.Selector.Nth_of_type (nth, of_) ->
        Css.Selector.Nth_of_type
          ( nth,
            Option.map
              (List.map (replace_class_with ~old_class ~replacement))
              of_ )
    | Css.Selector.Nth_last_of_type (nth, of_) ->
        Css.Selector.Nth_last_of_type
          ( nth,
            Option.map
              (List.map (replace_class_with ~old_class ~replacement))
              of_ )
    | other -> other

  (* Replace every occurrence of a class name in a selector AST. *)
  let replace_class_in_selector ~old_class ~new_class =
    replace_class_with ~old_class ~replacement:(Css.Selector.Class new_class)

  (* The class a modifier's selector carries — the base class with the variant
     prefixed. It does not always sit at the top: the child variant nests it
     inside an [:is] with a child combinator, and a group variant puts the
     anchor class beside it. So look for the class derived from [base_class]
     anywhere in the selector, rather than taking whichever one comes first. *)
  let extract_modified_class_name modified_base_selector base_class =
    let derived cls =
      String.equal cls base_class
      || String.length cls > String.length base_class + 1
         && String.equal
              (String.sub cls
                 (String.length cls - String.length base_class - 1)
                 (String.length base_class + 1))
              (":" ^ base_class)
    in
    let rec find sel =
      match sel with
      | Css.Selector.Class cls when derived cls -> Some cls
      | Css.Selector.Compound sels
      | Css.Selector.List sels
      | Css.Selector.Is sels
      | Css.Selector.Where sels
      | Css.Selector.Not sels
      | Css.Selector.Has sels ->
          List.find_map find sels
      | Css.Selector.Combined (a, _, b) -> (
          match find a with Some _ as r -> r | None -> find b)
      | Css.Selector.Relative (_, b) -> find b
      | _ -> None
    in
    Option.value (find modified_base_selector) ~default:base_class

  (* A selector carrying a combinator cannot stand on the right of another one,
     nor inside a compound, so it goes in [:is()] there - the same rule CSS
     Nesting applies when substituting [&]. *)
  let rec is_complex_selector = function
    | Css.Selector.Combined _ -> true
    | Css.Selector.Compound sels | Css.Selector.List sels ->
        List.exists is_complex_selector sels
    | _ -> false

  (* Transform selector by applying modifier to base class and updating
     descendants. *)
  let transform_selector_with_modifier modified_base_selector base_class
      modified_class selector =
    let replace_in_children =
      replace_class_in_selector ~old_class:base_class ~new_class:modified_class
    in
    let substitute ~enclose =
      if enclose && is_complex_selector modified_base_selector then
        Css.Selector.is_ [ modified_base_selector ]
      else modified_base_selector
    in
    (* [descendant] marks the right of a combinator. A [:where] there holds the
       utility's own zero-specificity self-reference (as prose's [:where(.prose
       > :last-child)] does), which only wants the new class name. An [:is] is
       where the child and descendant variants bury the class, so an outer
       variant has to reach into it. *)
    let rec transform ~enclose ~descendant = function
      | Css.Selector.Class cls when String.equal cls base_class ->
          substitute ~enclose
      | Css.Selector.Combined (base_sel, combinator, complex_sel) ->
          Css.Selector.Combined
            ( transform ~enclose ~descendant base_sel,
              combinator,
              transform ~enclose:true ~descendant:true complex_sel )
      | Css.Selector.Compound selectors ->
          Css.Selector.Compound
            (List.map (transform ~enclose:true ~descendant) selectors)
      | Css.Selector.List selectors ->
          Css.Selector.List
            (List.map (transform ~enclose ~descendant) selectors)
      | Css.Selector.Is selectors ->
          Css.Selector.Is
            (List.map (transform ~enclose:false ~descendant:false) selectors)
      | Css.Selector.Where selectors when not descendant ->
          Css.Selector.Where
            (List.map (transform ~enclose:false ~descendant) selectors)
      | Css.Selector.Where _ as sel -> replace_in_children sel
      | Css.Selector.Relative (comb, sel) ->
          Css.Selector.Relative
            (comb, transform ~enclose:true ~descendant:true sel)
      | other -> other
    in
    transform ~enclose:false ~descendant:false selector
end

let resolve_scheme = function Some s -> s | None -> Scheme.default

let breakpoint_rem = function
  | `Sm -> 40.
  | `Md -> 48.
  | `Lg -> 64.
  | `Xl -> 80.
  | `Xl_2 -> 96.

(* Publish the breakpoints through the theme-token registry so [theme()] in a
   project's CSS resolves against the same values the [sm:]/[md:] variants use.
   [Scheme.token] then answers for [--breakpoint-*] without a second table. *)
let () =
  List.iter
    (fun bp ->
      Scheme.register_default_token
        ("breakpoint-" ^ string_of_breakpoint bp)
        (Css.Pp.to_string Css.pp_length (Css.Rem (breakpoint_rem bp))))
    [ `Sm; `Md; `Lg; `Xl; `Xl_2 ]

let media_min_width_px px = Css.media_min_width_length (Css.Px px)
let media_min_width_rem rem = Css.media_min_width_length (Css.Rem rem)
let media_not_min_width_px px = Css.media_not_min_width_length (Css.Px px)
let media_not_min_width_rem rem = Css.media_not_min_width_length (Css.Rem rem)

let media_feature name ident =
  Css.Media.Cond
    (Css.Media.Feature (Css.Media.Plain (name, Css.Media.Ident ident)))

let hover_media = media_feature Css.Media.Hover Css.Media.Hover

let print_media =
  Css.Media.Type { prefix = None; type_ = Css.Media.Print; trailing = None }

let negate_media = function
  | Css.Media.Cond condition ->
      (* Tailwind negates a feature query with the legacy [not all and (...)]
         form rather than the Level 4 [not (...)] condition negation. *)
      Css.Media.Type
        {
          prefix = Some Css.Media.Not;
          type_ = Css.Media.All;
          trailing = Some condition;
        }
  | Css.Media.Type ({ prefix = Some Css.Media.Not; _ } as media) ->
      Css.Media.Type { media with prefix = None }
  | Css.Media.Type media ->
      Css.Media.Type { media with prefix = Some Css.Media.Not }
  | Css.Media.List _ as media ->
      Css.Media.of_string ("not " ^ Css.Media.to_string media)

(** Get the media condition for a breakpoint, using the scheme override when
    available, otherwise the default rem value. *)
let breakpoint_condition ?theme bp =
  let name = string_of_breakpoint bp in
  match Scheme.breakpoint_length (resolve_scheme theme) name with
  | Some length -> Css.media_min_width_length length
  | None -> media_min_width_rem (breakpoint_rem bp)

(** Get the negated media condition for max-* breakpoints. *)
let breakpoint_not_condition ?theme bp =
  let name = string_of_breakpoint bp in
  match Scheme.breakpoint_length (resolve_scheme theme) name with
  | Some length -> Css.media_not_min_width_length length
  | None -> media_not_min_width_rem (breakpoint_rem bp)

(** Get the media condition and class prefix for a responsive modifier. *)
let responsive_modifier_condition ?theme = function
  | Style.Responsive bp ->
      let prefix = string_of_breakpoint bp in
      (breakpoint_condition ?theme bp, prefix)
  | Style.Min_responsive bp ->
      let prefix =
        match bp with
        | `Sm -> "min-sm"
        | `Md -> "min-md"
        | `Lg -> "min-lg"
        | `Xl -> "min-xl"
        | `Xl_2 -> "min-2xl"
      in
      (breakpoint_condition ?theme bp, prefix)
  | Style.Max_responsive bp ->
      let prefix =
        match bp with
        | `Sm -> "max-sm"
        | `Md -> "max-md"
        | `Lg -> "max-lg"
        | `Xl -> "max-xl"
        | `Xl_2 -> "max-2xl"
      in
      (breakpoint_not_condition ?theme bp, prefix)
  | Style.Min_arbitrary w -> (media_min_width_px w.px, "min-[" ^ w.text ^ "]")
  | Style.Max_arbitrary w ->
      (media_not_min_width_px w.px, "max-[" ^ w.text ^ "]")
  | Style.Min_arbitrary_length l ->
      (Css.media_min_width_length l.len, "min-[" ^ l.text ^ "]")
  | Style.Max_arbitrary_length l ->
      (Css.media_not_min_width_length l.len, "max-[" ^ l.text ^ "]")
  | Style.Custom_responsive name ->
      let length =
        match Scheme.breakpoint_length (resolve_scheme theme) name with
        | Some length -> length
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.media_min_width_length length, name)
  | Style.Min_custom name ->
      let length =
        match Scheme.breakpoint_length (resolve_scheme theme) name with
        | Some length -> length
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.media_min_width_length length, "min-" ^ name)
  | Style.Max_custom name ->
      let length =
        match Scheme.breakpoint_length (resolve_scheme theme) name with
        | Some length -> length
        | None -> failwith ("unknown custom breakpoint: " ^ name)
      in
      (Css.media_not_min_width_length length, "max-" ^ name)
  | _ -> failwith "not a responsive modifier"

let selector_with_data_key selector key value =
  let attr_selector = Css.Selector.attribute key (Exact value) in
  Css.Selector.combine selector Descendant attr_selector

(* A [hover:] rule carries its own [@media (hover:hover)]. An outer query nests
   the two rather than swallowing the inner one, which is the structure Tailwind
   emits for [lg:hover:X] and [@md:hover:X]. *)
let nested_hover ~selector ~props =
  [ Css.media ~condition:hover_media [ Css.rule ~selector props ] ]

(* At-rule variants carry their rule in [nested] when an inner hover-gated
   variant is present. Keeping the declarations on the wrapper would discard
   [has_hover], because only [Regular] outputs retain that flag. *)
let at_rule_body ~inner_has_hover ~selector ~props =
  if inner_has_hover then ([], nested_hover ~selector ~props) else (props, [])

let media_rule_with_prefix ?(inner_has_hover = false) prefix condition
    base_class selector props =
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  if inner_has_hover then
    media_query ~condition ~selector:new_selector ~props:[]
      ~base_class:modified_class
      ~nested:(nested_hover ~selector:new_selector ~props)
      ()
  else
    media_query ~condition ~selector:new_selector ~props
      ~base_class:modified_class ()

let responsive_rule ?theme ?inner_has_hover breakpoint base_class selector props
    =
  media_rule_with_prefix ?inner_has_hover
    (string_of_breakpoint breakpoint)
    (breakpoint_condition ?theme breakpoint)
    base_class selector props

let responsive_breakpoint_prefix prefix breakpoint =
  let suffix =
    match breakpoint with
    | `Sm -> "sm"
    | `Md -> "md"
    | `Lg -> "lg"
    | `Xl -> "xl"
    | `Xl_2 -> "2xl"
  in
  prefix ^ "-" ^ suffix

let min_responsive_rule ?theme ?inner_has_hover breakpoint base_class selector
    props =
  media_rule_with_prefix ?inner_has_hover
    (responsive_breakpoint_prefix "min" breakpoint)
    (breakpoint_condition ?theme breakpoint)
    base_class selector props

let max_responsive_rule ?theme ?inner_has_hover breakpoint base_class selector
    props =
  media_rule_with_prefix ?inner_has_hover
    (responsive_breakpoint_prefix "max" breakpoint)
    (breakpoint_not_condition ?theme breakpoint)
    base_class selector props

let min_arbitrary_rule ?inner_has_hover (w : Style.arbitrary_px) base_class
    selector props =
  let prefix = "min-[" ^ w.text ^ "]" in
  media_rule_with_prefix ?inner_has_hover prefix (media_min_width_px w.px)
    base_class selector props

let max_arbitrary_rule ?inner_has_hover (w : Style.arbitrary_px) base_class
    selector props =
  let prefix = "max-[" ^ w.text ^ "]" in
  media_rule_with_prefix ?inner_has_hover prefix
    (media_not_min_width_px w.px)
    base_class selector props

let arbitrary_length_rule ?inner_has_hover prefix condition
    (l : Style.arbitrary_length) base_class selector props =
  media_rule_with_prefix ?inner_has_hover
    (prefix ^ "-[" ^ l.text ^ "]")
    condition base_class selector props

let min_arbitrary_length_rule ?inner_has_hover (l : Style.arbitrary_length)
    base_class selector props =
  arbitrary_length_rule ?inner_has_hover "min"
    (Css.media_min_width_length l.len)
    l base_class selector props

let max_arbitrary_length_rule ?inner_has_hover (l : Style.arbitrary_length)
    base_class selector props =
  arbitrary_length_rule ?inner_has_hover "max"
    (Css.media_not_min_width_length l.len)
    l base_class selector props

let custom_breakpoint ?theme name =
  match Scheme.breakpoint_length (resolve_scheme theme) name with
  | Some length -> length
  | None -> failwith ("unknown custom breakpoint: " ^ name)

let custom_media_rule ?theme ?inner_has_hover prefix condition_of_length name
    base_class selector props =
  let length = custom_breakpoint ?theme name in
  media_rule_with_prefix ?inner_has_hover (prefix name)
    (condition_of_length length)
    base_class selector props

let custom_responsive_rule ?theme ?inner_has_hover name base_class selector
    props =
  custom_media_rule ?theme ?inner_has_hover Fun.id Css.media_min_width_length
    name base_class selector props

let min_custom_rule ?theme ?inner_has_hover name base_class selector props =
  custom_media_rule ?theme ?inner_has_hover
    (fun name -> "min-" ^ name)
    Css.media_min_width_length name base_class selector props

let max_custom_rule ?theme ?inner_has_hover name base_class selector props =
  custom_media_rule ?theme ?inner_has_hover
    (fun name -> "max-" ^ name)
    Css.media_not_min_width_length name base_class selector props

let container_rule ?(inner_has_hover = false) query base_class selector props =
  let prefix = Containers.container_query_to_class_prefix query in
  let modified_class = prefix ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition = Containers.container_query_to_condition query in
  if inner_has_hover then
    container_query ~condition ~selector:new_selector ~props:[]
      ~base_class:modified_class
      ~nested:(nested_hover ~selector:new_selector ~props)
      ()
  else
    container_query ~condition ~selector:new_selector ~props
      ~base_class:modified_class ()

(** Parse a has-[...] selector string as a relative CSS selector ([:has()]
    accepts a bare leading combinator, e.g. [>div] or [~img]), with [&] (the
    utility's own element) resolved to the universal selector - the same
    substitution {!Modifiers.nest_selector} performs for [group-[...]] /
    [peer-[...]] templates, via {!Cascade.Nest.substitute}. The boolean records
    whether that substitution happened, because Tailwind leaves the resulting
    complex selector outside [:is()]. *)
let has_relative_selector s =
  let unresolved =
    Css.Selector.read_relative
      (Cascade.Cursor.of_string (Parse.decode_underscores s))
  in
  let has_nesting =
    Css.Selector.any
      (function Css.Selector.Nesting -> true | _ -> false)
      unresolved
  in
  ( Cascade.Nest.substitute ~parent:Css.Selector.universal unresolved,
    has_nesting )

(* The selector a has-shorthand name stands for. [has-<state>] takes the same
   state names as the group/peer variants and matches what that state matches,
   so most map straight to their pseudo-class. *)
let resolve_has_shorthand = function
  | "hocus" -> ":hover, :focus"
  | "open" -> ":is([open], :popover-open, :open)"
  | "inert" -> ":is([inert], [inert] *)"
  | "odd" -> ":nth-child(odd)"
  | "even" -> ":nth-child(even)"
  | "first" -> ":first-child"
  | "last" -> ":last-child"
  | "only" -> ":only-child"
  | s when Style.is_data_attr_name s -> "[" ^ s ^ "]"
  | s -> ":" ^ s

(* Tailwind puts a bracketed type, universal, selector-list, or complex selector
   through [:is()] before it enters [:has()]. A class, ID, attribute or
   pseudo-class stays simple, and a leading combinator stays relative. *)
let wrap_has_bracket_selector ~has_nesting =
  let open Css.Selector in
  function
  | sel when has_nesting -> sel
  | List sels -> is_ sels
  | ( Element _ | Universal _ | Combined _
    | Compound ((Element _ | Universal _) :: _) ) as sel ->
      is_ [ sel ]
  | sel -> sel

(* The [:has()] argument a has-variant matches, from the spelling the modifier
   stored: a state name resolves to its pseudo-class, anything else is already a
   selector. *)
let has_inner_selector raw =
  let str =
    if Style.is_has_shorthand raw then resolve_has_shorthand raw else raw
  in
  let sel, has_nesting = has_relative_selector str in
  if Style.is_has_shorthand raw then sel
  else wrap_has_bracket_selector ~has_nesting sel

(* The relative selector a group/peer has-variant scopes to:
   [:where(.group):has(<inner>) *]. *)
let has_anchor_rel ~anchor ~combinator ?name inner =
  let open Css.Selector in
  let anchor_class =
    match name with Some n -> anchor ^ "/" ^ n | None -> anchor
  in
  combine
    (compound [ where [ Class anchor_class ]; has [ inner ] ])
    combinator universal

(* Rebuild [selector] around the [modified] selector a route builds for the bare
   class, so an inner variant's own work survives: [aria-selected:hover:X] keeps
   its [:hover]. With no inner variant [selector] is the bare class and the
   result is [modified] itself. *)
let route_regular ~selector ~base_class ~modified_class ~modified ?has_hover
    props =
  let sel =
    Rules_selector.transform_selector_with_modifier modified base_class
      modified_class selector
  in
  regular ~selector:sel ~props ~base_class:modified_class ?has_hover ()

let has_like_selector kind ?name ?shorthand ?(has_hover = false) ~selector
    selector_str base_class props =
  let open Css.Selector in
  let parsed_selector, has_nesting = has_relative_selector selector_str in
  (* A bracket value is one arbitrary selector and is wrapped when it is not
     already relative. The [hocus] shorthand is genuinely two selectors and
     stays a list. *)
  let parsed_selector =
    match shorthand with
    | None -> wrap_has_bracket_selector ~has_nesting parsed_selector
    | Some _ -> parsed_selector
  in
  let has_part s =
    match shorthand with Some sh -> sh | None -> "[" ^ s ^ "]"
  in
  match kind with
  | `Has ->
      let class_name = "has-" ^ has_part selector_str ^ ":" ^ base_class in
      let modified = compound [ class_ class_name; has [ parsed_selector ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        ~has_hover props
  | `Group_has ->
      let name_suffix = match name with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-has-" ^ has_part selector_str ^ name_suffix ^ ":" ^ base_class
      in
      let rel =
        has_anchor_rel ~anchor:"group" ~combinator:Descendant ?name
          parsed_selector
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        ~has_hover props
  | `Peer_has ->
      let name_suffix = match name with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-has-" ^ has_part selector_str ^ name_suffix ^ ":" ^ base_class
      in
      let rel =
        has_anchor_rel ~anchor:"peer" ~combinator:Subsequent_sibling ?name
          parsed_selector
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        ~has_hover props

(* Pseudo-class modifiers: transform the base selector and mark hover when
   needed. *)
let handle_pseudo_class_modifier ?(inner_has_hover = false) modifier base_class
    selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  let has_hover = Modifiers.is_hover modifier in
  if has_hover && inner_has_hover then
    (* Nested hover: wrap in @media (hover:hover) { @media (hover:hover) { }
       } *)
    let hover : Css.Media.t = hover_media in
    let inner_rule = Css.rule ~selector:new_selector props in
    let inner_media = Css.media ~condition:hover [ inner_rule ] in
    media_query ~condition:hover ~selector:new_selector ~props:[]
      ~base_class:modified_class ~nested:[ inner_media ] ()
  else
    regular ~selector:new_selector ~props ~base_class:modified_class ~has_hover
      ()

(** Handle data attribute modifiers (data-state, data-variant, etc.) *)
let handle_data_modifier key value selector props base_class =
  regular
    ~selector:(selector_with_data_key selector ("data-" ^ key) value)
    ~props ~base_class ()

(* Media-like modifiers (dark, motion/contrast prefs) should transform the
   existing selector structure rather than rebuilding a flat class selector.
   When the inner rule has_hover, we need nested media queries to match
   Tailwind's structure: @media (condition) { @media (hover:hover) { ... } } *)
let handle_media_like_modifier (modifier : Style.modifier)
    ~(condition : Css.Media.t) ?(inner_has_hover = false) base_class selector
    props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  if inner_has_hover then
    (* For compound dark:hover: case, generate a Media_query with nested hover
       media. This allows the optimizer to group these rules together. The
       selector already has :hover from the inner rule transformation - do NOT
       add another one. *)
    let hover_selector =
      Rules_selector.transform_selector_with_modifier modified_base_selector
        base_class modified_class selector
    in
    (* Nested @media (hover:hover) { .dark\:hover\:X:hover { props } } *)
    let inner_hover_media =
      let hover : Css.Media.t = hover_media in
      Css.media ~condition:hover [ Css.rule ~selector:hover_selector props ]
    in
    media_query ~condition ~selector:hover_selector ~props:[]
      ~base_class:modified_class ~nested:[ inner_hover_media ] ()
  else
    let new_selector =
      Rules_selector.transform_selector_with_modifier modified_base_selector
        base_class modified_class selector
    in
    media_query ~condition ~selector:new_selector ~props
      ~base_class:modified_class ()

(* Route data attribute modifiers *)
let route_data_modifier modifier base_class selector props =
  match modifier with
  | Style.Data_state v ->
      handle_data_modifier "state" v selector props base_class
  | Style.Data_variant v ->
      handle_data_modifier "variant" v selector props base_class
  | Style.Data_active ->
      handle_data_modifier "active" "" selector props base_class
  | Style.Data_inactive ->
      handle_data_modifier "inactive" "" selector props base_class
  | Style.Data_custom (k, v) ->
      handle_data_modifier k v selector props base_class
  | _ -> regular ~selector ~props ~base_class ()

(* Known data shorthand names *)
let _is_data_shorthand_name = function
  | "disabled" | "active" | "inactive" -> true
  | _ -> false

(* Route data bracket variants to appropriate handler *)
let route_data_bracket_modifier modifier ~selector base_class props =
  let kind, raw_str, name_opt =
    match modifier with
    | Style.Data_bracket s -> (`Data, "[" ^ s ^ "]", None)
    | Style.Group_data (s, n) -> (`Group_data, s, n)
    | Style.Peer_data (s, n) -> (`Peer_data, s, n)
    | _ -> failwith "Invalid data bracket modifier"
  in
  (* [raw_str] is the spelling: [[state=open]] bracketed, [dragging] bare. The
     expression to parse is the same either way. *)
  let expr =
    let n = String.length raw_str in
    if n > 1 && raw_str.[0] = '[' && raw_str.[n - 1] = ']' then
      String.sub raw_str 1 (n - 2)
    else raw_str
  in
  let attr_name, attr_match, attr_flag = Modifiers.parse_data_expr expr in
  let open Css.Selector in
  let class_part = raw_str in
  match kind with
  | `Data ->
      let class_name = "data-" ^ class_part ^ ":" ^ base_class in
      let modified =
        compound
          [ class_ class_name; attribute ?flag:attr_flag attr_name attr_match ]
      in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props
  | `Group_data ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-data-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let group_class =
        match name_opt with Some n -> "group/" ^ n | None -> "group"
      in
      let rel =
        combine
          (compound
             [
               where [ Class group_class ];
               attribute ?flag:attr_flag attr_name attr_match;
             ])
          Descendant universal
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props
  | `Peer_data ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-data-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let peer_class =
        match name_opt with Some n -> "peer/" ^ n | None -> "peer"
      in
      let rel =
        combine
          (compound
             [
               where [ Class peer_class ];
               attribute ?flag:attr_flag attr_name attr_match;
             ])
          Subsequent_sibling universal
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props

(* Route :has() variants to appropriate handler *)
let route_has_modifier modifier ~selector base_class props =
  let kind, raw_str, name =
    match modifier with
    | Style.Has s -> (`Has, s, None)
    | Style.Group_has (s, name) -> (`Group_has, s, name)
    | Style.Peer_has (s, name) -> (`Peer_has, s, name)
    | _ -> failwith "Invalid has modifier"
  in
  (* A shorthand is one of the state names, spelled bare; anything else was
     written in brackets and keeps them in the class name. Deciding this by
     punctuation alone read a bare type selector like [has-[a]] as the state
     name [a] and dropped the brackets. *)
  let is_shorthand = Style.is_has_shorthand raw_str in
  let selector_str =
    if is_shorthand then resolve_has_shorthand raw_str else raw_str
  in
  let shorthand = if is_shorthand then Some raw_str else None in
  (* [has-hover] gates on the pointer just like [hover] itself does. *)
  let has_hover = is_shorthand && raw_str = "hover" in
  has_like_selector kind ?name ?shorthand ~has_hover ~selector selector_str
    base_class props

(* Parse an aria expression string into an attribute name and match. "modal" →
   ("aria-modal", Presence) "valuenow=1" → ("aria-valuenow", Exact "1")
   "invalid=spelling" → ("aria-invalid", Exact "spelling") The expression is an
   arbitrary value, so [_] is a space in it and [\_] a literal underscore. *)
let parse_aria_expr expr =
  let expr = Parse.decode_underscores expr in
  let expr = String.trim expr in
  match String.index_opt expr '=' with
  | None -> ("aria-" ^ expr, Css.Selector.Presence)
  | Some i ->
      let attr = String.trim (String.sub expr 0 i) in
      let raw_value =
        String.trim (String.sub expr (i + 1) (String.length expr - i - 1))
      in
      (* The quotes an author writes around the value are the attribute
         selector's own, which the printer puts back; either style spells the
         same value. *)
      let value =
        let len = String.length raw_value in
        if
          len >= 2
          && (raw_value.[0] = '"' || raw_value.[0] = '\'')
          && raw_value.[len - 1] = raw_value.[0]
        then String.sub raw_value 1 (len - 2)
        else raw_value
      in
      ("aria-" ^ attr, Css.Selector.Exact value)

(* Route aria variants to appropriate handler *)
let route_aria_modifier modifier ~selector base_class props =
  let kind, raw_str, name_opt =
    match modifier with
    | Style.Aria_bracket s -> (`Aria, s, None)
    | Style.Group_aria (s, n) -> (`Group_aria, s, n)
    | Style.Peer_aria (s, n) -> (`Peer_aria, s, n)
    | _ -> failwith "Invalid aria modifier"
  in
  let open Css.Selector in
  let is_shorthand = Modifiers.is_aria_shorthand raw_str in
  let aria_attr, aria_match =
    if is_shorthand then ("aria-" ^ raw_str, Exact "true")
    else parse_aria_expr raw_str
  in
  let class_part = if is_shorthand then raw_str else "[" ^ raw_str ^ "]" in
  match kind with
  | `Aria ->
      let class_name = "aria-" ^ class_part ^ ":" ^ base_class in
      let modified =
        compound [ class_ class_name; attribute aria_attr aria_match ]
      in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props
  | `Group_aria ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "group-aria-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let group_class =
        match name_opt with Some n -> "group/" ^ n | None -> "group"
      in
      let rel =
        combine
          (compound
             [ where [ Class group_class ]; attribute aria_attr aria_match ])
          Descendant universal
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props
  | `Peer_aria ->
      let name_suffix = match name_opt with Some n -> "/" ^ n | None -> "" in
      let class_name =
        "peer-aria-" ^ class_part ^ name_suffix ^ ":" ^ base_class
      in
      let peer_class =
        match name_opt with Some n -> "peer/" ^ n | None -> "peer"
      in
      let rel =
        combine
          (compound
             [ where [ Class peer_class ]; attribute aria_attr aria_match ])
          Subsequent_sibling universal
      in
      let modified = compound [ Class class_name; is_ [ rel ] ] in
      route_regular ~selector ~base_class ~modified_class:class_name ~modified
        props

(* Handle fallback for unmatched modifiers. Must extract modified_class so that
   outer modifiers like dark: can properly transform the selector. *)
let handle_fallback_modifier ?(inner_has_hover = false) modifier base_class
    selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  let has_hover = Modifiers.is_hover modifier || inner_has_hover in
  regular ~selector:new_selector ~props ~base_class:modified_class ~has_hover ()

(* [before:]/[after:] always carry a content declaration. They report the class
   they prefixed, not the bare one, so an outer variant can find it in the
   selector and prefix its own name in turn. *)
let handle_pseudo_element_modifier modifier base_class props =
  let sel = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name sel base_class
  in
  (* [before:]/[after:] need a [content], but a [content-*] utility already
     brings its own; adding a second one leaves it declared twice. *)
  let reads_content_var d =
    Css.declaration_name d = "content"
    && Css.declaration_value d = "var(--tw-content)"
  in
  let props =
    if List.exists reads_content_var props then props
    else Css.content (Var (Var.reference Typography.content_var)) :: props
  in
  regular ~selector:sel ~props ~base_class:modified_class ()

let normalize_supports_condition = Modifiers.normalize_supports_condition

(** Handle [supports-<property>] modifier: builds the shorthand class name and
    emits [\@supports (prop: var(--tw))] directly, typed. *)
let handle_supports_property_modifier ?(inner_has_hover = false) prop base_class
    selector props =
  let modified_class = "supports-" ^ prop ^ ":" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition = Css.Supports.property prop "var(--tw)" in
  let props, nested =
    at_rule_body ~inner_has_hover ~selector:new_selector ~props
  in
  supports_query ~condition ~selector:new_selector ~props ~nested
    ~base_class:modified_class ()

(** Handle [supports-[condition]] modifier: builds the bracket class name,
    normalizes the author's condition text, and emits a supports query rule. *)
let handle_supports_condition_modifier ?(inner_has_hover = false) condition_str
    base_class selector props =
  let modified_class = "supports-[" ^ condition_str ^ "]:" ^ base_class in
  let new_selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let condition = normalize_supports_condition condition_str in
  let props, nested =
    at_rule_body ~inner_has_hover ~selector:new_selector ~props
  in
  supports_query ~condition ~selector:new_selector ~props ~nested
    ~base_class:modified_class ()

(** Map a media-like modifier to its corresponding Css.Media.t condition.
    Returns [Some condition] for modifiers that map to media queries, [None] for
    non-media modifiers. *)
let media_condition_of_modifier = function
  | Style.Dark ->
      Some (media_feature Css.Media.Prefers_color_scheme Css.Media.Dark)
  | Style.Motion_safe ->
      Some
        (media_feature Css.Media.Prefers_reduced_motion Css.Media.No_preference)
  | Style.Motion_reduce ->
      Some (media_feature Css.Media.Prefers_reduced_motion Css.Media.Reduce)
  | Style.Contrast_more ->
      Some (media_feature Css.Media.Prefers_contrast Css.Media.More)
  | Style.Contrast_less ->
      Some (media_feature Css.Media.Prefers_contrast Css.Media.Less)
  | Style.Print -> Some print_media
  | Style.Portrait ->
      Some (media_feature Css.Media.Orientation Css.Media.Portrait)
  | Style.Landscape ->
      Some (media_feature Css.Media.Orientation Css.Media.Landscape)
  | Style.Forced_colors ->
      Some (media_feature Css.Media.Forced_colors Css.Media.Active)
  | Style.Inverted_colors ->
      Some (media_feature Css.Media.Inverted_colors Css.Media.Inverted)
  | Style.Pointer_none -> Some (media_feature Css.Media.Pointer Css.Media.None)
  | Style.Pointer_coarse ->
      Some (media_feature Css.Media.Pointer Css.Media.Coarse)
  | Style.Pointer_fine -> Some (media_feature Css.Media.Pointer Css.Media.Fine)
  | Style.Any_pointer_none ->
      Some (media_feature Css.Media.Any_pointer Css.Media.None)
  | Style.Any_pointer_coarse ->
      Some (media_feature Css.Media.Any_pointer Css.Media.Coarse)
  | Style.Any_pointer_fine ->
      Some (media_feature Css.Media.Any_pointer Css.Media.Fine)
  | Style.Noscript -> Some (media_feature Css.Media.Scripting Css.Media.None)
  | _ -> None

(** Variant order for not-* inner modifiers. Returns a large offset that encodes
    the inner modifier's position in the Tailwind v4 variant order. This ensures
    not-* rules sort by variant position, not alphabetically. The offset is
    multiplied by 100 to leave room for the base suborder. *)

(** Compute variant_order from base_class and selector. A stacked candidate is
    placed by its highest-order modifier, matching the descending key list used
    by the comparator. For before/after, the base_class is the raw utility name
    without prefix, so we detect them from the selector content. [selector_str]
    is the caller's already-rendered selector: [Build.add_index] renders it two
    lines before calling this. *)
let compute_variant_order ~selector_str base_class =
  let from_base_class bc =
    let modifiers, _ = Modifiers.of_string bc in
    List.fold_left
      (fun order modifier ->
        Int.max order (Modifiers.variant_order_of_prefix modifier))
      0 modifiers
  in
  let vo = match base_class with None -> 0 | Some bc -> from_base_class bc in
  (* If no variant_order from base_class, check selector for modifier-based
     pseudo-elements (before:/after: modifiers). Only detect when the selector
     class name contains the escaped modifier prefix (e.g., "before\:absolute")
     to avoid matching utility-generated pseudo-elements like prose's
     ::before. *)
  if vo > 0 then vo
  else if Strings.contains ~sub:"before\\:" selector_str then
    Modifiers.variant_order_of_prefix "before"
  else if Strings.contains ~sub:"after\\:" selector_str then
    Modifiers.variant_order_of_prefix "after"
  else 0

(** Build the class name prefix for a not-* inner modifier. Handles shorthand
    forms like data-foo, has-checked, nth-2 that need different class names than
    their pp_modifier representation. *)
let not_class_prefix inner_modifier =
  match inner_modifier with
  | Style.Data_custom (attr, "") -> "data-" ^ attr
  | Style.Has pseudo_str
    when String.length pseudo_str > 0 && pseudo_str.[0] = ':' ->
      "has-" ^ String.sub pseudo_str 1 (String.length pseudo_str - 1)
  | Style.Has shorthand_name -> "has-" ^ shorthand_name
  | Style.Has_variant m -> "has-" ^ Modifiers.pp_modifier m
  | Style.Nth expr -> Style.pp_nth "nth" expr
  | Style.Nth_last expr -> Style.pp_nth "nth-last" expr
  | Style.Nth_of_type expr -> Style.pp_nth "nth-of-type" expr
  | Style.Nth_last_of_type expr -> Style.pp_nth "nth-last-of-type" expr
  | Style.Supports_property prop -> "supports-" ^ prop
  | inner -> Modifiers.pp_modifier inner

(* The relative selector a [group-not-]/[peer-not-] variant contributes:
   [:where(.group):not(<conditions>) *]. *)

(** Extract the pseudo-class selector(s) from a modifier for use in :not().
    Returns a list of selectors that go inside :not(sel1, sel2, ...). *)
let named_rel ~marker ~combinator ~name_opt extra =
  let marker_class =
    match name_opt with
    | Option.None -> Css.Selector.Class marker
    | Option.Some n -> Css.Selector.Class (marker ^ "/" ^ n)
  in
  Css.Selector.combine
    (Css.Selector.compound (Css.Selector.where [ marker_class ] :: extra))
    combinator Css.Selector.universal

let named_not_rel ~marker ~combinator ~name_opt conditions =
  named_rel ~marker ~combinator ~name_opt [ Css.Selector.Not conditions ]

let rec scoped_conditions ~marker ~combinator ~name_opt ~negated m base_class =
  let inner = extract_not_conditions m base_class in
  let extra = if negated then [ Css.Selector.Not inner ] else inner in
  [ Css.Selector.is_ [ named_rel ~marker ~combinator ~name_opt extra ] ]

and extract_not_conditions inner_modifier base_class =
  match inner_modifier with
  | Style.Hocus | Style.Device_hocus ->
      [ Css.Selector.Hover; Css.Selector.Focus ]
  | Style.Has selector_str ->
      [ Css.Selector.Has [ has_inner_selector selector_str ] ]
  (* [has-<variant>] holds the variant itself, so its selector - the same one
     [:not()] would negate - is what goes inside [:has()]. *)
  | Style.Has_variant m ->
      [ Css.Selector.Has (extract_not_conditions m base_class) ]
  (* A scoped variant carries its own relative selector - negated for the [not-]
     forms - and that is the whole of what an outer variant sees of it. *)
  | Style.Group_not (m, name_opt) ->
      scoped_conditions ~marker:"group" ~combinator:Css.Selector.Descendant
        ~name_opt ~negated:true m base_class
  | Style.Peer_not (m, name_opt) ->
      scoped_conditions ~marker:"peer"
        ~combinator:Css.Selector.Subsequent_sibling ~name_opt ~negated:true m
        base_class
  | Style.Named_group (m, name) ->
      scoped_conditions ~marker:"group" ~combinator:Css.Selector.Descendant
        ~name_opt:(Option.Some name) ~negated:false m base_class
  | Style.Named_peer (m, name) ->
      scoped_conditions ~marker:"peer"
        ~combinator:Css.Selector.Subsequent_sibling ~name_opt:(Option.Some name)
        ~negated:false m base_class
  (* [not-group-has-X] negates the whole scoped relative selector, not just its
     [:has()] part. *)
  | Style.Group_has (selector_str, name) ->
      [
        Css.Selector.is_
          [
            has_anchor_rel ~anchor:"group" ~combinator:Css.Selector.Descendant
              ?name
              (has_inner_selector selector_str);
          ];
      ]
  | Style.Peer_has (selector_str, name) ->
      [
        Css.Selector.is_
          [
            has_anchor_rel ~anchor:"peer"
              ~combinator:Css.Selector.Subsequent_sibling ?name
              (has_inner_selector selector_str);
          ];
      ]
  | Style.Data_custom (attr, "") ->
      [ Css.Selector.attribute ("data-" ^ attr) Presence ]
  | Style.Data_custom (attr, value) ->
      [ Css.Selector.attribute ("data-" ^ attr) (Exact value) ]
  | Style.Aria_selected ->
      [ Css.Selector.attribute "aria-selected" (Exact "true") ]
  | Style.Aria_checked ->
      [ Css.Selector.attribute "aria-checked" (Exact "true") ]
  | Style.Aria_expanded ->
      [ Css.Selector.attribute "aria-expanded" (Exact "true") ]
  | Style.Aria_disabled ->
      [ Css.Selector.attribute "aria-disabled" (Exact "true") ]
  | _ -> (
      (* Generic extraction: get selector from to_selector and strip the leading
         Class element to get just the pseudo-class part *)
      let sel = Modifiers.to_selector inner_modifier base_class in
      match sel with
      | Css.Selector.Compound (Css.Selector.Class _ :: rest) when rest <> [] ->
          rest
      (* A descendant-style variant anchors the class under an ancestor.
         Negating it negates the ancestor relation, so the class's own position
         becomes a universal: [not-in-data-open] negates [:where([data-open]) *]
         rather than the class itself. *)
      | Css.Selector.Combined (ancestor, comb, Css.Selector.Class c)
        when String.equal c base_class ->
          [ Css.Selector.Combined (ancestor, comb, Css.Selector.universal) ]
      | _ -> [ sel ])

(** Build a regular rule with :not() selector for a not-* modifier. *)
let not_selector_rule inner_modifier modified_class base_class ~selector props =
  let conditions = extract_not_conditions inner_modifier base_class in
  let not_sel = Css.Selector.Not conditions in
  let modified =
    Css.Selector.compound [ Css.Selector.Class modified_class; not_sel ]
  in
  route_regular ~selector ~base_class ~modified_class ~modified props

(* Create a single not-media rule *)
let not_media_rule ~condition modified_class props =
  [
    media_query ~condition ~selector:(Css.Selector.Class modified_class) ~props
      ~base_class:modified_class ();
  ]

(** Handle :not() pseudo-class modifier: dispatches to the right rule type based
    on the inner modifier. Returns a list of rules since some modifiers (like
    hover) produce both a selector rule and a media rule. *)
let handle_not_modifier ?theme inner_modifier base_class selector props =
  let modified_class =
    "not-" ^ not_class_prefix inner_modifier ^ ":" ^ base_class
  in
  let sel_rule () =
    not_selector_rule inner_modifier modified_class base_class ~selector props
  in
  let not_hover_media () =
    not_media_rule ~condition:(negate_media hover_media) modified_class props
  in
  match inner_modifier with
  | Style.Hover -> [ sel_rule () ] @ not_hover_media ()
  | Style.Device_hocus -> [ sel_rule () ] @ not_hover_media ()
  | Style.Hocus -> [ sel_rule () ]
  | _ when Option.is_some (media_condition_of_modifier inner_modifier) ->
      let condition = Option.get (media_condition_of_modifier inner_modifier) in
      not_media_rule ~condition:(negate_media condition) modified_class props
  | Style.Supports_property prop ->
      [
        supports_query
          ~condition:(Css.Supports.Not (Css.Supports.property prop "var(--tw)"))
          ~selector:(Css.Selector.Class modified_class) ~props
          ~base_class:modified_class ();
      ]
  | Style.Supports_condition condition_str ->
      let inner_condition = normalize_supports_condition condition_str in
      [
        supports_query ~condition:(Css.Supports.Not inner_condition)
          ~selector:(Css.Selector.Class modified_class) ~props
          ~base_class:modified_class ();
      ]
  | Style.Responsive bp | Style.Min_responsive bp ->
      not_media_rule
        ~condition:(breakpoint_not_condition ?theme bp)
        modified_class props
  | Style.Max_responsive bp ->
      not_media_rule
        ~condition:(breakpoint_condition ?theme bp)
        modified_class props
  | Style.Min_arbitrary w ->
      not_media_rule
        ~condition:(media_not_min_width_px w.px)
        modified_class props
  | Style.Max_arbitrary w ->
      not_media_rule ~condition:(media_min_width_px w.px) modified_class props
  | Style.Min_arbitrary_length l ->
      not_media_rule
        ~condition:(Css.media_not_min_width_length l.len)
        modified_class props
  | Style.Max_arbitrary_length l ->
      not_media_rule
        ~condition:(Css.media_min_width_length l.len)
        modified_class props
  | _ -> [ sel_rule () ]

(** Parse a bracket media condition string (from not-[@media...]) and return the
    appropriate negated media condition. Handles double negation: not of "not
    (cond)" → positive cond. *)
let parse_bracket_media content =
  let s = Parse.decode_underscores content in
  (* Strip @media prefix *)
  let rest =
    String.trim
      (if String.length s > 7 && String.sub s 0 7 = "@media " then
         String.sub s 7 (String.length s - 7)
       else if String.length s > 6 && String.sub s 0 6 = "@media" then
         String.sub s 6 (String.length s - 6)
       else s)
  in
  (* Check for "not" prefix (double negation → positive) *)
  if String.length rest > 4 && String.sub rest 0 4 = "not " then
    let inner = String.trim (String.sub rest 4 (String.length rest - 4)) in
    (* Double negation: return the positive condition *)
    (* The reader takes the condition however it is spelled, so there is no
       table of spellings here to fall out of step with itself. *)
    Css.Media.of_string inner
  else
    (* Negate the condition *)
    match rest with
    | "print" -> negate_media print_media
    | _ -> negate_media (Css.Media.of_string rest)

(** Parse a bracket pseudo-class string into a CSS selector. *)
let parse_bracket_pseudo content =
  match content with
  | ":checked" -> Css.Selector.Checked
  | ":hover" -> Css.Selector.Hover
  | ":focus" -> Css.Selector.Focus
  | ":active" -> Css.Selector.Active
  | ":disabled" -> Css.Selector.Disabled
  | ":first-child" -> Css.Selector.First_child
  | ":last-child" -> Css.Selector.Last_child
  | ":focus-within" -> Css.Selector.Focus_within
  | ":focus-visible" -> Css.Selector.Focus_visible
  | _ -> Css.Selector.Class content

(** Parse in-[...] bracket content into an ancestor selector. Class selectors
    (starting with .) are used directly; others are wrapped in :is(). *)
let in_bracket_ancestor content =
  if content <> "" && content.[0] = '.' then
    (* Class selector: .group → Class "group" inside :where() *)
    let cls = String.sub content 1 (String.length content - 1) in
    Css.Selector.Class cls
  else
    (* Element or other selector: p → :is(p) inside :where() *)
    Css.Selector.Is [ Css.Selector.Element (None, content) ]

(** Handle in-[...] bracket modifier. Returns a list of rules. *)
let handle_in_bracket content base_class props =
  let modified_class = "in-[" ^ content ^ "]:" ^ base_class in
  let ancestor = in_bracket_ancestor content in
  let sel =
    Css.Selector.combine (Css.Selector.Where [ ancestor ])
      Css.Selector.Descendant (Css.Selector.Class modified_class)
  in
  [ regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in" () ]

(** Handle in-data-X modifier. Returns a list of rules. *)
let handle_in_data attr base_class props =
  let modified_class = "in-data-" ^ attr ^ ":" ^ base_class in
  let sel =
    Css.Selector.combine
      (Css.Selector.Where
         [
           Css.Selector.Attribute (None, Data attr, Css.Selector.Presence, None);
         ])
      Css.Selector.Descendant (Css.Selector.Class modified_class)
  in
  [ regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in" () ]

(* [not-in-*] negates the ancestor relation rather than the class, so the
   class's own position in the negated selector is a universal. *)
let not_in_rule ~modified_class ~ancestor props =
  let sel =
    Css.Selector.compound
      [
        Css.Selector.Class modified_class;
        Css.Selector.Not
          [
            Css.Selector.combine (Css.Selector.Where [ ancestor ])
              Css.Selector.Descendant (Css.Selector.Universal None);
          ];
      ]
  in
  [ regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in" () ]

(** Handle not-in-[...] bracket modifier. Returns a list of rules. *)
let handle_not_in_bracket content base_class props =
  not_in_rule
    ~modified_class:("not-in-[" ^ content ^ "]:" ^ base_class)
    ~ancestor:(in_bracket_ancestor content)
    props

(** Handle the not-in-data-* modifier. Returns a list of rules. *)
let handle_not_in_data attr base_class props =
  not_in_rule
    ~modified_class:("not-in-data-" ^ attr ^ ":" ^ base_class)
    ~ancestor:
      (Css.Selector.Attribute (None, Data attr, Css.Selector.Presence, None))
    props

(** Handle not-[...] bracket modifier. Returns a list of rules. *)
let handle_not_bracket content base_class props =
  let modified_class = "not-[" ^ content ^ "]:" ^ base_class in
  if
    (String.length content > 6 && String.sub content 0 6 = "@media")
    || (String.length content > 7 && String.sub content 0 7 = "@media_")
  then
    (* Media bracket pattern: not-[@media...] → negated media query *)
    let condition = parse_bracket_media content in
    [
      media_query ~condition ~selector:(Css.Selector.Class modified_class)
        ~props ~base_class:modified_class ();
    ]
  else if content <> "" && content.[0] = ':' then
    (* Pseudo-class bracket: not-[:checked] → :not(:checked) *)
    let pseudo = parse_bracket_pseudo content in
    [
      regular
        ~selector:
          (Css.Selector.compound
             [ Css.Selector.Class modified_class; Css.Selector.Not [ pseudo ] ])
        ~props ~base_class:modified_class ();
    ]
  else
    (* Arbitrary selector content: [_] is a space and [&] is the utility's own
       element, which inside a negation becomes the universal selector. So
       [not-[.os-macos_&]] negates the descendant context [.os-macos &] as
       [:not(.os-macos <star>)]. Parse the transformed string as a selector so
       combinators and compounds flatten, rather than escaping it as a single
       class name. *)
    let sel_str = Parse.decode_underscores content in
    let inner =
      Cascade.Nest.substitute ~parent:Css.Selector.universal
        (Css.Selector.read (Cascade.Cursor.of_string sel_str))
    in
    [
      regular
        ~selector:
          (Css.Selector.compound
             [ Css.Selector.Class modified_class; Css.Selector.Not [ inner ] ])
        ~props ~base_class:modified_class ();
    ]

(** Handle group-not-X modifier. Produces selector with
    :is(:where(.group):not(...) descendant) pattern. *)
let not_modifier_inner_string = function
  | Style.Not_bracket content -> "[" ^ content ^ "]"
  | m -> Modifiers.pp_modifier m

let named_modifier_suffix = function None -> "" | Some n -> "/" ^ n

let is_media_inner_modifier = function
  | Style.Hover | Style.Device_hocus -> true
  | inner -> Option.is_some (media_condition_of_modifier inner)

let handle_named_not ~prefix ~base_marker_class ~combinator inner name_opt
    base_class props =
  let inner_str = not_modifier_inner_string inner in
  let name_suffix = named_modifier_suffix name_opt in
  let modified_class =
    prefix ^ "-not-" ^ inner_str ^ name_suffix ^ ":" ^ base_class
  in
  if is_media_inner_modifier inner then []
  else
    let not_conditions =
      match inner with
      | Style.Not_bracket content when content <> "" && content.[0] = ':' ->
          [ parse_bracket_pseudo content ]
      | _ -> extract_not_conditions inner base_class
    in
    let open Css.Selector in
    let rel =
      named_not_rel ~marker:base_marker_class ~combinator ~name_opt
        not_conditions
    in
    [
      regular
        ~selector:(compound [ Class modified_class; is_ [ rel ] ])
        ~props ~base_class:modified_class ();
    ]

let handle_group_not_modifier inner name_opt base_class props =
  handle_named_not ~prefix:"group" ~base_marker_class:"group"
    ~combinator:Descendant inner name_opt base_class props

(** Handle peer-not-X modifier. Produces selector with
    :is(:where(.peer):not(...) sibling) pattern. *)
let handle_peer_not_modifier inner name_opt base_class props =
  handle_named_not ~prefix:"peer" ~base_marker_class:"peer"
    ~combinator:Subsequent_sibling inner name_opt base_class props

(** Convert a base modifier to its CSS pseudo-class selector. *)
let pseudo_selector_of_modifier = function
  | Style.Hover -> Css.Selector.Hover
  | Style.Focus -> Css.Selector.Focus
  | Style.Active -> Css.Selector.Active
  | Style.Disabled -> Css.Selector.Disabled
  | Style.Checked -> Css.Selector.Checked
  | Style.First -> Css.Selector.First_child
  | Style.Last -> Css.Selector.Last_child
  | Style.Odd -> Css.Selector.(Nth_child (Odd, None))
  | Style.Even -> Css.Selector.(Nth_child (Even, None))
  | Style.Only -> Css.Selector.Only_child
  | Style.First_of_type -> Css.Selector.First_of_type
  | Style.Last_of_type -> Css.Selector.Last_of_type
  | Style.Only_of_type -> Css.Selector.Only_of_type
  | Style.Visited -> Css.Selector.Visited
  | Style.Target -> Css.Selector.Target
  | Style.Default -> Css.Selector.Default
  | Style.Indeterminate -> Css.Selector.Indeterminate
  | Style.Placeholder_shown -> Css.Selector.Placeholder_shown
  | Style.Autofill -> Css.Selector.Autofill
  | Style.Optional -> Css.Selector.Optional
  | Style.Required -> Css.Selector.Required
  | Style.Valid -> Css.Selector.Valid
  | Style.Invalid -> Css.Selector.Invalid
  | Style.In_range -> Css.Selector.In_range
  | Style.Out_of_range -> Css.Selector.Out_of_range
  | Style.Read_only -> Css.Selector.Read_only
  | Style.Read_write -> Css.Selector.Read_write
  | Style.User_valid -> Css.Selector.User_valid
  | Style.User_invalid -> Css.Selector.User_invalid
  | Style.Enabled -> Css.Selector.Enabled
  | Style.Empty -> Css.Selector.Empty
  | Style.Focus_within -> Css.Selector.Focus_within
  | Style.Focus_visible -> Css.Selector.Focus_visible
  | Style.Open ->
      Css.Selector.(is_ [ attribute "open" Presence; Popover_open; Open ])
  | _ -> Css.Selector.Focus (* fallback *)

(** Build the group-STATE selector rel: :where(.group/name):STATE descendant *)
let named_group_rel name pseudo =
  let open Css.Selector in
  combine
    (compound [ where [ Class ("group/" ^ name) ]; pseudo ])
    Descendant universal

(* [group-X/name] / [peer-X/name]: a state variant scoped to a named anchor. The
   anchor's own [hover] must still gate on the pointer, so the rule carries
   [has_hover] like a plain [group-hover] does. *)
let named_anchor_rule ~anchor ~combinator inner name base_class props =
  let open Css.Selector in
  let modified_class =
    String.concat ""
      [ anchor; "-"; Modifiers.pp_modifier inner; "/"; name; ":"; base_class ]
  in
  let rel =
    combine
      (compound
         [
           where [ Class (anchor ^ "/" ^ name) ];
           pseudo_selector_of_modifier inner;
         ])
      combinator universal
  in
  let sel = compound [ Class modified_class; is_ [ rel ] ] in
  [
    regular ~selector:sel ~props ~base_class:modified_class
      ~has_hover:(Modifiers.is_hover inner) ();
  ]

let handle_named_group inner name base_class props =
  named_anchor_rule ~anchor:"group" ~combinator:Css.Selector.Descendant inner
    name base_class props

let handle_named_peer inner name base_class props =
  named_anchor_rule ~anchor:"peer" ~combinator:Css.Selector.Subsequent_sibling
    inner name base_class props

(* [in-focus]: an ancestor in that state. Same shape as [in-data-X], with the
   state's pseudo-class in place of the attribute. *)
let handle_in_state inner name base_class props =
  let modified_class = "in-" ^ name ^ ":" ^ base_class in
  let sel =
    Css.Selector.combine
      (Css.Selector.Where [ pseudo_selector_of_modifier inner ])
      Css.Selector.Descendant (Css.Selector.Class modified_class)
  in
  [
    (* [in-hover] gates on the pointer just as [hover] itself does. *)
    regular ~selector:sel ~props ~base_class:modified_class ~merge_key:"in"
      ~has_hover:(Modifiers.is_hover inner) ();
  ]

(** Handle not-group-STATE/name compound variant *)
let named_group_modified_class prefix inner name base_class =
  let inner_str = Modifiers.pp_modifier inner in
  prefix ^ "-group-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class

let named_group_rule ~selector_of_rel prefix inner name base_class props =
  let modified_class =
    named_group_modified_class prefix inner name base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let rel = named_group_rel name pseudo in
  let sel = selector_of_rel modified_class rel in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

let handle_not_named_group inner name base_class props =
  let open Css.Selector in
  named_group_rule "not" inner name base_class props
    ~selector_of_rel:(fun modified_class rel ->
      compound [ Class modified_class; Not [ is_ [ rel ] ] ])

(** Handle has-group-STATE/name compound variant *)
let handle_has_named_group inner name base_class props =
  let open Css.Selector in
  named_group_rule "has" inner name base_class props
    ~selector_of_rel:(fun modified_class rel ->
      compound [ Class modified_class; Has [ is_ [ rel ] ] ])

(** Handle in-group-STATE/name compound variant — ancestor pattern *)
let handle_in_named_group inner name base_class props =
  let open Css.Selector in
  named_group_rule "in" inner name base_class props
    ~selector_of_rel:(fun modified_class rel ->
      combine (Where [ is_ [ rel ] ]) Descendant (Class modified_class))

(** Handle group-peer-STATE/name compound variant *)
let handle_group_peer_named inner name base_class props =
  let inner_str = Modifiers.pp_modifier inner in
  let modified_class =
    "group-peer-" ^ inner_str ^ "/" ^ name ^ ":" ^ base_class
  in
  let pseudo = pseudo_selector_of_modifier inner in
  let open Css.Selector in
  let peer_rel =
    combine
      (compound [ where [ Class "peer" ]; pseudo ])
      Subsequent_sibling universal
  in
  let group_rel =
    combine
      (compound [ where [ Class ("group/" ^ name) ]; is_ [ peer_rel ] ])
      Descendant universal
  in
  let sel = compound [ Class modified_class; is_ [ group_rel ] ] in
  [ regular ~selector:sel ~props ~base_class:modified_class () ]

(* Compound [extra] onto the subject of [sel] — its rightmost compound — so a
   variant decorates the element the selector actually matches. *)
let rec attach_to_subject extra sel =
  let open Css.Selector in
  match sel with
  | Combined (a, comb, b) -> Combined (a, comb, attach_to_subject extra b)
  | Relative (comb, b) -> Relative (comb, attach_to_subject extra b)
  | Compound parts -> compound (parts @ extra)
  | other -> compound (other :: extra)

(* Arbitrary selector: [&_p] → .class p. The inner rule may already carry
   structure around its class ([first:] adds a [:first-child]), and that
   structure belongs on whatever element this variant makes the subject: for
   [[&_p]:first:] Tailwind matches the first [p], not a [p] under a first
   child. *)
let arbitrary_selector_rule content base_class selector props =
  let open Css.Selector in
  (* In arbitrary variants, [_] denotes a space. *)
  let s = Parse.decode_underscores content in
  let modified_class = "[" ^ content ^ "]:" ^ base_class in
  (* What the inner rule compounded onto its own class, if that class is still
     the subject. Anything else (an inner variant that moved the subject) has no
     decoration to lift, and the class is rewritten in place instead. *)
  let decoration =
    match selector with
    | Class cls when String.equal cls base_class -> Some []
    | Compound (Class cls :: rest) when String.equal cls base_class -> Some rest
    | _ -> None
  in
  (* With no decoration to lift, an inner variant has moved the subject, and the
     variant's selector belongs at the class's own position inside what that
     inner variant built - not wrapped around the whole of it. *)
  let rebase modified =
    Rules_selector.transform_selector_with_modifier modified base_class
      modified_class selector
  in
  (* Each [&] anchor stands for the utility's own class, so combinators
     ([&>div], [&+p], [&~p]), compounds ([&:hover]) and trailing anchors
     ([input&]) all flatten correctly. *)
  let anchored = Modifiers.nest_selector ~parent:(Class modified_class) s in
  let sel =
    match decoration with
    | Some [] -> anchored
    | Some extra -> attach_to_subject extra anchored
    | None -> rebase anchored
  in
  regular ~selector:sel ~props ~base_class:modified_class ()

(* An at-rule written in brackets: [[@supports(display:grid)]] wraps the utility
   in that query, [[@starting-style]] in [@starting-style]. The class name keeps
   the brackets, so it cannot go through the [supports-] spelling. *)
let at_rule_variant ?(inner_has_hover = false) content ~selector base_class
    props =
  let modified_class = "[" ^ content ^ "]:" ^ base_class in
  let selector =
    Rules_selector.transform_selector_with_modifier
      (Css.Selector.Class modified_class) base_class modified_class selector
  in
  let props, nested = at_rule_body ~inner_has_hover ~selector ~props in
  if content = "@starting-style" then
    starting_style ~selector ~props ~nested ~base_class:modified_class ()
  else
    let cond = String.trim (String.sub content 9 (String.length content - 9)) in
    let condition = normalize_supports_condition cond in
    supports_query ~condition ~selector ~props ~nested
      ~base_class:modified_class ()

(* A [matchVariant]-registered custom variant. The class name is the token
   ([is-data-foo]); the selector is the template with [&] replaced by the
   element's own class, e.g. [&:is([data-foo])] -> [.is-data-foo\:flex:is(...)].
   The canonical optimizer then reduces the single-argument [:is()]. *)
let custom_variant_rule token template base_class props =
  let modified_class = token ^ ":" ^ base_class in
  let sel =
    Modifiers.nest_selector ~parent:(Css.Selector.Class modified_class) template
  in
  regular ~selector:sel ~props ~base_class:modified_class ()

(* Rewrite [selector] for [modifier] and emit a single regular rule under the
   modified class. The common shape for modifiers that only decorate the
   selector (no outer media/supports wrapper). *)

(** Convert a modifier and its context to a CSS rule. [inner_has_hover]
    indicates if the inner rule has a hover modifier that needs to be wrapped in
    CSS nesting with {i \@media (hover:hover)}. *)
let modified_selector_rule modifier base_class selector props =
  let modified_base_selector = Modifiers.to_selector modifier base_class in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector base_class
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector
      base_class modified_class selector
  in
  regular ~selector:new_selector ~props ~base_class:modified_class ()

(* [has-<variant>]: the inner variant's own selector goes inside [:has()]. *)
let route_has_variant inner ~selector base_class props =
  let modified_class =
    "has-" ^ Modifiers.pp_modifier inner ^ ":" ^ base_class
  in
  let modified =
    Css.Selector.compound
      [
        Css.Selector.Class modified_class;
        Css.Selector.Has (extract_not_conditions inner base_class);
      ]
  in
  route_regular ~selector ~base_class ~modified_class ~modified props

(* A prose element variant puts the elements it targets under the utility's own
   class. Substituting for that class rather than rebuilding the selector from
   it keeps what an inner variant already put there: [prose-a:hover:] wants its
   [:hover] on the link, [prose-li:marker:] its [::marker] on the item. *)
let prose_element_rule name ~base_class ~selector props =
  let replacement =
    Modifiers.to_selector (Style.Prose_element name) base_class
  in
  let combined_sel =
    Rules_selector.replace_class_with ~old_class:base_class ~replacement
      selector
  in
  regular ~selector:combined_sel ~props
    ~base_class:("prose-" ^ name ^ ":" ^ base_class)
    ()

let starting_rule ~inner_has_hover base_class selector props =
  let modified_class = "starting:" ^ base_class in
  (* Rebuilding the selector from the bare class would drop what an inner
     variant put on it, as [open:]'s [:is([open], :popover-open, :open)]. *)
  let selector =
    Rules_selector.transform_selector_with_modifier
      (Css.Selector.Class modified_class) base_class modified_class selector
  in
  let props, nested = at_rule_body ~inner_has_hover ~selector ~props in
  starting_style ~selector ~props ~nested ~base_class:modified_class ()

let container_style_rule ~inner_has_hover token condition base_class selector
    props =
  (* A [@custom-variant] whose body is a container query wraps the utility in
     its structural condition. *)
  let modified_class = token ^ ":" ^ base_class in
  let selector =
    Rules_selector.replace_class_in_selector ~old_class:base_class
      ~new_class:modified_class selector
  in
  let props, nested = at_rule_body ~inner_has_hover ~selector ~props in
  container_query ~condition ~selector ~props ~nested ~base_class:modified_class
    ()

let dispatch_modifier ?theme ?(inner_has_hover = false) modifier base_class
    selector props =
  match modifier with
  (* Data modifiers *)
  | Style.Data_state _ | Style.Data_variant _ ->
      route_data_modifier modifier base_class selector props
  | Style.Data_custom _ ->
      modified_selector_rule modifier base_class selector props
  (* Media-like modifiers: dark, motion, contrast, print, orientation,
     forced-colors, inverted-colors, pointer, any-pointer, noscript *)
  | _ when Option.is_some (media_condition_of_modifier modifier) ->
      let condition = Option.get (media_condition_of_modifier modifier) in
      handle_media_like_modifier modifier ~condition ~inner_has_hover base_class
        selector props
  (* Supports feature query *)
  | Style.Supports_property prop ->
      handle_supports_property_modifier ~inner_has_hover prop base_class
        selector props
  | Style.Supports_condition condition_str ->
      handle_supports_condition_modifier ~inner_has_hover condition_str
        base_class selector props
  (* Responsive and container *)
  | Style.Responsive breakpoint ->
      responsive_rule ?theme ~inner_has_hover breakpoint base_class selector
        props
  | Style.Min_responsive breakpoint ->
      min_responsive_rule ?theme ~inner_has_hover breakpoint base_class selector
        props
  | Style.Max_responsive breakpoint ->
      max_responsive_rule ?theme ~inner_has_hover breakpoint base_class selector
        props
  | Style.Min_arbitrary w ->
      min_arbitrary_rule ~inner_has_hover w base_class selector props
  | Style.Max_arbitrary w ->
      max_arbitrary_rule ~inner_has_hover w base_class selector props
  | Style.Min_arbitrary_length l ->
      min_arbitrary_length_rule ~inner_has_hover l base_class selector props
  | Style.Max_arbitrary_length l ->
      max_arbitrary_length_rule ~inner_has_hover l base_class selector props
  | Style.Custom_responsive name ->
      custom_responsive_rule ?theme ~inner_has_hover name base_class selector
        props
  | Style.Min_custom name ->
      min_custom_rule ?theme ~inner_has_hover name base_class selector props
  | Style.Max_custom name ->
      max_custom_rule ?theme ~inner_has_hover name base_class selector props
  | Style.Container query ->
      container_rule ~inner_has_hover query base_class selector props
  (* :not(), :not-bracket, group-not, peer-not — handled in
     apply_modifier_to_rule for multi-rule support *)
  | Style.Not _ | Style.Not_bracket _ | Style.Group_not _ | Style.Peer_not _ ->
      (* Should not be reached — these are handled in apply_modifier_to_rule *)
      regular ~selector ~props ~base_class ()
  (* :has() variants *)
  | Style.Has _ | Style.Group_has _ | Style.Peer_has _ ->
      route_has_modifier modifier ~selector base_class props
  | Style.Has_variant inner ->
      route_has_variant inner ~selector base_class props
  (* Aria bracket and group/peer aria variants *)
  | Style.Aria_bracket _ | Style.Group_aria _ | Style.Peer_aria _
  | Style.Aria_checked | Style.Aria_expanded | Style.Aria_selected
  | Style.Aria_disabled ->
      let modifier =
        match modifier with
        | Style.Aria_checked -> Style.Aria_bracket "checked"
        | Style.Aria_expanded -> Style.Aria_bracket "expanded"
        | Style.Aria_selected -> Style.Aria_bracket "selected"
        | Style.Aria_disabled -> Style.Aria_bracket "disabled"
        | m -> m
      in
      route_aria_modifier modifier ~selector base_class props
  (* Data bracket and group/peer data variants *)
  | Style.Data_bracket _ | Style.Group_data _ | Style.Peer_data _ ->
      route_data_bracket_modifier modifier ~selector base_class props
  (* Starting style - selector includes starting: prefix *)
  | Style.Starting -> starting_rule ~inner_has_hover base_class selector props
  (* Interactive pseudo-classes *)
  | Style.Hover | Style.Focus | Style.Active | Style.Focus_within
  | Style.Focus_visible | Style.Disabled ->
      handle_pseudo_class_modifier ~inner_has_hover modifier base_class selector
        props
  (* Pseudo-elements ::before and ::after - always prepend content property *)
  | Style.Pseudo_before | Style.Pseudo_after ->
      handle_pseudo_element_modifier modifier base_class props
  | Style.Arbitrary_selector content ->
      arbitrary_selector_rule content base_class selector props
  | Style.At_rule content ->
      at_rule_variant ~inner_has_hover content ~selector base_class props
  | Style.Custom_variant (token, template) ->
      custom_variant_rule token template base_class props
  | Style.Container_style (token, condition) ->
      container_style_rule ~inner_has_hover token condition base_class selector
        props
  (* Prose element variants — descendant selector with element filter *)
  | Style.Prose_element name ->
      prose_element_rule name ~base_class ~selector props
  (* Fallback for other modifiers *)
  | _ ->
      handle_fallback_modifier ~inner_has_hover modifier base_class selector
        props

(* An outer modifier must not swallow the [@media (hover:hover)] gate the inner
   [hover:] carries: [disabled:hover:X] and [has-checked:hover:X] keep it. Only
   the routes that build a media query of their own consume the flag. *)
let modifier_to_rule_themed ?theme ?(inner_has_hover = false) modifier
    base_class selector props =
  let rule =
    dispatch_modifier ?theme ~inner_has_hover modifier base_class selector props
  in
  match rule with
  | Output.Regular r when inner_has_hover && not r.has_hover ->
      Output.Regular { r with has_hover = true }
  | rule -> rule

let modifier_to_rule ?inner_has_hover modifier base_class selector props =
  modifier_to_rule_themed ?inner_has_hover modifier base_class selector props

(** Generate pseudo-element rules with separate selectors for browser
    compatibility. An invalid pseudo-element in a comma list causes the entire
    rule to be dropped, so each variant gets its own rule. *)
let pseudo_element_rules ~pseudo_selectors bc props prefix =
  let c = Css.Selector.Class (prefix ^ ":" ^ bc) in
  let mc = prefix ^ ":" ^ bc in
  let open Css.Selector in
  List.map
    (fun sel -> regular ~selector:sel ~props ~base_class:mc ())
    (List.map
       (fun ps -> [ combine c Descendant ps; compound [ c; ps ] ])
       pseudo_selectors
    |> List.concat)

(* An arbitrary selector restructures the rule around the utility's class, which
   [Modifiers.to_selector] has no spelling for, so the variant would be dropped.
   Build the selector the way the non-media path does and keep it inside the
   block. *)
let arbitrary_selector_in_media content bc selector props ~inner_condition
    ~nested =
  match arbitrary_selector_rule content bc selector props with
  | Regular { selector; props; base_class; _ } ->
      [
        media_query ~condition:inner_condition ~selector ~props ?base_class
          ~nested ();
      ]
  | other -> [ other ]

(* An inner media query keeps its own nested blocks - a [hover:] rule carries an
   [@media (hover:hover)] one - and those hold the utility's class too. Rename
   it there as well, or the outer variant drops out of the class name. *)

(** Apply a modifier to a Media_query rule by wrapping it in an outer media
    query. Handles media-like modifiers, responsive modifiers, and falls back to
    returning the rule unchanged. *)
let rec map_selectors_in_stmt f (stmt : Css.statement) =
  let recurse = map_selectors_in_stmt f in
  let open Cascade.Stylesheet in
  match stmt with
  | Rule r ->
      Rule
        { r with selector = f r.selector; nested = List.map recurse r.nested }
  | Media (condition, stmts) -> Media (condition, List.map recurse stmts)
  | Container (name, condition, stmts) ->
      Container (name, condition, List.map recurse stmts)
  | Supports (condition, stmts) -> Supports (condition, List.map recurse stmts)
  | Starting_style stmts -> Starting_style (List.map recurse stmts)
  | other -> other

let rename_class_in_stmt ~old_class ~new_class =
  map_selectors_in_stmt
    (Rules_selector.replace_class_in_selector ~old_class ~new_class)

let apply_modifier_to_media_query ?theme modifier ~inner_condition ~selector
    ~props ~base_class ~nested =
  let bc = Option.value base_class ~default:"" in
  let modified_base_selector = Modifiers.to_selector modifier bc in
  let modified_class =
    Rules_selector.extract_modified_class_name modified_base_selector bc
  in
  let new_selector =
    Rules_selector.transform_selector_with_modifier modified_base_selector bc
      modified_class selector
  in
  let inner_media =
    let rename = rename_class_in_stmt ~old_class:bc ~new_class:modified_class in
    Css.media ~condition:inner_condition
      (Css.rule ~selector:new_selector props :: List.map rename nested)
  in
  let wrap_in_media condition =
    (* Use the modified class (e.g. "dark:md:block") as the wrapped rule's base
       class, not the inner rule's original one ("md:block"), so the sort sees
       the full variant stack and orders it by its highest-order component. *)
    [
      media_query ~condition ~selector:new_selector ~props:[]
        ~base_class:modified_class ~nested:[ inner_media ] ();
    ]
  in
  match media_condition_of_modifier modifier with
  | Some condition -> wrap_in_media condition
  | None -> (
      match modifier with
      | Style.Responsive _ | Style.Min_responsive _ | Style.Max_responsive _
      | Style.Min_arbitrary _ | Style.Max_arbitrary _
      | Style.Min_arbitrary_length _ | Style.Max_arbitrary_length _
      | Style.Custom_responsive _ | Style.Min_custom _ | Style.Max_custom _ ->
          let outer_condition, _ =
            responsive_modifier_condition ?theme modifier
          in
          wrap_in_media outer_condition
      | _ when Modifiers.is_hover modifier ->
          (* hover: gates on hover-capable devices via @media (hover:hover).
             Applied to an inner media block (e.g. dark:), it must nest as
             @media (hover:hover) { @media (dark) { .sel:hover { props } } };
             dropping the wrapper makes the hover style apply on touch. *)
          [
            media_query ~condition:hover_media ~selector:new_selector ~props:[]
              ~base_class:modified_class ~nested:[ inner_media ] ();
          ]
      | Style.Arbitrary_selector content ->
          arbitrary_selector_in_media content bc selector props ~inner_condition
            ~nested
      | _ ->
          (* Other state/pseudo modifiers (focus, active, ...) don't add an
             outer media condition; they rewrite the inner rule's selector so a
             utility whose own output is a media block (e.g. outline-hidden's
             forced-colors reset) gets the modified selector inside the block.
             The base_class is updated to the modified class so this media stays
             grouped with the utility's regular rule (same-utility order),
             rather than being treated as a different utility and reordered. *)
          [
            media_query ~condition:inner_condition ~selector:new_selector ~props
              ~base_class:modified_class ~nested ();
          ])

(* The multi-rule routes below build their selector from the bare class, so an
   outer variant would discard whatever an inner one already did. Rebuild each
   rule they produce around the incoming selector; with no inner variant that
   selector is the bare class and this changes nothing. *)
let rebase_on_selector ~base_class ~selector rules =
  match selector with
  | Css.Selector.Class cls when String.equal cls base_class -> rules
  | _ ->
      List.map
        (fun rule ->
          match rule with
          | Output.Regular ({ base_class = Some modified_class; _ } as r) ->
              Output.Regular
                {
                  r with
                  selector =
                    Rules_selector.transform_selector_with_modifier r.selector
                      base_class modified_class selector;
                }
          | rule -> rule)
        rules

(* Variants whose whole effect is an at-rule around the utility, rather than a
   change to its selector. Applied to a rule that is already a query, they have
   to keep that at-rule and nest the inner query inside it; the selector-rewrite
   arms cannot express them. *)
let wraps_in_at_rule = function
  | Style.Supports_property _ | Style.Supports_condition _ | Style.Starting
  | Style.Container _
  | Style.Not (Style.Supports_property _)
  | Style.Not (Style.Supports_condition _) ->
      true
  | _ -> false

(* Put [inner] inside the at-rule the variant just built. The wrapper is built
   from an empty rule, so its own body is empty and [inner] is all it
   carries. *)
let nest_inside_at_rule inner = function
  | Output.Supports_query ({ nested; _ } as r) ->
      Output.Supports_query { r with nested = inner :: nested }
  | Output.Starting_style ({ nested; _ } as r) ->
      Output.Starting_style { r with nested = inner :: nested }
  | Output.Container_query ({ nested; _ } as r) ->
      Output.Container_query { r with nested = inner :: nested }
  | Output.Media_query ({ nested; _ } as r) ->
      Output.Media_query { r with nested = inner :: nested }
  | other -> other

(* Rewrite every rule in an at-rule's finished body with the same selector
   transformation its outer variant applied to the wrapper's selector. The
   exact-selector case also covers arbitrary selector variants, whose spelling
   cannot be reconstructed through [Modifiers.to_selector]. *)
let rewrite_at_rule_body modifier ~base_class ~selector ~modified_class
    ~modified_selector nested =
  let modified_base_selector =
    if wraps_in_at_rule modifier then None
    else
      match Modifiers.to_selector modifier base_class with
      | selector -> Some selector
      | exception Invalid_argument _ -> None
  in
  let rewrite inner_selector =
    if Css.Selector.equal inner_selector selector then modified_selector
    else
      match modified_base_selector with
      | Some modified_base_selector ->
          Rules_selector.transform_selector_with_modifier modified_base_selector
            base_class modified_class inner_selector
      | None ->
          Rules_selector.replace_class_in_selector ~old_class:base_class
            ~new_class:modified_class inner_selector
  in
  List.map (map_selectors_in_stmt rewrite) nested

(* Extract selector and properties from a single Utility *)
(* Apply modifier to extracted rule *)
let preserve_hover_gate has_hover rules =
  if has_hover then
    List.map
      (function
        | Output.Regular r -> Output.Regular { r with has_hover = true }
        | rule -> rule)
      rules
  else rules

let rec apply_modifier_to_rule ?theme modifier = function
  | Regular { selector; props; base_class; has_hover; _ } ->
      let bc = Option.value base_class ~default:"" in
      let rebase ~selector rules =
        rebase_on_selector ~base_class:bc ~selector rules
      in
      let rules =
        match modifier with
        | Style.Pseudo_marker ->
            let open Css.Selector in
            pseudo_element_rules
              ~pseudo_selectors:[ Marker; Webkit_details_marker ]
              bc props "marker"
        | Style.Pseudo_selection ->
            let open Css.Selector in
            pseudo_element_rules ~pseudo_selectors:[ Selection ] bc props
              "selection"
        | Style.Not inner_modifier -> (
            match inner_modifier with
            | Style.In_bracket content -> handle_not_in_bracket content bc props
            | Style.In_data attr -> handle_not_in_data attr bc props
            | _ -> handle_not_modifier ?theme inner_modifier bc selector props)
        | Style.Not_bracket content ->
            rebase ~selector (handle_not_bracket content bc props)
        | Style.In_bracket content ->
            rebase ~selector (handle_in_bracket content bc props)
        | Style.In_data attr -> rebase ~selector (handle_in_data attr bc props)
        | Style.In_state (inner, name) ->
            rebase ~selector (handle_in_state inner name bc props)
        | Style.Group_not (inner, name_opt) ->
            rebase ~selector (handle_group_not_modifier inner name_opt bc props)
        | Style.Peer_not (inner, name_opt) ->
            rebase ~selector (handle_peer_not_modifier inner name_opt bc props)
        | Style.Named_group (inner, name) ->
            rebase ~selector (handle_named_group inner name bc props)
        | Style.Named_peer (inner, name) ->
            rebase ~selector (handle_named_peer inner name bc props)
        | Style.Not_named_group (inner, name) ->
            rebase ~selector (handle_not_named_group inner name bc props)
        | Style.Has_named_group (inner, name) ->
            rebase ~selector (handle_has_named_group inner name bc props)
        | Style.In_named_group (inner, name) ->
            rebase ~selector (handle_in_named_group inner name bc props)
        | Style.Group_peer_named (inner, name) ->
            rebase ~selector (handle_group_peer_named inner name bc props)
        | _ -> (
            try
              [
                modifier_to_rule_themed ?theme ~inner_has_hover:has_hover
                  modifier bc selector props;
              ]
            with Invalid_argument _ -> [])
      in
      preserve_hover_gate has_hover rules
  | Media_query
      { condition = inner_condition; selector; props; base_class; nested; _ }
    when wraps_in_at_rule modifier ->
      let bc = Option.value base_class ~default:"" in
      let wrappers =
        apply_modifier_to_rule ?theme modifier
          (regular ~selector:(Css.Selector.Class bc) ~props:[] ~base_class:bc ())
      in
      let nest wrapper =
        let modified_class =
          match Output.base_class wrapper with Some c -> c | None -> bc
        in
        let rename =
          rename_class_in_stmt ~old_class:bc ~new_class:modified_class
        in
        let inner_selector =
          Rules_selector.replace_class_in_selector ~old_class:bc
            ~new_class:modified_class selector
        in
        let inner_media =
          Css.media ~condition:inner_condition
            (Css.rule ~selector:inner_selector props :: List.map rename nested)
        in
        nest_inside_at_rule inner_media wrapper
      in
      List.map nest wrappers
  | Media_query
      { condition = inner_condition; selector; props; base_class; nested; _ } ->
      apply_modifier_to_media_query ?theme modifier ~inner_condition ~selector
        ~props ~base_class ~nested
  | ( Supports_query { nested; _ }
    | Container_query { nested; _ }
    | Starting_style { nested; _ } ) as output
    when nested <> [] ->
      apply_modifier_to_nested_output ?theme modifier output
  | Supports_query { condition; selector; props; base_class; merge_key; _ } ->
      apply_modifier_to_supports_query ?theme modifier ~condition ~selector
        ~props ~base_class ~merge_key
  | Container_query { condition; selector; props; base_class; nested } ->
      apply_modifier_to_container_query ?theme modifier ~condition ~selector
        ~props ~base_class ~nested
  | Starting_style { selector; props; base_class; nested } ->
      apply_modifier_to_starting_style ?theme modifier ~selector ~props
        ~base_class ~nested

and apply_modifier_to_nested_output ?theme modifier = function
  | Supports_query { condition; selector; props; base_class; merge_key; nested }
    ->
      apply_nested_at_rule ?theme modifier ~selector ~props ~base_class ~nested
        ~wrap_statement:(fun body -> Css.supports ~condition body)
        ~wrap_output:(fun ~selector ~base_class ~nested ->
          supports_query ~condition ~selector ~props:[] ?base_class ?merge_key
            ~nested ())
  | Container_query { condition; selector; props; base_class; nested } ->
      apply_nested_at_rule ?theme modifier ~selector ~props ~base_class ~nested
        ~wrap_statement:(fun body -> Css.container ~condition body)
        ~wrap_output:(fun ~selector ~base_class ~nested ->
          container_query ~condition ~selector ~props:[] ?base_class ~nested ())
  | Starting_style { selector; props; base_class; nested } ->
      apply_nested_at_rule ?theme modifier ~selector ~props ~base_class ~nested
        ~wrap_statement:Css.starting_style
        ~wrap_output:(fun ~selector ~base_class ~nested ->
          starting_style ~selector ~props:[] ?base_class ~nested ())
  | _ -> invalid_arg "nested at-rule output"

(* A hover-gated at-rule has a finished body in [nested] and no declarations on
   the wrapper itself. Apply the outer variant to a plain rule to obtain its
   selector/query, rewrite the finished body with that same selector change,
   then put the original at-rule at the correct depth. *)
and apply_nested_at_rule ?theme modifier ~selector ~props ~base_class ~nested
    ~wrap_statement ~wrap_output =
  let bc = Option.value base_class ~default:"" in
  apply_modifier_to_rule ?theme modifier
    (regular ~selector ~props ?base_class ())
  |> List.map (fun outer ->
      let modified_selector, modified_props, modified_base_class =
        match outer with
        | Regular { selector; props; base_class; _ }
        | Media_query { selector; props; base_class; _ }
        | Container_query { selector; props; base_class; _ }
        | Starting_style { selector; props; base_class; _ }
        | Supports_query { selector; props; base_class; _ } ->
            (selector, props, base_class)
      in
      let modified_class = Option.value modified_base_class ~default:bc in
      let nested =
        rewrite_at_rule_body modifier ~base_class:bc ~selector ~modified_class
          ~modified_selector nested
      in
      let body =
        (if modified_props = [] then []
         else [ Css.rule ~selector:modified_selector modified_props ])
        @ nested
      in
      let inner = wrap_statement body in
      match outer with
      | Regular { has_hover = true; _ } ->
          media_query ~condition:hover_media ~selector:modified_selector
            ~props:[] ?base_class:modified_base_class ~nested:[ inner ] ()
      | Regular _ ->
          wrap_output ~selector:modified_selector
            ~base_class:modified_base_class ~nested:body
      | Media_query ({ nested; _ } as r) ->
          Media_query { r with props = []; nested = inner :: nested }
      | (Container_query _ | Starting_style _ | Supports_query _) as wrapper ->
          nest_inside_at_rule inner wrapper)

(* Apply a modifier to a [@container] rule, the way the [@supports] case does:
   run it on the inner rule so all the selector and media machinery applies,
   then put each result back inside [@container]. Without this an outer variant
   fell through and vanished -- [sm:@max-md:X] lost its breakpoint entirely. *)
and apply_modifier_to_container_query ?theme modifier ~condition ~selector
    ~props ~base_class ~nested =
  let inner = regular ~selector ~props ?base_class ~nested () in
  let contained sel p = Css.container ~condition [ Css.rule ~selector:sel p ] in
  apply_modifier_to_rule ?theme modifier inner
  |> List.map (function
    | Regular { selector; props; base_class; has_hover; nested; _ } ->
        if has_hover then
          media_query ~condition:hover_media ~selector ~props:[] ?base_class
            ~nested:(contained selector props :: nested)
            ()
        else container_query ~condition ~selector ~props ?base_class ~nested ()
    | Media_query { condition = outer; selector; props; base_class; nested; _ }
      ->
        (* Tailwind keeps the outer query outside and nests the container. *)
        media_query ~condition:outer ~selector ~props:[] ?base_class
          ~nested:(contained selector props :: nested)
          ()
    | Container_query { condition = outer; selector; props; base_class; nested }
      ->
        container_query ~condition:outer ~selector ~props:[] ?base_class
          ~nested:(contained selector props :: nested)
          ()
    | other -> other)

(* Apply a modifier to a [@supports] rule (the progressive-enhancement block an
   opacity color or gradient emits). The modifier is applied to the inner rule
   as if it were a plain rule, reusing all the modifier machinery (selector
   rewriting, hover/responsive media, ...); each result is then re-wrapped in
   [@supports]. Without this the block would keep the base class and leak a bare
   rule outside the variant. *)
and apply_modifier_to_supports_query ?theme modifier ~condition ~selector ~props
    ~base_class ~merge_key =
  let inner = regular ~selector ~props ?base_class ?merge_key () in
  let wrap_supports_in_media outer sel p bc nested =
    (* Nest the @supports inside a media query so the block stays scoped to the
       variant. *)
    let supports_stmt = Css.supports ~condition [ Css.rule ~selector:sel p ] in
    media_query ~condition:outer ~selector:sel ~props:[] ?base_class:bc
      ~nested:(supports_stmt :: nested) ()
  in
  let wrap_supports_in_at_rule = function
    | Container_query ({ selector; props; nested; _ } as r) ->
        let supports_stmt =
          Css.supports ~condition [ Css.rule ~selector props ]
        in
        Container_query { r with props = []; nested = supports_stmt :: nested }
    | Supports_query ({ selector; props; nested; _ } as r) ->
        let supports_stmt =
          Css.supports ~condition [ Css.rule ~selector props ]
        in
        Supports_query { r with props = []; nested = supports_stmt :: nested }
    | Starting_style ({ selector; props; nested; _ } as r) ->
        let supports_stmt =
          Css.supports ~condition [ Css.rule ~selector props ]
        in
        Starting_style { r with props = []; nested = supports_stmt :: nested }
    | other -> other
  in
  apply_modifier_to_rule ?theme modifier inner
  |> List.map (function
    | Regular { selector; props; base_class; has_hover; _ } ->
        (* A bare hover: rule carries [has_hover] instead of an outer media;
           wrap the @supports in @media (hover:hover) to match. *)
        if has_hover then
          wrap_supports_in_media hover_media selector props base_class []
        else
          (* Keep the variant's own order: without it the block sorts before the
             rule it enhances, and the plain fallback wins instead. *)
          supports_query ~condition ~selector ~props ?base_class ?merge_key ()
    | Media_query { condition = outer; selector; props; base_class; nested; _ }
      ->
        wrap_supports_in_media outer selector props base_class nested
    | (Container_query _ | Supports_query _ | Starting_style _) as outer ->
        wrap_supports_in_at_rule outer)

(* An outer variant on a [@starting-style] rule, the way the [@supports] case
   does it: run the modifier on the inner rule so all the selector and media
   machinery applies, then put each result back inside the at-rule. With no arm
   of its own the rule fell through the catch-all and the variant went with it,
   so [hover:starting:p-4] named its rule [.starting\:p-4] and matched nothing
   in the markup. *)
and apply_modifier_to_starting_style ?theme modifier ~selector ~props
    ~base_class ~nested =
  let inner = regular ~selector ~props ?base_class ~nested () in
  let started sel p = Css.starting_style [ Css.rule ~selector:sel p ] in
  apply_modifier_to_rule ?theme modifier inner
  |> List.map (function
    | Regular { selector; props; base_class; has_hover; nested; _ } ->
        (* A bare hover: rule carries [has_hover] rather than an outer media, so
           the at-rule goes inside [@media (hover:hover)] to match. *)
        if has_hover then
          media_query ~condition:hover_media ~selector ~props:[] ?base_class
            ~nested:(started selector props :: nested)
            ()
        else starting_style ~selector ~props ?base_class ~nested ()
    | Media_query { condition = outer; selector; props; base_class; nested; _ }
      ->
        media_query ~condition:outer ~selector ~props:[] ?base_class
          ~nested:(started selector props :: nested)
          ()
    | other -> other)

(* Handle Modified style by recursively extracting and applying modifier *)
let handle_modified ?theme util_inner modifier base_style extract_fn =
  (* Strip the outermost modifier if it matches the one being applied, to avoid
     doubled prefixes like .focus\:focus\:ring. But preserve inner modifiers for
     nested cases like dark [ aria_selected [...] ] ->
     .dark\:aria-selected\:... *)
  let inner_util, style =
    match util_inner with
    | Utility.Modified (inner_mod, u)
      when Style.equal_modifier inner_mod modifier ->
        (* Same modifier - strip it to avoid doubling *)
        (u, base_style)
    | _ ->
        (* Different or no modifier - preserve it *)
        (util_inner, base_style)
  in
  let base_class_name = Utility.to_class inner_util in
  let base_rules = extract_fn base_class_name inner_util style in
  let expanded = List.map (apply_modifier_to_rule ?theme modifier) base_rules in
  match modifier with
  | Style.Pseudo_marker | Style.Pseudo_selection ->
      (* These variants expand one source rule into several browser-safe
         selectors. Keep each selector's declarations and nested fallback query
         together: Tailwind writes [fallback; @supports] for one marker before
         moving to the next, rather than all fallbacks followed by all
         enhancements. *)
      let rec interleave rows =
        let heads, tails =
          List.fold_right
            (fun row (heads, tails) ->
              match row with
              | [] -> (heads, tails)
              | head :: tail -> (head :: heads, tail :: tails))
            rows ([], [])
        in
        match heads with [] -> [] | _ -> heads @ interleave tails
      in
      interleave expanded
  | _ -> List.concat expanded

(* Handle Group style by extracting each item *)
let handle_group class_name util_inner styles extract_fn =
  match util_inner with
  | Utility.Group util_items ->
      let extract_item style_item util_item =
        let class_name_item = Utility.to_class util_item in
        extract_fn class_name_item util_item style_item
      in
      List.map2 extract_item styles util_items |> List.concat
  | _ -> List.concat_map (extract_fn class_name util_inner) styles

(** Replace placeholder selector "_" with the actual utility class selector.
    Matches the node rather than rendering the selector to compare it: this runs
    per rule for every utility carrying an explicit [rule_list] (prose, forms,
    gradients). *)
let resolve_placeholder_selector sel selector =
  match selector with Css.Selector.Class "_" -> sel | _ -> selector

let extract_rule_outputs build_output statements =
  statements
  |> List.filter_map (fun inner ->
      match Css.as_rule inner with
      | Some (selector, declarations, _) ->
          Some (build_output selector declarations)
      | None -> None)

(** Extract outputs from a [@media] statement's inner rules *)
let extract_media_outputs ~class_name ~sel condition statements =
  extract_rule_outputs
    (fun selector declarations ->
      let actual_selector = resolve_placeholder_selector sel selector in
      media_query ~condition ~selector:actual_selector ~props:declarations
        ~base_class:class_name ())
    statements

(** Extract outputs from a [@container] statement's inner rules *)
let extract_container_outputs ~class_name condition statements =
  extract_rule_outputs
    (fun selector declarations ->
      container_query ~condition ~selector ~props:declarations
        ~base_class:class_name ())
    statements

(** Extract outputs from a [@supports] statement's inner rules *)
let extract_supports_outputs ~class_name ~sel ?merge_key condition statements =
  extract_rule_outputs
    (fun selector declarations ->
      let actual_selector = resolve_placeholder_selector sel selector in
      supports_query ~condition ~selector:actual_selector ~props:declarations
        ~base_class:class_name ?merge_key ())
    statements

(** Process a non-nested statement from a rule_list, returning outputs. Sets
    [has_regular_rules] to true when a regular rule is found. *)
let process_rule_list_stmt ~sel ~class_name ?merge_key ~has_regular_rules stmt =
  match Css.as_rule stmt with
  | Some (selector, declarations, nested) ->
      has_regular_rules := true;
      let actual_selector = resolve_placeholder_selector sel selector in
      Some
        [
          regular ~selector:actual_selector ~props:declarations
            ~base_class:class_name ~nested ?merge_key ();
        ]
  | None -> (
      match Css.as_media stmt with
      | Some (condition, statements) ->
          Some (extract_media_outputs ~class_name ~sel condition statements)
      | None -> (
          match Css.as_container stmt with
          | Some (_, Some condition, statements) ->
              Some (extract_container_outputs ~class_name condition statements)
          | Some (_, None, _) -> None
          | None -> (
              match Css.as_supports stmt with
              | Some (condition, statements) ->
                  Some
                    (extract_supports_outputs ~class_name ~sel ?merge_key
                       condition statements)
              | None -> None)))

(* Process a Style with a rule_list into output rules. Processes rule_list items
   in order, preserving the original interleaving of Regular rules, @supports
   blocks, and @media queries.

   @supports blocks become separate [Supports_query] entries so they sort
   independently and don't prevent the base rule from being combined by the
   optimizer. @media blocks that appear at the top level of rule_list are
   collected and nested on the base rule (they represent modifier-based media
   that must stay grouped with the utility). *)
let extract_style_with_rules ~sel ~class_name ?merge_key ~props rule_list =
  (* Collect top-level @media for nesting on the base rule *)
  let nested_media = rule_list |> List.filter Css.is_nested_media in
  (* Process rule_list items in order, preserving interleaving *)
  let has_regular_rules = ref false in
  let ordered_entries =
    rule_list
    |> List.concat_map (fun stmt ->
        if Css.is_nested_media stmt then
          (* @media stays nested on base rule, skip here *)
          []
        else if Css.is_nested_supports stmt then
          (* @supports → hoist to Supports_query entries *)
          match Css.as_supports stmt with
          | Some (condition, statements) ->
              extract_supports_outputs ~class_name ~sel ?merge_key condition
                statements
          | None -> []
        else
          match
            process_rule_list_stmt ~sel ~class_name ?merge_key
              ~has_regular_rules stmt
          with
          | Some entries -> entries
          | None -> [])
  in
  (* Base rule with props and nested @media *)
  let base_rule =
    if props = [] && nested_media = [] then []
    else
      [
        regular ~selector:sel ~props ~base_class:class_name ~nested:nested_media
          ?merge_key ();
      ]
  in
  (* If the rule_list contains regular rules (forms plugin, gradient utilities),
     they come first so custom selectors precede the base props rule. Otherwise,
     base comes first to maintain fallback → @supports cascade order. *)
  if !has_regular_rules then ordered_entries @ base_rule
  else base_rule @ ordered_entries

let outputs ?(theme = Scheme.default) ?order_tbl util =
  let rec utility_order = function
    | Utility.Base b -> Some (Utility.order b)
    | Utility.Modified (_, u) | Utility.Important (_, u) | Utility.Aliased (_, u)
      ->
        utility_order u
    | Utility.Group _ -> None
  in
  let rec extract_with_class class_name util_inner = function
    | Style.Style { props; rules; merge_key; pseudo_suffix; _ } -> (
        (* Record the base utility's order under the class name we already
           built, so the caller does not have to re-derive it from the
           string. *)
        (match (order_tbl, utility_order util_inner) with
        | Some tbl, Some order ->
            if not (Hashtbl.mem tbl class_name) then
              Hashtbl.add tbl class_name order
        | _ -> ());
        let sel =
          match pseudo_suffix with
          | None -> Css.Selector.Class class_name
          | Some pseudo ->
              Css.Selector.compound [ Css.Selector.Class class_name; pseudo ]
        in
        match rules with
        | None ->
            if props = [] then []
            else
              [
                regular ~selector:sel ~props ~base_class:class_name ?merge_key
                  ();
              ]
        | Some rule_list ->
            extract_style_with_rules ~sel ~class_name ?merge_key ~props
              rule_list)
    | Style.Modified (modifier, base_style) ->
        handle_modified ~theme util_inner modifier base_style extract_with_class
    | Style.Group styles ->
        handle_group class_name util_inner styles extract_with_class
  in
  let class_name = Utility.to_class util in
  let style = Utility.to_style theme util in
  extract_with_class class_name util style
