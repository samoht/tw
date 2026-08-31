(** CSS rule cascade sort order

    Provides the [indexed_rule] type and [compare_indexed_rules], the comparison
    function used to sort assembled CSS rules into Tailwind v4 cascade order. *)

module Css = Cascade.Css

(* ======================================================================== *)
(* Types *)
(* ======================================================================== *)

(** Classification of CSS selectors for ordering purposes *)
type selector_kind =
  | Simple  (** Plain class selector like .foo *)
  | Pseudo_element
      (** Selector with pseudo-element like .before:absolute::before *)
  | Complex of {
      has_focus : bool;
      has_focus_within : bool;
      has_focus_visible : bool;
      has_group : bool;  (** group-* without :has() like group-focus *)
      has_peer : bool;  (** peer-* without :has() like peer-checked *)
      has_group_has : bool;
          (** group-* with :has() like group-has-[:checked] *)
      has_peer_has : bool;  (** peer-* with :has() like peer-has-[:checked] *)
      has_standalone_has : bool;
      has_aria : bool;
    }  (** Selector with combinators, pseudo-classes, etc. *)

(* One component of a rule's variant sort key: the slot the token sorts in, plus
   what separates two tokens that share it. A breakpoint token carries the width
   it names, so [sm] and [md] no longer collapse onto one key; a group-/ peer-
   token carries the state it wraps, so [group-focus] and [group-has] keep their
   order. *)
type variant_component = {
  slot : int;
  breakpoint : Css.Media.key option;
  wrapped : int;
}

(** Relationship between two rules being compared *)
type rule_relationship =
  | Same_utility of string  (** Both rules from same base utility *)
  | Different_utilities  (** Rules from different utilities *)

type indexed_rule = {
  index : int;
  rule_type :
    [ `Regular
    | `Media of Css.Media.t
    | `Container of Css.Container.t
    | `Starting
    | `Supports of Css.Supports.t ];
  selector : Css.Selector.t;
  selector_str : string;
  selector_kind : selector_kind;
  has_modifier_colon : bool;
  props : Css.declaration list;
  declared : bool;
      (* The rule came in as finished CSS from a project's own [@utility]. *)
  declaration_count : int;
      (* How many declarations the rule emits, those of its nested rules
         included. Tailwind breaks a tie between two rules that write the same
         property by this, widest first, so the narrower one wins the cascade.
         Read only when one side is [declared]: the built-ins of a family are
         already separated by their suborders, and tw writes vendor-prefixed
         spellings Tailwind leaves to its optimizer, so counting them against
         each other would move rules the corpus pins. *)
  order : int * int;
  nested : Css.statement list;
  base_class : string option;
  merge_key : string option;
  variant_order : int;
  variant_key : string * int;
      (* Precomputed (variant prefix, effective inner order) - see
         [variant_sort_key]. Read by [compare_variant_ordered]. *)
  variant_orders : variant_component list;
      (* The rule's variant order keys sorted descending - see
         [variant_order_list]. Compared lexicographically by
         [compare_variant_ordered] so a stacked variant sorts into the group of
         its highest-order component, after that group's base rules, and two
         stacks with the same variant multiset (e.g. group:hover and
         hover:group) get identical keys. *)
  base_class_key : string;
      (* The rule's base class ("" when it has none), read by
         [compare_by_base_class] as the lexicographic sort key. *)
  media_key : Css.Media.key option;
      (* Precomputed sort key of the rule's own media condition (the [`Media]
         case of [rule_type]); [None] otherwise. Lets media comparisons use
         [Css.Media.compare_keys] instead of re-serializing the query. *)
  nested_media_key : Css.Media.key option;
      (* Precomputed sort key of a single nested media condition. *)
  responsive_media_key : Css.Media.key option;
      (* Precomputed sort key of the rule's breakpoint condition, taken from
         whichever nesting level carries it - see [responsive_media_key]. *)
}
(** An indexed CSS rule ready for sorting. [index] preserves source order;
    [order] is the [(priority, suborder)] pair from the utility definition;
    [variant_order] places modifier-prefixed rules after their base
    counterparts. *)

(* ======================================================================== *)
(* Debug *)
(* ======================================================================== *)

(* The comparator's tracing, driven by [TW_DEBUG_SORT]. Nothing called the
   setter this used to expose, so the flag was always false and every trace
   below it was unreachable in any build - the diagnostic that sort work most
   wants was dead. An environment variable makes it usable without a recompile
   and without an API nobody calls. *)
let debug_compare = Sys.getenv_opt "TW_DEBUG_SORT" <> None
let debug_compare_enabled () = debug_compare

(* ======================================================================== *)
(* Selector Classification *)
(* ======================================================================== *)

let is_simple_class_selector sel =
  match sel with Css.Selector.Class _ -> true | _ -> false

(** Compare complex selector kinds. Returns ordering value for sorting. At equal
    priority levels, the order is: simple/complex < pseudo-element < group <
    group-has < peer < peer-has < focus-within < focus-visible < has < aria *)
let complex_selector_order = function
  | Complex { has_aria = true; _ } -> 60
  | Complex { has_standalone_has = true; _ } -> 50
  | Complex { has_focus_visible = true; _ } -> 40
  | Complex { has_focus_within = true; _ } -> 30
  | Complex { has_peer_has = true; _ } -> 21
  | Complex { has_peer = true; _ } -> 20
  | Complex { has_group_has = true; _ } -> 11
  | Complex { has_group = true; _ } -> 10
  | Pseudo_element -> 5 (* After simple/complex but before late modifiers *)
  | Simple -> 0
  | Complex _ -> 0

(** Determine the relationship between two rules *)
let rule_relationship r1 r2 =
  match (r1.base_class, r2.base_class) with
  | Some bc1, Some bc2 when bc1 = bc2 -> Same_utility bc1
  | _ -> Different_utilities

(** Traverse a selector tree like [Css.Selector.any] but skip [Not] children.
    This prevents :not(:focus) from being classified as a focus modifier, which
    would break ordering for not-* variant rules. *)
let rec any_outside_not p = function
  | Css.Selector.Not _ -> false
  | Css.Selector.Compound xs -> List.exists (any_outside_not p) xs
  | Css.Selector.Combined (a, _, b) ->
      any_outside_not p a || any_outside_not p b
  | Css.Selector.Relative (_, b) -> any_outside_not p b
  | Css.Selector.List xs -> List.exists (any_outside_not p) xs
  | Css.Selector.Is xs | Css.Selector.Where xs | Css.Selector.Has xs ->
      List.exists (any_outside_not p) xs
  | s -> p s

(** Classify a selector into Simple or Complex with focus/has analysis. For List
    selectors (merged selectors like `.a, .b`), classify based on the first
    element to preserve sort order. *)
let classify_selector sel =
  let sel_to_classify =
    match Css.Selector.as_list sel with
    | Some (first :: _) -> first
    | Some [] -> sel
    | None -> sel
  in
  if is_simple_class_selector sel_to_classify then Simple
  else if Css.Selector.has_pseudo_element sel_to_classify then Pseudo_element
  else
    let has_has_pseudo s =
      any_outside_not (function Css.Selector.Has _ -> true | _ -> false) s
    in
    let has_aria_attr s =
      any_outside_not
        (function
          | Css.Selector.Attribute (_, Css.Selector.Aria _, _, _) -> true
          | _ -> false)
        s
    in
    let has_group = Css.Selector.has_group_marker sel_to_classify in
    let has_peer = Css.Selector.has_peer_marker sel_to_classify in
    let has_has = has_has_pseudo sel_to_classify in
    Complex
      {
        has_focus =
          any_outside_not
            (function Css.Selector.Focus -> true | _ -> false)
            sel_to_classify;
        has_focus_within =
          any_outside_not
            (function Css.Selector.Focus_within -> true | _ -> false)
            sel_to_classify;
        has_focus_visible =
          any_outside_not
            (function Css.Selector.Focus_visible -> true | _ -> false)
            sel_to_classify;
        has_group = has_group && not has_has;
        has_peer = has_peer && not has_has;
        has_group_has = has_group && has_has;
        has_peer_has = has_peer && has_has;
        has_standalone_has = has_has && (not has_group) && not has_peer;
        has_aria = has_aria_attr sel_to_classify;
      }

(** Get sort key for preference media conditions. Tailwind order: reduced-motion
    (no-preference, reduce) < contrast (more, less) *)
let preference_condition_order cond = Css.Media.preference_order cond

(** Count modifier colons in a selector's first class name. Used to determine
    modifier stacking depth for hover media interleaving. *)
let selector_modifier_depth sel =
  match Css.Selector.first_class sel with
  | Some cls ->
      String.fold_left (fun acc c -> if c = ':' then acc + 1 else acc) 0 cls
  | None -> 0

(* Shares [any_outside_not]'s traversal: the hand-rolled one missed [Has] and
   [Relative], so :hover inside a :has() did not count. Both want the same
   thing, a :hover that is not under a :not(). *)

(** Check if a selector contains :hover pseudo-class at any depth (used to
    detect compound variants like group-hocus that combine hover+focus). *)
let selector_has_hover sel =
  any_outside_not (function Css.Selector.Hover -> true | _ -> false) sel

(* Determine sort group for rule types. Regular and Media are grouped together
   to preserve utility grouping - media queries appear immediately after their
   base utility rule. *)
let rule_type_order = function
  | `Regular -> 0
  | `Media _ -> 0 (* Same as Regular to keep grouped *)
  | `Supports _ -> 0 (* Same as Regular to keep grouped with base rule *)
  | `Container _ -> 1
  | `Starting -> 2

(* Extract media sort key using Css.Media.kind and group_order. Returns (group,
   subkey) where subkey is rem value for responsive conditions. *)
let extract_media_sort_key = function
  | `Media cond -> Css.Media.group_order (Css.Media.kind cond)
  | _ -> (0, 0.)

(* Tailwind orders container variants exactly like breakpoints, and every
   container condition it emits is a width range wrapped in an optional
   container name and an optional negation. Project one onto the media query it
   is equivalent to, so the breakpoint ordering applies unchanged: [@max-sm] is
   a negated lower bound, which is the [not all and (...)] shape media already
   classifies as an upper bound. The name plays no part -- Tailwind interleaves
   a named container with the unnamed ones at its width. *)
let rec container_media_projection (c : Css.Container.t) =
  match c with
  | Css.Container.Named (_, inner) -> container_media_projection inner
  | Css.Container.Feature_query q -> Some q
  | Css.Container.Not inner -> (
      match container_media_projection inner with
      | Some (Css.Media.Cond cond) ->
          Some
            (Css.Media.Type
               {
                 prefix = Some Css.Media.Not;
                 type_ = Css.Media.All;
                 trailing = Some cond;
               })
      | _ -> None)
  | Css.Container.Min_width_rem _ | Css.Container.Min_width_px _
  | Css.Container.And _ | Css.Container.Or _ | Css.Container.Style _
  | Css.Container.Scroll_state _ ->
      None

(* Precompute the sort keys for a rule's own and nested media conditions, so the
   comparators use [Css.Media.compare_keys] (cheap) instead of re-serializing
   the query on every comparison. See [media_key]/[nested_media_key]. *)
let media_sort_keys rule_type nested =
  let media_key =
    match rule_type with
    | `Media c -> Some (Css.Media.sort_key c)
    | `Container c ->
        Stdlib.Option.map Css.Media.sort_key (container_media_projection c)
    | _ -> None
  in
  let nested_media_key =
    match nested with
    | [ n ] -> (
        match Css.as_media n with
        | Some (c, _) -> Some (Css.Media.sort_key c)
        | None -> None)
    | _ -> None
  in
  (media_key, nested_media_key)

(* The breakpoint a rule sorts under, wherever its variant stack puts it:
   sm:hover: writes the breakpoint outside the hover query and hover:sm: writes
   it inside, and Tailwind groups both under that breakpoint. *)
let responsive_media_key rule_type nested =
  let of_cond c =
    match Css.Media.kind c with
    | Css.Media.Responsive _ | Css.Media.Responsive_max _ ->
        Some (Css.Media.sort_key c)
    | _ -> None
  in
  match rule_type with
  | `Media c when Stdlib.Option.is_some (of_cond c) -> of_cond c
  | _ -> (
      match nested with
      | [ n ] -> (
          match Css.as_media n with Some (c, _) -> of_cond c | None -> None)
      | _ -> None)

(* ======================================================================== *)
(* Priority Comparison *)
(* ======================================================================== *)

(** Compare two simple selectors by suborder, then alphabetically, then index.
    Index fallback is critical for utilities like prose that emit multiple rules
    with the same selector - preserves original order. *)
let compare_simple_selectors sel_str1 sel_str2 s1 s2 i1 i2 =
  let sub_cmp = Int.compare s1 s2 in
  if sub_cmp <> 0 then sub_cmp
  else
    let sel_cmp = String.compare sel_str1 sel_str2 in
    if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare two complex selectors by kind, then selector string (for aria), then
    suborder, then index. Order: focus < group-has < peer-has < has < aria. For
    aria selectors, sort by attribute name before property to match Tailwind. *)
let compare_complex_selectors sel_str1 sel_str2 kind1 kind2 s1 s2 i1 i2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2
  else if match kind1 with Complex { has_aria = true; _ } -> true | _ -> false
  then
    (* Both are aria selectors - compare by selector string (aria attribute)
       before suborder (property shade) to match Tailwind v4 behavior *)
    let sel_cmp = String.compare sel_str1 sel_str2 in
    if sel_cmp <> 0 then sel_cmp
    else
      let sub_cmp = Int.compare s1 s2 in
      if sub_cmp <> 0 then sub_cmp else Int.compare i1 i2
  else
    (* Other complex selectors - use suborder first *)
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let sel_cmp = String.compare sel_str1 sel_str2 in
      if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare rules by priority, then suborder, then by selector kind. Uses
    type-directed dispatch based on selector classification. At the same
    priority/suborder, cross-kind comparisons preserve source order (index) to
    match tailwindcss output exactly. *)
let compare_by_priority_suborder_alpha kind1 kind2 sel_str1 sel_str2 (p1, s1)
    (p2, s2) i1 i2 =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      match (kind1, kind2) with
      | Simple, Simple -> compare_simple_selectors sel_str1 sel_str2 s1 s2 i1 i2
      | Pseudo_element, Pseudo_element ->
          compare_simple_selectors sel_str1 sel_str2 s1 s2 i1 i2
      | Pseudo_element, Simple -> Int.compare i1 i2
      | Pseudo_element, Complex _ ->
          (* Prose rules need pseudo-elements after complex selectors *)
          Int.compare i1 i2
      | Simple, Pseudo_element -> Int.compare i1 i2
      | Simple, Complex _ -> Int.compare i1 i2
      | Complex _, Pseudo_element -> Int.compare i1 i2
      | Complex _, Simple -> Int.compare i1 i2
      | Complex _, Complex _ ->
          compare_complex_selectors sel_str1 sel_str2 kind1 kind2 s1 s2 i1 i2

(* ======================================================================== *)
(* Media Query Comparison *)
(* ======================================================================== *)

(* The two groups below are compared on something other than their key, so they
   need naming. Taken from [Css.Media.group_order] rather than written out, so
   they follow it if it moves. *)
let responsive_group =
  fst (Css.Media.group_order (Css.Media.Responsive (0, 0.)))

let accessibility_preference_group =
  fst (Css.Media.group_order Css.Media.Preference_accessibility)

(* The rank the variant table gives a breakpoint prefix. Read back from the
   table for the same reason as the two groups above. *)
let responsive_variant_order = Modifiers.variant_order_of_prefix "sm"

(** Compare two media conditions within the same group *)
let compare_media_conditions group1 sub1 sub2 cond1 cond2 key1 key2 =
  if group1 = responsive_group then Float.compare sub1 sub2
  else if group1 = accessibility_preference_group then
    match (cond1, cond2) with
    | Some c1, Some c2 ->
        Int.compare
          (preference_condition_order c1)
          (preference_condition_order c2)
    | _ -> 0
  else
    match (key1, key2) with
    | Some k1, Some k2 -> Css.Media.compare_keys k1 k2
    | _ -> 0

(* The [order] field is a [(priority, suborder)] pair; comparing it with the
   polymorphic [compare] boxes both ints on every call, and the comparator runs
   once per rule pair. *)
let compare_order (p1, s1) (p2, s2) =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp else Int.compare s1 s2

(* For hover media, separate rules by modifier depth so that single-modifier
   hover rules (group-hover:flex) form a separate block from stacked hover rules
   (group-focus:group-hover:flex) *)
let compare_hover_depth cond1 cond2 sel1 sel2 =
  match (cond1, cond2) with
  | Some c1, Some c2
    when Css.Media.kind c1 = Css.Media.Hover
         && Css.Media.kind c2 = Css.Media.Hover ->
      Int.compare (selector_modifier_depth sel1) (selector_modifier_depth sel2)
  | _ -> 0

(* Compare by nested media condition when both have nested media. This sorts
   stacked min/max variants like min-sm:max-xl vs min-sm:max-lg by the inner
   media condition. *)
let compare_nested_media_cond nk1 nk2 =
  match (nk1, nk2) with
  | Some k1, Some k2 -> Css.Media.compare_keys k1 k2
  | _ -> 0

let compare_same_media_group (r1 : indexed_rule) (r2 : indexed_rule) cond1 cond2
    =
  let depth_cmp = compare_hover_depth cond1 cond2 r1.selector r2.selector in
  if depth_cmp <> 0 then depth_cmp
  else
    let nested_media_cmp =
      compare_nested_media_cond r1.nested_media_key r2.nested_media_key
    in
    if nested_media_cmp <> 0 then nested_media_cmp
    else
      let same_utility =
        match (r1.base_class, r2.base_class) with
        | Some b1, Some b2 -> String.equal b1 b2
        | _ -> false
      in
      if same_utility then
        let order_cmp = compare_order r1.order r2.order in
        if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index
      else
        compare_by_priority_suborder_alpha r1.selector_kind r2.selector_kind
          r1.selector_str r2.selector_str r1.order r2.order r1.index r2.index

let compare_media_rules (r1 : indexed_rule) (r2 : indexed_rule) =
  let nested_cmp = Bool.compare (r1.nested <> []) (r2.nested <> []) in
  if nested_cmp <> 0 then nested_cmp
  else
    let group1, sub1 = extract_media_sort_key r1.rule_type in
    let group2, sub2 = extract_media_sort_key r2.rule_type in
    let key_cmp = Int.compare group1 group2 in
    if key_cmp <> 0 then key_cmp
    else
      let cond1 = match r1.rule_type with `Media c -> Some c | _ -> None in
      let cond2 = match r2.rule_type with `Media c -> Some c | _ -> None in
      let cond_cmp =
        compare_media_conditions group1 sub1 sub2 cond1 cond2 r1.media_key
          r2.media_key
      in
      if cond_cmp <> 0 then cond_cmp
      else compare_same_media_group r1 r2 cond1 cond2

(* ======================================================================== *)
(* Regular vs Media Comparison *)
(* ======================================================================== *)

(* For the same base utility, preserve original order (index) to keep media
   rules adjacent to their related state rules. *)
let compare_same_utility_regular_media r1 r2 = Int.compare r1.index r2.index

(** Check if a selector has special modifiers that should come at the end.
    Includes :has() variants (group-has, peer-has, has) and focus-within/
    focus-visible variants that have modifier prefixes.

    Note: Regular focus: with modifier prefix is NOT a late modifier. Only
    focus-within: and focus-visible: are late modifiers.

    IMPORTANT: Only selectors with modifier colons (like `.focus-within\:ring`)
    are late modifiers. Native pseudo-classes like `.form-radio:focus` are not.
*)
let is_late_modifier sel_kind has_modifier_colon =
  match sel_kind with
  | Complex { has_group_has = true; _ } -> true
  | Complex { has_peer_has = true; _ } -> true
  | Complex { has_focus_within = true; _ } -> has_modifier_colon
  | Complex { has_focus_visible = true; _ } -> has_modifier_colon
  | Complex { has_standalone_has = true; _ } -> true
  | _ -> false

(** Check if a selector is a state modifier rule. These include :active,
    :disabled, [aria-*], and :has() selectors with modifier colons. *)
let is_state_modifier_rule sel_kind has_modifier_colon sel_str =
  if not has_modifier_colon then false
  else
    match sel_kind with
    | Complex { has_aria = true; _ } -> true
    | Complex { has_standalone_has = true; _ } -> true
    | Complex _ ->
        String.ends_with ~suffix:":active" sel_str
        || String.ends_with ~suffix:":disabled" sel_str
    | _ -> false

(** Check if a selector is a focus: modifier rule (has :focus pseudo-class and
    modifier colon). These rules come AFTER hover:hover media but BEFORE other
    modifier-prefixed media like motion-safe:/motion-reduce:/contrast-more:. *)
let is_focus_modifier_rule sel_kind has_modifier_colon =
  has_modifier_colon
  && match sel_kind with Complex { has_focus = true; _ } -> true | _ -> false

(** Compare by (priority, suborder), defaulting to [regular_first] (-1). *)
let compare_by_order_regular_first (p1, s1) (p2, s2) =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp else -1

(** Compare Regular vs Media rules from different utilities. Uses selector
    classification to determine ordering. *)
let try_hover_media_interleave (r1 : indexed_rule) (r2 : indexed_rule)
    media_type =
  match media_type with
  | Some Css.Media.Hover
    when is_focus_modifier_rule r1.selector_kind r1.has_modifier_colon ->
      let d1 = selector_modifier_depth r1.selector in
      let d2 = selector_modifier_depth r2.selector in
      let has_hov = selector_has_hover r1.selector in
      if d2 > d1 && not has_hov then Some (-1) else Some 1
  | _ -> None

let compare_different_utility_regular_media (r1 : indexed_rule)
    (r2 : indexed_rule) media_type =
  match try_hover_media_interleave r1 r2 media_type with
  | Some c -> c
  | None -> (
      if is_focus_modifier_rule r1.selector_kind r1.has_modifier_colon then 1
      else
        let is_modifier_prefixed_media =
          r2.has_modifier_colon
          &&
          match media_type with
          | Some
              ( Css.Media.Preference_appearance
              | Css.Media.Preference_accessibility ) ->
              true
          | _ -> false
        in
        if is_modifier_prefixed_media then
          if is_late_modifier r1.selector_kind r1.has_modifier_colon then 1
          else -1
        else if not r2.has_modifier_colon then
          let prio_cmp = Int.compare (fst r1.order) (fst r2.order) in
          if prio_cmp <> 0 then prio_cmp
          else
            match media_type with
            | Some (Css.Media.Responsive _ | Css.Media.Hover) -> -1
            | _ -> compare_by_order_regular_first r1.order r2.order
        else
          match media_type with
          | Some (Css.Media.Hover | Css.Media.Responsive _) -> -1
          | _ -> compare_by_order_regular_first r1.order r2.order)

(** Compare Regular vs Media rules using rule relationship dispatch. *)
let compare_regular_vs_media r1 r2 =
  match rule_relationship r1 r2 with
  | Same_utility _ -> compare_same_utility_regular_media r1 r2
  | Different_utilities ->
      let media_type =
        match r2.rule_type with
        | `Media m -> Some (Css.Media.kind m)
        | _ -> None
      in
      compare_different_utility_regular_media r1 r2 media_type

(* ======================================================================== *)
(* Regular Rule Comparison *)
(* ======================================================================== *)

(** Compare pseudo-element vs non-pseudo-element selectors. Simple selectors
    ALWAYS come before Pseudo_element selectors within the same priority group.
*)
let compare_pseudo_elements kind1 kind2 _sel1 _sel2 =
  match (kind1, kind2) with
  | Simple, Pseudo_element -> Some (-1)
  | Pseudo_element, Simple -> Some 1
  | Pseudo_element, Pseudo_element -> None
  | _, _ -> None

(** Compare rules by order tuple then index. Used for same-utility regular
    rules, starting style rules, and as a generic tiebreaker. *)
let compare_by_order_then_index r1 r2 =
  let order_cmp = compare_order r1.order r2.order in
  if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index

let compare_same_utility_regular = compare_by_order_then_index

let compare_base_class_option bc1 bc2 =
  match (bc1, bc2) with
  | Some bc1, Some bc2 -> String.compare bc1 bc2
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None -> 0

let compare_by_priority_index r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let bc_cmp = compare_base_class_option r1.base_class r2.base_class in
      if bc_cmp <> 0 then bc_cmp
      else
        let idx_cmp = Int.compare r1.index r2.index in
        if idx_cmp <> 0 then idx_cmp
        else String.compare r1.selector_str r2.selector_str

let is_digit c = c >= '0' && c <= '9'

(* Natural sort comparison: treats consecutive digit sequences as integers.
   E.g., "2.5" < "2.25" because 5 < 25 when compared as numbers. This matches
   Tailwind v4's selector ordering for opacity modifiers like /2.5 vs /2.25. *)
let natural_extract_number s i =
  let rec go j acc =
    if j >= String.length s || not (is_digit s.[j]) then (acc, j)
    else go (j + 1) ((acc * 10) + Char.code s.[j] - Char.code '0')
  in
  go i 0

(* Skip CSS escape backslash before '#': compare \# as #. Only unescape \# —
   other escapes like \/ need the backslash for correct opacity modifier
   ordering. *)
let natural_skip_hash_escape s i len =
  if i < len && s.[i] = '\\' && i + 1 < len && s.[i + 1] = '#' then i + 1 else i

let boundary_compare i1 len1 i2 len2 =
  if i1 >= len1 && i2 >= len2 then `Equal
  else if i1 >= len1 then `Less
  else if i2 >= len2 then `Greater
  else `Continue

let natural_compare s1 s2 =
  let len1 = String.length s1 and len2 = String.length s2 in
  let rec compare_at i1 i2 =
    match boundary_compare i1 len1 i2 len2 with
    | `Equal -> 0
    | `Less -> -1
    | `Greater -> 1
    | `Continue ->
        let i1 = natural_skip_hash_escape s1 i1 len1 in
        let i2 = natural_skip_hash_escape s2 i2 len2 in
        compare_at_chars i1 i2
  and compare_at_chars i1 i2 =
    match boundary_compare i1 len1 i2 len2 with
    | `Equal -> 0
    | `Less -> -1
    | `Greater -> 1
    | `Continue ->
        let c1 = s1.[i1] and c2 = s2.[i2] in
        if is_digit c1 && is_digit c2 then
          let n1, end1 = natural_extract_number s1 i1 in
          let n2, end2 = natural_extract_number s2 i2 in
          let num_cmp = Int.compare n1 n2 in
          if num_cmp <> 0 then num_cmp else compare_at end1 end2
        else
          let char_cmp = Char.compare c1 c2 in
          if char_cmp <> 0 then char_cmp else compare_at (i1 + 1) (i2 + 1)
  in
  compare_at 0 0

(* Tailwind orders the values of dynamic candidates by the raw candidate
   spelling. This naturally interleaves digit-led theme names with numeric
   values (2, 2xl, 10), and puts the [(--var)] shorthand before both. The
   handler suborders still separate the property families themselves. *)
let candidate_value_family base_class =
  let _, base = Modifiers.of_string base_class in
  List.find_opt
    (fun prefix ->
      let n = String.length prefix in
      String.length base > n && String.starts_with ~prefix:(prefix ^ "-") base)
    [
      "pbs";
      "pbe";
      "px";
      "py";
      "ps";
      "pe";
      "pt";
      "pr";
      "pb";
      "pl";
      "p";
      "min-inline";
      "max-inline";
      "min-block";
      "max-block";
      "min-w";
      "max-w";
      "min-h";
      "max-h";
      "inline";
      "block";
      "size";
      "basis";
      "w";
      "h";
    ]

let compare_candidate_values r1 r2 =
  if fst r1.order <> fst r2.order then None
  else
    match
      ( candidate_value_family r1.base_class_key,
        candidate_value_family r2.base_class_key )
    with
    | Some f1, Some f2 when String.equal f1 f2 ->
        Some (natural_compare r1.base_class_key r2.base_class_key)
    | _ -> None

let compare_late_modifiers r1 r2 kind1 kind2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2 else compare_by_priority_index r1 r2

(** Check if a selector kind is a focus-visible late modifier *)
let is_focus_visible_late_modifier kind has_modifier_colon =
  is_late_modifier kind has_modifier_colon
  &&
  match kind with
  | Complex { has_focus_visible = true; _ } -> true
  | _ -> false

(** Compare focus-visible and state modifier ordering. Returns [Some cmp] if at
    least one rule is a focus-visible or state modifier, [None] otherwise. *)
let compare_focus_visible_state r1 r2 kind1 kind2 =
  let fv1 = is_focus_visible_late_modifier kind1 r1.has_modifier_colon in
  let fv2 = is_focus_visible_late_modifier kind2 r2.has_modifier_colon in
  let s1 = is_state_modifier_rule kind1 r1.has_modifier_colon r1.selector_str in
  let s2 = is_state_modifier_rule kind2 r2.has_modifier_colon r2.selector_str in
  if fv1 && s2 then Some (-1)
  else if s1 && fv2 then Some 1
  else if fv1 && (not fv2) && not s2 then Some 1
  else if fv2 && (not fv1) && not s1 then Some (-1)
  else if fv1 && fv2 then Some (compare_by_priority_index r1 r2)
  else if s1 && not s2 then Some 1
  else if s2 && not s1 then Some (-1)
  else if s1 && s2 then Some (compare_by_priority_index r1 r2)
  else None

(** Compare focus modifier ordering. Returns [Some cmp] if at least one rule is
    a focus modifier, [None] otherwise. *)
let compare_focus_modifier_ordering r1 r2 kind1 kind2 =
  let f1 = is_focus_modifier_rule kind1 r1.has_modifier_colon in
  let f2 = is_focus_modifier_rule kind2 r2.has_modifier_colon in
  if f1 && not f2 then Some 1
  else if f2 && not f1 then Some (-1)
  else if f1 && f2 then Some (compare_by_priority_index r1 r2)
  else None

(* A project's [@utility] borrows the slot of the property it writes, so it
   lands among the built-ins of that family and the two orders decide which
   wins. Tailwind puts the rule carrying more declarations first: [select-none]
   writes the prefixed spelling of [user-select] as well as the plain one, so it
   comes before a declared utility writing [user-select] alone whatever that
   utility is called. *)
let compare_declared_width r1 r2 =
  if not (r1.declared || r2.declared) then 0
  else Int.compare r2.declaration_count r1.declaration_count

(** Compare by priority, suborder, late modifiers, then natural selector sort.
    Used as the final comparison when focus-visible/state/focus modifiers don't
    apply. *)
let compare_by_prio_sub_late r1 r2 kind1 kind2 =
  let p1, _ = r1.order and p2, _ = r2.order in
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let _, s1 = r1.order and _, s2 = r2.order in
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let late1 = is_late_modifier kind1 r1.has_modifier_colon in
      let late2 = is_late_modifier kind2 r2.has_modifier_colon in
      if late1 && not late2 then 1
      else if late2 && not late1 then -1
      else if late1 && late2 then compare_late_modifiers r1 r2 kind1 kind2
      else
        (* Two utilities share a slot when they are named for the same property,
           which is where a project's own [@utility] lands. The wider rule goes
           first - [select-none] writes the prefixed spelling of [user-select]
           as well as the plain one - and only rules of equal width fall through
           to the candidate name. *)
        let width_cmp = compare_declared_width r1 r2 in
        if width_cmp <> 0 then width_cmp
        else natural_compare r1.base_class_key r2.base_class_key

let compare_cross_utility_regular r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let kind1 = r1.selector_kind in
  let kind2 = r2.selector_kind in
  if debug_compare then (
    let sel1 = r1.selector_str in
    let sel2 = r2.selector_str in
    let kind_str = function
      | Simple -> "Simple"
      | Pseudo_element -> "Pseudo_element"
      | Complex _ -> "Complex"
    in
    prerr_string
      (String.concat ""
         [
           "compare_cross_prio: ";
           sel1;
           " (";
           string_of_int p1;
           ",";
           string_of_int s1;
           ") vs ";
           sel2;
           " (";
           string_of_int p2;
           ",";
           string_of_int s2;
           ")\n";
         ]);
    prerr_string
      (String.concat ""
         [
           "compare_cross_kind: ";
           sel1;
           " (";
           kind_str kind1;
           ") vs ";
           sel2;
           " (";
           kind_str kind2;
           ")\n";
         ]));
  match compare_candidate_values r1 r2 with
  | Some cmp when cmp <> 0 -> cmp
  | Some _ | None -> (
      let same_order = p1 = p2 && s1 = s2 in
      match
        if same_order then
          compare_pseudo_elements kind1 kind2 r1.selector r2.selector
        else None
      with
      | Some cmp -> cmp
      | None -> (
          match compare_focus_visible_state r1 r2 kind1 kind2 with
          | Some cmp -> cmp
          | None -> (
              match compare_focus_modifier_ordering r1 r2 kind1 kind2 with
              | Some cmp -> cmp
              | None -> compare_by_prio_sub_late r1 r2 kind1 kind2)))

(** Compare two Regular rules using rule relationship dispatch. *)
let compare_regular_rules r1 r2 =
  let rel = rule_relationship r1 r2 in
  if debug_compare then
    prerr_string
      (String.concat ""
         [
           "compare_regular: ";
           r1.selector_str;
           " vs ";
           r2.selector_str;
           " -> ";
           (match rel with
           | Same_utility bc -> "Same:" ^ bc
           | Different_utilities -> "Different");
           "\n";
         ]);
  match rel with
  | Same_utility _ -> compare_same_utility_regular r1 r2
  | Different_utilities -> compare_cross_utility_regular r1 r2

let compare_starting_rules = compare_by_order_then_index

(* ======================================================================== *)
(* Main Rule Comparison *)
(* ======================================================================== *)

let supports_suffix s =
  if String.starts_with ~prefix:"supports-" s then
    Some (String.sub s 9 (String.length s - 9))
  else if String.starts_with ~prefix:"not-supports-" s then
    Some (String.sub s 13 (String.length s - 13))
  else None

(* Sort key for supports modifier variants: named before bracket. Negating a
   supports variant changes its condition, not its position within this
   group. *)
let supports_sort_key bc =
  match Option.bind bc supports_suffix with
  | Some after ->
      if String.length after > 0 && after.[0] = '[' then (1, after)
      else (0, after)
  | None -> (0, "")

(* A [supports-*] variant rule, whose @supports condition is the variant itself.
   A colour utility's progressive-enhancement @supports carries the colour's own
   base class and must not be ordered by this key. *)
let is_modifier_supports bc =
  match bc with Some s -> Option.is_some (supports_suffix s) | None -> false

(* Compare supports modifier rules by sort key *)
let compare_supports_by_key r1 r2 =
  let g1, k1 = supports_sort_key r1.base_class in
  let g2, k2 = supports_sort_key r2.base_class in
  let grp_cmp = Int.compare g1 g2 in
  if grp_cmp <> 0 then grp_cmp
  else
    let key_cmp = natural_compare k1 k2 in
    if key_cmp <> 0 then key_cmp else Int.compare r1.index r2.index

(* Compare by order tuple, then selector, then index *)
let compare_by_order_then_selector r1 r2 =
  let order_cmp = compare_order r1.order r2.order in
  if order_cmp <> 0 then order_cmp
  else
    let sel_cmp = natural_compare r1.selector_str r2.selector_str in
    if sel_cmp <> 0 then sel_cmp else Int.compare r1.index r2.index

(* Compare nested media conditions *)
let compare_nested_media r1 r2 =
  match (r1.nested, r2.nested) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | [ _ ], [ _ ] -> (
      match (r1.nested_media_key, r2.nested_media_key) with
      | Some k1, Some k2 -> Css.Media.compare_keys k1 k2
      | _ -> 0)
  | _ -> 0

(* Extract the modifier prefix from a base_class, e.g. "hover:p-4" -> "hover".
   Split with the modifier parser, not on the last ':': an arbitrary value can
   hold one, and [hover:bg-[color:var(--x)]] split naively yields the prefix
   [hover:bg-[color]. *)
let variant_prefix = function
  | Some s -> String.concat ":" (fst (Modifiers.of_string s))
  | None -> ""

(* Compute variant order for a modifier prefix, stripping group-/peer-
   wrappers *)
let strip_group_peer_vo p =
  if String.starts_with ~prefix:"group-" p then
    Modifiers.variant_order_of_prefix (String.sub p 6 (String.length p - 6))
  else if String.starts_with ~prefix:"peer-" p then
    Modifiers.variant_order_of_prefix (String.sub p 5 (String.length p - 5))
  else Modifiers.variant_order_of_prefix p

(* What Tailwind sorts a container variant on. It reads the value off the class
   rather than the width that value resolves to, and keys it by the unit -- or,
   when the value is a call, by the name before the parenthesis. A size off the
   [--container] scale is resolved through the theme before the key is taken,
   and that scale is rem throughout. *)
type container_value = {
  name : string; (* The value's unit, or the name of the call it is. *)
  call : bool; (* The value is a function call. *)
  text : string; (* The value as the class spells it. *)
}

(* Tailwind strips every run of digits and dots to reach the unit, so a sign
   stays behind with it. *)
let unit_of_container_value text =
  let buf = Buffer.create (String.length text) in
  String.iter
    (fun c ->
      if not ((c >= '0' && c <= '9') || c = '.') then Buffer.add_char buf c)
    text;
  Buffer.contents buf

(* Read the container value out of one modifier token: [@min-[64rem]] and
   [@[theme(--breakpoint-lg)]] carry it in the bracket, [@lg] and [@min-lg] name
   a size on the [--container] scale. A [/name] tail aims the query at a named
   container and says nothing about the width. *)
let container_value_of_token token =
  let n = String.length token in
  if n < 2 || token.[0] <> '@' then None
  else
    let body = String.sub token 1 (n - 1) in
    let body =
      if
        String.starts_with ~prefix:"min-" body
        || String.starts_with ~prefix:"max-" body
      then String.sub body 4 (String.length body - 4)
      else body
    in
    if String.length body > 1 && body.[0] = '[' then
      match String.rindex_opt body ']' with
      | Some i when i > 1 -> (
          let text = String.sub body 1 (i - 1) in
          match String.index_opt text '(' with
          | Some j -> Some { name = String.sub text 0 j; call = true; text }
          | None ->
              Some { name = unit_of_container_value text; call = false; text })
      | _ -> None
    else Some { name = "rem"; call = false; text = body }

let container_value_of_prefix prefix =
  List.find_map container_value_of_token (Parse.split_on_colon prefix)

(* Only a call keys on a name the resolved length cannot carry, so every other
   pair keeps the length key, which already orders the way Tailwind does. *)
let compare_container_values r1 r2 p1 p2 =
  match (r1.rule_type, r2.rule_type) with
  | `Container _, `Container _ -> (
      match (container_value_of_prefix p1, container_value_of_prefix p2) with
      | Some v1, Some v2 when v1.call || v2.call ->
          let c = String.compare v1.name v2.name in
          if c <> 0 then Some c else Some (String.compare v1.text v2.text)
      | _ -> None)
  | _ -> None

(* Compute the inner variant order for a compound prefix like "hover:focus" *)
let inner_vo prefix =
  match Parse.split_on_colon prefix with
  | [] -> 0
  | [ _ ] ->
      if String.starts_with ~prefix:"group-" prefix then
        Modifiers.variant_order_of_prefix
          (String.sub prefix 6 (String.length prefix - 6))
      else if String.starts_with ~prefix:"peer-" prefix then
        Modifiers.variant_order_of_prefix
          (String.sub prefix 5 (String.length prefix - 5))
      else 0
  | outer :: _ :: _ as parts ->
      if
        String.starts_with ~prefix:"group-" outer
        || String.starts_with ~prefix:"peer-" outer
      then
        List.fold_left (fun acc p -> max acc (strip_group_peer_vo p)) 0 parts
        + 1
      else Modifiers.variant_order_of_prefix (String.concat ":" (List.tl parts))

(* Effective inner variant order: prefer prefix-derived, fall back to nested
   media *)
let effective_ivo_of nested prefix =
  let ivo = inner_vo prefix in
  if ivo > 0 then ivo
  else
    match nested with
    | [ n ] -> (
        match Css.as_media n with
        | Some (cond, _) -> Modifiers.variant_order_of_media_cond cond
        | None -> 0)
    | _ -> 0

(* The variant prefix and effective inner order are pure functions of a rule's
   base class and nested statements, but [compare_variant_ordered] needs them on
   every comparison. Precompute them once per rule (see [add_index]) so the hot
   sort comparator only reads the result. *)
let variant_sort_key base_class nested =
  let prefix = variant_prefix base_class in
  (prefix, effective_ivo_of nested prefix)

(* Two components in the same slot are separated by the breakpoint first: a rule
   whose highest-order variant is a breakpoint groups under that breakpoint, so
   first:sm:m-2 stays beside sm:bg-top instead of falling past md:block. A slot
   with no width on one side leaves that to the tie-breakers below, as the
   wrapped state and then the rest of the stack. *)
let compare_variant_components a b =
  let slot_cmp = Int.compare a.slot b.slot in
  if slot_cmp <> 0 then slot_cmp
  else
    let bp_cmp =
      match (a.breakpoint, b.breakpoint) with
      | Some k1, Some k2 -> Css.Media.compare_keys k1 k2
      | Some _, None | None, Some _ | None, None -> 0
    in
    if bp_cmp <> 0 then bp_cmp else Int.compare a.wrapped b.wrapped

(* One modifier token's sort key. The slot alone leaves every breakpoint on one
   key and every group-/peer- spelling on another, so the component carries what
   separates two tokens inside a slot: the width for a breakpoint, read off the
   media query the rule renders as, and the wrapped state for group-/peer-, so
   group-focus and group-has keep their focus-before-has order. *)
let token_order_key ~breakpoint token =
  let slot = Modifiers.variant_order_of_prefix token in
  let wrapped = Modifiers.variant_inner_order token in
  let breakpoint =
    if slot = responsive_variant_order then breakpoint else None
  in
  { slot; breakpoint; wrapped }

(* The variant order keys of a class's modifier stack, sorted descending.
   Tailwind sorts a candidate by this list compared lexicographically ascending,
   so a stacked variant sorts into the group of its highest-order component and
   after that group's base rules, and two stacks with the same variant multiset
   (group:hover vs hover:group) get identical keys. Falls back to the scalar
   [variant_order] for selector-derived variants (before:/after:) that carry no
   order-bearing prefix in the base class. *)
let variant_order_list base_class variant_order breakpoint =
  let from_bc =
    match base_class with
    | None -> []
    | Some bc ->
        let modifiers, _ = Modifiers.of_string bc in
        List.filter_map
          (fun m ->
            let key = token_order_key ~breakpoint m in
            if key.slot > 0 then Some key else None)
          modifiers
        |> List.sort (fun a b -> compare_variant_components b a)
  in
  match from_bc with
  | [] when variant_order > 0 ->
      [ { slot = variant_order; breakpoint = None; wrapped = 0 } ]
  | l -> l

(* Compare two descending variant-order-key lists lexicographically, ascending
   on the first differing key, with a shorter (prefix) list sorting first so
   base rules precede the compounds built on them. *)
let rec compare_variant_order_lists l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | a :: r1, b :: r2 ->
      let c = compare_variant_components a b in
      if c <> 0 then c else compare_variant_order_lists r1 r2

(** Classify bracket content: pseudo-class brackets ([:checked]) sort before
    combinator/ampersand brackets ([&>img], [+img], etc.). *)
let bracket_content_key p =
  match String.index_opt p '[' with
  | Some i when i + 1 < String.length p ->
      let first_char = p.[i + 1] in
      if first_char = ':' then 0 (* pseudo-class *)
      else 1 (* combinator/ampersand/other *)
  | _ -> 1

(** Compare two bracket-containing variant prefixes: by bracket content type
    (pseudo-class before combinator), then by raw name. *)
let compare_both_bracket_prefixes p1 p2 =
  let bk_cmp = Int.compare (bracket_content_key p1) (bracket_content_key p2) in
  if bk_cmp <> 0 then bk_cmp else String.compare p1 p2

(** Compare variant prefixes for bracket ordering. Named variants (has-checked)
    sort before bracket variants (has-[:checked]) within the same variant group.
*)
let compare_bracket_prefixes p1_prefix p2_prefix =
  let has_bracket p = String.length p > 0 && String.contains p '[' in
  let b1 = has_bracket p1_prefix and b2 = has_bracket p2_prefix in
  if b1 && not b2 then 1
  else if b2 && not b1 then -1
  else if b1 && b2 then compare_both_bracket_prefixes p1_prefix p2_prefix
  else String.compare p1_prefix p2_prefix

(* Compare rules when both have variant_order > 0 *)
let nested_order rule_type nested =
  match nested with
  | [] -> 0 (* non-nested: middle *)
  | [ stmt ] -> (
      match (rule_type, Css.as_media stmt) with
      | `Media c, Some (nested, _)
        when Css.Media.kind c = Css.Media.Hover
             && Css.Media.kind nested = Css.Media.Hover ->
          2 (* doubly-nested hover: after everything *)
      | _, Some (nested, _) when Css.Media.kind nested = Css.Media.Hover ->
          -1 (* single hover nested: first *)
      | _ -> 1 (* other nested: last *))
  | _ -> 1 (* multiple nested: last *)

(* Last resort for two rules that share a variant group, a breakpoint and a
   prefix: the utility's own priority, then the selector. *)
let compare_variant_tail r1 r2 =
  match compare_candidate_values r1 r2 with
  | Some cmp when cmp <> 0 -> cmp
  | Some _ | None -> (
      let p1, s1 = r1.order and p2, s2 = r2.order in
      let prio_cmp = Int.compare p1 p2 in
      if prio_cmp <> 0 then prio_cmp
      else
        let sub_cmp = Int.compare s1 s2 in
        if sub_cmp <> 0 then sub_cmp
        else
          match (r1.selector_kind, r2.selector_kind) with
          | Simple, Simple ->
              (* Same priority/suborder simple rules (e.g. two arbitrary bg
                 colors) break ties by selector like the regular layer, matching
                 Tailwind's alphabetical order. *)
              natural_compare r1.selector_str r2.selector_str
          | _ ->
              (* Complex rules (prose's descendant selectors) keep base class +
                 source order so a component stays one block. Arbitrary values
                 in a variant, e.g. hover:from-[rgba(5,...)] vs
                 hover:from-[rgba(14,...)], share a prefix and differ only in
                 the numeric part, so order those numerically like Tailwind.
                 Identical base classes (prose's :where rules all key on
                 "prose") tie at 0 and fall back to source order, unchanged. *)
              let class_cmp =
                natural_compare r1.base_class_key r2.base_class_key
              in
              if class_cmp <> 0 then class_cmp
              else Int.compare r1.index r2.index)

let compare_variant_ordered r1 r2 =
  match (r1.rule_type, r2.rule_type) with
  | `Supports _, `Supports _
    when r1.variant_order = r2.variant_order
         && is_modifier_supports r1.base_class
         && is_modifier_supports r2.base_class ->
      compare_supports_by_key r1 r2
  | _ ->
      let list_cmp =
        compare_variant_order_lists r1.variant_orders r2.variant_orders
      in
      if list_cmp <> 0 then list_cmp
      else
        let p1_prefix, _ = r1.variant_key in
        let p2_prefix, _ = r2.variant_key in
        (* The descending variant-order lists tie (same variant multiset), so
           hover:sm: and sm:hover: arrive here indistinguishable. The query a
           rule writes on the outside decides between them, hover before sm and
           sm before md; a nested breakpoint or hover, the prefix and the
           utility's own priority order what is left. *)
        let media_cmp =
          match compare_container_values r1 r2 p1_prefix p2_prefix with
          | Some c -> c
          | None -> (
              match (r1.media_key, r2.media_key) with
              | Some k1, Some k2 -> Css.Media.compare_keys k1 k2
              | _ -> 0)
        in
        if media_cmp <> 0 then media_cmp
        else
          let nested_cmp =
            Int.compare
              (nested_order r1.rule_type r1.nested)
              (nested_order r2.rule_type r2.nested)
          in
          if nested_cmp <> 0 then nested_cmp
          else
            let nested_media_cmp = compare_nested_media r1 r2 in
            if nested_media_cmp <> 0 then nested_media_cmp
            else
              (* Two container variants at the same width are already fully
                 ordered: what remains is the utility's own priority, so the
                 prefix must not step in and group @sm/main away from @sm. *)
              let prefix_cmp =
                match (r1.rule_type, r2.rule_type) with
                | `Container _, `Container _ -> 0
                | _ -> compare_bracket_prefixes p1_prefix p2_prefix
              in
              if prefix_cmp <> 0 then prefix_cmp else compare_variant_tail r1 r2

(* Compare two Supports rules *)
let compare_supports_rules r1 r2 =
  let m1 = is_modifier_supports r1.base_class in
  let m2 = is_modifier_supports r2.base_class in
  if m1 && m2 then compare_supports_by_key r1 r2
  else compare_by_order_then_selector r1 r2

(** Compare indexed rules for sorting. Uses type-directed dispatch based on
    rule_type. This is the main entry point for sorting assembled CSS rules into
    Tailwind v4 cascade order. *)
let compare_indexed_rules r1 r2 =
  (if debug_compare then
     let rule_type_str = function
       | `Regular -> "R"
       | `Media _ -> "M"
       | `Container _ -> "C"
       | `Starting -> "S"
       | `Supports _ -> "U"
     in
     prerr_string
       (String.concat ""
          [
            "compare_indexed: ";
            r1.selector_str;
            " vs ";
            r2.selector_str;
            " (types: ";
            rule_type_str r1.rule_type;
            "/";
            rule_type_str r2.rule_type;
            ")\n";
          ]));
  if r1.variant_order > 0 && r2.variant_order > 0 then
    compare_variant_ordered r1 r2
  else if r1.variant_order > 0 then 1
  else if r2.variant_order > 0 then -1
  else
    let type_cmp =
      Int.compare (rule_type_order r1.rule_type) (rule_type_order r2.rule_type)
    in
    if type_cmp <> 0 then type_cmp
    else
      match (r1.rule_type, r2.rule_type) with
      | `Regular, `Regular -> compare_regular_rules r1 r2
      | `Media _, `Media _ -> compare_media_rules r1 r2
      | `Regular, `Media _ -> compare_regular_vs_media r1 r2
      | `Media _, `Regular -> -compare_regular_vs_media r2 r1
      | `Starting, `Starting -> compare_starting_rules r1 r2
      | `Container _, `Container _ -> Int.compare r1.index r2.index
      | `Supports _, `Supports _ -> compare_supports_rules r1 r2
      | `Regular, `Supports _ | `Supports _, `Regular ->
          compare_by_order_then_selector r1 r2
      | `Supports _, `Media _ | `Media _, `Supports _ ->
          compare_by_order_then_index r1 r2
      | _, _ -> Int.compare r1.index r2.index
