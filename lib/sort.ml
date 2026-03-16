(** CSS rule cascade sort order

    Provides the [indexed_rule] type and [compare_indexed_rules], the comparison
    function used to sort assembled CSS rules into Tailwind v4 cascade order. *)

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
  props : Css.declaration list;
  order : int * int;
  nested : Css.statement list;
  base_class : string option;
  merge_key : string option;
  not_order : int;
  variant_order : int;
}
(** An indexed CSS rule ready for sorting. [index] preserves source order;
    [order] is the [(priority, suborder)] pair from the utility definition;
    [variant_order] places modifier-prefixed rules after their base
    counterparts. *)

(* ======================================================================== *)
(* Debug *)
(* ======================================================================== *)

let debug_compare = ref false
let set_debug_compare b = debug_compare := b
let debug_compare_enabled () = !debug_compare

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

(** Check if a selector contains :hover pseudo-class at any depth (used to
    detect compound variants like group-hocus that combine hover+focus). *)
let rec selector_has_hover = function
  | Css.Selector.Hover -> true
  | Css.Selector.Compound sels -> List.exists selector_has_hover sels
  | Css.Selector.Combined (l, _, r) ->
      selector_has_hover l || selector_has_hover r
  | Css.Selector.Is sels | Css.Selector.Where sels ->
      List.exists selector_has_hover sels
  | Css.Selector.List sels -> List.exists selector_has_hover sels
  | _ -> false

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

(* ======================================================================== *)
(* Priority Comparison *)
(* ======================================================================== *)

(** Compare two simple selectors by suborder, then alphabetically, then index.
    Index fallback is critical for utilities like prose that emit multiple rules
    with the same selector - preserves original order. *)
let compare_simple_selectors sel1 sel2 s1 s2 i1 i2 =
  let sub_cmp = Int.compare s1 s2 in
  if sub_cmp <> 0 then sub_cmp
  else
    let sel_cmp =
      String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
    in
    if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare two complex selectors by kind, then selector string (for aria), then
    suborder, then index. Order: focus < group-has < peer-has < has < aria. For
    aria selectors, sort by attribute name before property to match Tailwind. *)
let compare_complex_selectors sel1 sel2 kind1 kind2 s1 s2 i1 i2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2
  else if k1 = 60 then
    (* Both are aria selectors - compare by selector string (aria attribute)
       before suborder (property shade) to match Tailwind v4 behavior *)
    let sel_cmp =
      String.compare (Css.Selector.to_string sel1) (Css.Selector.to_string sel2)
    in
    if sel_cmp <> 0 then sel_cmp
    else
      let sub_cmp = Int.compare s1 s2 in
      if sub_cmp <> 0 then sub_cmp else Int.compare i1 i2
  else
    (* Other complex selectors - use suborder first *)
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let sel_cmp =
        String.compare
          (Css.Selector.to_string sel1)
          (Css.Selector.to_string sel2)
      in
      if sel_cmp <> 0 then sel_cmp else Int.compare i1 i2

(** Compare rules by priority, then suborder, then by selector kind. Uses
    type-directed dispatch based on selector classification. At the same
    priority/suborder, cross-kind comparisons preserve source order (index) to
    match tailwindcss output exactly. *)
let compare_by_priority_suborder_alpha sel1 sel2 (p1, s1) (p2, s2) i1 i2 =
  let prio_cmp = Int.compare p1 p2 in
  if prio_cmp <> 0 then prio_cmp
  else
    let sub_cmp = Int.compare s1 s2 in
    if sub_cmp <> 0 then sub_cmp
    else
      let kind1 = classify_selector sel1 in
      let kind2 = classify_selector sel2 in
      match (kind1, kind2) with
      | Simple, Simple -> compare_simple_selectors sel1 sel2 s1 s2 i1 i2
      | Pseudo_element, Pseudo_element ->
          compare_simple_selectors sel1 sel2 s1 s2 i1 i2
      | Pseudo_element, Simple -> Int.compare i1 i2
      | Pseudo_element, Complex _ ->
          (* Prose rules need pseudo-elements after complex selectors *)
          Int.compare i1 i2
      | Simple, Pseudo_element -> Int.compare i1 i2
      | Simple, Complex _ -> Int.compare i1 i2
      | Complex _, Pseudo_element -> Int.compare i1 i2
      | Complex _, Simple -> Int.compare i1 i2
      | Complex _, Complex _ ->
          compare_complex_selectors sel1 sel2 kind1 kind2 s1 s2 i1 i2

(* ======================================================================== *)
(* Media Query Comparison *)
(* ======================================================================== *)

(** Compare two media conditions within the same group *)
let compare_media_conditions group1 sub1 sub2 cond1 cond2 =
  if group1 = 2000 then Float.compare sub1 sub2
  else if group1 = 1000 then
    match (cond1, cond2) with
    | Some c1, Some c2 ->
        Int.compare
          (preference_condition_order c1)
          (preference_condition_order c2)
    | _ -> 0
  else
    match (cond1, cond2) with
    | Some c1, Some c2 -> Css.Media.compare c1 c2
    | _ -> 0

(* For hover media, separate rules by modifier depth so that single-modifier
   hover rules (group-hover:flex) form a separate block from stacked hover rules
   (group-focus:group-hover:flex) *)
let compare_hover_depth cond1 cond2 sel1 sel2 =
  match (cond1, cond2) with
  | Some Css.Media.Hover, Some Css.Media.Hover ->
      Int.compare (selector_modifier_depth sel1) (selector_modifier_depth sel2)
  | _ -> 0

(* Compare by nested media condition when both have nested media. This sorts
   stacked min/max variants like min-sm:max-xl vs min-sm:max-lg by the inner
   media condition. *)
let compare_nested_media_cond nested1 nested2 =
  match (nested1, nested2) with
  | [ n1 ], [ n2 ] -> (
      match (Css.as_media n1, Css.as_media n2) with
      | Some (c1, _), Some (c2, _) -> Css.Media.compare c1 c2
      | _ -> 0)
  | _ -> 0

let compare_same_media_group sel1 sel2 order1 order2 i1 i2 nested1 nested2 bc1
    bc2 cond1 cond2 =
  let depth_cmp = compare_hover_depth cond1 cond2 sel1 sel2 in
  if depth_cmp <> 0 then depth_cmp
  else
    let nested_media_cmp = compare_nested_media_cond nested1 nested2 in
    if nested_media_cmp <> 0 then nested_media_cmp
    else
      let same_utility =
        match (bc1, bc2) with
        | Some b1, Some b2 -> String.equal b1 b2
        | _ -> false
      in
      if same_utility then
        let order_cmp = compare order1 order2 in
        if order_cmp <> 0 then order_cmp else Int.compare i1 i2
      else compare_by_priority_suborder_alpha sel1 sel2 order1 order2 i1 i2

let compare_media_rules typ1 typ2 sel1 sel2 order1 order2 i1 i2 nested1 nested2
    bc1 bc2 =
  let nested_cmp = Bool.compare (nested1 <> []) (nested2 <> []) in
  if nested_cmp <> 0 then nested_cmp
  else
    let group1, sub1 = extract_media_sort_key typ1 in
    let group2, sub2 = extract_media_sort_key typ2 in
    let key_cmp = Int.compare group1 group2 in
    if key_cmp <> 0 then key_cmp
    else
      let cond1 = match typ1 with `Media c -> Some c | _ -> None in
      let cond2 = match typ2 with `Media c -> Some c | _ -> None in
      let cond_cmp = compare_media_conditions group1 sub1 sub2 cond1 cond2 in
      if cond_cmp <> 0 then cond_cmp
      else
        compare_same_media_group sel1 sel2 order1 order2 i1 i2 nested1 nested2
          bc1 bc2 cond1 cond2

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
let is_late_modifier sel_kind selector =
  let has_modifier_colon = Css.Selector.contains_modifier_colon selector in
  match sel_kind with
  | Complex { has_group_has = true; _ } -> true
  | Complex { has_peer_has = true; _ } -> true
  | Complex { has_focus_within = true; _ } -> has_modifier_colon
  | Complex { has_focus_visible = true; _ } -> has_modifier_colon
  | Complex { has_standalone_has = true; _ } -> true
  | _ -> false

(** Check if a selector is a state modifier rule. These include :active,
    :disabled, [aria-*], and :has() selectors with modifier colons. *)
let is_state_modifier_rule sel_kind selector =
  if not (Css.Selector.contains_modifier_colon selector) then false
  else
    match sel_kind with
    | Complex { has_aria = true; _ } -> true
    | Complex { has_standalone_has = true; _ } -> true
    | Complex _ ->
        let sel_str = Css.Selector.to_string selector in
        String.ends_with ~suffix:":active" sel_str
        || String.ends_with ~suffix:":disabled" sel_str
    | _ -> false

(** Check if a selector is a focus: modifier rule (has :focus pseudo-class and
    modifier colon). These rules come AFTER hover:hover media but BEFORE other
    modifier-prefixed media like motion-safe:/motion-reduce:/contrast-more:. *)
let is_focus_modifier_rule sel_kind selector =
  Css.Selector.contains_modifier_colon selector
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
let try_hover_media_interleave kind1 sel1 sel2 media_type =
  match media_type with
  | Some Css.Media.Hover when is_focus_modifier_rule kind1 sel1 ->
      let d1 = selector_modifier_depth sel1 in
      let d2 = selector_modifier_depth sel2 in
      let has_hov = selector_has_hover sel1 in
      if d2 > d1 && not has_hov then Some (-1) else Some 1
  | _ -> None

let compare_different_utility_regular_media sel1 sel2 order1 order2 media_type =
  let kind1 = classify_selector sel1 in
  match try_hover_media_interleave kind1 sel1 sel2 media_type with
  | Some c -> c
  | None -> (
      if is_focus_modifier_rule kind1 sel1 then 1
      else
        let is_modifier_prefixed_media =
          Css.Selector.contains_modifier_colon sel2
          &&
          match media_type with
          | Some
              ( Css.Media.Prefers_color_scheme _
              | Css.Media.Prefers_reduced_motion _
              | Css.Media.Prefers_contrast _ ) ->
              true
          | _ -> false
        in
        if is_modifier_prefixed_media then
          if is_late_modifier kind1 sel1 then 1 else -1
        else
          let has_modifier_colon = Css.Selector.contains_modifier_colon sel2 in
          if not has_modifier_colon then
            let prio_cmp = Int.compare (fst order1) (fst order2) in
            if prio_cmp <> 0 then prio_cmp
            else
              match media_type with
              | Some
                  ( Css.Media.Min_width _ | Css.Media.Min_width_rem _
                  | Css.Media.Hover ) ->
                  -1
              | _ -> compare_by_order_regular_first order1 order2
          else
            match media_type with
            | Some
                ( Css.Media.Hover | Css.Media.Min_width _
                | Css.Media.Min_width_rem _ ) ->
                -1
            | _ -> compare_by_order_regular_first order1 order2)

(** Compare Regular vs Media rules using rule relationship dispatch. *)
let compare_regular_vs_media r1 r2 =
  match rule_relationship r1 r2 with
  | Same_utility _ -> compare_same_utility_regular_media r1 r2
  | Different_utilities ->
      let media_type =
        match r2.rule_type with `Media m -> Some m | _ -> None
      in
      compare_different_utility_regular_media r1.selector r2.selector r1.order
        r2.order media_type

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

(** Compare regular rules from the same base utility. Preserves original index
    order to maintain source ordering within a utility. *)
let compare_same_utility_regular r1 r2 =
  let prio_cmp = compare r1.order r2.order in
  if prio_cmp <> 0 then prio_cmp else Int.compare r1.index r2.index

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
        else
          String.compare
            (Css.Selector.to_string r1.selector)
            (Css.Selector.to_string r2.selector)

let is_outline_utility bc =
  match bc with
  | Some s ->
      String.contains s ':' && String.contains s 'o'
      &&
      let idx = String.index s ':' in
      idx + 8 <= String.length s && String.sub s idx 8 = ":outline"
  | None -> false

(* Natural sort comparison: treats consecutive digit sequences as integers.
   E.g., "2.5" < "2.25" because 5 < 25 when compared as numbers. This matches
   Tailwind v4's selector ordering for opacity modifiers like /2.5 vs /2.25. *)
let natural_is_digit c = c >= '0' && c <= '9'

let natural_extract_number s i =
  let rec go j acc =
    if j >= String.length s || not (natural_is_digit s.[j]) then (acc, j)
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
        if natural_is_digit c1 && natural_is_digit c2 then
          let n1, end1 = natural_extract_number s1 i1 in
          let n2, end2 = natural_extract_number s2 i2 in
          let num_cmp = Int.compare n1 n2 in
          if num_cmp <> 0 then num_cmp else compare_at end1 end2
        else
          let char_cmp = Char.compare c1 c2 in
          if char_cmp <> 0 then char_cmp else compare_at (i1 + 1) (i2 + 1)
  in
  compare_at 0 0

let compare_late_modifiers r1 r2 kind1 kind2 =
  let k1 = complex_selector_order kind1 and k2 = complex_selector_order kind2 in
  if k1 <> k2 then Int.compare k1 k2 else compare_by_priority_index r1 r2

let compare_focus_modifiers r1 r2 =
  let outline1 = is_outline_utility r1.base_class in
  let outline2 = is_outline_utility r2.base_class in
  if outline1 && not outline2 then 1
  else if outline2 && not outline1 then -1
  else compare_by_priority_index r1 r2

(** Check if a selector kind is a focus-visible late modifier *)
let is_focus_visible_late_modifier kind selector =
  is_late_modifier kind selector
  &&
  match kind with
  | Complex { has_focus_visible = true; _ } -> true
  | _ -> false

(** Compare focus-visible and state modifier ordering. Returns [Some cmp] if at
    least one rule is a focus-visible or state modifier, [None] otherwise. *)
let compare_focus_visible_state r1 r2 kind1 kind2 =
  let fv1 = is_focus_visible_late_modifier kind1 r1.selector in
  let fv2 = is_focus_visible_late_modifier kind2 r2.selector in
  let s1 = is_state_modifier_rule kind1 r1.selector in
  let s2 = is_state_modifier_rule kind2 r2.selector in
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
  let f1 = is_focus_modifier_rule kind1 r1.selector in
  let f2 = is_focus_modifier_rule kind2 r2.selector in
  if f1 && not f2 then Some 1
  else if f2 && not f1 then Some (-1)
  else if f1 && f2 then Some (compare_focus_modifiers r1 r2)
  else None

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
      let late1 = is_late_modifier kind1 r1.selector in
      let late2 = is_late_modifier kind2 r2.selector in
      if late1 && not late2 then 1
      else if late2 && not late1 then -1
      else if late1 && late2 then compare_late_modifiers r1 r2 kind1 kind2
      else
        natural_compare
          (Css.Selector.to_string r1.selector)
          (Css.Selector.to_string r2.selector)

let compare_cross_utility_regular r1 r2 =
  let p1, s1 = r1.order and p2, s2 = r2.order in
  let kind1 = classify_selector r1.selector in
  let kind2 = classify_selector r2.selector in
  if !debug_compare then (
    let sel1 = Css.Selector.to_string r1.selector in
    let sel2 = Css.Selector.to_string r2.selector in
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
  let same_priority = p1 = p2 in
  match
    if same_priority then
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
          | None -> compare_by_prio_sub_late r1 r2 kind1 kind2))

(** Compare two Regular rules using rule relationship dispatch. *)
let compare_regular_rules r1 r2 =
  let rel = rule_relationship r1 r2 in
  if !debug_compare then
    prerr_string
      (String.concat ""
         [
           "compare_regular: ";
           Css.Selector.to_string r1.selector;
           " vs ";
           Css.Selector.to_string r2.selector;
           " -> ";
           (match rel with
           | Same_utility bc -> "Same:" ^ bc
           | Different_utilities -> "Different");
           "\n";
         ]);
  match rel with
  | Same_utility _ -> compare_same_utility_regular r1 r2
  | Different_utilities -> compare_cross_utility_regular r1 r2

(** Compare two Starting style rules by priority then index. *)
let compare_starting_rules r1 r2 =
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index

(* ======================================================================== *)
(* Main Rule Comparison *)
(* ======================================================================== *)

(* Normalize base_class for lexicographic comparison *)
let normalize_for_sort s =
  String.map
    (function
      | '_' -> ' ' | '[' | ']' -> '~' | '/' -> '|' | ':' -> '!' | c -> c)
    s

(* Compare by normalized base_class, then index *)
let compare_by_base_class r1 r2 =
  let bc1 =
    match r1.base_class with Some s -> normalize_for_sort s | None -> ""
  in
  let bc2 =
    match r2.base_class with Some s -> normalize_for_sort s | None -> ""
  in
  let class_cmp = String.compare bc1 bc2 in
  if class_cmp <> 0 then class_cmp else Int.compare r1.index r2.index

(* Sort key for supports modifier variants: named before bracket *)
let supports_sort_key bc =
  match bc with
  | Some s when String.length s > 9 && String.sub s 0 9 = "supports-" ->
      let after = String.sub s 9 (String.length s - 9) in
      if String.length after > 0 && after.[0] = '[' then (1, after)
      else (0, after)
  | Some s -> (0, s)
  | None -> (0, "")

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
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp
  else
    let sel_cmp =
      natural_compare
        (Css.Selector.to_string r1.selector)
        (Css.Selector.to_string r2.selector)
    in
    if sel_cmp <> 0 then sel_cmp else Int.compare r1.index r2.index

(* Compare by order tuple, then index *)
let compare_by_order_then_index r1 r2 =
  let order_cmp = compare r1.order r2.order in
  if order_cmp <> 0 then order_cmp else Int.compare r1.index r2.index

(* Compare nested media conditions *)
let compare_nested_media r1 r2 =
  match (r1.nested, r2.nested) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | [ n1 ], [ n2 ] -> (
      match (Css.as_media n1, Css.as_media n2) with
      | Some (c1, _), Some (c2, _) -> Css.Media.compare c1 c2
      | _ -> 0)
  | _ -> 0

(* Extract the modifier prefix from a base_class, e.g. "hover:p-4" -> "hover" *)
let variant_prefix = function
  | Some s -> (
      match String.rindex_opt s ':' with
      | Some i -> String.sub s 0 i
      | None -> "")
  | None -> ""

let starts_with s p =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* Compute variant order for a modifier prefix, stripping group-/peer-
   wrappers *)
let strip_group_peer_vo p =
  if starts_with p "group-" then
    Modifiers.variant_order_of_prefix (String.sub p 6 (String.length p - 6))
  else if starts_with p "peer-" then
    Modifiers.variant_order_of_prefix (String.sub p 5 (String.length p - 5))
  else Modifiers.variant_order_of_prefix p

(* Find the first ':' in a string that is not inside brackets. Returns None if
   all colons are inside bracket pairs. *)
let index_colon_outside_brackets s =
  let len = String.length s in
  let rec loop i depth =
    if i >= len then None
    else
      match s.[i] with
      | '[' -> loop (i + 1) (depth + 1)
      | ']' -> loop (i + 1) (max 0 (depth - 1))
      | ':' when depth = 0 -> Some i
      | _ -> loop (i + 1) depth
  in
  loop 0 0

(* Split a string on ':' but respecting bracket nesting, so colons inside [...]
   are not treated as separators. *)
let split_on_colon_outside_brackets s =
  let len = String.length s in
  let buf = Buffer.create 16 in
  let acc = ref [] in
  let rec loop i depth =
    if i >= len then (
      let last = Buffer.contents buf in
      if last <> "" then acc := last :: !acc;
      List.rev !acc)
    else
      match s.[i] with
      | '[' ->
          Buffer.add_char buf '[';
          loop (i + 1) (depth + 1)
      | ']' ->
          Buffer.add_char buf ']';
          loop (i + 1) (max 0 (depth - 1))
      | ':' when depth = 0 ->
          acc := Buffer.contents buf :: !acc;
          Buffer.clear buf;
          loop (i + 1) depth
      | c ->
          Buffer.add_char buf c;
          loop (i + 1) depth
  in
  loop 0 0

(* Compute the inner variant order for a compound prefix like "hover:focus" *)
let inner_vo prefix =
  match index_colon_outside_brackets prefix with
  | Some j ->
      let outer = String.sub prefix 0 j in
      if starts_with outer "group-" || starts_with outer "peer-" then
        let parts = split_on_colon_outside_brackets prefix in
        List.fold_left (fun acc p -> max acc (strip_group_peer_vo p)) 0 parts
        + 1
      else
        let inner = String.sub prefix (j + 1) (String.length prefix - j - 1) in
        Modifiers.variant_order_of_prefix inner
  | None ->
      if starts_with prefix "group-" then
        Modifiers.variant_order_of_prefix
          (String.sub prefix 6 (String.length prefix - 6))
      else if starts_with prefix "peer-" then
        Modifiers.variant_order_of_prefix
          (String.sub prefix 5 (String.length prefix - 5))
      else 0

(* Effective inner variant order: prefer prefix-derived, fall back to nested
   media *)
let effective_ivo r prefix =
  let ivo = inner_vo prefix in
  if ivo > 0 then ivo
  else
    match r.nested with
    | [ n ] -> (
        match Css.as_media n with
        | Some (cond, _) -> Modifiers.variant_order_of_media_cond cond
        | None -> 0)
    | _ -> 0

(** Classify bracket content: pseudo-class brackets ([:checked]) sort before
    combinator/ampersand brackets ([&>img], [+img], etc.). *)
let bracket_content_key p =
  match String.index_opt p '[' with
  | Some i when i + 1 < String.length p ->
      let first_char = p.[i + 1] in
      if first_char = ':' then 0 (* pseudo-class *)
      else 1 (* combinator/ampersand/other *)
  | _ -> 1

(** Check if a prefix is an aria-/data- attribute variant where underscores
    represent spaces. *)
let is_attr_variant p =
  starts_with p "aria-[" || starts_with p "data-["
  || starts_with p "group-aria-["
  || starts_with p "group-data-["
  || starts_with p "peer-aria-["
  || starts_with p "peer-data-["

(** Compare two bracket-containing variant prefixes. Sorts by bracket content
    type (pseudo-class before combinator), then by normalized name for
    aria-/data- attributes, or plain string comparison otherwise. *)
let compare_both_bracket_prefixes p1 p2 =
  let bk_cmp = Int.compare (bracket_content_key p1) (bracket_content_key p2) in
  if bk_cmp <> 0 then bk_cmp
  else if is_attr_variant p1 || is_attr_variant p2 then
    String.compare (normalize_for_sort p1) (normalize_for_sort p2)
  else String.compare p1 p2

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
let compare_variant_ordered r1 r2 =
  match (r1.rule_type, r2.rule_type) with
  | `Supports _, `Supports _ when r1.variant_order = r2.variant_order ->
      compare_supports_by_key r1 r2
  | _ ->
      let vo_cmp = Int.compare r1.variant_order r2.variant_order in
      if vo_cmp <> 0 then vo_cmp
      else
        let p1_prefix = variant_prefix r1.base_class in
        let p2_prefix = variant_prefix r2.base_class in
        let ivo_cmp =
          Int.compare (effective_ivo r1 p1_prefix) (effective_ivo r2 p2_prefix)
        in
        if ivo_cmp <> 0 then ivo_cmp
        else
          let has_nested = function [] -> 0 | _ -> 1 in
          let nested_cmp =
            Int.compare (has_nested r1.nested) (has_nested r2.nested)
          in
          if nested_cmp <> 0 then nested_cmp
          else
            let media_cmp =
              match (r1.rule_type, r2.rule_type) with
              | `Media c1, `Media c2 ->
                  let cmp = Css.Media.compare c1 c2 in
                  if cmp <> 0 then cmp else compare_nested_media r1 r2
              | _ -> 0
            in
            if media_cmp <> 0 then media_cmp
            else
              let prefix_cmp = compare_bracket_prefixes p1_prefix p2_prefix in
              if prefix_cmp <> 0 then prefix_cmp
              else
                let p1, s1 = r1.order and p2, s2 = r2.order in
                let prio_cmp = Int.compare p1 p2 in
                if prio_cmp <> 0 then prio_cmp
                else
                  let sub_cmp = Int.compare s1 s2 in
                  if sub_cmp <> 0 then sub_cmp else compare_by_base_class r1 r2

(* Compare two Supports rules *)
let compare_supports_rules r1 r2 =
  let is_modifier_supports bc =
    match bc with
    | Some s -> String.length s > 9 && String.sub s 0 9 = "supports-"
    | None -> false
  in
  let m1 = is_modifier_supports r1.base_class in
  let m2 = is_modifier_supports r2.base_class in
  if m1 && m2 then compare_supports_by_key r1 r2
  else compare_by_order_then_selector r1 r2

(** Compare indexed rules for sorting. Uses type-directed dispatch based on
    rule_type. This is the main entry point for sorting assembled CSS rules into
    Tailwind v4 cascade order. *)
let compare_indexed_rules r1 r2 =
  (if !debug_compare then
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
            Css.Selector.to_string r1.selector;
            " vs ";
            Css.Selector.to_string r2.selector;
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
  else if r1.not_order > 0 || r2.not_order > 0 then
    let order_cmp = compare r1.order r2.order in
    if order_cmp <> 0 then order_cmp else compare_by_base_class r1 r2
  else
    let type_cmp =
      Int.compare (rule_type_order r1.rule_type) (rule_type_order r2.rule_type)
    in
    if type_cmp <> 0 then type_cmp
    else
      match (r1.rule_type, r2.rule_type) with
      | `Regular, `Regular -> compare_regular_rules r1 r2
      | `Media _, `Media _ ->
          compare_media_rules r1.rule_type r2.rule_type r1.selector r2.selector
            r1.order r2.order r1.index r2.index r1.nested r2.nested
            r1.base_class r2.base_class
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
