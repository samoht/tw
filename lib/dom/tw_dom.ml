module Css = Cascade.Css
(* Client-side CSS injection.

   Maintains [<style data-tw="runtime">] elements in [<head>] containing CSS for
   all utilities registered via [use]. Each utility's class name is tracked to
   avoid re-injection.

   A full [Tw.to_css] over every utility seen so far, run on every newly
   discovered class, costs 1 + 2 + ... + n compilations over a mount pass that
   introduces n distinct classes - O(n^2). Rebuilds are coalesced onto a
   microtask (a mount pass compiles once, not once per [use] call), but that
   alone does not bound the *total* work over the app's lifetime: a class
   trickling in on its own microtask on every tick still hits the same pattern,
   one full recompile per tick. [flush] instead keeps one "consolidated" element
   holding a single from-scratch, fully sorted and deduplicated compile, and
   injects each tick's new classes as their own small "delta" element compiled
   from just that tick's batch - O(that batch) instead of O(everything so far).
   Deltas fold back into the consolidated element once they have grown to match
   it, the same growth-factor rule a dynamic array doubles by, which keeps the
   *total* cost of every [Tw.to_css] call across the app's lifetime O(n) in
   distinct classes rather than O(n^2). Base (Preflight) is never recompiled per
   delta - it does not depend on which utilities are present, so it is folded in
   once, at each consolidation. *)

open Brr

let registered : (string, unit) Hashtbl.t = Hashtbl.create 128

(* Every utility ever registered, newest-first (prepended in O(1)); reversed
   only at a consolidation. *)
let all_styles : Tw.t list ref = ref []
let all_styles_count = ref 0

(* Styles [use] has registered since the last [flush], newest-first. *)
let since_flush : Tw.t list ref = ref []

(* The prefix of [all_styles] (by count) already folded into [main_el] by a
   from-scratch compile. *)
let consolidated_count = ref 0
let main_el : El.t option ref = ref None
let delta_els : El.t list ref = ref []
let include_base : bool ref = ref true

let mk_style_el () =
  let el = El.v El.Name.style [] in
  El.set_at (Jstr.v "data-tw") (Some (Jstr.v "runtime")) el;
  el

let ensure_style_el () =
  match !main_el with
  | Some el -> el
  | None ->
      let head = Document.head G.document in
      let el = mk_style_el () in
      El.append_children head [ el ];
      main_el := Some el;
      el

let set_text_content el s = El.set_children el [ El.txt (Jstr.v s) ]

(* Fold everything registered so far into [main_el] with one from-scratch
   compile, matching exactly what a full rebuild produces today, and drop the
   delta elements it now supersedes. *)
let consolidate () =
  let el = ensure_style_el () in
  let styles = List.rev !all_styles in
  let css = Tw.to_css ~base:!include_base styles in
  set_text_content el (Css.to_string ~minify:true css);
  List.iter El.remove !delta_els;
  delta_els := [];
  consolidated_count := !all_styles_count

(* Compile and inject just [batch] (this tick's new styles) as its own small
   element, appended after everything already in the document. [base] is never
   included here - see the module comment - so a batch this only picks up
   utility rules, folded into the canonical, deduplicated compile at the next
   consolidation. *)
let inject_delta batch =
  let head = Document.head G.document in
  let css = Tw.to_css ~base:false batch in
  let el = mk_style_el () in
  El.append_children head [ el ];
  set_text_content el (Css.to_string ~minify:true css);
  delta_els := !delta_els @ [ el ]

let pending : bool ref = ref false

let flush () =
  if !pending then (
    pending := false;
    let batch = List.rev !since_flush in
    since_flush := [];
    if batch <> [] then (
      inject_delta batch;
      let pending_count = !all_styles_count - !consolidated_count in
      if pending_count >= max 1 !consolidated_count then consolidate ()))

(* Coalesced onto a microtask, which the browser drains at the end of the
   current task and before it paints, so a mount pass calling [use] many times
   still injects once. *)
let schedule_rebuild () =
  if not !pending then (
    pending := true;
    Fut.await (Fut.return ()) flush)

let init ?(base = true) () =
  include_base := base;
  ignore (ensure_style_el ())

let use styles =
  let new_found = ref false in
  List.iter
    (fun s ->
      let cls = Tw.pp s in
      if not (Hashtbl.mem registered cls) then (
        Hashtbl.add registered cls ();
        all_styles := s :: !all_styles;
        incr all_styles_count;
        since_flush := s :: !since_flush;
        new_found := true))
    styles;
  if !new_found then schedule_rebuild ();
  Tw.to_classes styles

(* Names [use_str] was given that are no utility, newest-first and each recorded
   once. Rendering must not stop for one: a class attribute legitimately carries
   names this library knows nothing about, and a browser cannot tell those from
   a typo any better than the parser can. The name reaches the element either
   way; this list is what makes the typo findable. *)
let unknown_seen : (string, unit) Hashtbl.t = Hashtbl.create 16
let unknown_rev : string list ref = ref []
let unknown_classes () = List.rev !unknown_rev

let use_str s =
  let styles, unknown = Tw.of_classes s in
  ignore (use styles);
  List.iter
    (fun cls ->
      if not (Hashtbl.mem unknown_seen cls) then (
        Hashtbl.add unknown_seen cls ();
        unknown_rev := cls :: !unknown_rev))
    unknown;
  s

let css () =
  let styles = List.rev !all_styles in
  let css = Tw.to_css ~base:!include_base styles in
  Css.to_string ~minify:true css
