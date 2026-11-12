module Css = Cascade.Css
module Index = Cascade.Source_index

(* The at-keywords Tailwind's dialect adds to CSS. They are input for the
   generator, which reads each of them -- for a definition, for an expansion, or
   for the theme and the plugins the entrypoint asks for -- and Tailwind emits
   none of them. This list is the whole of what makes an at-rule one of
   Tailwind's; anything else is the author's CSS, and passes through whether or
   not tw or a parser knows what it means. *)
let tailwind_directives =
  [
    "@apply";
    "@config";
    "@custom-variant";
    "@plugin";
    "@reference";
    "@slot";
    "@source";
    "@tailwind";
    "@theme";
    "@utility";
    "@variant";
  ]

(* Drop them, so none reaches a browser that has no meaning for it. What is
   still here declared nothing usable -- an [@utility] with no name, a variant
   tw cannot expand -- or names something outside the stylesheet, so there is
   nothing to salvage from the text either. *)
let drop_directives css =
  Text_pass.rewrite ~names:tailwind_directives css (fun index _ i ->
      List.find_map
        (fun name ->
          match Index.at_rule index ~name i with
          | Some { block = { next; _ }; _ } -> Some next
          | None ->
              Option.map
                (fun ({ next; _ } : Index.statement) -> next)
                (Index.at_statement index ~name i))
        tailwind_directives)

(* [to_css] heads a utility's selector with the utility's own class, and a
   variant decorates it in place, as [.dark\:fill-gray-400:where(.dark, ...)].
   Swapping that class for [&] turns the rule into a nested one the author's
   selector can host, so a variant survives [@apply] without being reimplemented
   here. The class is not always the leftmost one in the selector: [divide-*]
   wraps it in [:where(.divide-x > :not(:last-child))] and [in-*] heads the
   selector with the ancestor's class instead, so it is picked out by name among
   the classes the [@apply] asked for. A selector naming none of them keeps the
   leftmost class, which is what the variants tw generates put there. *)
let rec heads_with_class pick = function
  | Cascade.Selector.Class name -> pick name
  | Compound parts -> List.exists (heads_with_class pick) parts
  | Combined (left, _, right) ->
      heads_with_class pick left || heads_with_class pick right
  | List arms -> List.exists (heads_with_class pick) arms
  | _ -> false

let rec swap_heading pick = function
  | Cascade.Selector.Class name when pick name -> Cascade.Selector.Nesting
  | Compound parts -> Compound (List.map (swap_heading pick) parts)
  | Combined (left, combinator, right) ->
      Combined (swap_heading pick left, combinator, swap_heading pick right)
  | List arms -> List (List.map (swap_heading pick) arms)
  | node -> node

(* The utility's class can also stand inside a pseudo-class argument, as the
   typography plugin's [:where(.prose > ul > li p)] under [.prose] does. Where
   the class also stands outside every argument, that occurrence is the one the
   applying rule takes the place of, and the one inside is a descendant test
   that stays. Only a class found nowhere else, as [divide-*] puts it in
   [:where()], is swapped where it sits. *)
let nest_on_ampersand ~classes sel =
  let swap pick =
    Cascade.Selector.map (function
      | Cascade.Selector.Class name when pick name -> Cascade.Selector.Nesting
      | node -> node)
  in
  let own name = List.mem name classes in
  let arm a =
    if heads_with_class own a then swap_heading own a
    else if Cascade.Selector.exists_class own a then swap own a
    else
      match Cascade.Selector.first_class a with
      | Some name -> swap (String.equal name) a
      | None -> a
  in
  match Cascade.Selector.as_list sel with
  | Some arms -> Cascade.Selector.list (List.map arm arms)
  | None -> arm sel

let render_nested_utilities ~classes stmts =
  stmts
  |> Css.map (fun sel decls ->
      Css.rule ~selector:(nest_on_ampersand ~classes sel) decls)
  |> Statements.merge_same_selector |> Css.v |> Css.to_string ~minify:true

(* The declarations of [names], rewritten to nest under the [&] of whatever rule
   applies them, plus the statements that must stay at the top of the sheet. A
   [@layer properties] block holds the initial value of the variables the
   utilities set, on the universal selector: nested under an author rule it
   would come out as [.box *], so it is hoisted instead. *)
(* What goes to the top of the sheet rather than under the [&] that applied the
   utilities: a rule naming no class, which is the [:root] block declaring the
   theme tokens they read, and the three at-rules that stand on their own -
   [@layer properties] (nested it would come out as [.box *]), [@property], and
   the [@keyframes] an animation utility names. An at-rule that wraps utility
   rules - [@supports], [@media] - is not one of those: it nests with what it
   holds.

   The theme block used to be dropped here rather than hoisted, so
   [@apply rounded-lg] emitted a rule reading [var(--radius-lg)] with nothing
   in the sheet declaring it. Colours and spacing hid it for a long time: those
   two are rescued downstream by [Build.referenced_theme_decls], so only the
   families carrying their own namespace showed the defect. *)
let stands_at_top stmt =
  match Css.statement_selector stmt with
  | Some sel -> not (Css.Selector.exists_class (fun _ -> true) sel)
  | None ->
      Css.as_layer stmt <> None
      || Css.as_property stmt <> None
      || Css.as_keyframes stmt <> None

(* The theme block goes back inside [@layer theme], where [merge_named_layers]
   folds it into the generated sheet's own block. An at-rule stands at top
   level, which is where Tailwind emits it. *)
let at_top_of_sheet stmt =
  match Css.statement_selector stmt with
  | None -> stmt
  | Some _ -> Css.layer ~name:[ "theme" ] [ stmt ]

let nested_utilities ~theme names =
  (* The generated sheet already declares a [theme(static)] theme whole, with
     every keyframe, so a render that only lends its declarations to author CSS
     or a routed variant must not declare it again. *)
  let theme = { theme with Tw.Scheme.static_theme = false } in
  let of_name n =
    match Tw.of_string ~theme n with Ok s -> Some s | Error _ -> None
  in
  match List.filter_map of_name names with
  | [] -> ("", [])
  | styles ->
      let sheet =
        Tw.to_css ~theme ~base:false ~forms:false ~layers:false styles
      in
      (* The class each utility carries in its own selector, spelled the way
         [to_css] spells it rather than the way the [@apply] did. *)
      let classes = String.split_on_char ' ' (Tw.to_classes styles) in
      let top, nestable = List.partition stands_at_top (Css.statements sheet) in
      let hoisted = List.map at_top_of_sheet top in
      (* One [@apply] pulls in several utilities, each with a rule of its own.
         They all decorate the same [&], so they belong in one rule, the way
         Tailwind emits them; left apart, each is a rule of the author's
         selector holding one declaration. *)
      (* The hoisted statements go back one by one, not as one block: two
         utilities bring overlapping [@property] sets, and deduping the blocks
         whole re-emits every property they do not share. *)
      (render_nested_utilities ~classes nestable, hoisted)

(* Append each statement unless it is already there: every utility that sets the
   same variable brings back the same hoisted [@property]. *)
let add_once buf seen items =
  List.iter
    (fun stmt ->
      if not (Statements.seen seen stmt) then
        Buffer.add_string buf (Css.to_string ~minify:true (Css.v [ stmt ])))
    items

let apply_names css start stop =
  Tw.split_whitespace (String.sub css start (stop - start))

let emit_apply_name ~theme ~defs ~udefs ~buf ~hoisted ~seen name =
  let variants, bare = Custom_variant.split_declared defs name in
  (* [@apply line-t] names a utility the project declared, whose body is author
     CSS in its own right. *)
  let body, top =
    match List.assoc_opt bare udefs with
    | Some decls -> (decls, [])
    | None -> (
        match Custom_utility.functional_body ~theme ~udefs bare with
        | Some decls -> (decls, [])
        | None -> nested_utilities ~theme [ bare ])
  in
  add_once hoisted seen top;
  if body <> "" then begin
    List.iter
      (fun variant ->
        Buffer.add_string buf (String.concat "" [ "@variant "; variant; "{" ]))
      variants;
    Buffer.add_string buf body;
    List.iter (fun _ -> Buffer.add_char buf '}') variants
  end

let plain_apply_name ~defs ~udefs name =
  match Custom_variant.split_declared defs name with
  | [], bare
    when (not (List.mem_assoc bare udefs))
         && not (Custom_utility.is_functional_candidate udefs bare) ->
      Some bare
  | _ -> None

let rec take_plain_apply_run ~defs ~udefs acc = function
  | name :: rest when Option.is_some (plain_apply_name ~defs ~udefs name) ->
      take_plain_apply_run ~defs ~udefs (name :: acc) rest
  | rest -> (List.rev acc, rest)

let rec emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen = function
  | [] -> ()
  | name :: rest as names -> (
      match plain_apply_name ~defs ~udefs name with
      | None ->
          emit_apply_name ~theme ~defs ~udefs ~buf ~hoisted ~seen name;
          emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen rest
      | Some _ ->
          let run, rest = take_plain_apply_run ~defs ~udefs [] names in
          let bare = List.filter_map (plain_apply_name ~defs ~udefs) run in
          let body, top = nested_utilities ~theme bare in
          add_once hoisted seen top;
          Buffer.add_string buf body;
          emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen rest)

(* Tailwind's [@apply] pulls a utility's declarations into an author rule. It is
   not CSS, so the at-rule drops out and takes the whole rule with it once the
   rule is left empty. *)
let expand_apply ~theme ~defs ?(udefs = []) css =
  let hoisted = Buffer.create 0 in
  let seen = Hashtbl.create 64 in
  let css =
    Text_pass.rewrite ~names:[ "@apply" ] css (fun index buf i ->
        match Index.at_statement index ~name:"@apply" i with
        | Some { prelude; next } ->
            (* A utility with no declared variant and no body of its own
               decorates the applying rule's [&] directly. A run of those
               renders in one call, so their declarations land in a single rule
               the way Tailwind emits them, rather than one rule of the author's
               selector per utility. *)
            let names = apply_names prelude 0 (String.length prelude) in
            emit_apply_names ~theme ~defs ~udefs ~buf ~hoisted ~seen names;
            Some next
        | None -> None)
  in
  (* The hoisted blocks go last: their layer is ordered by the sheet's [@layer]
     statement, not by where they sit. *)
  if Buffer.length hoisted = 0 then css else css ^ Buffer.contents hoisted

(* The names an [@variant NAME {] header uses inside a body. *)
let variant_names_in css =
  if not (Text_pass.mentions "@variant" css) then []
  else
    let index = Index.v css in
    let len = String.length css in
    let rec go i acc =
      if i >= len then List.rev acc
      else
        match Index.at_rule index ~name:"@variant" i with
        | Some { prelude; brace; _ } when prelude <> "" ->
            go (brace + 1) (prelude :: acc)
        | _ -> go (i + 1) acc
    in
    go 0 []

(* The [@variant] body a built-in variant expands to. [Tw.of_string] knows the
   variants, but only as part of a whole utility, so derive the wrapper from
   what it emits around a probe with a single declaration and put [@slot] where
   that declaration was. This is what lets a project's [@utility] carry a
   built-in prefix, which the [@variant] machinery otherwise only has templates
   for when the project declared it. *)
(* Replace every occurrence of [needle] in [hay]. *)
let replace_all ~needle ~by hay =
  Re.replace_string (Re.compile (Re.str needle)) ~by hay

(* A rule that is the bare [&] alone, as a media variant wraps the probe in,
   adds a nesting level the utility's own body cannot survive: its [@variant
   before] and the [@supports] an opacity colour emits end up three deep and the
   sheet no longer parses. That level goes, so the slot takes its place; a [&]
   that ends a longer selector, [:where([data-stack]) &], is the rule's own and
   stays. *)
let drop_bare_amp_rules hay =
  let needle = "&{float:none}" in
  let n = String.length needle in
  let buf = Buffer.create (String.length hay) in
  let rec go i =
    if i >= String.length hay then ()
    else if
      i + n <= String.length hay
      && String.sub hay i n = needle
      && (i = 0 || match hay.[i - 1] with '{' | '}' | ';' -> true | _ -> false)
    then begin
      Buffer.add_string buf "float:none";
      go (i + n)
    end
    else begin
      Buffer.add_char buf hay.[i];
      go (i + 1)
    end
  in
  go 0;
  Buffer.contents buf

(* The template a built-in variant stands for, derived from tw's own render of a
   probe utility under it: every probe declaration becomes the slot, so a
   variant of several rules, [marker:]'s three, slots each of them. *)
let builtin_variant_template ~theme name =
  let body, _ = nested_utilities ~theme [ name ^ ":float-none" ] in
  if body = "" then None
  else
    Some
      (replace_all ~needle:"float:none" ~by:"@slot;" (drop_bare_amp_rules body))

let expand ?(extra_defs = []) ?(udefs = []) ~theme css =
  (* What an [@apply] pulls into the author's CSS is not a utility, so the
     import's [important] does not reach it. *)
  let theme = { theme with Tw.Scheme.important = false } in
  (* The [@utility] declarations taken out here are the ones an [@apply] in this
     same file names. Dropping them left [@apply card] resolving against an
     empty table, so it named no utility and the rule it decorated came out
     without the declarations the custom utility carries - silently, since an
     [@apply] that names nothing is not an error. *)
  let css, own_udefs = Custom_utility.take css in
  let udefs = udefs @ own_udefs in
  let css, defs = Custom_variant.take css in
  let defs = defs @ extra_defs in
  (* A project declaration wins over the built-in of the same name. Any other
     built-in the CSS names has its template derived from tw's own output for a
     probe utility, so [@variant sm] is not silently dropped along with the
     declarations it guards. *)
  let derived =
    variant_names_in css
    |> List.sort_uniq String.compare
    |> List.filter (fun n -> not (List.mem_assoc n defs))
    |> List.filter_map (fun n ->
        Option.map (fun t -> (n, t)) (builtin_variant_template ~theme n))
  in
  let defs = defs @ Custom_variant.builtins @ derived in
  (* A declared utility's body may [@apply] another one, so keep expanding until
     nothing is left (bounded, in case two reference each other). *)
  let rec expand depth css =
    let out = expand_apply ~theme ~defs ~udefs css in
    if depth >= 4 || String.equal out css then out else expand (depth + 1) out
  in
  drop_directives
    (Value_fn.resolve_theme ~theme
       (Value_fn.expand_alpha
          (Value_fn.resolve_dashed_theme ~theme
             (Value_fn.expand_spacing ~theme
                (Custom_variant.expand defs (expand 0 css))))))
