module Css = Cascade.Css

(* The escaped class with its declarations under the [@variant]s that wrap
   it. *)
let wrapped_block cls variants body =
  let class_sel = Css.Selector.to_string (Css.Selector.Class cls) in
  let wrapped =
    List.fold_right
      (fun v acc -> String.concat "" [ "@variant "; v; "{"; acc; "}" ])
      variants body
  in
  String.concat "" [ class_sel; "{"; wrapped; "}" ]

(* A candidate the project's own declarations govern: it carries a declared
   variant, it is a declared utility, or it reads as one of a declared
   functional utility's candidates. *)
(* The [!] a candidate carries, as a suffix or the v3 prefix, and the name
   under it: Tailwind marks every declaration of the utility [!important]. *)
let importance name =
  let n = String.length name in
  if n > 1 && name.[n - 1] = '!' then (true, String.sub name 0 (n - 1))
  else if n > 1 && name.[0] = '!' then (true, String.sub name 1 (n - 1))
  else (false, name)

let is_custom_routed ~theme ~defs ~udefs cls =
  let variants, bare = Custom_variant.split_declared defs cls in
  let segs = Custom_variant.segments bare in
  let _, name = importance (List.nth segs (List.length segs - 1)) in
  variants <> [] || List.mem_assoc name udefs
  (* A functional declaration claims a candidate its value reads resolve for and
     no other: Tailwind tries every utility registered for a root, so [tab-[13]]
     under [@utility tab-* { tab-size: --value(integer) }] falls to the built-in
     [tab-*], which takes the bracket. *)
  || Option.is_some (Custom_utility.functional_body ~theme ~udefs name)

(* Candidates the built-in generator cannot produce: a variant the project
   redefined via [@custom-variant] (e.g. a class-based [dark:]), which
   [Tw.of_string] only knows in its built-in [@media (prefers-color-scheme:
   dark)] form, and a class the project declared with [@utility], which
   [Tw.of_string] does not know at all. Both go through the same expansion the
   author CSS uses: the declarations land under the escaped class, wrapped in
   the declared variants, and cascade flattens the nesting into the project's
   selector. *)
(* The class a routed rule belongs to: its selector's first class, which is the
   declared utility itself for both [.line-y] and [.line-y:before]. *)
let rec first_class_of_statement stmt =
  match Css.as_rule stmt with
  | Some (selector, _, _) -> Css.Selector.first_class selector
  | None -> (
      match Css.as_media stmt with
      | Some (_, inner) -> List.find_map first_class_of_statement inner
      | None -> (
          match Css.as_supports stmt with
          | Some (_, inner) -> List.find_map first_class_of_statement inner
          | None -> None))

(* Where a declared utility sorts: the slot of the property it writes first. *)
let rec slot_of_statement stmt =
  match Css.as_rule stmt with
  | Some (_, d :: _, _) ->
      Tw.Utility.order_of_property (Css.Declaration.property_key d)
  | Some (_, [], _) -> None
  | None -> (
      match Css.as_media stmt with
      | Some (_, inner) -> List.find_map slot_of_statement inner
      | None -> (
          match Css.as_supports stmt with
          | Some (_, inner) -> List.find_map slot_of_statement inner
          | None -> None))

let routed_template ~theme derived name =
  match Hashtbl.find_opt derived name with
  | Some template -> template
  | None ->
      let template = Author_css.builtin_variant_template ~theme name in
      Hashtbl.add derived name template;
      template

let record_routed_order ~theme own_order cls name =
  match Tw.Utility.base_of_class theme name with
  | Ok base -> Hashtbl.replace own_order cls (Tw.Utility.order base)
  | Error _ -> ()

(* Tailwind applies the variants left to right, each putting the selector so far
   in its [&], so a declared variant sits where it was written among the
   built-in ones: [hover:dark:] is [.x:hover:where(...)] and [dark:hover:] is
   [.x:where(...):hover]. Every prefix becomes a [@variant] in the written order
   while each built-in one has a template, and the block then wraps the bare
   utility's declarations; what the sheet hoists for it, the [@property] a
   [before:] registers for its content, still comes off the built-in prefixes. A
   built-in prefix without a template stays in the utility, and the declared
   ones wrap it. *)
let routed_body ~theme ~ordered ~name ~bare =
  if ordered then
    let body, _ = Author_css.nested_utilities ~theme [ name ] in
    (body, snd (Author_css.nested_utilities ~theme [ bare ]))
  else Author_css.nested_utilities ~theme [ bare ]

let routed_block ~theme ~defs ~udefs ~hoisted ~seen ~derived ~own_order cls =
  let variants, bare = Custom_variant.split_declared defs cls in
  (* [bare] still carries the built-in prefixes; the utility itself is its last
     segment. *)
  let segments = Custom_variant.segments bare in
  let last = List.length segments - 1 in
  let _, name = importance (List.nth segments last) in
  let builtin = List.filteri (fun index _ -> index < last) segments in
  (* Every prefix of the candidate, declared and built-in, as written. *)
  let prefixes =
    let all = Custom_variant.segments cls in
    List.filteri (fun index _ -> index < List.length all - 1) all
  in
  (* Every body declared for the name, in the order they were written: Tailwind
     registers each [@utility] of a name and applies them all, so a second
     declaration adds to the first rather than replacing it. *)
  let declared =
    match List.filter (fun (n, _) -> n = name) udefs with
    | _ :: _ as declared -> Some (String.concat "" (List.map snd declared))
    | [] -> Custom_utility.functional_body ~theme ~udefs name
  in
  match declared with
  | Some body ->
      (* A declared utility means nothing to [Tw.of_string], so every prefix has
         to become a [@variant], the built-in ones included. *)
      if
        List.for_all
          (fun variant ->
            Option.is_some (routed_template ~theme derived variant))
          builtin
      then Some (wrapped_block cls prefixes body)
      else None
  | None when variants = [] -> None
  | None ->
      let ordered =
        List.for_all
          (fun variant ->
            List.mem_assoc variant defs
            || Option.is_some (routed_template ~theme derived variant))
          prefixes
      in
      (* A built-in utility reads its own [!]; the mark stays on the name. *)
      let name = List.nth segments last in
      let body, top = routed_body ~theme ~ordered ~name ~bare in
      Author_css.add_once hoisted seen top;
      if body = "" then None
      else begin
        record_routed_order ~theme own_order cls name;
        Some (wrapped_block cls (if ordered then prefixes else variants) body)
      end

let collect_routed_templates ~theme derived udefs =
  List.iter
    (fun (_, body) ->
      List.iter
        (fun name -> ignore (routed_template ~theme derived name))
        (Author_css.variant_names_in body))
    udefs

let routed_variant_defs defs derived =
  defs
  @ Hashtbl.fold
      (fun name template acc ->
        match template with Some body -> (name, body) :: acc | None -> acc)
      derived []

(* A parsed routed statement keeps the candidate that produced its block.
   Selector recovery remains the fallback for independently hoisted statements,
   but cannot own a compound selector whose leading [:where(...)] hides the
   candidate class from [first_class_of_statement]. *)
let routed_owner owner stmt =
  match owner with Some _ -> owner | None -> first_class_of_statement stmt

let group_routed_rules ~own_order rules =
  let group = Hashtbl.create 8 in
  let order_of = Hashtbl.create 8 in
  let classless = ref [] in
  List.iter
    (fun (owner, stmt) ->
      match routed_owner owner stmt with
      | None -> classless := stmt :: !classless
      | Some cls -> (
          let prev = Option.value ~default:[] (Hashtbl.find_opt group cls) in
          Hashtbl.replace group cls (prev @ [ stmt ]);
          if (not (Hashtbl.mem own_order cls)) && not (Hashtbl.mem order_of cls)
          then
            match slot_of_statement stmt with
            | Some order -> Hashtbl.add order_of cls order
            | None -> ()))
    rules;
  (group, order_of, List.rev !classless)

let routed_slot ~own_order ~order_of cls =
  match Hashtbl.find_opt own_order cls with
  | Some order -> order
  | None ->
      Option.value ~default:(max_int, max_int) (Hashtbl.find_opt order_of cls)

(* Tailwind counts every declaration in a utility's AST, including ones in
   nested rules and at-rules. [Css.fold] follows every kind of nested block, so
   this does not need a brittle list of the block at-rules cascade knows. *)
let routed_declaration_count stmts =
  Css.fold
    (fun count stmt ->
      match Css.as_rule stmt with
      | Some (_, declarations, _) -> count + List.length declarations
      | None -> (
          match Css.as_declarations stmt with
          | Some declarations -> count + List.length declarations
          | None -> count))
    0 (Css.v stmts)

let compare_routed_entries ~own_order ~order_of (c1, stmts1) (c2, stmts2) =
  let p1, s1 = routed_slot ~own_order ~order_of c1 in
  let p2, s2 = routed_slot ~own_order ~order_of c2 in
  let priority = Int.compare p1 p2 in
  let suborder = if priority <> 0 then priority else Int.compare s1 s2 in
  if suborder <> 0 then suborder
  else
    let count =
      Int.compare
        (routed_declaration_count stmts2)
        (routed_declaration_count stmts1)
    in
    if count <> 0 then count else String.compare c1 c2

(* Within one declared utility, the rules it writes outright come before the
   ones a variant wrapped in an at-rule, the order the generator gives a
   built-in utility and its own media queries. *)
let unwrapped_first stmts =
  let plain, wrapped =
    List.partition (fun stmt -> Option.is_some (Css.as_rule stmt)) stmts
  in
  plain @ wrapped

let ordered_routed_entries ~own_order ~order_of group =
  Hashtbl.fold
    (fun cls stmts acc -> (cls, unwrapped_first stmts) :: acc)
    group []
  |> List.sort (compare_routed_entries ~own_order ~order_of)

let routed_statements ~block_count ~own_order stmts =
  (* [@layer properties], [@property] and [@keyframes] sit beside the utilities
     layer, not in it: nested, the first would become [utilities.properties],
     and the [@keyframes] an applied animation utility names would go into the
     classless bucket and be wrapped in a utilities layer of its own, which is
     not where Tailwind puts it. *)
  let hoisted, rules =
    List.partition
      (fun (_, stmt) ->
        Css.as_layer stmt <> None
        || Css.as_property stmt <> None
        || Css.as_keyframes stmt <> None)
      stmts
  in
  let group, order_of, classless = group_routed_rules ~own_order rules in
  (* A declared utility whose first property tw has no slot for still belongs
     among the utilities, at the end, so it goes over with the rest: sorted with
     them, and read by the theme layer for the tokens it names. A statement
     naming no class at all has nothing to sort by, and gets a utilities layer
     of its own after them. *)
  let ordered =
    ordered_routed_entries ~own_order ~order_of group
    |> List.map (fun (cls, stmts) ->
        (cls, routed_slot ~own_order ~order_of cls, stmts))
  in
  let unplaced =
    if classless = [] then [] else [ Css.layer ~name:[ "utilities" ] classless ]
  in
  (block_count, ordered, unplaced @ Statements.dedup (List.map snd hoisted))

(* Flattening is what turns the wrappers a variant builds around a [@utility]
   body into selectors: [.focus\:line-y { &:focus { ... } }] has to become
   [.focus\:line-y:focus]. The body's own nesting is not a wrapper, and Tailwind
   keeps it - [.line-y { padding: 5px; &::before { color: red } }] is one block
   in its output. A rule already carrying declarations of its own is the utility
   rather than a wrapper, so it goes through as written; flattening it would
   split the utility into a rule per selector, each sorting by the property it
   writes. *)
let flattened_statement stmt =
  match Css.as_rule stmt with
  | Some (_, _ :: _, nested)
    when List.for_all (fun st -> Css.as_rule st <> None) nested ->
      [ stmt ]
  | _ -> Css.statements (Css.flatten_nesting (Css.v [ stmt ]))

(* The statements of one generated block, or none when it will not parse at all.
   A malformed body has to cost its own class and nothing else: read as one
   assembled sheet, an unclosed brace nests every block written after it inside
   the broken one, and a parse the recovery cannot save loses the lot. *)
let parse_routed_block css =
  match Css.of_string css with
  | Error _ -> None
  | Ok parsed ->
      Some
        (Css.statements parsed.Css.stylesheet
        |> List.concat_map flattened_statement)

(* The [!] a candidate carries marks every declaration of the utility
   [!important], as Tailwind marks them; a declared utility's body is author CSS
   the parse above reads, so the mark goes on once the block is typed. *)
let important_block cls stmts =
  let segments = Custom_variant.segments cls in
  let important, _ =
    importance (List.nth segments (List.length segments - 1))
  in
  if not important then stmts
  else
    Css.statements
      (Css.Stylesheet.map_declarations (List.map Css.important) (Css.v stmts))

let parse_routed_blocks ~own_order ~hoisted blocks =
  let parsed =
    List.filter_map
      (fun (cls, block) ->
        Option.map
          (fun stmts ->
            List.map (fun stmt -> (Some cls, stmt)) (important_block cls stmts))
          (parse_routed_block block))
      blocks
  in
  let hoisted =
    Option.value ~default:[] (parse_routed_block hoisted)
    |> List.map (fun stmt -> (None, stmt))
  in
  List.concat parsed @ hoisted
  |> routed_statements ~block_count:(List.length parsed) ~own_order

(* A declared utility hoists the same [@layer properties] fallback block the
   generated sheet emits, and that block belongs where the sheet puts its own:
   ahead of the theme, not after the utilities it initialises. The rest of what
   it hoists follows the sheet. *)
let place_routed stmts sheet =
  let is_properties_layer stmt =
    match Css.layer_block_name stmt with
    | Some name -> Css.Stylesheet.equal_layer_name name [ "properties" ]
    | None -> false
  in
  match List.partition is_properties_layer stmts with
  | [], [] -> sheet
  | lead, trail -> Css.v (lead @ Css.statements sheet @ trail)

let custom_routed_utilities ~theme ~defs ~udefs candidates =
  let hoisted = Buffer.create 0 in
  let seen = Hashtbl.create 64 in
  let derived = Hashtbl.create 8 in
  let own_order = Hashtbl.create 8 in
  let blocks =
    List.filter_map
      (fun cls ->
        Option.map
          (fun block -> (cls, block))
          (routed_block ~theme ~defs ~udefs ~hoisted ~seen ~derived ~own_order
             cls))
      candidates
  in
  match blocks with
  | [] -> (0, [], [])
  | _ ->
      collect_routed_templates ~theme derived udefs;
      let extra_defs = routed_variant_defs defs derived in
      (* A [@utility] body is author CSS: it may hold [@apply], [@variant] and
         the [--spacing()]/[theme()] shorthands. Each block is expanded and read
         on its own so one unparseable body cannot take the others down. *)
      let expand = Author_css.expand ~extra_defs ~udefs ~theme in
      parse_routed_blocks ~own_order
        ~hoisted:(expand (Buffer.contents hoisted))
        (List.map (fun (cls, block) -> (cls, expand block)) blocks)
