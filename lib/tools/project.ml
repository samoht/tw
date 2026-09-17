(* [prose] comes from @tailwindcss/typography, which Tailwind only applies when
   the entrypoint asks for it. A project that styles [.prose] itself, as
   tailwindcss.com does, gets the plugin's whole stylesheet on top otherwise. *)
let declares_plugin css name =
  match css with
  | None -> false
  | Some css -> Re.execp (Re.compile (Re.str ("@tailwindcss/" ^ name))) css

let is_prose_class cls =
  cls = "prose"
  || String.starts_with ~prefix:"prose-" cls
  ||
  (* variants keep the utility at the end: [lg:prose-sm] *)
  match String.rindex_opt cls ':' with
  | Some i ->
      let bare = String.sub cls (i + 1) (String.length cls - i - 1) in
      bare = "prose" || String.starts_with ~prefix:"prose-" bare
  | None -> false

let raised_rendering = function
  | Invalid_argument _ | Failure _ | Cascade.Error.Parse_error _ -> true
  | _ -> false

(* A handler may accept a class at parse yet raise when it renders an arbitrary
   value it cannot serialise. Such a class produces no rule, so it is dropped
   rather than let it abort the whole sheet. The sheet is rendered once for
   every class, and only a render that raises splits the classes, down to those
   that do: a separate probe render before the real one cost as much as the real
   one. *)
let render_known render known =
  match render known with
  | sheet -> (known, sheet)
  | exception e when raised_rendering e ->
      let renders known =
        match render known with
        | (_ : Cascade.Css.t) -> true
        | exception e when raised_rendering e -> false
      in
      let rec keep = function
        | [] -> []
        | known when renders known -> known
        | [ _ ] -> []
        | known ->
            let half = List.length known / 2 in
            keep (List.filteri (fun i _ -> i < half) known)
            @ keep (List.filteri (fun i _ -> i >= half) known)
      in
      let known = keep known in
      (known, render known)

let parse_known_candidates ~theme ?input_css candidates =
  let typography = declares_plugin input_css "typography" in
  List.filter_map
    (fun cls ->
      if (not typography) && is_prose_class cls then None
      else
        match Tw.of_string ~theme cls with
        | Ok style -> Some (cls, style)
        | Error _ -> None)
    candidates

(* A comparison against the real Tailwind has to be made against the whole of
   this, not against the built-in utilities alone: Tailwind reads the same
   entrypoint, so every declared utility would otherwise read as a rule tw
   failed to emit. *)
let utilities ~theme ?entrypoint ~base classes =
  let input_css = Option.map Entrypoint.read_file entrypoint in
  let declared = Entrypoint.entry_variant_defs entrypoint in
  let defs, refused = Entrypoint.with_negated_variants declared in
  let udefs = Entrypoint.entry_utility_defs entrypoint in
  let names_refused cls =
    List.exists (fun v -> List.mem v refused) (Entrypoint.variant_segments cls)
  in
  let classes = List.filter (fun cls -> not (names_refused cls)) classes in
  let routed, normal =
    List.partition (Entrypoint.is_custom_routed ~theme ~defs ~udefs) classes
  in
  let known = parse_known_candidates ~theme ?input_css normal in
  let routed_count, routed_extra, routed_stmts =
    Entrypoint.custom_routed_utilities ~theme ~defs ~udefs routed
  in
  (* Routed custom variants no longer pass through the typed modifier parser,
     but the sorter still needs their exact names so a declaration such as
     [not-dark] is not mistaken for the built-in [not-] compound slot. Dummy
     selector values are sufficient here: routed candidates already carry the
     expanded author CSS in [extra], and only the registered names are read. *)
  let sort_theme =
    let custom = Tw.Scheme.{ values = [ ("", "&") ]; template = "{}" } in
    let custom_variants =
      List.fold_left
        (fun variants (name, _) ->
          if List.mem_assoc name variants then variants
          else (name, custom) :: variants)
        theme.Tw.Scheme.custom_variants declared
    in
    { theme with custom_variants }
  in
  (* The forms plugin's reset belongs to the base layer, which [to_css] writes
     only when it is asked for. *)
  let forms = Option.fold ~none:false ~some:Entrypoint.forms_base input_css in
  let known, sheet =
    render_known
      (fun known ->
        Tw.to_css ~theme:sort_theme ~base ~forms ~extra:routed_extra
          (List.map snd known))
      known
  in
  (List.length known + routed_count, Entrypoint.place_routed routed_stmts sheet)

let stylesheet ~theme ?entrypoint ~base classes =
  let input_css = Option.map Entrypoint.read_file entrypoint in
  (* An entrypoint importing Tailwind in parts asks for the reset only through
     [tailwindcss/preflight.css]. *)
  let base =
    base && Option.fold ~none:true ~some:Entrypoint.imports_preflight input_css
  in
  (* The entrypoint's safelist joins the markup's classes, and its blocklist
     takes a class out whichever of the two it came from. *)
  let classes =
    match input_css with
    | None -> classes
    | Some css ->
        let safelist, blocklist = Entrypoint.source_inline css in
        List.sort_uniq String.compare (classes @ safelist)
        |> List.filter (fun cls -> not (List.mem cls blocklist))
  in
  let count, sheet = utilities ~theme ?entrypoint ~base classes in
  match entrypoint with
  | Some path -> (count, Entrypoint.splice_into_entrypoint ~theme ~path sheet)
  | None -> (count, sheet)
