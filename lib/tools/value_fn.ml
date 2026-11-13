module Css = Cascade.Css
module Index = Cascade.Source_index

(* A project that declared [--spacing] in an [@theme inline] block has no
   variable to reference, so the step is multiplied out here instead, the way
   Tailwind's inline theme does. *)
let inline_spacing ~theme multiple =
  if not (Tw.Scheme.is_inline_token theme "spacing") then None
  else
    let scaled (step : Css.length) times : Css.length option =
      match step with
      | Css.Px v -> Some (Css.Px (v *. times))
      | Css.Rem v -> Some (Css.Rem (v *. times))
      | Css.Em v -> Some (Css.Em (v *. times))
      | _ -> None
    in
    match
      ( Option.bind (Tw.Scheme.token theme "spacing") Css.parse_length,
        float_of_string_opt (String.trim multiple) )
    with
    | Some step, Some times ->
        Option.map (Css.Pp.to_string Css.pp_length) (scaled step times)
    | _ -> None

(* Tailwind's [--spacing(N)] is shorthand for the spacing scale. It is not CSS,
   so a parser rejects the declaration and it drops out of the output. *)
let expand_spacing ~theme css =
  Text_pass.rewrite ~names:[ "--spacing(" ] css (fun index buf i ->
      match Index.call index ~name:"--spacing" i with
      | Some { body; next } ->
          Buffer.add_string buf
            (match inline_spacing ~theme body with
            | Some value -> value
            | None -> String.concat "" [ "calc(var(--spacing) * "; body; ")" ]);
          Some next
      | None -> None)

(* The value a theme token carries. The palette is not in [Scheme]'s table: a
   colour is catalogued, and [theme_color_decl] is what reads it back. Asking
   [Scheme.token] alone left [theme(--color-red-500)] unresolved, and an
   unresolved [theme()] is not CSS, so the parser dropped the declaration and
   the author's rule with it. *)
let theme_token_value theme name =
  match Tw.Scheme.token theme name with
  | Some _ as value -> value
  | None ->
      Option.map
        (fun decl -> String.trim (Cascade.Css.declaration_value decl))
        (Tw.Color.Handler.theme_color_decl ~theme name)

(* Tailwind's [--alpha(<color> / <percentage>)] is shorthand for compositing a
   colour with its own alpha. It is not CSS, so a parser rejects the declaration
   and it drops out of the output, leaving the element with no colour at all. It
   spells the same [color-mix()] the author could have written, which is what
   [authored_color_mix_fallbacks] then gives a legacy fallback and an
   [@supports] arm - the shape the reference emits. *)
let expand_alpha css =
  Text_pass.rewrite ~names:[ "--alpha(" ] css (fun index buf i ->
      match Index.call index ~name:"--alpha" i with
      | Some { body; next } -> (
          match String.index_opt body '/' with
          | None -> None
          | Some slash ->
              let colour = String.trim (String.sub body 0 slash) in
              let alpha =
                String.trim
                  (String.sub body (slash + 1) (String.length body - slash - 1))
              in
              Buffer.add_string buf
                (String.concat ""
                   [
                     "color-mix(in oklab, ";
                     colour;
                     " ";
                     alpha;
                     ", transparent)";
                   ]);
              Some next)
      | None -> None)

(* [theme()] also takes the dotted path of a v3 config ([theme(fontSize.sm)]),
   which names the same token under its old namespace. *)
let v3_theme_namespaces =
  [
    ("fontSize", "text");
    ("lineHeight", "leading");
    ("letterSpacing", "tracking");
    ("fontWeight", "font-weight");
    ("fontFamily", "font");
    ("colors", "color");
    ("borderRadius", "radius");
    ("boxShadow", "shadow");
    ("dropShadow", "drop-shadow");
    ("screens", "breakpoint");
    ("spacing", "spacing");
    ("transitionTimingFunction", "ease");
    ("animation", "animate");
    ("blur", "blur");
  ]

let v3_theme_token theme path =
  match String.split_on_char '.' (Text_pass.unquote path) with
  | [] | [ _ ] -> None
  | ns :: rest -> (
      match List.assoc_opt ns v3_theme_namespaces with
      | None -> None
      | Some prefix -> (
          let key = String.concat "-" rest in
          match theme_token_value theme (prefix ^ "-" ^ key) with
          | Some _ as v -> v
          | None -> (
              match (ns, float_of_string_opt key) with
              | ("spacing" | "lineHeight"), Some n -> Tw.Theme.spacing_times n
              | _ -> None)))

(* Tailwind's [theme(--token)] inlines the token's value. It is not CSS, and it
   appears in places a [var()] could not stand anyway, such as a media query
   condition. An unknown token is left alone rather than guessed at. *)
let resolve_theme ~theme css =
  Text_pass.rewrite ~names:[ "theme(" ] css (fun index buf i ->
      match Index.call index ~name:"theme" i with
      | None -> None
      | Some { body; next } -> (
          let name = String.trim body in
          let bare = Option.value ~default:name (Tw.Parse.bare_name name) in
          match
            match theme_token_value theme bare with
            | Some _ as v -> v
            | None -> v3_theme_token theme name
          with
          | Some value ->
              Buffer.add_string buf value;
              Some next
          | None -> None))

(* Where an at-rule prelude runs, for the at-rules whose prelude can hold a
   [--theme()] call. A [var()] is not read there, so such a call gives the value
   itself. *)
let at_rule_preludes index css =
  let names = [ "@media"; "@container"; "@supports"; "@custom-media" ] in
  let prelude_at acc i name =
    match Index.at_rule index ~name i with
    | Some { brace; _ } -> (i, brace) :: acc
    | None -> (
        match Index.at_statement index ~name i with
        | Some { next; _ } -> (i, next) :: acc
        | None -> acc)
  in
  let rec scan from acc =
    match String.index_from_opt css from '@' with
    | None -> acc
    | Some i ->
        scan (i + 1)
          (List.fold_left (fun acc n -> prelude_at acc i n) acc names)
  in
  scan 0 []

(* [--theme(--token)] reads a theme token from author CSS. It is a reference the
   theme layer then declares, with any fallback threaded into it; the value
   itself where the call says [inline] or stands in an at-rule prelude; and the
   fallback alone when the theme has no such token. A call naming no token is
   left alone, as the reference refuses to compile one. *)
let dashed_theme_value ~theme ~in_prelude body =
  match Tw.Parse.split_top_level ',' body with
  | [] -> None
  | first :: fallback -> (
      let first = String.trim first in
      let inline = String.ends_with ~suffix:" inline" first in
      let token =
        if inline then
          String.trim (String.sub first 0 (String.length first - 7))
        else first
      in
      let fallback = String.concat ", " (List.map String.trim fallback) in
      match Tw.Parse.bare_name token with
      | None -> None
      | Some bare -> (
          match theme_token_value theme bare with
          | Some value when inline || in_prelude -> Some value
          | Some _ when fallback = "" ->
              Some (String.concat "" [ "var("; token; ")" ])
          | Some _ ->
              Some (String.concat "" [ "var("; token; ", "; fallback; ")" ])
          | None when fallback = "" -> None
          | None -> Some fallback))

let resolve_dashed_theme ~theme css =
  Text_pass.rewrite ~names:[ "--theme(" ] css (fun index buf ->
      let preludes = at_rule_preludes index css in
      fun i ->
        match Index.call index ~name:"--theme" i with
        | Some { body; next } -> (
            let in_prelude =
              List.exists (fun (start, stop) -> start < i && i < stop) preludes
            in
            match dashed_theme_value ~theme ~in_prelude body with
            | Some value ->
                Buffer.add_string buf value;
                Some next
            | None -> None)
        | None -> None)
