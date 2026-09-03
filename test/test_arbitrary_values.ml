open Alcotest

(* Tailwind's arbitrary-value contract is token-stream based: a candidate is
   generated when its bracket contains one safe CSS declaration value, even when
   the browser will reject that value for the utility's property. The generated
   documentation corpus uses [<value>] to exercise this boundary. *)
let generated_placeholders () =
  let cases =
    [
      ("border-spacing-[<value>]", "--tw-border-spacing-x:<value>");
      ("border-spacing-x-[<value>]", "--tw-border-spacing-x:<value>");
      ("border-spacing-y-[<value>]", "--tw-border-spacing-y:<value>");
      ("ease-[<value>]", "--tw-ease:<value>");
      ("from-[<value>]", "--tw-gradient-from:<value>");
      ("inset-ring-[<value>]", "--tw-inset-ring-color:<value>");
      ("ring-[<value>]", "--tw-ring-color:<value>");
      ("skew-[<value>]", "--tw-skew-x:skewX(<value>)");
      ("skew-x-[<value>]", "--tw-skew-x:skewX(<value>)");
      ("skew-y-[<value>]", "--tw-skew-y:skewY(<value>)");
      ("line-clamp-[<value>]", "-webkit-box-orient:vertical");
      ("translate-z-[<value>]", "--tw-translate-z:<value>");
      ("divide-x-[<value>]", "--tw-divide-x-reverse:0");
      ("divide-y-[<value>]", "--tw-divide-y-reverse:0");
      ("scrollbar-thumb-[<value>]", "--tw-scrollbar-thumb:<value>");
      ("scrollbar-track-[<value>]", "--tw-scrollbar-track:<value>");
      ("via-[<value>]", "--tw-gradient-via:<value>");
      ("backdrop-blur-[<value>]", "--tw-backdrop-blur:blur(<value>)");
      ( "backdrop-brightness-[<value>]",
        "--tw-backdrop-brightness:brightness(<value>)" );
      ("backdrop-contrast-[<value>]", "--tw-backdrop-contrast:contrast(<value>)");
      ( "backdrop-grayscale-[<value>]",
        "--tw-backdrop-grayscale:grayscale(<value>)" );
      ( "backdrop-hue-rotate-[<value>]",
        "--tw-backdrop-hue-rotate:hue-rotate(<value>)" );
      ("backdrop-invert-[<value>]", "--tw-backdrop-invert:invert(<value>)");
      ("backdrop-opacity-[<value>]", "--tw-backdrop-opacity:opacity(<value>)");
      ("backdrop-saturate-[<value>]", "--tw-backdrop-saturate:saturate(<value>)");
      ("backdrop-sepia-[<value>]", "--tw-backdrop-sepia:sepia(<value>)");
      ("bg-conic-[<value>]", "--tw-gradient-position:<value>");
      ("blur-[<value>]", "--tw-blur:blur(<value>)");
      ("brightness-[<value>]", "--tw-brightness:brightness(<value>)");
      ("content-[<value>]", "--tw-content:<value>");
      ("content-[attr(<name>)]", "--tw-content:attr(<name>)");
      ("contrast-[<value>]", "--tw-contrast:contrast(<value>)");
      ("drop-shadow-[<value>]", "--tw-drop-shadow-size:drop-shadow(<value>)");
      ("grayscale-[<value>]", "--tw-grayscale:grayscale(<value>)");
      ("hue-rotate-[<value>]", "--tw-hue-rotate:hue-rotate(<value>)");
      ("inset-shadow-[<value>]", "--tw-inset-shadow:inset <value>");
      ("invert-[<value>]", "--tw-invert:invert(<value>)");
      ("saturate-[<value>]", "--tw-saturate:saturate(<value>)");
      ("sepia-[<value>]", "--tw-sepia:sepia(<value>)");
      ("shadow-[<value>]", "--tw-shadow:<value>");
      ("leading-[<value>]", "--tw-leading:<value>");
      ("tracking-[<value>]", "--tw-tracking:<value>");
      ("transition-[<value>]", "transition-property:<value>");
      ("duration-[<value>]", "--tw-duration:<value>");
      ("to-[<value>]", "--tw-gradient-to:<value>");
      ("animate-[<value>]", "animation:<value>");
    ]
  in
  List.iter
    (fun (cls, fragment) ->
      match Tw.of_string cls with
      | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
      | Ok utility ->
          Alcotest.(check string) (cls ^ " round-trips") cls (Tw.pp utility);
          let css =
            Tw.to_css ~base:false [ utility ] |> Tw.Css.to_string ~minify:true
          in
          Alcotest.(check bool)
            (cls ^ " emits its arbitrary value")
            true
            (Astring.String.is_infix ~affix:fragment css))
    cases

(* Semantic nonsense is still one declaration value; syntax that can start a
   second declaration or close the current rule is not. *)
let unsafe_values_are_rejected () =
  List.iter
    (fun cls ->
      match Tw.of_string cls with
      | Ok utility ->
          Alcotest.failf "expected %s to be rejected, got %s" cls
            (Tw.pp utility)
      | Error _ -> ())
    [
      "border-spacing-[x;y]";
      "ease-[linear;color:red]";
      "content-[attr(x);display:block]";
      "animate-[spin;display:block]";
      "rotate-[123deg]/foo";
      "scale-[123]/foo";
      "skew-[123deg]/foo";
      "skew-x-[123deg]/foo";
      "skew-y-[123deg]/foo";
    ]

let suite =
  ( "arbitrary_values",
    [
      test_case "generated placeholders" `Quick generated_placeholders;
      test_case "unsafe values are rejected" `Quick unsafe_values_are_rejected;
    ] )
