(** Seed corpus generator for CSS parser fuzz tests.

    Generates representative CSS inputs in the corpus/ directory to seed AFL
    fuzzing campaigns. *)

let corpus_dir = "corpus"

let write name content =
  let path = Filename.concat corpus_dir name in
  let oc = open_out path in
  output_string oc content;
  close_out oc

let () =
  (try Unix.mkdir corpus_dir 0o755
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());

  (* Empty input *)
  write "empty" "";

  (* Basic selectors *)
  write "selector_class" ".foo";
  write "selector_id" "#bar";
  write "selector_element" "div";
  write "selector_universal" "*";
  write "selector_compound" "div.foo#bar";
  write "selector_descendant" ".foo .bar";
  write "selector_child" ".foo > .bar";
  write "selector_sibling" ".foo ~ .bar";
  write "selector_adjacent" ".foo + .bar";
  write "selector_attribute" "[data-active]";
  write "selector_attribute_exact" {|[aria-checked="true"]|};
  write "selector_pseudo_class" ".foo:hover";
  write "selector_pseudo_element" ".foo::before";
  write "selector_nth" ":nth-child(2n+1)";
  write "selector_not" ":not(.foo)";
  write "selector_is" ":is(.foo, .bar)";
  write "selector_where" ":where(.foo)";
  write "selector_has" ":has(> .bar)";
  write "selector_escaped" {|.sm\:p-4|};
  write "selector_list" ".foo, .bar, .baz";

  (* Colors *)
  write "color_hex3" "#f00";
  write "color_hex6" "#ff0000";
  write "color_hex8" "#ff000080";
  write "color_named" "red";
  write "color_rgb" "rgb(255, 0, 0)";
  write "color_rgba" "rgba(255, 0, 0, 0.5)";
  write "color_rgb_modern" "rgb(255 0 0 / 50%)";
  write "color_hsl" "hsl(120, 100%, 50%)";
  write "color_hsla" "hsla(120, 100%, 50%, 0.5)";
  write "color_hwb" "hwb(120 0% 0%)";
  write "color_oklch" "oklch(0.7 0.15 180)";
  write "color_oklab" "oklab(0.7 -0.1 0.1)";
  write "color_lch" "lch(50 30 180)";
  write "color_mix" "color-mix(in srgb, red 50%, blue)";
  write "color_currentcolor" "currentcolor";
  write "color_transparent" "transparent";

  (* Lengths and values *)
  write "length_px" "16px";
  write "length_rem" "1.5rem";
  write "length_em" "2em";
  write "length_percent" "50%";
  write "length_vw" "100vw";
  write "length_vh" "100vh";
  write "length_zero" "0";
  write "length_negative" "-1px";
  write "angle_deg" "90deg";
  write "angle_rad" "1.5708rad";
  write "angle_turn" "0.25turn";
  write "duration_s" "0.3s";
  write "duration_ms" "300ms";
  write "number_int" "42";
  write "number_float" "3.14";

  (* Calc expressions *)
  write "calc_simple" "calc(100% - 20px)";
  write "calc_nested" "calc(100% - calc(2 * 1rem))";
  write "calc_multiply" "calc(16px * 1.5)";
  write "calc_divide" "calc(100vw / 3)";
  write "calc_complex" "calc(50% + 2rem - 1px)";

  (* Variables *)
  write "var_simple" "var(--tw-color)";
  write "var_fallback" "var(--tw-color, red)";
  write "var_nested" "var(--tw-color, var(--fallback, blue))";

  (* Simple rules *)
  write "rule_simple" ".foo { color: red; }";
  write "rule_multi_decl" ".foo { color: red; font-size: 16px; margin: 0; }";
  write "rule_nested" ".foo { .bar { color: red; } }";
  write "rule_custom_prop" ".foo { --tw-color: #ff0000; }";

  (* At-rules *)
  write "media_simple" "@media (min-width: 768px) { .foo { padding: 1rem; } }";
  write "media_complex"
    "@media (min-width: 768px) and (max-width: 1024px) { .foo { display: grid; \
     } }";
  write "supports_simple" "(display: grid)";
  write "supports_not" "not (display: grid)";
  write "supports_and" "(display: grid) and (gap: 1rem)";
  write "supports_or" "(display: grid) or (display: flex)";
  write "supports_selector" "selector(:is(.foo))";
  write "layer" "@layer utilities { .foo { color: red; } }";
  write "keyframes"
    "@keyframes spin { from { transform: rotate(0deg); } to { transform: \
     rotate(360deg); } }";
  write "property"
    "@property --tw-color { syntax: '<color>'; inherits: false; initial-value: \
     transparent; }";

  (* Font face *)
  write "font_src_url" {|url("font.woff2") format("woff2")|};
  write "font_src_local" {|local("Arial")|};
  write "font_metric" "normal";
  write "font_metric_pct" "110%";
  write "font_size_adjust" "105%";

  (* Keyframe positions *)
  write "keyframe_from" "from";
  write "keyframe_to" "to";
  write "keyframe_percent" "50%";
  write "keyframe_multi" "0%, 100%";

  (* Full stylesheet *)
  write "stylesheet_full"
    {|@layer base, components, utilities;
@property --tw-color { syntax: '<color>'; inherits: false; initial-value: transparent; }
.foo { color: var(--tw-color); }
@media (min-width: 768px) { .foo { padding: 1rem; } }
@supports (display: grid) { .bar { display: grid; } }|};

  (* Edge cases *)
  write "edge_whitespace" "   \t\n  ";
  write "edge_semicolons" ";;;";
  write "edge_braces" "{ }";
  write "edge_parens" "()";
  write "edge_deep_nesting" "(((())))";
  write "edge_unicode" "\xc3\xa9\xc3\xa0\xc3\xbc";
  write "edge_long_ident" (String.make 256 'a');
  write "edge_special_chars" {|.foo\:bar\>baz|};

  Fmt.pr "Generated %d seed corpus files in %s/\n" 87 corpus_dir
