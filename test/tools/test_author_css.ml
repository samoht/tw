open Alcotest
open Tw_tools.Author_css

(* Nesting a utility under the rule that applied it. *)

let nested ~classes s =
  Cascade.Selector.to_string ~minify:true
    (nest_on_ampersand ~classes (Cascade.Selector.of_string s))

let test_nest_on_ampersand () =
  check string "class escaped as a hex code point" "&"
    (nested ~classes:[ "2xl:flex" ]
       (Cascade.Selector.to_string (Cascade.Selector.class_ "2xl:flex")));
  check string "utility wrapped in :where" ":where(&>:not(:last-child))"
    (nested ~classes:[ "divide-x" ] ":where(.divide-x > :not(:last-child))");
  check string "group class left alone" "&:is(:where(.group):hover *)"
    (nested ~classes:[ "group-hover:flex" ]
       ".group-hover\\:flex:is(:where(.group):hover *)");
  check string "ancestor class heads the selector" ":where(.group) &"
    (nested ~classes:[ "in-[.group]:flex" ]
       ":where(.group) .in-\\[\\.group\\]\\:flex");
  check string "every arm of a list" "&,&"
    (nested ~classes:[ "a"; "b" ] ".a, .b");
  check string "no class of ours: the leftmost goes" "& .other"
    (nested ~classes:[ "nothing" ] ".first .other")

(* A utility may name its own class a second time inside its selector, as the
   typography plugin writes [:where(.prose > ul > li p)] under [.prose]. The
   occurrence that heads the selector is the one the applying rule stands for;
   the one inside the argument is a descendant test, and it stays. A class that
   only occurs inside an argument is still the utility's own, as for
   [divide-*]. *)
let test_nest_on_ampersand_keeps_nested_mentions () =
  check string "only the heading occurrence is swapped"
    "& :where(.prose>ul>li p)"
    (nested ~classes:[ "prose" ] ".prose :where(.prose > ul > li p)");
  check string "an argument-only occurrence is still swapped"
    ":where(&>:not(:last-child))"
    (nested ~classes:[ "divide-x" ] ":where(.divide-x > :not(:last-child))")

let test_drop_directives () =
  check string "directives gone, author CSS kept" "\n\n.a { color: red }\n"
    (drop_directives
       "@tailwind utilities;\n@theme { --x: 1 }\n.a { color: red }\n")

let tests =
  [
    test_case "nest on ampersand" `Quick test_nest_on_ampersand;
    test_case "nest on ampersand keeps nested mentions" `Quick
      test_nest_on_ampersand_keeps_nested_mentions;
    test_case "directives dropped" `Quick test_drop_directives;
  ]

let suite = ("author_css", tests)
