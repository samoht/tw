open Alcotest

(* Test parsing valid class strings and converting to CSS *)
let test_base_of_class_valid () =
  let open Tw.Utility in
  match base_of_class Tw.Scheme.default "p-4" with
  | Ok base -> check string "parsed class name" "p-4" (to_class (Base base))
  | Error _ -> fail "Failed to parse p-4"

(* Test parsing invalid class strings returns error *)
let test_base_of_class_invalid () =
  let open Tw.Utility in
  match base_of_class Tw.Scheme.default "invalid-class" with
  | Ok base ->
      let name = to_class (Base base) in
      fail ("Should not parse invalid class, got: " ^ name)
  | Error (`Msg msg) ->
      check bool "error message not empty" true (String.length msg > 0)

(* Test deduplication preserves order and keeps last occurrence *)
let test_deduplicate () =
  let open Tw.Utility in
  (* Parse some utilities *)
  let u1 =
    match base_of_class Tw.Scheme.default "p-0" with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in
  let u2 =
    match base_of_class Tw.Scheme.default "p-1" with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in
  let u3 =
    match base_of_class Tw.Scheme.default "p-0" with
    | Ok u -> Base u
    | Error _ -> failwith "parse failed"
  in

  (* Last occurrence should win *)
  let result = deduplicate [ u1; u2; u3 ] in
  check int "deduplicate length" 2 (List.length result)

(* Test deduplication with empty list *)
let test_deduplicate_empty () =
  let open Tw.Utility in
  let result = deduplicate [] in
  check int "empty list dedup" 0 (List.length result)

(* Test CSS parsing - commented out as css_of_string was removed *)
(* let test_css_of_string_valid () =
  match Tw.Utility.css_of_string ".test { color: red; }" with
  | Ok _ -> check bool "can parse CSS" true true
  | Error _ -> fail "Failed to parse valid CSS"

(* Test CSS parsing with invalid input *)
let test_css_of_string_invalid () =
  match Tw.Utility.css_of_string ".test { color }" with
  | Ok _ -> fail "Should not parse invalid CSS"
  | Error _ -> check bool "rejects invalid CSS" true true *)

(* Test Utility.order returns correct priority and suborder *)
let test_order_priorities () =
  let open Tw.Utility in
  (* Test various utilities - using actual module assignments, not ideal
     priorities *)
  let parse_and_order class_name =
    match base_of_class Tw.Scheme.default class_name with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ class_name)
  in

  (* Test relative ordering between different priority groups *)
  let pos_prio, _ = parse_and_order "top-0" in
  let margin_prio, _ = parse_and_order "m-4" in
  let grid_prio, _ = parse_and_order "col-span-2" in
  let padding_prio, _ = parse_and_order "p-4" in
  let typo_prio, _ = parse_and_order "text-xl" in

  (* Verify relative ordering (what matters for CSS) *)
  check bool "positioning before grid" true (pos_prio < grid_prio);
  check bool "grid before margin" true (grid_prio < margin_prio);
  check bool "margin before padding" true (margin_prio < padding_prio);
  check bool "padding before typography" true (padding_prio < typo_prio)

(* Test suborder within same priority group *)
let test_order_suborders () =
  let open Tw.Utility in
  let parse_and_order class_name =
    match base_of_class Tw.Scheme.default class_name with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ class_name)
  in

  (* Test padding suborders (all priority 19) *)
  let _, p_sub = parse_and_order "p-4" in
  let _, px_sub = parse_and_order "px-4" in
  let _, py_sub = parse_and_order "py-4" in
  let _, pt_sub = parse_and_order "pt-4" in

  (* These should have different suborders for proper CSS ordering *)
  check bool "p before px" true (p_sub < px_sub);
  check bool "px before py" true (px_sub < py_sub);
  check bool "py before pt" true (py_sub < pt_sub)

(* Test that ordering is consistent *)
let test_order_consistency () =
  let open Tw.Utility in
  let parse_and_order class_name =
    match base_of_class Tw.Scheme.default class_name with
    | Ok u -> order u
    | Error _ -> failwith ("Failed to parse: " ^ class_name)
  in

  (* Call multiple times to ensure deterministic *)
  let o1 = parse_and_order "p-4" in
  let o2 = parse_and_order "p-4" in
  let o3 = parse_and_order "p-4" in

  check (pair int int) "first equals second" o1 o2;
  check (pair int int) "second equals third" o2 o3

(* A project's [@utility] sorts at the slot of the property it declares, so a
   property has to resolve to the order of the utilities that set it. The slots
   are derived from each handler's examples, not from a table restating the
   order. *)
let test_order_of_property () =
  let open Tw.Utility in
  let order_of cls =
    match base_of_class Tw.Scheme.default cls with
    | Ok b -> order b
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check
    (option (pair int int))
    "display resolves to the display utilities"
    (Some (order_of "block"))
    (order_of_property (Key Display));
  check
    (option (pair int int))
    "box-sizing resolves to the box-sizing utilities"
    (Some (order_of "box-border"))
    (order_of_property (Key Box_sizing));
  check
    (option (pair int int))
    "isolation keeps its own slot, not the display family's"
    (Some (order_of "isolate"))
    (order_of_property (Key Isolation));
  check
    (option (pair int int))
    "padding skips its spacing token declaration"
    (Some (order_of "p-4"))
    (order_of_property (Key Padding));
  check
    (option (pair int int))
    "margin skips its spacing token declaration"
    (Some (order_of "m-auto"))
    (order_of_property (Key Margin));
  (* [placeholder-transparent] writes [color] too, but inside a [::placeholder]
     rule: the slot belongs to the utilities that colour the element itself. *)
  check
    (option (pair int int))
    "color resolves to the text-colour utilities"
    (Some (order_of "text-transparent"))
    (order_of_property (Key Color));
  check
    (option (pair int int))
    "border style skips width-utility carrier declarations"
    (Some (order_of "border-solid"))
    (order_of_property (Key Border_style))

(* A utility writes a property twice when the vendor-prefixed spelling still
   buys reach ([-webkit-user-select] then [user-select]). The slot belongs to
   the standard spelling, so a declared [@utility] setting it sorts with the
   family rather than at the layer's tail. A prefixed property with no
   unprefixed twin ([-webkit-line-clamp]) is the utility's own and keeps its
   slot. *)
let test_order_of_property_skips_vendor_prefix () =
  let open Tw.Utility in
  let order_of cls =
    match base_of_class Tw.Scheme.default cls with
    | Ok b -> order b
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check
    (option (pair int int))
    "user-select resolves to the select utilities"
    (Some (order_of "select-none"))
    (order_of_property (Key User_select));
  check
    (option (pair int int))
    "hyphens resolves to the hyphens utilities"
    (Some (order_of "hyphens-none"))
    (order_of_property (Key Hyphens));
  check
    (option (pair int int))
    "backdrop-filter resolves to the backdrop utilities"
    (Some (order_of "backdrop-filter-none"))
    (order_of_property (Key Backdrop_filter));
  check
    (option (pair int int))
    "mask-image resolves to the mask-gradient utilities"
    (Some (order_of "mask-linear-0"))
    (order_of_property (Key Mask_image));
  check
    (option (pair int int))
    "a prefixed property with no twin keeps its own slot"
    (Some (order_of "line-clamp-none"))
    (order_of_property (Key Webkit_line_clamp))

(* [outline-2] writes [outline-style: var(--tw-outline-style)] as a carrier for
   the style utilities, the same shape [border-2] uses. The width utility owns
   the width slot and the style utilities own the style slot. *)
let test_order_of_property_skips_outline_carrier () =
  let open Tw.Utility in
  let order_of cls =
    match base_of_class Tw.Scheme.default cls with
    | Ok b -> order b
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  check
    (option (pair int int))
    "outline width skips the style carrier"
    (Some (order_of "outline-2"))
    (order_of_property (Key Outline_width));
  check
    (option (pair int int))
    "outline style resolves to the style utilities"
    (Some (order_of "outline-solid"))
    (order_of_property (Key Outline_style))

(* Tailwind sorts a declared [@utility] by the property it writes, so every
   property a family is named for needs a slot. A family with no example
   covering its property leaves a declared utility at the layer's tail. *)
let test_order_of_property_covers_named_families () =
  let open Tw.Utility in
  let order_of cls =
    match base_of_class Tw.Scheme.default cls with
    | Ok b -> order b
    | Error (`Msg m) -> Alcotest.failf "%s: %s" cls m
  in
  List.iter
    (fun (key, cls) ->
      check
        (option (pair int int))
        (cls ^ " owns the property it is named for")
        (Some (order_of cls))
        (order_of_property key))
    [
      ((Key Order : Cascade.Css.Declaration.prop_key), "order-1");
      (Key Rotate, "rotate-45");
      (Key Scale, "scale-50");
      (Key Translate, "translate-x-2");
      (Key Transform, "transform-none");
      (Key Transform_style, "transform-flat");
      (Key Scroll_snap_stop, "snap-always");
      (Key Break_before, "break-before-page");
      (Key Break_after, "break-after-page");
      (Key Break_inside, "break-inside-avoid");
      (Key Border_radius, "rounded-sm");
      (Key Outline_offset, "outline-offset-2");
      (Key Background_position, "bg-center");
      (Key Text_align, "text-center");
      (Key Font_family, "font-sans");
      (Key Font_style, "italic");
      (Key Line_height, "leading-none");
    ]

(* [base_of_class] takes the FIRST handler that accepts a class, and handlers
   are tried in dune link order, so a class two handlers both accept would
   resolve on an unrelated build detail rather than on anything declared - and
   would change silently when someone edits a dune file. No such class was ever
   demonstrated; this asserts there is none, over the utilities every handler
   offers as its own examples plus the families whose prefixes overlap most
   (border/divide, text/font, shadow/inset-shadow, bg gradients, ring). *)
let test_no_class_claimed_twice () =
  let corpus =
    Tw.Utility.examples_classes ()
    @ [
        "border-2";
        "border-x-2";
        "divide-x-2";
        "divide-x-reverse";
        "text-lg";
        "text-red-500";
        "text-center";
        "font-bold";
        "font-sans";
        "shadow-lg";
        "shadow-red-500";
        "inset-shadow-sm";
        "ring-2";
        "ring-offset-2";
        "inset-ring-2";
        "bg-red-500";
        "bg-current";
        "bg-transparent";
        "border-red-500";
        "border-current";
        "border-transparent";
        "bg-linear-45";
        "from-red-500";
        "via-red-500";
        "to-red-500";
        "p-4";
        "px-4";
        "m-4";
        "gap-4";
        "space-x-4";
        "w-4";
        "h-4";
        "size-4";
        "min-w-4";
        "max-w-4";
        "rounded-lg";
        "outline-2";
        "leading-6";
        "tracking-wide";
        "opacity-50";
        "blur-sm";
        "drop-shadow-lg";
        "scale-95";
        "rotate-45";
        "translate-x-4";
        "grid-cols-3";
        "col-span-2";
        "flex-1";
        "basis-1/2";
        "aspect-video";
        "columns-3";
        "line-clamp-3";
        "indent-4";
        "border-spacing-2";
        "scroll-m-4";
        "stroke-2";
        "fill-red-500";
      ]
  in
  let doubled =
    List.filter_map
      (fun cls ->
        match Tw.Utility.claiming_handlers Tw.Scheme.default cls with
        | [] | [ _ ] -> None
        | names -> Some (cls ^ " -> " ^ String.concat ", " names))
      (List.sort_uniq String.compare corpus)
  in
  (* The border colours are still claimed twice: [borders] and [color] both
     accept them, and which one answers is decided by dune link order. Both
     spell the same rule at the same (priority, suborder), so the output is
     right today; this pins the set rather than the emptiness, so that a new
     overlap fails here and so does resolving one of these. *)
  let known_doubled =
    [
      "border-current -> borders, color";
      "border-red-500 -> borders, color";
      "border-transparent -> borders, color";
    ]
  in
  Alcotest.(check (list string))
    "no class is claimed twice beyond the known set" known_doubled doubled

let tests =
  [
    test_case "base_of_class valid input" `Quick test_base_of_class_valid;
    test_case "no class claimed twice" `Quick test_no_class_claimed_twice;
    test_case "base_of_class invalid input" `Quick test_base_of_class_invalid;
    test_case "deduplicate preserves order" `Quick test_deduplicate;
    test_case "deduplicate handles empty list" `Quick test_deduplicate_empty;
    (* test_case "css_of_string valid input" `Quick test_css_of_string_valid; *)
    (* test_case "css_of_string invalid input" `Quick test_css_of_string_invalid; *)
    test_case "order_of_property" `Quick test_order_of_property;
    test_case "order_of_property skips a vendor prefix" `Quick
      test_order_of_property_skips_vendor_prefix;
    test_case "order_of_property skips the outline carrier" `Quick
      test_order_of_property_skips_outline_carrier;
    test_case "order_of_property covers the named families" `Quick
      test_order_of_property_covers_named_families;
    test_case "order returns correct priorities" `Quick test_order_priorities;
    test_case "order returns correct suborders" `Quick test_order_suborders;
    test_case "order is consistent" `Quick test_order_consistency;
  ]

let suite = ("utility", tests)
