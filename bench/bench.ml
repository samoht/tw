(* Micro-benchmark for the core CSS pipeline: parse class strings, build the
   stylesheet ([Tw.to_css]), and render it ([Css.to_string]).

   Usage: bench [parse|tocss|tostring|full] [iterations]

   Set MEMTRACE=/path/run.ctf to capture an allocation trace of the chosen
   phase, then read it with [memtrace_hotspots]. *)

let () = Memtrace.trace_if_requested ()

let colors =
  [
    "slate"; "gray"; "zinc"; "red"; "orange"; "amber"; "yellow"; "lime";
    "green"; "emerald"; "teal"; "cyan"; "sky"; "blue"; "indigo"; "violet";
    "purple"; "fuchsia"; "pink"; "rose";
  ]

let shades = [ 50; 100; 200; 300; 400; 500; 600; 700; 800; 900 ]
let variants = [ ""; "hover:"; "focus:"; "md:"; "lg:"; "dark:" ]
let spacing = [ 0; 1; 2; 3; 4; 5; 6; 8; 10; 12; 16; 20; 24; 32 ]

(* A large, deterministic corpus of candidate class strings spanning spacing,
   the colour palette, and common layout/typography utilities, each under a set
   of variants. *)
let candidates =
  let acc = ref [] in
  let add c = acc := c :: !acc in
  List.iter
    (fun v ->
      List.iter
        (fun n ->
          add (v ^ "p-" ^ string_of_int n);
          add (v ^ "px-" ^ string_of_int n);
          add (v ^ "m-" ^ string_of_int n);
          add (v ^ "gap-" ^ string_of_int n))
        spacing;
      List.iter
        (fun col ->
          List.iter
            (fun s ->
              let sh = string_of_int s in
              add (v ^ "bg-" ^ col ^ "-" ^ sh);
              add (v ^ "text-" ^ col ^ "-" ^ sh);
              add (v ^ "border-" ^ col ^ "-" ^ sh))
            shades)
        colors;
      List.iter
        (fun u -> add (v ^ u))
        [
          "flex"; "grid"; "block"; "hidden"; "rounded-lg"; "shadow-sm";
          "font-bold"; "text-sm"; "w-full"; "h-screen"; "items-center";
          "justify-between";
        ])
    variants;
  List.rev !acc

let parse cs =
  List.filter_map
    (fun c -> match Tw.of_string c with Ok t -> Some t | Error _ -> None)
    cs

let styles = parse candidates

let time ?(runs = 5) f =
  ignore (f ());
  let ts =
    Array.init runs (fun _ ->
        Gc.compact ();
        let t0 = Unix.gettimeofday () in
        ignore (f ());
        Unix.gettimeofday () -. t0)
  in
  Array.sort compare ts;
  ts.(runs / 2)

let words_per_op n f =
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to n do
    ignore (f ())
  done;
  (Gc.minor_words () -. before) /. float_of_int n

let run_phase phase iters =
  let css = lazy (Tw.to_css ~base:true styles) in
  let body () =
    match phase with
    | "parse" -> ignore (parse candidates)
    | "tocss" -> ignore (Tw.to_css ~base:true styles)
    | "tostring" -> ignore (Tw.Css.to_string ~minify:true (Lazy.force css))
    | "full" ->
        ignore (Tw.Css.to_string ~minify:true (Tw.to_css ~base:true (parse candidates)))
    | other -> failwith ("unknown phase: " ^ other)
  in
  for _ = 1 to iters do
    body ()
  done

let () =
  let phase = if Array.length Sys.argv > 1 then Sys.argv.(1) else "report" in
  let iters =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 30
  in
  if phase = "report" then (
    Printf.printf "corpus: %d candidates, %d valid utilities\n%!"
      (List.length candidates) (List.length styles);
    let p = time (fun () -> parse candidates) in
    let c = time (fun () -> Tw.to_css ~base:true styles) in
    let css = Tw.to_css ~base:true styles in
    let s = time (fun () -> Tw.Css.to_string ~minify:true css) in
    Printf.printf "parse     : %7.3f ms  (%8.0f words/op)\n" (p *. 1000.)
      (words_per_op 20 (fun () -> parse candidates));
    Printf.printf "to_css    : %7.3f ms  (%8.0f words/op)\n" (c *. 1000.)
      (words_per_op 20 (fun () -> Tw.to_css ~base:true styles));
    Printf.printf "to_string : %7.3f ms  (%8.0f words/op)\n%!" (s *. 1000.)
      (words_per_op 20 (fun () -> Tw.Css.to_string ~minify:true css)))
  else run_phase phase iters
