open Tw

type barrier = {
  parties : int;
  arrived : int Atomic.t;
  generation : int Atomic.t;
}

let barrier parties =
  { parties; arrived = Atomic.make 0; generation = Atomic.make 0 }

let await barrier =
  let generation = Atomic.get barrier.generation in
  if Atomic.fetch_and_add barrier.arrived 1 = barrier.parties - 1 then (
    Atomic.set barrier.arrived 0;
    Atomic.set barrier.generation (generation + 1))
  else
    while Atomic.get barrier.generation = generation do
      Domain.cpu_relax ()
    done

let parse theme class_name =
  match Tw.of_string ~theme class_name with
  | Ok utility -> utility
  | Error (`Msg message) -> Alcotest.failf "%s: %s" class_name message

(* Construction is local to its caller even when several Domains happen to use
   the same variable name. The same request path also exercises the dynamic
   colour/radius caches and the class-split cache. *)
let construction_and_parsing_are_domain_safe () =
  let domain_count = 4 in
  let rounds = 500 in
  let start = barrier domain_count in
  let orders =
    Array.init rounds (fun _ ->
        Array.make domain_count (None : (int * int) option))
  in
  let worker worker_id () =
    for round = 0 to rounds - 1 do
      await start;
      let suffix = string_of_int round in
      let variable =
        Var.theme Css.Length ("domain-order-" ^ suffix) ~order:(91, worker_id)
      in
      let declaration, _ = Var.binding variable (Css.Px 1.) in
      orders.(round).(worker_id) <- Var.order_of_declaration declaration;
      let radius_name = "domainradius" ^ suffix in
      let theme =
        {
          Scheme.default with
          colors = [ ("red-500", Scheme.Hex "#123456") ];
          radius = [ (radius_name, Css.Px 3.) ];
          token_overrides = [ ("radius-" ^ radius_name, "3px") ];
        }
      in
      let color = parse theme "bg-red-500" in
      let radius = parse theme ("rounded-" ^ radius_name) in
      if round land 63 = 0 then
        ignore (Tw.to_css ~base:false ~theme [ color; radius ])
    done
  in
  let domains =
    List.init domain_count (fun worker_id -> Domain.spawn (worker worker_id))
  in
  List.iter Domain.join domains;
  Array.iteri
    (fun round row ->
      Array.iteri
        (fun worker_id actual ->
          Alcotest.(check (option (pair int int)))
            ("caller order in round " ^ string_of_int round)
            (Some (91, worker_id))
            actual)
        row)
    orders

let suite =
  ( "domain_cache",
    [
      Alcotest.test_case "concurrent construction and parsing" `Quick
        construction_and_parsing_are_domain_safe;
    ] )
