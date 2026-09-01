(** See {!Cascade_provenance}. *)

let run_line cmd =
  match Unix.open_process_in (cmd ^ " 2>/dev/null") with
  | exception Unix.Unix_error _ -> None
  | ic -> (
      let line =
        try Some (String.trim (input_line ic)) with End_of_file -> None
      in
      (match Unix.close_process_in ic with
      | _ -> ()
      | exception Unix.Unix_error _ -> ());
      match line with Some "" -> None | v -> v)

(* [dune exec] preserves the caller's cwd, so a repo-root checkout is found
   without climbing; a few parent hops cover a run from a subdirectory too. *)
let is_repo_root dir =
  Sys.file_exists (Filename.concat dir "dune-project")
  && Sys.file_exists (Filename.concat dir "cascade")

let repo_root () =
  let rec climb dir depth =
    if is_repo_root dir then Some dir
    else if depth <= 0 then None
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else climb parent (depth - 1)
  in
  match Sys.getcwd () with exception Sys_error _ -> None | cwd -> climb cwd 4

let contains haystack word =
  let n = String.length haystack and m = String.length word in
  let rec loop i =
    i + m <= n && (String.sub haystack i m = word || loop (i + 1))
  in
  m > 0 && loop 0

(* The [dune-project] depends line is [(cascade (and (>= X) (< Y)))]; grabbed
   verbatim rather than sexp-parsed since it is only ever displayed, never
   compared against. *)
let cascade_constraint root =
  let path = Filename.concat root "dune-project" in
  match open_in path with
  | exception Sys_error _ -> None
  | ic ->
      Fun.protect ~finally:(fun () -> close_in_noerr ic) @@ fun () ->
      let rec loop () =
        match input_line ic with
        | exception End_of_file -> None
        | line ->
            let trimmed = String.trim line in
            if contains trimmed "cascade" && contains trimmed "(and" then
              Some trimmed
            else loop ()
      in
      loop ()

(* The sha alone does not say how far a live checkout has moved. A sibling
   checkout parked on someone else's branch fails tw tests that pass against
   cascade's main, and reading that as a tw regression costs an afternoon; the
   branch name and the distance are what turn the failure into a question about
   cascade. *)
let checkout_position dir =
  let branch =
    match Fmt.kstr run_line "git -C %s rev-parse --abbrev-ref HEAD" dir with
    | Some b -> "branch " ^ b
    | None -> "detached HEAD"
  in
  let distance =
    match
      Fmt.kstr run_line "git -C %s rev-list --count origin/main..HEAD" dir
    with
    | Some "0" -> "level with origin/main"
    | Some n -> n ^ " commit(s) not in origin/main"
    | None -> "distance from origin/main unknown"
  in
  let dirty =
    match Fmt.kstr run_line "git -C %s status --porcelain" dir with
    | Some _ -> ", uncommitted changes"
    | None -> ""
  in
  branch ^ ", " ^ distance ^ dirty

let report () =
  match repo_root () with
  | None -> ()
  | Some root -> (
      let cascade_dir = Filename.quote (Filename.concat root "cascade") in
      match
        Fmt.kstr run_line "git -C %s rev-parse --short=12 HEAD" cascade_dir
      with
      | None -> ()
      | Some sha -> (
          let exact_tag =
            Fmt.kstr run_line "git -C %s describe --tags --exact-match"
              cascade_dir
          in
          let constraint_ =
            match cascade_constraint root with
            | Some p -> p
            | None -> "(unreadable)"
          in
          let position = checkout_position cascade_dir in
          match exact_tag with
          | Some tag ->
              Fmt.pr
                "cascade: local checkout %s (tag %s, %s); dune-project \
                 requires %s@."
                sha tag position constraint_
          | None ->
              Fmt.pr
                "@.WARNING: cascade checkout %s (%s) is not sitting on a \
                 release tag; dune-project requires %s, and CI resolves that \
                 constraint through an opam pin to cascade main, not this \
                 checkout. The cascade/ symlink is a live sibling checkout \
                 other sessions move, so a test failing here may be failing \
                 against cascade code that is not on cascade's main.@.@."
                sha position constraint_))
