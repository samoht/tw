(** Version information for the tw library *)

let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  Printf.sprintf "/*! tw v%s | MIT License | https://github.com/samoht/tw */"
    version
