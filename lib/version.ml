(** Version information for the tw library *)

let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let header =
  Pp.str
    [ "/*! tw v"; version; " | MIT License | https://github.com/samoht/tw */" ]
