(* Auto-generated from "endpoints.atd" *)
[@@@ocaml.warning "-27-32-35-39"]

type service_defaults =
  { protocols : string list option
  ; ssl_common_name : string option
  }

type endpoint = { hostname : string option }

type service =
  { defaults : service_defaults option
  ; endpoints : (string * endpoint) list
  }

type region = { description : string }

type partition_defaults =
  { hostname : string option
  ; protocols : string list
  ; signature_versions : string list
  }

type partition =
  { defaults : partition_defaults
  ; dns_suffix : string
  ; partition : string
  ; partition_name : string
  ; region_regex : string
  ; regions : (string * region) list
  ; services : (string * service) list
  }

type endpoints = { partitions : partition list }
