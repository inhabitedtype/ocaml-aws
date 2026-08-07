open Aws.BaseTypes

type t =
  | Static_cidr
  | Ipam_resource_cidr
  | Ipam_pool_cidr

let str_to_t =
  [ "ipam-pool-cidr", Ipam_pool_cidr
  ; "ipam-resource-cidr", Ipam_resource_cidr
  ; "static-cidr", Static_cidr
  ]

let t_to_str =
  [ Ipam_pool_cidr, "ipam-pool-cidr"
  ; Ipam_resource_cidr, "ipam-resource-cidr"
  ; Static_cidr, "static-cidr"
  ]

let to_string e = Aws.Util.of_option_exn (Aws.Util.list_find t_to_str e)
let of_string s = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t s)
let make v () = v

let parse xml =
  Aws.Util.option_bind (String.parse xml) (fun s -> Aws.Util.list_find str_to_t s)

let to_query v =
  Aws.Query.Value (Some (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v)))

let to_json v = String.to_json (Aws.Util.of_option_exn (Aws.Util.list_find t_to_str v))
let of_json j = Aws.Util.of_option_exn (Aws.Util.list_find str_to_t (String.of_json j))
