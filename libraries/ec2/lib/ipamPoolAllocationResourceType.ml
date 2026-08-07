open Aws.BaseTypes

type t =
  | Ipam_pool
  | Vpc
  | Ec2_public_ipv4_pool
  | Custom
  | Subnet
  | Eip
  | Anycast_ip_list

let str_to_t =
  [ "anycast-ip-list", Anycast_ip_list
  ; "eip", Eip
  ; "subnet", Subnet
  ; "custom", Custom
  ; "ec2-public-ipv4-pool", Ec2_public_ipv4_pool
  ; "vpc", Vpc
  ; "ipam-pool", Ipam_pool
  ]

let t_to_str =
  [ Anycast_ip_list, "anycast-ip-list"
  ; Eip, "eip"
  ; Subnet, "subnet"
  ; Custom, "custom"
  ; Ec2_public_ipv4_pool, "ec2-public-ipv4-pool"
  ; Vpc, "vpc"
  ; Ipam_pool, "ipam-pool"
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
