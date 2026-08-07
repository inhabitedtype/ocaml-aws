open Aws.BaseTypes

type t =
  | Vpc
  | Subnet
  | Eip
  | Public_ipv4_pool
  | Ipv6_pool
  | Eni
  | Anycast_ip_list

let str_to_t =
  [ "anycast-ip-list", Anycast_ip_list
  ; "eni", Eni
  ; "ipv6-pool", Ipv6_pool
  ; "public-ipv4-pool", Public_ipv4_pool
  ; "eip", Eip
  ; "subnet", Subnet
  ; "vpc", Vpc
  ]

let t_to_str =
  [ Anycast_ip_list, "anycast-ip-list"
  ; Eni, "eni"
  ; Ipv6_pool, "ipv6-pool"
  ; Public_ipv4_pool, "public-ipv4-pool"
  ; Eip, "eip"
  ; Subnet, "subnet"
  ; Vpc, "vpc"
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
