open Aws.BaseTypes

type t =
  | Vpc
  | Vpn
  | Vpn_concentrator
  | Direct_connect_gateway
  | Connect
  | Peering
  | Tgw_peering
  | Network_function
  | Client_vpn

let str_to_t =
  [ "client-vpn", Client_vpn
  ; "network-function", Network_function
  ; "tgw-peering", Tgw_peering
  ; "peering", Peering
  ; "connect", Connect
  ; "direct-connect-gateway", Direct_connect_gateway
  ; "vpn-concentrator", Vpn_concentrator
  ; "vpn", Vpn
  ; "vpc", Vpc
  ]

let t_to_str =
  [ Client_vpn, "client-vpn"
  ; Network_function, "network-function"
  ; Tgw_peering, "tgw-peering"
  ; Peering, "peering"
  ; Connect, "connect"
  ; Direct_connect_gateway, "direct-connect-gateway"
  ; Vpn_concentrator, "vpn-concentrator"
  ; Vpn, "vpn"
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
