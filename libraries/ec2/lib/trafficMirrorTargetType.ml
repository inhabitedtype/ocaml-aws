open Aws.BaseTypes

type t =
  | Network_interface
  | Network_load_balancer
  | Gateway_load_balancer_endpoint

let str_to_t =
  [ "gateway-load-balancer-endpoint", Gateway_load_balancer_endpoint
  ; "network-load-balancer", Network_load_balancer
  ; "network-interface", Network_interface
  ]

let t_to_str =
  [ Gateway_load_balancer_endpoint, "gateway-load-balancer-endpoint"
  ; Network_load_balancer, "network-load-balancer"
  ; Network_interface, "network-interface"
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
