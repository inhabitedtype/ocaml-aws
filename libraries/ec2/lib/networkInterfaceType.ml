open Aws.BaseTypes

type t =
  | Interface
  | NatGateway
  | Efa
  | Efa_only
  | Trunk
  | Load_balancer
  | Network_load_balancer
  | Vpc_endpoint
  | Branch
  | Transit_gateway
  | Lambda
  | Quicksight
  | Global_accelerator_managed
  | Api_gateway_managed
  | Gateway_load_balancer
  | Gateway_load_balancer_endpoint
  | Iot_rules_managed
  | Aws_codestar_connections_managed

let str_to_t =
  [ "aws_codestar_connections_managed", Aws_codestar_connections_managed
  ; "iot_rules_managed", Iot_rules_managed
  ; "gateway_load_balancer_endpoint", Gateway_load_balancer_endpoint
  ; "gateway_load_balancer", Gateway_load_balancer
  ; "api_gateway_managed", Api_gateway_managed
  ; "global_accelerator_managed", Global_accelerator_managed
  ; "quicksight", Quicksight
  ; "lambda", Lambda
  ; "transit_gateway", Transit_gateway
  ; "branch", Branch
  ; "vpc_endpoint", Vpc_endpoint
  ; "network_load_balancer", Network_load_balancer
  ; "load_balancer", Load_balancer
  ; "trunk", Trunk
  ; "efa-only", Efa_only
  ; "efa", Efa
  ; "natGateway", NatGateway
  ; "interface", Interface
  ]

let t_to_str =
  [ Aws_codestar_connections_managed, "aws_codestar_connections_managed"
  ; Iot_rules_managed, "iot_rules_managed"
  ; Gateway_load_balancer_endpoint, "gateway_load_balancer_endpoint"
  ; Gateway_load_balancer, "gateway_load_balancer"
  ; Api_gateway_managed, "api_gateway_managed"
  ; Global_accelerator_managed, "global_accelerator_managed"
  ; Quicksight, "quicksight"
  ; Lambda, "lambda"
  ; Transit_gateway, "transit_gateway"
  ; Branch, "branch"
  ; Vpc_endpoint, "vpc_endpoint"
  ; Network_load_balancer, "network_load_balancer"
  ; Load_balancer, "load_balancer"
  ; Trunk, "trunk"
  ; Efa_only, "efa-only"
  ; Efa, "efa"
  ; NatGateway, "natGateway"
  ; Interface, "interface"
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
