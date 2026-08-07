open Aws.BaseTypes

type t =
  { network_interface_id : String.t option
  ; network_load_balancer_arn : String.t option
  ; description : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; dry_run : Boolean.t option
  ; client_token : String.t option
  ; gateway_load_balancer_endpoint_id : String.t option
  }

let make
    ?network_interface_id
    ?network_load_balancer_arn
    ?description
    ?(tag_specifications = [])
    ?dry_run
    ?client_token
    ?gateway_load_balancer_endpoint_id
    () =
  { network_interface_id
  ; network_load_balancer_arn
  ; description
  ; tag_specifications
  ; dry_run
  ; client_token
  ; gateway_load_balancer_endpoint_id
  }

let parse xml =
  Some
    { network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "NetworkInterfaceId" xml) String.parse
    ; network_load_balancer_arn =
        Aws.Util.option_bind (Aws.Xml.member "NetworkLoadBalancerArn" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; gateway_load_balancer_endpoint_id =
        Aws.Util.option_bind
          (Aws.Xml.member "GatewayLoadBalancerEndpointId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_load_balancer_endpoint_id (fun f ->
             Aws.Query.Pair ("GatewayLoadBalancerEndpointId", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.network_load_balancer_arn (fun f ->
             Aws.Query.Pair ("NetworkLoadBalancerArn", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_load_balancer_endpoint_id (fun f ->
             "GatewayLoadBalancerEndpointId", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.network_load_balancer_arn (fun f ->
             "NetworkLoadBalancerArn", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "NetworkInterfaceId", String.to_json f)
       ])

let of_json j =
  { network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkInterfaceId") String.of_json
  ; network_load_balancer_arn =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkLoadBalancerArn") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; gateway_load_balancer_endpoint_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "GatewayLoadBalancerEndpointId")
        String.of_json
  }
