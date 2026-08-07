open Aws.BaseTypes

type t =
  { traffic_mirror_target_id : String.t option
  ; network_interface_id : String.t option
  ; network_load_balancer_arn : String.t option
  ; type_ : TrafficMirrorTargetType.t option
  ; description : String.t option
  ; owner_id : String.t option
  ; tags : TagList.t
  ; gateway_load_balancer_endpoint_id : String.t option
  }

let make
    ?traffic_mirror_target_id
    ?network_interface_id
    ?network_load_balancer_arn
    ?type_
    ?description
    ?owner_id
    ?(tags = [])
    ?gateway_load_balancer_endpoint_id
    () =
  { traffic_mirror_target_id
  ; network_interface_id
  ; network_load_balancer_arn
  ; type_
  ; description
  ; owner_id
  ; tags
  ; gateway_load_balancer_endpoint_id
  }

let parse xml =
  Some
    { traffic_mirror_target_id =
        Aws.Util.option_bind (Aws.Xml.member "trafficMirrorTargetId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; network_load_balancer_arn =
        Aws.Util.option_bind (Aws.Xml.member "networkLoadBalancerArn" xml) String.parse
    ; type_ =
        Aws.Util.option_bind (Aws.Xml.member "type" xml) TrafficMirrorTargetType.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; gateway_load_balancer_endpoint_id =
        Aws.Util.option_bind
          (Aws.Xml.member "gatewayLoadBalancerEndpointId" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_load_balancer_endpoint_id (fun f ->
             Aws.Query.Pair ("GatewayLoadBalancerEndpointId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", TrafficMirrorTargetType.to_query f))
       ; Aws.Util.option_map v.network_load_balancer_arn (fun f ->
             Aws.Query.Pair ("NetworkLoadBalancerArn", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             Aws.Query.Pair ("TrafficMirrorTargetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.gateway_load_balancer_endpoint_id (fun f ->
             "gatewayLoadBalancerEndpointId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", TrafficMirrorTargetType.to_json f)
       ; Aws.Util.option_map v.network_load_balancer_arn (fun f ->
             "networkLoadBalancerArn", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.traffic_mirror_target_id (fun f ->
             "trafficMirrorTargetId", String.to_json f)
       ])

let of_json j =
  { traffic_mirror_target_id =
      Aws.Util.option_map (Aws.Json.lookup j "trafficMirrorTargetId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; network_load_balancer_arn =
      Aws.Util.option_map (Aws.Json.lookup j "networkLoadBalancerArn") String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") TrafficMirrorTargetType.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; gateway_load_balancer_endpoint_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "gatewayLoadBalancerEndpointId")
        String.of_json
  }
