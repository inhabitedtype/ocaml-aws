open Aws.BaseTypes

type t =
  { service_id : String.t option
  ; vpc_endpoint_id : String.t option
  ; vpc_endpoint_owner : String.t option
  ; vpc_endpoint_state : State.t option
  ; creation_timestamp : DateTime.t option
  ; dns_entries : DnsEntrySet.t
  ; network_load_balancer_arns : ValueStringList.t
  ; gateway_load_balancer_arns : ValueStringList.t
  ; ip_address_type : IpAddressType.t option
  ; vpc_endpoint_connection_id : String.t option
  ; tags : TagList.t
  ; vpc_endpoint_region : String.t option
  }

let make
    ?service_id
    ?vpc_endpoint_id
    ?vpc_endpoint_owner
    ?vpc_endpoint_state
    ?creation_timestamp
    ?(dns_entries = [])
    ?(network_load_balancer_arns = [])
    ?(gateway_load_balancer_arns = [])
    ?ip_address_type
    ?vpc_endpoint_connection_id
    ?(tags = [])
    ?vpc_endpoint_region
    () =
  { service_id
  ; vpc_endpoint_id
  ; vpc_endpoint_owner
  ; vpc_endpoint_state
  ; creation_timestamp
  ; dns_entries
  ; network_load_balancer_arns
  ; gateway_load_balancer_arns
  ; ip_address_type
  ; vpc_endpoint_connection_id
  ; tags
  ; vpc_endpoint_region
  }

let parse xml =
  Some
    { service_id = Aws.Util.option_bind (Aws.Xml.member "serviceId" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointId" xml) String.parse
    ; vpc_endpoint_owner =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointOwner" xml) String.parse
    ; vpc_endpoint_state =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointState" xml) State.parse
    ; creation_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "creationTimestamp" xml) DateTime.parse
    ; dns_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "dnsEntrySet" xml) DnsEntrySet.parse)
    ; network_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "networkLoadBalancerArnSet" xml)
             ValueStringList.parse)
    ; gateway_load_balancer_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "gatewayLoadBalancerArnSet" xml)
             ValueStringList.parse)
    ; ip_address_type =
        Aws.Util.option_bind (Aws.Xml.member "ipAddressType" xml) IpAddressType.parse
    ; vpc_endpoint_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointConnectionId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; vpc_endpoint_region =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointRegion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_endpoint_region (fun f ->
             Aws.Query.Pair ("VpcEndpointRegion", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.vpc_endpoint_connection_id (fun f ->
             Aws.Query.Pair ("VpcEndpointConnectionId", String.to_query f))
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             Aws.Query.Pair ("IpAddressType", IpAddressType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "GatewayLoadBalancerArnSet"
              , ValueStringList.to_query v.gateway_load_balancer_arns ))
       ; Some
           (Aws.Query.Pair
              ( "NetworkLoadBalancerArnSet"
              , ValueStringList.to_query v.network_load_balancer_arns ))
       ; Some (Aws.Query.Pair ("DnsEntrySet", DnsEntrySet.to_query v.dns_entries))
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_state (fun f ->
             Aws.Query.Pair ("VpcEndpointState", State.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_owner (fun f ->
             Aws.Query.Pair ("VpcEndpointOwner", String.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.vpc_endpoint_region (fun f ->
             "vpcEndpointRegion", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.vpc_endpoint_connection_id (fun f ->
             "vpcEndpointConnectionId", String.to_json f)
       ; Aws.Util.option_map v.ip_address_type (fun f ->
             "ipAddressType", IpAddressType.to_json f)
       ; Some
           ( "gatewayLoadBalancerArnSet"
           , ValueStringList.to_json v.gateway_load_balancer_arns )
       ; Some
           ( "networkLoadBalancerArnSet"
           , ValueStringList.to_json v.network_load_balancer_arns )
       ; Some ("dnsEntrySet", DnsEntrySet.to_json v.dns_entries)
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             "creationTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_state (fun f ->
             "vpcEndpointState", State.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_owner (fun f ->
             "vpcEndpointOwner", String.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "vpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.service_id (fun f -> "serviceId", String.to_json f)
       ])

let of_json j =
  { service_id = Aws.Util.option_map (Aws.Json.lookup j "serviceId") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointId") String.of_json
  ; vpc_endpoint_owner =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointOwner") String.of_json
  ; vpc_endpoint_state =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointState") State.of_json
  ; creation_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "creationTimestamp") DateTime.of_json
  ; dns_entries =
      DnsEntrySet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "dnsEntrySet"))
  ; network_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "networkLoadBalancerArnSet"))
  ; gateway_load_balancer_arns =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "gatewayLoadBalancerArnSet"))
  ; ip_address_type =
      Aws.Util.option_map (Aws.Json.lookup j "ipAddressType") IpAddressType.of_json
  ; vpc_endpoint_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointConnectionId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; vpc_endpoint_region =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointRegion") String.of_json
  }
