open Aws.BaseTypes

type t =
  { destination_prefix_list_id : String.t option
  ; vpc_endpoint_id : String.t option
  ; local_target : Boolean.t option
  ; transit_gateway_id : String.t option
  ; local_gateway_id : String.t option
  ; carrier_gateway_id : String.t option
  ; core_network_arn : String.t option
  ; odb_network_arn : String.t option
  ; dry_run : Boolean.t option
  ; route_table_id : String.t
  ; destination_cidr_block : String.t option
  ; gateway_id : String.t option
  ; destination_ipv6_cidr_block : String.t option
  ; egress_only_internet_gateway_id : String.t option
  ; instance_id : String.t option
  ; network_interface_id : String.t option
  ; vpc_peering_connection_id : String.t option
  ; nat_gateway_id : String.t option
  }

let make
    ?destination_prefix_list_id
    ?vpc_endpoint_id
    ?local_target
    ?transit_gateway_id
    ?local_gateway_id
    ?carrier_gateway_id
    ?core_network_arn
    ?odb_network_arn
    ?dry_run
    ~route_table_id
    ?destination_cidr_block
    ?gateway_id
    ?destination_ipv6_cidr_block
    ?egress_only_internet_gateway_id
    ?instance_id
    ?network_interface_id
    ?vpc_peering_connection_id
    ?nat_gateway_id
    () =
  { destination_prefix_list_id
  ; vpc_endpoint_id
  ; local_target
  ; transit_gateway_id
  ; local_gateway_id
  ; carrier_gateway_id
  ; core_network_arn
  ; odb_network_arn
  ; dry_run
  ; route_table_id
  ; destination_cidr_block
  ; gateway_id
  ; destination_ipv6_cidr_block
  ; egress_only_internet_gateway_id
  ; instance_id
  ; network_interface_id
  ; vpc_peering_connection_id
  ; nat_gateway_id
  }

let parse xml =
  Some
    { destination_prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "DestinationPrefixListId" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "VpcEndpointId" xml) String.parse
    ; local_target = Aws.Util.option_bind (Aws.Xml.member "LocalTarget" xml) Boolean.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "TransitGatewayId" xml) String.parse
    ; local_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "LocalGatewayId" xml) String.parse
    ; carrier_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "CarrierGatewayId" xml) String.parse
    ; core_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "CoreNetworkArn" xml) String.parse
    ; odb_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "OdbNetworkArn" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    ; route_table_id =
        Aws.Xml.required
          "routeTableId"
          (Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse)
    ; destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; gateway_id = Aws.Util.option_bind (Aws.Xml.member "gatewayId" xml) String.parse
    ; destination_ipv6_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationIpv6CidrBlock" xml) String.parse
    ; egress_only_internet_gateway_id =
        Aws.Util.option_bind
          (Aws.Xml.member "egressOnlyInternetGatewayId" xml)
          String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; vpc_peering_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcPeeringConnectionId" xml) String.parse
    ; nat_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "natGatewayId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway_id (fun f ->
             Aws.Query.Pair ("NatGatewayId", String.to_query f))
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             Aws.Query.Pair ("VpcPeeringConnectionId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             Aws.Query.Pair ("EgressOnlyInternetGatewayId", String.to_query f))
       ; Aws.Util.option_map v.destination_ipv6_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationIpv6CidrBlock", String.to_query f))
       ; Aws.Util.option_map v.gateway_id (fun f ->
             Aws.Query.Pair ("GatewayId", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ; Some (Aws.Query.Pair ("RouteTableId", String.to_query v.route_table_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.odb_network_arn (fun f ->
             Aws.Query.Pair ("OdbNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             Aws.Query.Pair ("CoreNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             Aws.Query.Pair ("CarrierGatewayId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             Aws.Query.Pair ("LocalGatewayId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.local_target (fun f ->
             Aws.Query.Pair ("LocalTarget", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             Aws.Query.Pair ("DestinationPrefixListId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.nat_gateway_id (fun f -> "natGatewayId", String.to_json f)
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             "vpcPeeringConnectionId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             "egressOnlyInternetGatewayId", String.to_json f)
       ; Aws.Util.option_map v.destination_ipv6_cidr_block (fun f ->
             "destinationIpv6CidrBlock", String.to_json f)
       ; Aws.Util.option_map v.gateway_id (fun f -> "gatewayId", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ; Some ("routeTableId", String.to_json v.route_table_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.odb_network_arn (fun f ->
             "OdbNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             "CoreNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             "CarrierGatewayId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_id (fun f ->
             "LocalGatewayId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "TransitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.local_target (fun f -> "LocalTarget", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "VpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             "DestinationPrefixListId", String.to_json f)
       ])

let of_json j =
  { destination_prefix_list_id =
      Aws.Util.option_map (Aws.Json.lookup j "DestinationPrefixListId") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "VpcEndpointId") String.of_json
  ; local_target = Aws.Util.option_map (Aws.Json.lookup j "LocalTarget") Boolean.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "TransitGatewayId") String.of_json
  ; local_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "LocalGatewayId") String.of_json
  ; carrier_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "CarrierGatewayId") String.of_json
  ; core_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "CoreNetworkArn") String.of_json
  ; odb_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "OdbNetworkArn") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  ; route_table_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "routeTableId"))
  ; destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; gateway_id = Aws.Util.option_map (Aws.Json.lookup j "gatewayId") String.of_json
  ; destination_ipv6_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationIpv6CidrBlock") String.of_json
  ; egress_only_internet_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "egressOnlyInternetGatewayId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; vpc_peering_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcPeeringConnectionId") String.of_json
  ; nat_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "natGatewayId") String.of_json
  }
