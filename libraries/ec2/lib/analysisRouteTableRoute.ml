open Aws.BaseTypes

type t =
  { destination_cidr : String.t option
  ; destination_prefix_list_id : String.t option
  ; egress_only_internet_gateway_id : String.t option
  ; gateway_id : String.t option
  ; instance_id : String.t option
  ; nat_gateway_id : String.t option
  ; network_interface_id : String.t option
  ; origin : String.t option
  ; transit_gateway_id : String.t option
  ; vpc_peering_connection_id : String.t option
  ; state : String.t option
  ; carrier_gateway_id : String.t option
  ; core_network_arn : String.t option
  ; local_gateway_id : String.t option
  }

let make
    ?destination_cidr
    ?destination_prefix_list_id
    ?egress_only_internet_gateway_id
    ?gateway_id
    ?instance_id
    ?nat_gateway_id
    ?network_interface_id
    ?origin
    ?transit_gateway_id
    ?vpc_peering_connection_id
    ?state
    ?carrier_gateway_id
    ?core_network_arn
    ?local_gateway_id
    () =
  { destination_cidr
  ; destination_prefix_list_id
  ; egress_only_internet_gateway_id
  ; gateway_id
  ; instance_id
  ; nat_gateway_id
  ; network_interface_id
  ; origin
  ; transit_gateway_id
  ; vpc_peering_connection_id
  ; state
  ; carrier_gateway_id
  ; core_network_arn
  ; local_gateway_id
  }

let parse xml =
  Some
    { destination_cidr =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidr" xml) String.parse
    ; destination_prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "destinationPrefixListId" xml) String.parse
    ; egress_only_internet_gateway_id =
        Aws.Util.option_bind
          (Aws.Xml.member "egressOnlyInternetGatewayId" xml)
          String.parse
    ; gateway_id = Aws.Util.option_bind (Aws.Xml.member "gatewayId" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; nat_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "natGatewayId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; origin = Aws.Util.option_bind (Aws.Xml.member "origin" xml) String.parse
    ; transit_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "transitGatewayId" xml) String.parse
    ; vpc_peering_connection_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcPeeringConnectionId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) String.parse
    ; carrier_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "carrierGatewayId" xml) String.parse
    ; core_network_arn =
        Aws.Util.option_bind (Aws.Xml.member "coreNetworkArn" xml) String.parse
    ; local_gateway_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_id (fun f ->
             Aws.Query.Pair ("LocalGatewayId", String.to_query f))
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             Aws.Query.Pair ("CoreNetworkArn", String.to_query f))
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             Aws.Query.Pair ("CarrierGatewayId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", String.to_query f))
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             Aws.Query.Pair ("VpcPeeringConnectionId", String.to_query f))
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             Aws.Query.Pair ("TransitGatewayId", String.to_query f))
       ; Aws.Util.option_map v.origin (fun f ->
             Aws.Query.Pair ("Origin", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.nat_gateway_id (fun f ->
             Aws.Query.Pair ("NatGatewayId", String.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.gateway_id (fun f ->
             Aws.Query.Pair ("GatewayId", String.to_query f))
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             Aws.Query.Pair ("EgressOnlyInternetGatewayId", String.to_query f))
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             Aws.Query.Pair ("DestinationPrefixListId", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             Aws.Query.Pair ("DestinationCidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_id (fun f ->
             "localGatewayId", String.to_json f)
       ; Aws.Util.option_map v.core_network_arn (fun f ->
             "coreNetworkArn", String.to_json f)
       ; Aws.Util.option_map v.carrier_gateway_id (fun f ->
             "carrierGatewayId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", String.to_json f)
       ; Aws.Util.option_map v.vpc_peering_connection_id (fun f ->
             "vpcPeeringConnectionId", String.to_json f)
       ; Aws.Util.option_map v.transit_gateway_id (fun f ->
             "transitGatewayId", String.to_json f)
       ; Aws.Util.option_map v.origin (fun f -> "origin", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.nat_gateway_id (fun f -> "natGatewayId", String.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.gateway_id (fun f -> "gatewayId", String.to_json f)
       ; Aws.Util.option_map v.egress_only_internet_gateway_id (fun f ->
             "egressOnlyInternetGatewayId", String.to_json f)
       ; Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             "destinationPrefixListId", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr (fun f ->
             "destinationCidr", String.to_json f)
       ])

let of_json j =
  { destination_cidr =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidr") String.of_json
  ; destination_prefix_list_id =
      Aws.Util.option_map (Aws.Json.lookup j "destinationPrefixListId") String.of_json
  ; egress_only_internet_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "egressOnlyInternetGatewayId") String.of_json
  ; gateway_id = Aws.Util.option_map (Aws.Json.lookup j "gatewayId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; nat_gateway_id = Aws.Util.option_map (Aws.Json.lookup j "natGatewayId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; origin = Aws.Util.option_map (Aws.Json.lookup j "origin") String.of_json
  ; transit_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "transitGatewayId") String.of_json
  ; vpc_peering_connection_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcPeeringConnectionId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") String.of_json
  ; carrier_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "carrierGatewayId") String.of_json
  ; core_network_arn =
      Aws.Util.option_map (Aws.Json.lookup j "coreNetworkArn") String.of_json
  ; local_gateway_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayId") String.of_json
  }
