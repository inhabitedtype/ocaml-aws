open Aws.BaseTypes

type t =
  { destination_cidr_block : String.t option
  ; local_gateway_virtual_interface_group_id : String.t option
  ; type_ : LocalGatewayRouteType.t option
  ; state : LocalGatewayRouteState.t option
  ; local_gateway_route_table_id : String.t option
  ; local_gateway_route_table_arn : String.t option
  ; owner_id : String.t option
  ; subnet_id : String.t option
  ; coip_pool_id : String.t option
  ; network_interface_id : String.t option
  ; destination_prefix_list_id : String.t option
  }

let make
    ?destination_cidr_block
    ?local_gateway_virtual_interface_group_id
    ?type_
    ?state
    ?local_gateway_route_table_id
    ?local_gateway_route_table_arn
    ?owner_id
    ?subnet_id
    ?coip_pool_id
    ?network_interface_id
    ?destination_prefix_list_id
    () =
  { destination_cidr_block
  ; local_gateway_virtual_interface_group_id
  ; type_
  ; state
  ; local_gateway_route_table_id
  ; local_gateway_route_table_arn
  ; owner_id
  ; subnet_id
  ; coip_pool_id
  ; network_interface_id
  ; destination_prefix_list_id
  }

let parse xml =
  Some
    { destination_cidr_block =
        Aws.Util.option_bind (Aws.Xml.member "destinationCidrBlock" xml) String.parse
    ; local_gateway_virtual_interface_group_id =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayVirtualInterfaceGroupId" xml)
          String.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) LocalGatewayRouteType.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) LocalGatewayRouteState.parse
    ; local_gateway_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableId" xml) String.parse
    ; local_gateway_route_table_arn =
        Aws.Util.option_bind (Aws.Xml.member "localGatewayRouteTableArn" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; subnet_id = Aws.Util.option_bind (Aws.Xml.member "subnetId" xml) String.parse
    ; coip_pool_id = Aws.Util.option_bind (Aws.Xml.member "coipPoolId" xml) String.parse
    ; network_interface_id =
        Aws.Util.option_bind (Aws.Xml.member "networkInterfaceId" xml) String.parse
    ; destination_prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "destinationPrefixListId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             Aws.Query.Pair ("DestinationPrefixListId", String.to_query f))
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             Aws.Query.Pair ("NetworkInterfaceId", String.to_query f))
       ; Aws.Util.option_map v.coip_pool_id (fun f ->
             Aws.Query.Pair ("CoipPoolId", String.to_query f))
       ; Aws.Util.option_map v.subnet_id (fun f ->
             Aws.Query.Pair ("SubnetId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_arn (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableArn", String.to_query f))
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", LocalGatewayRouteState.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", LocalGatewayRouteType.to_query f))
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             Aws.Query.Pair ("LocalGatewayVirtualInterfaceGroupId", String.to_query f))
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             Aws.Query.Pair ("DestinationCidrBlock", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.destination_prefix_list_id (fun f ->
             "destinationPrefixListId", String.to_json f)
       ; Aws.Util.option_map v.network_interface_id (fun f ->
             "networkInterfaceId", String.to_json f)
       ; Aws.Util.option_map v.coip_pool_id (fun f -> "coipPoolId", String.to_json f)
       ; Aws.Util.option_map v.subnet_id (fun f -> "subnetId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_arn (fun f ->
             "localGatewayRouteTableArn", String.to_json f)
       ; Aws.Util.option_map v.local_gateway_route_table_id (fun f ->
             "localGatewayRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", LocalGatewayRouteState.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", LocalGatewayRouteType.to_json f)
       ; Aws.Util.option_map v.local_gateway_virtual_interface_group_id (fun f ->
             "localGatewayVirtualInterfaceGroupId", String.to_json f)
       ; Aws.Util.option_map v.destination_cidr_block (fun f ->
             "destinationCidrBlock", String.to_json f)
       ])

let of_json j =
  { destination_cidr_block =
      Aws.Util.option_map (Aws.Json.lookup j "destinationCidrBlock") String.of_json
  ; local_gateway_virtual_interface_group_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayVirtualInterfaceGroupId")
        String.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") LocalGatewayRouteType.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") LocalGatewayRouteState.of_json
  ; local_gateway_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableId") String.of_json
  ; local_gateway_route_table_arn =
      Aws.Util.option_map (Aws.Json.lookup j "localGatewayRouteTableArn") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; subnet_id = Aws.Util.option_map (Aws.Json.lookup j "subnetId") String.of_json
  ; coip_pool_id = Aws.Util.option_map (Aws.Json.lookup j "coipPoolId") String.of_json
  ; network_interface_id =
      Aws.Util.option_map (Aws.Json.lookup j "networkInterfaceId") String.of_json
  ; destination_prefix_list_id =
      Aws.Util.option_map (Aws.Json.lookup j "destinationPrefixListId") String.of_json
  }
