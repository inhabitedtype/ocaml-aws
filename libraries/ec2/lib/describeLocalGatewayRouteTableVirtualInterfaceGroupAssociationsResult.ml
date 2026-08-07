open Aws.BaseTypes

type t =
  { local_gateway_route_table_virtual_interface_group_associations :
      LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet.t
  ; next_token : String.t option
  }

let make
    ?(local_gateway_route_table_virtual_interface_group_associations = [])
    ?next_token
    () =
  { local_gateway_route_table_virtual_interface_group_associations; next_token }

let parse xml =
  Some
    { local_gateway_route_table_virtual_interface_group_associations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member
                "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
                xml)
             LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet"
              , LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet.to_query
                  v.local_gateway_route_table_virtual_interface_group_associations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"
           , LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet.to_json
               v.local_gateway_route_table_virtual_interface_group_associations )
       ])

let of_json j =
  { local_gateway_route_table_virtual_interface_group_associations =
      LocalGatewayRouteTableVirtualInterfaceGroupAssociationSet.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "localGatewayRouteTableVirtualInterfaceGroupAssociationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
