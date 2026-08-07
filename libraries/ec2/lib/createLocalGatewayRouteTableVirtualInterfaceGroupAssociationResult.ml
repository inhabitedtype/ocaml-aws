type t =
  { local_gateway_route_table_virtual_interface_group_association :
      LocalGatewayRouteTableVirtualInterfaceGroupAssociation.t option
  }

let make ?local_gateway_route_table_virtual_interface_group_association () =
  { local_gateway_route_table_virtual_interface_group_association }

let parse xml =
  Some
    { local_gateway_route_table_virtual_interface_group_association =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayRouteTableVirtualInterfaceGroupAssociation" xml)
          LocalGatewayRouteTableVirtualInterfaceGroupAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.local_gateway_route_table_virtual_interface_group_association
           (fun f ->
             Aws.Query.Pair
               ( "LocalGatewayRouteTableVirtualInterfaceGroupAssociation"
               , LocalGatewayRouteTableVirtualInterfaceGroupAssociation.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map
           v.local_gateway_route_table_virtual_interface_group_association
           (fun f ->
             ( "localGatewayRouteTableVirtualInterfaceGroupAssociation"
             , LocalGatewayRouteTableVirtualInterfaceGroupAssociation.to_json f ))
       ])

let of_json j =
  { local_gateway_route_table_virtual_interface_group_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayRouteTableVirtualInterfaceGroupAssociation")
        LocalGatewayRouteTableVirtualInterfaceGroupAssociation.of_json
  }
