type t =
  { local_gateway_route_table_vpc_association :
      LocalGatewayRouteTableVpcAssociation.t option
  }

let make ?local_gateway_route_table_vpc_association () =
  { local_gateway_route_table_vpc_association }

let parse xml =
  Some
    { local_gateway_route_table_vpc_association =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayRouteTableVpcAssociation" xml)
          LocalGatewayRouteTableVpcAssociation.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table_vpc_association (fun f ->
             Aws.Query.Pair
               ( "LocalGatewayRouteTableVpcAssociation"
               , LocalGatewayRouteTableVpcAssociation.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table_vpc_association (fun f ->
             ( "localGatewayRouteTableVpcAssociation"
             , LocalGatewayRouteTableVpcAssociation.to_json f ))
       ])

let of_json j =
  { local_gateway_route_table_vpc_association =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayRouteTableVpcAssociation")
        LocalGatewayRouteTableVpcAssociation.of_json
  }
