type t = { transit_gateway_route_table : TransitGatewayRouteTable.t option }

let make ?transit_gateway_route_table () = { transit_gateway_route_table }

let parse xml =
  Some
    { transit_gateway_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTable" xml)
          TransitGatewayRouteTable.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table (fun f ->
             Aws.Query.Pair
               ("TransitGatewayRouteTable", TransitGatewayRouteTable.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table (fun f ->
             "transitGatewayRouteTable", TransitGatewayRouteTable.to_json f)
       ])

let of_json j =
  { transit_gateway_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTable")
        TransitGatewayRouteTable.of_json
  }
