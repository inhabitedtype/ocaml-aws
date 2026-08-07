type t = { local_gateway_route_table : LocalGatewayRouteTable.t option }

let make ?local_gateway_route_table () = { local_gateway_route_table }

let parse xml =
  Some
    { local_gateway_route_table =
        Aws.Util.option_bind
          (Aws.Xml.member "localGatewayRouteTable" xml)
          LocalGatewayRouteTable.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table (fun f ->
             Aws.Query.Pair ("LocalGatewayRouteTable", LocalGatewayRouteTable.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.local_gateway_route_table (fun f ->
             "localGatewayRouteTable", LocalGatewayRouteTable.to_json f)
       ])

let of_json j =
  { local_gateway_route_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "localGatewayRouteTable")
        LocalGatewayRouteTable.of_json
  }
