type t =
  { transit_gateway_route_table_announcement :
      TransitGatewayRouteTableAnnouncement.t option
  }

let make ?transit_gateway_route_table_announcement () =
  { transit_gateway_route_table_announcement }

let parse xml =
  Some
    { transit_gateway_route_table_announcement =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayRouteTableAnnouncement" xml)
          TransitGatewayRouteTableAnnouncement.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table_announcement (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayRouteTableAnnouncement"
               , TransitGatewayRouteTableAnnouncement.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_route_table_announcement (fun f ->
             ( "transitGatewayRouteTableAnnouncement"
             , TransitGatewayRouteTableAnnouncement.to_json f ))
       ])

let of_json j =
  { transit_gateway_route_table_announcement =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayRouteTableAnnouncement")
        TransitGatewayRouteTableAnnouncement.of_json
  }
