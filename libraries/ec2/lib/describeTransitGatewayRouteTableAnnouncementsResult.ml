open Aws.BaseTypes

type t =
  { transit_gateway_route_table_announcements : TransitGatewayRouteTableAnnouncementList.t
  ; next_token : String.t option
  }

let make ?(transit_gateway_route_table_announcements = []) ?next_token () =
  { transit_gateway_route_table_announcements; next_token }

let parse xml =
  Some
    { transit_gateway_route_table_announcements =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "transitGatewayRouteTableAnnouncements" xml)
             TransitGatewayRouteTableAnnouncementList.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableAnnouncements"
              , TransitGatewayRouteTableAnnouncementList.to_query
                  v.transit_gateway_route_table_announcements ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "transitGatewayRouteTableAnnouncements"
           , TransitGatewayRouteTableAnnouncementList.to_json
               v.transit_gateway_route_table_announcements )
       ])

let of_json j =
  { transit_gateway_route_table_announcements =
      TransitGatewayRouteTableAnnouncementList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "transitGatewayRouteTableAnnouncements"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
