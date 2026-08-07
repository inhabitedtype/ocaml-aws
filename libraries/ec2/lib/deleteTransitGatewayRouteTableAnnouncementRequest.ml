open Aws.BaseTypes

type t =
  { transit_gateway_route_table_announcement_id : String.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_route_table_announcement_id ?dry_run () =
  { transit_gateway_route_table_announcement_id; dry_run }

let parse xml =
  Some
    { transit_gateway_route_table_announcement_id =
        Aws.Xml.required
          "TransitGatewayRouteTableAnnouncementId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayRouteTableAnnouncementId" xml)
             String.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayRouteTableAnnouncementId"
              , String.to_query v.transit_gateway_route_table_announcement_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some
           ( "TransitGatewayRouteTableAnnouncementId"
           , String.to_json v.transit_gateway_route_table_announcement_id )
       ])

let of_json j =
  { transit_gateway_route_table_announcement_id =
      String.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "TransitGatewayRouteTableAnnouncementId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
