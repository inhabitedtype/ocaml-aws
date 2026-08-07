type t = { transit_gateway_peering_attachment : TransitGatewayPeeringAttachment.t option }

let make ?transit_gateway_peering_attachment () = { transit_gateway_peering_attachment }

let parse xml =
  Some
    { transit_gateway_peering_attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayPeeringAttachment" xml)
          TransitGatewayPeeringAttachment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_peering_attachment (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayPeeringAttachment"
               , TransitGatewayPeeringAttachment.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_peering_attachment (fun f ->
             "transitGatewayPeeringAttachment", TransitGatewayPeeringAttachment.to_json f)
       ])

let of_json j =
  { transit_gateway_peering_attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayPeeringAttachment")
        TransitGatewayPeeringAttachment.of_json
  }
