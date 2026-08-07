type t = { transit_gateway_vpc_attachment : TransitGatewayVpcAttachment.t option }

let make ?transit_gateway_vpc_attachment () = { transit_gateway_vpc_attachment }

let parse xml =
  Some
    { transit_gateway_vpc_attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayVpcAttachment" xml)
          TransitGatewayVpcAttachment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_vpc_attachment (fun f ->
             Aws.Query.Pair
               ("TransitGatewayVpcAttachment", TransitGatewayVpcAttachment.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_vpc_attachment (fun f ->
             "transitGatewayVpcAttachment", TransitGatewayVpcAttachment.to_json f)
       ])

let of_json j =
  { transit_gateway_vpc_attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayVpcAttachment")
        TransitGatewayVpcAttachment.of_json
  }
