type t =
  { transit_gateway_client_vpn_attachment : TransitGatewayClientVpnAttachment.t option }

let make ?transit_gateway_client_vpn_attachment () =
  { transit_gateway_client_vpn_attachment }

let parse xml =
  Some
    { transit_gateway_client_vpn_attachment =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayClientVpnAttachment" xml)
          TransitGatewayClientVpnAttachment.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_client_vpn_attachment (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayClientVpnAttachment"
               , TransitGatewayClientVpnAttachment.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_client_vpn_attachment (fun f ->
             ( "transitGatewayClientVpnAttachment"
             , TransitGatewayClientVpnAttachment.to_json f ))
       ])

let of_json j =
  { transit_gateway_client_vpn_attachment =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayClientVpnAttachment")
        TransitGatewayClientVpnAttachment.of_json
  }
