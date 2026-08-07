type t = { transit_gateway_multicast_domain : TransitGatewayMulticastDomain.t option }

let make ?transit_gateway_multicast_domain () = { transit_gateway_multicast_domain }

let parse xml =
  Some
    { transit_gateway_multicast_domain =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMulticastDomain" xml)
          TransitGatewayMulticastDomain.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_multicast_domain (fun f ->
             Aws.Query.Pair
               ("TransitGatewayMulticastDomain", TransitGatewayMulticastDomain.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_multicast_domain (fun f ->
             "transitGatewayMulticastDomain", TransitGatewayMulticastDomain.to_json f)
       ])

let of_json j =
  { transit_gateway_multicast_domain =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMulticastDomain")
        TransitGatewayMulticastDomain.of_json
  }
