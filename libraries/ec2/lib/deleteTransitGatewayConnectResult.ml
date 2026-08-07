type t = { transit_gateway_connect : TransitGatewayConnect.t option }

let make ?transit_gateway_connect () = { transit_gateway_connect }

let parse xml =
  Some
    { transit_gateway_connect =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayConnect" xml)
          TransitGatewayConnect.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_connect (fun f ->
             Aws.Query.Pair ("TransitGatewayConnect", TransitGatewayConnect.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_connect (fun f ->
             "transitGatewayConnect", TransitGatewayConnect.to_json f)
       ])

let of_json j =
  { transit_gateway_connect =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayConnect")
        TransitGatewayConnect.of_json
  }
