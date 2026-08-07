type t = { transit_gateway_connect_peer : TransitGatewayConnectPeer.t option }

let make ?transit_gateway_connect_peer () = { transit_gateway_connect_peer }

let parse xml =
  Some
    { transit_gateway_connect_peer =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayConnectPeer" xml)
          TransitGatewayConnectPeer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_connect_peer (fun f ->
             Aws.Query.Pair
               ("TransitGatewayConnectPeer", TransitGatewayConnectPeer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_connect_peer (fun f ->
             "transitGatewayConnectPeer", TransitGatewayConnectPeer.to_json f)
       ])

let of_json j =
  { transit_gateway_connect_peer =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayConnectPeer")
        TransitGatewayConnectPeer.of_json
  }
