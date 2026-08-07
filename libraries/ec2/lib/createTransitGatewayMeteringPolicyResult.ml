type t = { transit_gateway_metering_policy : TransitGatewayMeteringPolicy.t option }

let make ?transit_gateway_metering_policy () = { transit_gateway_metering_policy }

let parse xml =
  Some
    { transit_gateway_metering_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMeteringPolicy" xml)
          TransitGatewayMeteringPolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_metering_policy (fun f ->
             Aws.Query.Pair
               ("TransitGatewayMeteringPolicy", TransitGatewayMeteringPolicy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_metering_policy (fun f ->
             "transitGatewayMeteringPolicy", TransitGatewayMeteringPolicy.to_json f)
       ])

let of_json j =
  { transit_gateway_metering_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMeteringPolicy")
        TransitGatewayMeteringPolicy.of_json
  }
