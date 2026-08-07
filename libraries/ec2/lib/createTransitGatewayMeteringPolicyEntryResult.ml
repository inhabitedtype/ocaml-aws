type t =
  { transit_gateway_metering_policy_entry : TransitGatewayMeteringPolicyEntry.t option }

let make ?transit_gateway_metering_policy_entry () =
  { transit_gateway_metering_policy_entry }

let parse xml =
  Some
    { transit_gateway_metering_policy_entry =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayMeteringPolicyEntry" xml)
          TransitGatewayMeteringPolicyEntry.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_metering_policy_entry (fun f ->
             Aws.Query.Pair
               ( "TransitGatewayMeteringPolicyEntry"
               , TransitGatewayMeteringPolicyEntry.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_metering_policy_entry (fun f ->
             ( "transitGatewayMeteringPolicyEntry"
             , TransitGatewayMeteringPolicyEntry.to_json f ))
       ])

let of_json j =
  { transit_gateway_metering_policy_entry =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayMeteringPolicyEntry")
        TransitGatewayMeteringPolicyEntry.of_json
  }
