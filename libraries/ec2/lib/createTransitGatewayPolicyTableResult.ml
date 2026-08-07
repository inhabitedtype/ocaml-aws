type t = { transit_gateway_policy_table : TransitGatewayPolicyTable.t option }

let make ?transit_gateway_policy_table () = { transit_gateway_policy_table }

let parse xml =
  Some
    { transit_gateway_policy_table =
        Aws.Util.option_bind
          (Aws.Xml.member "transitGatewayPolicyTable" xml)
          TransitGatewayPolicyTable.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_policy_table (fun f ->
             Aws.Query.Pair
               ("TransitGatewayPolicyTable", TransitGatewayPolicyTable.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_gateway_policy_table (fun f ->
             "transitGatewayPolicyTable", TransitGatewayPolicyTable.to_json f)
       ])

let of_json j =
  { transit_gateway_policy_table =
      Aws.Util.option_map
        (Aws.Json.lookup j "transitGatewayPolicyTable")
        TransitGatewayPolicyTable.of_json
  }
