type t = { transit_gateway_policy_table_entries : TransitGatewayPolicyTableEntryList.t }

let make ?(transit_gateway_policy_table_entries = []) () =
  { transit_gateway_policy_table_entries }

let parse xml =
  Some
    { transit_gateway_policy_table_entries =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "transitGatewayPolicyTableEntries" xml)
             TransitGatewayPolicyTableEntryList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TransitGatewayPolicyTableEntries"
              , TransitGatewayPolicyTableEntryList.to_query
                  v.transit_gateway_policy_table_entries ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "transitGatewayPolicyTableEntries"
           , TransitGatewayPolicyTableEntryList.to_json
               v.transit_gateway_policy_table_entries )
       ])

let of_json j =
  { transit_gateway_policy_table_entries =
      TransitGatewayPolicyTableEntryList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "transitGatewayPolicyTableEntries"))
  }
