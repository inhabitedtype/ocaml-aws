open Aws.BaseTypes

type t =
  { transit_gateway_metering_policy_id : String.t
  ; policy_rule_number : Integer.t
  ; dry_run : Boolean.t option
  }

let make ~transit_gateway_metering_policy_id ~policy_rule_number ?dry_run () =
  { transit_gateway_metering_policy_id; policy_rule_number; dry_run }

let parse xml =
  Some
    { transit_gateway_metering_policy_id =
        Aws.Xml.required
          "TransitGatewayMeteringPolicyId"
          (Aws.Util.option_bind
             (Aws.Xml.member "TransitGatewayMeteringPolicyId" xml)
             String.parse)
    ; policy_rule_number =
        Aws.Xml.required
          "PolicyRuleNumber"
          (Aws.Util.option_bind (Aws.Xml.member "PolicyRuleNumber" xml) Integer.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("PolicyRuleNumber", Integer.to_query v.policy_rule_number))
       ; Some
           (Aws.Query.Pair
              ( "TransitGatewayMeteringPolicyId"
              , String.to_query v.transit_gateway_metering_policy_id ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Some ("PolicyRuleNumber", Integer.to_json v.policy_rule_number)
       ; Some
           ( "TransitGatewayMeteringPolicyId"
           , String.to_json v.transit_gateway_metering_policy_id )
       ])

let of_json j =
  { transit_gateway_metering_policy_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TransitGatewayMeteringPolicyId"))
  ; policy_rule_number =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "PolicyRuleNumber"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
