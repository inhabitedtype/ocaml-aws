open Aws.BaseTypes

type t =
  { policy_rule_number : String.t option
  ; policy_rule : TransitGatewayPolicyRule.t option
  ; target_route_table_id : String.t option
  }

let make ?policy_rule_number ?policy_rule ?target_route_table_id () =
  { policy_rule_number; policy_rule; target_route_table_id }

let parse xml =
  Some
    { policy_rule_number =
        Aws.Util.option_bind (Aws.Xml.member "policyRuleNumber" xml) String.parse
    ; policy_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "policyRule" xml)
          TransitGatewayPolicyRule.parse
    ; target_route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "targetRouteTableId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_route_table_id (fun f ->
             Aws.Query.Pair ("TargetRouteTableId", String.to_query f))
       ; Aws.Util.option_map v.policy_rule (fun f ->
             Aws.Query.Pair ("PolicyRule", TransitGatewayPolicyRule.to_query f))
       ; Aws.Util.option_map v.policy_rule_number (fun f ->
             Aws.Query.Pair ("PolicyRuleNumber", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_route_table_id (fun f ->
             "targetRouteTableId", String.to_json f)
       ; Aws.Util.option_map v.policy_rule (fun f ->
             "policyRule", TransitGatewayPolicyRule.to_json f)
       ; Aws.Util.option_map v.policy_rule_number (fun f ->
             "policyRuleNumber", String.to_json f)
       ])

let of_json j =
  { policy_rule_number =
      Aws.Util.option_map (Aws.Json.lookup j "policyRuleNumber") String.of_json
  ; policy_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "policyRule")
        TransitGatewayPolicyRule.of_json
  ; target_route_table_id =
      Aws.Util.option_map (Aws.Json.lookup j "targetRouteTableId") String.of_json
  }
