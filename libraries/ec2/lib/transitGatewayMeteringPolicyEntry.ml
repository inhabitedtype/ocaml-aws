open Aws.BaseTypes

type t =
  { policy_rule_number : String.t option
  ; metered_account : TransitGatewayMeteringPayerType.t option
  ; state : TransitGatewayMeteringPolicyEntryState.t option
  ; updated_at : DateTime.t option
  ; update_effective_at : DateTime.t option
  ; metering_policy_rule : TransitGatewayMeteringPolicyRule.t option
  }

let make
    ?policy_rule_number
    ?metered_account
    ?state
    ?updated_at
    ?update_effective_at
    ?metering_policy_rule
    () =
  { policy_rule_number
  ; metered_account
  ; state
  ; updated_at
  ; update_effective_at
  ; metering_policy_rule
  }

let parse xml =
  Some
    { policy_rule_number =
        Aws.Util.option_bind (Aws.Xml.member "policyRuleNumber" xml) String.parse
    ; metered_account =
        Aws.Util.option_bind
          (Aws.Xml.member "meteredAccount" xml)
          TransitGatewayMeteringPayerType.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          TransitGatewayMeteringPolicyEntryState.parse
    ; updated_at = Aws.Util.option_bind (Aws.Xml.member "updatedAt" xml) DateTime.parse
    ; update_effective_at =
        Aws.Util.option_bind (Aws.Xml.member "updateEffectiveAt" xml) DateTime.parse
    ; metering_policy_rule =
        Aws.Util.option_bind
          (Aws.Xml.member "meteringPolicyRule" xml)
          TransitGatewayMeteringPolicyRule.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metering_policy_rule (fun f ->
             Aws.Query.Pair
               ("MeteringPolicyRule", TransitGatewayMeteringPolicyRule.to_query f))
       ; Aws.Util.option_map v.update_effective_at (fun f ->
             Aws.Query.Pair ("UpdateEffectiveAt", DateTime.to_query f))
       ; Aws.Util.option_map v.updated_at (fun f ->
             Aws.Query.Pair ("UpdatedAt", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", TransitGatewayMeteringPolicyEntryState.to_query f))
       ; Aws.Util.option_map v.metered_account (fun f ->
             Aws.Query.Pair ("MeteredAccount", TransitGatewayMeteringPayerType.to_query f))
       ; Aws.Util.option_map v.policy_rule_number (fun f ->
             Aws.Query.Pair ("PolicyRuleNumber", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metering_policy_rule (fun f ->
             "meteringPolicyRule", TransitGatewayMeteringPolicyRule.to_json f)
       ; Aws.Util.option_map v.update_effective_at (fun f ->
             "updateEffectiveAt", DateTime.to_json f)
       ; Aws.Util.option_map v.updated_at (fun f -> "updatedAt", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", TransitGatewayMeteringPolicyEntryState.to_json f)
       ; Aws.Util.option_map v.metered_account (fun f ->
             "meteredAccount", TransitGatewayMeteringPayerType.to_json f)
       ; Aws.Util.option_map v.policy_rule_number (fun f ->
             "policyRuleNumber", String.to_json f)
       ])

let of_json j =
  { policy_rule_number =
      Aws.Util.option_map (Aws.Json.lookup j "policyRuleNumber") String.of_json
  ; metered_account =
      Aws.Util.option_map
        (Aws.Json.lookup j "meteredAccount")
        TransitGatewayMeteringPayerType.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        TransitGatewayMeteringPolicyEntryState.of_json
  ; updated_at = Aws.Util.option_map (Aws.Json.lookup j "updatedAt") DateTime.of_json
  ; update_effective_at =
      Aws.Util.option_map (Aws.Json.lookup j "updateEffectiveAt") DateTime.of_json
  ; metering_policy_rule =
      Aws.Util.option_map
        (Aws.Json.lookup j "meteringPolicyRule")
        TransitGatewayMeteringPolicyRule.of_json
  }
