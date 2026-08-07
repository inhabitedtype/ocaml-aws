open Aws.BaseTypes

type t =
  { template_name : String.t option
  ; resource_a_r_n : String.t option
  ; rule_state : ManagedRuleState.t option
  }

let make ?template_name ?resource_a_r_n ?rule_state () =
  { template_name; resource_a_r_n; rule_state }

let parse xml =
  Some
    { template_name =
        Aws.Util.option_bind (Aws.Xml.member "TemplateName" xml) String.parse
    ; resource_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "ResourceARN" xml) String.parse
    ; rule_state =
        Aws.Util.option_bind (Aws.Xml.member "RuleState" xml) ManagedRuleState.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_state (fun f ->
             Aws.Query.Pair ("RuleState", ManagedRuleState.to_query f))
       ; Aws.Util.option_map v.resource_a_r_n (fun f ->
             Aws.Query.Pair ("ResourceARN", String.to_query f))
       ; Aws.Util.option_map v.template_name (fun f ->
             Aws.Query.Pair ("TemplateName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.rule_state (fun f ->
             "RuleState", ManagedRuleState.to_json f)
       ; Aws.Util.option_map v.resource_a_r_n (fun f -> "ResourceARN", String.to_json f)
       ; Aws.Util.option_map v.template_name (fun f -> "TemplateName", String.to_json f)
       ])

let of_json j =
  { template_name = Aws.Util.option_map (Aws.Json.lookup j "TemplateName") String.of_json
  ; resource_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ResourceARN") String.of_json
  ; rule_state =
      Aws.Util.option_map (Aws.Json.lookup j "RuleState") ManagedRuleState.of_json
  }
