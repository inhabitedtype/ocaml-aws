open Aws.BaseTypes

type t =
  { additional_detail_type : String.t option
  ; component : AnalysisComponent.t option
  ; vpc_endpoint_service : AnalysisComponent.t option
  ; rule_options : RuleOptionList.t
  ; rule_group_type_pairs : RuleGroupTypePairList.t
  ; rule_group_rule_options_pairs : RuleGroupRuleOptionsPairList.t
  ; service_name : String.t option
  ; load_balancers : AnalysisComponentList.t
  }

let make
    ?additional_detail_type
    ?component
    ?vpc_endpoint_service
    ?(rule_options = [])
    ?(rule_group_type_pairs = [])
    ?(rule_group_rule_options_pairs = [])
    ?service_name
    ?(load_balancers = [])
    () =
  { additional_detail_type
  ; component
  ; vpc_endpoint_service
  ; rule_options
  ; rule_group_type_pairs
  ; rule_group_rule_options_pairs
  ; service_name
  ; load_balancers
  }

let parse xml =
  Some
    { additional_detail_type =
        Aws.Util.option_bind (Aws.Xml.member "additionalDetailType" xml) String.parse
    ; component =
        Aws.Util.option_bind (Aws.Xml.member "component" xml) AnalysisComponent.parse
    ; vpc_endpoint_service =
        Aws.Util.option_bind
          (Aws.Xml.member "vpcEndpointService" xml)
          AnalysisComponent.parse
    ; rule_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "ruleOptionSet" xml) RuleOptionList.parse)
    ; rule_group_type_pairs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ruleGroupTypePairSet" xml)
             RuleGroupTypePairList.parse)
    ; rule_group_rule_options_pairs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ruleGroupRuleOptionsPairSet" xml)
             RuleGroupRuleOptionsPairList.parse)
    ; service_name = Aws.Util.option_bind (Aws.Xml.member "serviceName" xml) String.parse
    ; load_balancers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "loadBalancerSet" xml)
             AnalysisComponentList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("LoadBalancerSet", AnalysisComponentList.to_query v.load_balancers))
       ; Aws.Util.option_map v.service_name (fun f ->
             Aws.Query.Pair ("ServiceName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "RuleGroupRuleOptionsPairSet"
              , RuleGroupRuleOptionsPairList.to_query v.rule_group_rule_options_pairs ))
       ; Some
           (Aws.Query.Pair
              ( "RuleGroupTypePairSet"
              , RuleGroupTypePairList.to_query v.rule_group_type_pairs ))
       ; Some (Aws.Query.Pair ("RuleOptionSet", RuleOptionList.to_query v.rule_options))
       ; Aws.Util.option_map v.vpc_endpoint_service (fun f ->
             Aws.Query.Pair ("VpcEndpointService", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.component (fun f ->
             Aws.Query.Pair ("Component", AnalysisComponent.to_query f))
       ; Aws.Util.option_map v.additional_detail_type (fun f ->
             Aws.Query.Pair ("AdditionalDetailType", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("loadBalancerSet", AnalysisComponentList.to_json v.load_balancers)
       ; Aws.Util.option_map v.service_name (fun f -> "serviceName", String.to_json f)
       ; Some
           ( "ruleGroupRuleOptionsPairSet"
           , RuleGroupRuleOptionsPairList.to_json v.rule_group_rule_options_pairs )
       ; Some
           ("ruleGroupTypePairSet", RuleGroupTypePairList.to_json v.rule_group_type_pairs)
       ; Some ("ruleOptionSet", RuleOptionList.to_json v.rule_options)
       ; Aws.Util.option_map v.vpc_endpoint_service (fun f ->
             "vpcEndpointService", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.component (fun f ->
             "component", AnalysisComponent.to_json f)
       ; Aws.Util.option_map v.additional_detail_type (fun f ->
             "additionalDetailType", String.to_json f)
       ])

let of_json j =
  { additional_detail_type =
      Aws.Util.option_map (Aws.Json.lookup j "additionalDetailType") String.of_json
  ; component =
      Aws.Util.option_map (Aws.Json.lookup j "component") AnalysisComponent.of_json
  ; vpc_endpoint_service =
      Aws.Util.option_map
        (Aws.Json.lookup j "vpcEndpointService")
        AnalysisComponent.of_json
  ; rule_options =
      RuleOptionList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleOptionSet"))
  ; rule_group_type_pairs =
      RuleGroupTypePairList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleGroupTypePairSet"))
  ; rule_group_rule_options_pairs =
      RuleGroupRuleOptionsPairList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ruleGroupRuleOptionsPairSet"))
  ; service_name = Aws.Util.option_map (Aws.Json.lookup j "serviceName") String.of_json
  ; load_balancers =
      AnalysisComponentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "loadBalancerSet"))
  }
