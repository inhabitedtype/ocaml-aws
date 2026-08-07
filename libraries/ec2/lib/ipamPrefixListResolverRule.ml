open Aws.BaseTypes

type t =
  { rule_type : IpamPrefixListResolverRuleType.t option
  ; static_cidr : String.t option
  ; ipam_scope_id : String.t option
  ; resource_type : IpamResourceType.t option
  ; conditions : IpamPrefixListResolverRuleConditionSet.t
  }

let make ?rule_type ?static_cidr ?ipam_scope_id ?resource_type ?(conditions = []) () =
  { rule_type; static_cidr; ipam_scope_id; resource_type; conditions }

let parse xml =
  Some
    { rule_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ruleType" xml)
          IpamPrefixListResolverRuleType.parse
    ; static_cidr = Aws.Util.option_bind (Aws.Xml.member "staticCidr" xml) String.parse
    ; ipam_scope_id = Aws.Util.option_bind (Aws.Xml.member "ipamScopeId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "resourceType" xml) IpamResourceType.parse
    ; conditions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "conditionSet" xml)
             IpamPrefixListResolverRuleConditionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ConditionSet"
              , IpamPrefixListResolverRuleConditionSet.to_query v.conditions ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamResourceType.to_query f))
       ; Aws.Util.option_map v.ipam_scope_id (fun f ->
             Aws.Query.Pair ("IpamScopeId", String.to_query f))
       ; Aws.Util.option_map v.static_cidr (fun f ->
             Aws.Query.Pair ("StaticCidr", String.to_query f))
       ; Aws.Util.option_map v.rule_type (fun f ->
             Aws.Query.Pair ("RuleType", IpamPrefixListResolverRuleType.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("conditionSet", IpamPrefixListResolverRuleConditionSet.to_json v.conditions)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamResourceType.to_json f)
       ; Aws.Util.option_map v.ipam_scope_id (fun f -> "ipamScopeId", String.to_json f)
       ; Aws.Util.option_map v.static_cidr (fun f -> "staticCidr", String.to_json f)
       ; Aws.Util.option_map v.rule_type (fun f ->
             "ruleType", IpamPrefixListResolverRuleType.to_json f)
       ])

let of_json j =
  { rule_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ruleType")
        IpamPrefixListResolverRuleType.of_json
  ; static_cidr = Aws.Util.option_map (Aws.Json.lookup j "staticCidr") String.of_json
  ; ipam_scope_id = Aws.Util.option_map (Aws.Json.lookup j "ipamScopeId") String.of_json
  ; resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "resourceType") IpamResourceType.of_json
  ; conditions =
      IpamPrefixListResolverRuleConditionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "conditionSet"))
  }
