open Aws.BaseTypes

type t =
  { rule_type : IpamPrefixListResolverRuleType.t
  ; static_cidr : String.t option
  ; ipam_scope_id : String.t option
  ; resource_type : IpamResourceType.t option
  ; conditions : IpamPrefixListResolverRuleConditionRequestSet.t
  }

let make ~rule_type ?static_cidr ?ipam_scope_id ?resource_type ?(conditions = []) () =
  { rule_type; static_cidr; ipam_scope_id; resource_type; conditions }

let parse xml =
  Some
    { rule_type =
        Aws.Xml.required
          "RuleType"
          (Aws.Util.option_bind
             (Aws.Xml.member "RuleType" xml)
             IpamPrefixListResolverRuleType.parse)
    ; static_cidr = Aws.Util.option_bind (Aws.Xml.member "StaticCidr" xml) String.parse
    ; ipam_scope_id = Aws.Util.option_bind (Aws.Xml.member "IpamScopeId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind (Aws.Xml.member "ResourceType" xml) IpamResourceType.parse
    ; conditions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "Condition" xml)
             IpamPrefixListResolverRuleConditionRequestSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "Condition"
              , IpamPrefixListResolverRuleConditionRequestSet.to_query v.conditions ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamResourceType.to_query f))
       ; Aws.Util.option_map v.ipam_scope_id (fun f ->
             Aws.Query.Pair ("IpamScopeId", String.to_query f))
       ; Aws.Util.option_map v.static_cidr (fun f ->
             Aws.Query.Pair ("StaticCidr", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("RuleType", IpamPrefixListResolverRuleType.to_query v.rule_type))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "Condition"
           , IpamPrefixListResolverRuleConditionRequestSet.to_json v.conditions )
       ; Aws.Util.option_map v.resource_type (fun f ->
             "ResourceType", IpamResourceType.to_json f)
       ; Aws.Util.option_map v.ipam_scope_id (fun f -> "IpamScopeId", String.to_json f)
       ; Aws.Util.option_map v.static_cidr (fun f -> "StaticCidr", String.to_json f)
       ; Some ("RuleType", IpamPrefixListResolverRuleType.to_json v.rule_type)
       ])

let of_json j =
  { rule_type =
      IpamPrefixListResolverRuleType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "RuleType"))
  ; static_cidr = Aws.Util.option_map (Aws.Json.lookup j "StaticCidr") String.of_json
  ; ipam_scope_id = Aws.Util.option_map (Aws.Json.lookup j "IpamScopeId") String.of_json
  ; resource_type =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceType") IpamResourceType.of_json
  ; conditions =
      IpamPrefixListResolverRuleConditionRequestSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Condition"))
  }
