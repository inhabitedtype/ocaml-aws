open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; ipam_policy_id : String.t
  ; locale : String.t
  ; resource_type : IpamPolicyResourceType.t
  ; allocation_rules : IpamPolicyAllocationRuleListRequest.t
  }

let make ?dry_run ~ipam_policy_id ~locale ~resource_type ?(allocation_rules = []) () =
  { dry_run; ipam_policy_id; locale; resource_type; allocation_rules }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; ipam_policy_id =
        Aws.Xml.required
          "IpamPolicyId"
          (Aws.Util.option_bind (Aws.Xml.member "IpamPolicyId" xml) String.parse)
    ; locale =
        Aws.Xml.required
          "Locale"
          (Aws.Util.option_bind (Aws.Xml.member "Locale" xml) String.parse)
    ; resource_type =
        Aws.Xml.required
          "ResourceType"
          (Aws.Util.option_bind
             (Aws.Xml.member "ResourceType" xml)
             IpamPolicyResourceType.parse)
    ; allocation_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AllocationRule" xml)
             IpamPolicyAllocationRuleListRequest.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AllocationRule"
              , IpamPolicyAllocationRuleListRequest.to_query v.allocation_rules ))
       ; Some
           (Aws.Query.Pair
              ("ResourceType", IpamPolicyResourceType.to_query v.resource_type))
       ; Some (Aws.Query.Pair ("Locale", String.to_query v.locale))
       ; Some (Aws.Query.Pair ("IpamPolicyId", String.to_query v.ipam_policy_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AllocationRule"
           , IpamPolicyAllocationRuleListRequest.to_json v.allocation_rules )
       ; Some ("ResourceType", IpamPolicyResourceType.to_json v.resource_type)
       ; Some ("Locale", String.to_json v.locale)
       ; Some ("IpamPolicyId", String.to_json v.ipam_policy_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; ipam_policy_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "IpamPolicyId"))
  ; locale = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Locale"))
  ; resource_type =
      IpamPolicyResourceType.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ResourceType"))
  ; allocation_rules =
      IpamPolicyAllocationRuleListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AllocationRule"))
  }
