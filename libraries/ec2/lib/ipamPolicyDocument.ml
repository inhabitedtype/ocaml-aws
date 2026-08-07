open Aws.BaseTypes

type t =
  { ipam_policy_id : String.t option
  ; locale : String.t option
  ; resource_type : IpamPolicyResourceType.t option
  ; allocation_rules : IpamPolicyAllocationRuleList.t
  }

let make ?ipam_policy_id ?locale ?resource_type ?(allocation_rules = []) () =
  { ipam_policy_id; locale; resource_type; allocation_rules }

let parse xml =
  Some
    { ipam_policy_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPolicyId" xml) String.parse
    ; locale = Aws.Util.option_bind (Aws.Xml.member "locale" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          IpamPolicyResourceType.parse
    ; allocation_rules =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "allocationRuleSet" xml)
             IpamPolicyAllocationRuleList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AllocationRuleSet"
              , IpamPolicyAllocationRuleList.to_query v.allocation_rules ))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamPolicyResourceType.to_query f))
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Aws.Util.option_map v.ipam_policy_id (fun f ->
             Aws.Query.Pair ("IpamPolicyId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("allocationRuleSet", IpamPolicyAllocationRuleList.to_json v.allocation_rules)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamPolicyResourceType.to_json f)
       ; Aws.Util.option_map v.locale (fun f -> "locale", String.to_json f)
       ; Aws.Util.option_map v.ipam_policy_id (fun f -> "ipamPolicyId", String.to_json f)
       ])

let of_json j =
  { ipam_policy_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPolicyId") String.of_json
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "locale") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        IpamPolicyResourceType.of_json
  ; allocation_rules =
      IpamPolicyAllocationRuleList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "allocationRuleSet"))
  }
