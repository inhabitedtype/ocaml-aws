open Aws.BaseTypes

type t =
  { operation : IpamPrefixListResolverRuleConditionOperation.t
  ; ipam_pool_id : String.t option
  ; resource_id : String.t option
  ; resource_owner : String.t option
  ; resource_region : String.t option
  ; resource_tag : RequestIpamResourceTag.t option
  ; cidr : String.t option
  }

let make
    ~operation
    ?ipam_pool_id
    ?resource_id
    ?resource_owner
    ?resource_region
    ?resource_tag
    ?cidr
    () =
  { operation
  ; ipam_pool_id
  ; resource_id
  ; resource_owner
  ; resource_region
  ; resource_tag
  ; cidr
  }

let parse xml =
  Some
    { operation =
        Aws.Xml.required
          "Operation"
          (Aws.Util.option_bind
             (Aws.Xml.member "Operation" xml)
             IpamPrefixListResolverRuleConditionOperation.parse)
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "IpamPoolId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "ResourceId" xml) String.parse
    ; resource_owner =
        Aws.Util.option_bind (Aws.Xml.member "ResourceOwner" xml) String.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "ResourceRegion" xml) String.parse
    ; resource_tag =
        Aws.Util.option_bind
          (Aws.Xml.member "ResourceTag" xml)
          RequestIpamResourceTag.parse
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "Cidr" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Aws.Util.option_map v.resource_tag (fun f ->
             Aws.Query.Pair ("ResourceTag", RequestIpamResourceTag.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_owner (fun f ->
             Aws.Query.Pair ("ResourceOwner", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "Operation"
              , IpamPrefixListResolverRuleConditionOperation.to_query v.operation ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> "Cidr", String.to_json f)
       ; Aws.Util.option_map v.resource_tag (fun f ->
             "ResourceTag", RequestIpamResourceTag.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "ResourceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_owner (fun f -> "ResourceOwner", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "ResourceId", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "IpamPoolId", String.to_json f)
       ; Some
           ("Operation", IpamPrefixListResolverRuleConditionOperation.to_json v.operation)
       ])

let of_json j =
  { operation =
      IpamPrefixListResolverRuleConditionOperation.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "Operation"))
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "IpamPoolId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "ResourceId") String.of_json
  ; resource_owner =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceOwner") String.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceRegion") String.of_json
  ; resource_tag =
      Aws.Util.option_map (Aws.Json.lookup j "ResourceTag") RequestIpamResourceTag.of_json
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "Cidr") String.of_json
  }
