open Aws.BaseTypes

type t =
  { operation : IpamPrefixListResolverRuleConditionOperation.t option
  ; ipam_pool_id : String.t option
  ; resource_id : String.t option
  ; resource_owner : String.t option
  ; resource_region : String.t option
  ; resource_tag : IpamResourceTag.t option
  ; cidr : String.t option
  }

let make
    ?operation
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
        Aws.Util.option_bind
          (Aws.Xml.member "operation" xml)
          IpamPrefixListResolverRuleConditionOperation.parse
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "ipamPoolId" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_owner =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwner" xml) String.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; resource_tag =
        Aws.Util.option_bind (Aws.Xml.member "resourceTag" xml) IpamResourceTag.parse
    ; cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ; Aws.Util.option_map v.resource_tag (fun f ->
             Aws.Query.Pair ("ResourceTag", IpamResourceTag.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_owner (fun f ->
             Aws.Query.Pair ("ResourceOwner", String.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.operation (fun f ->
             Aws.Query.Pair
               ("Operation", IpamPrefixListResolverRuleConditionOperation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ; Aws.Util.option_map v.resource_tag (fun f ->
             "resourceTag", IpamResourceTag.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_owner (fun f -> "resourceOwner", String.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "ipamPoolId", String.to_json f)
       ; Aws.Util.option_map v.operation (fun f ->
             "operation", IpamPrefixListResolverRuleConditionOperation.to_json f)
       ])

let of_json j =
  { operation =
      Aws.Util.option_map
        (Aws.Json.lookup j "operation")
        IpamPrefixListResolverRuleConditionOperation.of_json
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPoolId") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_owner =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwner") String.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; resource_tag =
      Aws.Util.option_map (Aws.Json.lookup j "resourceTag") IpamResourceTag.of_json
  ; cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  }
