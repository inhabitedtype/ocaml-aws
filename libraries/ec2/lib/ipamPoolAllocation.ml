open Aws.BaseTypes

type t =
  { cidr : String.t option
  ; ipam_pool_allocation_id : String.t option
  ; description : String.t option
  ; resource_id : String.t option
  ; resource_type : IpamPoolAllocationResourceType.t option
  ; resource_region : String.t option
  ; resource_owner : String.t option
  ; tags : TagList.t
  }

let make
    ?cidr
    ?ipam_pool_allocation_id
    ?description
    ?resource_id
    ?resource_type
    ?resource_region
    ?resource_owner
    ?(tags = [])
    () =
  { cidr
  ; ipam_pool_allocation_id
  ; description
  ; resource_id
  ; resource_type
  ; resource_region
  ; resource_owner
  ; tags
  }

let parse xml =
  Some
    { cidr = Aws.Util.option_bind (Aws.Xml.member "cidr" xml) String.parse
    ; ipam_pool_allocation_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPoolAllocationId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; resource_id = Aws.Util.option_bind (Aws.Xml.member "resourceId" xml) String.parse
    ; resource_type =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceType" xml)
          IpamPoolAllocationResourceType.parse
    ; resource_region =
        Aws.Util.option_bind (Aws.Xml.member "resourceRegion" xml) String.parse
    ; resource_owner =
        Aws.Util.option_bind (Aws.Xml.member "resourceOwner" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.resource_owner (fun f ->
             Aws.Query.Pair ("ResourceOwner", String.to_query f))
       ; Aws.Util.option_map v.resource_region (fun f ->
             Aws.Query.Pair ("ResourceRegion", String.to_query f))
       ; Aws.Util.option_map v.resource_type (fun f ->
             Aws.Query.Pair ("ResourceType", IpamPoolAllocationResourceType.to_query f))
       ; Aws.Util.option_map v.resource_id (fun f ->
             Aws.Query.Pair ("ResourceId", String.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_allocation_id (fun f ->
             Aws.Query.Pair ("IpamPoolAllocationId", String.to_query f))
       ; Aws.Util.option_map v.cidr (fun f -> Aws.Query.Pair ("Cidr", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.resource_owner (fun f -> "resourceOwner", String.to_json f)
       ; Aws.Util.option_map v.resource_region (fun f ->
             "resourceRegion", String.to_json f)
       ; Aws.Util.option_map v.resource_type (fun f ->
             "resourceType", IpamPoolAllocationResourceType.to_json f)
       ; Aws.Util.option_map v.resource_id (fun f -> "resourceId", String.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_allocation_id (fun f ->
             "ipamPoolAllocationId", String.to_json f)
       ; Aws.Util.option_map v.cidr (fun f -> "cidr", String.to_json f)
       ])

let of_json j =
  { cidr = Aws.Util.option_map (Aws.Json.lookup j "cidr") String.of_json
  ; ipam_pool_allocation_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPoolAllocationId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; resource_id = Aws.Util.option_map (Aws.Json.lookup j "resourceId") String.of_json
  ; resource_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceType")
        IpamPoolAllocationResourceType.of_json
  ; resource_region =
      Aws.Util.option_map (Aws.Json.lookup j "resourceRegion") String.of_json
  ; resource_owner =
      Aws.Util.option_map (Aws.Json.lookup j "resourceOwner") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
