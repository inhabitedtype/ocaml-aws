open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_resource_discovery_id : String.t option
  ; ipam_resource_discovery_arn : String.t option
  ; ipam_resource_discovery_region : String.t option
  ; description : String.t option
  ; operating_regions : IpamOperatingRegionSet.t
  ; is_default : Boolean.t option
  ; state : IpamResourceDiscoveryState.t option
  ; tags : TagList.t
  ; organizational_unit_exclusions : IpamOrganizationalUnitExclusionSet.t
  }

let make
    ?owner_id
    ?ipam_resource_discovery_id
    ?ipam_resource_discovery_arn
    ?ipam_resource_discovery_region
    ?description
    ?(operating_regions = [])
    ?is_default
    ?state
    ?(tags = [])
    ?(organizational_unit_exclusions = [])
    () =
  { owner_id
  ; ipam_resource_discovery_id
  ; ipam_resource_discovery_arn
  ; ipam_resource_discovery_region
  ; description
  ; operating_regions
  ; is_default
  ; state
  ; tags
  ; organizational_unit_exclusions
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_resource_discovery_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamResourceDiscoveryId" xml) String.parse
    ; ipam_resource_discovery_arn =
        Aws.Util.option_bind (Aws.Xml.member "ipamResourceDiscoveryArn" xml) String.parse
    ; ipam_resource_discovery_region =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceDiscoveryRegion" xml)
          String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; operating_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "operatingRegionSet" xml)
             IpamOperatingRegionSet.parse)
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "isDefault" xml) Boolean.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamResourceDiscoveryState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; organizational_unit_exclusions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "organizationalUnitExclusionSet" xml)
             IpamOrganizationalUnitExclusionSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "OrganizationalUnitExclusionSet"
              , IpamOrganizationalUnitExclusionSet.to_query
                  v.organizational_unit_exclusions ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamResourceDiscoveryState.to_query f))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OperatingRegionSet", IpamOperatingRegionSet.to_query v.operating_regions))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_region (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_arn (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "organizationalUnitExclusionSet"
           , IpamOrganizationalUnitExclusionSet.to_json v.organizational_unit_exclusions
           )
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", IpamResourceDiscoveryState.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "isDefault", Boolean.to_json f)
       ; Some ("operatingRegionSet", IpamOperatingRegionSet.to_json v.operating_regions)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_region (fun f ->
             "ipamResourceDiscoveryRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_arn (fun f ->
             "ipamResourceDiscoveryArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             "ipamResourceDiscoveryId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_resource_discovery_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryId") String.of_json
  ; ipam_resource_discovery_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryArn") String.of_json
  ; ipam_resource_discovery_region =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryRegion") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; operating_regions =
      IpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "operatingRegionSet"))
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "isDefault") Boolean.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") IpamResourceDiscoveryState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; organizational_unit_exclusions =
      IpamOrganizationalUnitExclusionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "organizationalUnitExclusionSet"))
  }
