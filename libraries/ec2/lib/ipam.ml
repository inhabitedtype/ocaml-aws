open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_id : String.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; public_default_scope_id : String.t option
  ; private_default_scope_id : String.t option
  ; scope_count : Integer.t option
  ; description : String.t option
  ; operating_regions : IpamOperatingRegionSet.t
  ; state : IpamState.t option
  ; tags : TagList.t
  ; default_resource_discovery_id : String.t option
  ; default_resource_discovery_association_id : String.t option
  ; resource_discovery_association_count : Integer.t option
  ; state_message : String.t option
  ; tier : IpamTier.t option
  ; enable_private_gua : Boolean.t option
  ; metered_account : IpamMeteredAccount.t option
  }

let make
    ?owner_id
    ?ipam_id
    ?ipam_arn
    ?ipam_region
    ?public_default_scope_id
    ?private_default_scope_id
    ?scope_count
    ?description
    ?(operating_regions = [])
    ?state
    ?(tags = [])
    ?default_resource_discovery_id
    ?default_resource_discovery_association_id
    ?resource_discovery_association_count
    ?state_message
    ?tier
    ?enable_private_gua
    ?metered_account
    () =
  { owner_id
  ; ipam_id
  ; ipam_arn
  ; ipam_region
  ; public_default_scope_id
  ; private_default_scope_id
  ; scope_count
  ; description
  ; operating_regions
  ; state
  ; tags
  ; default_resource_discovery_id
  ; default_resource_discovery_association_id
  ; resource_discovery_association_count
  ; state_message
  ; tier
  ; enable_private_gua
  ; metered_account
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_id = Aws.Util.option_bind (Aws.Xml.member "ipamId" xml) String.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; public_default_scope_id =
        Aws.Util.option_bind (Aws.Xml.member "publicDefaultScopeId" xml) String.parse
    ; private_default_scope_id =
        Aws.Util.option_bind (Aws.Xml.member "privateDefaultScopeId" xml) String.parse
    ; scope_count = Aws.Util.option_bind (Aws.Xml.member "scopeCount" xml) Integer.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; operating_regions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "operatingRegionSet" xml)
             IpamOperatingRegionSet.parse)
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; default_resource_discovery_id =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultResourceDiscoveryId" xml)
          String.parse
    ; default_resource_discovery_association_id =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultResourceDiscoveryAssociationId" xml)
          String.parse
    ; resource_discovery_association_count =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceDiscoveryAssociationCount" xml)
          Integer.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; tier = Aws.Util.option_bind (Aws.Xml.member "tier" xml) IpamTier.parse
    ; enable_private_gua =
        Aws.Util.option_bind (Aws.Xml.member "enablePrivateGua" xml) Boolean.parse
    ; metered_account =
        Aws.Util.option_bind
          (Aws.Xml.member "meteredAccount" xml)
          IpamMeteredAccount.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metered_account (fun f ->
             Aws.Query.Pair ("MeteredAccount", IpamMeteredAccount.to_query f))
       ; Aws.Util.option_map v.enable_private_gua (fun f ->
             Aws.Query.Pair ("EnablePrivateGua", Boolean.to_query f))
       ; Aws.Util.option_map v.tier (fun f ->
             Aws.Query.Pair ("Tier", IpamTier.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.resource_discovery_association_count (fun f ->
             Aws.Query.Pair ("ResourceDiscoveryAssociationCount", Integer.to_query f))
       ; Aws.Util.option_map v.default_resource_discovery_association_id (fun f ->
             Aws.Query.Pair ("DefaultResourceDiscoveryAssociationId", String.to_query f))
       ; Aws.Util.option_map v.default_resource_discovery_id (fun f ->
             Aws.Query.Pair ("DefaultResourceDiscoveryId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamState.to_query f))
       ; Some
           (Aws.Query.Pair
              ("OperatingRegionSet", IpamOperatingRegionSet.to_query v.operating_regions))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.scope_count (fun f ->
             Aws.Query.Pair ("ScopeCount", Integer.to_query f))
       ; Aws.Util.option_map v.private_default_scope_id (fun f ->
             Aws.Query.Pair ("PrivateDefaultScopeId", String.to_query f))
       ; Aws.Util.option_map v.public_default_scope_id (fun f ->
             Aws.Query.Pair ("PublicDefaultScopeId", String.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_id (fun f ->
             Aws.Query.Pair ("IpamId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metered_account (fun f ->
             "meteredAccount", IpamMeteredAccount.to_json f)
       ; Aws.Util.option_map v.enable_private_gua (fun f ->
             "enablePrivateGua", Boolean.to_json f)
       ; Aws.Util.option_map v.tier (fun f -> "tier", IpamTier.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.resource_discovery_association_count (fun f ->
             "resourceDiscoveryAssociationCount", Integer.to_json f)
       ; Aws.Util.option_map v.default_resource_discovery_association_id (fun f ->
             "defaultResourceDiscoveryAssociationId", String.to_json f)
       ; Aws.Util.option_map v.default_resource_discovery_id (fun f ->
             "defaultResourceDiscoveryId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f -> "state", IpamState.to_json f)
       ; Some ("operatingRegionSet", IpamOperatingRegionSet.to_json v.operating_regions)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.scope_count (fun f -> "scopeCount", Integer.to_json f)
       ; Aws.Util.option_map v.private_default_scope_id (fun f ->
             "privateDefaultScopeId", String.to_json f)
       ; Aws.Util.option_map v.public_default_scope_id (fun f ->
             "publicDefaultScopeId", String.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_id (fun f -> "ipamId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_id = Aws.Util.option_map (Aws.Json.lookup j "ipamId") String.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; public_default_scope_id =
      Aws.Util.option_map (Aws.Json.lookup j "publicDefaultScopeId") String.of_json
  ; private_default_scope_id =
      Aws.Util.option_map (Aws.Json.lookup j "privateDefaultScopeId") String.of_json
  ; scope_count = Aws.Util.option_map (Aws.Json.lookup j "scopeCount") Integer.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; operating_regions =
      IpamOperatingRegionSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "operatingRegionSet"))
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") IpamState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; default_resource_discovery_id =
      Aws.Util.option_map (Aws.Json.lookup j "defaultResourceDiscoveryId") String.of_json
  ; default_resource_discovery_association_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultResourceDiscoveryAssociationId")
        String.of_json
  ; resource_discovery_association_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceDiscoveryAssociationCount")
        Integer.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; tier = Aws.Util.option_map (Aws.Json.lookup j "tier") IpamTier.of_json
  ; enable_private_gua =
      Aws.Util.option_map (Aws.Json.lookup j "enablePrivateGua") Boolean.of_json
  ; metered_account =
      Aws.Util.option_map (Aws.Json.lookup j "meteredAccount") IpamMeteredAccount.of_json
  }
