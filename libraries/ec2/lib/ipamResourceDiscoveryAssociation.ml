open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_resource_discovery_association_id : String.t option
  ; ipam_resource_discovery_association_arn : String.t option
  ; ipam_resource_discovery_id : String.t option
  ; ipam_id : String.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; is_default : Boolean.t option
  ; resource_discovery_status : IpamAssociatedResourceDiscoveryStatus.t option
  ; state : IpamResourceDiscoveryAssociationState.t option
  ; tags : TagList.t
  }

let make
    ?owner_id
    ?ipam_resource_discovery_association_id
    ?ipam_resource_discovery_association_arn
    ?ipam_resource_discovery_id
    ?ipam_id
    ?ipam_arn
    ?ipam_region
    ?is_default
    ?resource_discovery_status
    ?state
    ?(tags = [])
    () =
  { owner_id
  ; ipam_resource_discovery_association_id
  ; ipam_resource_discovery_association_arn
  ; ipam_resource_discovery_id
  ; ipam_id
  ; ipam_arn
  ; ipam_region
  ; is_default
  ; resource_discovery_status
  ; state
  ; tags
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_resource_discovery_association_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceDiscoveryAssociationId" xml)
          String.parse
    ; ipam_resource_discovery_association_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamResourceDiscoveryAssociationArn" xml)
          String.parse
    ; ipam_resource_discovery_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamResourceDiscoveryId" xml) String.parse
    ; ipam_id = Aws.Util.option_bind (Aws.Xml.member "ipamId" xml) String.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "isDefault" xml) Boolean.parse
    ; resource_discovery_status =
        Aws.Util.option_bind
          (Aws.Xml.member "resourceDiscoveryStatus" xml)
          IpamAssociatedResourceDiscoveryStatus.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          IpamResourceDiscoveryAssociationState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamResourceDiscoveryAssociationState.to_query f))
       ; Aws.Util.option_map v.resource_discovery_status (fun f ->
             Aws.Query.Pair
               ( "ResourceDiscoveryStatus"
               , IpamAssociatedResourceDiscoveryStatus.to_query f ))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_id (fun f ->
             Aws.Query.Pair ("IpamId", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryId", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_association_arn (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryAssociationArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_resource_discovery_association_id (fun f ->
             Aws.Query.Pair ("IpamResourceDiscoveryAssociationId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", IpamResourceDiscoveryAssociationState.to_json f)
       ; Aws.Util.option_map v.resource_discovery_status (fun f ->
             "resourceDiscoveryStatus", IpamAssociatedResourceDiscoveryStatus.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "isDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_id (fun f -> "ipamId", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_id (fun f ->
             "ipamResourceDiscoveryId", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_association_arn (fun f ->
             "ipamResourceDiscoveryAssociationArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_resource_discovery_association_id (fun f ->
             "ipamResourceDiscoveryAssociationId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_resource_discovery_association_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamResourceDiscoveryAssociationId")
        String.of_json
  ; ipam_resource_discovery_association_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamResourceDiscoveryAssociationArn")
        String.of_json
  ; ipam_resource_discovery_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamResourceDiscoveryId") String.of_json
  ; ipam_id = Aws.Util.option_map (Aws.Json.lookup j "ipamId") String.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "isDefault") Boolean.of_json
  ; resource_discovery_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "resourceDiscoveryStatus")
        IpamAssociatedResourceDiscoveryStatus.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        IpamResourceDiscoveryAssociationState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
