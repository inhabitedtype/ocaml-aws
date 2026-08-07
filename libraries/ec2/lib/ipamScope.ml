open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_scope_id : String.t option
  ; ipam_scope_arn : String.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; ipam_scope_type : IpamScopeType.t option
  ; is_default : Boolean.t option
  ; description : String.t option
  ; pool_count : Integer.t option
  ; state : IpamScopeState.t option
  ; tags : TagList.t
  ; external_authority_configuration : IpamScopeExternalAuthorityConfiguration.t option
  }

let make
    ?owner_id
    ?ipam_scope_id
    ?ipam_scope_arn
    ?ipam_arn
    ?ipam_region
    ?ipam_scope_type
    ?is_default
    ?description
    ?pool_count
    ?state
    ?(tags = [])
    ?external_authority_configuration
    () =
  { owner_id
  ; ipam_scope_id
  ; ipam_scope_arn
  ; ipam_arn
  ; ipam_region
  ; ipam_scope_type
  ; is_default
  ; description
  ; pool_count
  ; state
  ; tags
  ; external_authority_configuration
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_scope_id = Aws.Util.option_bind (Aws.Xml.member "ipamScopeId" xml) String.parse
    ; ipam_scope_arn =
        Aws.Util.option_bind (Aws.Xml.member "ipamScopeArn" xml) String.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; ipam_scope_type =
        Aws.Util.option_bind (Aws.Xml.member "ipamScopeType" xml) IpamScopeType.parse
    ; is_default = Aws.Util.option_bind (Aws.Xml.member "isDefault" xml) Boolean.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; pool_count = Aws.Util.option_bind (Aws.Xml.member "poolCount" xml) Integer.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamScopeState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; external_authority_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "externalAuthorityConfiguration" xml)
          IpamScopeExternalAuthorityConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_authority_configuration (fun f ->
             Aws.Query.Pair
               ( "ExternalAuthorityConfiguration"
               , IpamScopeExternalAuthorityConfiguration.to_query f ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamScopeState.to_query f))
       ; Aws.Util.option_map v.pool_count (fun f ->
             Aws.Query.Pair ("PoolCount", Integer.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.is_default (fun f ->
             Aws.Query.Pair ("IsDefault", Boolean.to_query f))
       ; Aws.Util.option_map v.ipam_scope_type (fun f ->
             Aws.Query.Pair ("IpamScopeType", IpamScopeType.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_scope_arn (fun f ->
             Aws.Query.Pair ("IpamScopeArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_scope_id (fun f ->
             Aws.Query.Pair ("IpamScopeId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.external_authority_configuration (fun f ->
             ( "externalAuthorityConfiguration"
             , IpamScopeExternalAuthorityConfiguration.to_json f ))
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f -> "state", IpamScopeState.to_json f)
       ; Aws.Util.option_map v.pool_count (fun f -> "poolCount", Integer.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.is_default (fun f -> "isDefault", Boolean.to_json f)
       ; Aws.Util.option_map v.ipam_scope_type (fun f ->
             "ipamScopeType", IpamScopeType.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_scope_arn (fun f -> "ipamScopeArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_scope_id (fun f -> "ipamScopeId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_scope_id = Aws.Util.option_map (Aws.Json.lookup j "ipamScopeId") String.of_json
  ; ipam_scope_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamScopeArn") String.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; ipam_scope_type =
      Aws.Util.option_map (Aws.Json.lookup j "ipamScopeType") IpamScopeType.of_json
  ; is_default = Aws.Util.option_map (Aws.Json.lookup j "isDefault") Boolean.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; pool_count = Aws.Util.option_map (Aws.Json.lookup j "poolCount") Integer.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") IpamScopeState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; external_authority_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "externalAuthorityConfiguration")
        IpamScopeExternalAuthorityConfiguration.of_json
  }
