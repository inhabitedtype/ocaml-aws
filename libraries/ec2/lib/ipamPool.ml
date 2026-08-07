open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_pool_id : String.t option
  ; source_ipam_pool_id : String.t option
  ; ipam_pool_arn : String.t option
  ; ipam_scope_arn : String.t option
  ; ipam_scope_type : IpamScopeType.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; locale : String.t option
  ; pool_depth : Integer.t option
  ; state : IpamPoolState.t option
  ; state_message : String.t option
  ; description : String.t option
  ; auto_import : Boolean.t option
  ; publicly_advertisable : Boolean.t option
  ; address_family : AddressFamily.t option
  ; allocation_min_netmask_length : Integer.t option
  ; allocation_max_netmask_length : Integer.t option
  ; allocation_default_netmask_length : Integer.t option
  ; allocation_resource_tags : IpamResourceTagList.t
  ; tags : TagList.t
  ; aws_service : IpamPoolAwsService.t option
  ; public_ip_source : IpamPoolPublicIpSource.t option
  ; source_resource : IpamPoolSourceResource.t option
  }

let make
    ?owner_id
    ?ipam_pool_id
    ?source_ipam_pool_id
    ?ipam_pool_arn
    ?ipam_scope_arn
    ?ipam_scope_type
    ?ipam_arn
    ?ipam_region
    ?locale
    ?pool_depth
    ?state
    ?state_message
    ?description
    ?auto_import
    ?publicly_advertisable
    ?address_family
    ?allocation_min_netmask_length
    ?allocation_max_netmask_length
    ?allocation_default_netmask_length
    ?(allocation_resource_tags = [])
    ?(tags = [])
    ?aws_service
    ?public_ip_source
    ?source_resource
    () =
  { owner_id
  ; ipam_pool_id
  ; source_ipam_pool_id
  ; ipam_pool_arn
  ; ipam_scope_arn
  ; ipam_scope_type
  ; ipam_arn
  ; ipam_region
  ; locale
  ; pool_depth
  ; state
  ; state_message
  ; description
  ; auto_import
  ; publicly_advertisable
  ; address_family
  ; allocation_min_netmask_length
  ; allocation_max_netmask_length
  ; allocation_default_netmask_length
  ; allocation_resource_tags
  ; tags
  ; aws_service
  ; public_ip_source
  ; source_resource
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_pool_id = Aws.Util.option_bind (Aws.Xml.member "ipamPoolId" xml) String.parse
    ; source_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceIpamPoolId" xml) String.parse
    ; ipam_pool_arn = Aws.Util.option_bind (Aws.Xml.member "ipamPoolArn" xml) String.parse
    ; ipam_scope_arn =
        Aws.Util.option_bind (Aws.Xml.member "ipamScopeArn" xml) String.parse
    ; ipam_scope_type =
        Aws.Util.option_bind (Aws.Xml.member "ipamScopeType" xml) IpamScopeType.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; locale = Aws.Util.option_bind (Aws.Xml.member "locale" xml) String.parse
    ; pool_depth = Aws.Util.option_bind (Aws.Xml.member "poolDepth" xml) Integer.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) IpamPoolState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; auto_import = Aws.Util.option_bind (Aws.Xml.member "autoImport" xml) Boolean.parse
    ; publicly_advertisable =
        Aws.Util.option_bind (Aws.Xml.member "publiclyAdvertisable" xml) Boolean.parse
    ; address_family =
        Aws.Util.option_bind (Aws.Xml.member "addressFamily" xml) AddressFamily.parse
    ; allocation_min_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "allocationMinNetmaskLength" xml)
          Integer.parse
    ; allocation_max_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "allocationMaxNetmaskLength" xml)
          Integer.parse
    ; allocation_default_netmask_length =
        Aws.Util.option_bind
          (Aws.Xml.member "allocationDefaultNetmaskLength" xml)
          Integer.parse
    ; allocation_resource_tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "allocationResourceTagSet" xml)
             IpamResourceTagList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; aws_service =
        Aws.Util.option_bind (Aws.Xml.member "awsService" xml) IpamPoolAwsService.parse
    ; public_ip_source =
        Aws.Util.option_bind
          (Aws.Xml.member "publicIpSource" xml)
          IpamPoolPublicIpSource.parse
    ; source_resource =
        Aws.Util.option_bind
          (Aws.Xml.member "sourceResource" xml)
          IpamPoolSourceResource.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_resource (fun f ->
             Aws.Query.Pair ("SourceResource", IpamPoolSourceResource.to_query f))
       ; Aws.Util.option_map v.public_ip_source (fun f ->
             Aws.Query.Pair ("PublicIpSource", IpamPoolPublicIpSource.to_query f))
       ; Aws.Util.option_map v.aws_service (fun f ->
             Aws.Query.Pair ("AwsService", IpamPoolAwsService.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "AllocationResourceTagSet"
              , IpamResourceTagList.to_query v.allocation_resource_tags ))
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationDefaultNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMaxNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             Aws.Query.Pair ("AllocationMinNetmaskLength", Integer.to_query f))
       ; Aws.Util.option_map v.address_family (fun f ->
             Aws.Query.Pair ("AddressFamily", AddressFamily.to_query f))
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             Aws.Query.Pair ("PubliclyAdvertisable", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_import (fun f ->
             Aws.Query.Pair ("AutoImport", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamPoolState.to_query f))
       ; Aws.Util.option_map v.pool_depth (fun f ->
             Aws.Query.Pair ("PoolDepth", Integer.to_query f))
       ; Aws.Util.option_map v.locale (fun f ->
             Aws.Query.Pair ("Locale", String.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_scope_type (fun f ->
             Aws.Query.Pair ("IpamScopeType", IpamScopeType.to_query f))
       ; Aws.Util.option_map v.ipam_scope_arn (fun f ->
             Aws.Query.Pair ("IpamScopeArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_arn (fun f ->
             Aws.Query.Pair ("IpamPoolArn", String.to_query f))
       ; Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             Aws.Query.Pair ("SourceIpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.ipam_pool_id (fun f ->
             Aws.Query.Pair ("IpamPoolId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_resource (fun f ->
             "sourceResource", IpamPoolSourceResource.to_json f)
       ; Aws.Util.option_map v.public_ip_source (fun f ->
             "publicIpSource", IpamPoolPublicIpSource.to_json f)
       ; Aws.Util.option_map v.aws_service (fun f ->
             "awsService", IpamPoolAwsService.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "allocationResourceTagSet"
           , IpamResourceTagList.to_json v.allocation_resource_tags )
       ; Aws.Util.option_map v.allocation_default_netmask_length (fun f ->
             "allocationDefaultNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_max_netmask_length (fun f ->
             "allocationMaxNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.allocation_min_netmask_length (fun f ->
             "allocationMinNetmaskLength", Integer.to_json f)
       ; Aws.Util.option_map v.address_family (fun f ->
             "addressFamily", AddressFamily.to_json f)
       ; Aws.Util.option_map v.publicly_advertisable (fun f ->
             "publiclyAdvertisable", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_import (fun f -> "autoImport", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", IpamPoolState.to_json f)
       ; Aws.Util.option_map v.pool_depth (fun f -> "poolDepth", Integer.to_json f)
       ; Aws.Util.option_map v.locale (fun f -> "locale", String.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_scope_type (fun f ->
             "ipamScopeType", IpamScopeType.to_json f)
       ; Aws.Util.option_map v.ipam_scope_arn (fun f -> "ipamScopeArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_arn (fun f -> "ipamPoolArn", String.to_json f)
       ; Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             "sourceIpamPoolId", String.to_json f)
       ; Aws.Util.option_map v.ipam_pool_id (fun f -> "ipamPoolId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_pool_id = Aws.Util.option_map (Aws.Json.lookup j "ipamPoolId") String.of_json
  ; source_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceIpamPoolId") String.of_json
  ; ipam_pool_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamPoolArn") String.of_json
  ; ipam_scope_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamScopeArn") String.of_json
  ; ipam_scope_type =
      Aws.Util.option_map (Aws.Json.lookup j "ipamScopeType") IpamScopeType.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; locale = Aws.Util.option_map (Aws.Json.lookup j "locale") String.of_json
  ; pool_depth = Aws.Util.option_map (Aws.Json.lookup j "poolDepth") Integer.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") IpamPoolState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; auto_import = Aws.Util.option_map (Aws.Json.lookup j "autoImport") Boolean.of_json
  ; publicly_advertisable =
      Aws.Util.option_map (Aws.Json.lookup j "publiclyAdvertisable") Boolean.of_json
  ; address_family =
      Aws.Util.option_map (Aws.Json.lookup j "addressFamily") AddressFamily.of_json
  ; allocation_min_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "allocationMinNetmaskLength") Integer.of_json
  ; allocation_max_netmask_length =
      Aws.Util.option_map (Aws.Json.lookup j "allocationMaxNetmaskLength") Integer.of_json
  ; allocation_default_netmask_length =
      Aws.Util.option_map
        (Aws.Json.lookup j "allocationDefaultNetmaskLength")
        Integer.of_json
  ; allocation_resource_tags =
      IpamResourceTagList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "allocationResourceTagSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; aws_service =
      Aws.Util.option_map (Aws.Json.lookup j "awsService") IpamPoolAwsService.of_json
  ; public_ip_source =
      Aws.Util.option_map
        (Aws.Json.lookup j "publicIpSource")
        IpamPoolPublicIpSource.of_json
  ; source_resource =
      Aws.Util.option_map
        (Aws.Json.lookup j "sourceResource")
        IpamPoolSourceResource.of_json
  }
