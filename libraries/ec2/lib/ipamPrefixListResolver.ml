open Aws.BaseTypes

type t =
  { owner_id : String.t option
  ; ipam_prefix_list_resolver_id : String.t option
  ; ipam_prefix_list_resolver_arn : String.t option
  ; ipam_arn : String.t option
  ; ipam_region : String.t option
  ; description : String.t option
  ; address_family : AddressFamily.t option
  ; state : IpamPrefixListResolverState.t option
  ; tags : TagList.t
  ; last_version_creation_status : IpamPrefixListResolverVersionCreationStatus.t option
  ; last_version_creation_status_message : String.t option
  }

let make
    ?owner_id
    ?ipam_prefix_list_resolver_id
    ?ipam_prefix_list_resolver_arn
    ?ipam_arn
    ?ipam_region
    ?description
    ?address_family
    ?state
    ?(tags = [])
    ?last_version_creation_status
    ?last_version_creation_status_message
    () =
  { owner_id
  ; ipam_prefix_list_resolver_id
  ; ipam_prefix_list_resolver_arn
  ; ipam_arn
  ; ipam_region
  ; description
  ; address_family
  ; state
  ; tags
  ; last_version_creation_status
  ; last_version_creation_status_message
  }

let parse xml =
  Some
    { owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_prefix_list_resolver_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPrefixListResolverId" xml) String.parse
    ; ipam_prefix_list_resolver_arn =
        Aws.Util.option_bind (Aws.Xml.member "ipamPrefixListResolverArn" xml) String.parse
    ; ipam_arn = Aws.Util.option_bind (Aws.Xml.member "ipamArn" xml) String.parse
    ; ipam_region = Aws.Util.option_bind (Aws.Xml.member "ipamRegion" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; address_family =
        Aws.Util.option_bind (Aws.Xml.member "addressFamily" xml) AddressFamily.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          IpamPrefixListResolverState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; last_version_creation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "lastVersionCreationStatus" xml)
          IpamPrefixListResolverVersionCreationStatus.parse
    ; last_version_creation_status_message =
        Aws.Util.option_bind
          (Aws.Xml.member "lastVersionCreationStatusMessage" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_version_creation_status_message (fun f ->
             Aws.Query.Pair ("LastVersionCreationStatusMessage", String.to_query f))
       ; Aws.Util.option_map v.last_version_creation_status (fun f ->
             Aws.Query.Pair
               ( "LastVersionCreationStatus"
               , IpamPrefixListResolverVersionCreationStatus.to_query f ))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", IpamPrefixListResolverState.to_query f))
       ; Aws.Util.option_map v.address_family (fun f ->
             Aws.Query.Pair ("AddressFamily", AddressFamily.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.ipam_region (fun f ->
             Aws.Query.Pair ("IpamRegion", String.to_query f))
       ; Aws.Util.option_map v.ipam_arn (fun f ->
             Aws.Query.Pair ("IpamArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_arn (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_id (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.last_version_creation_status_message (fun f ->
             "lastVersionCreationStatusMessage", String.to_json f)
       ; Aws.Util.option_map v.last_version_creation_status (fun f ->
             ( "lastVersionCreationStatus"
             , IpamPrefixListResolverVersionCreationStatus.to_json f ))
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", IpamPrefixListResolverState.to_json f)
       ; Aws.Util.option_map v.address_family (fun f ->
             "addressFamily", AddressFamily.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.ipam_region (fun f -> "ipamRegion", String.to_json f)
       ; Aws.Util.option_map v.ipam_arn (fun f -> "ipamArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_arn (fun f ->
             "ipamPrefixListResolverArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_id (fun f ->
             "ipamPrefixListResolverId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ])

let of_json j =
  { owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_prefix_list_resolver_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPrefixListResolverId") String.of_json
  ; ipam_prefix_list_resolver_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPrefixListResolverArn") String.of_json
  ; ipam_arn = Aws.Util.option_map (Aws.Json.lookup j "ipamArn") String.of_json
  ; ipam_region = Aws.Util.option_map (Aws.Json.lookup j "ipamRegion") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; address_family =
      Aws.Util.option_map (Aws.Json.lookup j "addressFamily") AddressFamily.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") IpamPrefixListResolverState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; last_version_creation_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastVersionCreationStatus")
        IpamPrefixListResolverVersionCreationStatus.of_json
  ; last_version_creation_status_message =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastVersionCreationStatusMessage")
        String.of_json
  }
