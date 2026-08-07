open Aws.BaseTypes

type t =
  { prefix_list_id : String.t option
  ; address_family : String.t option
  ; state : PrefixListState.t option
  ; state_message : String.t option
  ; prefix_list_arn : String.t option
  ; prefix_list_name : String.t option
  ; max_entries : Integer.t option
  ; version : Long.t option
  ; tags : TagList.t
  ; owner_id : String.t option
  ; ipam_prefix_list_resolver_target_id : String.t option
  ; ipam_prefix_list_resolver_sync_enabled : Boolean.t option
  }

let make
    ?prefix_list_id
    ?address_family
    ?state
    ?state_message
    ?prefix_list_arn
    ?prefix_list_name
    ?max_entries
    ?version
    ?(tags = [])
    ?owner_id
    ?ipam_prefix_list_resolver_target_id
    ?ipam_prefix_list_resolver_sync_enabled
    () =
  { prefix_list_id
  ; address_family
  ; state
  ; state_message
  ; prefix_list_arn
  ; prefix_list_name
  ; max_entries
  ; version
  ; tags
  ; owner_id
  ; ipam_prefix_list_resolver_target_id
  ; ipam_prefix_list_resolver_sync_enabled
  }

let parse xml =
  Some
    { prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; address_family =
        Aws.Util.option_bind (Aws.Xml.member "addressFamily" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) PrefixListState.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; prefix_list_arn =
        Aws.Util.option_bind (Aws.Xml.member "prefixListArn" xml) String.parse
    ; prefix_list_name =
        Aws.Util.option_bind (Aws.Xml.member "prefixListName" xml) String.parse
    ; max_entries = Aws.Util.option_bind (Aws.Xml.member "maxEntries" xml) Integer.parse
    ; version = Aws.Util.option_bind (Aws.Xml.member "version" xml) Long.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; ipam_prefix_list_resolver_target_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolverTargetId" xml)
          String.parse
    ; ipam_prefix_list_resolver_sync_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolverSyncEnabled" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_sync_enabled (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverSyncEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_id (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverTargetId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", Long.to_query f))
       ; Aws.Util.option_map v.max_entries (fun f ->
             Aws.Query.Pair ("MaxEntries", Integer.to_query f))
       ; Aws.Util.option_map v.prefix_list_name (fun f ->
             Aws.Query.Pair ("PrefixListName", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_arn (fun f ->
             Aws.Query.Pair ("PrefixListArn", String.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", PrefixListState.to_query f))
       ; Aws.Util.option_map v.address_family (fun f ->
             Aws.Query.Pair ("AddressFamily", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ipam_prefix_list_resolver_sync_enabled (fun f ->
             "ipamPrefixListResolverSyncEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_id (fun f ->
             "ipamPrefixListResolverTargetId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.version (fun f -> "version", Long.to_json f)
       ; Aws.Util.option_map v.max_entries (fun f -> "maxEntries", Integer.to_json f)
       ; Aws.Util.option_map v.prefix_list_name (fun f ->
             "prefixListName", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_arn (fun f ->
             "prefixListArn", String.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", PrefixListState.to_json f)
       ; Aws.Util.option_map v.address_family (fun f -> "addressFamily", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ])

let of_json j =
  { prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; address_family =
      Aws.Util.option_map (Aws.Json.lookup j "addressFamily") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") PrefixListState.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; prefix_list_arn =
      Aws.Util.option_map (Aws.Json.lookup j "prefixListArn") String.of_json
  ; prefix_list_name =
      Aws.Util.option_map (Aws.Json.lookup j "prefixListName") String.of_json
  ; max_entries = Aws.Util.option_map (Aws.Json.lookup j "maxEntries") Integer.of_json
  ; version = Aws.Util.option_map (Aws.Json.lookup j "version") Long.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; ipam_prefix_list_resolver_target_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolverTargetId")
        String.of_json
  ; ipam_prefix_list_resolver_sync_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolverSyncEnabled")
        Boolean.of_json
  }
