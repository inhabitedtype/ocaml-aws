open Aws.BaseTypes

type t =
  { ipam_prefix_list_resolver_target_id : String.t option
  ; ipam_prefix_list_resolver_target_arn : String.t option
  ; ipam_prefix_list_resolver_id : String.t option
  ; owner_id : String.t option
  ; prefix_list_id : String.t option
  ; prefix_list_region : String.t option
  ; desired_version : Long.t option
  ; last_synced_version : Long.t option
  ; track_latest_version : Boolean.t option
  ; state_message : String.t option
  ; state : IpamPrefixListResolverTargetState.t option
  ; tags : TagList.t
  }

let make
    ?ipam_prefix_list_resolver_target_id
    ?ipam_prefix_list_resolver_target_arn
    ?ipam_prefix_list_resolver_id
    ?owner_id
    ?prefix_list_id
    ?prefix_list_region
    ?desired_version
    ?last_synced_version
    ?track_latest_version
    ?state_message
    ?state
    ?(tags = [])
    () =
  { ipam_prefix_list_resolver_target_id
  ; ipam_prefix_list_resolver_target_arn
  ; ipam_prefix_list_resolver_id
  ; owner_id
  ; prefix_list_id
  ; prefix_list_region
  ; desired_version
  ; last_synced_version
  ; track_latest_version
  ; state_message
  ; state
  ; tags
  }

let parse xml =
  Some
    { ipam_prefix_list_resolver_target_id =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolverTargetId" xml)
          String.parse
    ; ipam_prefix_list_resolver_target_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "ipamPrefixListResolverTargetArn" xml)
          String.parse
    ; ipam_prefix_list_resolver_id =
        Aws.Util.option_bind (Aws.Xml.member "ipamPrefixListResolverId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; prefix_list_id =
        Aws.Util.option_bind (Aws.Xml.member "prefixListId" xml) String.parse
    ; prefix_list_region =
        Aws.Util.option_bind (Aws.Xml.member "prefixListRegion" xml) String.parse
    ; desired_version =
        Aws.Util.option_bind (Aws.Xml.member "desiredVersion" xml) Long.parse
    ; last_synced_version =
        Aws.Util.option_bind (Aws.Xml.member "lastSyncedVersion" xml) Long.parse
    ; track_latest_version =
        Aws.Util.option_bind (Aws.Xml.member "trackLatestVersion" xml) Boolean.parse
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "stateMessage" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          IpamPrefixListResolverTargetState.parse
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
             Aws.Query.Pair ("State", IpamPrefixListResolverTargetState.to_query f))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StateMessage", String.to_query f))
       ; Aws.Util.option_map v.track_latest_version (fun f ->
             Aws.Query.Pair ("TrackLatestVersion", Boolean.to_query f))
       ; Aws.Util.option_map v.last_synced_version (fun f ->
             Aws.Query.Pair ("LastSyncedVersion", Long.to_query f))
       ; Aws.Util.option_map v.desired_version (fun f ->
             Aws.Query.Pair ("DesiredVersion", Long.to_query f))
       ; Aws.Util.option_map v.prefix_list_region (fun f ->
             Aws.Query.Pair ("PrefixListRegion", String.to_query f))
       ; Aws.Util.option_map v.prefix_list_id (fun f ->
             Aws.Query.Pair ("PrefixListId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_id (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverId", String.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_arn (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverTargetArn", String.to_query f))
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_id (fun f ->
             Aws.Query.Pair ("IpamPrefixListResolverTargetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f ->
             "state", IpamPrefixListResolverTargetState.to_json f)
       ; Aws.Util.option_map v.state_message (fun f -> "stateMessage", String.to_json f)
       ; Aws.Util.option_map v.track_latest_version (fun f ->
             "trackLatestVersion", Boolean.to_json f)
       ; Aws.Util.option_map v.last_synced_version (fun f ->
             "lastSyncedVersion", Long.to_json f)
       ; Aws.Util.option_map v.desired_version (fun f -> "desiredVersion", Long.to_json f)
       ; Aws.Util.option_map v.prefix_list_region (fun f ->
             "prefixListRegion", String.to_json f)
       ; Aws.Util.option_map v.prefix_list_id (fun f -> "prefixListId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_id (fun f ->
             "ipamPrefixListResolverId", String.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_arn (fun f ->
             "ipamPrefixListResolverTargetArn", String.to_json f)
       ; Aws.Util.option_map v.ipam_prefix_list_resolver_target_id (fun f ->
             "ipamPrefixListResolverTargetId", String.to_json f)
       ])

let of_json j =
  { ipam_prefix_list_resolver_target_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolverTargetId")
        String.of_json
  ; ipam_prefix_list_resolver_target_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "ipamPrefixListResolverTargetArn")
        String.of_json
  ; ipam_prefix_list_resolver_id =
      Aws.Util.option_map (Aws.Json.lookup j "ipamPrefixListResolverId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; prefix_list_id = Aws.Util.option_map (Aws.Json.lookup j "prefixListId") String.of_json
  ; prefix_list_region =
      Aws.Util.option_map (Aws.Json.lookup j "prefixListRegion") String.of_json
  ; desired_version =
      Aws.Util.option_map (Aws.Json.lookup j "desiredVersion") Long.of_json
  ; last_synced_version =
      Aws.Util.option_map (Aws.Json.lookup j "lastSyncedVersion") Long.of_json
  ; track_latest_version =
      Aws.Util.option_map (Aws.Json.lookup j "trackLatestVersion") Boolean.of_json
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "stateMessage") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        IpamPrefixListResolverTargetState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
