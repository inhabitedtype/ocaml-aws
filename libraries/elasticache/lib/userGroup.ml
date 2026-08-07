open Aws.BaseTypes

type t =
  { user_group_id : String.t option
  ; status : String.t option
  ; engine : String.t option
  ; user_ids : UserIdList.t
  ; minimum_engine_version : String.t option
  ; pending_changes : UserGroupPendingChanges.t option
  ; replication_groups : UGReplicationGroupIdList.t
  ; serverless_caches : UGServerlessCacheIdList.t
  ; a_r_n : String.t option
  }

let make
    ?user_group_id
    ?status
    ?engine
    ?(user_ids = [])
    ?minimum_engine_version
    ?pending_changes
    ?(replication_groups = [])
    ?(serverless_caches = [])
    ?a_r_n
    () =
  { user_group_id
  ; status
  ; engine
  ; user_ids
  ; minimum_engine_version
  ; pending_changes
  ; replication_groups
  ; serverless_caches
  ; a_r_n
  }

let parse xml =
  Some
    { user_group_id = Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserIds" xml) UserIdList.parse)
    ; minimum_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MinimumEngineVersion" xml) String.parse
    ; pending_changes =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingChanges" xml)
          UserGroupPendingChanges.parse
    ; replication_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroups" xml)
             UGReplicationGroupIdList.parse)
    ; serverless_caches =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessCaches" xml)
             UGServerlessCacheIdList.parse)
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ServerlessCaches.member"
              , UGServerlessCacheIdList.to_query v.serverless_caches ))
       ; Some
           (Aws.Query.Pair
              ( "ReplicationGroups.member"
              , UGReplicationGroupIdList.to_query v.replication_groups ))
       ; Aws.Util.option_map v.pending_changes (fun f ->
             Aws.Query.Pair ("PendingChanges", UserGroupPendingChanges.to_query f))
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             Aws.Query.Pair ("MinimumEngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("UserIds.member", UserIdList.to_query v.user_ids))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.user_group_id (fun f ->
             Aws.Query.Pair ("UserGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Some ("ServerlessCaches", UGServerlessCacheIdList.to_json v.serverless_caches)
       ; Some ("ReplicationGroups", UGReplicationGroupIdList.to_json v.replication_groups)
       ; Aws.Util.option_map v.pending_changes (fun f ->
             "PendingChanges", UserGroupPendingChanges.to_json f)
       ; Aws.Util.option_map v.minimum_engine_version (fun f ->
             "MinimumEngineVersion", String.to_json f)
       ; Some ("UserIds", UserIdList.to_json v.user_ids)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
       ])

let of_json j =
  { user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; user_ids = UserIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserIds"))
  ; minimum_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MinimumEngineVersion") String.of_json
  ; pending_changes =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingChanges")
        UserGroupPendingChanges.of_json
  ; replication_groups =
      UGReplicationGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroups"))
  ; serverless_caches =
      UGServerlessCacheIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCaches"))
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  }
