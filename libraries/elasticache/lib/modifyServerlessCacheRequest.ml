open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t
  ; description : String.t option
  ; cache_usage_limits : CacheUsageLimits.t option
  ; remove_user_group : Boolean.t option
  ; user_group_id : String.t option
  ; security_group_ids : SecurityGroupIdsList.t
  ; snapshot_retention_limit : Integer.t option
  ; daily_snapshot_time : String.t option
  ; engine : String.t option
  ; major_engine_version : String.t option
  }

let make
    ~serverless_cache_name
    ?description
    ?cache_usage_limits
    ?remove_user_group
    ?user_group_id
    ?(security_group_ids = [])
    ?snapshot_retention_limit
    ?daily_snapshot_time
    ?engine
    ?major_engine_version
    () =
  { serverless_cache_name
  ; description
  ; cache_usage_limits
  ; remove_user_group
  ; user_group_id
  ; security_group_ids
  ; snapshot_retention_limit
  ; daily_snapshot_time
  ; engine
  ; major_engine_version
  }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Xml.required
          "ServerlessCacheName"
          (Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; cache_usage_limits =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheUsageLimits" xml)
          CacheUsageLimits.parse
    ; remove_user_group =
        Aws.Util.option_bind (Aws.Xml.member "RemoveUserGroup" xml) Boolean.parse
    ; user_group_id = Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupIds" xml)
             SecurityGroupIdsList.parse)
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; daily_snapshot_time =
        Aws.Util.option_bind (Aws.Xml.member "DailySnapshotTime" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.daily_snapshot_time (fun f ->
             Aws.Query.Pair ("DailySnapshotTime", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIds.member"
              , SecurityGroupIdsList.to_query v.security_group_ids ))
       ; Aws.Util.option_map v.user_group_id (fun f ->
             Aws.Query.Pair ("UserGroupId", String.to_query f))
       ; Aws.Util.option_map v.remove_user_group (fun f ->
             Aws.Query.Pair ("RemoveUserGroup", Boolean.to_query f))
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             Aws.Query.Pair ("CacheUsageLimits", CacheUsageLimits.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ServerlessCacheName", String.to_query v.serverless_cache_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.daily_snapshot_time (fun f ->
             "DailySnapshotTime", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
       ; Aws.Util.option_map v.remove_user_group (fun f ->
             "RemoveUserGroup", Boolean.to_json f)
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             "CacheUsageLimits", CacheUsageLimits.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("ServerlessCacheName", String.to_json v.serverless_cache_name)
       ])

let of_json j =
  { serverless_cache_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheName"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; cache_usage_limits =
      Aws.Util.option_map (Aws.Json.lookup j "CacheUsageLimits") CacheUsageLimits.of_json
  ; remove_user_group =
      Aws.Util.option_map (Aws.Json.lookup j "RemoveUserGroup") Boolean.of_json
  ; user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
  ; security_group_ids =
      SecurityGroupIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; daily_snapshot_time =
      Aws.Util.option_map (Aws.Json.lookup j "DailySnapshotTime") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  }
