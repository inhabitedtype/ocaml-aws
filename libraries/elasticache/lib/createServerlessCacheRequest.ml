open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t
  ; description : String.t option
  ; engine : String.t
  ; major_engine_version : String.t option
  ; cache_usage_limits : CacheUsageLimits.t option
  ; kms_key_id : String.t option
  ; security_group_ids : SecurityGroupIdsList.t
  ; snapshot_arns_to_restore : SnapshotArnsList.t
  ; tags : TagList.t
  ; user_group_id : String.t option
  ; subnet_ids : SubnetIdsList.t
  ; snapshot_retention_limit : Integer.t option
  ; daily_snapshot_time : String.t option
  ; network_type : NetworkType.t option
  }

let make
    ~serverless_cache_name
    ?description
    ~engine
    ?major_engine_version
    ?cache_usage_limits
    ?kms_key_id
    ?(security_group_ids = [])
    ?(snapshot_arns_to_restore = [])
    ?(tags = [])
    ?user_group_id
    ?(subnet_ids = [])
    ?snapshot_retention_limit
    ?daily_snapshot_time
    ?network_type
    () =
  { serverless_cache_name
  ; description
  ; engine
  ; major_engine_version
  ; cache_usage_limits
  ; kms_key_id
  ; security_group_ids
  ; snapshot_arns_to_restore
  ; tags
  ; user_group_id
  ; subnet_ids
  ; snapshot_retention_limit
  ; daily_snapshot_time
  ; network_type
  }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Xml.required
          "ServerlessCacheName"
          (Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse)
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; cache_usage_limits =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheUsageLimits" xml)
          CacheUsageLimits.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupIds" xml)
             SecurityGroupIdsList.parse)
    ; snapshot_arns_to_restore =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SnapshotArnsToRestore" xml)
             SnapshotArnsList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; user_group_id = Aws.Util.option_bind (Aws.Xml.member "UserGroupId" xml) String.parse
    ; subnet_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "SubnetIds" xml) SubnetIdsList.parse)
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; daily_snapshot_time =
        Aws.Util.option_bind (Aws.Xml.member "DailySnapshotTime" xml) String.parse
    ; network_type =
        Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) NetworkType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", NetworkType.to_query f))
       ; Aws.Util.option_map v.daily_snapshot_time (fun f ->
             Aws.Query.Pair ("DailySnapshotTime", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SubnetIds.member", SubnetIdsList.to_query v.subnet_ids))
       ; Aws.Util.option_map v.user_group_id (fun f ->
             Aws.Query.Pair ("UserGroupId", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "SnapshotArnsToRestore.member"
              , SnapshotArnsList.to_query v.snapshot_arns_to_restore ))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIds.member"
              , SecurityGroupIdsList.to_query v.security_group_ids ))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             Aws.Query.Pair ("CacheUsageLimits", CacheUsageLimits.to_query f))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ServerlessCacheName", String.to_query v.serverless_cache_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.network_type (fun f ->
             "NetworkType", NetworkType.to_json f)
       ; Aws.Util.option_map v.daily_snapshot_time (fun f ->
             "DailySnapshotTime", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Some ("SubnetIds", SubnetIdsList.to_json v.subnet_ids)
       ; Aws.Util.option_map v.user_group_id (fun f -> "UserGroupId", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some
           ("SnapshotArnsToRestore", SnapshotArnsList.to_json v.snapshot_arns_to_restore)
       ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             "CacheUsageLimits", CacheUsageLimits.to_json f)
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Some ("Engine", String.to_json v.engine)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Some ("ServerlessCacheName", String.to_json v.serverless_cache_name)
       ])

let of_json j =
  { serverless_cache_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheName"))
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; cache_usage_limits =
      Aws.Util.option_map (Aws.Json.lookup j "CacheUsageLimits") CacheUsageLimits.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; security_group_ids =
      SecurityGroupIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
  ; snapshot_arns_to_restore =
      SnapshotArnsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotArnsToRestore"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; user_group_id = Aws.Util.option_map (Aws.Json.lookup j "UserGroupId") String.of_json
  ; subnet_ids =
      SubnetIdsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SubnetIds"))
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; daily_snapshot_time =
      Aws.Util.option_map (Aws.Json.lookup j "DailySnapshotTime") String.of_json
  ; network_type =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkType") NetworkType.of_json
  }
