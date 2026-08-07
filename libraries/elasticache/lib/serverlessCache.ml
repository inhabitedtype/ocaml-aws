open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t option
  ; description : String.t option
  ; create_time : DateTime.t option
  ; status : String.t option
  ; engine : String.t option
  ; major_engine_version : String.t option
  ; full_engine_version : String.t option
  ; cache_usage_limits : CacheUsageLimits.t option
  ; kms_key_id : String.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; security_group_ids : SecurityGroupIdsList.t
  ; endpoint : Endpoint.t option
  ; reader_endpoint : Endpoint.t option
  ; a_r_n : String.t option
  ; user_group_id : String.t option
  ; subnet_ids : SubnetIdsList.t
  ; snapshot_retention_limit : Integer.t option
  ; daily_snapshot_time : String.t option
  ; network_type : NetworkType.t option
  }

let make
    ?serverless_cache_name
    ?description
    ?create_time
    ?status
    ?engine
    ?major_engine_version
    ?full_engine_version
    ?cache_usage_limits
    ?kms_key_id
    ?storage_encryption_type
    ?(security_group_ids = [])
    ?endpoint
    ?reader_endpoint
    ?a_r_n
    ?user_group_id
    ?(subnet_ids = [])
    ?snapshot_retention_limit
    ?daily_snapshot_time
    ?network_type
    () =
  { serverless_cache_name
  ; description
  ; create_time
  ; status
  ; engine
  ; major_engine_version
  ; full_engine_version
  ; cache_usage_limits
  ; kms_key_id
  ; storage_encryption_type
  ; security_group_ids
  ; endpoint
  ; reader_endpoint
  ; a_r_n
  ; user_group_id
  ; subnet_ids
  ; snapshot_retention_limit
  ; daily_snapshot_time
  ; network_type
  }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "CreateTime" xml) DateTime.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; full_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "FullEngineVersion" xml) String.parse
    ; cache_usage_limits =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheUsageLimits" xml)
          CacheUsageLimits.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupIds" xml)
             SecurityGroupIdsList.parse)
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) Endpoint.parse
    ; reader_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ReaderEndpoint" xml) Endpoint.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
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
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             Aws.Query.Pair ("ReaderEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", Endpoint.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIds.member"
              , SecurityGroupIdsList.to_query v.security_group_ids ))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             Aws.Query.Pair ("CacheUsageLimits", CacheUsageLimits.to_query f))
       ; Aws.Util.option_map v.full_engine_version (fun f ->
             Aws.Query.Pair ("FullEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheName", String.to_query f))
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
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             "ReaderEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", Endpoint.to_json f)
       ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.cache_usage_limits (fun f ->
             "CacheUsageLimits", CacheUsageLimits.to_json f)
       ; Aws.Util.option_map v.full_engine_version (fun f ->
             "FullEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "CreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             "ServerlessCacheName", String.to_json f)
       ])

let of_json j =
  { serverless_cache_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheName") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "CreateTime") DateTime.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; full_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "FullEngineVersion") String.of_json
  ; cache_usage_limits =
      Aws.Util.option_map (Aws.Json.lookup j "CacheUsageLimits") CacheUsageLimits.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; security_group_ids =
      SecurityGroupIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") Endpoint.of_json
  ; reader_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ReaderEndpoint") Endpoint.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
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
