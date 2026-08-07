open Aws.BaseTypes

type t =
  { global_cluster_identifier : String.t option
  ; global_cluster_resource_id : String.t option
  ; global_cluster_arn : String.t option
  ; status : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; engine_lifecycle_support : String.t option
  ; database_name : String.t option
  ; storage_encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; deletion_protection : Boolean.t option
  ; global_cluster_members : GlobalClusterMemberList.t
  ; endpoint : String.t option
  ; failover_state : FailoverState.t option
  ; tag_list : TagList.t
  }

let make
    ?global_cluster_identifier
    ?global_cluster_resource_id
    ?global_cluster_arn
    ?status
    ?engine
    ?engine_version
    ?engine_lifecycle_support
    ?database_name
    ?storage_encrypted
    ?storage_encryption_type
    ?deletion_protection
    ?(global_cluster_members = [])
    ?endpoint
    ?failover_state
    ?(tag_list = [])
    () =
  { global_cluster_identifier
  ; global_cluster_resource_id
  ; global_cluster_arn
  ; status
  ; engine
  ; engine_version
  ; engine_lifecycle_support
  ; database_name
  ; storage_encrypted
  ; storage_encryption_type
  ; deletion_protection
  ; global_cluster_members
  ; endpoint
  ; failover_state
  ; tag_list
  }

let parse xml =
  Some
    { global_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterIdentifier" xml) String.parse
    ; global_cluster_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterResourceId" xml) String.parse
    ; global_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterArn" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
    ; database_name =
        Aws.Util.option_bind (Aws.Xml.member "DatabaseName" xml) String.parse
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; global_cluster_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "GlobalClusterMembers" xml)
             GlobalClusterMemberList.parse)
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; failover_state =
        Aws.Util.option_bind (Aws.Xml.member "FailoverState" xml) FailoverState.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.failover_state (fun f ->
             Aws.Query.Pair ("FailoverState", FailoverState.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "GlobalClusterMembers.member"
              , GlobalClusterMemberList.to_query v.global_cluster_members ))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.database_name (fun f ->
             Aws.Query.Pair ("DatabaseName", String.to_query f))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.global_cluster_arn (fun f ->
             Aws.Query.Pair ("GlobalClusterArn", String.to_query f))
       ; Aws.Util.option_map v.global_cluster_resource_id (fun f ->
             Aws.Query.Pair ("GlobalClusterResourceId", String.to_query f))
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             Aws.Query.Pair ("GlobalClusterIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.failover_state (fun f ->
             "FailoverState", FailoverState.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Some
           ( "GlobalClusterMembers"
           , GlobalClusterMemberList.to_json v.global_cluster_members )
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.database_name (fun f -> "DatabaseName", String.to_json f)
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.global_cluster_arn (fun f ->
             "GlobalClusterArn", String.to_json f)
       ; Aws.Util.option_map v.global_cluster_resource_id (fun f ->
             "GlobalClusterResourceId", String.to_json f)
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             "GlobalClusterIdentifier", String.to_json f)
       ])

let of_json j =
  { global_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterIdentifier") String.of_json
  ; global_cluster_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterResourceId") String.of_json
  ; global_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterArn") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; database_name = Aws.Util.option_map (Aws.Json.lookup j "DatabaseName") String.of_json
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; global_cluster_members =
      GlobalClusterMemberList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "GlobalClusterMembers"))
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; failover_state =
      Aws.Util.option_map (Aws.Json.lookup j "FailoverState") FailoverState.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
