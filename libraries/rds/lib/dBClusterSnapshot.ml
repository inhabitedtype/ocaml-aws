open Aws.BaseTypes

type t =
  { availability_zones : AvailabilityZones.t
  ; d_b_cluster_snapshot_identifier : String.t option
  ; d_b_cluster_identifier : String.t option
  ; snapshot_create_time : DateTime.t option
  ; engine : String.t option
  ; engine_mode : String.t option
  ; allocated_storage : Integer.t option
  ; status : String.t option
  ; port : Integer.t option
  ; vpc_id : String.t option
  ; cluster_create_time : DateTime.t option
  ; master_username : String.t option
  ; engine_version : String.t option
  ; license_model : String.t option
  ; snapshot_type : String.t option
  ; percent_progress : Integer.t option
  ; storage_encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; kms_key_id : String.t option
  ; d_b_cluster_snapshot_arn : String.t option
  ; source_d_b_cluster_snapshot_arn : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; tag_list : TagList.t
  ; storage_type : String.t option
  ; storage_throughput : Integer.t option
  ; db_cluster_resource_id : String.t option
  ; d_b_system_id : String.t option
  }

let make
    ?(availability_zones = [])
    ?d_b_cluster_snapshot_identifier
    ?d_b_cluster_identifier
    ?snapshot_create_time
    ?engine
    ?engine_mode
    ?allocated_storage
    ?status
    ?port
    ?vpc_id
    ?cluster_create_time
    ?master_username
    ?engine_version
    ?license_model
    ?snapshot_type
    ?percent_progress
    ?storage_encrypted
    ?storage_encryption_type
    ?backup_retention_period
    ?preferred_backup_window
    ?kms_key_id
    ?d_b_cluster_snapshot_arn
    ?source_d_b_cluster_snapshot_arn
    ?i_a_m_database_authentication_enabled
    ?(tag_list = [])
    ?storage_type
    ?storage_throughput
    ?db_cluster_resource_id
    ?d_b_system_id
    () =
  { availability_zones
  ; d_b_cluster_snapshot_identifier
  ; d_b_cluster_identifier
  ; snapshot_create_time
  ; engine
  ; engine_mode
  ; allocated_storage
  ; status
  ; port
  ; vpc_id
  ; cluster_create_time
  ; master_username
  ; engine_version
  ; license_model
  ; snapshot_type
  ; percent_progress
  ; storage_encrypted
  ; storage_encryption_type
  ; backup_retention_period
  ; preferred_backup_window
  ; kms_key_id
  ; d_b_cluster_snapshot_arn
  ; source_d_b_cluster_snapshot_arn
  ; i_a_m_database_authentication_enabled
  ; tag_list
  ; storage_type
  ; storage_throughput
  ; db_cluster_resource_id
  ; d_b_system_id
  }

let parse xml =
  Some
    { availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; d_b_cluster_snapshot_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
          String.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; snapshot_create_time =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotCreateTime" xml) DateTime.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_mode = Aws.Util.option_bind (Aws.Xml.member "EngineMode" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; cluster_create_time =
        Aws.Util.option_bind (Aws.Xml.member "ClusterCreateTime" xml) DateTime.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; percent_progress =
        Aws.Util.option_bind (Aws.Xml.member "PercentProgress" xml) Integer.parse
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; d_b_cluster_snapshot_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterSnapshotArn" xml) String.parse
    ; source_d_b_cluster_snapshot_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceDBClusterSnapshotArn" xml)
          String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; db_cluster_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbClusterResourceId" xml) String.parse
    ; d_b_system_id = Aws.Util.option_bind (Aws.Xml.member "DBSystemId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_system_id (fun f ->
             Aws.Query.Pair ("DBSystemId", String.to_query f))
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             Aws.Query.Pair ("DbClusterResourceId", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.source_d_b_cluster_snapshot_arn (fun f ->
             Aws.Query.Pair ("SourceDBClusterSnapshotArn", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_snapshot_arn (fun f ->
             Aws.Query.Pair ("DBClusterSnapshotArn", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.percent_progress (fun f ->
             Aws.Query.Pair ("PercentProgress", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             Aws.Query.Pair ("ClusterCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.engine_mode (fun f ->
             Aws.Query.Pair ("EngineMode", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.snapshot_create_time (fun f ->
             Aws.Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_system_id (fun f -> "DBSystemId", String.to_json f)
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             "DbClusterResourceId", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.source_d_b_cluster_snapshot_arn (fun f ->
             "SourceDBClusterSnapshotArn", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_snapshot_arn (fun f ->
             "DBClusterSnapshotArn", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.percent_progress (fun f ->
             "PercentProgress", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             "ClusterCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.engine_mode (fun f -> "EngineMode", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.snapshot_create_time (fun f ->
             "SnapshotCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             "DBClusterSnapshotIdentifier", String.to_json f)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ])

let of_json j =
  { availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; d_b_cluster_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterSnapshotIdentifier") String.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; snapshot_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotCreateTime") DateTime.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_mode = Aws.Util.option_map (Aws.Json.lookup j "EngineMode") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; cluster_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterCreateTime") DateTime.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; percent_progress =
      Aws.Util.option_map (Aws.Json.lookup j "PercentProgress") Integer.of_json
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; d_b_cluster_snapshot_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterSnapshotArn") String.of_json
  ; source_d_b_cluster_snapshot_arn =
      Aws.Util.option_map (Aws.Json.lookup j "SourceDBClusterSnapshotArn") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; db_cluster_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbClusterResourceId") String.of_json
  ; d_b_system_id = Aws.Util.option_map (Aws.Json.lookup j "DBSystemId") String.of_json
  }
