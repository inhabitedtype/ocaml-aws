open Aws.BaseTypes

type t =
  { engine : String.t option
  ; vpc_id : String.t option
  ; d_b_cluster_automated_backups_arn : String.t option
  ; d_b_cluster_identifier : String.t option
  ; restore_window : RestoreWindow.t option
  ; master_username : String.t option
  ; db_cluster_resource_id : String.t option
  ; region : String.t option
  ; license_model : String.t option
  ; status : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; cluster_create_time : DateTime.t option
  ; storage_encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; allocated_storage : Integer.t option
  ; engine_version : String.t option
  ; d_b_cluster_arn : String.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; engine_mode : String.t option
  ; availability_zones : AvailabilityZones.t
  ; port : Integer.t option
  ; kms_key_id : String.t option
  ; storage_type : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; aws_backup_recovery_point_arn : String.t option
  ; tag_list : TagList.t
  }

let make
    ?engine
    ?vpc_id
    ?d_b_cluster_automated_backups_arn
    ?d_b_cluster_identifier
    ?restore_window
    ?master_username
    ?db_cluster_resource_id
    ?region
    ?license_model
    ?status
    ?i_a_m_database_authentication_enabled
    ?cluster_create_time
    ?storage_encrypted
    ?storage_encryption_type
    ?allocated_storage
    ?engine_version
    ?d_b_cluster_arn
    ?backup_retention_period
    ?preferred_backup_window
    ?engine_mode
    ?(availability_zones = [])
    ?port
    ?kms_key_id
    ?storage_type
    ?iops
    ?storage_throughput
    ?aws_backup_recovery_point_arn
    ?(tag_list = [])
    () =
  { engine
  ; vpc_id
  ; d_b_cluster_automated_backups_arn
  ; d_b_cluster_identifier
  ; restore_window
  ; master_username
  ; db_cluster_resource_id
  ; region
  ; license_model
  ; status
  ; i_a_m_database_authentication_enabled
  ; cluster_create_time
  ; storage_encrypted
  ; storage_encryption_type
  ; allocated_storage
  ; engine_version
  ; d_b_cluster_arn
  ; backup_retention_period
  ; preferred_backup_window
  ; engine_mode
  ; availability_zones
  ; port
  ; kms_key_id
  ; storage_type
  ; iops
  ; storage_throughput
  ; aws_backup_recovery_point_arn
  ; tag_list
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; d_b_cluster_automated_backups_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterAutomatedBackupsArn" xml)
          String.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; restore_window =
        Aws.Util.option_bind (Aws.Xml.member "RestoreWindow" xml) RestoreWindow.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; db_cluster_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbClusterResourceId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; cluster_create_time =
        Aws.Util.option_bind (Aws.Xml.member "ClusterCreateTime" xml) DateTime.parse
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; d_b_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterArn" xml) String.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; engine_mode = Aws.Util.option_bind (Aws.Xml.member "EngineMode" xml) String.parse
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; aws_backup_recovery_point_arn =
        Aws.Util.option_bind (Aws.Xml.member "AwsBackupRecoveryPointArn" xml) String.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             Aws.Query.Pair ("AwsBackupRecoveryPointArn", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ; Aws.Util.option_map v.engine_mode (fun f ->
             Aws.Query.Pair ("EngineMode", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f ->
             Aws.Query.Pair ("DBClusterArn", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             Aws.Query.Pair ("ClusterCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             Aws.Query.Pair ("DbClusterResourceId", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.restore_window (fun f ->
             Aws.Query.Pair ("RestoreWindow", RestoreWindow.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_automated_backups_arn (fun f ->
             Aws.Query.Pair ("DBClusterAutomatedBackupsArn", String.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             "AwsBackupRecoveryPointArn", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ; Aws.Util.option_map v.engine_mode (fun f -> "EngineMode", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f -> "DBClusterArn", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             "ClusterCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             "DbClusterResourceId", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.restore_window (fun f ->
             "RestoreWindow", RestoreWindow.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_automated_backups_arn (fun f ->
             "DBClusterAutomatedBackupsArn", String.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; d_b_cluster_automated_backups_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterAutomatedBackupsArn")
        String.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; restore_window =
      Aws.Util.option_map (Aws.Json.lookup j "RestoreWindow") RestoreWindow.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; db_cluster_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbClusterResourceId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; cluster_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterCreateTime") DateTime.of_json
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; d_b_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterArn") String.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; engine_mode = Aws.Util.option_map (Aws.Json.lookup j "EngineMode") String.of_json
  ; availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; aws_backup_recovery_point_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AwsBackupRecoveryPointArn") String.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  }
