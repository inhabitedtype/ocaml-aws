open Aws.BaseTypes

type t =
  { d_b_snapshot_identifier : String.t option
  ; d_b_instance_identifier : String.t option
  ; snapshot_create_time : DateTime.t option
  ; engine : String.t option
  ; allocated_storage : Integer.t option
  ; status : String.t option
  ; port : Integer.t option
  ; availability_zone : String.t option
  ; vpc_id : String.t option
  ; instance_create_time : DateTime.t option
  ; master_username : String.t option
  ; engine_version : String.t option
  ; license_model : String.t option
  ; snapshot_type : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_name : String.t option
  ; percent_progress : Integer.t option
  ; source_region : String.t option
  ; source_d_b_snapshot_identifier : String.t option
  ; storage_type : String.t option
  ; tde_credential_arn : String.t option
  ; encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; kms_key_id : String.t option
  ; d_b_snapshot_arn : String.t option
  ; timezone : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; processor_features : ProcessorFeatureList.t
  ; dbi_resource_id : String.t option
  ; tag_list : TagList.t
  ; snapshot_target : String.t option
  ; original_snapshot_create_time : DateTime.t option
  ; snapshot_database_time : DateTime.t option
  ; d_b_system_id : String.t option
  ; multi_tenant : Boolean.t option
  ; dedicated_log_volume : Boolean.t option
  ; additional_storage_volumes : AdditionalStorageVolumesList.t
  ; snapshot_availability_zone : String.t option
  }

let make
    ?d_b_snapshot_identifier
    ?d_b_instance_identifier
    ?snapshot_create_time
    ?engine
    ?allocated_storage
    ?status
    ?port
    ?availability_zone
    ?vpc_id
    ?instance_create_time
    ?master_username
    ?engine_version
    ?license_model
    ?snapshot_type
    ?iops
    ?storage_throughput
    ?option_group_name
    ?percent_progress
    ?source_region
    ?source_d_b_snapshot_identifier
    ?storage_type
    ?tde_credential_arn
    ?encrypted
    ?storage_encryption_type
    ?backup_retention_period
    ?preferred_backup_window
    ?kms_key_id
    ?d_b_snapshot_arn
    ?timezone
    ?i_a_m_database_authentication_enabled
    ?(processor_features = [])
    ?dbi_resource_id
    ?(tag_list = [])
    ?snapshot_target
    ?original_snapshot_create_time
    ?snapshot_database_time
    ?d_b_system_id
    ?multi_tenant
    ?dedicated_log_volume
    ?(additional_storage_volumes = [])
    ?snapshot_availability_zone
    () =
  { d_b_snapshot_identifier
  ; d_b_instance_identifier
  ; snapshot_create_time
  ; engine
  ; allocated_storage
  ; status
  ; port
  ; availability_zone
  ; vpc_id
  ; instance_create_time
  ; master_username
  ; engine_version
  ; license_model
  ; snapshot_type
  ; iops
  ; storage_throughput
  ; option_group_name
  ; percent_progress
  ; source_region
  ; source_d_b_snapshot_identifier
  ; storage_type
  ; tde_credential_arn
  ; encrypted
  ; storage_encryption_type
  ; backup_retention_period
  ; preferred_backup_window
  ; kms_key_id
  ; d_b_snapshot_arn
  ; timezone
  ; i_a_m_database_authentication_enabled
  ; processor_features
  ; dbi_resource_id
  ; tag_list
  ; snapshot_target
  ; original_snapshot_create_time
  ; snapshot_database_time
  ; d_b_system_id
  ; multi_tenant
  ; dedicated_log_volume
  ; additional_storage_volumes
  ; snapshot_availability_zone
  }

let parse xml =
  Some
    { d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    ; d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; snapshot_create_time =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotCreateTime" xml) DateTime.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; instance_create_time =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCreateTime" xml) DateTime.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; percent_progress =
        Aws.Util.option_bind (Aws.Xml.member "PercentProgress" xml) Integer.parse
    ; source_region =
        Aws.Util.option_bind (Aws.Xml.member "SourceRegion" xml) String.parse
    ; source_d_b_snapshot_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceDBSnapshotIdentifier" xml)
          String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; tde_credential_arn =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialArn" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "Encrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; d_b_snapshot_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotArn" xml) String.parse
    ; timezone = Aws.Util.option_bind (Aws.Xml.member "Timezone" xml) String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProcessorFeatures" xml)
             ProcessorFeatureList.parse)
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; snapshot_target =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotTarget" xml) String.parse
    ; original_snapshot_create_time =
        Aws.Util.option_bind
          (Aws.Xml.member "OriginalSnapshotCreateTime" xml)
          DateTime.parse
    ; snapshot_database_time =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotDatabaseTime" xml) DateTime.parse
    ; d_b_system_id = Aws.Util.option_bind (Aws.Xml.member "DBSystemId" xml) String.parse
    ; multi_tenant = Aws.Util.option_bind (Aws.Xml.member "MultiTenant" xml) Boolean.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; additional_storage_volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStorageVolumes" xml)
             AdditionalStorageVolumesList.parse)
    ; snapshot_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotAvailabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_availability_zone (fun f ->
             Aws.Query.Pair ("SnapshotAvailabilityZone", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , AdditionalStorageVolumesList.to_query v.additional_storage_volumes ))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_tenant (fun f ->
             Aws.Query.Pair ("MultiTenant", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_system_id (fun f ->
             Aws.Query.Pair ("DBSystemId", String.to_query f))
       ; Aws.Util.option_map v.snapshot_database_time (fun f ->
             Aws.Query.Pair ("SnapshotDatabaseTime", DateTime.to_query f))
       ; Aws.Util.option_map v.original_snapshot_create_time (fun f ->
             Aws.Query.Pair ("OriginalSnapshotCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_target (fun f ->
             Aws.Query.Pair ("SnapshotTarget", String.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ProcessorFeatures.member"
              , ProcessorFeatureList.to_query v.processor_features ))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.timezone (fun f ->
             Aws.Query.Pair ("Timezone", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_arn (fun f ->
             Aws.Query.Pair ("DBSnapshotArn", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             Aws.Query.Pair ("TdeCredentialArn", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.source_d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("SourceDBSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.source_region (fun f ->
             Aws.Query.Pair ("SourceRegion", String.to_query f))
       ; Aws.Util.option_map v.percent_progress (fun f ->
             Aws.Query.Pair ("PercentProgress", Integer.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.instance_create_time (fun f ->
             Aws.Query.Pair ("InstanceCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.snapshot_create_time (fun f ->
             Aws.Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_availability_zone (fun f ->
             "SnapshotAvailabilityZone", String.to_json f)
       ; Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_tenant (fun f -> "MultiTenant", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_system_id (fun f -> "DBSystemId", String.to_json f)
       ; Aws.Util.option_map v.snapshot_database_time (fun f ->
             "SnapshotDatabaseTime", DateTime.to_json f)
       ; Aws.Util.option_map v.original_snapshot_create_time (fun f ->
             "OriginalSnapshotCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_target (fun f ->
             "SnapshotTarget", String.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.timezone (fun f -> "Timezone", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_arn (fun f ->
             "DBSnapshotArn", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "Encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             "TdeCredentialArn", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.source_d_b_snapshot_identifier (fun f ->
             "SourceDBSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.source_region (fun f -> "SourceRegion", String.to_json f)
       ; Aws.Util.option_map v.percent_progress (fun f ->
             "PercentProgress", Integer.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.instance_create_time (fun f ->
             "InstanceCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.snapshot_create_time (fun f ->
             "SnapshotCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  ; d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; snapshot_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotCreateTime") DateTime.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; instance_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCreateTime") DateTime.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; percent_progress =
      Aws.Util.option_map (Aws.Json.lookup j "PercentProgress") Integer.of_json
  ; source_region = Aws.Util.option_map (Aws.Json.lookup j "SourceRegion") String.of_json
  ; source_d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceDBSnapshotIdentifier") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; tde_credential_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialArn") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "Encrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; d_b_snapshot_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotArn") String.of_json
  ; timezone = Aws.Util.option_map (Aws.Json.lookup j "Timezone") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; processor_features =
      ProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessorFeatures"))
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; snapshot_target =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotTarget") String.of_json
  ; original_snapshot_create_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "OriginalSnapshotCreateTime")
        DateTime.of_json
  ; snapshot_database_time =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotDatabaseTime") DateTime.of_json
  ; d_b_system_id = Aws.Util.option_map (Aws.Json.lookup j "DBSystemId") String.of_json
  ; multi_tenant = Aws.Util.option_map (Aws.Json.lookup j "MultiTenant") Boolean.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  ; snapshot_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotAvailabilityZone") String.of_json
  }
