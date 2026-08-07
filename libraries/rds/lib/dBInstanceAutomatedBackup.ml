open Aws.BaseTypes

type t =
  { d_b_instance_arn : String.t option
  ; dbi_resource_id : String.t option
  ; region : String.t option
  ; d_b_instance_identifier : String.t option
  ; restore_window : RestoreWindow.t option
  ; allocated_storage : Integer.t option
  ; status : String.t option
  ; port : Integer.t option
  ; availability_zone : String.t option
  ; vpc_id : String.t option
  ; instance_create_time : DateTime.t option
  ; master_username : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; license_model : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_name : String.t option
  ; tde_credential_arn : String.t option
  ; encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; storage_type : String.t option
  ; kms_key_id : String.t option
  ; timezone : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; d_b_instance_automated_backups_arn : String.t option
  ; d_b_instance_automated_backups_replications :
      DBInstanceAutomatedBackupsReplicationList.t
  ; backup_target : String.t option
  ; multi_tenant : Boolean.t option
  ; aws_backup_recovery_point_arn : String.t option
  ; tag_list : TagList.t
  ; dedicated_log_volume : Boolean.t option
  ; additional_storage_volumes : AdditionalStorageVolumesList.t
  }

let make
    ?d_b_instance_arn
    ?dbi_resource_id
    ?region
    ?d_b_instance_identifier
    ?restore_window
    ?allocated_storage
    ?status
    ?port
    ?availability_zone
    ?vpc_id
    ?instance_create_time
    ?master_username
    ?engine
    ?engine_version
    ?license_model
    ?iops
    ?storage_throughput
    ?option_group_name
    ?tde_credential_arn
    ?encrypted
    ?storage_encryption_type
    ?storage_type
    ?kms_key_id
    ?timezone
    ?i_a_m_database_authentication_enabled
    ?backup_retention_period
    ?preferred_backup_window
    ?d_b_instance_automated_backups_arn
    ?(d_b_instance_automated_backups_replications = [])
    ?backup_target
    ?multi_tenant
    ?aws_backup_recovery_point_arn
    ?(tag_list = [])
    ?dedicated_log_volume
    ?(additional_storage_volumes = [])
    () =
  { d_b_instance_arn
  ; dbi_resource_id
  ; region
  ; d_b_instance_identifier
  ; restore_window
  ; allocated_storage
  ; status
  ; port
  ; availability_zone
  ; vpc_id
  ; instance_create_time
  ; master_username
  ; engine
  ; engine_version
  ; license_model
  ; iops
  ; storage_throughput
  ; option_group_name
  ; tde_credential_arn
  ; encrypted
  ; storage_encryption_type
  ; storage_type
  ; kms_key_id
  ; timezone
  ; i_a_m_database_authentication_enabled
  ; backup_retention_period
  ; preferred_backup_window
  ; d_b_instance_automated_backups_arn
  ; d_b_instance_automated_backups_replications
  ; backup_target
  ; multi_tenant
  ; aws_backup_recovery_point_arn
  ; tag_list
  ; dedicated_log_volume
  ; additional_storage_volumes
  }

let parse xml =
  Some
    { d_b_instance_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceArn" xml) String.parse
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; region = Aws.Util.option_bind (Aws.Xml.member "Region" xml) String.parse
    ; d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; restore_window =
        Aws.Util.option_bind (Aws.Xml.member "RestoreWindow" xml) RestoreWindow.parse
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
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; tde_credential_arn =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialArn" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "Encrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; timezone = Aws.Util.option_bind (Aws.Xml.member "Timezone" xml) String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; d_b_instance_automated_backups_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "DBInstanceAutomatedBackupsArn" xml)
          String.parse
    ; d_b_instance_automated_backups_replications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBInstanceAutomatedBackupsReplications" xml)
             DBInstanceAutomatedBackupsReplicationList.parse)
    ; backup_target =
        Aws.Util.option_bind (Aws.Xml.member "BackupTarget" xml) String.parse
    ; multi_tenant = Aws.Util.option_bind (Aws.Xml.member "MultiTenant" xml) Boolean.parse
    ; aws_backup_recovery_point_arn =
        Aws.Util.option_bind (Aws.Xml.member "AwsBackupRecoveryPointArn" xml) String.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; additional_storage_volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStorageVolumes" xml)
             AdditionalStorageVolumesList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , AdditionalStorageVolumesList.to_query v.additional_storage_volumes ))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             Aws.Query.Pair ("AwsBackupRecoveryPointArn", String.to_query f))
       ; Aws.Util.option_map v.multi_tenant (fun f ->
             Aws.Query.Pair ("MultiTenant", Boolean.to_query f))
       ; Aws.Util.option_map v.backup_target (fun f ->
             Aws.Query.Pair ("BackupTarget", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBInstanceAutomatedBackupsReplications.member"
              , DBInstanceAutomatedBackupsReplicationList.to_query
                  v.d_b_instance_automated_backups_replications ))
       ; Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             Aws.Query.Pair ("DBInstanceAutomatedBackupsArn", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.timezone (fun f ->
             Aws.Query.Pair ("Timezone", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             Aws.Query.Pair ("TdeCredentialArn", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
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
       ; Aws.Util.option_map v.restore_window (fun f ->
             Aws.Query.Pair ("RestoreWindow", RestoreWindow.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.region (fun f ->
             Aws.Query.Pair ("Region", String.to_query f))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_arn (fun f ->
             Aws.Query.Pair ("DBInstanceArn", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             "AwsBackupRecoveryPointArn", String.to_json f)
       ; Aws.Util.option_map v.multi_tenant (fun f -> "MultiTenant", Boolean.to_json f)
       ; Aws.Util.option_map v.backup_target (fun f -> "BackupTarget", String.to_json f)
       ; Some
           ( "DBInstanceAutomatedBackupsReplications"
           , DBInstanceAutomatedBackupsReplicationList.to_json
               v.d_b_instance_automated_backups_replications )
       ; Aws.Util.option_map v.d_b_instance_automated_backups_arn (fun f ->
             "DBInstanceAutomatedBackupsArn", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.timezone (fun f -> "Timezone", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "Encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             "TdeCredentialArn", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
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
       ; Aws.Util.option_map v.restore_window (fun f ->
             "RestoreWindow", RestoreWindow.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.region (fun f -> "Region", String.to_json f)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_arn (fun f ->
             "DBInstanceArn", String.to_json f)
       ])

let of_json j =
  { d_b_instance_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceArn") String.of_json
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; region = Aws.Util.option_map (Aws.Json.lookup j "Region") String.of_json
  ; d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; restore_window =
      Aws.Util.option_map (Aws.Json.lookup j "RestoreWindow") RestoreWindow.of_json
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
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; tde_credential_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialArn") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "Encrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; timezone = Aws.Util.option_map (Aws.Json.lookup j "Timezone") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; d_b_instance_automated_backups_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBInstanceAutomatedBackupsArn")
        String.of_json
  ; d_b_instance_automated_backups_replications =
      DBInstanceAutomatedBackupsReplicationList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "DBInstanceAutomatedBackupsReplications"))
  ; backup_target = Aws.Util.option_map (Aws.Json.lookup j "BackupTarget") String.of_json
  ; multi_tenant = Aws.Util.option_map (Aws.Json.lookup j "MultiTenant") Boolean.of_json
  ; aws_backup_recovery_point_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AwsBackupRecoveryPointArn") String.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  }
