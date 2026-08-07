open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t option
  ; d_b_instance_class : String.t option
  ; engine : String.t option
  ; d_b_instance_status : String.t option
  ; master_username : String.t option
  ; d_b_name : String.t option
  ; endpoint : Endpoint.t option
  ; allocated_storage : Integer.t option
  ; instance_create_time : DateTime.t option
  ; preferred_backup_window : String.t option
  ; backup_retention_period : Integer.t option
  ; d_b_security_groups : DBSecurityGroupMembershipList.t
  ; vpc_security_groups : VpcSecurityGroupMembershipList.t
  ; d_b_parameter_groups : DBParameterGroupStatusList.t
  ; availability_zone : String.t option
  ; d_b_subnet_group : DBSubnetGroup.t option
  ; preferred_maintenance_window : String.t option
  ; upgrade_rollout_order : UpgradeRolloutOrder.t option
  ; pending_modified_values : PendingModifiedValues.t option
  ; latest_restorable_time : DateTime.t option
  ; multi_a_z : Boolean.t option
  ; engine_version : String.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; read_replica_source_d_b_instance_identifier : String.t option
  ; read_replica_d_b_instance_identifiers : ReadReplicaDBInstanceIdentifierList.t
  ; read_replica_d_b_cluster_identifiers : ReadReplicaDBClusterIdentifierList.t
  ; replica_mode : ReplicaMode.t option
  ; license_model : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_memberships : OptionGroupMembershipList.t
  ; character_set_name : String.t option
  ; nchar_character_set_name : String.t option
  ; secondary_availability_zone : String.t option
  ; publicly_accessible : Boolean.t option
  ; status_infos : DBInstanceStatusInfoList.t
  ; storage_type : String.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; tde_credential_arn : String.t option
  ; db_instance_port : Integer.t option
  ; d_b_cluster_identifier : String.t option
  ; storage_encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; dbi_resource_id : String.t option
  ; c_a_certificate_identifier : String.t option
  ; domain_memberships : DomainMembershipList.t
  ; copy_tags_to_snapshot : Boolean.t option
  ; monitoring_interval : Integer.t option
  ; enhanced_monitoring_resource_arn : String.t option
  ; monitoring_role_arn : String.t option
  ; promotion_tier : Integer.t option
  ; d_b_instance_arn : String.t option
  ; timezone : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; database_insights_mode : DatabaseInsightsMode.t option
  ; performance_insights_enabled : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; enabled_cloudwatch_logs_exports : LogTypeList.t
  ; processor_features : ProcessorFeatureList.t
  ; deletion_protection : Boolean.t option
  ; associated_roles : DBInstanceRoles.t
  ; listener_endpoint : Endpoint.t option
  ; max_allocated_storage : Integer.t option
  ; tag_list : TagList.t
  ; automation_mode : AutomationMode.t option
  ; resume_full_automation_mode_time : DateTime.t option
  ; customer_owned_ip_enabled : Boolean.t option
  ; network_type : String.t option
  ; activity_stream_status : ActivityStreamStatus.t option
  ; activity_stream_kms_key_id : String.t option
  ; activity_stream_kinesis_stream_name : String.t option
  ; activity_stream_mode : ActivityStreamMode.t option
  ; activity_stream_engine_native_audit_fields_included : Boolean.t option
  ; aws_backup_recovery_point_arn : String.t option
  ; d_b_instance_automated_backups_replications :
      DBInstanceAutomatedBackupsReplicationList.t
  ; backup_target : String.t option
  ; automatic_restart_time : DateTime.t option
  ; custom_iam_instance_profile : String.t option
  ; activity_stream_policy_status : ActivityStreamPolicyStatus.t option
  ; certificate_details : CertificateDetails.t option
  ; d_b_system_id : String.t option
  ; master_user_secret : MasterUserSecret.t option
  ; read_replica_source_d_b_cluster_identifier : String.t option
  ; percent_progress : String.t option
  ; multi_tenant : Boolean.t option
  ; dedicated_log_volume : Boolean.t option
  ; is_storage_config_upgrade_available : Boolean.t option
  ; engine_lifecycle_support : String.t option
  ; additional_storage_volumes : AdditionalStorageVolumesOutputList.t
  ; storage_volume_status : String.t option
  }

let make
    ?d_b_instance_identifier
    ?d_b_instance_class
    ?engine
    ?d_b_instance_status
    ?master_username
    ?d_b_name
    ?endpoint
    ?allocated_storage
    ?instance_create_time
    ?preferred_backup_window
    ?backup_retention_period
    ?(d_b_security_groups = [])
    ?(vpc_security_groups = [])
    ?(d_b_parameter_groups = [])
    ?availability_zone
    ?d_b_subnet_group
    ?preferred_maintenance_window
    ?upgrade_rollout_order
    ?pending_modified_values
    ?latest_restorable_time
    ?multi_a_z
    ?engine_version
    ?auto_minor_version_upgrade
    ?read_replica_source_d_b_instance_identifier
    ?(read_replica_d_b_instance_identifiers = [])
    ?(read_replica_d_b_cluster_identifiers = [])
    ?replica_mode
    ?license_model
    ?iops
    ?storage_throughput
    ?(option_group_memberships = [])
    ?character_set_name
    ?nchar_character_set_name
    ?secondary_availability_zone
    ?publicly_accessible
    ?(status_infos = [])
    ?storage_type
    ?storage_encryption_type
    ?tde_credential_arn
    ?db_instance_port
    ?d_b_cluster_identifier
    ?storage_encrypted
    ?kms_key_id
    ?dbi_resource_id
    ?c_a_certificate_identifier
    ?(domain_memberships = [])
    ?copy_tags_to_snapshot
    ?monitoring_interval
    ?enhanced_monitoring_resource_arn
    ?monitoring_role_arn
    ?promotion_tier
    ?d_b_instance_arn
    ?timezone
    ?i_a_m_database_authentication_enabled
    ?database_insights_mode
    ?performance_insights_enabled
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?(enabled_cloudwatch_logs_exports = [])
    ?(processor_features = [])
    ?deletion_protection
    ?(associated_roles = [])
    ?listener_endpoint
    ?max_allocated_storage
    ?(tag_list = [])
    ?automation_mode
    ?resume_full_automation_mode_time
    ?customer_owned_ip_enabled
    ?network_type
    ?activity_stream_status
    ?activity_stream_kms_key_id
    ?activity_stream_kinesis_stream_name
    ?activity_stream_mode
    ?activity_stream_engine_native_audit_fields_included
    ?aws_backup_recovery_point_arn
    ?(d_b_instance_automated_backups_replications = [])
    ?backup_target
    ?automatic_restart_time
    ?custom_iam_instance_profile
    ?activity_stream_policy_status
    ?certificate_details
    ?d_b_system_id
    ?master_user_secret
    ?read_replica_source_d_b_cluster_identifier
    ?percent_progress
    ?multi_tenant
    ?dedicated_log_volume
    ?is_storage_config_upgrade_available
    ?engine_lifecycle_support
    ?(additional_storage_volumes = [])
    ?storage_volume_status
    () =
  { d_b_instance_identifier
  ; d_b_instance_class
  ; engine
  ; d_b_instance_status
  ; master_username
  ; d_b_name
  ; endpoint
  ; allocated_storage
  ; instance_create_time
  ; preferred_backup_window
  ; backup_retention_period
  ; d_b_security_groups
  ; vpc_security_groups
  ; d_b_parameter_groups
  ; availability_zone
  ; d_b_subnet_group
  ; preferred_maintenance_window
  ; upgrade_rollout_order
  ; pending_modified_values
  ; latest_restorable_time
  ; multi_a_z
  ; engine_version
  ; auto_minor_version_upgrade
  ; read_replica_source_d_b_instance_identifier
  ; read_replica_d_b_instance_identifiers
  ; read_replica_d_b_cluster_identifiers
  ; replica_mode
  ; license_model
  ; iops
  ; storage_throughput
  ; option_group_memberships
  ; character_set_name
  ; nchar_character_set_name
  ; secondary_availability_zone
  ; publicly_accessible
  ; status_infos
  ; storage_type
  ; storage_encryption_type
  ; tde_credential_arn
  ; db_instance_port
  ; d_b_cluster_identifier
  ; storage_encrypted
  ; kms_key_id
  ; dbi_resource_id
  ; c_a_certificate_identifier
  ; domain_memberships
  ; copy_tags_to_snapshot
  ; monitoring_interval
  ; enhanced_monitoring_resource_arn
  ; monitoring_role_arn
  ; promotion_tier
  ; d_b_instance_arn
  ; timezone
  ; i_a_m_database_authentication_enabled
  ; database_insights_mode
  ; performance_insights_enabled
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; enabled_cloudwatch_logs_exports
  ; processor_features
  ; deletion_protection
  ; associated_roles
  ; listener_endpoint
  ; max_allocated_storage
  ; tag_list
  ; automation_mode
  ; resume_full_automation_mode_time
  ; customer_owned_ip_enabled
  ; network_type
  ; activity_stream_status
  ; activity_stream_kms_key_id
  ; activity_stream_kinesis_stream_name
  ; activity_stream_mode
  ; activity_stream_engine_native_audit_fields_included
  ; aws_backup_recovery_point_arn
  ; d_b_instance_automated_backups_replications
  ; backup_target
  ; automatic_restart_time
  ; custom_iam_instance_profile
  ; activity_stream_policy_status
  ; certificate_details
  ; d_b_system_id
  ; master_user_secret
  ; read_replica_source_d_b_cluster_identifier
  ; percent_progress
  ; multi_tenant
  ; dedicated_log_volume
  ; is_storage_config_upgrade_available
  ; engine_lifecycle_support
  ; additional_storage_volumes
  ; storage_volume_status
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; d_b_instance_status =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceStatus" xml) String.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; d_b_name = Aws.Util.option_bind (Aws.Xml.member "DBName" xml) String.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) Endpoint.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; instance_create_time =
        Aws.Util.option_bind (Aws.Xml.member "InstanceCreateTime" xml) DateTime.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; d_b_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSecurityGroups" xml)
             DBSecurityGroupMembershipList.parse)
    ; vpc_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroups" xml)
             VpcSecurityGroupMembershipList.parse)
    ; d_b_parameter_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBParameterGroups" xml)
             DBParameterGroupStatusList.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; d_b_subnet_group =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroup" xml) DBSubnetGroup.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; upgrade_rollout_order =
        Aws.Util.option_bind
          (Aws.Xml.member "UpgradeRolloutOrder" xml)
          UpgradeRolloutOrder.parse
    ; pending_modified_values =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingModifiedValues" xml)
          PendingModifiedValues.parse
    ; latest_restorable_time =
        Aws.Util.option_bind (Aws.Xml.member "LatestRestorableTime" xml) DateTime.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; read_replica_source_d_b_instance_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ReadReplicaSourceDBInstanceIdentifier" xml)
          String.parse
    ; read_replica_d_b_instance_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReadReplicaDBInstanceIdentifiers" xml)
             ReadReplicaDBInstanceIdentifierList.parse)
    ; read_replica_d_b_cluster_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReadReplicaDBClusterIdentifiers" xml)
             ReadReplicaDBClusterIdentifierList.parse)
    ; replica_mode =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaMode" xml) ReplicaMode.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "OptionGroupMemberships" xml)
             OptionGroupMembershipList.parse)
    ; character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; nchar_character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "NcharCharacterSetName" xml) String.parse
    ; secondary_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "SecondaryAvailabilityZone" xml) String.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; status_infos =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StatusInfos" xml)
             DBInstanceStatusInfoList.parse)
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; tde_credential_arn =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialArn" xml) String.parse
    ; db_instance_port =
        Aws.Util.option_bind (Aws.Xml.member "DbInstancePort" xml) Integer.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; dbi_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbiResourceId" xml) String.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; domain_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DomainMemberships" xml)
             DomainMembershipList.parse)
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; enhanced_monitoring_resource_arn =
        Aws.Util.option_bind
          (Aws.Xml.member "EnhancedMonitoringResourceArn" xml)
          String.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
    ; promotion_tier =
        Aws.Util.option_bind (Aws.Xml.member "PromotionTier" xml) Integer.parse
    ; d_b_instance_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceArn" xml) String.parse
    ; timezone = Aws.Util.option_bind (Aws.Xml.member "Timezone" xml) String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; database_insights_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInsightsMode" xml)
          DatabaseInsightsMode.parse
    ; performance_insights_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsEnabled" xml)
          Boolean.parse
    ; performance_insights_k_m_s_key_id =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsKMSKeyId" xml)
          String.parse
    ; performance_insights_retention_period =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsRetentionPeriod" xml)
          Integer.parse
    ; enabled_cloudwatch_logs_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnabledCloudwatchLogsExports" xml)
             LogTypeList.parse)
    ; processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProcessorFeatures" xml)
             ProcessorFeatureList.parse)
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; associated_roles =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AssociatedRoles" xml)
             DBInstanceRoles.parse)
    ; listener_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ListenerEndpoint" xml) Endpoint.parse
    ; max_allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "MaxAllocatedStorage" xml) Integer.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; automation_mode =
        Aws.Util.option_bind (Aws.Xml.member "AutomationMode" xml) AutomationMode.parse
    ; resume_full_automation_mode_time =
        Aws.Util.option_bind
          (Aws.Xml.member "ResumeFullAutomationModeTime" xml)
          DateTime.parse
    ; customer_owned_ip_enabled =
        Aws.Util.option_bind (Aws.Xml.member "CustomerOwnedIpEnabled" xml) Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; activity_stream_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamStatus" xml)
          ActivityStreamStatus.parse
    ; activity_stream_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "ActivityStreamKmsKeyId" xml) String.parse
    ; activity_stream_kinesis_stream_name =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamKinesisStreamName" xml)
          String.parse
    ; activity_stream_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamMode" xml)
          ActivityStreamMode.parse
    ; activity_stream_engine_native_audit_fields_included =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamEngineNativeAuditFieldsIncluded" xml)
          Boolean.parse
    ; aws_backup_recovery_point_arn =
        Aws.Util.option_bind (Aws.Xml.member "AwsBackupRecoveryPointArn" xml) String.parse
    ; d_b_instance_automated_backups_replications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBInstanceAutomatedBackupsReplications" xml)
             DBInstanceAutomatedBackupsReplicationList.parse)
    ; backup_target =
        Aws.Util.option_bind (Aws.Xml.member "BackupTarget" xml) String.parse
    ; automatic_restart_time =
        Aws.Util.option_bind (Aws.Xml.member "AutomaticRestartTime" xml) DateTime.parse
    ; custom_iam_instance_profile =
        Aws.Util.option_bind (Aws.Xml.member "CustomIamInstanceProfile" xml) String.parse
    ; activity_stream_policy_status =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamPolicyStatus" xml)
          ActivityStreamPolicyStatus.parse
    ; certificate_details =
        Aws.Util.option_bind
          (Aws.Xml.member "CertificateDetails" xml)
          CertificateDetails.parse
    ; d_b_system_id = Aws.Util.option_bind (Aws.Xml.member "DBSystemId" xml) String.parse
    ; master_user_secret =
        Aws.Util.option_bind
          (Aws.Xml.member "MasterUserSecret" xml)
          MasterUserSecret.parse
    ; read_replica_source_d_b_cluster_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ReadReplicaSourceDBClusterIdentifier" xml)
          String.parse
    ; percent_progress =
        Aws.Util.option_bind (Aws.Xml.member "PercentProgress" xml) String.parse
    ; multi_tenant = Aws.Util.option_bind (Aws.Xml.member "MultiTenant" xml) Boolean.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; is_storage_config_upgrade_available =
        Aws.Util.option_bind
          (Aws.Xml.member "IsStorageConfigUpgradeAvailable" xml)
          Boolean.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
    ; additional_storage_volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStorageVolumes" xml)
             AdditionalStorageVolumesOutputList.parse)
    ; storage_volume_status =
        Aws.Util.option_bind (Aws.Xml.member "StorageVolumeStatus" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_volume_status (fun f ->
             Aws.Query.Pair ("StorageVolumeStatus", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , AdditionalStorageVolumesOutputList.to_query v.additional_storage_volumes
              ))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.is_storage_config_upgrade_available (fun f ->
             Aws.Query.Pair ("IsStorageConfigUpgradeAvailable", Boolean.to_query f))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_tenant (fun f ->
             Aws.Query.Pair ("MultiTenant", Boolean.to_query f))
       ; Aws.Util.option_map v.percent_progress (fun f ->
             Aws.Query.Pair ("PercentProgress", String.to_query f))
       ; Aws.Util.option_map v.read_replica_source_d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("ReadReplicaSourceDBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             Aws.Query.Pair ("MasterUserSecret", MasterUserSecret.to_query f))
       ; Aws.Util.option_map v.d_b_system_id (fun f ->
             Aws.Query.Pair ("DBSystemId", String.to_query f))
       ; Aws.Util.option_map v.certificate_details (fun f ->
             Aws.Query.Pair ("CertificateDetails", CertificateDetails.to_query f))
       ; Aws.Util.option_map v.activity_stream_policy_status (fun f ->
             Aws.Query.Pair
               ("ActivityStreamPolicyStatus", ActivityStreamPolicyStatus.to_query f))
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             Aws.Query.Pair ("CustomIamInstanceProfile", String.to_query f))
       ; Aws.Util.option_map v.automatic_restart_time (fun f ->
             Aws.Query.Pair ("AutomaticRestartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.backup_target (fun f ->
             Aws.Query.Pair ("BackupTarget", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBInstanceAutomatedBackupsReplications.member"
              , DBInstanceAutomatedBackupsReplicationList.to_query
                  v.d_b_instance_automated_backups_replications ))
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             Aws.Query.Pair ("AwsBackupRecoveryPointArn", String.to_query f))
       ; Aws.Util.option_map
           v.activity_stream_engine_native_audit_fields_included
           (fun f ->
             Aws.Query.Pair
               ("ActivityStreamEngineNativeAuditFieldsIncluded", Boolean.to_query f))
       ; Aws.Util.option_map v.activity_stream_mode (fun f ->
             Aws.Query.Pair ("ActivityStreamMode", ActivityStreamMode.to_query f))
       ; Aws.Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
             Aws.Query.Pair ("ActivityStreamKinesisStreamName", String.to_query f))
       ; Aws.Util.option_map v.activity_stream_kms_key_id (fun f ->
             Aws.Query.Pair ("ActivityStreamKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.activity_stream_status (fun f ->
             Aws.Query.Pair ("ActivityStreamStatus", ActivityStreamStatus.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.customer_owned_ip_enabled (fun f ->
             Aws.Query.Pair ("CustomerOwnedIpEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.resume_full_automation_mode_time (fun f ->
             Aws.Query.Pair ("ResumeFullAutomationModeTime", DateTime.to_query f))
       ; Aws.Util.option_map v.automation_mode (fun f ->
             Aws.Query.Pair ("AutomationMode", AutomationMode.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             Aws.Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.listener_endpoint (fun f ->
             Aws.Query.Pair ("ListenerEndpoint", Endpoint.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AssociatedRoles.member", DBInstanceRoles.to_query v.associated_roles))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ProcessorFeatures.member"
              , ProcessorFeatureList.to_query v.processor_features ))
       ; Some
           (Aws.Query.Pair
              ( "EnabledCloudwatchLogsExports.member"
              , LogTypeList.to_query v.enabled_cloudwatch_logs_exports ))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             Aws.Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             Aws.Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.performance_insights_enabled (fun f ->
             Aws.Query.Pair ("PerformanceInsightsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             Aws.Query.Pair ("DatabaseInsightsMode", DatabaseInsightsMode.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.timezone (fun f ->
             Aws.Query.Pair ("Timezone", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_arn (fun f ->
             Aws.Query.Pair ("DBInstanceArn", String.to_query f))
       ; Aws.Util.option_map v.promotion_tier (fun f ->
             Aws.Query.Pair ("PromotionTier", Integer.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.enhanced_monitoring_resource_arn (fun f ->
             Aws.Query.Pair ("EnhancedMonitoringResourceArn", String.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DomainMemberships.member"
              , DomainMembershipList.to_query v.domain_memberships ))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             Aws.Query.Pair ("DbiResourceId", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.db_instance_port (fun f ->
             Aws.Query.Pair ("DbInstancePort", Integer.to_query f))
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             Aws.Query.Pair ("TdeCredentialArn", String.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("StatusInfos.member", DBInstanceStatusInfoList.to_query v.status_infos))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.secondary_availability_zone (fun f ->
             Aws.Query.Pair ("SecondaryAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             Aws.Query.Pair ("NcharCharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "OptionGroupMemberships.member"
              , OptionGroupMembershipList.to_query v.option_group_memberships ))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.replica_mode (fun f ->
             Aws.Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReadReplicaDBClusterIdentifiers.member"
              , ReadReplicaDBClusterIdentifierList.to_query
                  v.read_replica_d_b_cluster_identifiers ))
       ; Some
           (Aws.Query.Pair
              ( "ReadReplicaDBInstanceIdentifiers.member"
              , ReadReplicaDBInstanceIdentifierList.to_query
                  v.read_replica_d_b_instance_identifiers ))
       ; Aws.Util.option_map v.read_replica_source_d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("ReadReplicaSourceDBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.latest_restorable_time (fun f ->
             Aws.Query.Pair ("LatestRestorableTime", DateTime.to_query f))
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             Aws.Query.Pair ("PendingModifiedValues", PendingModifiedValues.to_query f))
       ; Aws.Util.option_map v.upgrade_rollout_order (fun f ->
             Aws.Query.Pair ("UpgradeRolloutOrder", UpgradeRolloutOrder.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group (fun f ->
             Aws.Query.Pair ("DBSubnetGroup", DBSubnetGroup.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBParameterGroups.member"
              , DBParameterGroupStatusList.to_query v.d_b_parameter_groups ))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroups.member"
              , VpcSecurityGroupMembershipList.to_query v.vpc_security_groups ))
       ; Some
           (Aws.Query.Pair
              ( "DBSecurityGroups.member"
              , DBSecurityGroupMembershipList.to_query v.d_b_security_groups ))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.instance_create_time (fun f ->
             Aws.Query.Pair ("InstanceCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.d_b_name (fun f ->
             Aws.Query.Pair ("DBName", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_status (fun f ->
             Aws.Query.Pair ("DBInstanceStatus", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.storage_volume_status (fun f ->
             "StorageVolumeStatus", String.to_json f)
       ; Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesOutputList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.is_storage_config_upgrade_available (fun f ->
             "IsStorageConfigUpgradeAvailable", Boolean.to_json f)
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_tenant (fun f -> "MultiTenant", Boolean.to_json f)
       ; Aws.Util.option_map v.percent_progress (fun f ->
             "PercentProgress", String.to_json f)
       ; Aws.Util.option_map v.read_replica_source_d_b_cluster_identifier (fun f ->
             "ReadReplicaSourceDBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             "MasterUserSecret", MasterUserSecret.to_json f)
       ; Aws.Util.option_map v.d_b_system_id (fun f -> "DBSystemId", String.to_json f)
       ; Aws.Util.option_map v.certificate_details (fun f ->
             "CertificateDetails", CertificateDetails.to_json f)
       ; Aws.Util.option_map v.activity_stream_policy_status (fun f ->
             "ActivityStreamPolicyStatus", ActivityStreamPolicyStatus.to_json f)
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             "CustomIamInstanceProfile", String.to_json f)
       ; Aws.Util.option_map v.automatic_restart_time (fun f ->
             "AutomaticRestartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.backup_target (fun f -> "BackupTarget", String.to_json f)
       ; Some
           ( "DBInstanceAutomatedBackupsReplications"
           , DBInstanceAutomatedBackupsReplicationList.to_json
               v.d_b_instance_automated_backups_replications )
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             "AwsBackupRecoveryPointArn", String.to_json f)
       ; Aws.Util.option_map
           v.activity_stream_engine_native_audit_fields_included
           (fun f -> "ActivityStreamEngineNativeAuditFieldsIncluded", Boolean.to_json f)
       ; Aws.Util.option_map v.activity_stream_mode (fun f ->
             "ActivityStreamMode", ActivityStreamMode.to_json f)
       ; Aws.Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
             "ActivityStreamKinesisStreamName", String.to_json f)
       ; Aws.Util.option_map v.activity_stream_kms_key_id (fun f ->
             "ActivityStreamKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.activity_stream_status (fun f ->
             "ActivityStreamStatus", ActivityStreamStatus.to_json f)
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.customer_owned_ip_enabled (fun f ->
             "CustomerOwnedIpEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.resume_full_automation_mode_time (fun f ->
             "ResumeFullAutomationModeTime", DateTime.to_json f)
       ; Aws.Util.option_map v.automation_mode (fun f ->
             "AutomationMode", AutomationMode.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             "MaxAllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.listener_endpoint (fun f ->
             "ListenerEndpoint", Endpoint.to_json f)
       ; Some ("AssociatedRoles", DBInstanceRoles.to_json v.associated_roles)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Some
           ( "EnabledCloudwatchLogsExports"
           , LogTypeList.to_json v.enabled_cloudwatch_logs_exports )
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             "PerformanceInsightsRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             "PerformanceInsightsKMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.performance_insights_enabled (fun f ->
             "PerformanceInsightsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             "DatabaseInsightsMode", DatabaseInsightsMode.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.timezone (fun f -> "Timezone", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_arn (fun f ->
             "DBInstanceArn", String.to_json f)
       ; Aws.Util.option_map v.promotion_tier (fun f ->
             "PromotionTier", Integer.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.enhanced_monitoring_resource_arn (fun f ->
             "EnhancedMonitoringResourceArn", String.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Some ("DomainMemberships", DomainMembershipList.to_json v.domain_memberships)
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.dbi_resource_id (fun f ->
             "DbiResourceId", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.db_instance_port (fun f ->
             "DbInstancePort", Integer.to_json f)
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             "TdeCredentialArn", String.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Some ("StatusInfos", DBInstanceStatusInfoList.to_json v.status_infos)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.secondary_availability_zone (fun f ->
             "SecondaryAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.nchar_character_set_name (fun f ->
             "NcharCharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Some
           ( "OptionGroupMemberships"
           , OptionGroupMembershipList.to_json v.option_group_memberships )
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.replica_mode (fun f ->
             "ReplicaMode", ReplicaMode.to_json f)
       ; Some
           ( "ReadReplicaDBClusterIdentifiers"
           , ReadReplicaDBClusterIdentifierList.to_json
               v.read_replica_d_b_cluster_identifiers )
       ; Some
           ( "ReadReplicaDBInstanceIdentifiers"
           , ReadReplicaDBInstanceIdentifierList.to_json
               v.read_replica_d_b_instance_identifiers )
       ; Aws.Util.option_map v.read_replica_source_d_b_instance_identifier (fun f ->
             "ReadReplicaSourceDBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.latest_restorable_time (fun f ->
             "LatestRestorableTime", DateTime.to_json f)
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             "PendingModifiedValues", PendingModifiedValues.to_json f)
       ; Aws.Util.option_map v.upgrade_rollout_order (fun f ->
             "UpgradeRolloutOrder", UpgradeRolloutOrder.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group (fun f ->
             "DBSubnetGroup", DBSubnetGroup.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Some
           ("DBParameterGroups", DBParameterGroupStatusList.to_json v.d_b_parameter_groups)
       ; Some
           ( "VpcSecurityGroups"
           , VpcSecurityGroupMembershipList.to_json v.vpc_security_groups )
       ; Some
           ( "DBSecurityGroups"
           , DBSecurityGroupMembershipList.to_json v.d_b_security_groups )
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.instance_create_time (fun f ->
             "InstanceCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.d_b_name (fun f -> "DBName", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_status (fun f ->
             "DBInstanceStatus", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; d_b_instance_status =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceStatus") String.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; d_b_name = Aws.Util.option_map (Aws.Json.lookup j "DBName") String.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") Endpoint.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; instance_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceCreateTime") DateTime.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; d_b_security_groups =
      DBSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroups"))
  ; vpc_security_groups =
      VpcSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroups"))
  ; d_b_parameter_groups =
      DBParameterGroupStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBParameterGroups"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; d_b_subnet_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroup") DBSubnetGroup.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; upgrade_rollout_order =
      Aws.Util.option_map
        (Aws.Json.lookup j "UpgradeRolloutOrder")
        UpgradeRolloutOrder.of_json
  ; pending_modified_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingModifiedValues")
        PendingModifiedValues.of_json
  ; latest_restorable_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestRestorableTime") DateTime.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; read_replica_source_d_b_instance_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReadReplicaSourceDBInstanceIdentifier")
        String.of_json
  ; read_replica_d_b_instance_identifiers =
      ReadReplicaDBInstanceIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReadReplicaDBInstanceIdentifiers"))
  ; read_replica_d_b_cluster_identifiers =
      ReadReplicaDBClusterIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReadReplicaDBClusterIdentifiers"))
  ; replica_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaMode") ReplicaMode.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_memberships =
      OptionGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "OptionGroupMemberships"))
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; nchar_character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "NcharCharacterSetName") String.of_json
  ; secondary_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "SecondaryAvailabilityZone") String.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; status_infos =
      DBInstanceStatusInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StatusInfos"))
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; tde_credential_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialArn") String.of_json
  ; db_instance_port =
      Aws.Util.option_map (Aws.Json.lookup j "DbInstancePort") Integer.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; dbi_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbiResourceId") String.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; domain_memberships =
      DomainMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainMemberships"))
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; enhanced_monitoring_resource_arn =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnhancedMonitoringResourceArn")
        String.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
  ; promotion_tier =
      Aws.Util.option_map (Aws.Json.lookup j "PromotionTier") Integer.of_json
  ; d_b_instance_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceArn") String.of_json
  ; timezone = Aws.Util.option_map (Aws.Json.lookup j "Timezone") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; database_insights_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInsightsMode")
        DatabaseInsightsMode.of_json
  ; performance_insights_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "PerformanceInsightsEnabled") Boolean.of_json
  ; performance_insights_k_m_s_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "PerformanceInsightsKMSKeyId") String.of_json
  ; performance_insights_retention_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "PerformanceInsightsRetentionPeriod")
        Integer.of_json
  ; enabled_cloudwatch_logs_exports =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnabledCloudwatchLogsExports"))
  ; processor_features =
      ProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessorFeatures"))
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; associated_roles =
      DBInstanceRoles.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociatedRoles"))
  ; listener_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ListenerEndpoint") Endpoint.of_json
  ; max_allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxAllocatedStorage") Integer.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; automation_mode =
      Aws.Util.option_map (Aws.Json.lookup j "AutomationMode") AutomationMode.of_json
  ; resume_full_automation_mode_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResumeFullAutomationModeTime")
        DateTime.of_json
  ; customer_owned_ip_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "CustomerOwnedIpEnabled") Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; activity_stream_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamStatus")
        ActivityStreamStatus.of_json
  ; activity_stream_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "ActivityStreamKmsKeyId") String.of_json
  ; activity_stream_kinesis_stream_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamKinesisStreamName")
        String.of_json
  ; activity_stream_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamMode")
        ActivityStreamMode.of_json
  ; activity_stream_engine_native_audit_fields_included =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamEngineNativeAuditFieldsIncluded")
        Boolean.of_json
  ; aws_backup_recovery_point_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AwsBackupRecoveryPointArn") String.of_json
  ; d_b_instance_automated_backups_replications =
      DBInstanceAutomatedBackupsReplicationList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "DBInstanceAutomatedBackupsReplications"))
  ; backup_target = Aws.Util.option_map (Aws.Json.lookup j "BackupTarget") String.of_json
  ; automatic_restart_time =
      Aws.Util.option_map (Aws.Json.lookup j "AutomaticRestartTime") DateTime.of_json
  ; custom_iam_instance_profile =
      Aws.Util.option_map (Aws.Json.lookup j "CustomIamInstanceProfile") String.of_json
  ; activity_stream_policy_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamPolicyStatus")
        ActivityStreamPolicyStatus.of_json
  ; certificate_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "CertificateDetails")
        CertificateDetails.of_json
  ; d_b_system_id = Aws.Util.option_map (Aws.Json.lookup j "DBSystemId") String.of_json
  ; master_user_secret =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecret") MasterUserSecret.of_json
  ; read_replica_source_d_b_cluster_identifier =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReadReplicaSourceDBClusterIdentifier")
        String.of_json
  ; percent_progress =
      Aws.Util.option_map (Aws.Json.lookup j "PercentProgress") String.of_json
  ; multi_tenant = Aws.Util.option_map (Aws.Json.lookup j "MultiTenant") Boolean.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; is_storage_config_upgrade_available =
      Aws.Util.option_map
        (Aws.Json.lookup j "IsStorageConfigUpgradeAvailable")
        Boolean.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesOutputList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  ; storage_volume_status =
      Aws.Util.option_map (Aws.Json.lookup j "StorageVolumeStatus") String.of_json
  }
