open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; allocated_storage : Integer.t option
  ; d_b_instance_class : String.t option
  ; d_b_subnet_group_name : String.t option
  ; d_b_security_groups : DBSecurityGroupNameList.t
  ; vpc_security_group_ids : VpcSecurityGroupIdList.t
  ; apply_immediately : Boolean.t option
  ; master_user_password : String.t option
  ; d_b_parameter_group_name : String.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; preferred_maintenance_window : String.t option
  ; multi_a_z : Boolean.t option
  ; engine_version : String.t option
  ; allow_major_version_upgrade : Boolean.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; license_model : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_name : String.t option
  ; new_d_b_instance_identifier : String.t option
  ; storage_type : String.t option
  ; tde_credential_arn : String.t option
  ; tde_credential_password : String.t option
  ; c_a_certificate_identifier : String.t option
  ; domain : String.t option
  ; domain_fqdn : String.t option
  ; domain_ou : String.t option
  ; domain_auth_secret_arn : String.t option
  ; domain_dns_ips : StringList.t
  ; disable_domain : Boolean.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; monitoring_interval : Integer.t option
  ; d_b_port_number : Integer.t option
  ; publicly_accessible : Boolean.t option
  ; monitoring_role_arn : String.t option
  ; domain_i_a_m_role_name : String.t option
  ; promotion_tier : Integer.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; database_insights_mode : DatabaseInsightsMode.t option
  ; enable_performance_insights : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; cloudwatch_logs_export_configuration : CloudwatchLogsExportConfiguration.t option
  ; processor_features : ProcessorFeatureList.t
  ; use_default_processor_features : Boolean.t option
  ; deletion_protection : Boolean.t option
  ; max_allocated_storage : Integer.t option
  ; certificate_rotation_restart : Boolean.t option
  ; replica_mode : ReplicaMode.t option
  ; automation_mode : AutomationMode.t option
  ; resume_full_automation_mode_minutes : Integer.t option
  ; enable_customer_owned_ip : Boolean.t option
  ; network_type : String.t option
  ; aws_backup_recovery_point_arn : String.t option
  ; manage_master_user_password : Boolean.t option
  ; rotate_master_user_password : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  ; multi_tenant : Boolean.t option
  ; dedicated_log_volume : Boolean.t option
  ; engine : String.t option
  ; additional_storage_volumes : ModifyAdditionalStorageVolumesList.t
  ; tag_specifications : TagSpecificationList.t
  ; master_user_authentication_type : MasterUserAuthenticationType.t option
  }

let make
    ~d_b_instance_identifier
    ?allocated_storage
    ?d_b_instance_class
    ?d_b_subnet_group_name
    ?(d_b_security_groups = [])
    ?(vpc_security_group_ids = [])
    ?apply_immediately
    ?master_user_password
    ?d_b_parameter_group_name
    ?backup_retention_period
    ?preferred_backup_window
    ?preferred_maintenance_window
    ?multi_a_z
    ?engine_version
    ?allow_major_version_upgrade
    ?auto_minor_version_upgrade
    ?license_model
    ?iops
    ?storage_throughput
    ?option_group_name
    ?new_d_b_instance_identifier
    ?storage_type
    ?tde_credential_arn
    ?tde_credential_password
    ?c_a_certificate_identifier
    ?domain
    ?domain_fqdn
    ?domain_ou
    ?domain_auth_secret_arn
    ?(domain_dns_ips = [])
    ?disable_domain
    ?copy_tags_to_snapshot
    ?monitoring_interval
    ?d_b_port_number
    ?publicly_accessible
    ?monitoring_role_arn
    ?domain_i_a_m_role_name
    ?promotion_tier
    ?enable_i_a_m_database_authentication
    ?database_insights_mode
    ?enable_performance_insights
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?cloudwatch_logs_export_configuration
    ?(processor_features = [])
    ?use_default_processor_features
    ?deletion_protection
    ?max_allocated_storage
    ?certificate_rotation_restart
    ?replica_mode
    ?automation_mode
    ?resume_full_automation_mode_minutes
    ?enable_customer_owned_ip
    ?network_type
    ?aws_backup_recovery_point_arn
    ?manage_master_user_password
    ?rotate_master_user_password
    ?master_user_secret_kms_key_id
    ?multi_tenant
    ?dedicated_log_volume
    ?engine
    ?(additional_storage_volumes = [])
    ?(tag_specifications = [])
    ?master_user_authentication_type
    () =
  { d_b_instance_identifier
  ; allocated_storage
  ; d_b_instance_class
  ; d_b_subnet_group_name
  ; d_b_security_groups
  ; vpc_security_group_ids
  ; apply_immediately
  ; master_user_password
  ; d_b_parameter_group_name
  ; backup_retention_period
  ; preferred_backup_window
  ; preferred_maintenance_window
  ; multi_a_z
  ; engine_version
  ; allow_major_version_upgrade
  ; auto_minor_version_upgrade
  ; license_model
  ; iops
  ; storage_throughput
  ; option_group_name
  ; new_d_b_instance_identifier
  ; storage_type
  ; tde_credential_arn
  ; tde_credential_password
  ; c_a_certificate_identifier
  ; domain
  ; domain_fqdn
  ; domain_ou
  ; domain_auth_secret_arn
  ; domain_dns_ips
  ; disable_domain
  ; copy_tags_to_snapshot
  ; monitoring_interval
  ; d_b_port_number
  ; publicly_accessible
  ; monitoring_role_arn
  ; domain_i_a_m_role_name
  ; promotion_tier
  ; enable_i_a_m_database_authentication
  ; database_insights_mode
  ; enable_performance_insights
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; cloudwatch_logs_export_configuration
  ; processor_features
  ; use_default_processor_features
  ; deletion_protection
  ; max_allocated_storage
  ; certificate_rotation_restart
  ; replica_mode
  ; automation_mode
  ; resume_full_automation_mode_minutes
  ; enable_customer_owned_ip
  ; network_type
  ; aws_backup_recovery_point_arn
  ; manage_master_user_password
  ; rotate_master_user_password
  ; master_user_secret_kms_key_id
  ; multi_tenant
  ; dedicated_log_volume
  ; engine
  ; additional_storage_volumes
  ; tag_specifications
  ; master_user_authentication_type
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; d_b_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSecurityGroups" xml)
             DBSecurityGroupNameList.parse)
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             VpcSecurityGroupIdList.parse)
    ; apply_immediately =
        Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; d_b_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupName" xml) String.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; allow_major_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AllowMajorVersionUpgrade" xml) Boolean.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; new_d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "NewDBInstanceIdentifier" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; tde_credential_arn =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialArn" xml) String.parse
    ; tde_credential_password =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialPassword" xml) String.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_fqdn = Aws.Util.option_bind (Aws.Xml.member "DomainFqdn" xml) String.parse
    ; domain_ou = Aws.Util.option_bind (Aws.Xml.member "DomainOu" xml) String.parse
    ; domain_auth_secret_arn =
        Aws.Util.option_bind (Aws.Xml.member "DomainAuthSecretArn" xml) String.parse
    ; domain_dns_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DomainDnsIps" xml) StringList.parse)
    ; disable_domain =
        Aws.Util.option_bind (Aws.Xml.member "DisableDomain" xml) Boolean.parse
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; d_b_port_number =
        Aws.Util.option_bind (Aws.Xml.member "DBPortNumber" xml) Integer.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; promotion_tier =
        Aws.Util.option_bind (Aws.Xml.member "PromotionTier" xml) Integer.parse
    ; enable_i_a_m_database_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableIAMDatabaseAuthentication" xml)
          Boolean.parse
    ; database_insights_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInsightsMode" xml)
          DatabaseInsightsMode.parse
    ; enable_performance_insights =
        Aws.Util.option_bind
          (Aws.Xml.member "EnablePerformanceInsights" xml)
          Boolean.parse
    ; performance_insights_k_m_s_key_id =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsKMSKeyId" xml)
          String.parse
    ; performance_insights_retention_period =
        Aws.Util.option_bind
          (Aws.Xml.member "PerformanceInsightsRetentionPeriod" xml)
          Integer.parse
    ; cloudwatch_logs_export_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "CloudwatchLogsExportConfiguration" xml)
          CloudwatchLogsExportConfiguration.parse
    ; processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProcessorFeatures" xml)
             ProcessorFeatureList.parse)
    ; use_default_processor_features =
        Aws.Util.option_bind
          (Aws.Xml.member "UseDefaultProcessorFeatures" xml)
          Boolean.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; max_allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "MaxAllocatedStorage" xml) Integer.parse
    ; certificate_rotation_restart =
        Aws.Util.option_bind
          (Aws.Xml.member "CertificateRotationRestart" xml)
          Boolean.parse
    ; replica_mode =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaMode" xml) ReplicaMode.parse
    ; automation_mode =
        Aws.Util.option_bind (Aws.Xml.member "AutomationMode" xml) AutomationMode.parse
    ; resume_full_automation_mode_minutes =
        Aws.Util.option_bind
          (Aws.Xml.member "ResumeFullAutomationModeMinutes" xml)
          Integer.parse
    ; enable_customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "EnableCustomerOwnedIp" xml) Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; aws_backup_recovery_point_arn =
        Aws.Util.option_bind (Aws.Xml.member "AwsBackupRecoveryPointArn" xml) String.parse
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; rotate_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "RotateMasterUserPassword" xml) Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    ; multi_tenant = Aws.Util.option_bind (Aws.Xml.member "MultiTenant" xml) Boolean.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; additional_storage_volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStorageVolumes" xml)
             ModifyAdditionalStorageVolumesList.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecifications" xml)
             TagSpecificationList.parse)
    ; master_user_authentication_type =
        Aws.Util.option_bind
          (Aws.Xml.member "MasterUserAuthenticationType" xml)
          MasterUserAuthenticationType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_authentication_type (fun f ->
             Aws.Query.Pair
               ("MasterUserAuthenticationType", MasterUserAuthenticationType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecifications.member"
              , TagSpecificationList.to_query v.tag_specifications ))
       ; Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , ModifyAdditionalStorageVolumesList.to_query v.additional_storage_volumes
              ))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_tenant (fun f ->
             Aws.Query.Pair ("MultiTenant", Boolean.to_query f))
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.rotate_master_user_password (fun f ->
             Aws.Query.Pair ("RotateMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             Aws.Query.Pair ("AwsBackupRecoveryPointArn", String.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             Aws.Query.Pair ("EnableCustomerOwnedIp", Boolean.to_query f))
       ; Aws.Util.option_map v.resume_full_automation_mode_minutes (fun f ->
             Aws.Query.Pair ("ResumeFullAutomationModeMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.automation_mode (fun f ->
             Aws.Query.Pair ("AutomationMode", AutomationMode.to_query f))
       ; Aws.Util.option_map v.replica_mode (fun f ->
             Aws.Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
       ; Aws.Util.option_map v.certificate_rotation_restart (fun f ->
             Aws.Query.Pair ("CertificateRotationRestart", Boolean.to_query f))
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             Aws.Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.use_default_processor_features (fun f ->
             Aws.Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ProcessorFeatures.member"
              , ProcessorFeatureList.to_query v.processor_features ))
       ; Aws.Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
             Aws.Query.Pair
               ( "CloudwatchLogsExportConfiguration"
               , CloudwatchLogsExportConfiguration.to_query f ))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             Aws.Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             Aws.Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             Aws.Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             Aws.Query.Pair ("DatabaseInsightsMode", DatabaseInsightsMode.to_query f))
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             Aws.Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.promotion_tier (fun f ->
             Aws.Query.Pair ("PromotionTier", Integer.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_port_number (fun f ->
             Aws.Query.Pair ("DBPortNumber", Integer.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.disable_domain (fun f ->
             Aws.Query.Pair ("DisableDomain", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair ("DomainDnsIps.member", StringList.to_query v.domain_dns_ips))
       ; Aws.Util.option_map v.domain_auth_secret_arn (fun f ->
             Aws.Query.Pair ("DomainAuthSecretArn", String.to_query f))
       ; Aws.Util.option_map v.domain_ou (fun f ->
             Aws.Query.Pair ("DomainOu", String.to_query f))
       ; Aws.Util.option_map v.domain_fqdn (fun f ->
             Aws.Query.Pair ("DomainFqdn", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.tde_credential_password (fun f ->
             Aws.Query.Pair ("TdeCredentialPassword", String.to_query f))
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             Aws.Query.Pair ("TdeCredentialArn", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.new_d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("NewDBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.allow_major_version_upgrade (fun f ->
             Aws.Query.Pair ("AllowMajorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupIds.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ( "DBSecurityGroups.member"
              , DBSecurityGroupNameList.to_query v.d_b_security_groups ))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_authentication_type (fun f ->
             "MasterUserAuthenticationType", MasterUserAuthenticationType.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "AdditionalStorageVolumes"
           , ModifyAdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_tenant (fun f -> "MultiTenant", Boolean.to_json f)
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.rotate_master_user_password (fun f ->
             "RotateMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             "AwsBackupRecoveryPointArn", String.to_json f)
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             "EnableCustomerOwnedIp", Boolean.to_json f)
       ; Aws.Util.option_map v.resume_full_automation_mode_minutes (fun f ->
             "ResumeFullAutomationModeMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.automation_mode (fun f ->
             "AutomationMode", AutomationMode.to_json f)
       ; Aws.Util.option_map v.replica_mode (fun f ->
             "ReplicaMode", ReplicaMode.to_json f)
       ; Aws.Util.option_map v.certificate_rotation_restart (fun f ->
             "CertificateRotationRestart", Boolean.to_json f)
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             "MaxAllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.use_default_processor_features (fun f ->
             "UseDefaultProcessorFeatures", Boolean.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Aws.Util.option_map v.cloudwatch_logs_export_configuration (fun f ->
             ( "CloudwatchLogsExportConfiguration"
             , CloudwatchLogsExportConfiguration.to_json f ))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             "PerformanceInsightsRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             "PerformanceInsightsKMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             "EnablePerformanceInsights", Boolean.to_json f)
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             "DatabaseInsightsMode", DatabaseInsightsMode.to_json f)
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             "EnableIAMDatabaseAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.promotion_tier (fun f ->
             "PromotionTier", Integer.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_port_number (fun f ->
             "DBPortNumber", Integer.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.disable_domain (fun f ->
             "DisableDomain", Boolean.to_json f)
       ; Some ("DomainDnsIps", StringList.to_json v.domain_dns_ips)
       ; Aws.Util.option_map v.domain_auth_secret_arn (fun f ->
             "DomainAuthSecretArn", String.to_json f)
       ; Aws.Util.option_map v.domain_ou (fun f -> "DomainOu", String.to_json f)
       ; Aws.Util.option_map v.domain_fqdn (fun f -> "DomainFqdn", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.tde_credential_password (fun f ->
             "TdeCredentialPassword", String.to_json f)
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             "TdeCredentialArn", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.new_d_b_instance_identifier (fun f ->
             "NewDBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.allow_major_version_upgrade (fun f ->
             "AllowMajorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             "DBParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             "ApplyImmediately", Boolean.to_json f)
       ; Some
           ("VpcSecurityGroupIds", VpcSecurityGroupIdList.to_json v.vpc_security_group_ids)
       ; Some ("DBSecurityGroups", DBSecurityGroupNameList.to_json v.d_b_security_groups)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; d_b_security_groups =
      DBSecurityGroupNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSecurityGroups"))
  ; vpc_security_group_ids =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; apply_immediately =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; d_b_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupName") String.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; allow_major_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AllowMajorVersionUpgrade") Boolean.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; new_d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "NewDBInstanceIdentifier") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; tde_credential_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialArn") String.of_json
  ; tde_credential_password =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialPassword") String.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_fqdn = Aws.Util.option_map (Aws.Json.lookup j "DomainFqdn") String.of_json
  ; domain_ou = Aws.Util.option_map (Aws.Json.lookup j "DomainOu") String.of_json
  ; domain_auth_secret_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DomainAuthSecretArn") String.of_json
  ; domain_dns_ips =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainDnsIps"))
  ; disable_domain =
      Aws.Util.option_map (Aws.Json.lookup j "DisableDomain") Boolean.of_json
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; d_b_port_number =
      Aws.Util.option_map (Aws.Json.lookup j "DBPortNumber") Integer.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; promotion_tier =
      Aws.Util.option_map (Aws.Json.lookup j "PromotionTier") Integer.of_json
  ; enable_i_a_m_database_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableIAMDatabaseAuthentication")
        Boolean.of_json
  ; database_insights_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInsightsMode")
        DatabaseInsightsMode.of_json
  ; enable_performance_insights =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePerformanceInsights") Boolean.of_json
  ; performance_insights_k_m_s_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "PerformanceInsightsKMSKeyId") String.of_json
  ; performance_insights_retention_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "PerformanceInsightsRetentionPeriod")
        Integer.of_json
  ; cloudwatch_logs_export_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "CloudwatchLogsExportConfiguration")
        CloudwatchLogsExportConfiguration.of_json
  ; processor_features =
      ProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessorFeatures"))
  ; use_default_processor_features =
      Aws.Util.option_map
        (Aws.Json.lookup j "UseDefaultProcessorFeatures")
        Boolean.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; max_allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxAllocatedStorage") Integer.of_json
  ; certificate_rotation_restart =
      Aws.Util.option_map (Aws.Json.lookup j "CertificateRotationRestart") Boolean.of_json
  ; replica_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaMode") ReplicaMode.of_json
  ; automation_mode =
      Aws.Util.option_map (Aws.Json.lookup j "AutomationMode") AutomationMode.of_json
  ; resume_full_automation_mode_minutes =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResumeFullAutomationModeMinutes")
        Integer.of_json
  ; enable_customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "EnableCustomerOwnedIp") Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; aws_backup_recovery_point_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AwsBackupRecoveryPointArn") String.of_json
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; rotate_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "RotateMasterUserPassword") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  ; multi_tenant = Aws.Util.option_map (Aws.Json.lookup j "MultiTenant") Boolean.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; additional_storage_volumes =
      ModifyAdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; master_user_authentication_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "MasterUserAuthenticationType")
        MasterUserAuthenticationType.of_json
  }
