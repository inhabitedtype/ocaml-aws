open Aws.BaseTypes

type t =
  { availability_zones : AvailabilityZones.t
  ; backup_retention_period : Integer.t option
  ; character_set_name : String.t option
  ; database_name : String.t option
  ; d_b_cluster_identifier : String.t
  ; d_b_cluster_parameter_group_name : String.t option
  ; vpc_security_group_ids : VpcSecurityGroupIdList.t
  ; d_b_subnet_group_name : String.t option
  ; engine : String.t
  ; engine_version : String.t option
  ; port : Integer.t option
  ; master_username : String.t option
  ; master_user_password : String.t option
  ; option_group_name : String.t option
  ; preferred_backup_window : String.t option
  ; preferred_maintenance_window : String.t option
  ; replication_source_identifier : String.t option
  ; tags : TagList.t
  ; storage_encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; pre_signed_url : String.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; backtrack_window : Long.t option
  ; enable_cloudwatch_logs_exports : LogTypeList.t
  ; engine_mode : String.t option
  ; scaling_configuration : ScalingConfiguration.t option
  ; rds_custom_cluster_configuration : RdsCustomClusterConfiguration.t option
  ; d_b_cluster_instance_class : String.t option
  ; allocated_storage : Integer.t option
  ; storage_type : String.t option
  ; iops : Integer.t option
  ; publicly_accessible : Boolean.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; deletion_protection : Boolean.t option
  ; global_cluster_identifier : String.t option
  ; enable_http_endpoint : Boolean.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; domain : String.t option
  ; domain_i_a_m_role_name : String.t option
  ; enable_global_write_forwarding : Boolean.t option
  ; network_type : String.t option
  ; serverless_v2_scaling_configuration : ServerlessV2ScalingConfiguration.t option
  ; monitoring_interval : Integer.t option
  ; monitoring_role_arn : String.t option
  ; database_insights_mode : DatabaseInsightsMode.t option
  ; enable_performance_insights : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; enable_limitless_database : Boolean.t option
  ; cluster_scalability_type : ClusterScalabilityType.t option
  ; d_b_system_id : String.t option
  ; manage_master_user_password : Boolean.t option
  ; enable_local_write_forwarding : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  ; c_a_certificate_identifier : String.t option
  ; engine_lifecycle_support : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; master_user_authentication_type : MasterUserAuthenticationType.t option
  ; with_express_configuration : Boolean.t option
  }

let make
    ?(availability_zones = [])
    ?backup_retention_period
    ?character_set_name
    ?database_name
    ~d_b_cluster_identifier
    ?d_b_cluster_parameter_group_name
    ?(vpc_security_group_ids = [])
    ?d_b_subnet_group_name
    ~engine
    ?engine_version
    ?port
    ?master_username
    ?master_user_password
    ?option_group_name
    ?preferred_backup_window
    ?preferred_maintenance_window
    ?replication_source_identifier
    ?(tags = [])
    ?storage_encrypted
    ?kms_key_id
    ?pre_signed_url
    ?enable_i_a_m_database_authentication
    ?backtrack_window
    ?(enable_cloudwatch_logs_exports = [])
    ?engine_mode
    ?scaling_configuration
    ?rds_custom_cluster_configuration
    ?d_b_cluster_instance_class
    ?allocated_storage
    ?storage_type
    ?iops
    ?publicly_accessible
    ?auto_minor_version_upgrade
    ?deletion_protection
    ?global_cluster_identifier
    ?enable_http_endpoint
    ?copy_tags_to_snapshot
    ?domain
    ?domain_i_a_m_role_name
    ?enable_global_write_forwarding
    ?network_type
    ?serverless_v2_scaling_configuration
    ?monitoring_interval
    ?monitoring_role_arn
    ?database_insights_mode
    ?enable_performance_insights
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?enable_limitless_database
    ?cluster_scalability_type
    ?d_b_system_id
    ?manage_master_user_password
    ?enable_local_write_forwarding
    ?master_user_secret_kms_key_id
    ?c_a_certificate_identifier
    ?engine_lifecycle_support
    ?(tag_specifications = [])
    ?master_user_authentication_type
    ?with_express_configuration
    () =
  { availability_zones
  ; backup_retention_period
  ; character_set_name
  ; database_name
  ; d_b_cluster_identifier
  ; d_b_cluster_parameter_group_name
  ; vpc_security_group_ids
  ; d_b_subnet_group_name
  ; engine
  ; engine_version
  ; port
  ; master_username
  ; master_user_password
  ; option_group_name
  ; preferred_backup_window
  ; preferred_maintenance_window
  ; replication_source_identifier
  ; tags
  ; storage_encrypted
  ; kms_key_id
  ; pre_signed_url
  ; enable_i_a_m_database_authentication
  ; backtrack_window
  ; enable_cloudwatch_logs_exports
  ; engine_mode
  ; scaling_configuration
  ; rds_custom_cluster_configuration
  ; d_b_cluster_instance_class
  ; allocated_storage
  ; storage_type
  ; iops
  ; publicly_accessible
  ; auto_minor_version_upgrade
  ; deletion_protection
  ; global_cluster_identifier
  ; enable_http_endpoint
  ; copy_tags_to_snapshot
  ; domain
  ; domain_i_a_m_role_name
  ; enable_global_write_forwarding
  ; network_type
  ; serverless_v2_scaling_configuration
  ; monitoring_interval
  ; monitoring_role_arn
  ; database_insights_mode
  ; enable_performance_insights
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; enable_limitless_database
  ; cluster_scalability_type
  ; d_b_system_id
  ; manage_master_user_password
  ; enable_local_write_forwarding
  ; master_user_secret_kms_key_id
  ; c_a_certificate_identifier
  ; engine_lifecycle_support
  ; tag_specifications
  ; master_user_authentication_type
  ; with_express_configuration
  }

let parse xml =
  Some
    { availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; character_set_name =
        Aws.Util.option_bind (Aws.Xml.member "CharacterSetName" xml) String.parse
    ; database_name =
        Aws.Util.option_bind (Aws.Xml.member "DatabaseName" xml) String.parse
    ; d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; d_b_cluster_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupName" xml)
          String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             VpcSecurityGroupIdList.parse)
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; replication_source_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationSourceIdentifier" xml)
          String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; pre_signed_url =
        Aws.Util.option_bind (Aws.Xml.member "PreSignedUrl" xml) String.parse
    ; enable_i_a_m_database_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableIAMDatabaseAuthentication" xml)
          Boolean.parse
    ; backtrack_window =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackWindow" xml) Long.parse
    ; enable_cloudwatch_logs_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnableCloudwatchLogsExports" xml)
             LogTypeList.parse)
    ; engine_mode = Aws.Util.option_bind (Aws.Xml.member "EngineMode" xml) String.parse
    ; scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ScalingConfiguration" xml)
          ScalingConfiguration.parse
    ; rds_custom_cluster_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsCustomClusterConfiguration" xml)
          RdsCustomClusterConfiguration.parse
    ; d_b_cluster_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterInstanceClass" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; global_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterIdentifier" xml) String.parse
    ; enable_http_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "EnableHttpEndpoint" xml) Boolean.parse
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; enable_global_write_forwarding =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableGlobalWriteForwarding" xml)
          Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; serverless_v2_scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2ScalingConfiguration" xml)
          ServerlessV2ScalingConfiguration.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
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
    ; enable_limitless_database =
        Aws.Util.option_bind (Aws.Xml.member "EnableLimitlessDatabase" xml) Boolean.parse
    ; cluster_scalability_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ClusterScalabilityType" xml)
          ClusterScalabilityType.parse
    ; d_b_system_id = Aws.Util.option_bind (Aws.Xml.member "DBSystemId" xml) String.parse
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; enable_local_write_forwarding =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableLocalWriteForwarding" xml)
          Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
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
    ; with_express_configuration =
        Aws.Util.option_bind (Aws.Xml.member "WithExpressConfiguration" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.with_express_configuration (fun f ->
             Aws.Query.Pair ("WithExpressConfiguration", Boolean.to_query f))
       ; Aws.Util.option_map v.master_user_authentication_type (fun f ->
             Aws.Query.Pair
               ("MasterUserAuthenticationType", MasterUserAuthenticationType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecifications.member"
              , TagSpecificationList.to_query v.tag_specifications ))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.enable_local_write_forwarding (fun f ->
             Aws.Query.Pair ("EnableLocalWriteForwarding", Boolean.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_system_id (fun f ->
             Aws.Query.Pair ("DBSystemId", String.to_query f))
       ; Aws.Util.option_map v.cluster_scalability_type (fun f ->
             Aws.Query.Pair ("ClusterScalabilityType", ClusterScalabilityType.to_query f))
       ; Aws.Util.option_map v.enable_limitless_database (fun f ->
             Aws.Query.Pair ("EnableLimitlessDatabase", Boolean.to_query f))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             Aws.Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             Aws.Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             Aws.Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             Aws.Query.Pair ("DatabaseInsightsMode", DatabaseInsightsMode.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             Aws.Query.Pair
               ( "ServerlessV2ScalingConfiguration"
               , ServerlessV2ScalingConfiguration.to_query f ))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.enable_global_write_forwarding (fun f ->
             Aws.Query.Pair ("EnableGlobalWriteForwarding", Boolean.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_http_endpoint (fun f ->
             Aws.Query.Pair ("EnableHttpEndpoint", Boolean.to_query f))
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             Aws.Query.Pair ("GlobalClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             Aws.Query.Pair ("DBClusterInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             Aws.Query.Pair
               ("RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_query f))
       ; Aws.Util.option_map v.scaling_configuration (fun f ->
             Aws.Query.Pair ("ScalingConfiguration", ScalingConfiguration.to_query f))
       ; Aws.Util.option_map v.engine_mode (fun f ->
             Aws.Query.Pair ("EngineMode", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EnableCloudwatchLogsExports.member"
              , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             Aws.Query.Pair ("BacktrackWindow", Long.to_query f))
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             Aws.Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.pre_signed_url (fun f ->
             Aws.Query.Pair ("PreSignedUrl", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.replication_source_identifier (fun f ->
             Aws.Query.Pair ("ReplicationSourceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupIds.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ; Aws.Util.option_map v.database_name (fun f ->
             Aws.Query.Pair ("DatabaseName", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.with_express_configuration (fun f ->
             "WithExpressConfiguration", Boolean.to_json f)
       ; Aws.Util.option_map v.master_user_authentication_type (fun f ->
             "MasterUserAuthenticationType", MasterUserAuthenticationType.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.enable_local_write_forwarding (fun f ->
             "EnableLocalWriteForwarding", Boolean.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_system_id (fun f -> "DBSystemId", String.to_json f)
       ; Aws.Util.option_map v.cluster_scalability_type (fun f ->
             "ClusterScalabilityType", ClusterScalabilityType.to_json f)
       ; Aws.Util.option_map v.enable_limitless_database (fun f ->
             "EnableLimitlessDatabase", Boolean.to_json f)
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             "PerformanceInsightsRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             "PerformanceInsightsKMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             "EnablePerformanceInsights", Boolean.to_json f)
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             "DatabaseInsightsMode", DatabaseInsightsMode.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             ( "ServerlessV2ScalingConfiguration"
             , ServerlessV2ScalingConfiguration.to_json f ))
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.enable_global_write_forwarding (fun f ->
             "EnableGlobalWriteForwarding", Boolean.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_http_endpoint (fun f ->
             "EnableHttpEndpoint", Boolean.to_json f)
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             "GlobalClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             "DBClusterInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             "RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_json f)
       ; Aws.Util.option_map v.scaling_configuration (fun f ->
             "ScalingConfiguration", ScalingConfiguration.to_json f)
       ; Aws.Util.option_map v.engine_mode (fun f -> "EngineMode", String.to_json f)
       ; Some
           ( "EnableCloudwatchLogsExports"
           , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             "BacktrackWindow", Long.to_json f)
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             "EnableIAMDatabaseAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.pre_signed_url (fun f -> "PreSignedUrl", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.replication_source_identifier (fun f ->
             "ReplicationSourceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Some ("Engine", String.to_json v.engine)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Some
           ("VpcSecurityGroupIds", VpcSecurityGroupIdList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             "DBClusterParameterGroupName", String.to_json f)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ; Aws.Util.option_map v.database_name (fun f -> "DatabaseName", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ])

let of_json j =
  { availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; database_name = Aws.Util.option_map (Aws.Json.lookup j "DatabaseName") String.of_json
  ; d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; d_b_cluster_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroupName") String.of_json
  ; vpc_security_group_ids =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; replication_source_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationSourceIdentifier") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; pre_signed_url = Aws.Util.option_map (Aws.Json.lookup j "PreSignedUrl") String.of_json
  ; enable_i_a_m_database_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableIAMDatabaseAuthentication")
        Boolean.of_json
  ; backtrack_window =
      Aws.Util.option_map (Aws.Json.lookup j "BacktrackWindow") Long.of_json
  ; enable_cloudwatch_logs_exports =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableCloudwatchLogsExports"))
  ; engine_mode = Aws.Util.option_map (Aws.Json.lookup j "EngineMode") String.of_json
  ; scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ScalingConfiguration")
        ScalingConfiguration.of_json
  ; rds_custom_cluster_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsCustomClusterConfiguration")
        RdsCustomClusterConfiguration.of_json
  ; d_b_cluster_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterInstanceClass") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; global_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterIdentifier") String.of_json
  ; enable_http_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "EnableHttpEndpoint") Boolean.of_json
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; enable_global_write_forwarding =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableGlobalWriteForwarding")
        Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; serverless_v2_scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2ScalingConfiguration")
        ServerlessV2ScalingConfiguration.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
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
  ; enable_limitless_database =
      Aws.Util.option_map (Aws.Json.lookup j "EnableLimitlessDatabase") Boolean.of_json
  ; cluster_scalability_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClusterScalabilityType")
        ClusterScalabilityType.of_json
  ; d_b_system_id = Aws.Util.option_map (Aws.Json.lookup j "DBSystemId") String.of_json
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; enable_local_write_forwarding =
      Aws.Util.option_map (Aws.Json.lookup j "EnableLocalWriteForwarding") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; master_user_authentication_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "MasterUserAuthenticationType")
        MasterUserAuthenticationType.of_json
  ; with_express_configuration =
      Aws.Util.option_map (Aws.Json.lookup j "WithExpressConfiguration") Boolean.of_json
  }
