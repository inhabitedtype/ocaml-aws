open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; source_d_b_instance_identifier : String.t option
  ; d_b_instance_class : String.t option
  ; availability_zone : String.t option
  ; port : Integer.t option
  ; multi_a_z : Boolean.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_name : String.t option
  ; d_b_parameter_group_name : String.t option
  ; publicly_accessible : Boolean.t option
  ; tags : TagList.t
  ; d_b_subnet_group_name : String.t option
  ; vpc_security_group_ids : VpcSecurityGroupIdList.t
  ; storage_type : String.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; monitoring_interval : Integer.t option
  ; monitoring_role_arn : String.t option
  ; kms_key_id : String.t option
  ; pre_signed_url : String.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; database_insights_mode : DatabaseInsightsMode.t option
  ; enable_performance_insights : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; enable_cloudwatch_logs_exports : LogTypeList.t
  ; processor_features : ProcessorFeatureList.t
  ; use_default_processor_features : Boolean.t option
  ; deletion_protection : Boolean.t option
  ; domain : String.t option
  ; domain_i_a_m_role_name : String.t option
  ; domain_fqdn : String.t option
  ; domain_ou : String.t option
  ; domain_auth_secret_arn : String.t option
  ; domain_dns_ips : StringList.t
  ; replica_mode : ReplicaMode.t option
  ; enable_customer_owned_ip : Boolean.t option
  ; network_type : String.t option
  ; max_allocated_storage : Integer.t option
  ; backup_target : String.t option
  ; custom_iam_instance_profile : String.t option
  ; allocated_storage : Integer.t option
  ; source_d_b_cluster_identifier : String.t option
  ; dedicated_log_volume : Boolean.t option
  ; upgrade_storage_config : Boolean.t option
  ; c_a_certificate_identifier : String.t option
  ; additional_storage_volumes : AdditionalStorageVolumesList.t
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~d_b_instance_identifier
    ?source_d_b_instance_identifier
    ?d_b_instance_class
    ?availability_zone
    ?port
    ?multi_a_z
    ?auto_minor_version_upgrade
    ?iops
    ?storage_throughput
    ?option_group_name
    ?d_b_parameter_group_name
    ?publicly_accessible
    ?(tags = [])
    ?d_b_subnet_group_name
    ?(vpc_security_group_ids = [])
    ?storage_type
    ?copy_tags_to_snapshot
    ?monitoring_interval
    ?monitoring_role_arn
    ?kms_key_id
    ?pre_signed_url
    ?enable_i_a_m_database_authentication
    ?database_insights_mode
    ?enable_performance_insights
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?(enable_cloudwatch_logs_exports = [])
    ?(processor_features = [])
    ?use_default_processor_features
    ?deletion_protection
    ?domain
    ?domain_i_a_m_role_name
    ?domain_fqdn
    ?domain_ou
    ?domain_auth_secret_arn
    ?(domain_dns_ips = [])
    ?replica_mode
    ?enable_customer_owned_ip
    ?network_type
    ?max_allocated_storage
    ?backup_target
    ?custom_iam_instance_profile
    ?allocated_storage
    ?source_d_b_cluster_identifier
    ?dedicated_log_volume
    ?upgrade_storage_config
    ?c_a_certificate_identifier
    ?(additional_storage_volumes = [])
    ?(tag_specifications = [])
    () =
  { d_b_instance_identifier
  ; source_d_b_instance_identifier
  ; d_b_instance_class
  ; availability_zone
  ; port
  ; multi_a_z
  ; auto_minor_version_upgrade
  ; iops
  ; storage_throughput
  ; option_group_name
  ; d_b_parameter_group_name
  ; publicly_accessible
  ; tags
  ; d_b_subnet_group_name
  ; vpc_security_group_ids
  ; storage_type
  ; copy_tags_to_snapshot
  ; monitoring_interval
  ; monitoring_role_arn
  ; kms_key_id
  ; pre_signed_url
  ; enable_i_a_m_database_authentication
  ; database_insights_mode
  ; enable_performance_insights
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; enable_cloudwatch_logs_exports
  ; processor_features
  ; use_default_processor_features
  ; deletion_protection
  ; domain
  ; domain_i_a_m_role_name
  ; domain_fqdn
  ; domain_ou
  ; domain_auth_secret_arn
  ; domain_dns_ips
  ; replica_mode
  ; enable_customer_owned_ip
  ; network_type
  ; max_allocated_storage
  ; backup_target
  ; custom_iam_instance_profile
  ; allocated_storage
  ; source_d_b_cluster_identifier
  ; dedicated_log_volume
  ; upgrade_storage_config
  ; c_a_certificate_identifier
  ; additional_storage_volumes
  ; tag_specifications
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; source_d_b_instance_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "SourceDBInstanceIdentifier" xml)
          String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; d_b_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupName" xml) String.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             VpcSecurityGroupIdList.parse)
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; pre_signed_url =
        Aws.Util.option_bind (Aws.Xml.member "PreSignedUrl" xml) String.parse
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
    ; enable_cloudwatch_logs_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnableCloudwatchLogsExports" xml)
             LogTypeList.parse)
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
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; domain_fqdn = Aws.Util.option_bind (Aws.Xml.member "DomainFqdn" xml) String.parse
    ; domain_ou = Aws.Util.option_bind (Aws.Xml.member "DomainOu" xml) String.parse
    ; domain_auth_secret_arn =
        Aws.Util.option_bind (Aws.Xml.member "DomainAuthSecretArn" xml) String.parse
    ; domain_dns_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DomainDnsIps" xml) StringList.parse)
    ; replica_mode =
        Aws.Util.option_bind (Aws.Xml.member "ReplicaMode" xml) ReplicaMode.parse
    ; enable_customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "EnableCustomerOwnedIp" xml) Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; max_allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "MaxAllocatedStorage" xml) Integer.parse
    ; backup_target =
        Aws.Util.option_bind (Aws.Xml.member "BackupTarget" xml) String.parse
    ; custom_iam_instance_profile =
        Aws.Util.option_bind (Aws.Xml.member "CustomIamInstanceProfile" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; source_d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "SourceDBClusterIdentifier" xml) String.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; upgrade_storage_config =
        Aws.Util.option_bind (Aws.Xml.member "UpgradeStorageConfig" xml) Boolean.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; additional_storage_volumes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AdditionalStorageVolumes" xml)
             AdditionalStorageVolumesList.parse)
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecifications" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "TagSpecifications.member"
              , TagSpecificationList.to_query v.tag_specifications ))
       ; Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , AdditionalStorageVolumesList.to_query v.additional_storage_volumes ))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.upgrade_storage_config (fun f ->
             Aws.Query.Pair ("UpgradeStorageConfig", Boolean.to_query f))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.source_d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("SourceDBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             Aws.Query.Pair ("CustomIamInstanceProfile", String.to_query f))
       ; Aws.Util.option_map v.backup_target (fun f ->
             Aws.Query.Pair ("BackupTarget", String.to_query f))
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             Aws.Query.Pair ("MaxAllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             Aws.Query.Pair ("EnableCustomerOwnedIp", Boolean.to_query f))
       ; Aws.Util.option_map v.replica_mode (fun f ->
             Aws.Query.Pair ("ReplicaMode", ReplicaMode.to_query f))
       ; Some
           (Aws.Query.Pair ("DomainDnsIps.member", StringList.to_query v.domain_dns_ips))
       ; Aws.Util.option_map v.domain_auth_secret_arn (fun f ->
             Aws.Query.Pair ("DomainAuthSecretArn", String.to_query f))
       ; Aws.Util.option_map v.domain_ou (fun f ->
             Aws.Query.Pair ("DomainOu", String.to_query f))
       ; Aws.Util.option_map v.domain_fqdn (fun f ->
             Aws.Query.Pair ("DomainFqdn", String.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.use_default_processor_features (fun f ->
             Aws.Query.Pair ("UseDefaultProcessorFeatures", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ProcessorFeatures.member"
              , ProcessorFeatureList.to_query v.processor_features ))
       ; Some
           (Aws.Query.Pair
              ( "EnableCloudwatchLogsExports.member"
              , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
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
       ; Aws.Util.option_map v.pre_signed_url (fun f ->
             Aws.Query.Pair ("PreSignedUrl", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupIds.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.source_d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("SourceDBInstanceIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.upgrade_storage_config (fun f ->
             "UpgradeStorageConfig", Boolean.to_json f)
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.source_d_b_cluster_identifier (fun f ->
             "SourceDBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             "CustomIamInstanceProfile", String.to_json f)
       ; Aws.Util.option_map v.backup_target (fun f -> "BackupTarget", String.to_json f)
       ; Aws.Util.option_map v.max_allocated_storage (fun f ->
             "MaxAllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             "EnableCustomerOwnedIp", Boolean.to_json f)
       ; Aws.Util.option_map v.replica_mode (fun f ->
             "ReplicaMode", ReplicaMode.to_json f)
       ; Some ("DomainDnsIps", StringList.to_json v.domain_dns_ips)
       ; Aws.Util.option_map v.domain_auth_secret_arn (fun f ->
             "DomainAuthSecretArn", String.to_json f)
       ; Aws.Util.option_map v.domain_ou (fun f -> "DomainOu", String.to_json f)
       ; Aws.Util.option_map v.domain_fqdn (fun f -> "DomainFqdn", String.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.use_default_processor_features (fun f ->
             "UseDefaultProcessorFeatures", Boolean.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Some
           ( "EnableCloudwatchLogsExports"
           , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
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
       ; Aws.Util.option_map v.pre_signed_url (fun f -> "PreSignedUrl", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Some
           ("VpcSecurityGroupIds", VpcSecurityGroupIdList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             "DBParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.source_d_b_instance_identifier (fun f ->
             "SourceDBInstanceIdentifier", String.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; source_d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceDBInstanceIdentifier") String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; d_b_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupName") String.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; vpc_security_group_ids =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; pre_signed_url = Aws.Util.option_map (Aws.Json.lookup j "PreSignedUrl") String.of_json
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
  ; enable_cloudwatch_logs_exports =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableCloudwatchLogsExports"))
  ; processor_features =
      ProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessorFeatures"))
  ; use_default_processor_features =
      Aws.Util.option_map
        (Aws.Json.lookup j "UseDefaultProcessorFeatures")
        Boolean.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; domain_fqdn = Aws.Util.option_map (Aws.Json.lookup j "DomainFqdn") String.of_json
  ; domain_ou = Aws.Util.option_map (Aws.Json.lookup j "DomainOu") String.of_json
  ; domain_auth_secret_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DomainAuthSecretArn") String.of_json
  ; domain_dns_ips =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainDnsIps"))
  ; replica_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicaMode") ReplicaMode.of_json
  ; enable_customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "EnableCustomerOwnedIp") Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; max_allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "MaxAllocatedStorage") Integer.of_json
  ; backup_target = Aws.Util.option_map (Aws.Json.lookup j "BackupTarget") String.of_json
  ; custom_iam_instance_profile =
      Aws.Util.option_map (Aws.Json.lookup j "CustomIamInstanceProfile") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; source_d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "SourceDBClusterIdentifier") String.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; upgrade_storage_config =
      Aws.Util.option_map (Aws.Json.lookup j "UpgradeStorageConfig") Boolean.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  }
