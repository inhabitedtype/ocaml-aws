open Aws.BaseTypes

type t =
  { allocated_storage : Integer.t option
  ; availability_zones : AvailabilityZones.t
  ; backup_retention_period : Integer.t option
  ; character_set_name : String.t option
  ; database_name : String.t option
  ; d_b_cluster_identifier : String.t option
  ; d_b_cluster_parameter_group : String.t option
  ; d_b_subnet_group : String.t option
  ; status : String.t option
  ; percent_progress : String.t option
  ; earliest_restorable_time : DateTime.t option
  ; endpoint : String.t option
  ; reader_endpoint : String.t option
  ; custom_endpoints : StringList.t
  ; multi_a_z : Boolean.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; latest_restorable_time : DateTime.t option
  ; port : Integer.t option
  ; master_username : String.t option
  ; d_b_cluster_option_group_memberships : DBClusterOptionGroupMemberships.t
  ; preferred_backup_window : String.t option
  ; preferred_maintenance_window : String.t option
  ; upgrade_rollout_order : UpgradeRolloutOrder.t option
  ; replication_source_identifier : String.t option
  ; read_replica_identifiers : ReadReplicaIdentifierList.t
  ; status_infos : DBClusterStatusInfoList.t
  ; d_b_cluster_members : DBClusterMemberList.t
  ; vpc_security_groups : VpcSecurityGroupMembershipList.t
  ; hosted_zone_id : String.t option
  ; storage_encrypted : Boolean.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; kms_key_id : String.t option
  ; db_cluster_resource_id : String.t option
  ; d_b_cluster_arn : String.t option
  ; associated_roles : DBClusterRoles.t
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; clone_group_id : String.t option
  ; cluster_create_time : DateTime.t option
  ; earliest_backtrack_time : DateTime.t option
  ; backtrack_window : Long.t option
  ; backtrack_consumed_change_records : Long.t option
  ; enabled_cloudwatch_logs_exports : LogTypeList.t
  ; capacity : Integer.t option
  ; pending_modified_values : ClusterPendingModifiedValues.t option
  ; engine_mode : String.t option
  ; scaling_configuration_info : ScalingConfigurationInfo.t option
  ; rds_custom_cluster_configuration : RdsCustomClusterConfiguration.t option
  ; d_b_cluster_instance_class : String.t option
  ; storage_type : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; i_o_optimized_next_allowed_modification_time : DateTime.t option
  ; publicly_accessible : Boolean.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; deletion_protection : Boolean.t option
  ; http_endpoint_enabled : Boolean.t option
  ; activity_stream_mode : ActivityStreamMode.t option
  ; activity_stream_status : ActivityStreamStatus.t option
  ; activity_stream_kms_key_id : String.t option
  ; activity_stream_kinesis_stream_name : String.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; cross_account_clone : Boolean.t option
  ; domain_memberships : DomainMembershipList.t
  ; tag_list : TagList.t
  ; global_cluster_identifier : String.t option
  ; global_write_forwarding_status : WriteForwardingStatus.t option
  ; global_write_forwarding_requested : Boolean.t option
  ; network_type : String.t option
  ; automatic_restart_time : DateTime.t option
  ; serverless_v2_scaling_configuration : ServerlessV2ScalingConfigurationInfo.t option
  ; serverless_v2_platform_version : String.t option
  ; monitoring_interval : Integer.t option
  ; monitoring_role_arn : String.t option
  ; database_insights_mode : DatabaseInsightsMode.t option
  ; performance_insights_enabled : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; d_b_system_id : String.t option
  ; master_user_secret : MasterUserSecret.t option
  ; local_write_forwarding_status : LocalWriteForwardingStatus.t option
  ; aws_backup_recovery_point_arn : String.t option
  ; limitless_database : LimitlessDatabase.t option
  ; cluster_scalability_type : ClusterScalabilityType.t option
  ; certificate_details : CertificateDetails.t option
  ; engine_lifecycle_support : String.t option
  ; v_p_c_networking_enabled : Boolean.t option
  ; internet_access_gateway_enabled : Boolean.t option
  }

let make
    ?allocated_storage
    ?(availability_zones = [])
    ?backup_retention_period
    ?character_set_name
    ?database_name
    ?d_b_cluster_identifier
    ?d_b_cluster_parameter_group
    ?d_b_subnet_group
    ?status
    ?percent_progress
    ?earliest_restorable_time
    ?endpoint
    ?reader_endpoint
    ?(custom_endpoints = [])
    ?multi_a_z
    ?engine
    ?engine_version
    ?latest_restorable_time
    ?port
    ?master_username
    ?(d_b_cluster_option_group_memberships = [])
    ?preferred_backup_window
    ?preferred_maintenance_window
    ?upgrade_rollout_order
    ?replication_source_identifier
    ?(read_replica_identifiers = [])
    ?(status_infos = [])
    ?(d_b_cluster_members = [])
    ?(vpc_security_groups = [])
    ?hosted_zone_id
    ?storage_encrypted
    ?storage_encryption_type
    ?kms_key_id
    ?db_cluster_resource_id
    ?d_b_cluster_arn
    ?(associated_roles = [])
    ?i_a_m_database_authentication_enabled
    ?clone_group_id
    ?cluster_create_time
    ?earliest_backtrack_time
    ?backtrack_window
    ?backtrack_consumed_change_records
    ?(enabled_cloudwatch_logs_exports = [])
    ?capacity
    ?pending_modified_values
    ?engine_mode
    ?scaling_configuration_info
    ?rds_custom_cluster_configuration
    ?d_b_cluster_instance_class
    ?storage_type
    ?iops
    ?storage_throughput
    ?i_o_optimized_next_allowed_modification_time
    ?publicly_accessible
    ?auto_minor_version_upgrade
    ?deletion_protection
    ?http_endpoint_enabled
    ?activity_stream_mode
    ?activity_stream_status
    ?activity_stream_kms_key_id
    ?activity_stream_kinesis_stream_name
    ?copy_tags_to_snapshot
    ?cross_account_clone
    ?(domain_memberships = [])
    ?(tag_list = [])
    ?global_cluster_identifier
    ?global_write_forwarding_status
    ?global_write_forwarding_requested
    ?network_type
    ?automatic_restart_time
    ?serverless_v2_scaling_configuration
    ?serverless_v2_platform_version
    ?monitoring_interval
    ?monitoring_role_arn
    ?database_insights_mode
    ?performance_insights_enabled
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?d_b_system_id
    ?master_user_secret
    ?local_write_forwarding_status
    ?aws_backup_recovery_point_arn
    ?limitless_database
    ?cluster_scalability_type
    ?certificate_details
    ?engine_lifecycle_support
    ?v_p_c_networking_enabled
    ?internet_access_gateway_enabled
    () =
  { allocated_storage
  ; availability_zones
  ; backup_retention_period
  ; character_set_name
  ; database_name
  ; d_b_cluster_identifier
  ; d_b_cluster_parameter_group
  ; d_b_subnet_group
  ; status
  ; percent_progress
  ; earliest_restorable_time
  ; endpoint
  ; reader_endpoint
  ; custom_endpoints
  ; multi_a_z
  ; engine
  ; engine_version
  ; latest_restorable_time
  ; port
  ; master_username
  ; d_b_cluster_option_group_memberships
  ; preferred_backup_window
  ; preferred_maintenance_window
  ; upgrade_rollout_order
  ; replication_source_identifier
  ; read_replica_identifiers
  ; status_infos
  ; d_b_cluster_members
  ; vpc_security_groups
  ; hosted_zone_id
  ; storage_encrypted
  ; storage_encryption_type
  ; kms_key_id
  ; db_cluster_resource_id
  ; d_b_cluster_arn
  ; associated_roles
  ; i_a_m_database_authentication_enabled
  ; clone_group_id
  ; cluster_create_time
  ; earliest_backtrack_time
  ; backtrack_window
  ; backtrack_consumed_change_records
  ; enabled_cloudwatch_logs_exports
  ; capacity
  ; pending_modified_values
  ; engine_mode
  ; scaling_configuration_info
  ; rds_custom_cluster_configuration
  ; d_b_cluster_instance_class
  ; storage_type
  ; iops
  ; storage_throughput
  ; i_o_optimized_next_allowed_modification_time
  ; publicly_accessible
  ; auto_minor_version_upgrade
  ; deletion_protection
  ; http_endpoint_enabled
  ; activity_stream_mode
  ; activity_stream_status
  ; activity_stream_kms_key_id
  ; activity_stream_kinesis_stream_name
  ; copy_tags_to_snapshot
  ; cross_account_clone
  ; domain_memberships
  ; tag_list
  ; global_cluster_identifier
  ; global_write_forwarding_status
  ; global_write_forwarding_requested
  ; network_type
  ; automatic_restart_time
  ; serverless_v2_scaling_configuration
  ; serverless_v2_platform_version
  ; monitoring_interval
  ; monitoring_role_arn
  ; database_insights_mode
  ; performance_insights_enabled
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; d_b_system_id
  ; master_user_secret
  ; local_write_forwarding_status
  ; aws_backup_recovery_point_arn
  ; limitless_database
  ; cluster_scalability_type
  ; certificate_details
  ; engine_lifecycle_support
  ; v_p_c_networking_enabled
  ; internet_access_gateway_enabled
  }

let parse xml =
  Some
    { allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; availability_zones =
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
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; d_b_cluster_parameter_group =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterParameterGroup" xml) String.parse
    ; d_b_subnet_group =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroup" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; percent_progress =
        Aws.Util.option_bind (Aws.Xml.member "PercentProgress" xml) String.parse
    ; earliest_restorable_time =
        Aws.Util.option_bind (Aws.Xml.member "EarliestRestorableTime" xml) DateTime.parse
    ; endpoint = Aws.Util.option_bind (Aws.Xml.member "Endpoint" xml) String.parse
    ; reader_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ReaderEndpoint" xml) String.parse
    ; custom_endpoints =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "CustomEndpoints" xml) StringList.parse)
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; latest_restorable_time =
        Aws.Util.option_bind (Aws.Xml.member "LatestRestorableTime" xml) DateTime.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; master_username =
        Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse
    ; d_b_cluster_option_group_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterOptionGroupMemberships" xml)
             DBClusterOptionGroupMemberships.parse)
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; upgrade_rollout_order =
        Aws.Util.option_bind
          (Aws.Xml.member "UpgradeRolloutOrder" xml)
          UpgradeRolloutOrder.parse
    ; replication_source_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationSourceIdentifier" xml)
          String.parse
    ; read_replica_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReadReplicaIdentifiers" xml)
             ReadReplicaIdentifierList.parse)
    ; status_infos =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "StatusInfos" xml)
             DBClusterStatusInfoList.parse)
    ; d_b_cluster_members =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterMembers" xml)
             DBClusterMemberList.parse)
    ; vpc_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroups" xml)
             VpcSecurityGroupMembershipList.parse)
    ; hosted_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "HostedZoneId" xml) String.parse
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; db_cluster_resource_id =
        Aws.Util.option_bind (Aws.Xml.member "DbClusterResourceId" xml) String.parse
    ; d_b_cluster_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterArn" xml) String.parse
    ; associated_roles =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AssociatedRoles" xml)
             DBClusterRoles.parse)
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; clone_group_id =
        Aws.Util.option_bind (Aws.Xml.member "CloneGroupId" xml) String.parse
    ; cluster_create_time =
        Aws.Util.option_bind (Aws.Xml.member "ClusterCreateTime" xml) DateTime.parse
    ; earliest_backtrack_time =
        Aws.Util.option_bind (Aws.Xml.member "EarliestBacktrackTime" xml) DateTime.parse
    ; backtrack_window =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackWindow" xml) Long.parse
    ; backtrack_consumed_change_records =
        Aws.Util.option_bind
          (Aws.Xml.member "BacktrackConsumedChangeRecords" xml)
          Long.parse
    ; enabled_cloudwatch_logs_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnabledCloudwatchLogsExports" xml)
             LogTypeList.parse)
    ; capacity = Aws.Util.option_bind (Aws.Xml.member "Capacity" xml) Integer.parse
    ; pending_modified_values =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingModifiedValues" xml)
          ClusterPendingModifiedValues.parse
    ; engine_mode = Aws.Util.option_bind (Aws.Xml.member "EngineMode" xml) String.parse
    ; scaling_configuration_info =
        Aws.Util.option_bind
          (Aws.Xml.member "ScalingConfigurationInfo" xml)
          ScalingConfigurationInfo.parse
    ; rds_custom_cluster_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsCustomClusterConfiguration" xml)
          RdsCustomClusterConfiguration.parse
    ; d_b_cluster_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterInstanceClass" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; i_o_optimized_next_allowed_modification_time =
        Aws.Util.option_bind
          (Aws.Xml.member "IOOptimizedNextAllowedModificationTime" xml)
          DateTime.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; http_endpoint_enabled =
        Aws.Util.option_bind (Aws.Xml.member "HttpEndpointEnabled" xml) Boolean.parse
    ; activity_stream_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "ActivityStreamMode" xml)
          ActivityStreamMode.parse
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
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; cross_account_clone =
        Aws.Util.option_bind (Aws.Xml.member "CrossAccountClone" xml) Boolean.parse
    ; domain_memberships =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DomainMemberships" xml)
             DomainMembershipList.parse)
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; global_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "GlobalClusterIdentifier" xml) String.parse
    ; global_write_forwarding_status =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalWriteForwardingStatus" xml)
          WriteForwardingStatus.parse
    ; global_write_forwarding_requested =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalWriteForwardingRequested" xml)
          Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; automatic_restart_time =
        Aws.Util.option_bind (Aws.Xml.member "AutomaticRestartTime" xml) DateTime.parse
    ; serverless_v2_scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2ScalingConfiguration" xml)
          ServerlessV2ScalingConfigurationInfo.parse
    ; serverless_v2_platform_version =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2PlatformVersion" xml)
          String.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
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
    ; d_b_system_id = Aws.Util.option_bind (Aws.Xml.member "DBSystemId" xml) String.parse
    ; master_user_secret =
        Aws.Util.option_bind
          (Aws.Xml.member "MasterUserSecret" xml)
          MasterUserSecret.parse
    ; local_write_forwarding_status =
        Aws.Util.option_bind
          (Aws.Xml.member "LocalWriteForwardingStatus" xml)
          LocalWriteForwardingStatus.parse
    ; aws_backup_recovery_point_arn =
        Aws.Util.option_bind (Aws.Xml.member "AwsBackupRecoveryPointArn" xml) String.parse
    ; limitless_database =
        Aws.Util.option_bind
          (Aws.Xml.member "LimitlessDatabase" xml)
          LimitlessDatabase.parse
    ; cluster_scalability_type =
        Aws.Util.option_bind
          (Aws.Xml.member "ClusterScalabilityType" xml)
          ClusterScalabilityType.parse
    ; certificate_details =
        Aws.Util.option_bind
          (Aws.Xml.member "CertificateDetails" xml)
          CertificateDetails.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
    ; v_p_c_networking_enabled =
        Aws.Util.option_bind (Aws.Xml.member "VPCNetworkingEnabled" xml) Boolean.parse
    ; internet_access_gateway_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "InternetAccessGatewayEnabled" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_access_gateway_enabled (fun f ->
             Aws.Query.Pair ("InternetAccessGatewayEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.v_p_c_networking_enabled (fun f ->
             Aws.Query.Pair ("VPCNetworkingEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.certificate_details (fun f ->
             Aws.Query.Pair ("CertificateDetails", CertificateDetails.to_query f))
       ; Aws.Util.option_map v.cluster_scalability_type (fun f ->
             Aws.Query.Pair ("ClusterScalabilityType", ClusterScalabilityType.to_query f))
       ; Aws.Util.option_map v.limitless_database (fun f ->
             Aws.Query.Pair ("LimitlessDatabase", LimitlessDatabase.to_query f))
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             Aws.Query.Pair ("AwsBackupRecoveryPointArn", String.to_query f))
       ; Aws.Util.option_map v.local_write_forwarding_status (fun f ->
             Aws.Query.Pair
               ("LocalWriteForwardingStatus", LocalWriteForwardingStatus.to_query f))
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             Aws.Query.Pair ("MasterUserSecret", MasterUserSecret.to_query f))
       ; Aws.Util.option_map v.d_b_system_id (fun f ->
             Aws.Query.Pair ("DBSystemId", String.to_query f))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             Aws.Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             Aws.Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.performance_insights_enabled (fun f ->
             Aws.Query.Pair ("PerformanceInsightsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             Aws.Query.Pair ("DatabaseInsightsMode", DatabaseInsightsMode.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             Aws.Query.Pair ("ServerlessV2PlatformVersion", String.to_query f))
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             Aws.Query.Pair
               ( "ServerlessV2ScalingConfiguration"
               , ServerlessV2ScalingConfigurationInfo.to_query f ))
       ; Aws.Util.option_map v.automatic_restart_time (fun f ->
             Aws.Query.Pair ("AutomaticRestartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.global_write_forwarding_requested (fun f ->
             Aws.Query.Pair ("GlobalWriteForwardingRequested", Boolean.to_query f))
       ; Aws.Util.option_map v.global_write_forwarding_status (fun f ->
             Aws.Query.Pair
               ("GlobalWriteForwardingStatus", WriteForwardingStatus.to_query f))
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             Aws.Query.Pair ("GlobalClusterIdentifier", String.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Some
           (Aws.Query.Pair
              ( "DomainMemberships.member"
              , DomainMembershipList.to_query v.domain_memberships ))
       ; Aws.Util.option_map v.cross_account_clone (fun f ->
             Aws.Query.Pair ("CrossAccountClone", Boolean.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
             Aws.Query.Pair ("ActivityStreamKinesisStreamName", String.to_query f))
       ; Aws.Util.option_map v.activity_stream_kms_key_id (fun f ->
             Aws.Query.Pair ("ActivityStreamKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.activity_stream_status (fun f ->
             Aws.Query.Pair ("ActivityStreamStatus", ActivityStreamStatus.to_query f))
       ; Aws.Util.option_map v.activity_stream_mode (fun f ->
             Aws.Query.Pair ("ActivityStreamMode", ActivityStreamMode.to_query f))
       ; Aws.Util.option_map v.http_endpoint_enabled (fun f ->
             Aws.Query.Pair ("HttpEndpointEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.i_o_optimized_next_allowed_modification_time (fun f ->
             Aws.Query.Pair ("IOOptimizedNextAllowedModificationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             Aws.Query.Pair ("DBClusterInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             Aws.Query.Pair
               ("RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_query f))
       ; Aws.Util.option_map v.scaling_configuration_info (fun f ->
             Aws.Query.Pair
               ("ScalingConfigurationInfo", ScalingConfigurationInfo.to_query f))
       ; Aws.Util.option_map v.engine_mode (fun f ->
             Aws.Query.Pair ("EngineMode", String.to_query f))
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             Aws.Query.Pair
               ("PendingModifiedValues", ClusterPendingModifiedValues.to_query f))
       ; Aws.Util.option_map v.capacity (fun f ->
             Aws.Query.Pair ("Capacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EnabledCloudwatchLogsExports.member"
              , LogTypeList.to_query v.enabled_cloudwatch_logs_exports ))
       ; Aws.Util.option_map v.backtrack_consumed_change_records (fun f ->
             Aws.Query.Pair ("BacktrackConsumedChangeRecords", Long.to_query f))
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             Aws.Query.Pair ("BacktrackWindow", Long.to_query f))
       ; Aws.Util.option_map v.earliest_backtrack_time (fun f ->
             Aws.Query.Pair ("EarliestBacktrackTime", DateTime.to_query f))
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             Aws.Query.Pair ("ClusterCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.clone_group_id (fun f ->
             Aws.Query.Pair ("CloneGroupId", String.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AssociatedRoles.member", DBClusterRoles.to_query v.associated_roles))
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f ->
             Aws.Query.Pair ("DBClusterArn", String.to_query f))
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             Aws.Query.Pair ("DbClusterResourceId", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.hosted_zone_id (fun f ->
             Aws.Query.Pair ("HostedZoneId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroups.member"
              , VpcSecurityGroupMembershipList.to_query v.vpc_security_groups ))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterMembers.member"
              , DBClusterMemberList.to_query v.d_b_cluster_members ))
       ; Some
           (Aws.Query.Pair
              ("StatusInfos.member", DBClusterStatusInfoList.to_query v.status_infos))
       ; Some
           (Aws.Query.Pair
              ( "ReadReplicaIdentifiers.member"
              , ReadReplicaIdentifierList.to_query v.read_replica_identifiers ))
       ; Aws.Util.option_map v.replication_source_identifier (fun f ->
             Aws.Query.Pair ("ReplicationSourceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.upgrade_rollout_order (fun f ->
             Aws.Query.Pair ("UpgradeRolloutOrder", UpgradeRolloutOrder.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterOptionGroupMemberships.member"
              , DBClusterOptionGroupMemberships.to_query
                  v.d_b_cluster_option_group_memberships ))
       ; Aws.Util.option_map v.master_username (fun f ->
             Aws.Query.Pair ("MasterUsername", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.latest_restorable_time (fun f ->
             Aws.Query.Pair ("LatestRestorableTime", DateTime.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("CustomEndpoints.member", StringList.to_query v.custom_endpoints))
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             Aws.Query.Pair ("ReaderEndpoint", String.to_query f))
       ; Aws.Util.option_map v.endpoint (fun f ->
             Aws.Query.Pair ("Endpoint", String.to_query f))
       ; Aws.Util.option_map v.earliest_restorable_time (fun f ->
             Aws.Query.Pair ("EarliestRestorableTime", DateTime.to_query f))
       ; Aws.Util.option_map v.percent_progress (fun f ->
             Aws.Query.Pair ("PercentProgress", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group (fun f ->
             Aws.Query.Pair ("DBSubnetGroup", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_parameter_group (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroup", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.database_name (fun f ->
             Aws.Query.Pair ("DatabaseName", String.to_query f))
       ; Aws.Util.option_map v.character_set_name (fun f ->
             Aws.Query.Pair ("CharacterSetName", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.internet_access_gateway_enabled (fun f ->
             "InternetAccessGatewayEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.v_p_c_networking_enabled (fun f ->
             "VPCNetworkingEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.certificate_details (fun f ->
             "CertificateDetails", CertificateDetails.to_json f)
       ; Aws.Util.option_map v.cluster_scalability_type (fun f ->
             "ClusterScalabilityType", ClusterScalabilityType.to_json f)
       ; Aws.Util.option_map v.limitless_database (fun f ->
             "LimitlessDatabase", LimitlessDatabase.to_json f)
       ; Aws.Util.option_map v.aws_backup_recovery_point_arn (fun f ->
             "AwsBackupRecoveryPointArn", String.to_json f)
       ; Aws.Util.option_map v.local_write_forwarding_status (fun f ->
             "LocalWriteForwardingStatus", LocalWriteForwardingStatus.to_json f)
       ; Aws.Util.option_map v.master_user_secret (fun f ->
             "MasterUserSecret", MasterUserSecret.to_json f)
       ; Aws.Util.option_map v.d_b_system_id (fun f -> "DBSystemId", String.to_json f)
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             "PerformanceInsightsRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             "PerformanceInsightsKMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.performance_insights_enabled (fun f ->
             "PerformanceInsightsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.database_insights_mode (fun f ->
             "DatabaseInsightsMode", DatabaseInsightsMode.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.serverless_v2_platform_version (fun f ->
             "ServerlessV2PlatformVersion", String.to_json f)
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             ( "ServerlessV2ScalingConfiguration"
             , ServerlessV2ScalingConfigurationInfo.to_json f ))
       ; Aws.Util.option_map v.automatic_restart_time (fun f ->
             "AutomaticRestartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.global_write_forwarding_requested (fun f ->
             "GlobalWriteForwardingRequested", Boolean.to_json f)
       ; Aws.Util.option_map v.global_write_forwarding_status (fun f ->
             "GlobalWriteForwardingStatus", WriteForwardingStatus.to_json f)
       ; Aws.Util.option_map v.global_cluster_identifier (fun f ->
             "GlobalClusterIdentifier", String.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Some ("DomainMemberships", DomainMembershipList.to_json v.domain_memberships)
       ; Aws.Util.option_map v.cross_account_clone (fun f ->
             "CrossAccountClone", Boolean.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.activity_stream_kinesis_stream_name (fun f ->
             "ActivityStreamKinesisStreamName", String.to_json f)
       ; Aws.Util.option_map v.activity_stream_kms_key_id (fun f ->
             "ActivityStreamKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.activity_stream_status (fun f ->
             "ActivityStreamStatus", ActivityStreamStatus.to_json f)
       ; Aws.Util.option_map v.activity_stream_mode (fun f ->
             "ActivityStreamMode", ActivityStreamMode.to_json f)
       ; Aws.Util.option_map v.http_endpoint_enabled (fun f ->
             "HttpEndpointEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.i_o_optimized_next_allowed_modification_time (fun f ->
             "IOOptimizedNextAllowedModificationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             "DBClusterInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             "RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_json f)
       ; Aws.Util.option_map v.scaling_configuration_info (fun f ->
             "ScalingConfigurationInfo", ScalingConfigurationInfo.to_json f)
       ; Aws.Util.option_map v.engine_mode (fun f -> "EngineMode", String.to_json f)
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             "PendingModifiedValues", ClusterPendingModifiedValues.to_json f)
       ; Aws.Util.option_map v.capacity (fun f -> "Capacity", Integer.to_json f)
       ; Some
           ( "EnabledCloudwatchLogsExports"
           , LogTypeList.to_json v.enabled_cloudwatch_logs_exports )
       ; Aws.Util.option_map v.backtrack_consumed_change_records (fun f ->
             "BacktrackConsumedChangeRecords", Long.to_json f)
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             "BacktrackWindow", Long.to_json f)
       ; Aws.Util.option_map v.earliest_backtrack_time (fun f ->
             "EarliestBacktrackTime", DateTime.to_json f)
       ; Aws.Util.option_map v.cluster_create_time (fun f ->
             "ClusterCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.clone_group_id (fun f -> "CloneGroupId", String.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Some ("AssociatedRoles", DBClusterRoles.to_json v.associated_roles)
       ; Aws.Util.option_map v.d_b_cluster_arn (fun f -> "DBClusterArn", String.to_json f)
       ; Aws.Util.option_map v.db_cluster_resource_id (fun f ->
             "DbClusterResourceId", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.hosted_zone_id (fun f -> "HostedZoneId", String.to_json f)
       ; Some
           ( "VpcSecurityGroups"
           , VpcSecurityGroupMembershipList.to_json v.vpc_security_groups )
       ; Some ("DBClusterMembers", DBClusterMemberList.to_json v.d_b_cluster_members)
       ; Some ("StatusInfos", DBClusterStatusInfoList.to_json v.status_infos)
       ; Some
           ( "ReadReplicaIdentifiers"
           , ReadReplicaIdentifierList.to_json v.read_replica_identifiers )
       ; Aws.Util.option_map v.replication_source_identifier (fun f ->
             "ReplicationSourceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.upgrade_rollout_order (fun f ->
             "UpgradeRolloutOrder", UpgradeRolloutOrder.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Some
           ( "DBClusterOptionGroupMemberships"
           , DBClusterOptionGroupMemberships.to_json
               v.d_b_cluster_option_group_memberships )
       ; Aws.Util.option_map v.master_username (fun f ->
             "MasterUsername", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.latest_restorable_time (fun f ->
             "LatestRestorableTime", DateTime.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Some ("CustomEndpoints", StringList.to_json v.custom_endpoints)
       ; Aws.Util.option_map v.reader_endpoint (fun f ->
             "ReaderEndpoint", String.to_json f)
       ; Aws.Util.option_map v.endpoint (fun f -> "Endpoint", String.to_json f)
       ; Aws.Util.option_map v.earliest_restorable_time (fun f ->
             "EarliestRestorableTime", DateTime.to_json f)
       ; Aws.Util.option_map v.percent_progress (fun f ->
             "PercentProgress", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group (fun f ->
             "DBSubnetGroup", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_parameter_group (fun f ->
             "DBClusterParameterGroup", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.database_name (fun f -> "DatabaseName", String.to_json f)
       ; Aws.Util.option_map v.character_set_name (fun f ->
             "CharacterSetName", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ])

let of_json j =
  { allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; character_set_name =
      Aws.Util.option_map (Aws.Json.lookup j "CharacterSetName") String.of_json
  ; database_name = Aws.Util.option_map (Aws.Json.lookup j "DatabaseName") String.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; d_b_cluster_parameter_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroup") String.of_json
  ; d_b_subnet_group =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroup") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; percent_progress =
      Aws.Util.option_map (Aws.Json.lookup j "PercentProgress") String.of_json
  ; earliest_restorable_time =
      Aws.Util.option_map (Aws.Json.lookup j "EarliestRestorableTime") DateTime.of_json
  ; endpoint = Aws.Util.option_map (Aws.Json.lookup j "Endpoint") String.of_json
  ; reader_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ReaderEndpoint") String.of_json
  ; custom_endpoints =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CustomEndpoints"))
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; latest_restorable_time =
      Aws.Util.option_map (Aws.Json.lookup j "LatestRestorableTime") DateTime.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; master_username =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUsername") String.of_json
  ; d_b_cluster_option_group_memberships =
      DBClusterOptionGroupMemberships.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterOptionGroupMemberships"))
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; upgrade_rollout_order =
      Aws.Util.option_map
        (Aws.Json.lookup j "UpgradeRolloutOrder")
        UpgradeRolloutOrder.of_json
  ; replication_source_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationSourceIdentifier") String.of_json
  ; read_replica_identifiers =
      ReadReplicaIdentifierList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReadReplicaIdentifiers"))
  ; status_infos =
      DBClusterStatusInfoList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "StatusInfos"))
  ; d_b_cluster_members =
      DBClusterMemberList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterMembers"))
  ; vpc_security_groups =
      VpcSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroups"))
  ; hosted_zone_id = Aws.Util.option_map (Aws.Json.lookup j "HostedZoneId") String.of_json
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; db_cluster_resource_id =
      Aws.Util.option_map (Aws.Json.lookup j "DbClusterResourceId") String.of_json
  ; d_b_cluster_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterArn") String.of_json
  ; associated_roles =
      DBClusterRoles.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AssociatedRoles"))
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; clone_group_id = Aws.Util.option_map (Aws.Json.lookup j "CloneGroupId") String.of_json
  ; cluster_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterCreateTime") DateTime.of_json
  ; earliest_backtrack_time =
      Aws.Util.option_map (Aws.Json.lookup j "EarliestBacktrackTime") DateTime.of_json
  ; backtrack_window =
      Aws.Util.option_map (Aws.Json.lookup j "BacktrackWindow") Long.of_json
  ; backtrack_consumed_change_records =
      Aws.Util.option_map
        (Aws.Json.lookup j "BacktrackConsumedChangeRecords")
        Long.of_json
  ; enabled_cloudwatch_logs_exports =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnabledCloudwatchLogsExports"))
  ; capacity = Aws.Util.option_map (Aws.Json.lookup j "Capacity") Integer.of_json
  ; pending_modified_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingModifiedValues")
        ClusterPendingModifiedValues.of_json
  ; engine_mode = Aws.Util.option_map (Aws.Json.lookup j "EngineMode") String.of_json
  ; scaling_configuration_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "ScalingConfigurationInfo")
        ScalingConfigurationInfo.of_json
  ; rds_custom_cluster_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsCustomClusterConfiguration")
        RdsCustomClusterConfiguration.of_json
  ; d_b_cluster_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterInstanceClass") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; i_o_optimized_next_allowed_modification_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "IOOptimizedNextAllowedModificationTime")
        DateTime.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; http_endpoint_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "HttpEndpointEnabled") Boolean.of_json
  ; activity_stream_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "ActivityStreamMode")
        ActivityStreamMode.of_json
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
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; cross_account_clone =
      Aws.Util.option_map (Aws.Json.lookup j "CrossAccountClone") Boolean.of_json
  ; domain_memberships =
      DomainMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainMemberships"))
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; global_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalClusterIdentifier") String.of_json
  ; global_write_forwarding_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalWriteForwardingStatus")
        WriteForwardingStatus.of_json
  ; global_write_forwarding_requested =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalWriteForwardingRequested")
        Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; automatic_restart_time =
      Aws.Util.option_map (Aws.Json.lookup j "AutomaticRestartTime") DateTime.of_json
  ; serverless_v2_scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2ScalingConfiguration")
        ServerlessV2ScalingConfigurationInfo.of_json
  ; serverless_v2_platform_version =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessV2PlatformVersion") String.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
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
  ; d_b_system_id = Aws.Util.option_map (Aws.Json.lookup j "DBSystemId") String.of_json
  ; master_user_secret =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecret") MasterUserSecret.of_json
  ; local_write_forwarding_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "LocalWriteForwardingStatus")
        LocalWriteForwardingStatus.of_json
  ; aws_backup_recovery_point_arn =
      Aws.Util.option_map (Aws.Json.lookup j "AwsBackupRecoveryPointArn") String.of_json
  ; limitless_database =
      Aws.Util.option_map
        (Aws.Json.lookup j "LimitlessDatabase")
        LimitlessDatabase.of_json
  ; cluster_scalability_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "ClusterScalabilityType")
        ClusterScalabilityType.of_json
  ; certificate_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "CertificateDetails")
        CertificateDetails.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; v_p_c_networking_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "VPCNetworkingEnabled") Boolean.of_json
  ; internet_access_gateway_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "InternetAccessGatewayEnabled")
        Boolean.of_json
  }
