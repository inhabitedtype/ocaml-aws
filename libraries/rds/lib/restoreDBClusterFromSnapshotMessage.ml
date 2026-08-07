open Aws.BaseTypes

type t =
  { availability_zones : AvailabilityZones.t
  ; d_b_cluster_identifier : String.t
  ; snapshot_identifier : String.t
  ; engine : String.t
  ; engine_version : String.t option
  ; port : Integer.t option
  ; d_b_subnet_group_name : String.t option
  ; database_name : String.t option
  ; option_group_name : String.t option
  ; vpc_security_group_ids : VpcSecurityGroupIdList.t
  ; tags : TagList.t
  ; kms_key_id : String.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; backtrack_window : Long.t option
  ; enable_cloudwatch_logs_exports : LogTypeList.t
  ; engine_mode : String.t option
  ; scaling_configuration : ScalingConfiguration.t option
  ; d_b_cluster_parameter_group_name : String.t option
  ; deletion_protection : Boolean.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; domain : String.t option
  ; domain_i_a_m_role_name : String.t option
  ; d_b_cluster_instance_class : String.t option
  ; storage_type : String.t option
  ; iops : Integer.t option
  ; publicly_accessible : Boolean.t option
  ; network_type : String.t option
  ; serverless_v2_scaling_configuration : ServerlessV2ScalingConfiguration.t option
  ; rds_custom_cluster_configuration : RdsCustomClusterConfiguration.t option
  ; monitoring_interval : Integer.t option
  ; monitoring_role_arn : String.t option
  ; enable_performance_insights : Boolean.t option
  ; performance_insights_k_m_s_key_id : String.t option
  ; performance_insights_retention_period : Integer.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; engine_lifecycle_support : String.t option
  ; tag_specifications : TagSpecificationList.t
  ; enable_v_p_c_networking : Boolean.t option
  ; enable_internet_access_gateway : Boolean.t option
  }

let make
    ?(availability_zones = [])
    ~d_b_cluster_identifier
    ~snapshot_identifier
    ~engine
    ?engine_version
    ?port
    ?d_b_subnet_group_name
    ?database_name
    ?option_group_name
    ?(vpc_security_group_ids = [])
    ?(tags = [])
    ?kms_key_id
    ?enable_i_a_m_database_authentication
    ?backtrack_window
    ?(enable_cloudwatch_logs_exports = [])
    ?engine_mode
    ?scaling_configuration
    ?d_b_cluster_parameter_group_name
    ?deletion_protection
    ?copy_tags_to_snapshot
    ?domain
    ?domain_i_a_m_role_name
    ?d_b_cluster_instance_class
    ?storage_type
    ?iops
    ?publicly_accessible
    ?network_type
    ?serverless_v2_scaling_configuration
    ?rds_custom_cluster_configuration
    ?monitoring_interval
    ?monitoring_role_arn
    ?enable_performance_insights
    ?performance_insights_k_m_s_key_id
    ?performance_insights_retention_period
    ?backup_retention_period
    ?preferred_backup_window
    ?engine_lifecycle_support
    ?(tag_specifications = [])
    ?enable_v_p_c_networking
    ?enable_internet_access_gateway
    () =
  { availability_zones
  ; d_b_cluster_identifier
  ; snapshot_identifier
  ; engine
  ; engine_version
  ; port
  ; d_b_subnet_group_name
  ; database_name
  ; option_group_name
  ; vpc_security_group_ids
  ; tags
  ; kms_key_id
  ; enable_i_a_m_database_authentication
  ; backtrack_window
  ; enable_cloudwatch_logs_exports
  ; engine_mode
  ; scaling_configuration
  ; d_b_cluster_parameter_group_name
  ; deletion_protection
  ; copy_tags_to_snapshot
  ; domain
  ; domain_i_a_m_role_name
  ; d_b_cluster_instance_class
  ; storage_type
  ; iops
  ; publicly_accessible
  ; network_type
  ; serverless_v2_scaling_configuration
  ; rds_custom_cluster_configuration
  ; monitoring_interval
  ; monitoring_role_arn
  ; enable_performance_insights
  ; performance_insights_k_m_s_key_id
  ; performance_insights_retention_period
  ; backup_retention_period
  ; preferred_backup_window
  ; engine_lifecycle_support
  ; tag_specifications
  ; enable_v_p_c_networking
  ; enable_internet_access_gateway
  }

let parse xml =
  Some
    { availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZones.parse)
    ; d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; snapshot_identifier =
        Aws.Xml.required
          "SnapshotIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotIdentifier" xml) String.parse)
    ; engine =
        Aws.Xml.required
          "Engine"
          (Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse)
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; database_name =
        Aws.Util.option_bind (Aws.Xml.member "DatabaseName" xml) String.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             VpcSecurityGroupIdList.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
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
    ; d_b_cluster_parameter_group_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterParameterGroupName" xml)
          String.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; d_b_cluster_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterInstanceClass" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; serverless_v2_scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2ScalingConfiguration" xml)
          ServerlessV2ScalingConfiguration.parse
    ; rds_custom_cluster_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsCustomClusterConfiguration" xml)
          RdsCustomClusterConfiguration.parse
    ; monitoring_interval =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringInterval" xml) Integer.parse
    ; monitoring_role_arn =
        Aws.Util.option_bind (Aws.Xml.member "MonitoringRoleArn" xml) String.parse
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
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecifications" xml)
             TagSpecificationList.parse)
    ; enable_v_p_c_networking =
        Aws.Util.option_bind (Aws.Xml.member "EnableVPCNetworking" xml) Boolean.parse
    ; enable_internet_access_gateway =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableInternetAccessGateway" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_internet_access_gateway (fun f ->
             Aws.Query.Pair ("EnableInternetAccessGateway", Boolean.to_query f))
       ; Aws.Util.option_map v.enable_v_p_c_networking (fun f ->
             Aws.Query.Pair ("EnableVPCNetworking", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecifications.member"
              , TagSpecificationList.to_query v.tag_specifications ))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             Aws.Query.Pair ("PerformanceInsightsRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             Aws.Query.Pair ("PerformanceInsightsKMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             Aws.Query.Pair ("EnablePerformanceInsights", Boolean.to_query f))
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             Aws.Query.Pair ("MonitoringRoleArn", String.to_query f))
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             Aws.Query.Pair ("MonitoringInterval", Integer.to_query f))
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             Aws.Query.Pair
               ("RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_query f))
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             Aws.Query.Pair
               ( "ServerlessV2ScalingConfiguration"
               , ServerlessV2ScalingConfiguration.to_query f ))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             Aws.Query.Pair ("DBClusterInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBClusterParameterGroupName", String.to_query f))
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
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupIds.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.database_name (fun f ->
             Aws.Query.Pair ("DatabaseName", String.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Some (Aws.Query.Pair ("Engine", String.to_query v.engine))
       ; Some
           (Aws.Query.Pair ("SnapshotIdentifier", String.to_query v.snapshot_identifier))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ("AvailabilityZones.member", AvailabilityZones.to_query v.availability_zones))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enable_internet_access_gateway (fun f ->
             "EnableInternetAccessGateway", Boolean.to_json f)
       ; Aws.Util.option_map v.enable_v_p_c_networking (fun f ->
             "EnableVPCNetworking", Boolean.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_retention_period (fun f ->
             "PerformanceInsightsRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.performance_insights_k_m_s_key_id (fun f ->
             "PerformanceInsightsKMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.enable_performance_insights (fun f ->
             "EnablePerformanceInsights", Boolean.to_json f)
       ; Aws.Util.option_map v.monitoring_role_arn (fun f ->
             "MonitoringRoleArn", String.to_json f)
       ; Aws.Util.option_map v.monitoring_interval (fun f ->
             "MonitoringInterval", Integer.to_json f)
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             "RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_json f)
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             ( "ServerlessV2ScalingConfiguration"
             , ServerlessV2ScalingConfiguration.to_json f ))
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_instance_class (fun f ->
             "DBClusterInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_parameter_group_name (fun f ->
             "DBClusterParameterGroupName", String.to_json f)
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
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some
           ("VpcSecurityGroupIds", VpcSecurityGroupIdList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.database_name (fun f -> "DatabaseName", String.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Some ("Engine", String.to_json v.engine)
       ; Some ("SnapshotIdentifier", String.to_json v.snapshot_identifier)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ; Some ("AvailabilityZones", AvailabilityZones.to_json v.availability_zones)
       ])

let of_json j =
  { availability_zones =
      AvailabilityZones.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; snapshot_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotIdentifier"))
  ; engine = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Engine"))
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; database_name = Aws.Util.option_map (Aws.Json.lookup j "DatabaseName") String.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; vpc_security_group_ids =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
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
  ; d_b_cluster_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterParameterGroupName") String.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; d_b_cluster_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterInstanceClass") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; serverless_v2_scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2ScalingConfiguration")
        ServerlessV2ScalingConfiguration.of_json
  ; rds_custom_cluster_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsCustomClusterConfiguration")
        RdsCustomClusterConfiguration.of_json
  ; monitoring_interval =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringInterval") Integer.of_json
  ; monitoring_role_arn =
      Aws.Util.option_map (Aws.Json.lookup j "MonitoringRoleArn") String.of_json
  ; enable_performance_insights =
      Aws.Util.option_map (Aws.Json.lookup j "EnablePerformanceInsights") Boolean.of_json
  ; performance_insights_k_m_s_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "PerformanceInsightsKMSKeyId") String.of_json
  ; performance_insights_retention_period =
      Aws.Util.option_map
        (Aws.Json.lookup j "PerformanceInsightsRetentionPeriod")
        Integer.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; enable_v_p_c_networking =
      Aws.Util.option_map (Aws.Json.lookup j "EnableVPCNetworking") Boolean.of_json
  ; enable_internet_access_gateway =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableInternetAccessGateway")
        Boolean.of_json
  }
