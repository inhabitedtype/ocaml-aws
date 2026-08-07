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
  ; master_username : String.t
  ; master_user_password : String.t option
  ; option_group_name : String.t option
  ; preferred_backup_window : String.t option
  ; preferred_maintenance_window : String.t option
  ; tags : TagList.t
  ; storage_encrypted : Boolean.t option
  ; kms_key_id : String.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; source_engine : String.t
  ; source_engine_version : String.t
  ; s3_bucket_name : String.t
  ; s3_prefix : String.t option
  ; s3_ingestion_role_arn : String.t
  ; backtrack_window : Long.t option
  ; enable_cloudwatch_logs_exports : LogTypeList.t
  ; deletion_protection : Boolean.t option
  ; copy_tags_to_snapshot : Boolean.t option
  ; domain : String.t option
  ; domain_i_a_m_role_name : String.t option
  ; storage_type : String.t option
  ; network_type : String.t option
  ; serverless_v2_scaling_configuration : ServerlessV2ScalingConfiguration.t option
  ; manage_master_user_password : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  ; engine_lifecycle_support : String.t option
  ; tag_specifications : TagSpecificationList.t
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
    ~master_username
    ?master_user_password
    ?option_group_name
    ?preferred_backup_window
    ?preferred_maintenance_window
    ?(tags = [])
    ?storage_encrypted
    ?kms_key_id
    ?enable_i_a_m_database_authentication
    ~source_engine
    ~source_engine_version
    ~s3_bucket_name
    ?s3_prefix
    ~s3_ingestion_role_arn
    ?backtrack_window
    ?(enable_cloudwatch_logs_exports = [])
    ?deletion_protection
    ?copy_tags_to_snapshot
    ?domain
    ?domain_i_a_m_role_name
    ?storage_type
    ?network_type
    ?serverless_v2_scaling_configuration
    ?manage_master_user_password
    ?master_user_secret_kms_key_id
    ?engine_lifecycle_support
    ?(tag_specifications = [])
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
  ; tags
  ; storage_encrypted
  ; kms_key_id
  ; enable_i_a_m_database_authentication
  ; source_engine
  ; source_engine_version
  ; s3_bucket_name
  ; s3_prefix
  ; s3_ingestion_role_arn
  ; backtrack_window
  ; enable_cloudwatch_logs_exports
  ; deletion_protection
  ; copy_tags_to_snapshot
  ; domain
  ; domain_i_a_m_role_name
  ; storage_type
  ; network_type
  ; serverless_v2_scaling_configuration
  ; manage_master_user_password
  ; master_user_secret_kms_key_id
  ; engine_lifecycle_support
  ; tag_specifications
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
        Aws.Xml.required
          "MasterUsername"
          (Aws.Util.option_bind (Aws.Xml.member "MasterUsername" xml) String.parse)
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
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; storage_encrypted =
        Aws.Util.option_bind (Aws.Xml.member "StorageEncrypted" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; enable_i_a_m_database_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableIAMDatabaseAuthentication" xml)
          Boolean.parse
    ; source_engine =
        Aws.Xml.required
          "SourceEngine"
          (Aws.Util.option_bind (Aws.Xml.member "SourceEngine" xml) String.parse)
    ; source_engine_version =
        Aws.Xml.required
          "SourceEngineVersion"
          (Aws.Util.option_bind (Aws.Xml.member "SourceEngineVersion" xml) String.parse)
    ; s3_bucket_name =
        Aws.Xml.required
          "S3BucketName"
          (Aws.Util.option_bind (Aws.Xml.member "S3BucketName" xml) String.parse)
    ; s3_prefix = Aws.Util.option_bind (Aws.Xml.member "S3Prefix" xml) String.parse
    ; s3_ingestion_role_arn =
        Aws.Xml.required
          "S3IngestionRoleArn"
          (Aws.Util.option_bind (Aws.Xml.member "S3IngestionRoleArn" xml) String.parse)
    ; backtrack_window =
        Aws.Util.option_bind (Aws.Xml.member "BacktrackWindow" xml) Long.parse
    ; enable_cloudwatch_logs_exports =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "EnableCloudwatchLogsExports" xml)
             LogTypeList.parse)
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; serverless_v2_scaling_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2ScalingConfiguration" xml)
          ServerlessV2ScalingConfiguration.parse
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
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
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             Aws.Query.Pair
               ( "ServerlessV2ScalingConfiguration"
               , ServerlessV2ScalingConfiguration.to_query f ))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.domain (fun f ->
             Aws.Query.Pair ("Domain", String.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "EnableCloudwatchLogsExports.member"
              , LogTypeList.to_query v.enable_cloudwatch_logs_exports ))
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             Aws.Query.Pair ("BacktrackWindow", Long.to_query f))
       ; Some
           (Aws.Query.Pair ("S3IngestionRoleArn", String.to_query v.s3_ingestion_role_arn))
       ; Aws.Util.option_map v.s3_prefix (fun f ->
             Aws.Query.Pair ("S3Prefix", String.to_query f))
       ; Some (Aws.Query.Pair ("S3BucketName", String.to_query v.s3_bucket_name))
       ; Some
           (Aws.Query.Pair ("SourceEngineVersion", String.to_query v.source_engine_version))
       ; Some (Aws.Query.Pair ("SourceEngine", String.to_query v.source_engine))
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             Aws.Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             Aws.Query.Pair ("StorageEncrypted", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Some (Aws.Query.Pair ("MasterUsername", String.to_query v.master_username))
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
       [ Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Aws.Util.option_map v.serverless_v2_scaling_configuration (fun f ->
             ( "ServerlessV2ScalingConfiguration"
             , ServerlessV2ScalingConfiguration.to_json f ))
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Some
           ( "EnableCloudwatchLogsExports"
           , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
       ; Aws.Util.option_map v.backtrack_window (fun f ->
             "BacktrackWindow", Long.to_json f)
       ; Some ("S3IngestionRoleArn", String.to_json v.s3_ingestion_role_arn)
       ; Aws.Util.option_map v.s3_prefix (fun f -> "S3Prefix", String.to_json f)
       ; Some ("S3BucketName", String.to_json v.s3_bucket_name)
       ; Some ("SourceEngineVersion", String.to_json v.source_engine_version)
       ; Some ("SourceEngine", String.to_json v.source_engine)
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             "EnableIAMDatabaseAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.storage_encrypted (fun f ->
             "StorageEncrypted", Boolean.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Some ("MasterUsername", String.to_json v.master_username)
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
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MasterUsername"))
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; storage_encrypted =
      Aws.Util.option_map (Aws.Json.lookup j "StorageEncrypted") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; enable_i_a_m_database_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableIAMDatabaseAuthentication")
        Boolean.of_json
  ; source_engine =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceEngine"))
  ; source_engine_version =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceEngineVersion"))
  ; s3_bucket_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3BucketName"))
  ; s3_prefix = Aws.Util.option_map (Aws.Json.lookup j "S3Prefix") String.of_json
  ; s3_ingestion_role_arn =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "S3IngestionRoleArn"))
  ; backtrack_window =
      Aws.Util.option_map (Aws.Json.lookup j "BacktrackWindow") Long.of_json
  ; enable_cloudwatch_logs_exports =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "EnableCloudwatchLogsExports"))
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; serverless_v2_scaling_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2ScalingConfiguration")
        ServerlessV2ScalingConfiguration.of_json
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  }
