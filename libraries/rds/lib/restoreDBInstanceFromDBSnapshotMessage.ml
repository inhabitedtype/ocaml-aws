open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; d_b_snapshot_identifier : String.t option
  ; d_b_instance_class : String.t option
  ; port : Integer.t option
  ; availability_zone : String.t option
  ; d_b_subnet_group_name : String.t option
  ; multi_a_z : Boolean.t option
  ; publicly_accessible : Boolean.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; license_model : String.t option
  ; d_b_name : String.t option
  ; engine : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; option_group_name : String.t option
  ; tags : TagList.t
  ; storage_type : String.t option
  ; tde_credential_arn : String.t option
  ; tde_credential_password : String.t option
  ; vpc_security_group_ids : VpcSecurityGroupIdList.t
  ; domain : String.t option
  ; domain_fqdn : String.t option
  ; domain_ou : String.t option
  ; domain_auth_secret_arn : String.t option
  ; domain_dns_ips : StringList.t
  ; copy_tags_to_snapshot : Boolean.t option
  ; domain_i_a_m_role_name : String.t option
  ; enable_i_a_m_database_authentication : Boolean.t option
  ; enable_cloudwatch_logs_exports : LogTypeList.t
  ; processor_features : ProcessorFeatureList.t
  ; use_default_processor_features : Boolean.t option
  ; d_b_parameter_group_name : String.t option
  ; deletion_protection : Boolean.t option
  ; enable_customer_owned_ip : Boolean.t option
  ; network_type : String.t option
  ; backup_target : String.t option
  ; custom_iam_instance_profile : String.t option
  ; allocated_storage : Integer.t option
  ; d_b_cluster_snapshot_identifier : String.t option
  ; backup_retention_period : Integer.t option
  ; preferred_backup_window : String.t option
  ; dedicated_log_volume : Boolean.t option
  ; c_a_certificate_identifier : String.t option
  ; engine_lifecycle_support : String.t option
  ; additional_storage_volumes : AdditionalStorageVolumesList.t
  ; tag_specifications : TagSpecificationList.t
  ; manage_master_user_password : Boolean.t option
  ; master_user_secret_kms_key_id : String.t option
  }

let make
    ~d_b_instance_identifier
    ?d_b_snapshot_identifier
    ?d_b_instance_class
    ?port
    ?availability_zone
    ?d_b_subnet_group_name
    ?multi_a_z
    ?publicly_accessible
    ?auto_minor_version_upgrade
    ?license_model
    ?d_b_name
    ?engine
    ?iops
    ?storage_throughput
    ?option_group_name
    ?(tags = [])
    ?storage_type
    ?tde_credential_arn
    ?tde_credential_password
    ?(vpc_security_group_ids = [])
    ?domain
    ?domain_fqdn
    ?domain_ou
    ?domain_auth_secret_arn
    ?(domain_dns_ips = [])
    ?copy_tags_to_snapshot
    ?domain_i_a_m_role_name
    ?enable_i_a_m_database_authentication
    ?(enable_cloudwatch_logs_exports = [])
    ?(processor_features = [])
    ?use_default_processor_features
    ?d_b_parameter_group_name
    ?deletion_protection
    ?enable_customer_owned_ip
    ?network_type
    ?backup_target
    ?custom_iam_instance_profile
    ?allocated_storage
    ?d_b_cluster_snapshot_identifier
    ?backup_retention_period
    ?preferred_backup_window
    ?dedicated_log_volume
    ?c_a_certificate_identifier
    ?engine_lifecycle_support
    ?(additional_storage_volumes = [])
    ?(tag_specifications = [])
    ?manage_master_user_password
    ?master_user_secret_kms_key_id
    () =
  { d_b_instance_identifier
  ; d_b_snapshot_identifier
  ; d_b_instance_class
  ; port
  ; availability_zone
  ; d_b_subnet_group_name
  ; multi_a_z
  ; publicly_accessible
  ; auto_minor_version_upgrade
  ; license_model
  ; d_b_name
  ; engine
  ; iops
  ; storage_throughput
  ; option_group_name
  ; tags
  ; storage_type
  ; tde_credential_arn
  ; tde_credential_password
  ; vpc_security_group_ids
  ; domain
  ; domain_fqdn
  ; domain_ou
  ; domain_auth_secret_arn
  ; domain_dns_ips
  ; copy_tags_to_snapshot
  ; domain_i_a_m_role_name
  ; enable_i_a_m_database_authentication
  ; enable_cloudwatch_logs_exports
  ; processor_features
  ; use_default_processor_features
  ; d_b_parameter_group_name
  ; deletion_protection
  ; enable_customer_owned_ip
  ; network_type
  ; backup_target
  ; custom_iam_instance_profile
  ; allocated_storage
  ; d_b_cluster_snapshot_identifier
  ; backup_retention_period
  ; preferred_backup_window
  ; dedicated_log_volume
  ; c_a_certificate_identifier
  ; engine_lifecycle_support
  ; additional_storage_volumes
  ; tag_specifications
  ; manage_master_user_password
  ; master_user_secret_kms_key_id
  }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; publicly_accessible =
        Aws.Util.option_bind (Aws.Xml.member "PubliclyAccessible" xml) Boolean.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; d_b_name = Aws.Util.option_bind (Aws.Xml.member "DBName" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; tde_credential_arn =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialArn" xml) String.parse
    ; tde_credential_password =
        Aws.Util.option_bind (Aws.Xml.member "TdeCredentialPassword" xml) String.parse
    ; vpc_security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "VpcSecurityGroupIds" xml)
             VpcSecurityGroupIdList.parse)
    ; domain = Aws.Util.option_bind (Aws.Xml.member "Domain" xml) String.parse
    ; domain_fqdn = Aws.Util.option_bind (Aws.Xml.member "DomainFqdn" xml) String.parse
    ; domain_ou = Aws.Util.option_bind (Aws.Xml.member "DomainOu" xml) String.parse
    ; domain_auth_secret_arn =
        Aws.Util.option_bind (Aws.Xml.member "DomainAuthSecretArn" xml) String.parse
    ; domain_dns_ips =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "DomainDnsIps" xml) StringList.parse)
    ; copy_tags_to_snapshot =
        Aws.Util.option_bind (Aws.Xml.member "CopyTagsToSnapshot" xml) Boolean.parse
    ; domain_i_a_m_role_name =
        Aws.Util.option_bind (Aws.Xml.member "DomainIAMRoleName" xml) String.parse
    ; enable_i_a_m_database_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "EnableIAMDatabaseAuthentication" xml)
          Boolean.parse
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
    ; d_b_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupName" xml) String.parse
    ; deletion_protection =
        Aws.Util.option_bind (Aws.Xml.member "DeletionProtection" xml) Boolean.parse
    ; enable_customer_owned_ip =
        Aws.Util.option_bind (Aws.Xml.member "EnableCustomerOwnedIp" xml) Boolean.parse
    ; network_type = Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) String.parse
    ; backup_target =
        Aws.Util.option_bind (Aws.Xml.member "BackupTarget" xml) String.parse
    ; custom_iam_instance_profile =
        Aws.Util.option_bind (Aws.Xml.member "CustomIamInstanceProfile" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; d_b_cluster_snapshot_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
          String.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; preferred_backup_window =
        Aws.Util.option_bind (Aws.Xml.member "PreferredBackupWindow" xml) String.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; engine_lifecycle_support =
        Aws.Util.option_bind (Aws.Xml.member "EngineLifecycleSupport" xml) String.parse
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
    ; manage_master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "ManageMasterUserPassword" xml) Boolean.parse
    ; master_user_secret_kms_key_id =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserSecretKmsKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             Aws.Query.Pair ("MasterUserSecretKmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             Aws.Query.Pair ("ManageMasterUserPassword", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TagSpecifications.member"
              , TagSpecificationList.to_query v.tag_specifications ))
       ; Some
           (Aws.Query.Pair
              ( "AdditionalStorageVolumes.member"
              , AdditionalStorageVolumesList.to_query v.additional_storage_volumes ))
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             Aws.Query.Pair ("EngineLifecycleSupport", String.to_query f))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             Aws.Query.Pair ("PreferredBackupWindow", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             Aws.Query.Pair ("CustomIamInstanceProfile", String.to_query f))
       ; Aws.Util.option_map v.backup_target (fun f ->
             Aws.Query.Pair ("BackupTarget", String.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", String.to_query f))
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             Aws.Query.Pair ("EnableCustomerOwnedIp", Boolean.to_query f))
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             Aws.Query.Pair ("DeletionProtection", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             Aws.Query.Pair ("DBParameterGroupName", String.to_query f))
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
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             Aws.Query.Pair ("EnableIAMDatabaseAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             Aws.Query.Pair ("DomainIAMRoleName", String.to_query f))
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             Aws.Query.Pair ("CopyTagsToSnapshot", Boolean.to_query f))
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
       ; Some
           (Aws.Query.Pair
              ( "VpcSecurityGroupIds.member"
              , VpcSecurityGroupIdList.to_query v.vpc_security_group_ids ))
       ; Aws.Util.option_map v.tde_credential_password (fun f ->
             Aws.Query.Pair ("TdeCredentialPassword", String.to_query f))
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             Aws.Query.Pair ("TdeCredentialArn", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.d_b_name (fun f ->
             Aws.Query.Pair ("DBName", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             Aws.Query.Pair ("PubliclyAccessible", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.master_user_secret_kms_key_id (fun f ->
             "MasterUserSecretKmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.manage_master_user_password (fun f ->
             "ManageMasterUserPassword", Boolean.to_json f)
       ; Some ("TagSpecifications", TagSpecificationList.to_json v.tag_specifications)
       ; Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.engine_lifecycle_support (fun f ->
             "EngineLifecycleSupport", String.to_json f)
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.preferred_backup_window (fun f ->
             "PreferredBackupWindow", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             "DBClusterSnapshotIdentifier", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.custom_iam_instance_profile (fun f ->
             "CustomIamInstanceProfile", String.to_json f)
       ; Aws.Util.option_map v.backup_target (fun f -> "BackupTarget", String.to_json f)
       ; Aws.Util.option_map v.network_type (fun f -> "NetworkType", String.to_json f)
       ; Aws.Util.option_map v.enable_customer_owned_ip (fun f ->
             "EnableCustomerOwnedIp", Boolean.to_json f)
       ; Aws.Util.option_map v.deletion_protection (fun f ->
             "DeletionProtection", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_name (fun f ->
             "DBParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.use_default_processor_features (fun f ->
             "UseDefaultProcessorFeatures", Boolean.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Some
           ( "EnableCloudwatchLogsExports"
           , LogTypeList.to_json v.enable_cloudwatch_logs_exports )
       ; Aws.Util.option_map v.enable_i_a_m_database_authentication (fun f ->
             "EnableIAMDatabaseAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.domain_i_a_m_role_name (fun f ->
             "DomainIAMRoleName", String.to_json f)
       ; Aws.Util.option_map v.copy_tags_to_snapshot (fun f ->
             "CopyTagsToSnapshot", Boolean.to_json f)
       ; Some ("DomainDnsIps", StringList.to_json v.domain_dns_ips)
       ; Aws.Util.option_map v.domain_auth_secret_arn (fun f ->
             "DomainAuthSecretArn", String.to_json f)
       ; Aws.Util.option_map v.domain_ou (fun f -> "DomainOu", String.to_json f)
       ; Aws.Util.option_map v.domain_fqdn (fun f -> "DomainFqdn", String.to_json f)
       ; Aws.Util.option_map v.domain (fun f -> "Domain", String.to_json f)
       ; Some
           ("VpcSecurityGroupIds", VpcSecurityGroupIdList.to_json v.vpc_security_group_ids)
       ; Aws.Util.option_map v.tde_credential_password (fun f ->
             "TdeCredentialPassword", String.to_json f)
       ; Aws.Util.option_map v.tde_credential_arn (fun f ->
             "TdeCredentialArn", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.d_b_name (fun f -> "DBName", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.publicly_accessible (fun f ->
             "PubliclyAccessible", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; publicly_accessible =
      Aws.Util.option_map (Aws.Json.lookup j "PubliclyAccessible") Boolean.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; d_b_name = Aws.Util.option_map (Aws.Json.lookup j "DBName") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; tde_credential_arn =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialArn") String.of_json
  ; tde_credential_password =
      Aws.Util.option_map (Aws.Json.lookup j "TdeCredentialPassword") String.of_json
  ; vpc_security_group_ids =
      VpcSecurityGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "VpcSecurityGroupIds"))
  ; domain = Aws.Util.option_map (Aws.Json.lookup j "Domain") String.of_json
  ; domain_fqdn = Aws.Util.option_map (Aws.Json.lookup j "DomainFqdn") String.of_json
  ; domain_ou = Aws.Util.option_map (Aws.Json.lookup j "DomainOu") String.of_json
  ; domain_auth_secret_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DomainAuthSecretArn") String.of_json
  ; domain_dns_ips =
      StringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DomainDnsIps"))
  ; copy_tags_to_snapshot =
      Aws.Util.option_map (Aws.Json.lookup j "CopyTagsToSnapshot") Boolean.of_json
  ; domain_i_a_m_role_name =
      Aws.Util.option_map (Aws.Json.lookup j "DomainIAMRoleName") String.of_json
  ; enable_i_a_m_database_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "EnableIAMDatabaseAuthentication")
        Boolean.of_json
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
  ; d_b_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupName") String.of_json
  ; deletion_protection =
      Aws.Util.option_map (Aws.Json.lookup j "DeletionProtection") Boolean.of_json
  ; enable_customer_owned_ip =
      Aws.Util.option_map (Aws.Json.lookup j "EnableCustomerOwnedIp") Boolean.of_json
  ; network_type = Aws.Util.option_map (Aws.Json.lookup j "NetworkType") String.of_json
  ; backup_target = Aws.Util.option_map (Aws.Json.lookup j "BackupTarget") String.of_json
  ; custom_iam_instance_profile =
      Aws.Util.option_map (Aws.Json.lookup j "CustomIamInstanceProfile") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; d_b_cluster_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterSnapshotIdentifier") String.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; preferred_backup_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredBackupWindow") String.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; engine_lifecycle_support =
      Aws.Util.option_map (Aws.Json.lookup j "EngineLifecycleSupport") String.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecifications"))
  ; manage_master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "ManageMasterUserPassword") Boolean.of_json
  ; master_user_secret_kms_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserSecretKmsKeyId") String.of_json
  }
