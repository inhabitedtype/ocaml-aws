open Aws.BaseTypes

type t =
  { d_b_instance_class : String.t option
  ; allocated_storage : Integer.t option
  ; master_user_password : String.t option
  ; port : Integer.t option
  ; backup_retention_period : Integer.t option
  ; multi_a_z : Boolean.t option
  ; engine_version : String.t option
  ; license_model : String.t option
  ; iops : Integer.t option
  ; storage_throughput : Integer.t option
  ; d_b_instance_identifier : String.t option
  ; storage_type : String.t option
  ; c_a_certificate_identifier : String.t option
  ; d_b_subnet_group_name : String.t option
  ; pending_cloudwatch_logs_exports : PendingCloudwatchLogsExports.t option
  ; processor_features : ProcessorFeatureList.t
  ; automation_mode : AutomationMode.t option
  ; resume_full_automation_mode_time : DateTime.t option
  ; multi_tenant : Boolean.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; dedicated_log_volume : Boolean.t option
  ; engine : String.t option
  ; additional_storage_volumes : AdditionalStorageVolumesList.t
  }

let make
    ?d_b_instance_class
    ?allocated_storage
    ?master_user_password
    ?port
    ?backup_retention_period
    ?multi_a_z
    ?engine_version
    ?license_model
    ?iops
    ?storage_throughput
    ?d_b_instance_identifier
    ?storage_type
    ?c_a_certificate_identifier
    ?d_b_subnet_group_name
    ?pending_cloudwatch_logs_exports
    ?(processor_features = [])
    ?automation_mode
    ?resume_full_automation_mode_time
    ?multi_tenant
    ?i_a_m_database_authentication_enabled
    ?dedicated_log_volume
    ?engine
    ?(additional_storage_volumes = [])
    () =
  { d_b_instance_class
  ; allocated_storage
  ; master_user_password
  ; port
  ; backup_retention_period
  ; multi_a_z
  ; engine_version
  ; license_model
  ; iops
  ; storage_throughput
  ; d_b_instance_identifier
  ; storage_type
  ; c_a_certificate_identifier
  ; d_b_subnet_group_name
  ; pending_cloudwatch_logs_exports
  ; processor_features
  ; automation_mode
  ; resume_full_automation_mode_time
  ; multi_tenant
  ; i_a_m_database_authentication_enabled
  ; dedicated_log_volume
  ; engine
  ; additional_storage_volumes
  }

let parse xml =
  Some
    { d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) Boolean.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; storage_throughput =
        Aws.Util.option_bind (Aws.Xml.member "StorageThroughput" xml) Integer.parse
    ; d_b_instance_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; c_a_certificate_identifier =
        Aws.Util.option_bind (Aws.Xml.member "CACertificateIdentifier" xml) String.parse
    ; d_b_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "DBSubnetGroupName" xml) String.parse
    ; pending_cloudwatch_logs_exports =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingCloudwatchLogsExports" xml)
          PendingCloudwatchLogsExports.parse
    ; processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ProcessorFeatures" xml)
             ProcessorFeatureList.parse)
    ; automation_mode =
        Aws.Util.option_bind (Aws.Xml.member "AutomationMode" xml) AutomationMode.parse
    ; resume_full_automation_mode_time =
        Aws.Util.option_bind
          (Aws.Xml.member "ResumeFullAutomationModeTime" xml)
          DateTime.parse
    ; multi_tenant = Aws.Util.option_bind (Aws.Xml.member "MultiTenant" xml) Boolean.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; dedicated_log_volume =
        Aws.Util.option_bind (Aws.Xml.member "DedicatedLogVolume" xml) Boolean.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
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
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             Aws.Query.Pair ("DedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_tenant (fun f ->
             Aws.Query.Pair ("MultiTenant", Boolean.to_query f))
       ; Aws.Util.option_map v.resume_full_automation_mode_time (fun f ->
             Aws.Query.Pair ("ResumeFullAutomationModeTime", DateTime.to_query f))
       ; Aws.Util.option_map v.automation_mode (fun f ->
             Aws.Query.Pair ("AutomationMode", AutomationMode.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ProcessorFeatures.member"
              , ProcessorFeatureList.to_query v.processor_features ))
       ; Aws.Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
             Aws.Query.Pair
               ("PendingCloudwatchLogsExports", PendingCloudwatchLogsExports.to_query f))
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             Aws.Query.Pair ("DBSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             Aws.Query.Pair ("CACertificateIdentifier", String.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             Aws.Query.Pair ("DBInstanceIdentifier", String.to_query f))
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             Aws.Query.Pair ("StorageThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", Boolean.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AdditionalStorageVolumes"
           , AdditionalStorageVolumesList.to_json v.additional_storage_volumes )
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.dedicated_log_volume (fun f ->
             "DedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_tenant (fun f -> "MultiTenant", Boolean.to_json f)
       ; Aws.Util.option_map v.resume_full_automation_mode_time (fun f ->
             "ResumeFullAutomationModeTime", DateTime.to_json f)
       ; Aws.Util.option_map v.automation_mode (fun f ->
             "AutomationMode", AutomationMode.to_json f)
       ; Some ("ProcessorFeatures", ProcessorFeatureList.to_json v.processor_features)
       ; Aws.Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
             "PendingCloudwatchLogsExports", PendingCloudwatchLogsExports.to_json f)
       ; Aws.Util.option_map v.d_b_subnet_group_name (fun f ->
             "DBSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.c_a_certificate_identifier (fun f ->
             "CACertificateIdentifier", String.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_identifier (fun f ->
             "DBInstanceIdentifier", String.to_json f)
       ; Aws.Util.option_map v.storage_throughput (fun f ->
             "StorageThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", Boolean.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ])

let of_json j =
  { d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") Boolean.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "StorageThroughput") Integer.of_json
  ; d_b_instance_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceIdentifier") String.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; c_a_certificate_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "CACertificateIdentifier") String.of_json
  ; d_b_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "DBSubnetGroupName") String.of_json
  ; pending_cloudwatch_logs_exports =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingCloudwatchLogsExports")
        PendingCloudwatchLogsExports.of_json
  ; processor_features =
      ProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ProcessorFeatures"))
  ; automation_mode =
      Aws.Util.option_map (Aws.Json.lookup j "AutomationMode") AutomationMode.of_json
  ; resume_full_automation_mode_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "ResumeFullAutomationModeTime")
        DateTime.of_json
  ; multi_tenant = Aws.Util.option_map (Aws.Json.lookup j "MultiTenant") Boolean.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "DedicatedLogVolume") Boolean.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; additional_storage_volumes =
      AdditionalStorageVolumesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AdditionalStorageVolumes"))
  }
