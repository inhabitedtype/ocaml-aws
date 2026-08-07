open Aws.BaseTypes

type t =
  { pending_cloudwatch_logs_exports : PendingCloudwatchLogsExports.t option
  ; d_b_cluster_identifier : String.t option
  ; master_user_password : String.t option
  ; i_a_m_database_authentication_enabled : Boolean.t option
  ; engine_version : String.t option
  ; backup_retention_period : Integer.t option
  ; storage_type : String.t option
  ; allocated_storage : Integer.t option
  ; rds_custom_cluster_configuration : RdsCustomClusterConfiguration.t option
  ; iops : Integer.t option
  ; certificate_details : CertificateDetails.t option
  }

let make
    ?pending_cloudwatch_logs_exports
    ?d_b_cluster_identifier
    ?master_user_password
    ?i_a_m_database_authentication_enabled
    ?engine_version
    ?backup_retention_period
    ?storage_type
    ?allocated_storage
    ?rds_custom_cluster_configuration
    ?iops
    ?certificate_details
    () =
  { pending_cloudwatch_logs_exports
  ; d_b_cluster_identifier
  ; master_user_password
  ; i_a_m_database_authentication_enabled
  ; engine_version
  ; backup_retention_period
  ; storage_type
  ; allocated_storage
  ; rds_custom_cluster_configuration
  ; iops
  ; certificate_details
  }

let parse xml =
  Some
    { pending_cloudwatch_logs_exports =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingCloudwatchLogsExports" xml)
          PendingCloudwatchLogsExports.parse
    ; d_b_cluster_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse
    ; master_user_password =
        Aws.Util.option_bind (Aws.Xml.member "MasterUserPassword" xml) String.parse
    ; i_a_m_database_authentication_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "IAMDatabaseAuthenticationEnabled" xml)
          Boolean.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; backup_retention_period =
        Aws.Util.option_bind (Aws.Xml.member "BackupRetentionPeriod" xml) Integer.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; allocated_storage =
        Aws.Util.option_bind (Aws.Xml.member "AllocatedStorage" xml) Integer.parse
    ; rds_custom_cluster_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "RdsCustomClusterConfiguration" xml)
          RdsCustomClusterConfiguration.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; certificate_details =
        Aws.Util.option_bind
          (Aws.Xml.member "CertificateDetails" xml)
          CertificateDetails.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate_details (fun f ->
             Aws.Query.Pair ("CertificateDetails", CertificateDetails.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             Aws.Query.Pair
               ("RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_query f))
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             Aws.Query.Pair ("AllocatedStorage", Integer.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             Aws.Query.Pair ("BackupRetentionPeriod", Integer.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             Aws.Query.Pair ("IAMDatabaseAuthenticationEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.master_user_password (fun f ->
             Aws.Query.Pair ("MasterUserPassword", String.to_query f))
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             Aws.Query.Pair ("DBClusterIdentifier", String.to_query f))
       ; Aws.Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
             Aws.Query.Pair
               ("PendingCloudwatchLogsExports", PendingCloudwatchLogsExports.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.certificate_details (fun f ->
             "CertificateDetails", CertificateDetails.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.rds_custom_cluster_configuration (fun f ->
             "RdsCustomClusterConfiguration", RdsCustomClusterConfiguration.to_json f)
       ; Aws.Util.option_map v.allocated_storage (fun f ->
             "AllocatedStorage", Integer.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.backup_retention_period (fun f ->
             "BackupRetentionPeriod", Integer.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.i_a_m_database_authentication_enabled (fun f ->
             "IAMDatabaseAuthenticationEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.master_user_password (fun f ->
             "MasterUserPassword", String.to_json f)
       ; Aws.Util.option_map v.d_b_cluster_identifier (fun f ->
             "DBClusterIdentifier", String.to_json f)
       ; Aws.Util.option_map v.pending_cloudwatch_logs_exports (fun f ->
             "PendingCloudwatchLogsExports", PendingCloudwatchLogsExports.to_json f)
       ])

let of_json j =
  { pending_cloudwatch_logs_exports =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingCloudwatchLogsExports")
        PendingCloudwatchLogsExports.of_json
  ; d_b_cluster_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterIdentifier") String.of_json
  ; master_user_password =
      Aws.Util.option_map (Aws.Json.lookup j "MasterUserPassword") String.of_json
  ; i_a_m_database_authentication_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "IAMDatabaseAuthenticationEnabled")
        Boolean.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; backup_retention_period =
      Aws.Util.option_map (Aws.Json.lookup j "BackupRetentionPeriod") Integer.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; allocated_storage =
      Aws.Util.option_map (Aws.Json.lookup j "AllocatedStorage") Integer.of_json
  ; rds_custom_cluster_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "RdsCustomClusterConfiguration")
        RdsCustomClusterConfiguration.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; certificate_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "CertificateDetails")
        CertificateDetails.of_json
  }
