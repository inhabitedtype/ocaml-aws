open Aws.BaseTypes

type t =
  { engine : String.t option
  ; major_engine_version : String.t option
  ; engine_version : String.t option
  ; database_installation_files_s3_bucket_name : String.t option
  ; database_installation_files_s3_prefix : String.t option
  ; database_installation_files : StringList.t
  ; custom_d_b_engine_version_manifest : String.t option
  ; d_b_parameter_group_family : String.t option
  ; d_b_engine_description : String.t option
  ; d_b_engine_version_arn : String.t option
  ; d_b_engine_version_description : String.t option
  ; default_character_set : CharacterSet.t option
  ; failure_reason : String.t option
  ; image : CustomDBEngineVersionAMI.t option
  ; d_b_engine_media_type : String.t option
  ; k_m_s_key_id : String.t option
  ; create_time : DateTime.t option
  ; supported_character_sets : SupportedCharacterSetsList.t
  ; supported_nchar_character_sets : SupportedCharacterSetsList.t
  ; valid_upgrade_target : ValidUpgradeTargetList.t
  ; supported_timezones : SupportedTimezonesList.t
  ; exportable_log_types : LogTypeList.t
  ; supports_log_exports_to_cloudwatch_logs : Boolean.t option
  ; supports_read_replica : Boolean.t option
  ; supported_engine_modes : EngineModeList.t
  ; supported_feature_names : FeatureNameList.t
  ; status : String.t option
  ; supports_parallel_query : Boolean.t option
  ; supports_global_databases : Boolean.t option
  ; tag_list : TagList.t
  ; supports_babelfish : Boolean.t option
  ; supports_limitless_database : Boolean.t option
  ; supports_certificate_rotation_without_restart : Boolean.t option
  ; supported_c_a_certificate_identifiers : CACertificateIdentifiersList.t
  ; supports_local_write_forwarding : Boolean.t option
  ; supports_integrations : Boolean.t option
  ; serverless_v2_features_support : ServerlessV2FeaturesSupport.t option
  }

let make
    ?engine
    ?major_engine_version
    ?engine_version
    ?database_installation_files_s3_bucket_name
    ?database_installation_files_s3_prefix
    ?(database_installation_files = [])
    ?custom_d_b_engine_version_manifest
    ?d_b_parameter_group_family
    ?d_b_engine_description
    ?d_b_engine_version_arn
    ?d_b_engine_version_description
    ?default_character_set
    ?failure_reason
    ?image
    ?d_b_engine_media_type
    ?k_m_s_key_id
    ?create_time
    ?(supported_character_sets = [])
    ?(supported_nchar_character_sets = [])
    ?(valid_upgrade_target = [])
    ?(supported_timezones = [])
    ?(exportable_log_types = [])
    ?supports_log_exports_to_cloudwatch_logs
    ?supports_read_replica
    ?(supported_engine_modes = [])
    ?(supported_feature_names = [])
    ?status
    ?supports_parallel_query
    ?supports_global_databases
    ?(tag_list = [])
    ?supports_babelfish
    ?supports_limitless_database
    ?supports_certificate_rotation_without_restart
    ?(supported_c_a_certificate_identifiers = [])
    ?supports_local_write_forwarding
    ?supports_integrations
    ?serverless_v2_features_support
    () =
  { engine
  ; major_engine_version
  ; engine_version
  ; database_installation_files_s3_bucket_name
  ; database_installation_files_s3_prefix
  ; database_installation_files
  ; custom_d_b_engine_version_manifest
  ; d_b_parameter_group_family
  ; d_b_engine_description
  ; d_b_engine_version_arn
  ; d_b_engine_version_description
  ; default_character_set
  ; failure_reason
  ; image
  ; d_b_engine_media_type
  ; k_m_s_key_id
  ; create_time
  ; supported_character_sets
  ; supported_nchar_character_sets
  ; valid_upgrade_target
  ; supported_timezones
  ; exportable_log_types
  ; supports_log_exports_to_cloudwatch_logs
  ; supports_read_replica
  ; supported_engine_modes
  ; supported_feature_names
  ; status
  ; supports_parallel_query
  ; supports_global_databases
  ; tag_list
  ; supports_babelfish
  ; supports_limitless_database
  ; supports_certificate_rotation_without_restart
  ; supported_c_a_certificate_identifiers
  ; supports_local_write_forwarding
  ; supports_integrations
  ; serverless_v2_features_support
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; major_engine_version =
        Aws.Util.option_bind (Aws.Xml.member "MajorEngineVersion" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; database_installation_files_s3_bucket_name =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInstallationFilesS3BucketName" xml)
          String.parse
    ; database_installation_files_s3_prefix =
        Aws.Util.option_bind
          (Aws.Xml.member "DatabaseInstallationFilesS3Prefix" xml)
          String.parse
    ; database_installation_files =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DatabaseInstallationFiles" xml)
             StringList.parse)
    ; custom_d_b_engine_version_manifest =
        Aws.Util.option_bind
          (Aws.Xml.member "CustomDBEngineVersionManifest" xml)
          String.parse
    ; d_b_parameter_group_family =
        Aws.Util.option_bind (Aws.Xml.member "DBParameterGroupFamily" xml) String.parse
    ; d_b_engine_description =
        Aws.Util.option_bind (Aws.Xml.member "DBEngineDescription" xml) String.parse
    ; d_b_engine_version_arn =
        Aws.Util.option_bind (Aws.Xml.member "DBEngineVersionArn" xml) String.parse
    ; d_b_engine_version_description =
        Aws.Util.option_bind
          (Aws.Xml.member "DBEngineVersionDescription" xml)
          String.parse
    ; default_character_set =
        Aws.Util.option_bind (Aws.Xml.member "DefaultCharacterSet" xml) CharacterSet.parse
    ; failure_reason =
        Aws.Util.option_bind (Aws.Xml.member "FailureReason" xml) String.parse
    ; image =
        Aws.Util.option_bind (Aws.Xml.member "Image" xml) CustomDBEngineVersionAMI.parse
    ; d_b_engine_media_type =
        Aws.Util.option_bind (Aws.Xml.member "DBEngineMediaType" xml) String.parse
    ; k_m_s_key_id = Aws.Util.option_bind (Aws.Xml.member "KMSKeyId" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "CreateTime" xml) DateTime.parse
    ; supported_character_sets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedCharacterSets" xml)
             SupportedCharacterSetsList.parse)
    ; supported_nchar_character_sets =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedNcharCharacterSets" xml)
             SupportedCharacterSetsList.parse)
    ; valid_upgrade_target =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ValidUpgradeTarget" xml)
             ValidUpgradeTargetList.parse)
    ; supported_timezones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedTimezones" xml)
             SupportedTimezonesList.parse)
    ; exportable_log_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ExportableLogTypes" xml)
             LogTypeList.parse)
    ; supports_log_exports_to_cloudwatch_logs =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsLogExportsToCloudwatchLogs" xml)
          Boolean.parse
    ; supports_read_replica =
        Aws.Util.option_bind (Aws.Xml.member "SupportsReadReplica" xml) Boolean.parse
    ; supported_engine_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedEngineModes" xml)
             EngineModeList.parse)
    ; supported_feature_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedFeatureNames" xml)
             FeatureNameList.parse)
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; supports_parallel_query =
        Aws.Util.option_bind (Aws.Xml.member "SupportsParallelQuery" xml) Boolean.parse
    ; supports_global_databases =
        Aws.Util.option_bind (Aws.Xml.member "SupportsGlobalDatabases" xml) Boolean.parse
    ; tag_list =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "TagList" xml) TagList.parse)
    ; supports_babelfish =
        Aws.Util.option_bind (Aws.Xml.member "SupportsBabelfish" xml) Boolean.parse
    ; supports_limitless_database =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsLimitlessDatabase" xml)
          Boolean.parse
    ; supports_certificate_rotation_without_restart =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsCertificateRotationWithoutRestart" xml)
          Boolean.parse
    ; supported_c_a_certificate_identifiers =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedCACertificateIdentifiers" xml)
             CACertificateIdentifiersList.parse)
    ; supports_local_write_forwarding =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsLocalWriteForwarding" xml)
          Boolean.parse
    ; supports_integrations =
        Aws.Util.option_bind (Aws.Xml.member "SupportsIntegrations" xml) Boolean.parse
    ; serverless_v2_features_support =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessV2FeaturesSupport" xml)
          ServerlessV2FeaturesSupport.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_v2_features_support (fun f ->
             Aws.Query.Pair
               ("ServerlessV2FeaturesSupport", ServerlessV2FeaturesSupport.to_query f))
       ; Aws.Util.option_map v.supports_integrations (fun f ->
             Aws.Query.Pair ("SupportsIntegrations", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_local_write_forwarding (fun f ->
             Aws.Query.Pair ("SupportsLocalWriteForwarding", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedCACertificateIdentifiers.member"
              , CACertificateIdentifiersList.to_query
                  v.supported_c_a_certificate_identifiers ))
       ; Aws.Util.option_map v.supports_certificate_rotation_without_restart (fun f ->
             Aws.Query.Pair
               ("SupportsCertificateRotationWithoutRestart", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_limitless_database (fun f ->
             Aws.Query.Pair ("SupportsLimitlessDatabase", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_babelfish (fun f ->
             Aws.Query.Pair ("SupportsBabelfish", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TagList.member", TagList.to_query v.tag_list))
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             Aws.Query.Pair ("SupportsGlobalDatabases", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_parallel_query (fun f ->
             Aws.Query.Pair ("SupportsParallelQuery", Boolean.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedFeatureNames.member"
              , FeatureNameList.to_query v.supported_feature_names ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedEngineModes.member"
              , EngineModeList.to_query v.supported_engine_modes ))
       ; Aws.Util.option_map v.supports_read_replica (fun f ->
             Aws.Query.Pair ("SupportsReadReplica", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_log_exports_to_cloudwatch_logs (fun f ->
             Aws.Query.Pair ("SupportsLogExportsToCloudwatchLogs", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ExportableLogTypes.member", LogTypeList.to_query v.exportable_log_types))
       ; Some
           (Aws.Query.Pair
              ( "SupportedTimezones.member"
              , SupportedTimezonesList.to_query v.supported_timezones ))
       ; Some
           (Aws.Query.Pair
              ( "ValidUpgradeTarget.member"
              , ValidUpgradeTargetList.to_query v.valid_upgrade_target ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedNcharCharacterSets.member"
              , SupportedCharacterSetsList.to_query v.supported_nchar_character_sets ))
       ; Some
           (Aws.Query.Pair
              ( "SupportedCharacterSets.member"
              , SupportedCharacterSetsList.to_query v.supported_character_sets ))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.k_m_s_key_id (fun f ->
             Aws.Query.Pair ("KMSKeyId", String.to_query f))
       ; Aws.Util.option_map v.d_b_engine_media_type (fun f ->
             Aws.Query.Pair ("DBEngineMediaType", String.to_query f))
       ; Aws.Util.option_map v.image (fun f ->
             Aws.Query.Pair ("Image", CustomDBEngineVersionAMI.to_query f))
       ; Aws.Util.option_map v.failure_reason (fun f ->
             Aws.Query.Pair ("FailureReason", String.to_query f))
       ; Aws.Util.option_map v.default_character_set (fun f ->
             Aws.Query.Pair ("DefaultCharacterSet", CharacterSet.to_query f))
       ; Aws.Util.option_map v.d_b_engine_version_description (fun f ->
             Aws.Query.Pair ("DBEngineVersionDescription", String.to_query f))
       ; Aws.Util.option_map v.d_b_engine_version_arn (fun f ->
             Aws.Query.Pair ("DBEngineVersionArn", String.to_query f))
       ; Aws.Util.option_map v.d_b_engine_description (fun f ->
             Aws.Query.Pair ("DBEngineDescription", String.to_query f))
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             Aws.Query.Pair ("DBParameterGroupFamily", String.to_query f))
       ; Aws.Util.option_map v.custom_d_b_engine_version_manifest (fun f ->
             Aws.Query.Pair ("CustomDBEngineVersionManifest", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "DatabaseInstallationFiles.member"
              , StringList.to_query v.database_installation_files ))
       ; Aws.Util.option_map v.database_installation_files_s3_prefix (fun f ->
             Aws.Query.Pair ("DatabaseInstallationFilesS3Prefix", String.to_query f))
       ; Aws.Util.option_map v.database_installation_files_s3_bucket_name (fun f ->
             Aws.Query.Pair ("DatabaseInstallationFilesS3BucketName", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             Aws.Query.Pair ("MajorEngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_v2_features_support (fun f ->
             "ServerlessV2FeaturesSupport", ServerlessV2FeaturesSupport.to_json f)
       ; Aws.Util.option_map v.supports_integrations (fun f ->
             "SupportsIntegrations", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_local_write_forwarding (fun f ->
             "SupportsLocalWriteForwarding", Boolean.to_json f)
       ; Some
           ( "SupportedCACertificateIdentifiers"
           , CACertificateIdentifiersList.to_json v.supported_c_a_certificate_identifiers
           )
       ; Aws.Util.option_map v.supports_certificate_rotation_without_restart (fun f ->
             "SupportsCertificateRotationWithoutRestart", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_limitless_database (fun f ->
             "SupportsLimitlessDatabase", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_babelfish (fun f ->
             "SupportsBabelfish", Boolean.to_json f)
       ; Some ("TagList", TagList.to_json v.tag_list)
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             "SupportsGlobalDatabases", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_parallel_query (fun f ->
             "SupportsParallelQuery", Boolean.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Some ("SupportedFeatureNames", FeatureNameList.to_json v.supported_feature_names)
       ; Some ("SupportedEngineModes", EngineModeList.to_json v.supported_engine_modes)
       ; Aws.Util.option_map v.supports_read_replica (fun f ->
             "SupportsReadReplica", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_log_exports_to_cloudwatch_logs (fun f ->
             "SupportsLogExportsToCloudwatchLogs", Boolean.to_json f)
       ; Some ("ExportableLogTypes", LogTypeList.to_json v.exportable_log_types)
       ; Some ("SupportedTimezones", SupportedTimezonesList.to_json v.supported_timezones)
       ; Some ("ValidUpgradeTarget", ValidUpgradeTargetList.to_json v.valid_upgrade_target)
       ; Some
           ( "SupportedNcharCharacterSets"
           , SupportedCharacterSetsList.to_json v.supported_nchar_character_sets )
       ; Some
           ( "SupportedCharacterSets"
           , SupportedCharacterSetsList.to_json v.supported_character_sets )
       ; Aws.Util.option_map v.create_time (fun f -> "CreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.k_m_s_key_id (fun f -> "KMSKeyId", String.to_json f)
       ; Aws.Util.option_map v.d_b_engine_media_type (fun f ->
             "DBEngineMediaType", String.to_json f)
       ; Aws.Util.option_map v.image (fun f ->
             "Image", CustomDBEngineVersionAMI.to_json f)
       ; Aws.Util.option_map v.failure_reason (fun f -> "FailureReason", String.to_json f)
       ; Aws.Util.option_map v.default_character_set (fun f ->
             "DefaultCharacterSet", CharacterSet.to_json f)
       ; Aws.Util.option_map v.d_b_engine_version_description (fun f ->
             "DBEngineVersionDescription", String.to_json f)
       ; Aws.Util.option_map v.d_b_engine_version_arn (fun f ->
             "DBEngineVersionArn", String.to_json f)
       ; Aws.Util.option_map v.d_b_engine_description (fun f ->
             "DBEngineDescription", String.to_json f)
       ; Aws.Util.option_map v.d_b_parameter_group_family (fun f ->
             "DBParameterGroupFamily", String.to_json f)
       ; Aws.Util.option_map v.custom_d_b_engine_version_manifest (fun f ->
             "CustomDBEngineVersionManifest", String.to_json f)
       ; Some
           ("DatabaseInstallationFiles", StringList.to_json v.database_installation_files)
       ; Aws.Util.option_map v.database_installation_files_s3_prefix (fun f ->
             "DatabaseInstallationFilesS3Prefix", String.to_json f)
       ; Aws.Util.option_map v.database_installation_files_s3_bucket_name (fun f ->
             "DatabaseInstallationFilesS3BucketName", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.major_engine_version (fun f ->
             "MajorEngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; major_engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "MajorEngineVersion") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; database_installation_files_s3_bucket_name =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInstallationFilesS3BucketName")
        String.of_json
  ; database_installation_files_s3_prefix =
      Aws.Util.option_map
        (Aws.Json.lookup j "DatabaseInstallationFilesS3Prefix")
        String.of_json
  ; database_installation_files =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DatabaseInstallationFiles"))
  ; custom_d_b_engine_version_manifest =
      Aws.Util.option_map
        (Aws.Json.lookup j "CustomDBEngineVersionManifest")
        String.of_json
  ; d_b_parameter_group_family =
      Aws.Util.option_map (Aws.Json.lookup j "DBParameterGroupFamily") String.of_json
  ; d_b_engine_description =
      Aws.Util.option_map (Aws.Json.lookup j "DBEngineDescription") String.of_json
  ; d_b_engine_version_arn =
      Aws.Util.option_map (Aws.Json.lookup j "DBEngineVersionArn") String.of_json
  ; d_b_engine_version_description =
      Aws.Util.option_map (Aws.Json.lookup j "DBEngineVersionDescription") String.of_json
  ; default_character_set =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultCharacterSet") CharacterSet.of_json
  ; failure_reason =
      Aws.Util.option_map (Aws.Json.lookup j "FailureReason") String.of_json
  ; image =
      Aws.Util.option_map (Aws.Json.lookup j "Image") CustomDBEngineVersionAMI.of_json
  ; d_b_engine_media_type =
      Aws.Util.option_map (Aws.Json.lookup j "DBEngineMediaType") String.of_json
  ; k_m_s_key_id = Aws.Util.option_map (Aws.Json.lookup j "KMSKeyId") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "CreateTime") DateTime.of_json
  ; supported_character_sets =
      SupportedCharacterSetsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedCharacterSets"))
  ; supported_nchar_character_sets =
      SupportedCharacterSetsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedNcharCharacterSets"))
  ; valid_upgrade_target =
      ValidUpgradeTargetList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ValidUpgradeTarget"))
  ; supported_timezones =
      SupportedTimezonesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedTimezones"))
  ; exportable_log_types =
      LogTypeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ExportableLogTypes"))
  ; supports_log_exports_to_cloudwatch_logs =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsLogExportsToCloudwatchLogs")
        Boolean.of_json
  ; supports_read_replica =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsReadReplica") Boolean.of_json
  ; supported_engine_modes =
      EngineModeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedEngineModes"))
  ; supported_feature_names =
      FeatureNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedFeatureNames"))
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; supports_parallel_query =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsParallelQuery") Boolean.of_json
  ; supports_global_databases =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsGlobalDatabases") Boolean.of_json
  ; tag_list = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TagList"))
  ; supports_babelfish =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsBabelfish") Boolean.of_json
  ; supports_limitless_database =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsLimitlessDatabase") Boolean.of_json
  ; supports_certificate_rotation_without_restart =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsCertificateRotationWithoutRestart")
        Boolean.of_json
  ; supported_c_a_certificate_identifiers =
      CACertificateIdentifiersList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedCACertificateIdentifiers"))
  ; supports_local_write_forwarding =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsLocalWriteForwarding")
        Boolean.of_json
  ; supports_integrations =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsIntegrations") Boolean.of_json
  ; serverless_v2_features_support =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessV2FeaturesSupport")
        ServerlessV2FeaturesSupport.of_json
  }
