open Aws.BaseTypes

type t =
  { engine : String.t option
  ; engine_version : String.t option
  ; d_b_instance_class : String.t option
  ; license_model : String.t option
  ; availability_zone_group : String.t option
  ; availability_zones : AvailabilityZoneList.t
  ; multi_a_z_capable : Boolean.t option
  ; read_replica_capable : Boolean.t option
  ; vpc : Boolean.t option
  ; supports_storage_encryption : Boolean.t option
  ; storage_type : String.t option
  ; supports_iops : Boolean.t option
  ; supports_storage_throughput : Boolean.t option
  ; supports_enhanced_monitoring : Boolean.t option
  ; supports_i_a_m_database_authentication : Boolean.t option
  ; supports_performance_insights : Boolean.t option
  ; min_storage_size : Integer.t option
  ; max_storage_size : Integer.t option
  ; min_iops_per_db_instance : Integer.t option
  ; max_iops_per_db_instance : Integer.t option
  ; min_iops_per_gib : Double.t option
  ; max_iops_per_gib : Double.t option
  ; min_storage_throughput_per_db_instance : Integer.t option
  ; max_storage_throughput_per_db_instance : Integer.t option
  ; min_storage_throughput_per_iops : Double.t option
  ; max_storage_throughput_per_iops : Double.t option
  ; available_processor_features : AvailableProcessorFeatureList.t
  ; supported_engine_modes : EngineModeList.t
  ; supports_storage_autoscaling : Boolean.t option
  ; supports_kerberos_authentication : Boolean.t option
  ; outpost_capable : Boolean.t option
  ; supported_activity_stream_modes : ActivityStreamModeList.t
  ; supports_global_databases : Boolean.t option
  ; supported_network_types : StringList.t
  ; supports_clusters : Boolean.t option
  ; supports_dedicated_log_volume : Boolean.t option
  ; supports_additional_storage_volumes : Boolean.t option
  ; supports_http_endpoint : Boolean.t option
  ; available_additional_storage_volumes_options :
      AvailableAdditionalStorageVolumesOptionList.t
  }

let make
    ?engine
    ?engine_version
    ?d_b_instance_class
    ?license_model
    ?availability_zone_group
    ?(availability_zones = [])
    ?multi_a_z_capable
    ?read_replica_capable
    ?vpc
    ?supports_storage_encryption
    ?storage_type
    ?supports_iops
    ?supports_storage_throughput
    ?supports_enhanced_monitoring
    ?supports_i_a_m_database_authentication
    ?supports_performance_insights
    ?min_storage_size
    ?max_storage_size
    ?min_iops_per_db_instance
    ?max_iops_per_db_instance
    ?min_iops_per_gib
    ?max_iops_per_gib
    ?min_storage_throughput_per_db_instance
    ?max_storage_throughput_per_db_instance
    ?min_storage_throughput_per_iops
    ?max_storage_throughput_per_iops
    ?(available_processor_features = [])
    ?(supported_engine_modes = [])
    ?supports_storage_autoscaling
    ?supports_kerberos_authentication
    ?outpost_capable
    ?(supported_activity_stream_modes = [])
    ?supports_global_databases
    ?(supported_network_types = [])
    ?supports_clusters
    ?supports_dedicated_log_volume
    ?supports_additional_storage_volumes
    ?supports_http_endpoint
    ?(available_additional_storage_volumes_options = [])
    () =
  { engine
  ; engine_version
  ; d_b_instance_class
  ; license_model
  ; availability_zone_group
  ; availability_zones
  ; multi_a_z_capable
  ; read_replica_capable
  ; vpc
  ; supports_storage_encryption
  ; storage_type
  ; supports_iops
  ; supports_storage_throughput
  ; supports_enhanced_monitoring
  ; supports_i_a_m_database_authentication
  ; supports_performance_insights
  ; min_storage_size
  ; max_storage_size
  ; min_iops_per_db_instance
  ; max_iops_per_db_instance
  ; min_iops_per_gib
  ; max_iops_per_gib
  ; min_storage_throughput_per_db_instance
  ; max_storage_throughput_per_db_instance
  ; min_storage_throughput_per_iops
  ; max_storage_throughput_per_iops
  ; available_processor_features
  ; supported_engine_modes
  ; supports_storage_autoscaling
  ; supports_kerberos_authentication
  ; outpost_capable
  ; supported_activity_stream_modes
  ; supports_global_databases
  ; supported_network_types
  ; supports_clusters
  ; supports_dedicated_log_volume
  ; supports_additional_storage_volumes
  ; supports_http_endpoint
  ; available_additional_storage_volumes_options
  }

let parse xml =
  Some
    { engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; d_b_instance_class =
        Aws.Util.option_bind (Aws.Xml.member "DBInstanceClass" xml) String.parse
    ; license_model =
        Aws.Util.option_bind (Aws.Xml.member "LicenseModel" xml) String.parse
    ; availability_zone_group =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneGroup" xml) String.parse
    ; availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailabilityZones" xml)
             AvailabilityZoneList.parse)
    ; multi_a_z_capable =
        Aws.Util.option_bind (Aws.Xml.member "MultiAZCapable" xml) Boolean.parse
    ; read_replica_capable =
        Aws.Util.option_bind (Aws.Xml.member "ReadReplicaCapable" xml) Boolean.parse
    ; vpc = Aws.Util.option_bind (Aws.Xml.member "Vpc" xml) Boolean.parse
    ; supports_storage_encryption =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageEncryption" xml)
          Boolean.parse
    ; storage_type = Aws.Util.option_bind (Aws.Xml.member "StorageType" xml) String.parse
    ; supports_iops =
        Aws.Util.option_bind (Aws.Xml.member "SupportsIops" xml) Boolean.parse
    ; supports_storage_throughput =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageThroughput" xml)
          Boolean.parse
    ; supports_enhanced_monitoring =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsEnhancedMonitoring" xml)
          Boolean.parse
    ; supports_i_a_m_database_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsIAMDatabaseAuthentication" xml)
          Boolean.parse
    ; supports_performance_insights =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsPerformanceInsights" xml)
          Boolean.parse
    ; min_storage_size =
        Aws.Util.option_bind (Aws.Xml.member "MinStorageSize" xml) Integer.parse
    ; max_storage_size =
        Aws.Util.option_bind (Aws.Xml.member "MaxStorageSize" xml) Integer.parse
    ; min_iops_per_db_instance =
        Aws.Util.option_bind (Aws.Xml.member "MinIopsPerDbInstance" xml) Integer.parse
    ; max_iops_per_db_instance =
        Aws.Util.option_bind (Aws.Xml.member "MaxIopsPerDbInstance" xml) Integer.parse
    ; min_iops_per_gib =
        Aws.Util.option_bind (Aws.Xml.member "MinIopsPerGib" xml) Double.parse
    ; max_iops_per_gib =
        Aws.Util.option_bind (Aws.Xml.member "MaxIopsPerGib" xml) Double.parse
    ; min_storage_throughput_per_db_instance =
        Aws.Util.option_bind
          (Aws.Xml.member "MinStorageThroughputPerDbInstance" xml)
          Integer.parse
    ; max_storage_throughput_per_db_instance =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxStorageThroughputPerDbInstance" xml)
          Integer.parse
    ; min_storage_throughput_per_iops =
        Aws.Util.option_bind
          (Aws.Xml.member "MinStorageThroughputPerIops" xml)
          Double.parse
    ; max_storage_throughput_per_iops =
        Aws.Util.option_bind
          (Aws.Xml.member "MaxStorageThroughputPerIops" xml)
          Double.parse
    ; available_processor_features =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailableProcessorFeatures" xml)
             AvailableProcessorFeatureList.parse)
    ; supported_engine_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedEngineModes" xml)
             EngineModeList.parse)
    ; supports_storage_autoscaling =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsStorageAutoscaling" xml)
          Boolean.parse
    ; supports_kerberos_authentication =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsKerberosAuthentication" xml)
          Boolean.parse
    ; outpost_capable =
        Aws.Util.option_bind (Aws.Xml.member "OutpostCapable" xml) Boolean.parse
    ; supported_activity_stream_modes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedActivityStreamModes" xml)
             ActivityStreamModeList.parse)
    ; supports_global_databases =
        Aws.Util.option_bind (Aws.Xml.member "SupportsGlobalDatabases" xml) Boolean.parse
    ; supported_network_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SupportedNetworkTypes" xml)
             StringList.parse)
    ; supports_clusters =
        Aws.Util.option_bind (Aws.Xml.member "SupportsClusters" xml) Boolean.parse
    ; supports_dedicated_log_volume =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsDedicatedLogVolume" xml)
          Boolean.parse
    ; supports_additional_storage_volumes =
        Aws.Util.option_bind
          (Aws.Xml.member "SupportsAdditionalStorageVolumes" xml)
          Boolean.parse
    ; supports_http_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "SupportsHttpEndpoint" xml) Boolean.parse
    ; available_additional_storage_volumes_options =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AvailableAdditionalStorageVolumesOptions" xml)
             AvailableAdditionalStorageVolumesOptionList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AvailableAdditionalStorageVolumesOptions.member"
              , AvailableAdditionalStorageVolumesOptionList.to_query
                  v.available_additional_storage_volumes_options ))
       ; Aws.Util.option_map v.supports_http_endpoint (fun f ->
             Aws.Query.Pair ("SupportsHttpEndpoint", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_additional_storage_volumes (fun f ->
             Aws.Query.Pair ("SupportsAdditionalStorageVolumes", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_dedicated_log_volume (fun f ->
             Aws.Query.Pair ("SupportsDedicatedLogVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_clusters (fun f ->
             Aws.Query.Pair ("SupportsClusters", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedNetworkTypes.member"
              , StringList.to_query v.supported_network_types ))
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             Aws.Query.Pair ("SupportsGlobalDatabases", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedActivityStreamModes.member"
              , ActivityStreamModeList.to_query v.supported_activity_stream_modes ))
       ; Aws.Util.option_map v.outpost_capable (fun f ->
             Aws.Query.Pair ("OutpostCapable", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_kerberos_authentication (fun f ->
             Aws.Query.Pair ("SupportsKerberosAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             Aws.Query.Pair ("SupportsStorageAutoscaling", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SupportedEngineModes.member"
              , EngineModeList.to_query v.supported_engine_modes ))
       ; Some
           (Aws.Query.Pair
              ( "AvailableProcessorFeatures.member"
              , AvailableProcessorFeatureList.to_query v.available_processor_features ))
       ; Aws.Util.option_map v.max_storage_throughput_per_iops (fun f ->
             Aws.Query.Pair ("MaxStorageThroughputPerIops", Double.to_query f))
       ; Aws.Util.option_map v.min_storage_throughput_per_iops (fun f ->
             Aws.Query.Pair ("MinStorageThroughputPerIops", Double.to_query f))
       ; Aws.Util.option_map v.max_storage_throughput_per_db_instance (fun f ->
             Aws.Query.Pair ("MaxStorageThroughputPerDbInstance", Integer.to_query f))
       ; Aws.Util.option_map v.min_storage_throughput_per_db_instance (fun f ->
             Aws.Query.Pair ("MinStorageThroughputPerDbInstance", Integer.to_query f))
       ; Aws.Util.option_map v.max_iops_per_gib (fun f ->
             Aws.Query.Pair ("MaxIopsPerGib", Double.to_query f))
       ; Aws.Util.option_map v.min_iops_per_gib (fun f ->
             Aws.Query.Pair ("MinIopsPerGib", Double.to_query f))
       ; Aws.Util.option_map v.max_iops_per_db_instance (fun f ->
             Aws.Query.Pair ("MaxIopsPerDbInstance", Integer.to_query f))
       ; Aws.Util.option_map v.min_iops_per_db_instance (fun f ->
             Aws.Query.Pair ("MinIopsPerDbInstance", Integer.to_query f))
       ; Aws.Util.option_map v.max_storage_size (fun f ->
             Aws.Query.Pair ("MaxStorageSize", Integer.to_query f))
       ; Aws.Util.option_map v.min_storage_size (fun f ->
             Aws.Query.Pair ("MinStorageSize", Integer.to_query f))
       ; Aws.Util.option_map v.supports_performance_insights (fun f ->
             Aws.Query.Pair ("SupportsPerformanceInsights", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_i_a_m_database_authentication (fun f ->
             Aws.Query.Pair ("SupportsIAMDatabaseAuthentication", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_enhanced_monitoring (fun f ->
             Aws.Query.Pair ("SupportsEnhancedMonitoring", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_storage_throughput (fun f ->
             Aws.Query.Pair ("SupportsStorageThroughput", Boolean.to_query f))
       ; Aws.Util.option_map v.supports_iops (fun f ->
             Aws.Query.Pair ("SupportsIops", Boolean.to_query f))
       ; Aws.Util.option_map v.storage_type (fun f ->
             Aws.Query.Pair ("StorageType", String.to_query f))
       ; Aws.Util.option_map v.supports_storage_encryption (fun f ->
             Aws.Query.Pair ("SupportsStorageEncryption", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc (fun f -> Aws.Query.Pair ("Vpc", Boolean.to_query f))
       ; Aws.Util.option_map v.read_replica_capable (fun f ->
             Aws.Query.Pair ("ReadReplicaCapable", Boolean.to_query f))
       ; Aws.Util.option_map v.multi_a_z_capable (fun f ->
             Aws.Query.Pair ("MultiAZCapable", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AvailabilityZones.member"
              , AvailabilityZoneList.to_query v.availability_zones ))
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             Aws.Query.Pair ("AvailabilityZoneGroup", String.to_query f))
       ; Aws.Util.option_map v.license_model (fun f ->
             Aws.Query.Pair ("LicenseModel", String.to_query f))
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             Aws.Query.Pair ("DBInstanceClass", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AvailableAdditionalStorageVolumesOptions"
           , AvailableAdditionalStorageVolumesOptionList.to_json
               v.available_additional_storage_volumes_options )
       ; Aws.Util.option_map v.supports_http_endpoint (fun f ->
             "SupportsHttpEndpoint", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_additional_storage_volumes (fun f ->
             "SupportsAdditionalStorageVolumes", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_dedicated_log_volume (fun f ->
             "SupportsDedicatedLogVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_clusters (fun f ->
             "SupportsClusters", Boolean.to_json f)
       ; Some ("SupportedNetworkTypes", StringList.to_json v.supported_network_types)
       ; Aws.Util.option_map v.supports_global_databases (fun f ->
             "SupportsGlobalDatabases", Boolean.to_json f)
       ; Some
           ( "SupportedActivityStreamModes"
           , ActivityStreamModeList.to_json v.supported_activity_stream_modes )
       ; Aws.Util.option_map v.outpost_capable (fun f ->
             "OutpostCapable", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_kerberos_authentication (fun f ->
             "SupportsKerberosAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_storage_autoscaling (fun f ->
             "SupportsStorageAutoscaling", Boolean.to_json f)
       ; Some ("SupportedEngineModes", EngineModeList.to_json v.supported_engine_modes)
       ; Some
           ( "AvailableProcessorFeatures"
           , AvailableProcessorFeatureList.to_json v.available_processor_features )
       ; Aws.Util.option_map v.max_storage_throughput_per_iops (fun f ->
             "MaxStorageThroughputPerIops", Double.to_json f)
       ; Aws.Util.option_map v.min_storage_throughput_per_iops (fun f ->
             "MinStorageThroughputPerIops", Double.to_json f)
       ; Aws.Util.option_map v.max_storage_throughput_per_db_instance (fun f ->
             "MaxStorageThroughputPerDbInstance", Integer.to_json f)
       ; Aws.Util.option_map v.min_storage_throughput_per_db_instance (fun f ->
             "MinStorageThroughputPerDbInstance", Integer.to_json f)
       ; Aws.Util.option_map v.max_iops_per_gib (fun f ->
             "MaxIopsPerGib", Double.to_json f)
       ; Aws.Util.option_map v.min_iops_per_gib (fun f ->
             "MinIopsPerGib", Double.to_json f)
       ; Aws.Util.option_map v.max_iops_per_db_instance (fun f ->
             "MaxIopsPerDbInstance", Integer.to_json f)
       ; Aws.Util.option_map v.min_iops_per_db_instance (fun f ->
             "MinIopsPerDbInstance", Integer.to_json f)
       ; Aws.Util.option_map v.max_storage_size (fun f ->
             "MaxStorageSize", Integer.to_json f)
       ; Aws.Util.option_map v.min_storage_size (fun f ->
             "MinStorageSize", Integer.to_json f)
       ; Aws.Util.option_map v.supports_performance_insights (fun f ->
             "SupportsPerformanceInsights", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_i_a_m_database_authentication (fun f ->
             "SupportsIAMDatabaseAuthentication", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_enhanced_monitoring (fun f ->
             "SupportsEnhancedMonitoring", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_storage_throughput (fun f ->
             "SupportsStorageThroughput", Boolean.to_json f)
       ; Aws.Util.option_map v.supports_iops (fun f -> "SupportsIops", Boolean.to_json f)
       ; Aws.Util.option_map v.storage_type (fun f -> "StorageType", String.to_json f)
       ; Aws.Util.option_map v.supports_storage_encryption (fun f ->
             "SupportsStorageEncryption", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc (fun f -> "Vpc", Boolean.to_json f)
       ; Aws.Util.option_map v.read_replica_capable (fun f ->
             "ReadReplicaCapable", Boolean.to_json f)
       ; Aws.Util.option_map v.multi_a_z_capable (fun f ->
             "MultiAZCapable", Boolean.to_json f)
       ; Some ("AvailabilityZones", AvailabilityZoneList.to_json v.availability_zones)
       ; Aws.Util.option_map v.availability_zone_group (fun f ->
             "AvailabilityZoneGroup", String.to_json f)
       ; Aws.Util.option_map v.license_model (fun f -> "LicenseModel", String.to_json f)
       ; Aws.Util.option_map v.d_b_instance_class (fun f ->
             "DBInstanceClass", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ])

let of_json j =
  { engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; d_b_instance_class =
      Aws.Util.option_map (Aws.Json.lookup j "DBInstanceClass") String.of_json
  ; license_model = Aws.Util.option_map (Aws.Json.lookup j "LicenseModel") String.of_json
  ; availability_zone_group =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneGroup") String.of_json
  ; availability_zones =
      AvailabilityZoneList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailabilityZones"))
  ; multi_a_z_capable =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAZCapable") Boolean.of_json
  ; read_replica_capable =
      Aws.Util.option_map (Aws.Json.lookup j "ReadReplicaCapable") Boolean.of_json
  ; vpc = Aws.Util.option_map (Aws.Json.lookup j "Vpc") Boolean.of_json
  ; supports_storage_encryption =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageEncryption") Boolean.of_json
  ; storage_type = Aws.Util.option_map (Aws.Json.lookup j "StorageType") String.of_json
  ; supports_iops = Aws.Util.option_map (Aws.Json.lookup j "SupportsIops") Boolean.of_json
  ; supports_storage_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageThroughput") Boolean.of_json
  ; supports_enhanced_monitoring =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsEnhancedMonitoring") Boolean.of_json
  ; supports_i_a_m_database_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsIAMDatabaseAuthentication")
        Boolean.of_json
  ; supports_performance_insights =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsPerformanceInsights")
        Boolean.of_json
  ; min_storage_size =
      Aws.Util.option_map (Aws.Json.lookup j "MinStorageSize") Integer.of_json
  ; max_storage_size =
      Aws.Util.option_map (Aws.Json.lookup j "MaxStorageSize") Integer.of_json
  ; min_iops_per_db_instance =
      Aws.Util.option_map (Aws.Json.lookup j "MinIopsPerDbInstance") Integer.of_json
  ; max_iops_per_db_instance =
      Aws.Util.option_map (Aws.Json.lookup j "MaxIopsPerDbInstance") Integer.of_json
  ; min_iops_per_gib =
      Aws.Util.option_map (Aws.Json.lookup j "MinIopsPerGib") Double.of_json
  ; max_iops_per_gib =
      Aws.Util.option_map (Aws.Json.lookup j "MaxIopsPerGib") Double.of_json
  ; min_storage_throughput_per_db_instance =
      Aws.Util.option_map
        (Aws.Json.lookup j "MinStorageThroughputPerDbInstance")
        Integer.of_json
  ; max_storage_throughput_per_db_instance =
      Aws.Util.option_map
        (Aws.Json.lookup j "MaxStorageThroughputPerDbInstance")
        Integer.of_json
  ; min_storage_throughput_per_iops =
      Aws.Util.option_map (Aws.Json.lookup j "MinStorageThroughputPerIops") Double.of_json
  ; max_storage_throughput_per_iops =
      Aws.Util.option_map (Aws.Json.lookup j "MaxStorageThroughputPerIops") Double.of_json
  ; available_processor_features =
      AvailableProcessorFeatureList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AvailableProcessorFeatures"))
  ; supported_engine_modes =
      EngineModeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedEngineModes"))
  ; supports_storage_autoscaling =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsStorageAutoscaling") Boolean.of_json
  ; supports_kerberos_authentication =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsKerberosAuthentication")
        Boolean.of_json
  ; outpost_capable =
      Aws.Util.option_map (Aws.Json.lookup j "OutpostCapable") Boolean.of_json
  ; supported_activity_stream_modes =
      ActivityStreamModeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedActivityStreamModes"))
  ; supports_global_databases =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsGlobalDatabases") Boolean.of_json
  ; supported_network_types =
      StringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SupportedNetworkTypes"))
  ; supports_clusters =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsClusters") Boolean.of_json
  ; supports_dedicated_log_volume =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsDedicatedLogVolume") Boolean.of_json
  ; supports_additional_storage_volumes =
      Aws.Util.option_map
        (Aws.Json.lookup j "SupportsAdditionalStorageVolumes")
        Boolean.of_json
  ; supports_http_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "SupportsHttpEndpoint") Boolean.of_json
  ; available_additional_storage_volumes_options =
      AvailableAdditionalStorageVolumesOptionList.of_json
        (Aws.Util.of_option_exn
           (Aws.Json.lookup j "AvailableAdditionalStorageVolumesOptions"))
  }
