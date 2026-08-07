open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t
  ; num_cache_nodes : Integer.t option
  ; cache_node_ids_to_remove : CacheNodeIdsList.t
  ; a_z_mode : AZMode.t option
  ; new_availability_zones : PreferredAvailabilityZoneList.t
  ; cache_security_group_names : CacheSecurityGroupNameList.t
  ; security_group_ids : SecurityGroupIdsList.t
  ; preferred_maintenance_window : String.t option
  ; notification_topic_arn : String.t option
  ; cache_parameter_group_name : String.t option
  ; notification_topic_status : String.t option
  ; apply_immediately : Boolean.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; snapshot_retention_limit : Integer.t option
  ; snapshot_window : String.t option
  ; cache_node_type : String.t option
  ; auth_token : String.t option
  ; auth_token_update_strategy : AuthTokenUpdateStrategyType.t option
  ; log_delivery_configurations : LogDeliveryConfigurationRequestList.t
  ; ip_discovery : IpDiscovery.t option
  ; scale_config : ScaleConfig.t option
  }

let make
    ~cache_cluster_id
    ?num_cache_nodes
    ?(cache_node_ids_to_remove = [])
    ?a_z_mode
    ?(new_availability_zones = [])
    ?(cache_security_group_names = [])
    ?(security_group_ids = [])
    ?preferred_maintenance_window
    ?notification_topic_arn
    ?cache_parameter_group_name
    ?notification_topic_status
    ?apply_immediately
    ?engine
    ?engine_version
    ?auto_minor_version_upgrade
    ?snapshot_retention_limit
    ?snapshot_window
    ?cache_node_type
    ?auth_token
    ?auth_token_update_strategy
    ?(log_delivery_configurations = [])
    ?ip_discovery
    ?scale_config
    () =
  { cache_cluster_id
  ; num_cache_nodes
  ; cache_node_ids_to_remove
  ; a_z_mode
  ; new_availability_zones
  ; cache_security_group_names
  ; security_group_ids
  ; preferred_maintenance_window
  ; notification_topic_arn
  ; cache_parameter_group_name
  ; notification_topic_status
  ; apply_immediately
  ; engine
  ; engine_version
  ; auto_minor_version_upgrade
  ; snapshot_retention_limit
  ; snapshot_window
  ; cache_node_type
  ; auth_token
  ; auth_token_update_strategy
  ; log_delivery_configurations
  ; ip_discovery
  ; scale_config
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Xml.required
          "CacheClusterId"
          (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
    ; num_cache_nodes =
        Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
    ; cache_node_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeIdsToRemove" xml)
             CacheNodeIdsList.parse)
    ; a_z_mode = Aws.Util.option_bind (Aws.Xml.member "AZMode" xml) AZMode.parse
    ; new_availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NewAvailabilityZones" xml)
             PreferredAvailabilityZoneList.parse)
    ; cache_security_group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroupNames" xml)
             CacheSecurityGroupNameList.parse)
    ; security_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroupIds" xml)
             SecurityGroupIdsList.parse)
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; notification_topic_arn =
        Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
    ; cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; notification_topic_status =
        Aws.Util.option_bind (Aws.Xml.member "NotificationTopicStatus" xml) String.parse
    ; apply_immediately =
        Aws.Util.option_bind (Aws.Xml.member "ApplyImmediately" xml) Boolean.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; snapshot_window =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
    ; auth_token_update_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthTokenUpdateStrategy" xml)
          AuthTokenUpdateStrategyType.parse
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationRequestList.parse)
    ; ip_discovery =
        Aws.Util.option_bind (Aws.Xml.member "IpDiscovery" xml) IpDiscovery.parse
    ; scale_config =
        Aws.Util.option_bind (Aws.Xml.member "ScaleConfig" xml) ScaleConfig.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_config (fun f ->
             Aws.Query.Pair ("ScaleConfig", ScaleConfig.to_query f))
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationRequestList.to_query v.log_delivery_configurations
              ))
       ; Aws.Util.option_map v.auth_token_update_strategy (fun f ->
             Aws.Query.Pair
               ("AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_query f))
       ; Aws.Util.option_map v.auth_token (fun f ->
             Aws.Query.Pair ("AuthToken", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             Aws.Query.Pair ("SnapshotWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             Aws.Query.Pair ("ApplyImmediately", Boolean.to_query f))
       ; Aws.Util.option_map v.notification_topic_status (fun f ->
             Aws.Query.Pair ("NotificationTopicStatus", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.notification_topic_arn (fun f ->
             Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIds.member"
              , SecurityGroupIdsList.to_query v.security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ( "CacheSecurityGroupNames.member"
              , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
       ; Some
           (Aws.Query.Pair
              ( "NewAvailabilityZones.member"
              , PreferredAvailabilityZoneList.to_query v.new_availability_zones ))
       ; Aws.Util.option_map v.a_z_mode (fun f ->
             Aws.Query.Pair ("AZMode", AZMode.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CacheNodeIdsToRemove.member"
              , CacheNodeIdsList.to_query v.cache_node_ids_to_remove ))
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
       ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.scale_config (fun f ->
             "ScaleConfig", ScaleConfig.to_json f)
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationRequestList.to_json v.log_delivery_configurations )
       ; Aws.Util.option_map v.auth_token_update_strategy (fun f ->
             "AuthTokenUpdateStrategy", AuthTokenUpdateStrategyType.to_json f)
       ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             "SnapshotWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.apply_immediately (fun f ->
             "ApplyImmediately", Boolean.to_json f)
       ; Aws.Util.option_map v.notification_topic_status (fun f ->
             "NotificationTopicStatus", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.notification_topic_arn (fun f ->
             "NotificationTopicArn", String.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
       ; Some
           ( "CacheSecurityGroupNames"
           , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
       ; Some
           ( "NewAvailabilityZones"
           , PreferredAvailabilityZoneList.to_json v.new_availability_zones )
       ; Aws.Util.option_map v.a_z_mode (fun f -> "AZMode", AZMode.to_json f)
       ; Some ("CacheNodeIdsToRemove", CacheNodeIdsList.to_json v.cache_node_ids_to_remove)
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             "NumCacheNodes", Integer.to_json f)
       ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
       ])

let of_json j =
  { cache_cluster_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
  ; num_cache_nodes =
      Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
  ; cache_node_ids_to_remove =
      CacheNodeIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToRemove"))
  ; a_z_mode = Aws.Util.option_map (Aws.Json.lookup j "AZMode") AZMode.of_json
  ; new_availability_zones =
      PreferredAvailabilityZoneList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NewAvailabilityZones"))
  ; cache_security_group_names =
      CacheSecurityGroupNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
  ; security_group_ids =
      SecurityGroupIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; notification_topic_arn =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
  ; cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; notification_topic_status =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicStatus") String.of_json
  ; apply_immediately =
      Aws.Util.option_map (Aws.Json.lookup j "ApplyImmediately") Boolean.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; snapshot_window =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
  ; auth_token_update_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "AuthTokenUpdateStrategy")
        AuthTokenUpdateStrategyType.of_json
  ; log_delivery_configurations =
      LogDeliveryConfigurationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; ip_discovery =
      Aws.Util.option_map (Aws.Json.lookup j "IpDiscovery") IpDiscovery.of_json
  ; scale_config =
      Aws.Util.option_map (Aws.Json.lookup j "ScaleConfig") ScaleConfig.of_json
  }
