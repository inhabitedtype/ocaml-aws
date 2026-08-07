open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t
  ; replication_group_id : String.t option
  ; a_z_mode : AZMode.t option
  ; preferred_availability_zone : String.t option
  ; preferred_availability_zones : PreferredAvailabilityZoneList.t
  ; num_cache_nodes : Integer.t option
  ; cache_node_type : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; cache_parameter_group_name : String.t option
  ; cache_subnet_group_name : String.t option
  ; cache_security_group_names : CacheSecurityGroupNameList.t
  ; security_group_ids : SecurityGroupIdsList.t
  ; tags : TagList.t
  ; snapshot_arns : SnapshotArnsList.t
  ; snapshot_name : String.t option
  ; preferred_maintenance_window : String.t option
  ; port : Integer.t option
  ; notification_topic_arn : String.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; snapshot_retention_limit : Integer.t option
  ; snapshot_window : String.t option
  ; auth_token : String.t option
  ; outpost_mode : OutpostMode.t option
  ; preferred_outpost_arn : String.t option
  ; preferred_outpost_arns : PreferredOutpostArnList.t
  ; log_delivery_configurations : LogDeliveryConfigurationRequestList.t
  ; transit_encryption_enabled : Boolean.t option
  ; network_type : NetworkType.t option
  ; ip_discovery : IpDiscovery.t option
  }

let make
    ~cache_cluster_id
    ?replication_group_id
    ?a_z_mode
    ?preferred_availability_zone
    ?(preferred_availability_zones = [])
    ?num_cache_nodes
    ?cache_node_type
    ?engine
    ?engine_version
    ?cache_parameter_group_name
    ?cache_subnet_group_name
    ?(cache_security_group_names = [])
    ?(security_group_ids = [])
    ?(tags = [])
    ?(snapshot_arns = [])
    ?snapshot_name
    ?preferred_maintenance_window
    ?port
    ?notification_topic_arn
    ?auto_minor_version_upgrade
    ?snapshot_retention_limit
    ?snapshot_window
    ?auth_token
    ?outpost_mode
    ?preferred_outpost_arn
    ?(preferred_outpost_arns = [])
    ?(log_delivery_configurations = [])
    ?transit_encryption_enabled
    ?network_type
    ?ip_discovery
    () =
  { cache_cluster_id
  ; replication_group_id
  ; a_z_mode
  ; preferred_availability_zone
  ; preferred_availability_zones
  ; num_cache_nodes
  ; cache_node_type
  ; engine
  ; engine_version
  ; cache_parameter_group_name
  ; cache_subnet_group_name
  ; cache_security_group_names
  ; security_group_ids
  ; tags
  ; snapshot_arns
  ; snapshot_name
  ; preferred_maintenance_window
  ; port
  ; notification_topic_arn
  ; auto_minor_version_upgrade
  ; snapshot_retention_limit
  ; snapshot_window
  ; auth_token
  ; outpost_mode
  ; preferred_outpost_arn
  ; preferred_outpost_arns
  ; log_delivery_configurations
  ; transit_encryption_enabled
  ; network_type
  ; ip_discovery
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Xml.required
          "CacheClusterId"
          (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
    ; replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; a_z_mode = Aws.Util.option_bind (Aws.Xml.member "AZMode" xml) AZMode.parse
    ; preferred_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "PreferredAvailabilityZone" xml) String.parse
    ; preferred_availability_zones =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredAvailabilityZones" xml)
             PreferredAvailabilityZoneList.parse)
    ; num_cache_nodes =
        Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; cache_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
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
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; snapshot_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SnapshotArns" xml)
             SnapshotArnsList.parse)
    ; snapshot_name =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; notification_topic_arn =
        Aws.Util.option_bind (Aws.Xml.member "NotificationTopicArn" xml) String.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; snapshot_window =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
    ; auth_token = Aws.Util.option_bind (Aws.Xml.member "AuthToken" xml) String.parse
    ; outpost_mode =
        Aws.Util.option_bind (Aws.Xml.member "OutpostMode" xml) OutpostMode.parse
    ; preferred_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
    ; preferred_outpost_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredOutpostArns" xml)
             PreferredOutpostArnList.parse)
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationRequestList.parse)
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; network_type =
        Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) NetworkType.parse
    ; ip_discovery =
        Aws.Util.option_bind (Aws.Xml.member "IpDiscovery" xml) IpDiscovery.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", NetworkType.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationRequestList.to_query v.log_delivery_configurations
              ))
       ; Some
           (Aws.Query.Pair
              ( "PreferredOutpostArns.member"
              , PreferredOutpostArnList.to_query v.preferred_outpost_arns ))
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
       ; Aws.Util.option_map v.outpost_mode (fun f ->
             Aws.Query.Pair ("OutpostMode", OutpostMode.to_query f))
       ; Aws.Util.option_map v.auth_token (fun f ->
             Aws.Query.Pair ("AuthToken", String.to_query f))
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             Aws.Query.Pair ("SnapshotWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.notification_topic_arn (fun f ->
             Aws.Query.Pair ("NotificationTopicArn", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_name (fun f ->
             Aws.Query.Pair ("SnapshotName", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("SnapshotArns.member", SnapshotArnsList.to_query v.snapshot_arns))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroupIds.member"
              , SecurityGroupIdsList.to_query v.security_group_ids ))
       ; Some
           (Aws.Query.Pair
              ( "CacheSecurityGroupNames.member"
              , CacheSecurityGroupNameList.to_query v.cache_security_group_names ))
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PreferredAvailabilityZones.member"
              , PreferredAvailabilityZoneList.to_query v.preferred_availability_zones ))
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.a_z_mode (fun f ->
             Aws.Query.Pair ("AZMode", AZMode.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Aws.Util.option_map v.network_type (fun f ->
             "NetworkType", NetworkType.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationRequestList.to_json v.log_delivery_configurations )
       ; Some
           ( "PreferredOutpostArns"
           , PreferredOutpostArnList.to_json v.preferred_outpost_arns )
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             "PreferredOutpostArn", String.to_json f)
       ; Aws.Util.option_map v.outpost_mode (fun f ->
             "OutpostMode", OutpostMode.to_json f)
       ; Aws.Util.option_map v.auth_token (fun f -> "AuthToken", String.to_json f)
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             "SnapshotWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.notification_topic_arn (fun f ->
             "NotificationTopicArn", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
       ; Some ("SnapshotArns", SnapshotArnsList.to_json v.snapshot_arns)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Some ("SecurityGroupIds", SecurityGroupIdsList.to_json v.security_group_ids)
       ; Some
           ( "CacheSecurityGroupNames"
           , CacheSecurityGroupNameList.to_json v.cache_security_group_names )
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             "CacheSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             "NumCacheNodes", Integer.to_json f)
       ; Some
           ( "PreferredAvailabilityZones"
           , PreferredAvailabilityZoneList.to_json v.preferred_availability_zones )
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             "PreferredAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.a_z_mode (fun f -> "AZMode", AZMode.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
       ])

let of_json j =
  { cache_cluster_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
  ; replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; a_z_mode = Aws.Util.option_map (Aws.Json.lookup j "AZMode") AZMode.of_json
  ; preferred_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
  ; preferred_availability_zones =
      PreferredAvailabilityZoneList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredAvailabilityZones"))
  ; num_cache_nodes =
      Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; cache_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
  ; cache_security_group_names =
      CacheSecurityGroupNameList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroupNames"))
  ; security_group_ids =
      SecurityGroupIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroupIds"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; snapshot_arns =
      SnapshotArnsList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotArns"))
  ; snapshot_name = Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; notification_topic_arn =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationTopicArn") String.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; snapshot_window =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
  ; auth_token = Aws.Util.option_map (Aws.Json.lookup j "AuthToken") String.of_json
  ; outpost_mode =
      Aws.Util.option_map (Aws.Json.lookup j "OutpostMode") OutpostMode.of_json
  ; preferred_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
  ; preferred_outpost_arns =
      PreferredOutpostArnList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredOutpostArns"))
  ; log_delivery_configurations =
      LogDeliveryConfigurationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; network_type =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkType") NetworkType.of_json
  ; ip_discovery =
      Aws.Util.option_map (Aws.Json.lookup j "IpDiscovery") IpDiscovery.of_json
  }
