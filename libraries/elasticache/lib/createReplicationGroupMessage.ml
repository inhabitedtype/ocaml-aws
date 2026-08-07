open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; replication_group_description : String.t
  ; global_replication_group_id : String.t option
  ; primary_cluster_id : String.t option
  ; automatic_failover_enabled : Boolean.t option
  ; multi_a_z_enabled : Boolean.t option
  ; num_cache_clusters : Integer.t option
  ; preferred_cache_cluster_a_zs : AvailabilityZonesList.t
  ; num_node_groups : Integer.t option
  ; replicas_per_node_group : Integer.t option
  ; node_group_configuration : NodeGroupConfigurationList.t
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
  ; transit_encryption_enabled : Boolean.t option
  ; at_rest_encryption_enabled : Boolean.t option
  ; kms_key_id : String.t option
  ; user_group_ids : UserGroupIdListInput.t
  ; log_delivery_configurations : LogDeliveryConfigurationRequestList.t
  ; data_tiering_enabled : Boolean.t option
  ; network_type : NetworkType.t option
  ; ip_discovery : IpDiscovery.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  ; cluster_mode : ClusterMode.t option
  ; serverless_cache_snapshot_name : String.t option
  ; durability : Durability.t option
  }

let make
    ~replication_group_id
    ~replication_group_description
    ?global_replication_group_id
    ?primary_cluster_id
    ?automatic_failover_enabled
    ?multi_a_z_enabled
    ?num_cache_clusters
    ?(preferred_cache_cluster_a_zs = [])
    ?num_node_groups
    ?replicas_per_node_group
    ?(node_group_configuration = [])
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
    ?transit_encryption_enabled
    ?at_rest_encryption_enabled
    ?kms_key_id
    ?(user_group_ids = [])
    ?(log_delivery_configurations = [])
    ?data_tiering_enabled
    ?network_type
    ?ip_discovery
    ?transit_encryption_mode
    ?cluster_mode
    ?serverless_cache_snapshot_name
    ?durability
    () =
  { replication_group_id
  ; replication_group_description
  ; global_replication_group_id
  ; primary_cluster_id
  ; automatic_failover_enabled
  ; multi_a_z_enabled
  ; num_cache_clusters
  ; preferred_cache_cluster_a_zs
  ; num_node_groups
  ; replicas_per_node_group
  ; node_group_configuration
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
  ; transit_encryption_enabled
  ; at_rest_encryption_enabled
  ; kms_key_id
  ; user_group_ids
  ; log_delivery_configurations
  ; data_tiering_enabled
  ; network_type
  ; ip_discovery
  ; transit_encryption_mode
  ; cluster_mode
  ; serverless_cache_snapshot_name
  ; durability
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; replication_group_description =
        Aws.Xml.required
          "ReplicationGroupDescription"
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroupDescription" xml)
             String.parse)
    ; global_replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "GlobalReplicationGroupId" xml) String.parse
    ; primary_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
    ; automatic_failover_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AutomaticFailoverEnabled" xml) Boolean.parse
    ; multi_a_z_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiAZEnabled" xml) Boolean.parse
    ; num_cache_clusters =
        Aws.Util.option_bind (Aws.Xml.member "NumCacheClusters" xml) Integer.parse
    ; preferred_cache_cluster_a_zs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "PreferredCacheClusterAZs" xml)
             AvailabilityZonesList.parse)
    ; num_node_groups =
        Aws.Util.option_bind (Aws.Xml.member "NumNodeGroups" xml) Integer.parse
    ; replicas_per_node_group =
        Aws.Util.option_bind (Aws.Xml.member "ReplicasPerNodeGroup" xml) Integer.parse
    ; node_group_configuration =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeGroupConfiguration" xml)
             NodeGroupConfigurationList.parse)
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
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; at_rest_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AtRestEncryptionEnabled" xml) Boolean.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; user_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroupIds" xml)
             UserGroupIdListInput.parse)
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationRequestList.parse)
    ; data_tiering_enabled =
        Aws.Util.option_bind (Aws.Xml.member "DataTieringEnabled" xml) Boolean.parse
    ; network_type =
        Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) NetworkType.parse
    ; ip_discovery =
        Aws.Util.option_bind (Aws.Xml.member "IpDiscovery" xml) IpDiscovery.parse
    ; transit_encryption_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitEncryptionMode" xml)
          TransitEncryptionMode.parse
    ; cluster_mode =
        Aws.Util.option_bind (Aws.Xml.member "ClusterMode" xml) ClusterMode.parse
    ; serverless_cache_snapshot_name =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
          String.parse
    ; durability = Aws.Util.option_bind (Aws.Xml.member "Durability" xml) Durability.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f ->
             Aws.Query.Pair ("Durability", Durability.to_query f))
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheSnapshotName", String.to_query f))
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             Aws.Query.Pair ("ClusterMode", ClusterMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", NetworkType.to_query f))
       ; Aws.Util.option_map v.data_tiering_enabled (fun f ->
             Aws.Query.Pair ("DataTieringEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationRequestList.to_query v.log_delivery_configurations
              ))
       ; Some
           (Aws.Query.Pair
              ("UserGroupIds.member", UserGroupIdListInput.to_query v.user_group_ids))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
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
       ; Some
           (Aws.Query.Pair
              ( "NodeGroupConfiguration.member"
              , NodeGroupConfigurationList.to_query v.node_group_configuration ))
       ; Aws.Util.option_map v.replicas_per_node_group (fun f ->
             Aws.Query.Pair ("ReplicasPerNodeGroup", Integer.to_query f))
       ; Aws.Util.option_map v.num_node_groups (fun f ->
             Aws.Query.Pair ("NumNodeGroups", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "PreferredCacheClusterAZs.member"
              , AvailabilityZonesList.to_query v.preferred_cache_cluster_a_zs ))
       ; Aws.Util.option_map v.num_cache_clusters (fun f ->
             Aws.Query.Pair ("NumCacheClusters", Integer.to_query f))
       ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
             Aws.Query.Pair ("MultiAZEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             Aws.Query.Pair ("GlobalReplicationGroupId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ReplicationGroupDescription"
              , String.to_query v.replication_group_description ))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f -> "Durability", Durability.to_json f)
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             "ServerlessCacheSnapshotName", String.to_json f)
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             "ClusterMode", ClusterMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Aws.Util.option_map v.network_type (fun f ->
             "NetworkType", NetworkType.to_json f)
       ; Aws.Util.option_map v.data_tiering_enabled (fun f ->
             "DataTieringEnabled", Boolean.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationRequestList.to_json v.log_delivery_configurations )
       ; Some ("UserGroupIds", UserGroupIdListInput.to_json v.user_group_ids)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             "AtRestEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
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
       ; Some
           ( "NodeGroupConfiguration"
           , NodeGroupConfigurationList.to_json v.node_group_configuration )
       ; Aws.Util.option_map v.replicas_per_node_group (fun f ->
             "ReplicasPerNodeGroup", Integer.to_json f)
       ; Aws.Util.option_map v.num_node_groups (fun f ->
             "NumNodeGroups", Integer.to_json f)
       ; Some
           ( "PreferredCacheClusterAZs"
           , AvailabilityZonesList.to_json v.preferred_cache_cluster_a_zs )
       ; Aws.Util.option_map v.num_cache_clusters (fun f ->
             "NumCacheClusters", Integer.to_json f)
       ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
             "MultiAZEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             "AutomaticFailoverEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             "PrimaryClusterId", String.to_json f)
       ; Aws.Util.option_map v.global_replication_group_id (fun f ->
             "GlobalReplicationGroupId", String.to_json f)
       ; Some
           ("ReplicationGroupDescription", String.to_json v.replication_group_description)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; replication_group_description =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupDescription"))
  ; global_replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalReplicationGroupId") String.of_json
  ; primary_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
  ; automatic_failover_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
  ; multi_a_z_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAZEnabled") Boolean.of_json
  ; num_cache_clusters =
      Aws.Util.option_map (Aws.Json.lookup j "NumCacheClusters") Integer.of_json
  ; preferred_cache_cluster_a_zs =
      AvailabilityZonesList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "PreferredCacheClusterAZs"))
  ; num_node_groups =
      Aws.Util.option_map (Aws.Json.lookup j "NumNodeGroups") Integer.of_json
  ; replicas_per_node_group =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicasPerNodeGroup") Integer.of_json
  ; node_group_configuration =
      NodeGroupConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroupConfiguration"))
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
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; at_rest_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; user_group_ids =
      UserGroupIdListInput.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
  ; log_delivery_configurations =
      LogDeliveryConfigurationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; data_tiering_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "DataTieringEnabled") Boolean.of_json
  ; network_type =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkType") NetworkType.of_json
  ; ip_discovery =
      Aws.Util.option_map (Aws.Json.lookup j "IpDiscovery") IpDiscovery.of_json
  ; transit_encryption_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitEncryptionMode")
        TransitEncryptionMode.of_json
  ; cluster_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterMode") ClusterMode.of_json
  ; serverless_cache_snapshot_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheSnapshotName") String.of_json
  ; durability = Aws.Util.option_map (Aws.Json.lookup j "Durability") Durability.of_json
  }
