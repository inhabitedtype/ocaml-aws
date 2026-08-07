open Aws.BaseTypes

type t =
  { replication_group_id : String.t
  ; replication_group_description : String.t option
  ; primary_cluster_id : String.t option
  ; snapshotting_cluster_id : String.t option
  ; automatic_failover_enabled : Boolean.t option
  ; multi_a_z_enabled : Boolean.t option
  ; node_group_id : String.t option
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
  ; user_group_ids_to_add : UserGroupIdList.t
  ; user_group_ids_to_remove : UserGroupIdList.t
  ; remove_user_groups : Boolean.t option
  ; log_delivery_configurations : LogDeliveryConfigurationRequestList.t
  ; ip_discovery : IpDiscovery.t option
  ; transit_encryption_enabled : Boolean.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  ; cluster_mode : ClusterMode.t option
  ; durability : Durability.t option
  }

let make
    ~replication_group_id
    ?replication_group_description
    ?primary_cluster_id
    ?snapshotting_cluster_id
    ?automatic_failover_enabled
    ?multi_a_z_enabled
    ?node_group_id
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
    ?(user_group_ids_to_add = [])
    ?(user_group_ids_to_remove = [])
    ?remove_user_groups
    ?(log_delivery_configurations = [])
    ?ip_discovery
    ?transit_encryption_enabled
    ?transit_encryption_mode
    ?cluster_mode
    ?durability
    () =
  { replication_group_id
  ; replication_group_description
  ; primary_cluster_id
  ; snapshotting_cluster_id
  ; automatic_failover_enabled
  ; multi_a_z_enabled
  ; node_group_id
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
  ; user_group_ids_to_add
  ; user_group_ids_to_remove
  ; remove_user_groups
  ; log_delivery_configurations
  ; ip_discovery
  ; transit_encryption_enabled
  ; transit_encryption_mode
  ; cluster_mode
  ; durability
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Xml.required
          "ReplicationGroupId"
          (Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse)
    ; replication_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationGroupDescription" xml)
          String.parse
    ; primary_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "PrimaryClusterId" xml) String.parse
    ; snapshotting_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "SnapshottingClusterId" xml) String.parse
    ; automatic_failover_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AutomaticFailoverEnabled" xml) Boolean.parse
    ; multi_a_z_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiAZEnabled" xml) Boolean.parse
    ; node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
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
    ; user_group_ids_to_add =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroupIdsToAdd" xml)
             UserGroupIdList.parse)
    ; user_group_ids_to_remove =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroupIdsToRemove" xml)
             UserGroupIdList.parse)
    ; remove_user_groups =
        Aws.Util.option_bind (Aws.Xml.member "RemoveUserGroups" xml) Boolean.parse
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationRequestList.parse)
    ; ip_discovery =
        Aws.Util.option_bind (Aws.Xml.member "IpDiscovery" xml) IpDiscovery.parse
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; transit_encryption_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitEncryptionMode" xml)
          TransitEncryptionMode.parse
    ; cluster_mode =
        Aws.Util.option_bind (Aws.Xml.member "ClusterMode" xml) ClusterMode.parse
    ; durability = Aws.Util.option_bind (Aws.Xml.member "Durability" xml) Durability.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f ->
             Aws.Query.Pair ("Durability", Durability.to_query f))
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             Aws.Query.Pair ("ClusterMode", ClusterMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationRequestList.to_query v.log_delivery_configurations
              ))
       ; Aws.Util.option_map v.remove_user_groups (fun f ->
             Aws.Query.Pair ("RemoveUserGroups", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "UserGroupIdsToRemove.member"
              , UserGroupIdList.to_query v.user_group_ids_to_remove ))
       ; Some
           (Aws.Query.Pair
              ( "UserGroupIdsToAdd.member"
              , UserGroupIdList.to_query v.user_group_ids_to_add ))
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
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
             Aws.Query.Pair ("MultiAZEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             Aws.Query.Pair ("AutomaticFailoverEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
             Aws.Query.Pair ("SnapshottingClusterId", String.to_query f))
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             Aws.Query.Pair ("PrimaryClusterId", String.to_query f))
       ; Aws.Util.option_map v.replication_group_description (fun f ->
             Aws.Query.Pair ("ReplicationGroupDescription", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ReplicationGroupId", String.to_query v.replication_group_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f -> "Durability", Durability.to_json f)
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             "ClusterMode", ClusterMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationRequestList.to_json v.log_delivery_configurations )
       ; Aws.Util.option_map v.remove_user_groups (fun f ->
             "RemoveUserGroups", Boolean.to_json f)
       ; Some ("UserGroupIdsToRemove", UserGroupIdList.to_json v.user_group_ids_to_remove)
       ; Some ("UserGroupIdsToAdd", UserGroupIdList.to_json v.user_group_ids_to_add)
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
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ; Aws.Util.option_map v.multi_a_z_enabled (fun f ->
             "MultiAZEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.automatic_failover_enabled (fun f ->
             "AutomaticFailoverEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
             "SnapshottingClusterId", String.to_json f)
       ; Aws.Util.option_map v.primary_cluster_id (fun f ->
             "PrimaryClusterId", String.to_json f)
       ; Aws.Util.option_map v.replication_group_description (fun f ->
             "ReplicationGroupDescription", String.to_json f)
       ; Some ("ReplicationGroupId", String.to_json v.replication_group_id)
       ])

let of_json j =
  { replication_group_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupId"))
  ; replication_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupDescription") String.of_json
  ; primary_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "PrimaryClusterId") String.of_json
  ; snapshotting_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshottingClusterId") String.of_json
  ; automatic_failover_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AutomaticFailoverEnabled") Boolean.of_json
  ; multi_a_z_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAZEnabled") Boolean.of_json
  ; node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
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
  ; user_group_ids_to_add =
      UserGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToAdd"))
  ; user_group_ids_to_remove =
      UserGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIdsToRemove"))
  ; remove_user_groups =
      Aws.Util.option_map (Aws.Json.lookup j "RemoveUserGroups") Boolean.of_json
  ; log_delivery_configurations =
      LogDeliveryConfigurationRequestList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; ip_discovery =
      Aws.Util.option_map (Aws.Json.lookup j "IpDiscovery") IpDiscovery.of_json
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; transit_encryption_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitEncryptionMode")
        TransitEncryptionMode.of_json
  ; cluster_mode =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterMode") ClusterMode.of_json
  ; durability = Aws.Util.option_map (Aws.Json.lookup j "Durability") Durability.of_json
  }
