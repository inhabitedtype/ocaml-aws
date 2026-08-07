open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; configuration_endpoint : Endpoint.t option
  ; client_download_landing_page : String.t option
  ; cache_node_type : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; cache_cluster_status : String.t option
  ; num_cache_nodes : Integer.t option
  ; preferred_availability_zone : String.t option
  ; preferred_outpost_arn : String.t option
  ; cache_cluster_create_time : DateTime.t option
  ; preferred_maintenance_window : String.t option
  ; pending_modified_values : PendingModifiedValues.t option
  ; notification_configuration : NotificationConfiguration.t option
  ; cache_security_groups : CacheSecurityGroupMembershipList.t
  ; cache_parameter_group : CacheParameterGroupStatus.t option
  ; cache_subnet_group_name : String.t option
  ; cache_nodes : CacheNodeList.t
  ; auto_minor_version_upgrade : Boolean.t option
  ; security_groups : SecurityGroupMembershipList.t
  ; replication_group_id : String.t option
  ; snapshot_retention_limit : Integer.t option
  ; snapshot_window : String.t option
  ; auth_token_enabled : Boolean.t option
  ; auth_token_last_modified_date : DateTime.t option
  ; transit_encryption_enabled : Boolean.t option
  ; at_rest_encryption_enabled : Boolean.t option
  ; a_r_n : String.t option
  ; replication_group_log_delivery_enabled : Boolean.t option
  ; log_delivery_configurations : LogDeliveryConfigurationList.t
  ; network_type : NetworkType.t option
  ; ip_discovery : IpDiscovery.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  }

let make
    ?cache_cluster_id
    ?configuration_endpoint
    ?client_download_landing_page
    ?cache_node_type
    ?engine
    ?engine_version
    ?cache_cluster_status
    ?num_cache_nodes
    ?preferred_availability_zone
    ?preferred_outpost_arn
    ?cache_cluster_create_time
    ?preferred_maintenance_window
    ?pending_modified_values
    ?notification_configuration
    ?(cache_security_groups = [])
    ?cache_parameter_group
    ?cache_subnet_group_name
    ?(cache_nodes = [])
    ?auto_minor_version_upgrade
    ?(security_groups = [])
    ?replication_group_id
    ?snapshot_retention_limit
    ?snapshot_window
    ?auth_token_enabled
    ?auth_token_last_modified_date
    ?transit_encryption_enabled
    ?at_rest_encryption_enabled
    ?a_r_n
    ?replication_group_log_delivery_enabled
    ?(log_delivery_configurations = [])
    ?network_type
    ?ip_discovery
    ?transit_encryption_mode
    () =
  { cache_cluster_id
  ; configuration_endpoint
  ; client_download_landing_page
  ; cache_node_type
  ; engine
  ; engine_version
  ; cache_cluster_status
  ; num_cache_nodes
  ; preferred_availability_zone
  ; preferred_outpost_arn
  ; cache_cluster_create_time
  ; preferred_maintenance_window
  ; pending_modified_values
  ; notification_configuration
  ; cache_security_groups
  ; cache_parameter_group
  ; cache_subnet_group_name
  ; cache_nodes
  ; auto_minor_version_upgrade
  ; security_groups
  ; replication_group_id
  ; snapshot_retention_limit
  ; snapshot_window
  ; auth_token_enabled
  ; auth_token_last_modified_date
  ; transit_encryption_enabled
  ; at_rest_encryption_enabled
  ; a_r_n
  ; replication_group_log_delivery_enabled
  ; log_delivery_configurations
  ; network_type
  ; ip_discovery
  ; transit_encryption_mode
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; configuration_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ConfigurationEndpoint" xml) Endpoint.parse
    ; client_download_landing_page =
        Aws.Util.option_bind (Aws.Xml.member "ClientDownloadLandingPage" xml) String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
    ; cache_cluster_status =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterStatus" xml) String.parse
    ; num_cache_nodes =
        Aws.Util.option_bind (Aws.Xml.member "NumCacheNodes" xml) Integer.parse
    ; preferred_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "PreferredAvailabilityZone" xml) String.parse
    ; preferred_outpost_arn =
        Aws.Util.option_bind (Aws.Xml.member "PreferredOutpostArn" xml) String.parse
    ; cache_cluster_create_time =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterCreateTime" xml) DateTime.parse
    ; preferred_maintenance_window =
        Aws.Util.option_bind
          (Aws.Xml.member "PreferredMaintenanceWindow" xml)
          String.parse
    ; pending_modified_values =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingModifiedValues" xml)
          PendingModifiedValues.parse
    ; notification_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "NotificationConfiguration" xml)
          NotificationConfiguration.parse
    ; cache_security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheSecurityGroups" xml)
             CacheSecurityGroupMembershipList.parse)
    ; cache_parameter_group =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheParameterGroup" xml)
          CacheParameterGroupStatus.parse
    ; cache_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
    ; cache_nodes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "CacheNodes" xml) CacheNodeList.parse)
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; security_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "SecurityGroups" xml)
             SecurityGroupMembershipList.parse)
    ; replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; snapshot_window =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
    ; auth_token_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AuthTokenEnabled" xml) Boolean.parse
    ; auth_token_last_modified_date =
        Aws.Util.option_bind
          (Aws.Xml.member "AuthTokenLastModifiedDate" xml)
          DateTime.parse
    ; transit_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "TransitEncryptionEnabled" xml) Boolean.parse
    ; at_rest_encryption_enabled =
        Aws.Util.option_bind (Aws.Xml.member "AtRestEncryptionEnabled" xml) Boolean.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    ; replication_group_log_delivery_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationGroupLogDeliveryEnabled" xml)
          Boolean.parse
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationList.parse)
    ; network_type =
        Aws.Util.option_bind (Aws.Xml.member "NetworkType" xml) NetworkType.parse
    ; ip_discovery =
        Aws.Util.option_bind (Aws.Xml.member "IpDiscovery" xml) IpDiscovery.parse
    ; transit_encryption_mode =
        Aws.Util.option_bind
          (Aws.Xml.member "TransitEncryptionMode" xml)
          TransitEncryptionMode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", NetworkType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationList.to_query v.log_delivery_configurations ))
       ; Aws.Util.option_map v.replication_group_log_delivery_enabled (fun f ->
             Aws.Query.Pair ("ReplicationGroupLogDeliveryEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
             Aws.Query.Pair ("AuthTokenLastModifiedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             Aws.Query.Pair ("SnapshotWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "SecurityGroups.member"
              , SecurityGroupMembershipList.to_query v.security_groups ))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("CacheNodes.member", CacheNodeList.to_query v.cache_nodes))
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group (fun f ->
             Aws.Query.Pair ("CacheParameterGroup", CacheParameterGroupStatus.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CacheSecurityGroups.member"
              , CacheSecurityGroupMembershipList.to_query v.cache_security_groups ))
       ; Aws.Util.option_map v.notification_configuration (fun f ->
             Aws.Query.Pair
               ("NotificationConfiguration", NotificationConfiguration.to_query f))
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             Aws.Query.Pair ("PendingModifiedValues", PendingModifiedValues.to_query f))
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             Aws.Query.Pair ("PreferredMaintenanceWindow", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
             Aws.Query.Pair ("CacheClusterCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             Aws.Query.Pair ("PreferredOutpostArn", String.to_query f))
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             Aws.Query.Pair ("PreferredAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             Aws.Query.Pair ("NumCacheNodes", Integer.to_query f))
       ; Aws.Util.option_map v.cache_cluster_status (fun f ->
             Aws.Query.Pair ("CacheClusterStatus", String.to_query f))
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.client_download_landing_page (fun f ->
             Aws.Query.Pair ("ClientDownloadLandingPage", String.to_query f))
       ; Aws.Util.option_map v.configuration_endpoint (fun f ->
             Aws.Query.Pair ("ConfigurationEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Aws.Util.option_map v.network_type (fun f ->
             "NetworkType", NetworkType.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationList.to_json v.log_delivery_configurations )
       ; Aws.Util.option_map v.replication_group_log_delivery_enabled (fun f ->
             "ReplicationGroupLogDeliveryEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             "AtRestEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
             "AuthTokenLastModifiedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             "AuthTokenEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             "SnapshotWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ; Some ("SecurityGroups", SecurityGroupMembershipList.to_json v.security_groups)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Some ("CacheNodes", CacheNodeList.to_json v.cache_nodes)
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             "CacheSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group (fun f ->
             "CacheParameterGroup", CacheParameterGroupStatus.to_json f)
       ; Some
           ( "CacheSecurityGroups"
           , CacheSecurityGroupMembershipList.to_json v.cache_security_groups )
       ; Aws.Util.option_map v.notification_configuration (fun f ->
             "NotificationConfiguration", NotificationConfiguration.to_json f)
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             "PendingModifiedValues", PendingModifiedValues.to_json f)
       ; Aws.Util.option_map v.preferred_maintenance_window (fun f ->
             "PreferredMaintenanceWindow", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_create_time (fun f ->
             "CacheClusterCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.preferred_outpost_arn (fun f ->
             "PreferredOutpostArn", String.to_json f)
       ; Aws.Util.option_map v.preferred_availability_zone (fun f ->
             "PreferredAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.num_cache_nodes (fun f ->
             "NumCacheNodes", Integer.to_json f)
       ; Aws.Util.option_map v.cache_cluster_status (fun f ->
             "CacheClusterStatus", String.to_json f)
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.client_download_landing_page (fun f ->
             "ClientDownloadLandingPage", String.to_json f)
       ; Aws.Util.option_map v.configuration_endpoint (fun f ->
             "ConfigurationEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; configuration_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ConfigurationEndpoint") Endpoint.of_json
  ; client_download_landing_page =
      Aws.Util.option_map (Aws.Json.lookup j "ClientDownloadLandingPage") String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
  ; cache_cluster_status =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterStatus") String.of_json
  ; num_cache_nodes =
      Aws.Util.option_map (Aws.Json.lookup j "NumCacheNodes") Integer.of_json
  ; preferred_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredAvailabilityZone") String.of_json
  ; preferred_outpost_arn =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredOutpostArn") String.of_json
  ; cache_cluster_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterCreateTime") DateTime.of_json
  ; preferred_maintenance_window =
      Aws.Util.option_map (Aws.Json.lookup j "PreferredMaintenanceWindow") String.of_json
  ; pending_modified_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingModifiedValues")
        PendingModifiedValues.of_json
  ; notification_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "NotificationConfiguration")
        NotificationConfiguration.of_json
  ; cache_security_groups =
      CacheSecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheSecurityGroups"))
  ; cache_parameter_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "CacheParameterGroup")
        CacheParameterGroupStatus.of_json
  ; cache_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
  ; cache_nodes =
      CacheNodeList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodes"))
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; security_groups =
      SecurityGroupMembershipList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SecurityGroups"))
  ; replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; snapshot_window =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
  ; auth_token_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
  ; auth_token_last_modified_date =
      Aws.Util.option_map (Aws.Json.lookup j "AuthTokenLastModifiedDate") DateTime.of_json
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; at_rest_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  ; replication_group_log_delivery_enabled =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReplicationGroupLogDeliveryEnabled")
        Boolean.of_json
  ; log_delivery_configurations =
      LogDeliveryConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; network_type =
      Aws.Util.option_map (Aws.Json.lookup j "NetworkType") NetworkType.of_json
  ; ip_discovery =
      Aws.Util.option_map (Aws.Json.lookup j "IpDiscovery") IpDiscovery.of_json
  ; transit_encryption_mode =
      Aws.Util.option_map
        (Aws.Json.lookup j "TransitEncryptionMode")
        TransitEncryptionMode.of_json
  }
