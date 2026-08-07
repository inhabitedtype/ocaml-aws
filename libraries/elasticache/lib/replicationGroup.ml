open Aws.BaseTypes

type t =
  { replication_group_id : String.t option
  ; description : String.t option
  ; global_replication_group_info : GlobalReplicationGroupInfo.t option
  ; status : String.t option
  ; pending_modified_values : ReplicationGroupPendingModifiedValues.t option
  ; member_clusters : ClusterIdList.t
  ; node_groups : NodeGroupList.t
  ; snapshotting_cluster_id : String.t option
  ; automatic_failover : AutomaticFailoverStatus.t option
  ; multi_a_z : MultiAZStatus.t option
  ; configuration_endpoint : Endpoint.t option
  ; snapshot_retention_limit : Integer.t option
  ; snapshot_window : String.t option
  ; cluster_enabled : Boolean.t option
  ; cache_node_type : String.t option
  ; auth_token_enabled : Boolean.t option
  ; auth_token_last_modified_date : DateTime.t option
  ; transit_encryption_enabled : Boolean.t option
  ; at_rest_encryption_enabled : Boolean.t option
  ; member_clusters_outpost_arns : ReplicationGroupOutpostArnList.t
  ; kms_key_id : String.t option
  ; storage_encryption_type : StorageEncryptionType.t option
  ; a_r_n : String.t option
  ; user_group_ids : UserGroupIdList.t
  ; log_delivery_configurations : LogDeliveryConfigurationList.t
  ; replication_group_create_time : DateTime.t option
  ; data_tiering : DataTieringStatus.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; network_type : NetworkType.t option
  ; ip_discovery : IpDiscovery.t option
  ; transit_encryption_mode : TransitEncryptionMode.t option
  ; cluster_mode : ClusterMode.t option
  ; engine : String.t option
  ; durability : Durability.t option
  ; effective_durability : EffectiveDurability.t option
  }

let make
    ?replication_group_id
    ?description
    ?global_replication_group_info
    ?status
    ?pending_modified_values
    ?(member_clusters = [])
    ?(node_groups = [])
    ?snapshotting_cluster_id
    ?automatic_failover
    ?multi_a_z
    ?configuration_endpoint
    ?snapshot_retention_limit
    ?snapshot_window
    ?cluster_enabled
    ?cache_node_type
    ?auth_token_enabled
    ?auth_token_last_modified_date
    ?transit_encryption_enabled
    ?at_rest_encryption_enabled
    ?(member_clusters_outpost_arns = [])
    ?kms_key_id
    ?storage_encryption_type
    ?a_r_n
    ?(user_group_ids = [])
    ?(log_delivery_configurations = [])
    ?replication_group_create_time
    ?data_tiering
    ?auto_minor_version_upgrade
    ?network_type
    ?ip_discovery
    ?transit_encryption_mode
    ?cluster_mode
    ?engine
    ?durability
    ?effective_durability
    () =
  { replication_group_id
  ; description
  ; global_replication_group_info
  ; status
  ; pending_modified_values
  ; member_clusters
  ; node_groups
  ; snapshotting_cluster_id
  ; automatic_failover
  ; multi_a_z
  ; configuration_endpoint
  ; snapshot_retention_limit
  ; snapshot_window
  ; cluster_enabled
  ; cache_node_type
  ; auth_token_enabled
  ; auth_token_last_modified_date
  ; transit_encryption_enabled
  ; at_rest_encryption_enabled
  ; member_clusters_outpost_arns
  ; kms_key_id
  ; storage_encryption_type
  ; a_r_n
  ; user_group_ids
  ; log_delivery_configurations
  ; replication_group_create_time
  ; data_tiering
  ; auto_minor_version_upgrade
  ; network_type
  ; ip_discovery
  ; transit_encryption_mode
  ; cluster_mode
  ; engine
  ; durability
  ; effective_durability
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "Description" xml) String.parse
    ; global_replication_group_info =
        Aws.Util.option_bind
          (Aws.Xml.member "GlobalReplicationGroupInfo" xml)
          GlobalReplicationGroupInfo.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; pending_modified_values =
        Aws.Util.option_bind
          (Aws.Xml.member "PendingModifiedValues" xml)
          ReplicationGroupPendingModifiedValues.parse
    ; member_clusters =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "MemberClusters" xml) ClusterIdList.parse)
    ; node_groups =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "NodeGroups" xml) NodeGroupList.parse)
    ; snapshotting_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "SnapshottingClusterId" xml) String.parse
    ; automatic_failover =
        Aws.Util.option_bind
          (Aws.Xml.member "AutomaticFailover" xml)
          AutomaticFailoverStatus.parse
    ; multi_a_z = Aws.Util.option_bind (Aws.Xml.member "MultiAZ" xml) MultiAZStatus.parse
    ; configuration_endpoint =
        Aws.Util.option_bind (Aws.Xml.member "ConfigurationEndpoint" xml) Endpoint.parse
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; snapshot_window =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
    ; cluster_enabled =
        Aws.Util.option_bind (Aws.Xml.member "ClusterEnabled" xml) Boolean.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
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
    ; member_clusters_outpost_arns =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "MemberClustersOutpostArns" xml)
             ReplicationGroupOutpostArnList.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; storage_encryption_type =
        Aws.Util.option_bind
          (Aws.Xml.member "StorageEncryptionType" xml)
          StorageEncryptionType.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    ; user_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserGroupIds" xml) UserGroupIdList.parse)
    ; log_delivery_configurations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LogDeliveryConfigurations" xml)
             LogDeliveryConfigurationList.parse)
    ; replication_group_create_time =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationGroupCreateTime" xml)
          DateTime.parse
    ; data_tiering =
        Aws.Util.option_bind (Aws.Xml.member "DataTiering" xml) DataTieringStatus.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
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
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; durability = Aws.Util.option_bind (Aws.Xml.member "Durability" xml) Durability.parse
    ; effective_durability =
        Aws.Util.option_bind
          (Aws.Xml.member "EffectiveDurability" xml)
          EffectiveDurability.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.effective_durability (fun f ->
             Aws.Query.Pair ("EffectiveDurability", EffectiveDurability.to_query f))
       ; Aws.Util.option_map v.durability (fun f ->
             Aws.Query.Pair ("Durability", Durability.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             Aws.Query.Pair ("ClusterMode", ClusterMode.to_query f))
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             Aws.Query.Pair ("TransitEncryptionMode", TransitEncryptionMode.to_query f))
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             Aws.Query.Pair ("IpDiscovery", IpDiscovery.to_query f))
       ; Aws.Util.option_map v.network_type (fun f ->
             Aws.Query.Pair ("NetworkType", NetworkType.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.data_tiering (fun f ->
             Aws.Query.Pair ("DataTiering", DataTieringStatus.to_query f))
       ; Aws.Util.option_map v.replication_group_create_time (fun f ->
             Aws.Query.Pair ("ReplicationGroupCreateTime", DateTime.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LogDeliveryConfigurations.member"
              , LogDeliveryConfigurationList.to_query v.log_delivery_configurations ))
       ; Some
           (Aws.Query.Pair
              ("UserGroupIds.member", UserGroupIdList.to_query v.user_group_ids))
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             Aws.Query.Pair ("StorageEncryptionType", StorageEncryptionType.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "MemberClustersOutpostArns.member"
              , ReplicationGroupOutpostArnList.to_query v.member_clusters_outpost_arns ))
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             Aws.Query.Pair ("AtRestEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             Aws.Query.Pair ("TransitEncryptionEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
             Aws.Query.Pair ("AuthTokenLastModifiedDate", DateTime.to_query f))
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             Aws.Query.Pair ("AuthTokenEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.cluster_enabled (fun f ->
             Aws.Query.Pair ("ClusterEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             Aws.Query.Pair ("SnapshotWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Aws.Util.option_map v.configuration_endpoint (fun f ->
             Aws.Query.Pair ("ConfigurationEndpoint", Endpoint.to_query f))
       ; Aws.Util.option_map v.multi_a_z (fun f ->
             Aws.Query.Pair ("MultiAZ", MultiAZStatus.to_query f))
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
       ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
             Aws.Query.Pair ("SnapshottingClusterId", String.to_query f))
       ; Some (Aws.Query.Pair ("NodeGroups.member", NodeGroupList.to_query v.node_groups))
       ; Some
           (Aws.Query.Pair
              ("MemberClusters.member", ClusterIdList.to_query v.member_clusters))
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             Aws.Query.Pair
               ("PendingModifiedValues", ReplicationGroupPendingModifiedValues.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.global_replication_group_info (fun f ->
             Aws.Query.Pair
               ("GlobalReplicationGroupInfo", GlobalReplicationGroupInfo.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.effective_durability (fun f ->
             "EffectiveDurability", EffectiveDurability.to_json f)
       ; Aws.Util.option_map v.durability (fun f -> "Durability", Durability.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cluster_mode (fun f ->
             "ClusterMode", ClusterMode.to_json f)
       ; Aws.Util.option_map v.transit_encryption_mode (fun f ->
             "TransitEncryptionMode", TransitEncryptionMode.to_json f)
       ; Aws.Util.option_map v.ip_discovery (fun f ->
             "IpDiscovery", IpDiscovery.to_json f)
       ; Aws.Util.option_map v.network_type (fun f ->
             "NetworkType", NetworkType.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.data_tiering (fun f ->
             "DataTiering", DataTieringStatus.to_json f)
       ; Aws.Util.option_map v.replication_group_create_time (fun f ->
             "ReplicationGroupCreateTime", DateTime.to_json f)
       ; Some
           ( "LogDeliveryConfigurations"
           , LogDeliveryConfigurationList.to_json v.log_delivery_configurations )
       ; Some ("UserGroupIds", UserGroupIdList.to_json v.user_group_ids)
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.storage_encryption_type (fun f ->
             "StorageEncryptionType", StorageEncryptionType.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some
           ( "MemberClustersOutpostArns"
           , ReplicationGroupOutpostArnList.to_json v.member_clusters_outpost_arns )
       ; Aws.Util.option_map v.at_rest_encryption_enabled (fun f ->
             "AtRestEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.transit_encryption_enabled (fun f ->
             "TransitEncryptionEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.auth_token_last_modified_date (fun f ->
             "AuthTokenLastModifiedDate", DateTime.to_json f)
       ; Aws.Util.option_map v.auth_token_enabled (fun f ->
             "AuthTokenEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.cluster_enabled (fun f ->
             "ClusterEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             "SnapshotWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Aws.Util.option_map v.configuration_endpoint (fun f ->
             "ConfigurationEndpoint", Endpoint.to_json f)
       ; Aws.Util.option_map v.multi_a_z (fun f -> "MultiAZ", MultiAZStatus.to_json f)
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             "AutomaticFailover", AutomaticFailoverStatus.to_json f)
       ; Aws.Util.option_map v.snapshotting_cluster_id (fun f ->
             "SnapshottingClusterId", String.to_json f)
       ; Some ("NodeGroups", NodeGroupList.to_json v.node_groups)
       ; Some ("MemberClusters", ClusterIdList.to_json v.member_clusters)
       ; Aws.Util.option_map v.pending_modified_values (fun f ->
             "PendingModifiedValues", ReplicationGroupPendingModifiedValues.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.global_replication_group_info (fun f ->
             "GlobalReplicationGroupInfo", GlobalReplicationGroupInfo.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "Description", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "Description") String.of_json
  ; global_replication_group_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "GlobalReplicationGroupInfo")
        GlobalReplicationGroupInfo.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; pending_modified_values =
      Aws.Util.option_map
        (Aws.Json.lookup j "PendingModifiedValues")
        ReplicationGroupPendingModifiedValues.of_json
  ; member_clusters =
      ClusterIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "MemberClusters"))
  ; node_groups =
      NodeGroupList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeGroups"))
  ; snapshotting_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshottingClusterId") String.of_json
  ; automatic_failover =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutomaticFailover")
        AutomaticFailoverStatus.of_json
  ; multi_a_z = Aws.Util.option_map (Aws.Json.lookup j "MultiAZ") MultiAZStatus.of_json
  ; configuration_endpoint =
      Aws.Util.option_map (Aws.Json.lookup j "ConfigurationEndpoint") Endpoint.of_json
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; snapshot_window =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
  ; cluster_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "ClusterEnabled") Boolean.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; auth_token_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AuthTokenEnabled") Boolean.of_json
  ; auth_token_last_modified_date =
      Aws.Util.option_map (Aws.Json.lookup j "AuthTokenLastModifiedDate") DateTime.of_json
  ; transit_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "TransitEncryptionEnabled") Boolean.of_json
  ; at_rest_encryption_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "AtRestEncryptionEnabled") Boolean.of_json
  ; member_clusters_outpost_arns =
      ReplicationGroupOutpostArnList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "MemberClustersOutpostArns"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; storage_encryption_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "StorageEncryptionType")
        StorageEncryptionType.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  ; user_group_ids =
      UserGroupIdList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroupIds"))
  ; log_delivery_configurations =
      LogDeliveryConfigurationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LogDeliveryConfigurations"))
  ; replication_group_create_time =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReplicationGroupCreateTime")
        DateTime.of_json
  ; data_tiering =
      Aws.Util.option_map (Aws.Json.lookup j "DataTiering") DataTieringStatus.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
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
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; durability = Aws.Util.option_map (Aws.Json.lookup j "Durability") Durability.of_json
  ; effective_durability =
      Aws.Util.option_map
        (Aws.Json.lookup j "EffectiveDurability")
        EffectiveDurability.of_json
  }
