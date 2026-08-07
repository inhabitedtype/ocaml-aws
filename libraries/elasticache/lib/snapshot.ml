open Aws.BaseTypes

type t =
  { snapshot_name : String.t option
  ; replication_group_id : String.t option
  ; replication_group_description : String.t option
  ; cache_cluster_id : String.t option
  ; snapshot_status : String.t option
  ; snapshot_source : String.t option
  ; cache_node_type : String.t option
  ; engine : String.t option
  ; engine_version : String.t option
  ; num_cache_nodes : Integer.t option
  ; preferred_availability_zone : String.t option
  ; preferred_outpost_arn : String.t option
  ; cache_cluster_create_time : DateTime.t option
  ; preferred_maintenance_window : String.t option
  ; topic_arn : String.t option
  ; port : Integer.t option
  ; cache_parameter_group_name : String.t option
  ; cache_subnet_group_name : String.t option
  ; vpc_id : String.t option
  ; auto_minor_version_upgrade : Boolean.t option
  ; snapshot_retention_limit : Integer.t option
  ; snapshot_window : String.t option
  ; num_node_groups : Integer.t option
  ; automatic_failover : AutomaticFailoverStatus.t option
  ; node_snapshots : NodeSnapshotList.t
  ; kms_key_id : String.t option
  ; a_r_n : String.t option
  ; data_tiering : DataTieringStatus.t option
  ; durability : Durability.t option
  }

let make
    ?snapshot_name
    ?replication_group_id
    ?replication_group_description
    ?cache_cluster_id
    ?snapshot_status
    ?snapshot_source
    ?cache_node_type
    ?engine
    ?engine_version
    ?num_cache_nodes
    ?preferred_availability_zone
    ?preferred_outpost_arn
    ?cache_cluster_create_time
    ?preferred_maintenance_window
    ?topic_arn
    ?port
    ?cache_parameter_group_name
    ?cache_subnet_group_name
    ?vpc_id
    ?auto_minor_version_upgrade
    ?snapshot_retention_limit
    ?snapshot_window
    ?num_node_groups
    ?automatic_failover
    ?(node_snapshots = [])
    ?kms_key_id
    ?a_r_n
    ?data_tiering
    ?durability
    () =
  { snapshot_name
  ; replication_group_id
  ; replication_group_description
  ; cache_cluster_id
  ; snapshot_status
  ; snapshot_source
  ; cache_node_type
  ; engine
  ; engine_version
  ; num_cache_nodes
  ; preferred_availability_zone
  ; preferred_outpost_arn
  ; cache_cluster_create_time
  ; preferred_maintenance_window
  ; topic_arn
  ; port
  ; cache_parameter_group_name
  ; cache_subnet_group_name
  ; vpc_id
  ; auto_minor_version_upgrade
  ; snapshot_retention_limit
  ; snapshot_window
  ; num_node_groups
  ; automatic_failover
  ; node_snapshots
  ; kms_key_id
  ; a_r_n
  ; data_tiering
  ; durability
  }

let parse xml =
  Some
    { snapshot_name =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
    ; replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; replication_group_description =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplicationGroupDescription" xml)
          String.parse
    ; cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; snapshot_status =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotStatus" xml) String.parse
    ; snapshot_source =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotSource" xml) String.parse
    ; cache_node_type =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeType" xml) String.parse
    ; engine = Aws.Util.option_bind (Aws.Xml.member "Engine" xml) String.parse
    ; engine_version =
        Aws.Util.option_bind (Aws.Xml.member "EngineVersion" xml) String.parse
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
    ; topic_arn = Aws.Util.option_bind (Aws.Xml.member "TopicArn" xml) String.parse
    ; port = Aws.Util.option_bind (Aws.Xml.member "Port" xml) Integer.parse
    ; cache_parameter_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheParameterGroupName" xml) String.parse
    ; cache_subnet_group_name =
        Aws.Util.option_bind (Aws.Xml.member "CacheSubnetGroupName" xml) String.parse
    ; vpc_id = Aws.Util.option_bind (Aws.Xml.member "VpcId" xml) String.parse
    ; auto_minor_version_upgrade =
        Aws.Util.option_bind (Aws.Xml.member "AutoMinorVersionUpgrade" xml) Boolean.parse
    ; snapshot_retention_limit =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotRetentionLimit" xml) Integer.parse
    ; snapshot_window =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotWindow" xml) String.parse
    ; num_node_groups =
        Aws.Util.option_bind (Aws.Xml.member "NumNodeGroups" xml) Integer.parse
    ; automatic_failover =
        Aws.Util.option_bind
          (Aws.Xml.member "AutomaticFailover" xml)
          AutomaticFailoverStatus.parse
    ; node_snapshots =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "NodeSnapshots" xml)
             NodeSnapshotList.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    ; data_tiering =
        Aws.Util.option_bind (Aws.Xml.member "DataTiering" xml) DataTieringStatus.parse
    ; durability = Aws.Util.option_bind (Aws.Xml.member "Durability" xml) Durability.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f ->
             Aws.Query.Pair ("Durability", Durability.to_query f))
       ; Aws.Util.option_map v.data_tiering (fun f ->
             Aws.Query.Pair ("DataTiering", DataTieringStatus.to_query f))
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("NodeSnapshots.member", NodeSnapshotList.to_query v.node_snapshots))
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             Aws.Query.Pair ("AutomaticFailover", AutomaticFailoverStatus.to_query f))
       ; Aws.Util.option_map v.num_node_groups (fun f ->
             Aws.Query.Pair ("NumNodeGroups", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             Aws.Query.Pair ("SnapshotWindow", String.to_query f))
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             Aws.Query.Pair ("SnapshotRetentionLimit", Integer.to_query f))
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             Aws.Query.Pair ("AutoMinorVersionUpgrade", Boolean.to_query f))
       ; Aws.Util.option_map v.vpc_id (fun f ->
             Aws.Query.Pair ("VpcId", String.to_query f))
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             Aws.Query.Pair ("CacheSubnetGroupName", String.to_query f))
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             Aws.Query.Pair ("CacheParameterGroupName", String.to_query f))
       ; Aws.Util.option_map v.port (fun f -> Aws.Query.Pair ("Port", Integer.to_query f))
       ; Aws.Util.option_map v.topic_arn (fun f ->
             Aws.Query.Pair ("TopicArn", String.to_query f))
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
       ; Aws.Util.option_map v.engine_version (fun f ->
             Aws.Query.Pair ("EngineVersion", String.to_query f))
       ; Aws.Util.option_map v.engine (fun f ->
             Aws.Query.Pair ("Engine", String.to_query f))
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             Aws.Query.Pair ("CacheNodeType", String.to_query f))
       ; Aws.Util.option_map v.snapshot_source (fun f ->
             Aws.Query.Pair ("SnapshotSource", String.to_query f))
       ; Aws.Util.option_map v.snapshot_status (fun f ->
             Aws.Query.Pair ("SnapshotStatus", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ; Aws.Util.option_map v.replication_group_description (fun f ->
             Aws.Query.Pair ("ReplicationGroupDescription", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ; Aws.Util.option_map v.snapshot_name (fun f ->
             Aws.Query.Pair ("SnapshotName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.durability (fun f -> "Durability", Durability.to_json f)
       ; Aws.Util.option_map v.data_tiering (fun f ->
             "DataTiering", DataTieringStatus.to_json f)
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some ("NodeSnapshots", NodeSnapshotList.to_json v.node_snapshots)
       ; Aws.Util.option_map v.automatic_failover (fun f ->
             "AutomaticFailover", AutomaticFailoverStatus.to_json f)
       ; Aws.Util.option_map v.num_node_groups (fun f ->
             "NumNodeGroups", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_window (fun f ->
             "SnapshotWindow", String.to_json f)
       ; Aws.Util.option_map v.snapshot_retention_limit (fun f ->
             "SnapshotRetentionLimit", Integer.to_json f)
       ; Aws.Util.option_map v.auto_minor_version_upgrade (fun f ->
             "AutoMinorVersionUpgrade", Boolean.to_json f)
       ; Aws.Util.option_map v.vpc_id (fun f -> "VpcId", String.to_json f)
       ; Aws.Util.option_map v.cache_subnet_group_name (fun f ->
             "CacheSubnetGroupName", String.to_json f)
       ; Aws.Util.option_map v.cache_parameter_group_name (fun f ->
             "CacheParameterGroupName", String.to_json f)
       ; Aws.Util.option_map v.port (fun f -> "Port", Integer.to_json f)
       ; Aws.Util.option_map v.topic_arn (fun f -> "TopicArn", String.to_json f)
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
       ; Aws.Util.option_map v.engine_version (fun f -> "EngineVersion", String.to_json f)
       ; Aws.Util.option_map v.engine (fun f -> "Engine", String.to_json f)
       ; Aws.Util.option_map v.cache_node_type (fun f ->
             "CacheNodeType", String.to_json f)
       ; Aws.Util.option_map v.snapshot_source (fun f ->
             "SnapshotSource", String.to_json f)
       ; Aws.Util.option_map v.snapshot_status (fun f ->
             "SnapshotStatus", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ; Aws.Util.option_map v.replication_group_description (fun f ->
             "ReplicationGroupDescription", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
       ])

let of_json j =
  { snapshot_name = Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
  ; replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; replication_group_description =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupDescription") String.of_json
  ; cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; snapshot_status =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotStatus") String.of_json
  ; snapshot_source =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotSource") String.of_json
  ; cache_node_type =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeType") String.of_json
  ; engine = Aws.Util.option_map (Aws.Json.lookup j "Engine") String.of_json
  ; engine_version =
      Aws.Util.option_map (Aws.Json.lookup j "EngineVersion") String.of_json
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
  ; topic_arn = Aws.Util.option_map (Aws.Json.lookup j "TopicArn") String.of_json
  ; port = Aws.Util.option_map (Aws.Json.lookup j "Port") Integer.of_json
  ; cache_parameter_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheParameterGroupName") String.of_json
  ; cache_subnet_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroupName") String.of_json
  ; vpc_id = Aws.Util.option_map (Aws.Json.lookup j "VpcId") String.of_json
  ; auto_minor_version_upgrade =
      Aws.Util.option_map (Aws.Json.lookup j "AutoMinorVersionUpgrade") Boolean.of_json
  ; snapshot_retention_limit =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotRetentionLimit") Integer.of_json
  ; snapshot_window =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotWindow") String.of_json
  ; num_node_groups =
      Aws.Util.option_map (Aws.Json.lookup j "NumNodeGroups") Integer.of_json
  ; automatic_failover =
      Aws.Util.option_map
        (Aws.Json.lookup j "AutomaticFailover")
        AutomaticFailoverStatus.of_json
  ; node_snapshots =
      NodeSnapshotList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NodeSnapshots"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  ; data_tiering =
      Aws.Util.option_map (Aws.Json.lookup j "DataTiering") DataTieringStatus.of_json
  ; durability = Aws.Util.option_map (Aws.Json.lookup j "Durability") Durability.of_json
  }
