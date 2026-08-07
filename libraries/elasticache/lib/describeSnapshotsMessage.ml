open Aws.BaseTypes

type t =
  { replication_group_id : String.t option
  ; cache_cluster_id : String.t option
  ; snapshot_name : String.t option
  ; snapshot_source : String.t option
  ; marker : String.t option
  ; max_records : Integer.t option
  ; show_node_group_config : Boolean.t option
  }

let make
    ?replication_group_id
    ?cache_cluster_id
    ?snapshot_name
    ?snapshot_source
    ?marker
    ?max_records
    ?show_node_group_config
    () =
  { replication_group_id
  ; cache_cluster_id
  ; snapshot_name
  ; snapshot_source
  ; marker
  ; max_records
  ; show_node_group_config
  }

let parse xml =
  Some
    { replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    ; cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; snapshot_name =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse
    ; snapshot_source =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotSource" xml) String.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; show_node_group_config =
        Aws.Util.option_bind (Aws.Xml.member "ShowNodeGroupConfig" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_node_group_config (fun f ->
             Aws.Query.Pair ("ShowNodeGroupConfig", Boolean.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.snapshot_source (fun f ->
             Aws.Query.Pair ("SnapshotSource", String.to_query f))
       ; Aws.Util.option_map v.snapshot_name (fun f ->
             Aws.Query.Pair ("SnapshotName", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_node_group_config (fun f ->
             "ShowNodeGroupConfig", Boolean.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.snapshot_source (fun f ->
             "SnapshotSource", String.to_json f)
       ; Aws.Util.option_map v.snapshot_name (fun f -> "SnapshotName", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ; Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ])

let of_json j =
  { replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  ; cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; snapshot_name = Aws.Util.option_map (Aws.Json.lookup j "SnapshotName") String.of_json
  ; snapshot_source =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotSource") String.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; show_node_group_config =
      Aws.Util.option_map (Aws.Json.lookup j "ShowNodeGroupConfig") Boolean.of_json
  }
