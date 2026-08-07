open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; node_group_id : String.t option
  ; cache_node_id : String.t option
  ; node_group_configuration : NodeGroupConfiguration.t option
  ; cache_size : String.t option
  ; cache_node_create_time : DateTime.t option
  ; snapshot_create_time : DateTime.t option
  }

let make
    ?cache_cluster_id
    ?node_group_id
    ?cache_node_id
    ?node_group_configuration
    ?cache_size
    ?cache_node_create_time
    ?snapshot_create_time
    () =
  { cache_cluster_id
  ; node_group_id
  ; cache_node_id
  ; node_group_configuration
  ; cache_size
  ; cache_node_create_time
  ; snapshot_create_time
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; node_group_id = Aws.Util.option_bind (Aws.Xml.member "NodeGroupId" xml) String.parse
    ; cache_node_id = Aws.Util.option_bind (Aws.Xml.member "CacheNodeId" xml) String.parse
    ; node_group_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "NodeGroupConfiguration" xml)
          NodeGroupConfiguration.parse
    ; cache_size = Aws.Util.option_bind (Aws.Xml.member "CacheSize" xml) String.parse
    ; cache_node_create_time =
        Aws.Util.option_bind (Aws.Xml.member "CacheNodeCreateTime" xml) DateTime.parse
    ; snapshot_create_time =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotCreateTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_create_time (fun f ->
             Aws.Query.Pair ("SnapshotCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.cache_node_create_time (fun f ->
             Aws.Query.Pair ("CacheNodeCreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.cache_size (fun f ->
             Aws.Query.Pair ("CacheSize", String.to_query f))
       ; Aws.Util.option_map v.node_group_configuration (fun f ->
             Aws.Query.Pair ("NodeGroupConfiguration", NodeGroupConfiguration.to_query f))
       ; Aws.Util.option_map v.cache_node_id (fun f ->
             Aws.Query.Pair ("CacheNodeId", String.to_query f))
       ; Aws.Util.option_map v.node_group_id (fun f ->
             Aws.Query.Pair ("NodeGroupId", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_create_time (fun f ->
             "SnapshotCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.cache_node_create_time (fun f ->
             "CacheNodeCreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.cache_size (fun f -> "CacheSize", String.to_json f)
       ; Aws.Util.option_map v.node_group_configuration (fun f ->
             "NodeGroupConfiguration", NodeGroupConfiguration.to_json f)
       ; Aws.Util.option_map v.cache_node_id (fun f -> "CacheNodeId", String.to_json f)
       ; Aws.Util.option_map v.node_group_id (fun f -> "NodeGroupId", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; node_group_id = Aws.Util.option_map (Aws.Json.lookup j "NodeGroupId") String.of_json
  ; cache_node_id = Aws.Util.option_map (Aws.Json.lookup j "CacheNodeId") String.of_json
  ; node_group_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "NodeGroupConfiguration")
        NodeGroupConfiguration.of_json
  ; cache_size = Aws.Util.option_map (Aws.Json.lookup j "CacheSize") String.of_json
  ; cache_node_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "CacheNodeCreateTime") DateTime.of_json
  ; snapshot_create_time =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotCreateTime") DateTime.of_json
  }
