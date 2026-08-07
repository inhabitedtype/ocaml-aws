open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; max_records : Integer.t option
  ; marker : String.t option
  ; show_cache_node_info : Boolean.t option
  ; show_cache_clusters_not_in_replication_groups : Boolean.t option
  }

let make
    ?cache_cluster_id
    ?max_records
    ?marker
    ?show_cache_node_info
    ?show_cache_clusters_not_in_replication_groups
    () =
  { cache_cluster_id
  ; max_records
  ; marker
  ; show_cache_node_info
  ; show_cache_clusters_not_in_replication_groups
  }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; max_records = Aws.Util.option_bind (Aws.Xml.member "MaxRecords" xml) Integer.parse
    ; marker = Aws.Util.option_bind (Aws.Xml.member "Marker" xml) String.parse
    ; show_cache_node_info =
        Aws.Util.option_bind (Aws.Xml.member "ShowCacheNodeInfo" xml) Boolean.parse
    ; show_cache_clusters_not_in_replication_groups =
        Aws.Util.option_bind
          (Aws.Xml.member "ShowCacheClustersNotInReplicationGroups" xml)
          Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_cache_clusters_not_in_replication_groups (fun f ->
             Aws.Query.Pair ("ShowCacheClustersNotInReplicationGroups", Boolean.to_query f))
       ; Aws.Util.option_map v.show_cache_node_info (fun f ->
             Aws.Query.Pair ("ShowCacheNodeInfo", Boolean.to_query f))
       ; Aws.Util.option_map v.marker (fun f ->
             Aws.Query.Pair ("Marker", String.to_query f))
       ; Aws.Util.option_map v.max_records (fun f ->
             Aws.Query.Pair ("MaxRecords", Integer.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.show_cache_clusters_not_in_replication_groups (fun f ->
             "ShowCacheClustersNotInReplicationGroups", Boolean.to_json f)
       ; Aws.Util.option_map v.show_cache_node_info (fun f ->
             "ShowCacheNodeInfo", Boolean.to_json f)
       ; Aws.Util.option_map v.marker (fun f -> "Marker", String.to_json f)
       ; Aws.Util.option_map v.max_records (fun f -> "MaxRecords", Integer.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; max_records = Aws.Util.option_map (Aws.Json.lookup j "MaxRecords") Integer.of_json
  ; marker = Aws.Util.option_map (Aws.Json.lookup j "Marker") String.of_json
  ; show_cache_node_info =
      Aws.Util.option_map (Aws.Json.lookup j "ShowCacheNodeInfo") Boolean.of_json
  ; show_cache_clusters_not_in_replication_groups =
      Aws.Util.option_map
        (Aws.Json.lookup j "ShowCacheClustersNotInReplicationGroups")
        Boolean.of_json
  }
