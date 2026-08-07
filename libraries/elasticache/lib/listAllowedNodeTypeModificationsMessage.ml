open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t option
  ; replication_group_id : String.t option
  }

let make ?cache_cluster_id ?replication_group_id () =
  { cache_cluster_id; replication_group_id }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse
    ; replication_group_id =
        Aws.Util.option_bind (Aws.Xml.member "ReplicationGroupId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replication_group_id (fun f ->
             Aws.Query.Pair ("ReplicationGroupId", String.to_query f))
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             Aws.Query.Pair ("CacheClusterId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.replication_group_id (fun f ->
             "ReplicationGroupId", String.to_json f)
       ; Aws.Util.option_map v.cache_cluster_id (fun f ->
             "CacheClusterId", String.to_json f)
       ])

let of_json j =
  { cache_cluster_id =
      Aws.Util.option_map (Aws.Json.lookup j "CacheClusterId") String.of_json
  ; replication_group_id =
      Aws.Util.option_map (Aws.Json.lookup j "ReplicationGroupId") String.of_json
  }
