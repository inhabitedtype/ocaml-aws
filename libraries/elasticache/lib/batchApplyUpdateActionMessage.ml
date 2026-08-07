open Aws.BaseTypes

type t =
  { replication_group_ids : ReplicationGroupIdList.t
  ; cache_cluster_ids : CacheClusterIdList.t
  ; service_update_name : String.t
  }

let make ?(replication_group_ids = []) ?(cache_cluster_ids = []) ~service_update_name () =
  { replication_group_ids; cache_cluster_ids; service_update_name }

let parse xml =
  Some
    { replication_group_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ReplicationGroupIds" xml)
             ReplicationGroupIdList.parse)
    ; cache_cluster_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheClusterIds" xml)
             CacheClusterIdList.parse)
    ; service_update_name =
        Aws.Xml.required
          "ServiceUpdateName"
          (Aws.Util.option_bind (Aws.Xml.member "ServiceUpdateName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair ("ServiceUpdateName", String.to_query v.service_update_name))
       ; Some
           (Aws.Query.Pair
              ("CacheClusterIds.member", CacheClusterIdList.to_query v.cache_cluster_ids))
       ; Some
           (Aws.Query.Pair
              ( "ReplicationGroupIds.member"
              , ReplicationGroupIdList.to_query v.replication_group_ids ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ServiceUpdateName", String.to_json v.service_update_name)
       ; Some ("CacheClusterIds", CacheClusterIdList.to_json v.cache_cluster_ids)
       ; Some
           ("ReplicationGroupIds", ReplicationGroupIdList.to_json v.replication_group_ids)
       ])

let of_json j =
  { replication_group_ids =
      ReplicationGroupIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ReplicationGroupIds"))
  ; cache_cluster_ids =
      CacheClusterIdList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterIds"))
  ; service_update_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServiceUpdateName"))
  }
