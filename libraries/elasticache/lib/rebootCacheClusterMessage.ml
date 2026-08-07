open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t
  ; cache_node_ids_to_reboot : CacheNodeIdsList.t
  }

let make ~cache_cluster_id ~cache_node_ids_to_reboot () =
  { cache_cluster_id; cache_node_ids_to_reboot }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Xml.required
          "CacheClusterId"
          (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
    ; cache_node_ids_to_reboot =
        Aws.Xml.required
          "CacheNodeIdsToReboot"
          (Aws.Util.option_bind
             (Aws.Xml.member "CacheNodeIdsToReboot" xml)
             CacheNodeIdsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "CacheNodeIdsToReboot.member"
              , CacheNodeIdsList.to_query v.cache_node_ids_to_reboot ))
       ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("CacheNodeIdsToReboot", CacheNodeIdsList.to_json v.cache_node_ids_to_reboot)
       ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
       ])

let of_json j =
  { cache_cluster_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
  ; cache_node_ids_to_reboot =
      CacheNodeIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheNodeIdsToReboot"))
  }
