type t = { cache_cluster : CacheCluster.t option }

let make ?cache_cluster () = { cache_cluster }

let parse xml =
  Some
    { cache_cluster =
        Aws.Util.option_bind (Aws.Xml.member "CacheCluster" xml) CacheCluster.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_cluster (fun f ->
             Aws.Query.Pair ("CacheCluster", CacheCluster.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_cluster (fun f ->
             "CacheCluster", CacheCluster.to_json f)
       ])

let of_json j =
  { cache_cluster =
      Aws.Util.option_map (Aws.Json.lookup j "CacheCluster") CacheCluster.of_json
  }
