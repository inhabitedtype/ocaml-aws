type t = { reserved_cache_node : ReservedCacheNode.t option }

let make ?reserved_cache_node () = { reserved_cache_node }

let parse xml =
  Some
    { reserved_cache_node =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedCacheNode" xml)
          ReservedCacheNode.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_cache_node (fun f ->
             Aws.Query.Pair ("ReservedCacheNode", ReservedCacheNode.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.reserved_cache_node (fun f ->
             "ReservedCacheNode", ReservedCacheNode.to_json f)
       ])

let of_json j =
  { reserved_cache_node =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedCacheNode")
        ReservedCacheNode.of_json
  }
