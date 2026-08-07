type t = { cache_subnet_group : CacheSubnetGroup.t option }

let make ?cache_subnet_group () = { cache_subnet_group }

let parse xml =
  Some
    { cache_subnet_group =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheSubnetGroup" xml)
          CacheSubnetGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_subnet_group (fun f ->
             Aws.Query.Pair ("CacheSubnetGroup", CacheSubnetGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_subnet_group (fun f ->
             "CacheSubnetGroup", CacheSubnetGroup.to_json f)
       ])

let of_json j =
  { cache_subnet_group =
      Aws.Util.option_map (Aws.Json.lookup j "CacheSubnetGroup") CacheSubnetGroup.of_json
  }
