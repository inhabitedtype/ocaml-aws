type t = { cache_parameter_group : CacheParameterGroup.t option }

let make ?cache_parameter_group () = { cache_parameter_group }

let parse xml =
  Some
    { cache_parameter_group =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheParameterGroup" xml)
          CacheParameterGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_parameter_group (fun f ->
             Aws.Query.Pair ("CacheParameterGroup", CacheParameterGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_parameter_group (fun f ->
             "CacheParameterGroup", CacheParameterGroup.to_json f)
       ])

let of_json j =
  { cache_parameter_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "CacheParameterGroup")
        CacheParameterGroup.of_json
  }
