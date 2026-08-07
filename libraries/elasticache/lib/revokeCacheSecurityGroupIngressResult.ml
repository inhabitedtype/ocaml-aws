type t = { cache_security_group : CacheSecurityGroup.t option }

let make ?cache_security_group () = { cache_security_group }

let parse xml =
  Some
    { cache_security_group =
        Aws.Util.option_bind
          (Aws.Xml.member "CacheSecurityGroup" xml)
          CacheSecurityGroup.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_security_group (fun f ->
             Aws.Query.Pair ("CacheSecurityGroup", CacheSecurityGroup.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.cache_security_group (fun f ->
             "CacheSecurityGroup", CacheSecurityGroup.to_json f)
       ])

let of_json j =
  { cache_security_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "CacheSecurityGroup")
        CacheSecurityGroup.of_json
  }
