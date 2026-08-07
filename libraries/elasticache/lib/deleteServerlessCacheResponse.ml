type t = { serverless_cache : ServerlessCache.t option }

let make ?serverless_cache () = { serverless_cache }

let parse xml =
  Some
    { serverless_cache =
        Aws.Util.option_bind (Aws.Xml.member "ServerlessCache" xml) ServerlessCache.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache (fun f ->
             Aws.Query.Pair ("ServerlessCache", ServerlessCache.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache (fun f ->
             "ServerlessCache", ServerlessCache.to_json f)
       ])

let of_json j =
  { serverless_cache =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCache") ServerlessCache.of_json
  }
