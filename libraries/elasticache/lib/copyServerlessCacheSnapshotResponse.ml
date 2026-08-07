type t = { serverless_cache_snapshot : ServerlessCacheSnapshot.t option }

let make ?serverless_cache_snapshot () = { serverless_cache_snapshot }

let parse xml =
  Some
    { serverless_cache_snapshot =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessCacheSnapshot" xml)
          ServerlessCacheSnapshot.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache_snapshot (fun f ->
             Aws.Query.Pair ("ServerlessCacheSnapshot", ServerlessCacheSnapshot.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache_snapshot (fun f ->
             "ServerlessCacheSnapshot", ServerlessCacheSnapshot.to_json f)
       ])

let of_json j =
  { serverless_cache_snapshot =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessCacheSnapshot")
        ServerlessCacheSnapshot.of_json
  }
