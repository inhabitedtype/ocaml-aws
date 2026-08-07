type t = { coip_pool : CoipPool.t option }

let make ?coip_pool () = { coip_pool }

let parse xml =
  Some { coip_pool = Aws.Util.option_bind (Aws.Xml.member "coipPool" xml) CoipPool.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.coip_pool (fun f ->
             Aws.Query.Pair ("CoipPool", CoipPool.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.coip_pool (fun f -> "coipPool", CoipPool.to_json f) ])

let of_json j =
  { coip_pool = Aws.Util.option_map (Aws.Json.lookup j "coipPool") CoipPool.of_json }
