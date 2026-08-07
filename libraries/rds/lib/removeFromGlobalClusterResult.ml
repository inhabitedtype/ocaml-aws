type t = { global_cluster : GlobalCluster.t option }

let make ?global_cluster () = { global_cluster }

let parse xml =
  Some
    { global_cluster =
        Aws.Util.option_bind (Aws.Xml.member "GlobalCluster" xml) GlobalCluster.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_cluster (fun f ->
             Aws.Query.Pair ("GlobalCluster", GlobalCluster.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.global_cluster (fun f ->
             "GlobalCluster", GlobalCluster.to_json f)
       ])

let of_json j =
  { global_cluster =
      Aws.Util.option_map (Aws.Json.lookup j "GlobalCluster") GlobalCluster.of_json
  }
