type t = { d_b_cluster : DBCluster.t option }

let make ?d_b_cluster () = { d_b_cluster }

let parse xml =
  Some
    { d_b_cluster = Aws.Util.option_bind (Aws.Xml.member "DBCluster" xml) DBCluster.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster (fun f ->
             Aws.Query.Pair ("DBCluster", DBCluster.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster (fun f -> "DBCluster", DBCluster.to_json f) ])

let of_json j =
  { d_b_cluster = Aws.Util.option_map (Aws.Json.lookup j "DBCluster") DBCluster.of_json }
