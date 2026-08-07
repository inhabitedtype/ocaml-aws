type t = { d_b_cluster_snapshot : DBClusterSnapshot.t option }

let make ?d_b_cluster_snapshot () = { d_b_cluster_snapshot }

let parse xml =
  Some
    { d_b_cluster_snapshot =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshot" xml)
          DBClusterSnapshot.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_snapshot (fun f ->
             Aws.Query.Pair ("DBClusterSnapshot", DBClusterSnapshot.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_snapshot (fun f ->
             "DBClusterSnapshot", DBClusterSnapshot.to_json f)
       ])

let of_json j =
  { d_b_cluster_snapshot =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterSnapshot")
        DBClusterSnapshot.of_json
  }
