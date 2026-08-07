type t =
  { d_b_cluster_snapshot_attributes_result : DBClusterSnapshotAttributesResult.t option }

let make ?d_b_cluster_snapshot_attributes_result () =
  { d_b_cluster_snapshot_attributes_result }

let parse xml =
  Some
    { d_b_cluster_snapshot_attributes_result =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshotAttributesResult" xml)
          DBClusterSnapshotAttributesResult.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
             Aws.Query.Pair
               ( "DBClusterSnapshotAttributesResult"
               , DBClusterSnapshotAttributesResult.to_query f ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_cluster_snapshot_attributes_result (fun f ->
             ( "DBClusterSnapshotAttributesResult"
             , DBClusterSnapshotAttributesResult.to_json f ))
       ])

let of_json j =
  { d_b_cluster_snapshot_attributes_result =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBClusterSnapshotAttributesResult")
        DBClusterSnapshotAttributesResult.of_json
  }
