open Aws.BaseTypes

type t =
  { d_b_cluster_snapshot_identifier : String.t option
  ; d_b_cluster_snapshot_attributes : DBClusterSnapshotAttributeList.t
  }

let make ?d_b_cluster_snapshot_identifier ?(d_b_cluster_snapshot_attributes = []) () =
  { d_b_cluster_snapshot_identifier; d_b_cluster_snapshot_attributes }

let parse xml =
  Some
    { d_b_cluster_snapshot_identifier =
        Aws.Util.option_bind
          (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
          String.parse
    ; d_b_cluster_snapshot_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterSnapshotAttributes" xml)
             DBClusterSnapshotAttributeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DBClusterSnapshotAttributes.member"
              , DBClusterSnapshotAttributeList.to_query v.d_b_cluster_snapshot_attributes
              ))
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBClusterSnapshotIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "DBClusterSnapshotAttributes"
           , DBClusterSnapshotAttributeList.to_json v.d_b_cluster_snapshot_attributes )
       ; Aws.Util.option_map v.d_b_cluster_snapshot_identifier (fun f ->
             "DBClusterSnapshotIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_cluster_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBClusterSnapshotIdentifier") String.of_json
  ; d_b_cluster_snapshot_attributes =
      DBClusterSnapshotAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterSnapshotAttributes"))
  }
