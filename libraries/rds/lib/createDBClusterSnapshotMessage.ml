open Aws.BaseTypes

type t =
  { d_b_cluster_snapshot_identifier : String.t
  ; d_b_cluster_identifier : String.t
  ; tags : TagList.t
  }

let make ~d_b_cluster_snapshot_identifier ~d_b_cluster_identifier ?(tags = []) () =
  { d_b_cluster_snapshot_identifier; d_b_cluster_identifier; tags }

let parse xml =
  Some
    { d_b_cluster_snapshot_identifier =
        Aws.Xml.required
          "DBClusterSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "DBClusterSnapshotIdentifier" xml)
             String.parse)
    ; d_b_cluster_identifier =
        Aws.Xml.required
          "DBClusterIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBClusterIdentifier" xml) String.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ("DBClusterIdentifier", String.to_query v.d_b_cluster_identifier))
       ; Some
           (Aws.Query.Pair
              ( "DBClusterSnapshotIdentifier"
              , String.to_query v.d_b_cluster_snapshot_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("DBClusterIdentifier", String.to_json v.d_b_cluster_identifier)
       ; Some
           ( "DBClusterSnapshotIdentifier"
           , String.to_json v.d_b_cluster_snapshot_identifier )
       ])

let of_json j =
  { d_b_cluster_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterSnapshotIdentifier"))
  ; d_b_cluster_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBClusterIdentifier"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
