open Aws.BaseTypes

type t =
  { d_b_snapshot_identifier : String.t
  ; d_b_instance_identifier : String.t
  ; tags : TagList.t
  }

let make ~d_b_snapshot_identifier ~d_b_instance_identifier ?(tags = []) () =
  { d_b_snapshot_identifier; d_b_instance_identifier; tags }

let parse xml =
  Some
    { d_b_snapshot_identifier =
        Aws.Xml.required
          "DBSnapshotIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse)
    ; d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
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
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ; Some
           (Aws.Query.Pair
              ("DBSnapshotIdentifier", String.to_query v.d_b_snapshot_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ; Some ("DBSnapshotIdentifier", String.to_json v.d_b_snapshot_identifier)
       ])

let of_json j =
  { d_b_snapshot_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSnapshotIdentifier"))
  ; d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
