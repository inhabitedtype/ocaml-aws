open Aws.BaseTypes

type t =
  { d_b_instance_identifier : String.t
  ; d_b_snapshot_identifier : String.t option
  }

let make ~d_b_instance_identifier ?d_b_snapshot_identifier () =
  { d_b_instance_identifier; d_b_snapshot_identifier }

let parse xml =
  Some
    { d_b_instance_identifier =
        Aws.Xml.required
          "DBInstanceIdentifier"
          (Aws.Util.option_bind (Aws.Xml.member "DBInstanceIdentifier" xml) String.parse)
    ; d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DBInstanceIdentifier", String.to_query v.d_b_instance_identifier))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ; Some ("DBInstanceIdentifier", String.to_json v.d_b_instance_identifier)
       ])

let of_json j =
  { d_b_instance_identifier =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "DBInstanceIdentifier"))
  ; d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  }
