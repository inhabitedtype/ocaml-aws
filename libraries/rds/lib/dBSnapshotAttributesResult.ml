open Aws.BaseTypes

type t =
  { d_b_snapshot_identifier : String.t option
  ; d_b_snapshot_attributes : DBSnapshotAttributeList.t
  }

let make ?d_b_snapshot_identifier ?(d_b_snapshot_attributes = []) () =
  { d_b_snapshot_identifier; d_b_snapshot_attributes }

let parse xml =
  Some
    { d_b_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "DBSnapshotIdentifier" xml) String.parse
    ; d_b_snapshot_attributes =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "DBSnapshotAttributes" xml)
             DBSnapshotAttributeList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DBSnapshotAttributes.member"
              , DBSnapshotAttributeList.to_query v.d_b_snapshot_attributes ))
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             Aws.Query.Pair ("DBSnapshotIdentifier", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "DBSnapshotAttributes"
           , DBSnapshotAttributeList.to_json v.d_b_snapshot_attributes )
       ; Aws.Util.option_map v.d_b_snapshot_identifier (fun f ->
             "DBSnapshotIdentifier", String.to_json f)
       ])

let of_json j =
  { d_b_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "DBSnapshotIdentifier") String.of_json
  ; d_b_snapshot_attributes =
      DBSnapshotAttributeList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DBSnapshotAttributes"))
  }
