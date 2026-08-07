type t = { d_b_snapshot_attributes_result : DBSnapshotAttributesResult.t option }

let make ?d_b_snapshot_attributes_result () = { d_b_snapshot_attributes_result }

let parse xml =
  Some
    { d_b_snapshot_attributes_result =
        Aws.Util.option_bind
          (Aws.Xml.member "DBSnapshotAttributesResult" xml)
          DBSnapshotAttributesResult.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot_attributes_result (fun f ->
             Aws.Query.Pair
               ("DBSnapshotAttributesResult", DBSnapshotAttributesResult.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.d_b_snapshot_attributes_result (fun f ->
             "DBSnapshotAttributesResult", DBSnapshotAttributesResult.to_json f)
       ])

let of_json j =
  { d_b_snapshot_attributes_result =
      Aws.Util.option_map
        (Aws.Json.lookup j "DBSnapshotAttributesResult")
        DBSnapshotAttributesResult.of_json
  }
