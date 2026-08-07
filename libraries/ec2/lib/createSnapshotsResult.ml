type t = { snapshots : SnapshotSet.t }

let make ?(snapshots = []) () = { snapshots }

let parse xml =
  Some
    { snapshots =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "snapshotSet" xml) SnapshotSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SnapshotSet", SnapshotSet.to_query v.snapshots)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("snapshotSet", SnapshotSet.to_json v.snapshots) ])

let of_json j =
  { snapshots =
      SnapshotSet.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "snapshotSet"))
  }
