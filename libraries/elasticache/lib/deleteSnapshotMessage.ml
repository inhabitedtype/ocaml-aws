open Aws.BaseTypes

type t = { snapshot_name : String.t }

let make ~snapshot_name () = { snapshot_name }

let parse xml =
  Some
    { snapshot_name =
        Aws.Xml.required
          "SnapshotName"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotName" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("SnapshotName", String.to_query v.snapshot_name)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt [ Some ("SnapshotName", String.to_json v.snapshot_name) ])

let of_json j =
  { snapshot_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotName"))
  }
