open Aws.BaseTypes

type t =
  { return : Boolean.t option
  ; delete_snapshot_results : DeleteSnapshotResultSet.t
  }

let make ?return ?(delete_snapshot_results = []) () = { return; delete_snapshot_results }

let parse xml =
  Some
    { return = Aws.Util.option_bind (Aws.Xml.member "return" xml) Boolean.parse
    ; delete_snapshot_results =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "deleteSnapshotResultSet" xml)
             DeleteSnapshotResultSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "DeleteSnapshotResultSet"
              , DeleteSnapshotResultSet.to_query v.delete_snapshot_results ))
       ; Aws.Util.option_map v.return (fun f ->
             Aws.Query.Pair ("Return", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "deleteSnapshotResultSet"
           , DeleteSnapshotResultSet.to_json v.delete_snapshot_results )
       ; Aws.Util.option_map v.return (fun f -> "return", Boolean.to_json f)
       ])

let of_json j =
  { return = Aws.Util.option_map (Aws.Json.lookup j "return") Boolean.of_json
  ; delete_snapshot_results =
      DeleteSnapshotResultSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "deleteSnapshotResultSet"))
  }
