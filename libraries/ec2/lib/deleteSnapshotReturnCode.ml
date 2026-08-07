open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; return_code : SnapshotReturnCodes.t option
  }

let make ?snapshot_id ?return_code () = { snapshot_id; return_code }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; return_code =
        Aws.Util.option_bind (Aws.Xml.member "returnCode" xml) SnapshotReturnCodes.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_code (fun f ->
             Aws.Query.Pair ("ReturnCode", SnapshotReturnCodes.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.return_code (fun f ->
             "returnCode", SnapshotReturnCodes.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; return_code =
      Aws.Util.option_map (Aws.Json.lookup j "returnCode") SnapshotReturnCodes.of_json
  }
