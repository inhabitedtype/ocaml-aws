open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; tiering_start_time : DateTime.t option
  }

let make ?snapshot_id ?tiering_start_time () = { snapshot_id; tiering_start_time }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; tiering_start_time =
        Aws.Util.option_bind (Aws.Xml.member "tieringStartTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tiering_start_time (fun f ->
             Aws.Query.Pair ("TieringStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.tiering_start_time (fun f ->
             "tieringStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; tiering_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "tieringStartTime") DateTime.of_json
  }
