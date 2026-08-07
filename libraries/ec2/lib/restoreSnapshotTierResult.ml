open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; restore_start_time : DateTime.t option
  ; restore_duration : Integer.t option
  ; is_permanent_restore : Boolean.t option
  }

let make ?snapshot_id ?restore_start_time ?restore_duration ?is_permanent_restore () =
  { snapshot_id; restore_start_time; restore_duration; is_permanent_restore }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; restore_start_time =
        Aws.Util.option_bind (Aws.Xml.member "restoreStartTime" xml) DateTime.parse
    ; restore_duration =
        Aws.Util.option_bind (Aws.Xml.member "restoreDuration" xml) Integer.parse
    ; is_permanent_restore =
        Aws.Util.option_bind (Aws.Xml.member "isPermanentRestore" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_permanent_restore (fun f ->
             Aws.Query.Pair ("IsPermanentRestore", Boolean.to_query f))
       ; Aws.Util.option_map v.restore_duration (fun f ->
             Aws.Query.Pair ("RestoreDuration", Integer.to_query f))
       ; Aws.Util.option_map v.restore_start_time (fun f ->
             Aws.Query.Pair ("RestoreStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.is_permanent_restore (fun f ->
             "isPermanentRestore", Boolean.to_json f)
       ; Aws.Util.option_map v.restore_duration (fun f ->
             "restoreDuration", Integer.to_json f)
       ; Aws.Util.option_map v.restore_start_time (fun f ->
             "restoreStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; restore_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "restoreStartTime") DateTime.of_json
  ; restore_duration =
      Aws.Util.option_map (Aws.Json.lookup j "restoreDuration") Integer.of_json
  ; is_permanent_restore =
      Aws.Util.option_map (Aws.Json.lookup j "isPermanentRestore") Boolean.of_json
  }
