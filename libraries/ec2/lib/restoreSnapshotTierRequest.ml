open Aws.BaseTypes

type t =
  { snapshot_id : String.t
  ; temporary_restore_days : Integer.t option
  ; permanent_restore : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make ~snapshot_id ?temporary_restore_days ?permanent_restore ?dry_run () =
  { snapshot_id; temporary_restore_days; permanent_restore; dry_run }

let parse xml =
  Some
    { snapshot_id =
        Aws.Xml.required
          "SnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse)
    ; temporary_restore_days =
        Aws.Util.option_bind (Aws.Xml.member "TemporaryRestoreDays" xml) Integer.parse
    ; permanent_restore =
        Aws.Util.option_bind (Aws.Xml.member "PermanentRestore" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.permanent_restore (fun f ->
             Aws.Query.Pair ("PermanentRestore", Boolean.to_query f))
       ; Aws.Util.option_map v.temporary_restore_days (fun f ->
             Aws.Query.Pair ("TemporaryRestoreDays", Integer.to_query f))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.permanent_restore (fun f ->
             "PermanentRestore", Boolean.to_json f)
       ; Aws.Util.option_map v.temporary_restore_days (fun f ->
             "TemporaryRestoreDays", Integer.to_json f)
       ; Some ("SnapshotId", String.to_json v.snapshot_id)
       ])

let of_json j =
  { snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; temporary_restore_days =
      Aws.Util.option_map (Aws.Json.lookup j "TemporaryRestoreDays") Integer.of_json
  ; permanent_restore =
      Aws.Util.option_map (Aws.Json.lookup j "PermanentRestore") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
