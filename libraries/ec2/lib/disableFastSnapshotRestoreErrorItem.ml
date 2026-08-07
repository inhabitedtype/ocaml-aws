open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; fast_snapshot_restore_state_errors : DisableFastSnapshotRestoreStateErrorSet.t
  }

let make ?snapshot_id ?(fast_snapshot_restore_state_errors = []) () =
  { snapshot_id; fast_snapshot_restore_state_errors }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; fast_snapshot_restore_state_errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "fastSnapshotRestoreStateErrorSet" xml)
             DisableFastSnapshotRestoreStateErrorSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "FastSnapshotRestoreStateErrorSet"
              , DisableFastSnapshotRestoreStateErrorSet.to_query
                  v.fast_snapshot_restore_state_errors ))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "fastSnapshotRestoreStateErrorSet"
           , DisableFastSnapshotRestoreStateErrorSet.to_json
               v.fast_snapshot_restore_state_errors )
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; fast_snapshot_restore_state_errors =
      DisableFastSnapshotRestoreStateErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "fastSnapshotRestoreStateErrorSet"))
  }
