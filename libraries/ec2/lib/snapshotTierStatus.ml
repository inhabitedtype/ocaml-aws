open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; volume_id : String.t option
  ; status : SnapshotState.t option
  ; owner_id : String.t option
  ; tags : TagList.t
  ; storage_tier : StorageTier.t option
  ; last_tiering_start_time : DateTime.t option
  ; last_tiering_progress : Integer.t option
  ; last_tiering_operation_status : TieringOperationStatus.t option
  ; last_tiering_operation_status_detail : String.t option
  ; archival_complete_time : DateTime.t option
  ; restore_expiry_time : DateTime.t option
  }

let make
    ?snapshot_id
    ?volume_id
    ?status
    ?owner_id
    ?(tags = [])
    ?storage_tier
    ?last_tiering_start_time
    ?last_tiering_progress
    ?last_tiering_operation_status
    ?last_tiering_operation_status_detail
    ?archival_complete_time
    ?restore_expiry_time
    () =
  { snapshot_id
  ; volume_id
  ; status
  ; owner_id
  ; tags
  ; storage_tier
  ; last_tiering_start_time
  ; last_tiering_progress
  ; last_tiering_operation_status
  ; last_tiering_operation_status_detail
  ; archival_complete_time
  ; restore_expiry_time
  }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "status" xml) SnapshotState.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; storage_tier =
        Aws.Util.option_bind (Aws.Xml.member "storageTier" xml) StorageTier.parse
    ; last_tiering_start_time =
        Aws.Util.option_bind (Aws.Xml.member "lastTieringStartTime" xml) DateTime.parse
    ; last_tiering_progress =
        Aws.Util.option_bind (Aws.Xml.member "lastTieringProgress" xml) Integer.parse
    ; last_tiering_operation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "lastTieringOperationStatus" xml)
          TieringOperationStatus.parse
    ; last_tiering_operation_status_detail =
        Aws.Util.option_bind
          (Aws.Xml.member "lastTieringOperationStatusDetail" xml)
          String.parse
    ; archival_complete_time =
        Aws.Util.option_bind (Aws.Xml.member "archivalCompleteTime" xml) DateTime.parse
    ; restore_expiry_time =
        Aws.Util.option_bind (Aws.Xml.member "restoreExpiryTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.restore_expiry_time (fun f ->
             Aws.Query.Pair ("RestoreExpiryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.archival_complete_time (fun f ->
             Aws.Query.Pair ("ArchivalCompleteTime", DateTime.to_query f))
       ; Aws.Util.option_map v.last_tiering_operation_status_detail (fun f ->
             Aws.Query.Pair ("LastTieringOperationStatusDetail", String.to_query f))
       ; Aws.Util.option_map v.last_tiering_operation_status (fun f ->
             Aws.Query.Pair
               ("LastTieringOperationStatus", TieringOperationStatus.to_query f))
       ; Aws.Util.option_map v.last_tiering_progress (fun f ->
             Aws.Query.Pair ("LastTieringProgress", Integer.to_query f))
       ; Aws.Util.option_map v.last_tiering_start_time (fun f ->
             Aws.Query.Pair ("LastTieringStartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.storage_tier (fun f ->
             Aws.Query.Pair ("StorageTier", StorageTier.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", SnapshotState.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.restore_expiry_time (fun f ->
             "restoreExpiryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.archival_complete_time (fun f ->
             "archivalCompleteTime", DateTime.to_json f)
       ; Aws.Util.option_map v.last_tiering_operation_status_detail (fun f ->
             "lastTieringOperationStatusDetail", String.to_json f)
       ; Aws.Util.option_map v.last_tiering_operation_status (fun f ->
             "lastTieringOperationStatus", TieringOperationStatus.to_json f)
       ; Aws.Util.option_map v.last_tiering_progress (fun f ->
             "lastTieringProgress", Integer.to_json f)
       ; Aws.Util.option_map v.last_tiering_start_time (fun f ->
             "lastTieringStartTime", DateTime.to_json f)
       ; Aws.Util.option_map v.storage_tier (fun f ->
             "storageTier", StorageTier.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "status", SnapshotState.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "status") SnapshotState.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; storage_tier =
      Aws.Util.option_map (Aws.Json.lookup j "storageTier") StorageTier.of_json
  ; last_tiering_start_time =
      Aws.Util.option_map (Aws.Json.lookup j "lastTieringStartTime") DateTime.of_json
  ; last_tiering_progress =
      Aws.Util.option_map (Aws.Json.lookup j "lastTieringProgress") Integer.of_json
  ; last_tiering_operation_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastTieringOperationStatus")
        TieringOperationStatus.of_json
  ; last_tiering_operation_status_detail =
      Aws.Util.option_map
        (Aws.Json.lookup j "lastTieringOperationStatusDetail")
        String.of_json
  ; archival_complete_time =
      Aws.Util.option_map (Aws.Json.lookup j "archivalCompleteTime") DateTime.of_json
  ; restore_expiry_time =
      Aws.Util.option_map (Aws.Json.lookup j "restoreExpiryTime") DateTime.of_json
  }
