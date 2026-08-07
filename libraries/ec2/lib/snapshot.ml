open Aws.BaseTypes

type t =
  { owner_alias : String.t option
  ; outpost_arn : String.t option
  ; tags : TagList.t
  ; storage_tier : StorageTier.t option
  ; restore_expiry_time : DateTime.t option
  ; sse_type : SSEType.t option
  ; availability_zone : String.t option
  ; transfer_type : TransferType.t option
  ; completion_duration_minutes : Integer.t option
  ; completion_time : DateTime.t option
  ; full_snapshot_size_in_bytes : Long.t option
  ; snapshot_id : String.t
  ; volume_id : String.t
  ; state : SnapshotState.t
  ; state_message : String.t option
  ; start_time : DateTime.t
  ; progress : String.t
  ; owner_id : String.t
  ; description : String.t
  ; volume_size : Integer.t
  ; encrypted : Boolean.t
  ; kms_key_id : String.t option
  ; data_encryption_key_id : String.t option
  }

let make
    ?owner_alias
    ?outpost_arn
    ?(tags = [])
    ?storage_tier
    ?restore_expiry_time
    ?sse_type
    ?availability_zone
    ?transfer_type
    ?completion_duration_minutes
    ?completion_time
    ?full_snapshot_size_in_bytes
    ~snapshot_id
    ~volume_id
    ~state
    ?state_message
    ~start_time
    ~progress
    ~owner_id
    ~description
    ~volume_size
    ~encrypted
    ?kms_key_id
    ?data_encryption_key_id
    () =
  { owner_alias
  ; outpost_arn
  ; tags
  ; storage_tier
  ; restore_expiry_time
  ; sse_type
  ; availability_zone
  ; transfer_type
  ; completion_duration_minutes
  ; completion_time
  ; full_snapshot_size_in_bytes
  ; snapshot_id
  ; volume_id
  ; state
  ; state_message
  ; start_time
  ; progress
  ; owner_id
  ; description
  ; volume_size
  ; encrypted
  ; kms_key_id
  ; data_encryption_key_id
  }

let parse xml =
  Some
    { owner_alias = Aws.Util.option_bind (Aws.Xml.member "ownerAlias" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; storage_tier =
        Aws.Util.option_bind (Aws.Xml.member "storageTier" xml) StorageTier.parse
    ; restore_expiry_time =
        Aws.Util.option_bind (Aws.Xml.member "restoreExpiryTime" xml) DateTime.parse
    ; sse_type = Aws.Util.option_bind (Aws.Xml.member "sseType" xml) SSEType.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; transfer_type =
        Aws.Util.option_bind (Aws.Xml.member "transferType" xml) TransferType.parse
    ; completion_duration_minutes =
        Aws.Util.option_bind
          (Aws.Xml.member "completionDurationMinutes" xml)
          Integer.parse
    ; completion_time =
        Aws.Util.option_bind (Aws.Xml.member "completionTime" xml) DateTime.parse
    ; full_snapshot_size_in_bytes =
        Aws.Util.option_bind (Aws.Xml.member "fullSnapshotSizeInBytes" xml) Long.parse
    ; snapshot_id =
        Aws.Xml.required
          "snapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse)
    ; volume_id =
        Aws.Xml.required
          "volumeId"
          (Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse)
    ; state =
        Aws.Xml.required
          "status"
          (Aws.Util.option_bind (Aws.Xml.member "status" xml) SnapshotState.parse)
    ; state_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; start_time =
        Aws.Xml.required
          "startTime"
          (Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse)
    ; progress =
        Aws.Xml.required
          "progress"
          (Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse)
    ; owner_id =
        Aws.Xml.required
          "ownerId"
          (Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse)
    ; description =
        Aws.Xml.required
          "description"
          (Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse)
    ; volume_size =
        Aws.Xml.required
          "volumeSize"
          (Aws.Util.option_bind (Aws.Xml.member "volumeSize" xml) Integer.parse)
    ; encrypted =
        Aws.Xml.required
          "encrypted"
          (Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; data_encryption_key_id =
        Aws.Util.option_bind (Aws.Xml.member "dataEncryptionKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.data_encryption_key_id (fun f ->
             Aws.Query.Pair ("DataEncryptionKeyId", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some (Aws.Query.Pair ("Encrypted", Boolean.to_query v.encrypted))
       ; Some (Aws.Query.Pair ("VolumeSize", Integer.to_query v.volume_size))
       ; Some (Aws.Query.Pair ("Description", String.to_query v.description))
       ; Some (Aws.Query.Pair ("OwnerId", String.to_query v.owner_id))
       ; Some (Aws.Query.Pair ("Progress", String.to_query v.progress))
       ; Some (Aws.Query.Pair ("StartTime", DateTime.to_query v.start_time))
       ; Aws.Util.option_map v.state_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Some (Aws.Query.Pair ("Status", SnapshotState.to_query v.state))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ; Aws.Util.option_map v.full_snapshot_size_in_bytes (fun f ->
             Aws.Query.Pair ("FullSnapshotSizeInBytes", Long.to_query f))
       ; Aws.Util.option_map v.completion_time (fun f ->
             Aws.Query.Pair ("CompletionTime", DateTime.to_query f))
       ; Aws.Util.option_map v.completion_duration_minutes (fun f ->
             Aws.Query.Pair ("CompletionDurationMinutes", Integer.to_query f))
       ; Aws.Util.option_map v.transfer_type (fun f ->
             Aws.Query.Pair ("TransferType", TransferType.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.sse_type (fun f ->
             Aws.Query.Pair ("SseType", SSEType.to_query f))
       ; Aws.Util.option_map v.restore_expiry_time (fun f ->
             Aws.Query.Pair ("RestoreExpiryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.storage_tier (fun f ->
             Aws.Query.Pair ("StorageTier", StorageTier.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.owner_alias (fun f ->
             Aws.Query.Pair ("OwnerAlias", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.data_encryption_key_id (fun f ->
             "dataEncryptionKeyId", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Some ("encrypted", Boolean.to_json v.encrypted)
       ; Some ("volumeSize", Integer.to_json v.volume_size)
       ; Some ("description", String.to_json v.description)
       ; Some ("ownerId", String.to_json v.owner_id)
       ; Some ("progress", String.to_json v.progress)
       ; Some ("startTime", DateTime.to_json v.start_time)
       ; Aws.Util.option_map v.state_message (fun f -> "statusMessage", String.to_json f)
       ; Some ("status", SnapshotState.to_json v.state)
       ; Some ("volumeId", String.to_json v.volume_id)
       ; Some ("snapshotId", String.to_json v.snapshot_id)
       ; Aws.Util.option_map v.full_snapshot_size_in_bytes (fun f ->
             "fullSnapshotSizeInBytes", Long.to_json f)
       ; Aws.Util.option_map v.completion_time (fun f ->
             "completionTime", DateTime.to_json f)
       ; Aws.Util.option_map v.completion_duration_minutes (fun f ->
             "completionDurationMinutes", Integer.to_json f)
       ; Aws.Util.option_map v.transfer_type (fun f ->
             "transferType", TransferType.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.sse_type (fun f -> "sseType", SSEType.to_json f)
       ; Aws.Util.option_map v.restore_expiry_time (fun f ->
             "restoreExpiryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.storage_tier (fun f ->
             "storageTier", StorageTier.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.owner_alias (fun f -> "ownerAlias", String.to_json f)
       ])

let of_json j =
  { owner_alias = Aws.Util.option_map (Aws.Json.lookup j "ownerAlias") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; storage_tier =
      Aws.Util.option_map (Aws.Json.lookup j "storageTier") StorageTier.of_json
  ; restore_expiry_time =
      Aws.Util.option_map (Aws.Json.lookup j "restoreExpiryTime") DateTime.of_json
  ; sse_type = Aws.Util.option_map (Aws.Json.lookup j "sseType") SSEType.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; transfer_type =
      Aws.Util.option_map (Aws.Json.lookup j "transferType") TransferType.of_json
  ; completion_duration_minutes =
      Aws.Util.option_map (Aws.Json.lookup j "completionDurationMinutes") Integer.of_json
  ; completion_time =
      Aws.Util.option_map (Aws.Json.lookup j "completionTime") DateTime.of_json
  ; full_snapshot_size_in_bytes =
      Aws.Util.option_map (Aws.Json.lookup j "fullSnapshotSizeInBytes") Long.of_json
  ; snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "snapshotId"))
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeId"))
  ; state = SnapshotState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "status"))
  ; state_message = Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; start_time = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "startTime"))
  ; progress = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "progress"))
  ; owner_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ownerId"))
  ; description =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "description"))
  ; volume_size =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeSize"))
  ; encrypted = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "encrypted"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; data_encryption_key_id =
      Aws.Util.option_map (Aws.Json.lookup j "dataEncryptionKeyId") String.of_json
  }
