open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; outpost_arn : String.t option
  ; description : String.t option
  ; encrypted : Boolean.t option
  ; owner_id : String.t option
  ; progress : String.t option
  ; start_time : DateTime.t option
  ; state : SnapshotState.t option
  ; volume_id : String.t option
  ; volume_size : Integer.t option
  ; sse_type : SSEType.t option
  }

let make
    ?snapshot_id
    ?outpost_arn
    ?description
    ?encrypted
    ?owner_id
    ?progress
    ?start_time
    ?state
    ?volume_id
    ?volume_size
    ?sse_type
    () =
  { snapshot_id
  ; outpost_arn
  ; description
  ; encrypted
  ; owner_id
  ; progress
  ; start_time
  ; state
  ; volume_id
  ; volume_size
  ; sse_type
  }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "status" xml) SnapshotState.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; volume_size = Aws.Util.option_bind (Aws.Xml.member "volumeSize" xml) Integer.parse
    ; sse_type = Aws.Util.option_bind (Aws.Xml.member "sseType" xml) SSEType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_type (fun f ->
             Aws.Query.Pair ("SseType", SSEType.to_query f))
       ; Aws.Util.option_map v.volume_size (fun f ->
             Aws.Query.Pair ("VolumeSize", Integer.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("Status", SnapshotState.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sse_type (fun f -> "sseType", SSEType.to_json f)
       ; Aws.Util.option_map v.volume_size (fun f -> "volumeSize", Integer.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "status", SnapshotState.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") String.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "status") SnapshotState.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "volumeSize") Integer.of_json
  ; sse_type = Aws.Util.option_map (Aws.Json.lookup j "sseType") SSEType.of_json
  }
