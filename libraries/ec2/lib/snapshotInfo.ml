open Aws.BaseTypes

type t =
  { description : String.t option
  ; tags : TagList.t
  ; encrypted : Boolean.t option
  ; volume_id : String.t option
  ; state : SnapshotState.t option
  ; volume_size : Integer.t option
  ; start_time : DateTime.t option
  ; progress : String.t option
  ; owner_id : String.t option
  ; snapshot_id : String.t option
  ; outpost_arn : String.t option
  ; sse_type : SSEType.t option
  ; availability_zone : String.t option
  }

let make
    ?description
    ?(tags = [])
    ?encrypted
    ?volume_id
    ?state
    ?volume_size
    ?start_time
    ?progress
    ?owner_id
    ?snapshot_id
    ?outpost_arn
    ?sse_type
    ?availability_zone
    () =
  { description
  ; tags
  ; encrypted
  ; volume_id
  ; state
  ; volume_size
  ; start_time
  ; progress
  ; owner_id
  ; snapshot_id
  ; outpost_arn
  ; sse_type
  ; availability_zone
  }

let parse xml =
  Some
    { description = Aws.Util.option_bind (Aws.Xml.member "description" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) SnapshotState.parse
    ; volume_size = Aws.Util.option_bind (Aws.Xml.member "volumeSize" xml) Integer.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; sse_type = Aws.Util.option_bind (Aws.Xml.member "sseType" xml) SSEType.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.sse_type (fun f ->
             Aws.Query.Pair ("SseType", SSEType.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", String.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.volume_size (fun f ->
             Aws.Query.Pair ("VolumeSize", Integer.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", SnapshotState.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.description (fun f ->
             Aws.Query.Pair ("Description", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.sse_type (fun f -> "sseType", SSEType.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", String.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.volume_size (fun f -> "volumeSize", Integer.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", SnapshotState.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.description (fun f -> "description", String.to_json f)
       ])

let of_json j =
  { description = Aws.Util.option_map (Aws.Json.lookup j "description") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") SnapshotState.of_json
  ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "volumeSize") Integer.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; sse_type = Aws.Util.option_map (Aws.Json.lookup j "sseType") SSEType.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  }
