open Aws.BaseTypes

type t =
  { availability_zone_id : String.t option
  ; outpost_arn : String.t option
  ; source_volume_id : String.t option
  ; iops : Integer.t option
  ; tags : TagList.t
  ; volume_type : VolumeType.t
  ; fast_restored : Boolean.t option
  ; multi_attach_enabled : Boolean.t option
  ; throughput : Integer.t option
  ; sse_type : SSEType.t option
  ; operator : OperatorResponse.t option
  ; volume_initialization_rate : Integer.t option
  ; volume_id : String.t
  ; size : Integer.t
  ; snapshot_id : String.t
  ; availability_zone : String.t
  ; state : VolumeState.t
  ; create_time : DateTime.t
  ; attachments : VolumeAttachmentList.t
  ; encrypted : Boolean.t
  ; kms_key_id : String.t option
  }

let make
    ?availability_zone_id
    ?outpost_arn
    ?source_volume_id
    ?iops
    ?(tags = [])
    ~volume_type
    ?fast_restored
    ?multi_attach_enabled
    ?throughput
    ?sse_type
    ?operator
    ?volume_initialization_rate
    ~volume_id
    ~size
    ~snapshot_id
    ~availability_zone
    ~state
    ~create_time
    ?(attachments = [])
    ~encrypted
    ?kms_key_id
    () =
  { availability_zone_id
  ; outpost_arn
  ; source_volume_id
  ; iops
  ; tags
  ; volume_type
  ; fast_restored
  ; multi_attach_enabled
  ; throughput
  ; sse_type
  ; operator
  ; volume_initialization_rate
  ; volume_id
  ; size
  ; snapshot_id
  ; availability_zone
  ; state
  ; create_time
  ; attachments
  ; encrypted
  ; kms_key_id
  }

let parse xml =
  Some
    { availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; source_volume_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceVolumeId" xml) String.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "iops" xml) Integer.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; volume_type =
        Aws.Xml.required
          "volumeType"
          (Aws.Util.option_bind (Aws.Xml.member "volumeType" xml) VolumeType.parse)
    ; fast_restored =
        Aws.Util.option_bind (Aws.Xml.member "fastRestored" xml) Boolean.parse
    ; multi_attach_enabled =
        Aws.Util.option_bind (Aws.Xml.member "multiAttachEnabled" xml) Boolean.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "throughput" xml) Integer.parse
    ; sse_type = Aws.Util.option_bind (Aws.Xml.member "sseType" xml) SSEType.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; volume_initialization_rate =
        Aws.Util.option_bind (Aws.Xml.member "volumeInitializationRate" xml) Integer.parse
    ; volume_id =
        Aws.Xml.required
          "volumeId"
          (Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse)
    ; size =
        Aws.Xml.required
          "size"
          (Aws.Util.option_bind (Aws.Xml.member "size" xml) Integer.parse)
    ; snapshot_id =
        Aws.Xml.required
          "snapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse)
    ; availability_zone =
        Aws.Xml.required
          "availabilityZone"
          (Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse)
    ; state =
        Aws.Xml.required
          "status"
          (Aws.Util.option_bind (Aws.Xml.member "status" xml) VolumeState.parse)
    ; create_time =
        Aws.Xml.required
          "createTime"
          (Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse)
    ; attachments =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachmentSet" xml)
             VolumeAttachmentList.parse)
    ; encrypted =
        Aws.Xml.required
          "encrypted"
          (Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some (Aws.Query.Pair ("Encrypted", Boolean.to_query v.encrypted))
       ; Some
           (Aws.Query.Pair ("AttachmentSet", VolumeAttachmentList.to_query v.attachments))
       ; Some (Aws.Query.Pair ("CreateTime", DateTime.to_query v.create_time))
       ; Some (Aws.Query.Pair ("Status", VolumeState.to_query v.state))
       ; Some (Aws.Query.Pair ("AvailabilityZone", String.to_query v.availability_zone))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ; Some (Aws.Query.Pair ("Size", Integer.to_query v.size))
       ; Some (Aws.Query.Pair ("VolumeId", String.to_query v.volume_id))
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             Aws.Query.Pair ("VolumeInitializationRate", Integer.to_query f))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.sse_type (fun f ->
             Aws.Query.Pair ("SseType", SSEType.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             Aws.Query.Pair ("MultiAttachEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.fast_restored (fun f ->
             Aws.Query.Pair ("FastRestored", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("VolumeType", VolumeType.to_query v.volume_type))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.source_volume_id (fun f ->
             Aws.Query.Pair ("SourceVolumeId", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Some ("encrypted", Boolean.to_json v.encrypted)
       ; Some ("attachmentSet", VolumeAttachmentList.to_json v.attachments)
       ; Some ("createTime", DateTime.to_json v.create_time)
       ; Some ("status", VolumeState.to_json v.state)
       ; Some ("availabilityZone", String.to_json v.availability_zone)
       ; Some ("snapshotId", String.to_json v.snapshot_id)
       ; Some ("size", Integer.to_json v.size)
       ; Some ("volumeId", String.to_json v.volume_id)
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             "volumeInitializationRate", Integer.to_json f)
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.sse_type (fun f -> "sseType", SSEType.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "throughput", Integer.to_json f)
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             "multiAttachEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.fast_restored (fun f -> "fastRestored", Boolean.to_json f)
       ; Some ("volumeType", VolumeType.to_json v.volume_type)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
       ; Aws.Util.option_map v.source_volume_id (fun f ->
             "sourceVolumeId", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ])

let of_json j =
  { availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; source_volume_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceVolumeId") String.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "iops") Integer.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; volume_type =
      VolumeType.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeType"))
  ; fast_restored = Aws.Util.option_map (Aws.Json.lookup j "fastRestored") Boolean.of_json
  ; multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "multiAttachEnabled") Boolean.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "throughput") Integer.of_json
  ; sse_type = Aws.Util.option_map (Aws.Json.lookup j "sseType") SSEType.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; volume_initialization_rate =
      Aws.Util.option_map (Aws.Json.lookup j "volumeInitializationRate") Integer.of_json
  ; volume_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "volumeId"))
  ; size = Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "size"))
  ; snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "snapshotId"))
  ; availability_zone =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "availabilityZone"))
  ; state = VolumeState.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "status"))
  ; create_time =
      DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "createTime"))
  ; attachments =
      VolumeAttachmentList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attachmentSet"))
  ; encrypted = Boolean.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "encrypted"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  }
