open Aws.BaseTypes

type t =
  { volume_id : String.t option
  ; volume_type : VolumeType.t option
  ; state : VolumeState.t option
  ; size : Integer.t option
  ; iops : Integer.t option
  ; throughput : Integer.t option
  ; outpost_arn : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; source_volume_id : String.t option
  ; snapshot_id : String.t option
  ; operator : OperatorResponse.t option
  ; create_time : DateTime.t option
  ; recycle_bin_enter_time : DateTime.t option
  ; recycle_bin_exit_time : DateTime.t option
  }

let make
    ?volume_id
    ?volume_type
    ?state
    ?size
    ?iops
    ?throughput
    ?outpost_arn
    ?availability_zone
    ?availability_zone_id
    ?source_volume_id
    ?snapshot_id
    ?operator
    ?create_time
    ?recycle_bin_enter_time
    ?recycle_bin_exit_time
    () =
  { volume_id
  ; volume_type
  ; state
  ; size
  ; iops
  ; throughput
  ; outpost_arn
  ; availability_zone
  ; availability_zone_id
  ; source_volume_id
  ; snapshot_id
  ; operator
  ; create_time
  ; recycle_bin_enter_time
  ; recycle_bin_exit_time
  }

let parse xml =
  Some
    { volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "volumeType" xml) VolumeType.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) VolumeState.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "size" xml) Integer.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "iops" xml) Integer.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "throughput" xml) Integer.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; source_volume_id =
        Aws.Util.option_bind (Aws.Xml.member "sourceVolumeId" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; recycle_bin_enter_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinEnterTime" xml) DateTime.parse
    ; recycle_bin_exit_time =
        Aws.Util.option_bind (Aws.Xml.member "recycleBinExitTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             Aws.Query.Pair ("RecycleBinExitTime", DateTime.to_query f))
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             Aws.Query.Pair ("RecycleBinEnterTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.source_volume_id (fun f ->
             Aws.Query.Pair ("SourceVolumeId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Integer.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", VolumeState.to_query f))
       ; Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.recycle_bin_exit_time (fun f ->
             "recycleBinExitTime", DateTime.to_json f)
       ; Aws.Util.option_map v.recycle_bin_enter_time (fun f ->
             "recycleBinEnterTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.source_volume_id (fun f ->
             "sourceVolumeId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "throughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
       ; Aws.Util.option_map v.size (fun f -> "size", Integer.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", VolumeState.to_json f)
       ; Aws.Util.option_map v.volume_type (fun f -> "volumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ])

let of_json j =
  { volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "volumeType") VolumeType.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") VolumeState.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "size") Integer.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "iops") Integer.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "throughput") Integer.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; source_volume_id =
      Aws.Util.option_map (Aws.Json.lookup j "sourceVolumeId") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; recycle_bin_enter_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinEnterTime") DateTime.of_json
  ; recycle_bin_exit_time =
      Aws.Util.option_map (Aws.Json.lookup j "recycleBinExitTime") DateTime.of_json
  }
